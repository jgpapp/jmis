import { CommonModule } from '@angular/common';
import { HttpClient } from '@angular/common/http';
import { Component, ElementRef, Input, OnChanges, OnInit, SimpleChanges, ViewChild } from '@angular/core';
import html2canvas from 'html2canvas';
import jsPDF from 'jspdf';
import L, { tileLayer, geoJSON, map, Map as Mapp, Layer, DivIcon, Marker } from 'leaflet';

@Component({
  selector: 'app-ke-map',
  standalone: true,
  imports: [CommonModule ],
  templateUrl: './ke-map.component.html',
  styleUrl: './ke-map.component.scss'
})
export class KeMapComponent implements OnInit, OnChanges {
  private map!: L.Map;
  private geoJsonLayer!: L.GeoJSON;
  private markers: Marker[] = [];
  private geoJsonData: any = null; // To store the GeoJSON data
  @Input('countyData') countyData: Map<number, any>;
  @Input('countyDataToBePicked') countyDataToBePicked: any;
  @Input('mapWidth') mapWidth: number;
  @Input('mapHeight') mapHeight: number;
  @ViewChild('mapContainer', { static: true }) mapContainer!: ElementRef;

  constructor(private http: HttpClient) {}

  ngOnInit(): void {
    this.initMap();
  }

  ngOnChanges(changes: SimpleChanges) {
    console.log(changes);
    //this.renderMap();
  }

  renderMap(): void {
    this.initializeMap();
    this.loadGeoJson(); // Load GeoJSON data from the file
  }

  private initMap(): void {
    this.map = map('map').setView([1.2921, 36.8219], 7); // Initial view Centered on Kenya

    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(this.map);

    // Fetch GeoJSON data
    this.http.get<any>('data/kenya-counties.json') // Replace with your GeoJSON data source
      .subscribe(data => {
        this.geoJsonLayer = L.geoJSON(data, {
          onEachFeature: (feature, layer) => {
            const properties = feature.properties;
            const countyCode = properties.COUNTY_COD;
            const dataMap = new Map(Object.entries(this.countyData))
            const dataToDisplay = dataMap.get(`${countyCode}`) ? dataMap.get(`${countyCode}`)[this.countyDataToBePicked] : '';
            const popupContent = `
              ${properties.COUNTY_NAM}:${dataToDisplay | 0}
            `;

            if (feature.geometry.type === 'Polygon') {
              const center = this.getPolygonCenter(feature.geometry.coordinates[0]);
              const label = new DivIcon({
                className: 'region-label',
                html: `${popupContent}`,
              });
              const marker = new Marker(center, { icon: label });
              marker.addTo(this.map);
              this.markers.push(marker);
            }

            layer.bindPopup(popupContent);
          }
        }).addTo(this.map);
      });
  }

  private initializeMap(): void {
    if (this.map) {
      console.log(this.map)
      this.map.remove(); // Properly destroy the existing map instance
    }
    console.log(this.map)
    this.map = map('map').setView([1.2921, 36.8219], 7); // Centered on Kenya
console.log(this.map)
    // Add a base layer
    tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
      maxZoom: 18,
    }).addTo(this.map);
  }

  private loadGeoJson(): void {
    // Fetch the GeoJSON file
    this.http.get('data/kenya-counties.json').subscribe((data) => {
      this.geoJsonData = data;
      this.renderGeoJsonLayer(); // Render the GeoJSON layer once the data is loaded
    });
  }

  private renderGeoJsonLayer(): void {
    // Remove old markers and layer if they exist
    this.markers.forEach((marker) => this.map.removeLayer(marker));
    this.markers = [];

    if (this.geoJsonLayer) {
      this.map.removeLayer(this.geoJsonLayer);
    }

    // Add GeoJSON layer with style and dynamic popups
    this.geoJsonLayer = geoJSON(this.geoJsonData, {
      style: () => ({
        color: '#3388ff',
        weight: 2,
        fillOpacity: 0.5,
      }),
      onEachFeature: (feature, layer) => {
        const properties = feature.properties;
        const countyCode = properties.COUNTY_COD;
        const dataMap = new Map(Object.entries(this.countyData))
        const dataToDisplay = dataMap.get(`${countyCode}`) ? dataMap.get(`${countyCode}`)[this.countyDataToBePicked] : '';
        if (properties) {
          layer.bindPopup(`${properties.COUNTY_NAM}: ${properties.value || 0}`);
        }

        // Add dynamic labels (numbers) at region centers
        if (feature.geometry.type === 'Polygon') {
          const center = this.getPolygonCenter(feature.geometry.coordinates[0]);
          const label = new DivIcon({
            className: 'region-label',
            html: `<strong>${dataToDisplay || 0}</strong>`,
          });
          const marker = new Marker(center, { icon: label });
          marker.addTo(this.map);
          this.markers.push(marker);
        }
      },
    });

    this.geoJsonLayer.addTo(this.map);
  }

  private getPolygonCenter(coordinates: any[]): [number, number] {
    let latSum = 0;
    let lngSum = 0;
    const len = coordinates.length;

    coordinates.forEach(([lng, lat]) => {
      latSum += lat;
      lngSum += lng;
    });

    return [latSum / len, lngSum / len];
  }

  downloadMapAsPDF() {
    html2canvas(this.mapContainer.nativeElement).then(canvas => {
      const imgData = canvas.toDataURL('image/png');
      const pdf = new jsPDF();
      const imgWidth = 210; // Adjust width as needed
      const imgHeight = canvas.height * imgWidth / canvas.width;
      pdf.addImage(imgData, 'PNG', 0, 0, imgWidth, imgHeight);
      pdf.save('map.pdf');
    });
  }
}
