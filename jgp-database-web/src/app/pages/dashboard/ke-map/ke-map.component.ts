import { CommonModule } from '@angular/common';
import { HttpClient } from '@angular/common/http';
import { Component, Input, OnChanges, OnInit, SimpleChanges } from '@angular/core';
import { tileLayer, geoJSON, map, Map as Mapp, Layer, DivIcon, Marker } from 'leaflet';

@Component({
  selector: 'app-ke-map',
  standalone: true,
  imports: [CommonModule ],
  templateUrl: './ke-map.component.html',
  styleUrl: './ke-map.component.scss'
})
export class KeMapComponent implements OnInit, OnChanges {
  private map!: Mapp;
  private geoJsonLayer!: Layer;
  private markers: Marker[] = [];
  private geoJsonData: any = null; // To store the GeoJSON data
  @Input('countyData') countyData: Map<number, any>;
  @Input('countyDataToBePicked') countyDataToBePicked: any;
  @Input('mapWidth') mapWidth: number;
  @Input('mapHeight') mapHeight: number;

  constructor(private http: HttpClient) {}

  ngOnInit(): void {
    this.renderMap();
  }

  ngOnChanges(changes: SimpleChanges) {
    console.log(changes);
    //this.renderMap();
  }

  renderMap(): void {
    this.initializeMap();
    this.loadGeoJson(); // Load GeoJSON data from the file
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

}
