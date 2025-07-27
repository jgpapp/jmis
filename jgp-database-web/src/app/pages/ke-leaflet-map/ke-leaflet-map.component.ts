import { AfterViewInit, Component, Input, OnDestroy, OnInit, SimpleChanges } from '@angular/core';
import * as L from 'leaflet';
import { HttpClient } from '@angular/common/http';
import { LeafletModule } from '@asymmetrik/ngx-leaflet';



@Component({
  selector: 'app-ke-leaflet-map',
  standalone: true,
  imports: [LeafletModule],
  templateUrl: './ke-leaflet-map.component.html',
  styleUrl: './ke-leaflet-map.component.scss'
})
export class KeLeafletMapComponent implements OnInit, AfterViewInit, OnDestroy {
  private map!: L.Map;
  private geoJsonLayer: L.GeoJSON | undefined;
  private kenyanMapJson = 'data/kenya-counties.json';
  @Input({required: true, alias: 'countyDataSummary'}) countyDataSummary: any[];
  @Input({required: true, alias: 'measureField'}) measureField: string;

  constructor(private http: HttpClient) { }

  ngOnInit(): void {
    // No specific initialization needed here for the map itself
  }

  ngAfterViewInit(): void {
    this.initMap();
    this.loadKenyanCountiesGeoJSON();
    this.addCircularMarkersWithTooltip(); // <--- Call the new method to add markers
  }

  ngOnChanges(changes: SimpleChanges): void {
      if (changes['countyDataSummary']) {
        this.countyDataSummary = changes['countyDataSummary']['currentValue']
        this.addCircularMarkersWithTooltip();
      }
      if (changes['measureField']) {
        this.measureField = changes['measureField']['currentValue']
        this.addCircularMarkersWithTooltip();
      }
      
    }

  private initMap(): void {
    // Center the map on Kenya
    this.map = L.map('map', {
      center: [ -0.0236, 37.9062 ], // Approximate center of Kenya
      zoom: 6 // Adjust zoom level to show all counties
    });

    const tiles = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
      maxZoom: 18,
      minZoom: 3,
      attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    });

    tiles.addTo(this.map);
  }

   // New method to add markers
  private addMarkers(): void {
    // Example marker data for a few locations in Kenya
    const markerData = [
      { lat: -1.2921, lng: 36.8219, popup: '<b>Nairobi City</b><br>Capital of Kenya' },
      { lat: -0.5167, lng: 35.2667, popup: '<b>Eldoret</b><br>Uasin Gishu County' },
      { lat: -4.0437, lng: 39.6682, popup: '<b>Mombasa</b><br>Mombasa County' },
      { lat: 0.5283, lng: 34.7829, popup: '<b>Kisumu</b><br>Kisumu County' }
    ];

    markerData.forEach(data => {
      L.marker([data.lat, data.lng])
        .addTo(this.map)
        .bindPopup(data.popup);
    });
  }

  // Updated method to add circular markers
  private addCircularMarkersWithTooltip(): void {
    let markerData: {
      lat: any; // Replace with actual latitude property
      lng: any; // Replace with actual longitude property
      tooltip: string; // Replace with actual tooltip text, e.g., county name
    }[] = this.createMarkers();


    markerData.forEach(data => {
      L.circleMarker([data.lat, data.lng], {
        radius: 6, // Smaller radius in pixels
        fillColor: "#ff7800", // Orange fill color
        color: "#000",       // Black stroke color
        weight: 1,           // Stroke width in pixels
        opacity: 1,          // Stroke opacity
        fillOpacity: 0.8     // Fill opacity
      })
      .addTo(this.map)
      .bindTooltip(data.tooltip, {
        permanent: false,
        direction: 'top',
        offset: L.point(0, -10)
      });
    });
  }

  createMarkers(): {lat: any, lng: any, tooltip: string}[] {
    let markerData: {
      lat: any; // Replace with actual latitude property
      lng: any; // Replace with actual longitude property
      tooltip: string; // Replace with actual tooltip text, e.g., county name
    }[] = [];

    this.countyDataSummary.forEach(county => {
      // Assuming each county has 'lat', 'lng', and 'tooltip' properties
      if (this.showMapMarker(county)) {
        markerData.push({
          lat: county.approximateCenterLatitude, // Replace with actual latitude property
          lng: county.approximateCenterLongitude, // Replace with actual longitude property
          tooltip: this.getMapToolTip(county) // Replace with actual tooltip text, e.g., county name
        });
      }
    });

    return markerData;
  }


  getMapToolTip(county: any): string {
      if (this.measureField === 'loanFields') {
        return `<b>County: ${county.countyName}</b><br><i>Loaned Businesses: ${county.businessesLoaned}</i><br><i>Amount Disbursed: ${county.amountDisbursed}</i>`;
      } 
      return `<b>County: ${county.countyName}</b><br><i>Result: ${county[this.measureField]}</i>`;
    }

    showMapMarker(county: any): boolean {
      if (this.measureField === 'loanFields') {
        return county.businessesLoaned > 0 || county.amountDisbursed > 0;
      }
      return county[this.measureField] > 0;
    }

  
  private loadKenyanCountiesGeoJSON(): void {
    // Path to your Kenyan counties GeoJSON file
    this.http.get(this.kenyanMapJson).subscribe((geojson: any) => {
      if (this.map) {
        // Remove existing GeoJSON layer if it exists (for updates)
        if (this.geoJsonLayer) {
          this.map.removeLayer(this.geoJsonLayer);
        }

        this.geoJsonLayer = L.geoJSON(geojson, {
          style: (feature) => {
            // Define a default style for the counties
            return {
              fillColor: '#6baed6', // Light blue fill
              weight: 2,
              opacity: 1,
              color: 'white', // White border
              dashArray: '3',
              fillOpacity: 0.7
            };
          },
          onEachFeature: (feature, layer) => {
            // Assuming your GeoJSON has a 'name' property for the county name
            // You might need to inspect your GeoJSON file to find the correct property name
            // Common property names for county/admin names include: 'name', 'ADM1_EN', 'County', etc.
            if (feature.properties && feature.properties.ADM1_EN) { // Example: using 'ADM1_EN'
              layer.bindPopup(`<b>${feature.properties.ADM1_EN}</b>`);
            } else if (feature.properties && feature.properties.COUNTY_NAM) { // Another common property
              layer.bindPopup(`<b>${feature.properties.COUNTY_NAM}</b>`);
            } else if (feature.properties && feature.properties.name) { // Generic 'name'
              layer.bindPopup(`<b>${feature.properties.name}</b>`);
            }

            // Add interaction (mouseover, mouseout, click)
            layer.on({
              mouseover: (e) => this.highlightFeature(e),
              mouseout: (e) => this.resetHighlight(e, this.geoJsonLayer!),
              click: (e) => this.zoomToFeature(e)
            });
          }
        }).addTo(this.map);

        // Optionally, fit the map bounds to the GeoJSON layer
        // This will zoom and pan the map to show all counties
        this.map.fitBounds(this.geoJsonLayer.getBounds());
      }
    });
  }

  // --- Interaction functions (same as previous example) ---
  private highlightFeature(e: any): void {
    const layer = e.target;

    layer.setStyle({
      weight: 5,
      color: '#666',
      dashArray: '',
      fillOpacity: 0.7
    });

    if (!L.Browser.ie && !L.Browser.opera && !L.Browser.edge) {
      layer.bringToFront();
    }
  }

  private resetHighlight(e: any, geoJsonLayer: L.GeoJSON): void {
    // Reset style to the original defined in the geoJsonLayer's style function
    geoJsonLayer.resetStyle(e.target);
  }

  private zoomToFeature(e: any): void {
    this.map.fitBounds(e.target.getBounds());
  }

  ngOnDestroy(): void {
    if (this.map) {
      this.map.remove();
    }
  }
}
