import { Component } from '@angular/core';
import { Settings, SettingsService } from '@services/settings.service';
import { GoogleMapsModule } from '@angular/google-maps';

@Component({
  selector: 'app-google-maps',
  standalone: true,
  imports: [
    GoogleMapsModule
  ],
  templateUrl: './google-maps.component.html'
})
export class GoogleMapsComponent {
  settings: Settings; 
  center: google.maps.LatLngLiteral = { lat: 45.421530, lng: -75.697193 };
  zoom = 7;
  markerOptions: google.maps.MarkerOptions = { draggable: false };
  markerPositions: google.maps.LatLngLiteral[] = [
    { lat: 45.421530, lng: -75.697193 }
  ];
  constructor(public settingsService: SettingsService) { 
    this.settings = this.settingsService.settings; 
  }
}
