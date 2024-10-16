import { Routes } from '@angular/router';

export const routes: Routes = [
    { path: '', redirectTo: 'googlemaps', pathMatch: 'full' },
    { path: 'googlemaps', loadComponent: () => import('./google-maps/google-maps.component').then(c => c.GoogleMapsComponent), data: { breadcrumb: 'Google Maps' } },
    { path: 'leafletmaps', loadComponent: () => import('./leaflet-maps/leaflet-maps.component').then(c => c.LeafletMapsComponent), data: { breadcrumb: 'Leaflet Maps' } }
];