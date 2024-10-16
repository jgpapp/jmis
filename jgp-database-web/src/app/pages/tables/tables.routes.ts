import { Routes } from '@angular/router';

export const routes: Routes = [
    { path: '', redirectTo: 'basic', pathMatch: 'full' },
    { path: 'basic', loadComponent: () => import('./basic/basic.component').then(c => c.BasicComponent), data: { breadcrumb: 'Basic table' } },
    { path: 'paging', loadComponent: () => import('./paging/paging.component').then(c => c.PagingComponent), data: { breadcrumb: 'Paging table' } },
    { path: 'sorting', loadComponent: () => import('./sorting/sorting.component').then(c => c.SortingComponent), data: { breadcrumb: 'Sorting table' } },
    { path: 'filtering', loadComponent: () => import('./filtering/filtering.component').then(c => c.FilteringComponent), data: { breadcrumb: 'Filtering table' } },
    { path: 'selecting', loadComponent: () => import('./selecting/selecting.component').then(c => c.SelectingComponent), data: { breadcrumb: 'Selecting table' } },
    { path: 'ngx-table', loadComponent: () => import('./ngx-table/ngx-table.component').then(c => c.NgxTableComponent), data: { breadcrumb: 'Ngx datatable' } },
];