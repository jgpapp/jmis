import { Routes } from '@angular/router';

export const routes: Routes = [
    { path: '', redirectTo: 'buttons', pathMatch: 'full'},
    { path: 'buttons', loadComponent: () => import('./buttons/buttons.component').then(c => c.ButtonsComponent), data: { breadcrumb: 'Buttons' } },
    { path: 'cards', loadComponent: () => import('./cards/cards.component').then(c => c.CardsComponent), data: { breadcrumb: 'Cards' } },
    { path: 'lists', loadComponent: () => import('./lists/lists.component').then(c => c.ListsComponent), data: { breadcrumb: 'Lists' } },
    { path: 'grids', loadComponent: () => import('./grids/grids.component').then(c => c.GridsComponent), data: { breadcrumb: 'Grids' } },
    { path: 'tabs', loadComponent: () => import('./tabs/tabs.component').then(c => c.TabsComponent), data: { breadcrumb: 'Tabs' } },
    { path: 'expansion-panel', loadComponent: () => import('./expansion-panel/expansion-panel.component').then(c => c.ExpansionPanelComponent), data: { breadcrumb: 'Expansion Panel' } },
    { path: 'chips', loadComponent: () => import('./chips/chips.component').then(c => c.ChipsComponent), data: { breadcrumb: 'Chips' } },
    { path: 'progress', loadComponent: () => import('./progress/progress.component').then(c => c.ProgressComponent), data: { breadcrumb: 'Progress' } },
    { path: 'dialog', loadComponent: () => import('./dialog/dialog.component').then(c => c.DialogComponent), data: { breadcrumb: 'Dialog' } },
    { path: 'tooltip', loadComponent: () => import('./tooltip/tooltip.component').then(c => c.TooltipComponent), data: { breadcrumb: 'Tooltip' } },
    { path: 'snackbar', loadComponent: () => import('./snackbar/snackbar.component').then(c => c.SnackbarComponent), data: { breadcrumb: 'Snackbar' } } 
];