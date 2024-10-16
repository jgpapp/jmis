import { Routes } from '@angular/router';
import { PagesComponent } from './pages.component';
import { AuthGuard } from '../util/AuthGuard';
import { userRoleResolver } from '../resolvers/user-role.resolver';

export const routes: Routes = [
  {
    path: '',
    component: PagesComponent,
    children: [
      {
        path: '',
        loadComponent: () => import('./dashboard/dashboard.component').then(c => c.DashboardComponent),
        canActivate: [AuthGuard],
        data: { breadcrumb: 'Dashboard' }
      },
      {
        path: 'my-partner-dashboard',
        loadComponent: () => import('./dashboard/my-dashboard/my-dashboard.component').then(c => c.MyDashboardComponent),
        canActivate: [AuthGuard],
        data: { breadcrumb: 'Dashboard' }
      },
      {
        path: 'participants',
        loadChildren: () => import('./clients/client.routes').then(p => p.routes),
        canActivate: [AuthGuard],
        data: { breadcrumb: 'Project Participants' }
      },
      {
        path: 'data-upload',
        loadComponent: () => import('./data/data-uploader/data-uploader.component').then(c => c.DataUploaderComponent),
        canActivate: [AuthGuard],
        data: { breadcrumb: 'Data Upload' }
      },
      {
        path: 'data-list',
        loadComponent: () => import('./data/data-list/data-list.component').then(c => c.DataListComponent),
        canActivate: [AuthGuard],
        data: { breadcrumb: 'Data Management' }
      },
      {
        path: 'lending-data',
        loadComponent: () => import('./lending-data/lending-data.component').then(c => c.LendingDataComponent),
        canActivate: [AuthGuard],
        data: { breadcrumb: 'Lending Data Management' }
      },
      {
        path: 'users',
        loadChildren: () => import('./users/user.routes').then(p => p.routes),
        canActivate: [AuthGuard],
        data: { breadcrumb: 'Users' }
      },
      {
        path: 'user-roles',
        loadChildren: () => import('./user-role/role.routes').then(p => p.routes),
        canActivate: [AuthGuard],
        data: { breadcrumb: 'User Roles' }
      },
      {
        path: 'partners',
        loadChildren: () => import('./partners/partner.routes').then(p => p.routes),
        data: { breadcrumb: 'Partners' }
      },
      {
        path: 'change-password',
        loadComponent: () => import('./change-password/change-password.component').then(c => c.ChangePasswordComponent),
        data: { breadcrumb: 'Change Password' }
      },
      {
        path: 'ui',
        loadChildren: () => import('./ui/ui.routes').then(p => p.routes),
        data: { breadcrumb: 'UI' }
      },
      {
        path: 'dynamic-menu',
        loadComponent: () => import('./dynamic-menu/dynamic-menu.component').then(c => c.DynamicMenuComponent),
        data: { breadcrumb: 'Dynamic Menu' }
      },
      {
        path: 'mailbox',
        loadComponent: () => import('./mailbox/mailbox.component').then(c => c.MailboxComponent),
        data: { breadcrumb: 'Mailbox' }
      },
      {
        path: 'chat',
        loadComponent: () => import('./chat/chat.component').then(c => c.ChatComponent),
        data: { breadcrumb: 'Chat' }
      },
      {
        path: 'form-controls',
        loadChildren: () => import('./form-controls/form-controls.routes').then(p => p.routes),
        data: { breadcrumb: 'Form Controls' }
      },
      {
        path: 'tables',
        loadChildren: () => import('./tables/tables.routes').then(p => p.routes),
        data: { breadcrumb: 'Tables' }
      },
      { 
        path: 'profile', 
        loadChildren: () => import('./profile/profile.routes').then(p => p.routes),
        data: { breadcrumb: 'Profile' } 
      },
      {
        path: 'schedule',
        loadComponent: () => import('./schedule/schedule.component').then(c => c.ScheduleComponent),
        data: { breadcrumb: 'Schedule' }
      },
      {
        path: 'maps',
        loadChildren: () => import('./maps/maps.routes').then(p => p.routes),
        data: { breadcrumb: 'Maps' }
      },
      {
        path: 'charts',
        loadChildren: () => import('./charts/charts.routes').then(p => p.routes),
        data: { breadcrumb: 'Charts' }
      },
      {
        path: 'drag-drop',
        loadComponent: () => import('./drag-drop/drag-drop.component').then(c => c.DragDropComponent),
        data: { breadcrumb: 'Drag & Drop' }
      },
      {
        path: 'icons',
        loadComponent: () => import('./icons/icons.component').then(c => c.IconsComponent),
        data: { breadcrumb: 'Icons' }
      },
      {
        path: 'blank',
        loadComponent: () => import('./blank/blank.component').then(c => c.BlankComponent),
        data: { breadcrumb: 'Blank page' }
      },
      {
        path: 'search',
        loadComponent: () => import('./search/search.component').then(c => c.SearchComponent),
        data: { breadcrumb: 'Search' }
      },
      {
        path: 'search/:name',
        loadComponent: () => import('./search/search.component').then(c => c.SearchComponent),
        data: { breadcrumb: 'Search' }
      }
    ]
  }
];