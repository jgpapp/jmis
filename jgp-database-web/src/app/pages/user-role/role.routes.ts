import { Routes } from '@angular/router';
import { AuthGuard } from '../../util/AuthGuard';
import { UserRoleComponent } from './user-role.component';
import { UserRoleDetailsComponent } from './user-role-details/user-role-details.component';
import { CreateRoleComponent } from './create-role/create-role.component';
import { EditRoleComponent } from './edit-role/edit-role.component';
import { userRoleResolver } from '../../resolvers/user-role.resolver';

export const routes: Routes = [
    {
        path: '',
        component: UserRoleComponent,
        canActivate: [AuthGuard],
    },
    { 
        path: ':id/details', 
        component: UserRoleDetailsComponent, 
        data: { breadcrumb: 'User Role Information' },
        canActivate: [AuthGuard],
        resolve: {selectedUserRole: userRoleResolver}
     },
     { 
         path: 'new', 
         component: CreateRoleComponent, 
         data: { breadcrumb: 'Create User Role' },
         canActivate: [AuthGuard]
      },
      { 
          path: ':id/edit', 
          component: EditRoleComponent, 
          data: { breadcrumb: 'Edit User Role' },
          canActivate: [AuthGuard],
          resolve: {selectedUserRole: userRoleResolver}
       }
];