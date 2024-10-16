import { Routes } from '@angular/router';
import { AuthGuard } from '../../util/AuthGuard';
import { PartnerResolver } from '../../resolvers/partner.resolver';
import { UsersComponent } from './users.component';
import { UserDetailsComponent } from './user-details/user-details.component';
import { CreateUserComponent } from './create-user/create-user.component';
import { EditUserComponent } from './edit-user/edit-user.component';
import { UserResolver } from '../../resolvers/user.resolver';

export const routes: Routes = [
    {
        path: '',
        component: UsersComponent,
        canActivate: [AuthGuard],
    },
    { 
        path: ':id/details', 
        component: UserDetailsComponent, 
        data: { breadcrumb: 'User Information' },
        canActivate: [AuthGuard],
        resolve: {selectedUser: UserResolver}
     },
     { 
         path: 'new', 
         component: CreateUserComponent, 
         data: { breadcrumb: 'Create User' },
         canActivate: [AuthGuard]
      },
      { 
          path: ':id/edit', 
          component: EditUserComponent, 
          data: { breadcrumb: 'Edit User' },
          canActivate: [AuthGuard],
          resolve: {selectedUser: UserResolver}
       }
];