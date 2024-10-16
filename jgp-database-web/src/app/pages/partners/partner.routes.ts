import { Routes } from '@angular/router';
import { PartnersComponent } from './partners.component';
import { EditPartnerComponent } from './edit-partner/edit-partner.component';
import { PartnerDetailsComponent } from './partner-details/partner-details.component';
import { AuthGuard } from '../../util/AuthGuard';
import { PartnerResolver } from '../../resolvers/partner.resolver';
import { CreatePartnerComponent } from './create-partner/create-partner.component';

export const routes: Routes = [
    {
        path: '',
        component: PartnersComponent,
        canActivate: [AuthGuard],
    },
    { 
        path: ':id/details', 
        component: PartnerDetailsComponent, 
        data: { breadcrumb: 'Partner Information' },
        canActivate: [AuthGuard],
        resolve: {selectedPartner: PartnerResolver}
     },
     { 
         path: 'new', 
         component: CreatePartnerComponent, 
         data: { breadcrumb: 'Create Partner' },
         canActivate: [AuthGuard]
      },
      { 
          path: ':id/edit', 
          component: EditPartnerComponent, 
          data: { breadcrumb: 'Edit Partner' },
          canActivate: [AuthGuard],
          resolve: {selectedPartner: PartnerResolver}
       }
];