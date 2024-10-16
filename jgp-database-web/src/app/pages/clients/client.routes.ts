import { Routes } from '@angular/router';
import { AuthGuard } from '../../util/AuthGuard';
import { ClientsComponent } from './clients.component';
import { ClientDetailsComponent } from './client-details/client-details.component';
import { ParticipantResolver } from '../../resolvers/participant.resolver';

export const routes: Routes = [
    {
        path: '',
        component: ClientsComponent,
        canActivate: [AuthGuard],
    },
    { 
        path: ':id/details', 
        component: ClientDetailsComponent, 
        data: { breadcrumb: 'Participant Information' },
        canActivate: [AuthGuard],
        resolve: {selectedParticipant: ParticipantResolver}
     }
];