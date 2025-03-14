import { Routes } from '@angular/router';
import { AuthGuard } from '../../util/AuthGuard';
import { ClientsComponent } from './clients.component';
import { ClientDetailsComponent } from './client-details/client-details.component';
import { ParticipantResolver } from '../../resolvers/participant.resolver';
import { EditParticipantComponent } from './edit-participant/edit-participant.component';

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
     },
    { 
        path: ':id/edit', 
        component: EditParticipantComponent, 
        data: { breadcrumb: 'Edit Participant' },
        canActivate: [AuthGuard],
        resolve: {selectedParticipant: ParticipantResolver}
    }
];