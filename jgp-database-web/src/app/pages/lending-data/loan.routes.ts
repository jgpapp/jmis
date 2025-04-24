import { Routes } from '@angular/router';
import { AuthGuard } from '../../util/AuthGuard';
import { LendingDataComponent } from './lending-data.component';
import { LoanDetailsComponent } from './loan-details/loan-details.component';
import { LoanResolver } from '../../resolvers/Loan.resolver';

export const routes: Routes = [
    {
        path: '',
        component: LendingDataComponent,
        canActivate: [AuthGuard],
    },
    { 
        path: ':id/details', 
        component: LoanDetailsComponent, 
        data: { breadcrumb: 'Loan Information' },
        canActivate: [AuthGuard],
        resolve: {selectedLoan: LoanResolver}
     }
];