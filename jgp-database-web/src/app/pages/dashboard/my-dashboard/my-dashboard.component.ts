import { Component } from '@angular/core';
import { FiDashboardComponent } from '../fi-dashboard/fi-dashboard.component';
import { BmoDashboardComponent } from '../bmo-dashboard/bmo-dashboard.component';
import { AuthService } from '@services/users/auth.service';
import { DashboardComponent } from "../dashboard.component";
import { NoPermissionComponent } from '../../errors/no-permission/no-permission.component';
import { DashboardFiltersComponent } from "../dashboard-filters/dashboard-filters.component";
import { BmoFiDashboardComponent } from '../bmo-fi-dashboard/bmo-fi-dashboard.component';

@Component({
  selector: 'app-my-dashboard',
  standalone: true,
  imports: [
    FiDashboardComponent,
    BmoDashboardComponent,
    BmoFiDashboardComponent,
    DashboardComponent,
    NoPermissionComponent,
    DashboardFiltersComponent
],
  templateUrl: './my-dashboard.component.html',
  styleUrl: './my-dashboard.component.scss'
})
export class MyDashboardComponent {

  partnerType: string | undefined = 'NONE';
  constructor(public authService: AuthService){}

  ngOnInit(): void {
    this.partnerType = this.authService.currentUser()?.partnerType === '-' ? 'NONE' : this.authService.currentUser()?.partnerType;
  }
}
