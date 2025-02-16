import { Component, OnInit } from '@angular/core';
import { AuthService } from '@services/users/auth.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { DashboardFiltersComponent } from '../dashboard-filters/dashboard-filters.component';
import { AnalyticsComponent } from '../analytics/analytics.component';
import { InfoCardsComponent } from '../info-cards/info-cards.component';
import { TilesComponent } from '../tiles/tiles.component';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { FormsModule } from '@angular/forms';
import { PerformanceSummaryComponent } from "../performance-summary/performance-summary.component";

@Component({
  selector: 'app-bmo-fi-dashboard',
  standalone: true,
  imports: [
    ContentHeaderComponent,
    DashboardFiltersComponent,
    FlexLayoutModule,
    MatProgressBarModule,
    MatCardModule,
    TilesComponent,
    InfoCardsComponent,
    AnalyticsComponent,
    MatButtonToggleModule,
    FormsModule,
    PerformanceSummaryComponent
],
  templateUrl: './bmo-fi-dashboard.component.html',
  styleUrl: './bmo-fi-dashboard.component.scss'
})
export class BmoFiDashboardComponent implements OnInit{
 
  selectedDashboardView: string = 'TA';
  dashBoardFilters: any;
  partnerName: string = '';
  resetDashBoardFilters: boolean = false;
  constructor(private authService: AuthService){}

  setDashBoardFilters(currentDashBoardFilters: any){
    this.dashBoardFilters = currentDashBoardFilters;
    this.resetDashBoardFilters = false;
  }

  doResetDashBoardFilters(){
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    this.resetDashBoardFilters = true;
  }

  ngOnInit(): void {
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    this.partnerName = `${this.authService.currentUser()?.partnerName} Dashboard !`;
  }

  isFinancialDashboard(): boolean {
    return 'FI' === this.selectedDashboardView;
  }

  isTADashboard(): boolean {
    return 'TA' === this.selectedDashboardView;
  }
}
