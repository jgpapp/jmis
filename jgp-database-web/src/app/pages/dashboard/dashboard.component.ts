import { Component, ViewChild } from '@angular/core';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { MatCardModule } from '@angular/material/card';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { TilesComponent } from './tiles/tiles.component';
import { InfoCardsComponent } from './info-cards/info-cards.component';
import { AnalyticsComponent } from './analytics/analytics.component';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { DashboardFiltersComponent } from './dashboard-filters/dashboard-filters.component';
import { MatPaginator, MatPaginatorModule } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { Subject, takeUntil } from 'rxjs';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { GlobalService } from '@services/shared/global.service';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { FormsModule } from '@angular/forms';
import {MatTreeFlatDataSource, MatTreeFlattener} from '@angular/material/tree';
import {FlatTreeControl} from '@angular/cdk/tree';
import { PerformanceSummaryComponent } from "./performance-summary/performance-summary.component";

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [
    MatTableModule,
    MatPaginatorModule,
    ContentHeaderComponent,
    FlexLayoutModule,
    MatCardModule,
    MatProgressBarModule,
    TilesComponent,
    InfoCardsComponent,
    AnalyticsComponent,
    NoPermissionComponent,
    DashboardFiltersComponent,
    MatButtonModule,
    MatIconModule,
    MatButtonToggleModule,
    FormsModule,
    PerformanceSummaryComponent
],
  templateUrl: './dashboard.component.html',
  styleUrl: './dashboard.component.scss'
})
export class DashboardComponent {

  selectedDashboardView: string = 'FI';
  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort!: MatSort;
  public displayedColumns = ['year', 'partnerName', 'genderName', 'value' ];

  accessedLoanData: any;
  accessedLoanCountData: any;
  trainedBusinessesCountData: any;
  public accessedLoanDataDataSource: any;
  public accessedLoanCountDataDataSource: any;
  public trainedBusinessesCountDataDataSource: any;
  private unsubscribe$ = new Subject<void>();


  dashBoardFilters: any;
  resetDashBoardFilters: boolean = false;
  constructor(private dashBoardService: DashboardService, public authService: AuthService, public gs: GlobalService){}

  

  setDashBoardFilters(currentDashBoardFilters: any){
    this.dashBoardFilters = currentDashBoardFilters;
    this.resetDashBoardFilters = false;
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
  }

  doResetDashBoardFilters(){
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    this.resetDashBoardFilters = true;
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
  }

  getLastThreeYearsAccessedLoanPerPartnerYearly() {
    this.dashBoardService.getLastThreeYearsAccessedLoanPerPartnerYearly(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.accessedLoanData = response;
          this.accessedLoanDataDataSource = new MatTableDataSource(this.accessedLoanData);
        },
        error: (error) => { }
      });
  }

  getLastThreeYearsAccessedLoansCountPerPartnerYearly() {
    this.dashBoardService.getLastThreeYearsAccessedLoansCountPerPartnerYearly(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.accessedLoanCountData = response;
          this.accessedLoanCountDataDataSource = new MatTableDataSource(this.accessedLoanCountData);
        },
        error: (error) => { }
      });
  }

  getLastThreeYearsTrainedBusinessesPerPartnerYearly() {
    this.dashBoardService.getLastThreeYearsTrainedBusinessesPerPartnerYearly(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.trainedBusinessesCountData = response;
          this.trainedBusinessesCountDataDataSource = new MatTableDataSource(this.trainedBusinessesCountData);
        },
        error: (error) => { }
      });
  }


  shouldDisplayAccessedLoanDataPartnerName(index: number): boolean {
    const data = this.accessedLoanDataDataSource.data; // Access the current data
    if (index === 0) {
      return true;
    }
    return data[index].partnerName !== data[index - 1].partnerName;
  }

  shouldDisplayAccessedLoanDataYear(index: number): boolean {
    const data = this.accessedLoanDataDataSource.data; // Access the current data
    if (index === 0) {
      return true;
    }
    return data[index].year !== data[index - 1].year;
  }

  shouldDisplayAccessedLoanCountDataPartnerName(index: number): boolean {
    const data = this.accessedLoanCountDataDataSource.data; // Access the current data
    if (index === 0) {
      return true;
    }
    return data[index].partnerName !== data[index - 1].partnerName;
  }

  shouldDisplayAccessedLoanCountDataYear(index: number): boolean {
    const data = this.accessedLoanCountDataDataSource.data; // Access the current data
    if (index === 0) {
      return true;
    }
    return data[index].year !== data[index - 1].year;
  }

  shouldDisplayTrainedBusinessesCountDataPartnerName(index: number): boolean {
    const data = this.trainedBusinessesCountDataDataSource.data; // Access the current data
    if (index === 0) {
      return true;
    }
    return data[index].partnerName !== data[index - 1].partnerName;
  }

  shouldDisplayTrainedBusinessesCountDataYear(index: number): boolean {
    const data = this.trainedBusinessesCountDataDataSource.data; // Access the current data
    if (index === 0) {
      return true;
    }
    return data[index].year !== data[index - 1].year;
  }

  ngOnInit(): void {
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
  }


  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

  isFinancialDashboard(): boolean {
    return 'FI' === this.selectedDashboardView;
  }

  isTADashboard(): boolean {
    return 'TA' === this.selectedDashboardView;
  }
}
