import { Component, OnInit, ViewChild } from '@angular/core';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { MatCardModule } from '@angular/material/card';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { TilesComponent } from './tiles/tiles.component';
import { InfoCardsComponent } from './info-cards/info-cards.component';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { DashboardFiltersComponent } from './dashboard-filters/dashboard-filters.component';
import { MatPaginator, MatPaginatorModule } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { GlobalService } from '@services/shared/global.service';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { FormsModule } from '@angular/forms';
import { PerformanceSummaryComponent } from "./performance-summary/performance-summary.component";
import { MonitoringDashboardComponent } from './monitoring-dashboard/monitoring-dashboard.component';
import { ActivatedRoute, Router } from '@angular/router';
import { DashboardTypeFilter } from '../../dto/dashboard-type-filter';
import { AnalyticsComponent } from './analytics/analytics.component';
import { SubscriptionsContainer } from '../../theme/utils/subscriptions-container';

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
        NoPermissionComponent,
        DashboardFiltersComponent,
        MatButtonModule,
        MatIconModule,
        MatButtonToggleModule,
        FormsModule,
        PerformanceSummaryComponent,
        MonitoringDashboardComponent,
        AnalyticsComponent
    ],
    templateUrl: './dashboard.component.html',
    styleUrl: './dashboard.component.scss'
})
export class DashboardComponent implements OnInit {

  private _selectedDashboardView: string = 'GE';
  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort!: MatSort;
  public displayedColumns = ['year', 'partnerName', 'genderName', 'value' ];
  public displayedColumnsTrainedPerTaType = ['partnerName', 'taType', 'genderCategory', 'businessesTrained' ];

  accessedLoanData: any;
  accessedLoanCountData: any;
  trainedBusinessesCountData: any;
  trainedBusinessesCountDataPerTaType: any;
  trainedBusinessesCountDataPerTaTypeSource: any;
  refugeesAndPlwdtrainedBusinessesCountDataPerGenderSource: any;
  public refugeesAndPlwdDisplayedColumnsTrainedPerGender = ['name', 'value'];
  public accessedLoanDataDataSource: any;
  public accessedLoanCountDataDataSource: any;
  public trainedBusinessesCountDataDataSource: any;
  subs = new SubscriptionsContainer();
  isPartnerDashboard: boolean = false;
  currentUserPartnerId: any;
  partnerType: string | undefined = 'NONE';


  dashBoardFilters: any;
  partnerSpecificDashBoardFilters: any;
  resetDashBoardFilters: boolean = false;
  currentDashBoardTypeFilters: DashboardTypeFilter = {};
  constructor(private dashBoardService: DashboardService, 
    public authService: AuthService, 
    public gs: GlobalService, 
    private route: ActivatedRoute,
    public router: Router){
     
    }

  

  setDashBoardFilters(currentDashBoardFilters: any){
    this.dashBoardFilters = currentDashBoardFilters;
    this.partnerSpecificDashBoardFilters = currentDashBoardFilters;
    if(this.currentDashBoardTypeFilters.isPartnerDashboard){
      this.dashBoardFilters['selectedPartnerId'] = this.currentUserPartnerId;
    }
    this.resetDashBoardFilters = false;
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
    this.getTaTypeTrainedBusinesses();
    this.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary();
  }

  doResetDashBoardFilters(){
    this.dashBoardFilters = {};
    if(this.currentDashBoardTypeFilters.isPartnerDashboard){
      this.dashBoardFilters = {'selectedPartnerId': this.currentUserPartnerId}
    }
    this.partnerSpecificDashBoardFilters = {'selectedPartnerId': this.currentUserPartnerId};
    this.resetDashBoardFilters = true;
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
    this.getTaTypeTrainedBusinesses();
    this.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary();
  }

  getLastThreeYearsAccessedLoanPerPartnerYearly() {
    this.subs.add = this.dashBoardService.getLastThreeYearsAccessedLoanPerPartnerYearly(this.partnerSpecificDashBoardFilters)
      .subscribe({
        next: (response) => {
          this.accessedLoanData = response;
          this.accessedLoanDataDataSource = new MatTableDataSource(this.accessedLoanData);
        },
        error: (error) => { }
      });
  }

  getLastThreeYearsAccessedLoansCountPerPartnerYearly() {
    this.subs.add = this.dashBoardService.getLastThreeYearsAccessedLoansCountPerPartnerYearly(this.partnerSpecificDashBoardFilters)
      .subscribe({
        next: (response) => {
          this.accessedLoanCountData = response;
          this.accessedLoanCountDataDataSource = new MatTableDataSource(this.accessedLoanCountData);
        },
        error: (error) => { }
      });
  }

  getLastThreeYearsTrainedBusinessesPerPartnerYearly() {
    this.subs.add = this.dashBoardService.getLastThreeYearsTrainedBusinessesPerPartnerYearly(this.partnerSpecificDashBoardFilters)
      .subscribe({
        next: (response) => {
          this.trainedBusinessesCountData = response;
          this.trainedBusinessesCountDataDataSource = new MatTableDataSource(this.trainedBusinessesCountData);
        },
        error: (error) => { }
      });
  }

  getTaTypeTrainedBusinesses() {
    this.subs.add = this.dashBoardService.getTaTypeTrainedBusinesses(this.partnerSpecificDashBoardFilters)
      .subscribe({
        next: (response) => {
          this.trainedBusinessesCountDataPerTaType = response;
          this.trainedBusinessesCountDataPerTaTypeSource = new MatTableDataSource(this.trainedBusinessesCountDataPerTaType);
        },
        error: (error) => { }
      });
  }

  getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary() {
    this.subs.add = this.dashBoardService.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary(this.partnerSpecificDashBoardFilters)
      .subscribe({
        next: (response) => {
          this.refugeesAndPlwdtrainedBusinessesCountDataPerGenderSource = response;
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

  shouldDisplayPartnerTrainedBusinessesCountPerTaType(index: number): boolean {
    const data = this.trainedBusinessesCountDataPerTaTypeSource.data; // Access the current data
    if (index === 0) {
      return true;
    }
    return data[index].partnerName !== data[index - 1].partnerName;
  }

  shouldDisplayTaTypeTrainedBusinessesCountPerTaType(index: number): boolean {
    const data = this.trainedBusinessesCountDataPerTaTypeSource.data; // Access the current data
    if (index === 0) {
      return true;
    }
    return data[index].taType !== data[index - 1].taType;
  }

  get selectedDashboardView(): string {
  return this._selectedDashboardView;
  }

  set selectedDashboardView(value: string) {
    this._selectedDashboardView = value;
    this.updateCurrentDashBoardTypeFilters();
  }

  updateCurrentDashBoardTypeFilters() {
  this.currentDashBoardTypeFilters = {
    isPartnerDashboard: this.isPartnerDashboard,
    currentUserPartnerId: this.currentUserPartnerId,
    partnerType: this.partnerType,
    isGeneralSummaryDashBoard: 'GE' === this.selectedDashboardView,
    isFinancialDashboard: 'FI' === this.selectedDashboardView,
    isTADashboard: 'TA' === this.selectedDashboardView,
    isMentorShipDashboard: 'MENTOR' === this.selectedDashboardView,
    isMonitoringDashBoard: 'MONITOR' === this.selectedDashboardView
  };
}

  ngOnInit(): void {
    this.isPartnerDashboard = this.route.snapshot.data['isPartnerDashboard'];
    const currentUser = this.authService.currentUser();
    this.currentUserPartnerId = currentUser?.partnerId;
    this.partnerType = currentUser?.partnerType === '-' ? 'NONE' : currentUser?.partnerType;
    this.dashBoardFilters = {};
   
    if(this.currentDashBoardTypeFilters.isPartnerDashboard){
      this.dashBoardFilters = {'selectedPartnerId': this.currentUserPartnerId}
    }
    this.partnerSpecificDashBoardFilters = {'selectedPartnerId': this.currentUserPartnerId};
    // Set initial dashboard type filters
    if ((this.partnerType === 'FI' || this.partnerType === 'FI_TA') && this.isPartnerDashboard) {
      this._selectedDashboardView = 'FI';
    }

    if (this.partnerType === 'TA' && this.isPartnerDashboard) {
      this._selectedDashboardView = 'TA';
    }
    this.updateCurrentDashBoardTypeFilters();


    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
    this.getTaTypeTrainedBusinesses();
    this.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary();
  }


  ngOnDestroy(): void {
    this.subs.dispose();
  }

  showGeneralDashboardSelector(): boolean {
    return !this.currentDashBoardTypeFilters.isPartnerDashboard;
  }

  showFinancialDashboardSelector(): boolean {
    return !this.currentDashBoardTypeFilters.isPartnerDashboard || 'FI' === this.partnerType || 'NONE' === this.partnerType || 'FI_TA' === this.partnerType;
  }

  showTADashboardSelector(): boolean {
    return !this.currentDashBoardTypeFilters.isPartnerDashboard || 'TA' === this.partnerType || 'NONE' === this.partnerType || 'FI_TA' === this.partnerType;
  }

  showMentorShipDashboardSelector(): boolean {
    return true;
  }
  showMonitoringDashBoardSelector(): boolean {
    return !this.currentDashBoardTypeFilters.isPartnerDashboard;
  }

  get filterFieldFlex(): number {
    if(this.currentDashBoardTypeFilters.isFinancialDashboard){
      if(this.currentDashBoardTypeFilters.isPartnerDashboard){
        return 33.3;
      }
      if(this.authService.hasPermission('DASHBOARD_VIEW_WITH_PARTNER_FILTER')){
        return 25;
      } else {
        return 33.3;
      }
    } else if(this.currentDashBoardTypeFilters.isTADashboard){
      if(this.currentDashBoardTypeFilters.isPartnerDashboard){
        return 25;
      }
      if(this.authService.hasPermission('DASHBOARD_VIEW_WITH_PARTNER_FILTER')){
        return 20;
      } else {
        return 25;
      }
    } else if(this.currentDashBoardTypeFilters.isMonitoringDashBoard){
        return 100/7; // 14.28
    } else if(this.currentDashBoardTypeFilters.isMentorShipDashboard){
      if(this.currentDashBoardTypeFilters.isPartnerDashboard){
        return 33.3;
      }
      return 25;
    }else if(this.currentDashBoardTypeFilters.isGeneralSummaryDashBoard){
        return 33.3; 
    }else {
      return 20;
    }
  }
}
