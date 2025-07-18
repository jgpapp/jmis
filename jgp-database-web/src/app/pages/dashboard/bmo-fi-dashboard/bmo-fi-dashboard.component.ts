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
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { GlobalService } from '@services/shared/global.service';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { Subject, takeUntil } from 'rxjs';
import { DashboardService } from '@services/dashboard/dashboard.service';

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
    PerformanceSummaryComponent,
    MatTableModule,
    MatButtonModule,
    MatIconModule
],
  templateUrl: './bmo-fi-dashboard.component.html',
  styleUrl: './bmo-fi-dashboard.component.scss'
})
export class BmoFiDashboardComponent implements OnInit{
 
  selectedDashboardView: string = 'FI';
  dashBoardFilters: any;
  //partnerName: string = '';
  resetDashBoardFilters: boolean = false;

  accessedLoanData: any;
  accessedLoanCountData: any;
  trainedBusinessesCountData: any;
  public accessedLoanDataDataSource: any;
  public accessedLoanCountDataDataSource: any;
  public trainedBusinessesCountDataDataSource: any;
  public displayedColumns = ['year', 'partnerName', 'genderName', 'value' ];
  refugeesAndPlwdtrainedBusinessesCountDataPerGenderSource: any;
  public refugeesAndPlwdDisplayedColumnsTrainedPerGender = ['name', 'value'];
  private unsubscribe$ = new Subject<void>();
  partnerId: any;

  trainedBusinessesCountDataPerTaType: any;
  trainedBusinessesCountDataPerTaTypeSource: any;
  public displayedColumnsTrainedPerTaType = ['partnerName', 'taType', 'genderCategory', 'businessesTrained' ];

  constructor(private authService: AuthService, public gs: GlobalService, private dashBoardService: DashboardService){}

  setDashBoardFilters(currentDashBoardFilters: any){
    this.dashBoardFilters = currentDashBoardFilters;
    this.dashBoardFilters['selectedPartnerId'] = this.partnerId;
    this.resetDashBoardFilters = false;
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
    this.getTaTypeTrainedBusinesses();
    this.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary();
  }

  doResetDashBoardFilters(){
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    this.resetDashBoardFilters = true;
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
    this.getTaTypeTrainedBusinesses();
    this.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary();
  }

  ngOnInit(): void {
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    //this.partnerName = `${this.authService.currentUser()?.partnerName} Dashboard !`;
    this.partnerId = this.authService.currentUser()?.partnerId;
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
    this.getTaTypeTrainedBusinesses();
    this.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary();
  }


  getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary() {
    this.dashBoardService.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.refugeesAndPlwdtrainedBusinessesCountDataPerGenderSource = response;
        },
        error: (error) => { }
      });
  }

  getTaTypeTrainedBusinesses() {
    this.dashBoardService.getTaTypeTrainedBusinesses(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.trainedBusinessesCountDataPerTaType = response;
          this.trainedBusinessesCountDataPerTaTypeSource = new MatTableDataSource(this.trainedBusinessesCountDataPerTaType);
        },
        error: (error) => { }
      });
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


  isFinancialDashboard(): boolean {
    return 'FI' === this.selectedDashboardView;
  }

  isTADashboard(): boolean {
    return 'TA' === this.selectedDashboardView;
  }

  isMentorShipDashboard(): boolean {
    return 'MENTOR' === this.selectedDashboardView;
  }

  get filterFieldFlex(): number {
    return this.isFinancialDashboard() ? 33.3 : 25;
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
