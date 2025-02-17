import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { AuthService } from '@services/users/auth.service';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { MatIconModule } from '@angular/material/icon';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { PieChartComponent } from '../pie-chart/pie-chart.component';
import { Subject, takeUntil } from 'rxjs';
import { DashboardFiltersComponent } from "../dashboard-filters/dashboard-filters.component";
import { HighLevelSummaryDto } from '../dto/highLevelSummaryDto';
import { PerformanceSummaryComponent } from "../performance-summary/performance-summary.component";
import { GlobalService } from '@services/shared/global.service';
import { MatButtonModule } from '@angular/material/button';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
@Component({
  selector: 'app-fi-dashboard',
  standalone: true,
  imports: [
    ContentHeaderComponent,
    FlexLayoutModule,
    MatCardModule,
    MatProgressBarModule,
    FlexLayoutModule,
    MatCardModule,
    MatIconModule,
    NgxChartsModule,
    PieChartComponent,
    DashboardFiltersComponent,
    PerformanceSummaryComponent,
    MatButtonModule,
    MatTableModule
],
  templateUrl: './fi-dashboard.component.html',
  styleUrl: './fi-dashboard.component.scss'
})
export class FiDashboardComponent implements OnInit, OnDestroy {

  dashBoardFilters: any;
  partnerName: string = '';
  partnerId: any;
  public gradient = false;
  public autoScale = true;
  @ViewChild('resizedDiv') resizedDiv: ElementRef;
  public previousWidthOfResizedDiv: number = 0;

  resetDashBoardFilters: boolean = false;

  selectedDashboardView: string = 'FI';
  accessedLoanData: any;
  accessedLoanCountData: any;
  public accessedLoanDataDataSource: any;
  public accessedLoanCountDataDataSource: any;
  public displayedColumns = ['year', 'partnerName', 'genderName', 'value' ];

  public loansDisbursedByGender: any[];
  public loansDisbursedByGenderShowLegend: boolean = false;
  public chartSColorScheme: any = {
    domain: ['#2F3E9E', '#D22E2E', '#378D3B', '#7f7f7f', '#c4a678', '#6a7b6a', '#191919', '#3d144c', '#f0e1dc', '#a04324', '#00ffff', '#0e5600', '#0e9697']
  };
  public loansDisbursedByGenderShowLabels: boolean = true;
  public loansDisbursedByGenderExplodeSlices: boolean = false;
  public loansDisbursedByGenderDoughnut: boolean = true;
  public loansDisbursedByGenderChartTitle: string = 'Loan Disbursed by Gender';


  public loansDisbursedByPipeline: any[];
  public loansDisbursedByPipelineShowLegend: boolean = false;
  public loansDisbursedByPipelineShowLabels: boolean = true;
  public loansDisbursedByPipelineExplodeSlices: boolean = false;
  public loansDisbursedByPipelineDoughnut: boolean = false;
  public loansDisbursedByPipelineChartTitle: string = 'Loan Disbursed by Pipeline Source';

  public loansDisbursedByStatus: any[];
  public loansDisbursedByStatusShowXAxis: boolean = true;
  public loansDisbursedByStatusShowYAxis: boolean = true;
  public loansDisbursedByStatusShowLegend: boolean = false;
  public loansDisbursedByStatusShowXAxisLabel: boolean = true;
  public loansDisbursedByStatusShowYAxisLabel: boolean = true;
  public loansDisbursedByStatusXAxisLabel: string = 'Status';
  public loansDisbursedByStatusYAxisLabel: string = 'Amount Disbursed';
  public loansDisbursedByStatusChartTitle: string = 'Loan Disbursed by Pipeline Source';

  public loansDisbursedBySector: any[];
  public loansDisbursedBySectorShowLegend: boolean = false;
  public loansDisbursedBySectorShowLabels: boolean = true;
  public loansDisbursedBySectorExplodeSlices: boolean = false;
  public loansDisbursedBySectorDoughnut: boolean = true;
  public loansDisbursedBySectorChartTitle: string = 'Loan Disbursed by Industry Sector';

  public accessedVSOutStandingAmountByGender: any[]
  public accessedVSOutStandingAmountByGenderShowXAxis = true;
  public accessedVSOutStandingAmountByGenderShowYAxis = true;
  public accessedVSOutStandingAmountByGenderShowLegend = false;
  public accessedVSOutStandingAmountByGenderShowXAxisLabel = true;
  public accessedVSOutStandingAmountByGenderXAxisLabel = 'Partners';
  public accessedVSOutStandingAmountByGenderShowYAxisLabel = true;
  public accessedVSOutStandingAmountByGenderYAxisLabel = 'Amount';
  public accessedVSOutStandingAmountByGenderChartTitle: string = 'Accessed Vs OutStanding By Gender';

  public loansDisbursedBySegment: any[];
  public loansDisbursedBySegmentShowLegend: boolean = false;
  public loansDisbursedBySegmentShowLabels: boolean = true;
  public loansDisbursedBySegmentExplodeSlices: boolean = false;
  public loansDisbursedBySegmentDoughnut: boolean = false;
  public loansDisbursedBySegmentChartTitle: string = 'Disbursed By Business Segment';

  public topFourCountiesloansDisbursed: any[];
  public topFourCountiesloansDisbursedChartTitle: string = 'Loan Disbursed Top Four Counties';

  highLevelSummary: HighLevelSummaryDto = {businessesTrained: '0', businessesLoaned: '0', amountDisbursed: '0', outStandingAmount: '0'}

  private unsubscribe$ = new Subject<void>();
  constructor(private authService: AuthService, private dashBoardService: DashboardService, public gs: GlobalService){

  }

  setDashBoardFilters(currentDashBoardFilters: any){
    this.dashBoardFilters = currentDashBoardFilters;
    this.resetDashBoardFilters = false;
    this.reloadData();
  }

  doResetDashBoardFilters(){
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    this.resetDashBoardFilters = true;
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
  }

  ngOnInit() {
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    this.reloadData();
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


  reloadData(){
    this.partnerName = `${this.authService.currentUser()?.partnerName} Dashboard !`;
    this.partnerId = this.authService.currentUser()?.partnerId;
    this.getHighLevelSummary();
    this.getLoansDisbursedByGenderSummary();
    this.getLoansDisbursedByPipelineSummary();
    this.getLoansDisbursedByStatusSummary();
    this.getLoanDisbursedByIndustrySectorSummary();
    this.getLoansAccessedVsOutStandingByGenderSummary();
    this.getLoanDisbursedByIndustrySegmentSummary();
    this.getLoanDisbursedTopFourCountiesSummary();
    this.getLastThreeYearsAccessedLoanPerPartnerYearly();
    this.getLastThreeYearsAccessedLoansCountPerPartnerYearly();
  }

  getHighLevelSummary() {
    this.dashBoardService.getHighLevelSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.highLevelSummary = this.dashBoardService.getFormattedTileData(response);
        },
        error: (error) => { }
      });
  }


  getLoansDisbursedByGenderSummary() {
    this.dashBoardService.getLoansDisbursedByGenderSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoansDisbursedByPipelineSummary() {
    this.dashBoardService.getLoansDisbursedByPipelineSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedByPipeline = response;
        },
        error: (error) => { }
      });
  }

  getLoansDisbursedByStatusSummary() {
    this.dashBoardService.getLoansDisbursedByStatusSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedByStatus = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedByIndustrySectorSummary() {
    this.dashBoardService.getLoanDisbursedByIndustrySectorSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedBySector = response;
        },
        error: (error) => { }
      });
  }

  getLoansAccessedVsOutStandingByGenderSummary() {
    this.dashBoardService.getLoansAccessedVsOutStandingByGenderSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.accessedVSOutStandingAmountByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedByIndustrySegmentSummary() {
    this.dashBoardService.getLoanDisbursedByIndustrySegmentSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedBySegment = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedTopFourCountiesSummary() {
    this.dashBoardService.getLoanDisbursedTopFourCountiesSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.topFourCountiesloansDisbursed = response;
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

  isFinancialDashboard(): boolean {
    return 'FI' === this.selectedDashboardView;
  }



  public onSelect(event: any) {
    console.log(event);
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

}
