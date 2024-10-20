import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { TilesComponent } from '../tiles/tiles.component';
import { InfoCardsComponent } from '../info-cards/info-cards.component';
import { AnalyticsComponent } from '../analytics/analytics.component';
import { AuthService } from '@services/users/auth.service';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { MatIconModule } from '@angular/material/icon';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { PieChartComponent } from '../pie-chart/pie-chart.component';
import { Subject, takeUntil } from 'rxjs';
@Component({
  selector: 'app-fi-dashboard',
  standalone: true,
  imports: [
    ContentHeaderComponent,
    FlexLayoutModule,
    MatCardModule,
    MatProgressBarModule,
    TilesComponent,
    InfoCardsComponent,
    AnalyticsComponent,
    FlexLayoutModule,
    MatCardModule,
    MatIconModule,
    NgxChartsModule,
    PieChartComponent
  ],
  templateUrl: './fi-dashboard.component.html',
  styleUrl: './fi-dashboard.component.scss'
})
export class FiDashboardComponent implements OnInit, OnDestroy {

  partnerName: string = '';
  partnerId: any;
  public gradient = false;
  public autoScale = true;
  @ViewChild('resizedDiv') resizedDiv: ElementRef;
  public previousWidthOfResizedDiv: number = 0;

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

  private unsubscribe$ = new Subject<void>();
  constructor(private authService: AuthService, private dashBoardService: DashboardService){

  }

  ngOnInit() {
    this.partnerName = `${this.authService.currentUser()?.partnerName} Dashboard !`;
    this.partnerId = this.authService.currentUser()?.partnerId;
    this.getLoansDisbursedByGenderSummary();
    this.getLoansDisbursedByPipelineSummary();
    this.getLoansDisbursedByStatusSummary();
    this.getLoanDisbursedByIndustrySectorSummary();
    this.getLoansAccessedVsOutStandingByGenderSummary();
    this.getLoanDisbursedByIndustrySegmentSummary();
  }

  getLoansDisbursedByGenderSummary() {
    this.dashBoardService.getLoansDisbursedByGenderSummary(this.partnerId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoansDisbursedByPipelineSummary() {
    this.dashBoardService.getLoansDisbursedByPipelineSummary(this.partnerId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedByPipeline = response;
        },
        error: (error) => { }
      });
  }

  getLoansDisbursedByStatusSummary() {
    this.dashBoardService.getLoansDisbursedByStatusSummary(this.partnerId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedByStatus = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedByIndustrySectorSummary() {
    this.dashBoardService.getLoanDisbursedByIndustrySectorSummary(this.partnerId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedBySector = response;
        },
        error: (error) => { }
      });
  }

  getLoansAccessedVsOutStandingByGenderSummary() {
    this.dashBoardService.getLoansAccessedVsOutStandingByGenderSummary(this.partnerId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.accessedVSOutStandingAmountByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedByIndustrySegmentSummary() {
    this.dashBoardService.getLoanDisbursedByIndustrySegmentSummary(this.partnerId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedBySegment = response;
        },
        error: (error) => { }
      });
  }


  public onSelect(event: any) {
    console.log(event);
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

}
