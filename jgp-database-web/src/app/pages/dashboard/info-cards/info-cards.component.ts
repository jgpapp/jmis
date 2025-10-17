import { AfterViewChecked, Component, ElementRef, Input, OnChanges, OnDestroy, OnInit, SimpleChanges, ViewChild } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { PieChartComponent } from "../pie-chart/pie-chart.component"; 
import { multi, single } from '@data/charts.data';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { DashboardTypeFilter } from '../../../dto/dashboard-type-filter';
import { CountySummaryDto } from '../dto/county-summary-dto';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';

@Component({
    selector: 'app-info-cards',
    imports: [
    FlexLayoutModule,
    MatCardModule,
    MatIconModule,
    NgxChartsModule,
    PieChartComponent,
    MatDialogModule
],
    templateUrl: './info-cards.component.html',
    styleUrl: './info-cards.component.scss'
})
export class InfoCardsComponent implements OnInit, AfterViewChecked, OnChanges, OnDestroy {
  @Input('dashBoardFilters') dashBoardFilters: any;
  @Input({required: true, alias: 'dashboardTypeFilter'}) dashboardTypeFilter: DashboardTypeFilter;
  public displayChart: boolean = false;
  public colorScheme: any = {
    domain: ['rgba(255,255,255,0.8)']
  };
  public autoScale = true;
  @ViewChild('resizedDiv') resizedDiv: ElementRef;
  public previousWidthOfResizedDiv: number = 0;
  public chartSColorScheme: any = {
    domain: [
      '#FF671B',
      '#8DB92E',
      '#4FCDB0',
      '#DE3C95',
      '#F38B00',
      '#2F7B6B',
      '#D22A2F',
      '#FFC81F',
      '#2F3E9E', 
      '#D22E2E', 
      '#378D3B', 
      '#7f7f7f', 
      '#c4a678', 
      '#6a7b6a', 
      '#191919', 
      '#3d144c', 
      '#f0e1dc', 
      '#a04324', 
      '#00ffff', 
      '#0e5600', 
      '#0e9697'
    ]
  };

  public loansDisbursedByGender: any[];
  public loansDisbursedByGenderShowLegend: boolean = false;
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
  public loansDisbursedByPipelineYAxisLabel: string = 'Pipeline Source';
  public loansDisbursedByPipelineXAxisLabel: string = 'Amount Disbursed';
  public loansDisbursedByPipelineShowXAxis: boolean = true;
  public loansDisbursedByPipelineShowYAxis: boolean = true;
  public loansDisbursedByPipelineShowXAxisLabel: boolean = true;
  public loansDisbursedByPipelineShowYAxisLabel: boolean = true;



  public mentorshipGenderSummary: any[];
  public mentorshipGenderSummaryShowLegend: boolean = false;
  public mentorshipGenderSummaryShowLabels: boolean = true;
  public mentorshipGenderSummaryExplodeSlices: boolean = false;
  public mentorshipGenderSummaryDoughnut: boolean = false;
  public mentorshipGenderSummaryChartTitle: string = 'Gender Distribution';

  public loansDisbursedByStatus: any[];
  public loansDisbursedByStatusShowXAxis: boolean = true;
  public loansDisbursedByStatusShowYAxis: boolean = true;
  public loansDisbursedByStatusShowLegend: boolean = false;
  public loansDisbursedByStatusShowXAxisLabel: boolean = true;
  public loansDisbursedByStatusShowYAxisLabel: boolean = true;
  public loansDisbursedByStatusXAxisLabel: string = 'Status';
  public loansDisbursedByStatusYAxisLabel: string = 'Amount Disbursed';
  public loansDisbursedByStatusChartTitle: string = 'Loans Disbursed By Status';

  public businessesTainedByGender: any[];
  public businessesTainedByGenderShowLegend: boolean = false;
  public businessesTainedByGenderShowLabels: boolean = true;
  public businessesTainedByGenderExplodeSlices: boolean = false;
  public businessesTainedByGenderDoughnut: boolean = true;
  public businessesTainedByGenderChartTitle: string = 'Business Trained By Gender';

  public mentorshipByGenderCategory: any[];
  public mentorshipByGenderCategoryShowLegend: boolean = false;
  public mentorshipByGenderCategoryShowLabels: boolean = true;
  public mentorshipByGenderCategoryExplodeSlices: boolean = false;
  public mentorshipByGenderCategoryDoughnut: boolean = true;
  public mentorshipByGenderCategoryChartTitle: string = 'Gender Category Distribution';

  public loanedBusinessesByGender: any[];
  public loanedBusinessesByGenderShowLegend: boolean = false;
  public loanedBusinessesByGenderShowLabels: boolean = true;
  public loanedBusinessesByGenderExplodeSlices: boolean = false;
  public loanedBusinessesByGenderDoughnut: boolean = true;
  public loanedBusinessesByGenderChartTitle: string = 'Business Loaned By Gender';

  public gradient = false;

  public single: any[];
  public multi: any[];
  public TANeedsByGender: any[]
  public TANeedsByGenderShowXAxis = true;
  public TANeedsByGenderShowYAxis = true;
  public TANeedsByGenderShowLegend = false;
  public TANeedsByGenderShowXAxisLabel = true;
  public TANeedsByGenderXAxisLabel = 'TA Needs';
  public TANeedsByGenderShowYAxisLabel = true;
  public TANeedsByGenderYAxisLabel = 'Number Of Participants';
  public TANeedsByGenderChartTitle: string = 'TA Needs By Gender';

 
  public trainingByPartnerByGender: any[]
  public trainingByPartnerByGenderShowXAxis = true;
  public trainingByPartnerByGenderShowYAxis = true;
  public trainingByPartnerByGenderShowLegend = false;
  public trainingByPartnerByGenderShowXAxisLabel = true;
  public trainingByPartnerByGenderXAxisLabel = 'Partners';
  public trainingByPartnerByGenderShowYAxisLabel = true;
  public trainingByPartnerByGenderYAxisLabel = 'Number';
  public trainingByPartnerByGenderChartTitle: string = 'Training By Partner By Gender';

  public loanDisbursedByProductByGender: any[]
  public loanDisbursedByProductByGenderShowXAxis = true;
  public loanDisbursedByProductByGenderShowYAxis = true;
  public loanDisbursedByProductByGenderShowLegend = false;
  public loanDisbursedByProductByGenderShowXAxisLabel = true;
  public loanDisbursedByProductByGenderXAxisLabel = 'Loan Products';
  public loanDisbursedByProductByGenderShowYAxisLabel = true;
  public loanDisbursedByProductByGenderYAxisLabel = 'Amount Disbursed';
  public loanDisbursedByProductByGenderChartTitle: string = 'Disbursed By Loan Product By Gender';


  public taTrainedBySector: any[];
  public taTrainedBySectorShowXAxis: boolean = true;
  public taTrainedBySectorShowYAxis: boolean = true;
  public taTrainedBySectorShowLegend: boolean = false;
  public taTrainedBySectorShowXAxisLabel: boolean = true;
  public taTrainedBySectorShowYAxisLabel: boolean = true;
  public taTrainedBySectorXAxisLabel: string = 'Industry Sectors';
  public taTrainedBySectorYAxisLabel: string = 'Number Of Participants';
  public taTrainedBySectorChartTitle: string = 'TA Training By Industry Sector';

  public loansDisbursedByProduct: any[];
  public loansDisbursedByProductShowXAxis: boolean = true;
  public loansDisbursedByProductShowYAxis: boolean = true;
  public loansDisbursedByProductShowLegend: boolean = false;
  public loansDisbursedByProductShowXAxisLabel: boolean = true;
  public loansDisbursedByProductShowYAxisLabel: boolean = true;
  public loansDisbursedByProductXAxisLabel: string = 'Loan Products';
  public loansDisbursedByProductYAxisLabel: string = 'Amount Disbursed';
  public loansDisbursedByProductChartTitle: string = 'Loan Disbursed By Loan Product';

  public employeesSummary: any[];
  public employeesSummaryShowXAxis: boolean = true;
  public employeesSummaryShowYAxis: boolean = true;
  public employeesSummaryShowLegend: boolean = false;
  public employeesSummaryShowXAxisLabel: boolean = true;
  public employeesSummaryShowYAxisLabel: boolean = true;
  public employeesSummaryXAxisLabel: string = 'Category';
  public employeesSummaryYAxisLabel: string = 'Number Of Employees';
  public employeesSummaryChartTitle: string = 'Employees Summary';

  public mentorshipDeliveryModeSummary: any[];
  public mentorshipDeliveryModeSummaryShowXAxis: boolean = true;
  public mentorshipDeliveryModeSummaryShowYAxis: boolean = true;
  public mentorshipDeliveryModeSummaryShowLegend: boolean = false;
  public mentorshipDeliveryModeSummaryShowXAxisLabel: boolean = true;
  public mentorshipDeliveryModeSummaryShowYAxisLabel: boolean = true;
  public mentorshipDeliveryModeSummaryXAxisLabel: string = 'Delivery Mode';
  public mentorshipDeliveryModeSummaryYAxisLabel: string = 'Count';
  public mentorshipDeliveryModeSummaryChartTitle: string = 'Delivery Mode Distribution';

  public mentorshipByDisabilitySummary: any[];
  public mentorshipByDisabilitySummaryShowXAxis: boolean = true;
  public mentorshipByDisabilitySummaryShowYAxis: boolean = true;
  public mentorshipByDisabilitySummaryShowLegend: boolean = false;
  public mentorshipByDisabilitySummaryShowXAxisLabel: boolean = true;
  public mentorshipByDisabilitySummaryShowYAxisLabel: boolean = true;
  public mentorshipByDisabilitySummaryXAxisLabel: string = 'Disability Type';
  public mentorshipByDisabilitySummaryYAxisLabel: string = 'Count';
  public mentorshipByDisabilitySummaryChartTitle: string = 'Disability Type Distribution';

  public mentorshipBusiCategoryByCountySummary: any[];
  public mentorshipBusiCategoryByCountySummaryShowXAxis: boolean = true;
  public mentorshipBusiCategoryByCountySummaryShowYAxis: boolean = true;
  public mentorshipBusiCategoryByCountySummaryShowLegend: boolean = false;
  public mentorshipBusiCategoryByCountySummaryShowXAxisLabel: boolean = true;
  public mentorshipBusiCategoryByCountySummaryShowYAxisLabel: boolean = true;
  public mentorshipBusiCategoryByCountySummaryXAxisLabel: string = 'Count';
  public mentorshipBusiCategoryByCountySummaryYAxisLabel: string = 'Industry Sector';
  public mentorshipBusiCategoryByCountySummaryChartTitle: string = 'Businesses By Industry Sector';

  public accessedVSOutStandingAmount: any[]
  public accessedVSOutStandingAmountShowXAxis = true;
  public accessedVSOutStandingAmountShowYAxis = true;
  public accessedVSOutStandingAmountShowLegend = false;
  public accessedVSOutStandingAmountShowXAxisLabel = true;
  public accessedVSOutStandingAmountXAxisLabel = 'Partners';
  public accessedVSOutStandingAmountShowYAxisLabel = true;
  public accessedVSOutStandingAmountYAxisLabel = 'Amount';
  public accessedVSOutStandingAmountChartTitle: string = 'Accessed Vs OutStanding By Partner';

  public accessedVSOutStandingAmountByGender: any[]
  public accessedVSOutStandingAmountByGenderShowXAxis = true;
  public accessedVSOutStandingAmountByGenderShowYAxis = true;
  public accessedVSOutStandingAmountByGenderShowLegend = false;
  public accessedVSOutStandingAmountByGenderShowXAxisLabel = true;
  public accessedVSOutStandingAmountByGenderXAxisLabel = 'Genders';
  public accessedVSOutStandingAmountByGenderShowYAxisLabel = true;
  public accessedVSOutStandingAmountByGenderYAxisLabel = 'Amount';
  public accessedVSOutStandingAmountByGenderChartTitle: string = 'Accessed Vs OutStanding By Gender';

  public taTrainedBySegment: any[];
  public taTrainedBySegmentShowLegend: boolean = false;
  public taTrainedBySegmentShowLabels: boolean = true;
  public taTrainedBySegmentExplodeSlices: boolean = false;
  public taTrainedBySegmentDoughnut: boolean = false;
  public taTrainedBySegmentChartTitle: string = 'TA By Business Segment';

  public loansDisbursedBySegment: any[];
  public loansDisbursedBySegmentShowLegend: boolean = false;
  public loansDisbursedBySegmentShowLabels: boolean = true;
  public loansDisbursedBySegmentExplodeSlices: boolean = false;
  public loansDisbursedBySegmentDoughnut: boolean = false;
  public loansDisbursedBySegmentChartTitle: string = 'Disbursed By Business Segment';

  public lastThreeYearLoansAccessedPerPartner: any[];
  public lastThreeYearLoansAccessedPerPartnerShowXAxis = true;
  public lastThreeYearLoansAccessedPerPartnerShowYAxis = true;
  public lastThreeYearLoansAccessedPerPartnerShowLegend = false;
  public lastThreeYearLoansAccessedPerPartnerShowXAxisLabel = true;
  public lastThreeYearLoansAccessedPerPartnerXAxisLabel = 'Year';
  public lastThreeYearLoansAccessedPerPartnerShowYAxisLabel = true;
  public lastThreeYearLoansAccessedPerPartnerYAxisLabel = 'Accessed Loans';
  public lastThreeYearLoansAccessedPerPartnerTitle = 'Loans Accessed Vs Last 2 Years';

  public loansDisbursedBySector: any[];
  public loansDisbursedBySectorShowLegend: boolean = false;
  public loansDisbursedBySectorShowLabels: boolean = true;
  public loansDisbursedBySectorExplodeSlices: boolean = false;
  public loansDisbursedBySectorDoughnut: boolean = true;
  public loansDisbursedBySectorChartTitle: string = 'Loan Disbursed by Industry Sector';

  public topFourPartnersloansDisbursed: any[];
  public topFourPartnersloansDisbursedChartTitle: string = 'Loan Disbursed Top Four Partners';

  public topFourCountiesloansDisbursed: any[];
  public topFourCountiesloansDisbursedChartTitle: string = 'Loan Disbursed Top Four Counties';

  public topFourCountiesBusinessesTrained: any[];
  public topFourCountiesBusinessesTrainedChartTitle: string = 'Businesses Trained Top Four Counties';

  public disabledBusinessesTainedByGender: any[];
  public disabledBusinessesTainedByGenderShowLegend: boolean = false;
  public disabledBusinessesTainedByGenderShowLabels: boolean = true;
  public disabledBusinessesTainedByGenderExplodeSlices: boolean = false;
  public disabledBusinessesTainedByGenderDoughnut: boolean = false;
  public disabledBusinessesTainedByGenderChartTitle: string = 'Businesses Trained With Disabilities';

  public refugeeBusinessesTainedByGender: any[];
  public refugeeBusinessesTainedByGenderShowLegend: boolean = false;
  public refugeeBusinessesTainedByGenderShowLabels: boolean = true;
  public refugeeBusinessesTainedByGenderExplodeSlices: boolean = false;
  public refugeeBusinessesTainedByGenderDoughnut: boolean = true;
  public refugeeBusinessesTainedByGenderChartTitle: string = 'Refugee Businesses Trained';
  
  //public countyData: Map<number, any>;
  public countySummary: CountySummaryDto[] = [];
  public businessesTrained: string;
  public businessesLoaned: string;
  public keMapImage = "ke_map/KE-MAP.png";


  subs = new SubscriptionsContainer();

  @ViewChild('loansDisbursedByGenderContentDiv', { static: false }) loansDisbursedByGenderContentDiv!: ElementRef;
  @ViewChild('loansDisbursedByGenderNewContentDiv', { static: false }) loansDisbursedByGenderNewContentDiv!: ElementRef;
  @ViewChild('businessesTainedByGenderContentDiv', { static: false }) businessesTainedByGenderContentDiv!: ElementRef;
  @ViewChild('mentorshipByGenderCategoryContentDiv', { static: false }) mentorshipByGenderCategoryContentDiv!: ElementRef;
  @ViewChild('loanedBusinessesByGenderContentDiv', { static: false }) loanedBusinessesByGenderContentDiv!: ElementRef;
  @ViewChild('loansDisbursedByPipelineContentDiv', { static: false }) loansDisbursedByPipelineContentDiv!: ElementRef;
  @ViewChild('mentorshipGenderSummaryContentDiv', { static: false }) mentorshipGenderSummaryContentDiv!: ElementRef;
  @ViewChild('countyTrainedBusinessesMapContentDiv', { static: false }) countyTrainedBusinessesMapContentDiv!: ElementRef;
  @ViewChild('countyMentorshipMapContentDiv', { static: false }) countyMentorshipMapContentDiv!: ElementRef;
  @ViewChild('countyLendingMapContentDiv', { static: false }) countyLendingMapContentDiv!: ElementRef;
  @ViewChild('loansDisbursedByStatusContentDiv', { static: false }) loansDisbursedByStatusContentDiv!: ElementRef;
  @ViewChild('taNeedsByGenderContentDiv', { static: false }) taNeedsByGenderContentDiv!: ElementRef;
  @ViewChild('loansDisbursedBySegmentContentDiv', { static: false }) loansDisbursedBySegmentContentDiv!: ElementRef;
  @ViewChild('loansDisbursedBySectorContentDiv', { static: false }) loansDisbursedBySectorContentDiv!: ElementRef;
  @ViewChild('trainingByPartnerByGenderContentDiv', { static: false }) trainingByPartnerByGenderContentDiv!: ElementRef;
  @ViewChild('loanDisbursedByProductByGenderContentDiv', { static: false }) loanDisbursedByProductByGenderContentDiv!: ElementRef;
  @ViewChild('taTrainedBySectorContentDiv', { static: false }) taTrainedBySectorContentDiv!: ElementRef;
  @ViewChild('loansDisbursedByProductContentDiv', { static: false }) loansDisbursedByProductContentDiv!: ElementRef;
  @ViewChild('employeesSummaryContentDiv', { static: false }) employeesSummaryContentDiv!: ElementRef;
  @ViewChild('mentorshipDeliveryModeSummaryContentDiv', { static: false }) mentorshipDeliveryModeSummaryContentDiv!: ElementRef;
  @ViewChild('mentorshipByDisabilitySummaryContentDiv', { static: false }) mentorshipByDisabilitySummaryContentDiv!: ElementRef;
  @ViewChild('mentorshipBusiCategoryByCountySummaryContentDiv', { static: false }) mentorshipBusiCategoryByCountySummaryContentDiv!: ElementRef;
  @ViewChild('accessedVSOutStandingAmountContentDiv', { static: false }) accessedVSOutStandingAmountContentDiv!: ElementRef;
  @ViewChild('accessedVSOutStandingAmountByGenderContentDiv', { static: false }) accessedVSOutStandingAmountByGenderContentDiv!: ElementRef;
  @ViewChild('taTrainedBySegmentContentDiv', { static: false }) taTrainedBySegmentContentDiv!: ElementRef;
  @ViewChild('refugeeBusinessesTainedByGenderContentDiv', { static: false }) refugeeBusinessesTainedByGenderContentDiv!: ElementRef;
  @ViewChild('disabledBusinessesTainedByGenderContentDiv', { static: false }) disabledBusinessesTainedByGenderContentDiv!: ElementRef;

  constructor(private dashBoardService: DashboardService, public dialog: MatDialog){
    Object.assign(this, { single, multi });
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['dashboardTypeFilter']) {
      this.dashboardTypeFilter = changes['dashboardTypeFilter']['currentValue']
      this.reloadData();
    }
    if (changes['dashBoardFilters']) {
      this.dashBoardFilters = changes['dashBoardFilters']['currentValue']
      this.reloadData();
    }
    
  }

  ngOnInit() {
    this.reloadData();
  }

  reloadData(){
    if (this.dashboardTypeFilter.isFinancialDashboard) {
      this.getLoansDisbursedByGenderSummary();
      this.getLoanDisbursedByIndustrySectorSummary();
      this.getLoansDisbursedByPipelineSummary();
      this.getLoansDisbursedByStatusSummary();
      this.getLoansAccessedVsOutStandingByPartnerSummary();
      this.getLastThreeYearsAccessedLoanPerPartnerSummary();
      this.getLoansAccessedVsOutStandingByGenderSummary();
      this.getLoanDisbursedByIndustrySegmentSummary();
      this.getLoanDisbursedTopFourPartnersSummary();
      this.getLoanDisbursedTopFourCountiesSummary();
      this.getLoansDisbursedByLoanProductSummary();
      this.getLoanedBusinessesByGenderSummary();
      this.getLoanDisbursedByLoanProductByGenderSummary();
    } else if (this.dashboardTypeFilter.isMentorShipDashboard) {
      this.getMentorshipGenderSummary();
      this.getParticipantsMentorshipDeliveryModeSummary();
      this.getParticipantsMentorshipBusiCategoryByCountySummary();
      this.getMentorshipByGenderCategorySummary();
      this.getMentorshipByDisabilitySummary();
    } else if (this.dashboardTypeFilter.isTADashboard) {
      this.getBusinessesTrainedByGenderSummary();
      this.getTaNeedsByGenderSummary();
      this.getTaTrainingBySectorSummary();
      this.getTaTrainingBySegmentSummary();
      this.getTrainingByPartnerByGenderSummary();
      this.getBusinessTrainedTopFourCountiesSummary();
      this.getParticipantsEmployeesSummary();
      this.getDisabledBusinessOwnersTrainedByGenderSummary();
      this.getRefugeeBusinessOwnersTrainedByGenderSummary();
    } 
    this.getCountySummary();
    
  }

  getLoansDisbursedByGenderSummary() {
    this.subs.add = this.dashBoardService.getLoansDisbursedByGenderSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.loansDisbursedByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedByIndustrySectorSummary() {
    this.subs.add = this.dashBoardService.getLoanDisbursedByIndustrySectorSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.loansDisbursedBySector = response;
        },
        error: (error) => { }
      });
  }

  getLoansDisbursedByLoanProductSummary() {
    this.subs.add = this.dashBoardService.getLoansDisbursedByLoanProductSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.loansDisbursedByProduct = response;
        },
        error: (error) => { }
      });
  }

  getLoansDisbursedByPipelineSummary() {
    this.subs.add = this.dashBoardService.getLoansDisbursedByPipelineSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.loansDisbursedByPipeline = response;
        },
        error: (error) => { }
      });
  }

  getMentorshipGenderSummary() {
    this.subs.add = this.dashBoardService.getMentorshipByGivenFieldSummary(this.dashBoardFilters, 'owner_gender')
      .subscribe({
        next: (response) => {
          this.mentorshipGenderSummary = response;
        },
        error: (error) => { }
      });
  }

  getLoansDisbursedByStatusSummary() {
    this.subs.add = this.dashBoardService.getLoansDisbursedByStatusSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.loansDisbursedByStatus = response;
        },
        error: (error) => { }
      });
  }

  getBusinessesTrainedByGenderSummary() {
    this.subs.add = this.dashBoardService.getBusinessesTrainedByGenderSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.businessesTainedByGender = response;
        },
        error: (error) => { }
      });
  }

  getMentorshipByGenderCategorySummary() {
    this.subs.add = this.dashBoardService.getMentorshipByGivenFieldSummary(this.dashBoardFilters, 'gender_category')
      .subscribe({
        next: (response) => {
          this.mentorshipByGenderCategory = response;
        },
        error: (error) => { }
      });
  }

  getMentorshipByDisabilitySummary() {
    this.subs.add = this.dashBoardService.getMentorshipByGivenFieldSummary(this.dashBoardFilters, 'disability_type')
      .subscribe({
        next: (response) => {
          this.mentorshipByDisabilitySummary = response;
        },
        error: (error) => { }
      });
  }

  getLoanedBusinessesByGenderSummary() {
    this.subs.add = this.dashBoardService.getLoanedBusinessesByGenderSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.loanedBusinessesByGender = response;
        },
        error: (error) => { }
      });
  }

  getDisabledBusinessOwnersTrainedByGenderSummary() {
    this.subs.add = this.dashBoardService.getDisabledBusinessOwnersTrainedByGenderSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.disabledBusinessesTainedByGender = response;
        },
        error: (error) => { }
      });
  }

  getRefugeeBusinessOwnersTrainedByGenderSummary() {
    this.subs.add = this.dashBoardService.getRefugeeBusinessOwnersTrainedByGenderSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.refugeeBusinessesTainedByGender = response;
        },
        error: (error) => { }
      });
  }

  getTaNeedsByGenderSummary() {
    this.subs.add = this.dashBoardService.getTaNeedsByGenderSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.TANeedsByGender = response;
        },
        error: (error) => { }
      });
  }

  getTaTrainingBySectorSummary() {
    this.subs.add = this.dashBoardService.getTaTrainingBySectorSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.taTrainedBySector = response;
        },
        error: (error) => { }
      });
  }

  getParticipantsEmployeesSummary() {
    this.subs.add = this.dashBoardService.getParticipantsEmployeesSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.employeesSummary = response;
        },
        error: (error) => { 
          console.log(error)
        }
      });
  }

  getParticipantsMentorshipDeliveryModeSummary() {
    this.subs.add = this.dashBoardService.getParticipantsMentorshipDeliveryModeSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.mentorshipDeliveryModeSummary = response;
        },
        error: (error) => { 
          console.log(error)
        }
      });
  }

  getParticipantsMentorshipBusiCategoryByCountySummary() {
    this.subs.add = this.dashBoardService.getParticipantsMentorshipBusiCategoryByCountySummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.mentorshipBusiCategoryByCountySummary = response;
        },
        error: (error) => { 
          console.log(error)
        }
      });
  }

  getTaTrainingBySegmentSummary() {
    this.subs.add = this.dashBoardService.getTaTrainingBySegmentSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.taTrainedBySegment = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedByIndustrySegmentSummary() {
    this.subs.add = this.dashBoardService.getLoanDisbursedByIndustrySegmentSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.loansDisbursedBySegment = response;
        },
        error: (error) => { }
      });
  }

  getTrainingByPartnerByGenderSummary() {
    this.subs.add = this.dashBoardService.getTrainingByPartnerByGenderSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.trainingByPartnerByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedByLoanProductByGenderSummary() {
    this.subs.add = this.dashBoardService.getLoanDisbursedByLoanProductByGenderSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.loanDisbursedByProductByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoansAccessedVsOutStandingByPartnerSummary() {
    this.subs.add = this.dashBoardService.getLoansAccessedVsOutStandingByPartnerSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.accessedVSOutStandingAmount = response;
        },
        error: (error) => { }
      });
  }

  getLoansAccessedVsOutStandingByGenderSummary() {
    this.subs.add = this.dashBoardService.getLoansAccessedVsOutStandingByGenderSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.accessedVSOutStandingAmountByGender = response;
        },
        error: (error) => { }
      });
  }

  getCountySummary() {
    this.subs.add = this.dashBoardService.getCountySummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.countySummary = response;
        },
        error: (error) => { }
      });
  }

  getLastThreeYearsAccessedLoanPerPartnerSummary() {
    this.subs.add = this.dashBoardService.getLastThreeYearsAccessedLoanPerPartnerSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.lastThreeYearLoansAccessedPerPartner = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedTopFourPartnersSummary() {
    this.subs.add = this.dashBoardService.getLoanDisbursedTopFourPartnersSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.topFourPartnersloansDisbursed = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedTopFourCountiesSummary() {
    this.subs.add = this.dashBoardService.getLoanDisbursedTopFourCountiesSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.topFourCountiesloansDisbursed = response;
        },
        error: (error) => { }
      });
  }

  getBusinessTrainedTopFourCountiesSummary() {
    this.subs.add = this.dashBoardService.getBusinessTrainedTopFourCountiesSummary(this.dashBoardFilters)
      .subscribe({
        next: (response) => {
          this.topFourCountiesBusinessesTrained = response;
        },
        error: (error) => { }
      });
  }
  


  public onSelect(event: any) {
    console.log(event);
  }

  valueFormatting = (value: number) => {
    return this.dashBoardService.formatNumberToShortForm(Number(value)); // Outputs as "8.94M"
  };

  valueFormattingWithThousandSeparator = (value: number) => {
    return this.dashBoardService.formatNumberWithThousandSeparatorForm(Number(value)); // Outputs as "55,746.00"
  };
  


  ngOnDestroy() {
    this.subs.dispose();
  }

  ngAfterViewChecked() {
    this.previousWidthOfResizedDiv = this.resizedDiv.nativeElement.clientWidth;
  }

  expandLoansDisbursedByGenderDoughnut(){
    const data = { 
      content: this.loansDisbursedByGenderContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.loansDisbursedByGender,
      chartShowLegend: this.loansDisbursedByGenderShowLegend,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.loansDisbursedByGenderShowLabels,
      chartExplodeSlices: this.loansDisbursedByGenderExplodeSlices,
      chartIsDoughnut: this.loansDisbursedByGenderDoughnut,
      labelFormatting: this.valueFormattingWithThousandSeparator,
      chartTitle: this.loansDisbursedByGenderChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.loansDisbursedByGender.find(data => data.name === label);
        return item ? `${this.valueFormattingWithThousandSeparator(item.value)}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandBusinessesTainedByGenderDoughnut(){
    const data = { 
      content: this.businessesTainedByGenderContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.businessesTainedByGender,
      chartShowLegend: this.businessesTainedByGenderShowLegend,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.businessesTainedByGenderShowLabels,
      chartExplodeSlices: this.businessesTainedByGenderExplodeSlices,
      chartIsDoughnut: this.businessesTainedByGenderDoughnut,
      labelFormatting: this.valueFormattingWithThousandSeparator,
      chartTitle: this.businessesTainedByGenderChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.businessesTainedByGender.find(data => data.name === label);
        return item ? `${this.valueFormatting(item.value)}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandMentorshipByGenderCategoryContentDivDoughnut(){
    const data = { 
      content: this.mentorshipByGenderCategoryContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.mentorshipByGenderCategory,
      chartShowLegend: this.mentorshipByGenderCategoryShowLegend,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.mentorshipByGenderCategoryShowLabels,
      chartExplodeSlices: this.mentorshipByGenderCategoryExplodeSlices,
      chartIsDoughnut: this.mentorshipByGenderCategoryDoughnut,
      labelFormatting: this.valueFormatting,
      chartTitle: this.mentorshipByGenderCategoryChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.mentorshipByGenderCategory.find(data => data.name === label);
        return item ? `${this.valueFormatting(item.value)}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandLoanedBusinessesByGenderDoughnut(){
    const data = { 
      content: this.loanedBusinessesByGenderContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.loanedBusinessesByGender,
      chartShowLegend: this.loanedBusinessesByGenderShowLegend,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.loanedBusinessesByGenderShowLabels,
      chartExplodeSlices: this.loanedBusinessesByGenderExplodeSlices,
      chartIsDoughnut: this.loanedBusinessesByGenderDoughnut,
      labelFormatting: this.valueFormatting,
      chartTitle: this.loanedBusinessesByGenderChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.loanedBusinessesByGender.find(data => data.name === label);
        return item ? `${this.valueFormatting(item.value)}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandLoansDisbursedBySegmentDoughnut(){
    const data = { 
      content: this.loansDisbursedBySegmentContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.loansDisbursedBySegment,
      chartShowLegend: this.loansDisbursedBySegmentShowLegend,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.loansDisbursedBySegmentShowLabels,
      chartExplodeSlices: this.loansDisbursedBySegmentExplodeSlices,
      chartIsDoughnut: this.loansDisbursedBySegmentDoughnut,
      labelFormatting: this.valueFormatting,
      chartTitle: this.loansDisbursedBySegmentChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.loansDisbursedBySegment.find(data => data.name === label);
        return item ? `${this.valueFormatting(item.value)}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }


  expandLoansDisbursedBySectorDoughnut(){
    const data = { 
      content: this.loansDisbursedBySectorContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.loansDisbursedBySector,
      chartShowLegend: this.loansDisbursedBySectorShowLegend,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.loansDisbursedBySectorShowLabels,
      chartExplodeSlices: this.loansDisbursedBySectorExplodeSlices,
      chartIsDoughnut: this.loansDisbursedBySectorDoughnut,
      labelFormatting: this.valueFormatting,
      chartTitle: this.loansDisbursedBySectorChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.loansDisbursedBySector.find(data => data.name === label);
        return item ? `${this.valueFormatting(item.value)}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandLoansDisbursedByPipelineBarChart(){
    const data = { 
      content: this.loansDisbursedByPipelineContentDiv.nativeElement.cloneNode(true),
      chartType: 'ngx-charts-bar-horizontal',
      chartData: this.loansDisbursedByPipeline,
      chartShowLegend: this.loansDisbursedByPipelineShowLegend,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.loansDisbursedByPipelineShowLabels,
      chartExplodeSlices: this.loansDisbursedByPipelineExplodeSlices,
      chartIsDoughnut: this.loansDisbursedByPipelineDoughnut,
      chartTitle: this.loansDisbursedByPipelineChartTitle,
      chartGradient: this.gradient,
      chartShowXAxis: this.loansDisbursedByPipelineShowXAxis,
      chartShowYAxis: this.loansDisbursedByPipelineShowYAxis,
      chartShowXAxisLabel: this.loansDisbursedByPipelineShowXAxisLabel,
      chartShowYAxisLabel: this.loansDisbursedByPipelineShowYAxisLabel,
      chartYAxisLabel: this.loansDisbursedByPipelineYAxisLabel,
      chartXAxisLabel: this.loansDisbursedByPipelineXAxisLabel,
      chartFormatLabel: this.valueFormatting,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandMentorshipGenderSummaryPieChart(){
    const data = { 
      content: this.mentorshipGenderSummaryContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.mentorshipGenderSummary,
      chartShowLegend: this.mentorshipGenderSummaryShowLegend,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.mentorshipGenderSummaryShowLabels,
      chartExplodeSlices: this.mentorshipGenderSummaryExplodeSlices,
      chartIsDoughnut: this.mentorshipGenderSummaryDoughnut,
      chartTitle: this.mentorshipGenderSummaryChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.mentorshipGenderSummary.find(data => data.name === label);
        return item ? `${this.valueFormatting(item.value)}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandCountyTrainedBusinessesMap(){
    this.expandCountyMap(this.countyTrainedBusinessesMapContentDiv, this.countySummary, 'businessesTrained', 'Training By County');
  }

  expandCountyMentorshipMap(){
    this.expandCountyMap(this.countyMentorshipMapContentDiv, this.countySummary, 'businessesMentored', 'Mentorship By County');
  }

  expandCountyLendingMap(){
    this.expandCountyMap(this.countyLendingMapContentDiv, this.countySummary, 'loanFields', 'Lending By County');
  }

  expandCountyMap(contentDivChild: any, mapData: any, countyDataToBePicked: string, chartTitle: string){
    const data = { 
      content: contentDivChild.nativeElement.cloneNode(true),
      mapContainerElement: contentDivChild,
      chartType: 'kenyan-leaflet-map',
      chartData: mapData,
      countyDataToBePicked: countyDataToBePicked,
      chartTitle: chartTitle
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandLoansDisbursedByStatusBarChart(){
    const data = { 
      content: this.loansDisbursedByStatusContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.loansDisbursedByStatusContentDiv,
      chartType: 'ngx-charts-bar-horizontal',
      chartData: this.loansDisbursedByStatus,
      chartGradient: this.gradient,
      chartShowXAxis: this.loansDisbursedByStatusShowXAxis,
      chartShowYAxis: this.loansDisbursedByStatusShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.loansDisbursedByStatusShowXAxisLabel,
      chartShowYAxisLabel: this.loansDisbursedByStatusShowYAxisLabel,
      chartYAxisLabel: this.loansDisbursedByStatusYAxisLabel,
      chartXAxisLabel: this.loansDisbursedByStatusXAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.loansDisbursedByStatusChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandTANeedsByGenderBarChart(){
    const data = { 
      content: this.taNeedsByGenderContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.taNeedsByGenderContentDiv,
      chartType: 'ngx-charts-bar-horizontal-2d',
      chartData: this.TANeedsByGender,
      chartGradient: this.gradient,
      chartShowXAxis: this.TANeedsByGenderShowXAxis,
      chartShowYAxis: this.TANeedsByGenderShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.TANeedsByGenderShowXAxisLabel,
      chartShowYAxisLabel: this.TANeedsByGenderShowYAxisLabel,
      chartYAxisLabel: this.TANeedsByGenderXAxisLabel,
      chartXAxisLabel: this.TANeedsByGenderYAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.TANeedsByGenderChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandTrainingByPartnerByGenderBarChart(){
    const data = { 
      content: this.trainingByPartnerByGenderContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.trainingByPartnerByGenderContentDiv,
      chartType: 'ngx-charts-bar-horizontal-2d',
      chartData: this.trainingByPartnerByGender,
      chartGradient: this.gradient,
      chartShowXAxis: this.trainingByPartnerByGenderShowXAxis,
      chartShowYAxis: this.trainingByPartnerByGenderShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.trainingByPartnerByGenderShowXAxisLabel,
      chartShowYAxisLabel: this.trainingByPartnerByGenderShowYAxisLabel,
      chartYAxisLabel: this.trainingByPartnerByGenderXAxisLabel,
      chartXAxisLabel: this.trainingByPartnerByGenderYAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.trainingByPartnerByGenderChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandLoanDisbursedByProductByGenderBarChart(){
    const data = { 
      content: this.loanDisbursedByProductByGenderContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.loanDisbursedByProductByGenderContentDiv,
      chartType: 'ngx-charts-bar-horizontal-2d',
      chartData: this.loanDisbursedByProductByGender,
      chartGradient: this.gradient,
      chartShowXAxis: this.loanDisbursedByProductByGenderShowXAxis,
      chartShowYAxis: this.loanDisbursedByProductByGenderShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.loanDisbursedByProductByGenderShowXAxisLabel,
      chartShowYAxisLabel: this.loanDisbursedByProductByGenderShowYAxisLabel,
      chartYAxisLabel: this.loanDisbursedByProductByGenderXAxisLabel,
      chartXAxisLabel: this.loanDisbursedByProductByGenderYAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.loanDisbursedByProductByGenderChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandTaTrainedBySectorBarChart(){
    const data = { 
      content: this.taTrainedBySectorContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.taTrainedBySectorContentDiv,
      chartType: 'ngx-charts-bar-horizontal',
      chartData: this.taTrainedBySector,
      chartGradient: this.gradient,
      chartShowXAxis: this.taTrainedBySectorShowXAxis,
      chartShowYAxis: this.taTrainedBySectorShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      
      chartShowXAxisLabel: this.taTrainedBySectorShowXAxisLabel,
      chartShowYAxisLabel: this.taTrainedBySectorShowYAxisLabel,
      chartYAxisLabel: this.taTrainedBySectorXAxisLabel,
      chartXAxisLabel: this.taTrainedBySectorYAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.taTrainedBySectorChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandLoansDisbursedByProductBarChart(){
    const data = { 
      content: this.loansDisbursedByProductContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.loansDisbursedByProductContentDiv,
      chartType: 'ngx-charts-bar-horizontal',
      chartData: this.loansDisbursedByProduct,
      chartGradient: this.gradient,
      chartShowXAxis: this.loansDisbursedByProductShowXAxis,
      chartShowYAxis: this.loansDisbursedByProductShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.loansDisbursedByProductShowXAxisLabel,
      chartShowYAxisLabel: this.loansDisbursedByProductShowYAxisLabel,
      chartYAxisLabel: this.loansDisbursedByProductXAxisLabel,
      chartXAxisLabel: this.loansDisbursedByProductYAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.loansDisbursedByProductChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandEmployeesSummaryBarChart(){
    const data = { 
      content: this.employeesSummaryContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.employeesSummaryContentDiv,
      chartType: 'ngx-charts-bar-horizontal',
      chartData: this.employeesSummary,
      chartGradient: this.gradient,
      chartShowXAxis: this.employeesSummaryShowXAxis,
      chartShowYAxis: this.employeesSummaryShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.employeesSummaryShowXAxisLabel,
      chartShowYAxisLabel: this.employeesSummaryShowYAxisLabel,
      chartYAxisLabel: this.employeesSummaryXAxisLabel,
      chartXAxisLabel: this.employeesSummaryYAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.employeesSummaryChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandMentorshipByDisabilitySummaryBarChart(){
    const data = { 
      content: this.mentorshipByDisabilitySummaryContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.mentorshipByDisabilitySummaryContentDiv,
      chartType: 'ngx-charts-bar-horizontal',
      chartData: this.mentorshipByDisabilitySummary,
      chartGradient: this.gradient,
      chartShowXAxis: this.mentorshipByDisabilitySummaryShowXAxis,
      chartShowYAxis: this.mentorshipByDisabilitySummaryShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.mentorshipByDisabilitySummaryShowXAxisLabel,
      chartShowYAxisLabel: this.mentorshipByDisabilitySummaryShowYAxisLabel,
      chartYAxisLabel: this.mentorshipByDisabilitySummaryXAxisLabel,
      chartXAxisLabel: this.mentorshipByDisabilitySummaryYAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.mentorshipByDisabilitySummaryChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandMentorshipDeliveryModeSummaryBarChart(){
    const data = { 
      content: this.mentorshipDeliveryModeSummaryContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.mentorshipDeliveryModeSummaryContentDiv,
      chartType: 'ngx-charts-bar-horizontal',
      chartData: this.mentorshipDeliveryModeSummary,
      chartGradient: this.gradient,
      chartShowXAxis: this.mentorshipDeliveryModeSummaryShowXAxis,
      chartShowYAxis: this.mentorshipDeliveryModeSummaryShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.mentorshipDeliveryModeSummaryShowXAxisLabel,
      chartShowYAxisLabel: this.mentorshipDeliveryModeSummaryShowYAxisLabel,
      chartYAxisLabel: this.mentorshipDeliveryModeSummaryXAxisLabel,
      chartXAxisLabel: this.mentorshipDeliveryModeSummaryYAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.mentorshipDeliveryModeSummaryChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandMentorshipBusiCategoryByCountySummaryBarChart(){
    const data = { 
      content: this.mentorshipBusiCategoryByCountySummaryContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.mentorshipBusiCategoryByCountySummaryContentDiv,
      chartType: 'ngx-charts-bar-horizontal',
      chartData: this.mentorshipBusiCategoryByCountySummary,
      chartGradient: this.gradient,
      chartShowXAxis: this.mentorshipBusiCategoryByCountySummaryShowXAxis,
      chartShowYAxis: this.mentorshipBusiCategoryByCountySummaryShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.mentorshipBusiCategoryByCountySummaryShowXAxisLabel,
      chartShowYAxisLabel: this.mentorshipBusiCategoryByCountySummaryShowYAxisLabel,
      chartYAxisLabel: this.mentorshipBusiCategoryByCountySummaryYAxisLabel,
      chartXAxisLabel: this.mentorshipBusiCategoryByCountySummaryXAxisLabel,
      //chartFormatLabel: this.valueFormatting,
      chartTitle: this.mentorshipBusiCategoryByCountySummaryChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandAccessedVSOutStandingAmountBarChart(){
    const data = { 
      content: this.accessedVSOutStandingAmountContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.accessedVSOutStandingAmountContentDiv,
      chartType: 'ngx-charts-bar-horizontal-2d',
      chartData: this.accessedVSOutStandingAmount,
      chartGradient: this.gradient,
      chartShowXAxis: this.accessedVSOutStandingAmountShowXAxis,
      chartShowYAxis: this.accessedVSOutStandingAmountShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.accessedVSOutStandingAmountShowXAxisLabel,
      chartShowYAxisLabel: this.accessedVSOutStandingAmountShowYAxisLabel,
      chartYAxisLabel: this.accessedVSOutStandingAmountYAxisLabel,
      chartXAxisLabel: this.accessedVSOutStandingAmountXAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.accessedVSOutStandingAmountChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandAccessedVSOutStandingAmountByGenderBarChart(){
    const data = { 
      content: this.accessedVSOutStandingAmountByGenderContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.accessedVSOutStandingAmountByGenderContentDiv,
      chartType: 'ngx-charts-bar-horizontal-2d',
      chartData: this.accessedVSOutStandingAmountByGender,
      chartGradient: this.gradient,
      chartShowXAxis: this.accessedVSOutStandingAmountByGenderShowXAxis,
      chartShowYAxis: this.accessedVSOutStandingAmountByGenderShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.accessedVSOutStandingAmountByGenderShowXAxisLabel,
      chartShowYAxisLabel: this.accessedVSOutStandingAmountByGenderShowYAxisLabel,
      chartYAxisLabel: this.accessedVSOutStandingAmountByGenderXAxisLabel,
      chartXAxisLabel: this.accessedVSOutStandingAmountByGenderYAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.accessedVSOutStandingAmountByGenderChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandTaTrainedBySegmentPieChart(){
    const data = { 
      content: this.taTrainedBySegmentContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.taTrainedBySegment,
      chartShowLegend: true,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.taTrainedBySegmentShowLabels,
      chartExplodeSlices: this.taTrainedBySegmentExplodeSlices,
      chartIsDoughnut: this.taTrainedBySegmentDoughnut,
      chartTitle: this.taTrainedBySegmentChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.taTrainedBySegment.find(data => data.name === label);
        return item ? `${item.value}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandRefugeeBusinessesTainedByGenderPieChart(){
    const data = { 
      content: this.refugeeBusinessesTainedByGenderContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.refugeeBusinessesTainedByGender,
      chartShowLegend: true,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.refugeeBusinessesTainedByGenderShowLabels,
      chartExplodeSlices: this.refugeeBusinessesTainedByGenderExplodeSlices,
      chartIsDoughnut: this.refugeeBusinessesTainedByGenderDoughnut,
      chartTitle: this.refugeeBusinessesTainedByGenderChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.refugeeBusinessesTainedByGender.find(data => data.name === label);
        return item ? `${item.value}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandDisabledBusinessesTainedByGenderPieChart(){
    const data = { 
      content: this.disabledBusinessesTainedByGenderContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.disabledBusinessesTainedByGender,
      chartShowLegend: true,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.disabledBusinessesTainedByGenderShowLabels,
      chartExplodeSlices: this.disabledBusinessesTainedByGenderExplodeSlices,
      chartIsDoughnut: this.disabledBusinessesTainedByGenderDoughnut,
      chartTitle: this.disabledBusinessesTainedByGenderChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.disabledBusinessesTainedByGender.find(data => data.name === label);
        return item ? `${item.value}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }
}