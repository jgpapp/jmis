import { AfterViewChecked, Component, ElementRef, Input, OnChanges, OnDestroy, OnInit, SimpleChanges, ViewChild } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { PieChartComponent } from "../pie-chart/pie-chart.component"; 
import { multi, single } from '@data/charts.data';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { Subject, takeUntil } from 'rxjs';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';

@Component({
  selector: 'app-info-cards',
  standalone: true,
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
  @Input('selectedDashboardView') selectedDashboardView: any;
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


  public taTrainedBySector: any[];
  public taTrainedBySectorShowXAxis: boolean = true;
  public taTrainedBySectorShowYAxis: boolean = true;
  public taTrainedBySectorShowLegend: boolean = false;
  public taTrainedBySectorShowXAxisLabel: boolean = true;
  public taTrainedBySectorShowYAxisLabel: boolean = true;
  public taTrainedBySectorXAxisLabel: string = 'Industry Sectors';
  public taTrainedBySectorYAxisLabel: string = 'Number Of Participants';
  public taTrainedBySectorChartTitle: string = 'TA Training By Industry Sector';

  public employeesSummary: any[];
  public employeesSummaryShowXAxis: boolean = true;
  public employeesSummaryShowYAxis: boolean = true;
  public employeesSummaryShowLegend: boolean = false;
  public employeesSummaryShowXAxisLabel: boolean = true;
  public employeesSummaryShowYAxisLabel: boolean = true;
  public employeesSummaryXAxisLabel: string = 'Category';
  public employeesSummaryYAxisLabel: string = 'Number Of Employees';
  public employeesSummaryChartTitle: string = 'Employees Summary';

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
  
  public countyData: Map<number, any>;
  public businessesTrained: string;
  public businessesLoaned: string;
  public keMapImage = "ke_map/KE-MAP.png";


  private unsubscribe$ = new Subject<void>();

  @ViewChild('loansDisbursedByGenderContentDiv', { static: false }) loansDisbursedByGenderContentDiv!: ElementRef;
  @ViewChild('businessesTainedByGenderContentDiv', { static: false }) businessesTainedByGenderContentDiv!: ElementRef;
  @ViewChild('loansDisbursedByPipelineContentDiv', { static: false }) loansDisbursedByPipelineContentDiv!: ElementRef;
  @ViewChild('countyTrainedBusinessesMapContentDiv', { static: false }) countyTrainedBusinessesMapContentDiv!: ElementRef;
  @ViewChild('loansDisbursedByStatusContentDiv', { static: false }) loansDisbursedByStatusContentDiv!: ElementRef;
  @ViewChild('taNeedsByGenderContentDiv', { static: false }) taNeedsByGenderContentDiv!: ElementRef;
  @ViewChild('loansDisbursedBySegmentContentDiv', { static: false }) loansDisbursedBySegmentContentDiv!: ElementRef;
  @ViewChild('loansDisbursedBySectorContentDiv', { static: false }) loansDisbursedBySectorContentDiv!: ElementRef;
  @ViewChild('trainingByPartnerByGenderContentDiv', { static: false }) trainingByPartnerByGenderContentDiv!: ElementRef;
  @ViewChild('taTrainedBySectorContentDiv', { static: false }) taTrainedBySectorContentDiv!: ElementRef;
  @ViewChild('employeesSummaryContentDiv', { static: false }) employeesSummaryContentDiv!: ElementRef;
  @ViewChild('accessedVSOutStandingAmountContentDiv', { static: false }) accessedVSOutStandingAmountContentDiv!: ElementRef;
  @ViewChild('accessedVSOutStandingAmountByGenderContentDiv', { static: false }) accessedVSOutStandingAmountByGenderContentDiv!: ElementRef;
  @ViewChild('taTrainedBySegmentContentDiv', { static: false }) taTrainedBySegmentContentDiv!: ElementRef;
  @ViewChild('refugeeBusinessesTainedByGenderContentDiv', { static: false }) refugeeBusinessesTainedByGenderContentDiv!: ElementRef;
  @ViewChild('disabledBusinessesTainedByGenderContentDiv', { static: false }) disabledBusinessesTainedByGenderContentDiv!: ElementRef;

  constructor(private dashBoardService: DashboardService, public dialog: MatDialog){
    Object.assign(this, { single, multi });
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['selectedDashboardView']) {
      this.selectedDashboardView = changes['selectedDashboardView']['currentValue']
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
    this.getLoansDisbursedByGenderSummary();
    this.getLoanDisbursedByIndustrySectorSummary();
    this.getLoansDisbursedByPipelineSummary();
    this.getBusinessesTrainedByGenderSummary();
    this.getLoansDisbursedByStatusSummary();
    this.getTaNeedsByGenderSummary();
    this.getTaTrainingBySectorSummary();
    this.getTaTrainingBySegmentSummary();
    this.getTrainingByPartnerByGenderSummary();
    this.getLoansAccessedVsOutStandingByPartnerSummary();
    this.getCountySummaryMap();
    this.getLastThreeYearsAccessedLoanPerPartnerSummary();
    this.getLoansAccessedVsOutStandingByGenderSummary();
    this.getLoanDisbursedByIndustrySegmentSummary();
    this.getLoanDisbursedTopFourPartnersSummary();
    this.getLoanDisbursedTopFourCountiesSummary();
    this.getBusinessTrainedTopFourCountiesSummary();
    this.getParticipantsEmployeesSummary();
    this.getDisabledBusinessOwnersTrainedByGenderSummary();
    this.getRefugeeBusinessOwnersTrainedByGenderSummary();
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

  getBusinessesTrainedByGenderSummary() {
    this.dashBoardService.getBusinessesTrainedByGenderSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.businessesTainedByGender = response;
        },
        error: (error) => { }
      });
  }

  getDisabledBusinessOwnersTrainedByGenderSummary() {
    this.dashBoardService.getDisabledBusinessOwnersTrainedByGenderSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.disabledBusinessesTainedByGender = response;
        },
        error: (error) => { }
      });
  }

  getRefugeeBusinessOwnersTrainedByGenderSummary() {
    this.dashBoardService.getRefugeeBusinessOwnersTrainedByGenderSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.refugeeBusinessesTainedByGender = response;
        },
        error: (error) => { }
      });
  }

  getTaNeedsByGenderSummary() {
    this.dashBoardService.getTaNeedsByGenderSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.TANeedsByGender = response;
        },
        error: (error) => { }
      });
  }

  getTaTrainingBySectorSummary() {
    this.dashBoardService.getTaTrainingBySectorSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.taTrainedBySector = response;
        },
        error: (error) => { }
      });
  }

  getParticipantsEmployeesSummary() {
    this.dashBoardService.getParticipantsEmployeesSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.employeesSummary = response;
        },
        error: (error) => { 
          console.log(error)
        }
      });
  }

  getTaTrainingBySegmentSummary() {
    this.dashBoardService.getTaTrainingBySegmentSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.taTrainedBySegment = response;
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

  getTrainingByPartnerByGenderSummary() {
    this.dashBoardService.getTrainingByPartnerByGenderSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.trainingByPartnerByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoansAccessedVsOutStandingByPartnerSummary() {
    this.dashBoardService.getLoansAccessedVsOutStandingByPartnerSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.accessedVSOutStandingAmount = response;
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

  getCountySummaryMap() {
    this.dashBoardService.getCountySummaryMap(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.countyData = response;
        },
        error: (error) => { }
      });
  }

  getLastThreeYearsAccessedLoanPerPartnerSummary() {
    this.dashBoardService.getLastThreeYearsAccessedLoanPerPartnerSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.lastThreeYearLoansAccessedPerPartner = response;
        },
        error: (error) => { }
      });
  }

  getLoanDisbursedTopFourPartnersSummary() {
    this.dashBoardService.getLoanDisbursedTopFourPartnersSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.topFourPartnersloansDisbursed = response;
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

  getBusinessTrainedTopFourCountiesSummary() {
    this.dashBoardService.getBusinessTrainedTopFourCountiesSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
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


  ngOnDestroy() {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
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
      labelFormatting: this.valueFormatting,
      chartTitle: this.loansDisbursedByGenderChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.loansDisbursedByGender.find(data => data.name === label);
        return item ? `${this.valueFormatting(item.value)}` : label; // If found, return the value; otherwise return the name as fallback
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
      labelFormatting: this.valueFormatting,
      chartTitle: this.businessesTainedByGenderChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.businessesTainedByGender.find(data => data.name === label);
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

  expandLoansDisbursedByPipelinePieChart(){
    const data = { 
      content: this.loansDisbursedByPipelineContentDiv.nativeElement.cloneNode(true),
      chartType: 'app-pie-chart',
      chartData: this.loansDisbursedByPipeline,
      chartShowLegend: this.loansDisbursedByPipelineShowLegend,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLabels: this.loansDisbursedByPipelineShowLabels,
      chartExplodeSlices: this.loansDisbursedByPipelineExplodeSlices,
      chartIsDoughnut: this.loansDisbursedByPipelineDoughnut,
      chartTitle: this.loansDisbursedByPipelineChartTitle,
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.loansDisbursedByPipeline.find(data => data.name === label);
        return item ? `${this.valueFormatting(item.value)}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandCountyTrainedBusinessesMap(){
    const data = { 
      content: this.countyTrainedBusinessesMapContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.countyTrainedBusinessesMapContentDiv,
      chartType: 'kenyan-county-map',
      chartData: this.countyData,
      countyDataToBePicked: 'businessesTrained',
      chartTitle: 'Training By County'
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandLoansDisbursedByStatusBarChart(){
    const data = { 
      content: this.loansDisbursedByStatusContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.loansDisbursedByStatusContentDiv,
      chartType: 'ngx-charts-bar-vertical',
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
      chartType: 'ngx-charts-bar-vertical-2d',
      chartData: this.TANeedsByGender,
      chartGradient: this.gradient,
      chartShowXAxis: this.TANeedsByGenderShowXAxis,
      chartShowYAxis: this.TANeedsByGenderShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.TANeedsByGenderShowXAxisLabel,
      chartShowYAxisLabel: this.TANeedsByGenderShowYAxisLabel,
      chartYAxisLabel: this.TANeedsByGenderYAxisLabel,
      chartXAxisLabel: this.TANeedsByGenderXAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.TANeedsByGenderChartTitle,
    };
    this.dashBoardService.openExpandedChartDialog(this.dialog, data);
  }

  expandTrainingByPartnerByGenderBarChart(){
    const data = { 
      content: this.trainingByPartnerByGenderContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.trainingByPartnerByGenderContentDiv,
      chartType: 'ngx-charts-bar-vertical-2d',
      chartData: this.trainingByPartnerByGender,
      chartGradient: this.gradient,
      chartShowXAxis: this.trainingByPartnerByGenderShowXAxis,
      chartShowYAxis: this.trainingByPartnerByGenderShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.trainingByPartnerByGenderShowXAxisLabel,
      chartShowYAxisLabel: this.trainingByPartnerByGenderShowYAxisLabel,
      chartYAxisLabel: this.trainingByPartnerByGenderYAxisLabel,
      chartXAxisLabel: this.trainingByPartnerByGenderXAxisLabel,
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.trainingByPartnerByGenderChartTitle,
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

  expandAccessedVSOutStandingAmountBarChart(){
    const data = { 
      content: this.accessedVSOutStandingAmountContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.accessedVSOutStandingAmountContentDiv,
      chartType: 'ngx-charts-bar-vertical-2d',
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
      chartType: 'ngx-charts-bar-vertical-2d',
      chartData: this.accessedVSOutStandingAmountByGender,
      chartGradient: this.gradient,
      chartShowXAxis: this.accessedVSOutStandingAmountByGenderShowXAxis,
      chartShowYAxis: this.accessedVSOutStandingAmountByGenderShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.accessedVSOutStandingAmountByGenderShowXAxisLabel,
      chartShowYAxisLabel: this.accessedVSOutStandingAmountByGenderShowYAxisLabel,
      chartYAxisLabel: this.accessedVSOutStandingAmountByGenderYAxisLabel,
      chartXAxisLabel: this.accessedVSOutStandingAmountByGenderXAxisLabel,
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
  

  isFinancialDashboard(): boolean {
    return 'FI' === this.selectedDashboardView;
  }

  isTADashboard(): boolean {
    return 'TA' === this.selectedDashboardView;
  }
}