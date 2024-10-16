import { AfterViewChecked, AfterViewInit, Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { customers, orders, products, refunds } from '@data/dashboard-data';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { PieChartComponent } from "../pie-chart/pie-chart.component"; 
import { DiskSpaceComponent } from "../disk-space/disk-space.component";
import { multi, single } from '@data/charts.data';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { Subject, takeUntil } from 'rxjs';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { ChartDialogComponent } from '../../chart-dialog/chart-dialog.component';
import { KenyanMapComponent } from "../kenyan-map/kenyan-map.component";
import { KenyanCountyMapComponent } from "../kenyan-county-map/kenyan-county-map.component";

@Component({
  selector: 'app-info-cards',
  standalone: true,
  imports: [
    FlexLayoutModule,
    MatCardModule,
    MatIconModule,
    NgxChartsModule,
    PieChartComponent,
    DiskSpaceComponent,
    MatDialogModule,
    KenyanMapComponent,
    KenyanCountyMapComponent
],
  templateUrl: './info-cards.component.html',
  styleUrl: './info-cards.component.scss'
})
export class InfoCardsComponent implements OnInit, AfterViewChecked, OnDestroy {
  public orders: any[];
  public products: any[];
  public customers: any[];
  public refunds: any[];
  public colorScheme: any = {
    domain: ['rgba(255,255,255,0.8)']
  };
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

  public accessedVSOutStandingAmount: any[]
  public accessedVSOutStandingAmountShowXAxis = true;
  public accessedVSOutStandingAmountShowYAxis = true;
  public accessedVSOutStandingAmountShowLegend = false;
  public accessedVSOutStandingAmountShowXAxisLabel = true;
  public accessedVSOutStandingAmountXAxisLabel = 'Partners';
  public accessedVSOutStandingAmountShowYAxisLabel = true;
  public accessedVSOutStandingAmountYAxisLabel = 'Amount';
  public accessedVSOutStandingAmountChartTitle: string = 'Accessed Vs OutStanding By Partner';

  public taTrainedBySegment: any[];
  public taTrainedBySegmentShowLegend: boolean = false;
  public taTrainedBySegmentShowLabels: boolean = true;
  public taTrainedBySegmentExplodeSlices: boolean = false;
  public taTrainedBySegmentDoughnut: boolean = false;
  public taTrainedBySegmentChartTitle: string = 'TA By Business Segment';
  
  public countyData: Map<number, any>;
  public businessesTrained: string;
  public businessesLoaned: string;


  private unsubscribe$ = new Subject<void>();

  @ViewChild('loansDisbursedByGenderContentDiv', { static: true }) loansDisbursedByGenderContentDiv!: ElementRef;
  @ViewChild('loansDisbursedByPipelineContentDiv', { static: true }) loansDisbursedByPipelineContentDiv!: ElementRef;
  @ViewChild('countyTrainedBusinessesMapContentDiv', { static: true }) countyTrainedBusinessesMapContentDiv!: ElementRef;
  @ViewChild('loansDisbursedByStatusContentDiv', { static: true }) loansDisbursedByStatusContentDiv!: ElementRef;
  @ViewChild('taNeedsByGenderContentDiv', { static: true }) taNeedsByGenderContentDiv!: ElementRef;
  @ViewChild('trainingByPartnerByGenderContentDiv', { static: true }) trainingByPartnerByGenderContentDiv!: ElementRef;
  @ViewChild('taTrainedBySectorContentDiv', { static: true }) taTrainedBySectorContentDiv!: ElementRef;
  @ViewChild('accessedVSOutStandingAmountContentDiv', { static: true }) accessedVSOutStandingAmountContentDiv!: ElementRef;
  @ViewChild('taTrainedBySegmentContentDiv', { static: true }) taTrainedBySegmentContentDiv!: ElementRef;

  constructor(private dashBoardService: DashboardService, public dialog: MatDialog){
    Object.assign(this, { single, multi });
  }

  ngOnInit() {
    this.orders = orders;
    this.products = products;
    this.customers = customers;
    this.refunds = refunds;
    this.orders = this.addRandomValue('orders');
    this.customers = this.addRandomValue('customers');
    this.getLoansDisbursedByGenderSummary();
    this.getLoansDisbursedByPipelineSummary();
    this.getBusinessesTrainedByGenderSummary();
    this.getLoansDisbursedByStatusSummary();
    this.getTaNeedsByGenderSummary();
    this.getTaTrainingBySectorSummary();
    this.getTaTrainingBySegmentSummary();
    this.getTrainingByPartnerByGenderSummary();
    this.getLoansAccessedVsOutStandingByPartnerSummary();
    this.getCountySummaryMap();
  }

  getLoansDisbursedByGenderSummary() {
    this.dashBoardService.getLoansDisbursedByGenderSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoansDisbursedByPipelineSummary() {
    this.dashBoardService.getLoansDisbursedByPipelineSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedByPipeline = response;
        },
        error: (error) => { }
      });
  }

  getLoansDisbursedByStatusSummary() {
    this.dashBoardService.getLoansDisbursedByStatusSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.loansDisbursedByStatus = response;
        },
        error: (error) => { }
      });
  }

  getBusinessesTrainedByGenderSummary() {
    this.dashBoardService.getBusinessesTrainedByGenderSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.businessesTainedByGender = response;
        },
        error: (error) => { }
      });
  }

  getTaNeedsByGenderSummary() {
    this.dashBoardService.getTaNeedsByGenderSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.TANeedsByGender = response;
        },
        error: (error) => { }
      });
  }

  getTaTrainingBySectorSummary() {
    this.dashBoardService.getTaTrainingBySectorSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.taTrainedBySector = response;
        },
        error: (error) => { }
      });
  }

  getTaTrainingBySegmentSummary() {
    this.dashBoardService.getTaTrainingBySegmentSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.taTrainedBySegment = response;
        },
        error: (error) => { }
      });
  }

  getTrainingByPartnerByGenderSummary() {
    this.dashBoardService.getTrainingByPartnerByGenderSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.trainingByPartnerByGender = response;
        },
        error: (error) => { }
      });
  }

  getLoansAccessedVsOutStandingByPartnerSummary() {
    this.dashBoardService.getLoansAccessedVsOutStandingByPartnerSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.accessedVSOutStandingAmount = response;
        },
        error: (error) => { }
      });
  }

  getCountySummaryMap() {
    this.dashBoardService.getCountySummaryMap()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.countyData = response;
        },
        error: (error) => { }
      });
  }


  public onSelect(event: any) {
    console.log(event);
  }

  public addRandomValue(param: string) {
    switch (param) {
      case 'orders':
        for (let i = 1; i < 30; i++) {
          this.orders[0].series.push({ "name": 1980 + i, "value": Math.ceil(Math.random() * 1000000) });
        }
        return this.orders;
      case 'customers':
        for (let i = 1; i < 15; i++) {
          this.customers[0].series.push({ "name": 2000 + i, "value": Math.ceil(Math.random() * 1000000) });
        }
        return this.customers;
      default:
        return this.orders;
    }
  }

  ngOnDestroy() {
    this.orders[0].series.length = 0;
    this.customers[0].series.length = 0;
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

  ngAfterViewChecked() {
    if (this.previousWidthOfResizedDiv != this.resizedDiv.nativeElement.clientWidth) {
      setTimeout(() => this.orders = [...orders]);
      setTimeout(() => this.products = [...products]);
      setTimeout(() => this.customers = [...customers]);
      setTimeout(() => this.refunds = [...refunds]);
    }
    this.previousWidthOfResizedDiv = this.resizedDiv.nativeElement.clientWidth;
  }

  openExpandedChartDialog(chartData: any): void {
    // Dynamically calculate dialog size
    const dialogWidth = window.innerWidth;
    const dialogHeight = window.innerHeight;
    const dialogRef = this.dialog.open(ChartDialogComponent, {
      width: `${dialogWidth}px`,
      height: `${dialogHeight}px`,
      data: chartData,
      panelClass: 'custom-dialog-container', // Custom styles can be added
    });

    dialogRef.afterClosed().subscribe(result => {
      console.log('Dialog was closed');
    });
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
      chartTitle: 'Loan Disbursed by Gender',
      chartFormatLabel: (label: string): string => {
        // Find the data object by name and return the value instead of name
        const item = this.loansDisbursedByGender.find(data => data.name === label);
        return item ? `${item.value}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.openExpandedChartDialog(data);
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
        return item ? `${item.value}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.openExpandedChartDialog(data);
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
    this.openExpandedChartDialog(data);
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
      chartTitle: this.loansDisbursedByStatusChartTitle,
    };
    this.openExpandedChartDialog(data);
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
      chartTitle: this.TANeedsByGenderChartTitle,
    };
    this.openExpandedChartDialog(data);
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
      chartTitle: this.trainingByPartnerByGenderChartTitle,
    };
    this.openExpandedChartDialog(data);
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
      chartTitle: this.taTrainedBySectorChartTitle,
    };
    this.openExpandedChartDialog(data);
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
      chartTitle: this.accessedVSOutStandingAmountChartTitle,
    };
    this.openExpandedChartDialog(data);
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
        const item = this.loansDisbursedByPipeline.find(data => data.name === label);
        return item ? `${item.value}` : label; // If found, return the value; otherwise return the name as fallback
      }
    };
    this.openExpandedChartDialog(data);
  }
  
}