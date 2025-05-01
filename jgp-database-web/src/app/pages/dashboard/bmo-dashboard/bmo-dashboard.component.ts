import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { AuthService } from '@services/users/auth.service';
import { MatIconModule } from '@angular/material/icon';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { PieChartComponent } from '../pie-chart/pie-chart.component';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { Subject, takeUntil } from 'rxjs';
import { DashboardFiltersComponent } from '../dashboard-filters/dashboard-filters.component';
import { HighLevelSummaryDto } from '../dto/highLevelSummaryDto';
import { PerformanceSummaryComponent } from "../performance-summary/performance-summary.component";
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { GlobalService } from '@services/shared/global.service';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'app-bmo-dashboard',
  standalone: true,
  imports: [
    ContentHeaderComponent,
    FlexLayoutModule,
    MatCardModule,
    MatProgressBarModule,
    MatIconModule,
    NgxChartsModule,
    PieChartComponent,
    DashboardFiltersComponent,
    PerformanceSummaryComponent,
    MatTableModule
],
  templateUrl: './bmo-dashboard.component.html',
  styleUrl: './bmo-dashboard.component.scss'
})
export class BmoDashboardComponent implements OnInit {

  public displayChart: boolean = false;
  dashBoardFilters: any;
  partnerName: string = '';
  partnerId: any;
  public autoScale = true;
  resetDashBoardFilters: boolean = false;
  @ViewChild('resizedDiv') resizedDiv: ElementRef;
  public previousWidthOfResizedDiv: number = 0;
  public chartSColorScheme: any = {
    domain: ['#2F3E9E', '#D22E2E', '#378D3B', '#7f7f7f', '#c4a678', '#6a7b6a', '#191919', '#3d144c', '#f0e1dc', '#a04324', '#00ffff', '#0e5600', '#0e9697']
  };
  public gradient = false;

  selectedDashboardView: string = 'TA';
  trainedBusinessesCountData: any;
  public trainedBusinessesCountDataDataSource: any;
  public displayedColumns = ['year', 'partnerName', 'genderName', 'value' ];

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

  public taTrainedBySegment: any[];
  public taTrainedBySegmentShowLegend: boolean = false;
  public taTrainedBySegmentShowLabels: boolean = true;
  public taTrainedBySegmentExplodeSlices: boolean = false;
  public taTrainedBySegmentDoughnut: boolean = false;
  public taTrainedBySegmentChartTitle: string = 'TA By Business Segment';

  public businessesTainedByGender: any[];
  public businessesTainedByGenderShowLegend: boolean = false;
  public businessesTainedByGenderShowLabels: boolean = true;
  public businessesTainedByGenderExplodeSlices: boolean = false;
  public businessesTainedByGenderDoughnut: boolean = true;
  public businessesTainedByGenderChartTitle: string = 'Business Trained By Gender';

  public topFourCountiesBusinessesTrained: any[];
  public topFourCountiesBusinessesTrainedChartTitle: string = 'Businesses Trained Top Four Counties';

  public disabledBusinessesTainedByGender: any[];
  public disabledBusinessesTainedByGenderShowLegend: boolean = false;
  public disabledBusinessesTainedByGenderShowLabels: boolean = true;
  public disabledBusinessesTainedByGenderExplodeSlices: boolean = false;
  public disabledBusinessesTainedByGenderDoughnut: boolean = true;
  public disabledBusinessesTainedByGenderChartTitle: string = 'Businesses Trained With Disabilities';

  public refugeeBusinessesTainedByGender: any[];
  public refugeeBusinessesTainedByGenderShowLegend: boolean = false;
  public refugeeBusinessesTainedByGenderShowLabels: boolean = true;
  public refugeeBusinessesTainedByGenderExplodeSlices: boolean = false;
  public refugeeBusinessesTainedByGenderDoughnut: boolean = true;
  public refugeeBusinessesTainedByGenderChartTitle: string = 'Refugee Businesses Trained';

  trainedBusinessesCountDataPerTaType: any;
  trainedBusinessesCountDataPerTaTypeSource: any;
  public displayedColumnsTrainedPerTaType = ['partnerName', 'taType', 'genderCategory', 'businessesTrained' ];
  refugeesAndPlwdtrainedBusinessesCountDataPerGenderSource: any;
  public refugeesAndPlwdDisplayedColumnsTrainedPerGender = ['name', 'value'];

  highLevelSummary: HighLevelSummaryDto = {businessesTrained: '0', businessesLoaned: '0', amountDisbursed: '0', amountDisbursedByTranches: '0'}

  private unsubscribe$ = new Subject<void>();

  @ViewChild('bmoTaNeedsByGenderContentDiv', { static: false }) bmoTaNeedsByGenderContentDiv!: ElementRef;
  constructor(
    private authService: AuthService, 
    private dashBoardService: DashboardService, 
    public gs: GlobalService, 
    public dialog: MatDialog){

  }

  setDashBoardFilters(currentDashBoardFilters: any){
    this.dashBoardFilters = currentDashBoardFilters;
    this.dashBoardFilters['selectedPartnerId'] = this.partnerId;
    this.resetDashBoardFilters = false;
    this.reloadData();
  }

  doResetDashBoardFilters(){
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    this.resetDashBoardFilters = true;
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
    this.getTaTypeTrainedBusinesses();
    this.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary();
  }

  ngOnInit(): void {
    this.partnerId = this.authService.currentUser()?.partnerId;
    this.partnerName = `${this.authService.currentUser()?.partnerName} Dashboard !`;
    this.dashBoardFilters = {'selectedPartnerId': this.authService.currentUser()?.partnerId}
    this.reloadData();
  }

  reloadData(): void {
    this.getHighLevelSummary();
    this.getTaNeedsByGenderSummary();
    this.getTaTrainingBySectorSummary();
    this.getTaTrainingBySegmentSummary();
    this.getBusinessesTrainedByGenderSummary();
    this.getBusinessTrainedTopFourCountiesSummary();
    this.getLastThreeYearsTrainedBusinessesPerPartnerYearly();
    this.getParticipantsEmployeesSummary();
    this.getDisabledBusinessOwnersTrainedByGenderSummary();
    this.getRefugeeBusinessOwnersTrainedByGenderSummary();
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


  expandTANeedsByGenderBarChart(){
    const data = { 
      content: this.bmoTaNeedsByGenderContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.bmoTaNeedsByGenderContentDiv,
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

  valueFormatting = (value: number) => {
    return this.dashBoardService.formatNumberToShortForm(Number(value)); // Outputs as "8.94M"
  };

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

  isTADashboard(): boolean {
    return 'TA' === this.selectedDashboardView;
  }



  public onSelect(event: any) {
    console.log(event);
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
