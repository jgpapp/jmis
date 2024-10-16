import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatDialog } from '@angular/material/dialog';
import { analytics } from '@data/dashboard-data';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { GlobalService } from '@services/shared/global.service';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { Subject, takeUntil, takeWhile } from 'rxjs';

@Component({
  selector: 'app-analytics',
  standalone: true,
  imports: [
    MatCardModule,
    NgxChartsModule
  ],
  templateUrl: './analytics.component.html',
  styleUrl: './analytics.component.scss'
})
export class AnalyticsComponent implements OnInit, OnDestroy {
  public chartSColorScheme: any = {
    domain: ['#2F3E9E', '#D22E2E', '#378D3B', '#7f7f7f', '#c4a678', '#6a7b6a', '#191919', '#3d144c', '#f0e1dc', '#a04324', '#00ffff', '#0e5600', '#0e9697']
  };
  public autoScale = true;
  public roundDomains = true;
  public gradient = false;

  public lastThreeYearLoansAccessedPerPartner: any[];
  public lastThreeYearLoansAccessedPerPartnerShowXAxis = true;
  public lastThreeYearLoansAccessedPerPartnerShowYAxis = true;
  public lastThreeYearLoansAccessedPerPartnerShowLegend = false;
  public lastThreeYearLoansAccessedPerPartnerShowXAxisLabel = true;
  public lastThreeYearLoansAccessedPerPartnerXAxisLabel = 'Year';
  public lastThreeYearLoansAccessedPerPartnerShowYAxisLabel = true;
  public lastThreeYearLoansAccessedPerPartnerYAxisLabel = 'Accessed Loans';
  public lastThreeYearLoansAccessedPerPartnerTitle = 'Loans Accessed Per Partner Vs Last 2 Years';
  

  public analytics: any[];
  public showXAxis = true;
  public showYAxis = true;
  public showLegend = false;
  public showXAxisLabel = true;
  public xAxisLabel = 'Year';
  public showYAxisLabel = true;
  public yAxisLabel = 'Placeholder';

  
  @ViewChild('resizedDiv') resizedDiv: ElementRef;
  public previousWidthOfResizedDiv: number = 0;
  private unsubscribe$ = new Subject<void>();

  @ViewChild('lastThreeYearLoansAccessedPerPartnerContentDiv', { static: true }) lastThreeYearLoansAccessedPerPartnerContentDiv!: ElementRef;

  constructor(private dashBoardService: DashboardService, private gs: GlobalService, private dialog: MatDialog){
    
  }
  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

  ngOnInit() {
    this.analytics = analytics;
    this.getLastThreeYearsAccessedLoanPerPartnerSummary();
  }

  getLastThreeYearsAccessedLoanPerPartnerSummary() {
    this.dashBoardService.getLastThreeYearsAccessedLoanPerPartnerSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.lastThreeYearLoansAccessedPerPartner = response;
        },
        error: (error) => { }
      });
  }

  onSelect(event: any) {
    console.log(event);
  }

  ngAfterViewChecked() {
    if (this.previousWidthOfResizedDiv != this.resizedDiv.nativeElement.clientWidth) {
      this.analytics = [...analytics];
    }
    this.previousWidthOfResizedDiv = this.resizedDiv.nativeElement.clientWidth;
  }

  expandLastThreeYearLoansAccessedPerPartnerLineChart(){
    const data = { 
      content: this.lastThreeYearLoansAccessedPerPartnerContentDiv.nativeElement.cloneNode(true),
      mapContainerElement: this.lastThreeYearLoansAccessedPerPartnerContentDiv,
      chartType: 'ngx-charts-line-chart',
      chartData: this.lastThreeYearLoansAccessedPerPartner,
      chartGradient: this.gradient,
      chartShowXAxis: this.lastThreeYearLoansAccessedPerPartnerShowXAxis,
      chartShowYAxis: this.lastThreeYearLoansAccessedPerPartnerShowYAxis,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: this.lastThreeYearLoansAccessedPerPartnerShowXAxisLabel,
      chartShowYAxisLabel: this.lastThreeYearLoansAccessedPerPartnerShowYAxisLabel,
      chartYAxisLabel: this.lastThreeYearLoansAccessedPerPartnerYAxisLabel,
      chartXAxisLabel: this.lastThreeYearLoansAccessedPerPartnerXAxisLabel,
      chartAutoScale: this.autoScale,
      chartRoundDomains: this.roundDomains,
      chartTitle: this.lastThreeYearLoansAccessedPerPartnerTitle,
    };
    this.gs.openExpandedChartDialog(data, this.dialog);
  }

}
