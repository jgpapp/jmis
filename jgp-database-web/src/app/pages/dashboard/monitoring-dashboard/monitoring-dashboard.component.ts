import { AfterViewInit, Component, ElementRef, Input, OnDestroy, OnInit, SimpleChanges, ViewChild } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';
import { MatRadioModule } from '@angular/material/radio';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { Subject, takeUntil } from 'rxjs';

@Component({
  selector: 'app-monitoring-dashboard',
  standalone: true,
  imports: [
    FlexLayoutModule,
    NgxChartsModule,
    FormsModule,
    MatRadioModule,
    MatIconModule
  ],
  templateUrl: './monitoring-dashboard.component.html',
  styleUrl: './monitoring-dashboard.component.scss'
})
export class MonitoringDashboardComponent implements AfterViewInit, OnInit, OnDestroy {

 public chartSColorScheme: any = {
    domain: ['#2F3E9E', '#D22E2E', '#378D3B', '#7f7f7f', '#c4a678', '#6a7b6a', '#191919', '#3d144c', '#f0e1dc', '#a04324', '#00ffff', '#0e5600', '#0e9697']
  };

questions = [
  { value: 'survey_language', text: 'What language do you wish to proceed with?' },
  { value: 'gender', text: 'Record the gender of the respondent' },
  { value: 'county_name', text: 'Record the county the business is located in.' },
  { value: 'business_setting', text: 'Select the Setting of the business location.' },
  { value: 'education_level', text: 'What is your highest level of education?' },
  { value: 'business_age', text: 'How old is your business?' },
  { value: 'regular_employees', text: 'How many regular employees do you have in your business?' },
  { value: 'casual_employees', text: 'How many casual employees do you have in your business?' },
  { value: 'household_income_change', text: 'How has your household income changed in the last 6 months since engaging with the Jiinue Growth Program?' },
  { value: 'financial_stability', text: 'How has the financial stability of your household changed since engaging with Jiinue?' },
  { value: 'group_membership', text: 'Do you belong to any of these groups of people?' },
  { value: 'quality_of_life', text: 'Has Quality of life improved?' },
  { value: 'empowerment', text: 'Do you feel more empowered?' },
  { value: 'voice_in_community', text: 'Voice heard in community?' },
  { value: 'respect_in_community', text: 'Feel respected in the community?' },
  { value: 'reliable_income', text: 'Reliable income' },
  { value: 'reputable_work', text: 'Work is reputable' },
  { value: 'sense_of_purpose', text: 'Sense of purpose' },
  { value: 'business_sector_growth', text: 'Participate in business sector growth' },
  { value: 'community_growth', text: 'Participate in community growth' },
  { value: 'work_opportunities', text: 'Access work opportunities' },
  { value: 'income_regularity', text: 'Regularity of business income' },
  { value: 'income_sufficiency', text: 'Income meets basic needs' },
  { value: 'income_predictability', text: 'Predictability of business income' },
  { value: 'financial_security', text: 'Financial security from work' }
];

questionsMap: any = new Map();

parentWidth = 0;
parentHeight = 0;
public questionOption: string = 'survey_language';
monitoringData: any[] = [];
private unsubscribe$ = new Subject<void>();

@ViewChild('chartContainer', { static: true }) chartContainer!: ElementRef;
@Input('dashBoardFilters') dashBoardFilters: any;

constructor(private dashboardService: DashboardService, public dialog: MatDialog) {
}

ngAfterViewInit() {
  setTimeout(() => {
    const width = this.chartContainer.nativeElement.offsetWidth;
    const height = this.chartContainer.nativeElement.offsetHeight;
    this.parentWidth = width;
    this.parentHeight = height;
  });
}

ngOnChanges(changes: SimpleChanges): void {
    if (changes['dashBoardFilters']) {
      this.dashBoardFilters = changes['dashBoardFilters']['currentValue']
      this.getOutcomeMonitoringSummary();
    }
    
  }

public chooseQuestion() {
    this.getOutcomeMonitoringSummary();
  }


  getOutcomeMonitoringSummary() {
      this.dashboardService.getOutcomeMonitoringSummary(this.dashBoardFilters, this.questionOption)
      .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
            this.monitoringData = response;
          },
          error: (error) => { }
        });
    }

  ngOnInit() {
    this.getOutcomeMonitoringSummary();
    for (const q of this.questions) {
      this.questionsMap.set(q.value, q.text);
    }
  }

   expandOutcomeMonitoringSummary(){
    if (!this.monitoringData || this.monitoringData.length < 1) {
      return;
    }
    const data = { 
      content: this.chartContainer.nativeElement.cloneNode(true),
      mapContainerElement: this.chartContainer,
      chartType: 'ngx-charts-bar-horizontal',
      chartData: this.monitoringData,
      chartGradient: false,
      chartShowXAxis: true,
      chartShowYAxis: true,
      chartSColorScheme: this.chartSColorScheme,
      chartShowLegend: true,
      chartShowXAxisLabel: true,
      chartShowYAxisLabel: true,
      chartYAxisLabel: "Outcome Monitoring",
      chartXAxisLabel: "Count",
      chartFormatLabel: this.valueFormatting,
      chartTitle: this.questionsMap.get(this.questionOption) || 'Outcome Monitoring Summary'
    };
    this.dashboardService.openExpandedChartDialog(this.dialog, data);
  }

    valueFormatting = (value: number) => {
    return this.dashboardService.formatNumberToShortForm(Number(value)); // Outputs as "8.94M"
  };

  ngOnDestroy() {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
