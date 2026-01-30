import { Component, Input, SimpleChanges } from '@angular/core';
import { MatCardModule } from "@angular/material/card";
import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { MatFormFieldModule } from "@angular/material/form-field";
import { MatSelectModule } from "@angular/material/select";
import { MatButtonToggleModule } from "@angular/material/button-toggle";
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { forkJoin, merge, skip } from 'rxjs';
import { ActivatedRoute } from '@angular/router';
import { Dates } from '@services/dates';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { 
  Chart, 
  BarController, 
  BarElement, 
  CategoryScale, 
  LinearScale, 
  Tooltip, 
  Legend 
} from 'chart.js';
import { MatButtonModule } from '@angular/material/button';

// Register Chart.js components
Chart.register(
  BarController,
  BarElement,
  CategoryScale,
  LinearScale,
  Tooltip,
  Legend
);


@Component({
  selector: 'app-participant-trends',
  standalone: true,
  imports: [
    MatCardModule,
    FontAwesomeModule,
    MatFormFieldModule,
    MatSelectModule,
    MatButtonToggleModule,
    MatButtonModule,
    FormsModule,
    CommonModule
],
  templateUrl: './participant-trends.component.html',
  styleUrl: './participant-trends.component.scss'
})
export class ParticipantTrendsComponent {

  @Input('dashBoardFilters') dashBoardFilters: any;
  private _selectedDashboardMode: string = 'DAILY';
  /** Chart.js chart */
    chart: any;
    /** Substitute for resolver */
    hideOutput = true;

  constructor(
    private route: ActivatedRoute, 
    private dateUtils: Dates,
  private dashboardService: DashboardService) {
    
    
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['dashBoardFilters']) {
      this.dashBoardFilters = changes['dashBoardFilters']['currentValue']
      this.getChartData();
    }
    
  }


get selectedDashboardMode(): string {
  return this._selectedDashboardMode;
}

set selectedDashboardMode(value: string) {
  this._selectedDashboardMode = value;
  this.getChartData();
}

  ngOnInit() {
      this.getChartData();
    }

  /**
   * Subscribes to value changes of officeID and timescale controls,
   * Fetches data accordingly and sets charts based on fetched data.
   */
  getChartData() {
    console.log('Fetching participant trends data for timescale:', this.selectedDashboardMode);
    merge(this.selectedDashboardMode).pipe(skip(1))
      .subscribe(() => {
        switch (this.selectedDashboardMode) {
          case 'DAILY':
            const trainedParticipantsByDay = this.dashboardService.getBusinessesTrainedByTimeScale(this.dashBoardFilters, this.selectedDashboardMode);
            const disbursedLoansByDay = this.dashboardService.getLoanDisbursedByTimeScale(this.dashBoardFilters, this.selectedDashboardMode);
            forkJoin({trainedParticipantsByDay, disbursedLoansByDay}).subscribe((data: any) => {
              const dayLabels = this.getLabels(data.trainedParticipantsByDay);
              const businessesTrained = this.getCounts(data.trainedParticipantsByDay, dayLabels);
              const disbursedLoans = this.getCounts(data.disbursedLoansByDay, dayLabels);
              this.setChart(dayLabels, businessesTrained, disbursedLoans);
              this.hideOutput = dayLabels.length === 0;
            });
            break;
          case 'WEEKLY':
            const trainedParticipantsByWeek = this.dashboardService.getBusinessesTrainedByTimeScale(this.dashBoardFilters, this.selectedDashboardMode);
            const disbursedLoansByWeek = this.dashboardService.getLoanDisbursedByTimeScale(this.dashBoardFilters, this.selectedDashboardMode);
            forkJoin({trainedParticipantsByWeek, disbursedLoansByWeek}).subscribe((data: any) => {
              const weekLabels = this.getLabels(data.trainedParticipantsByWeek);
              const businessesTrained = this.getCounts(data.trainedParticipantsByWeek, weekLabels);
              const disbursedLoans = this.getCounts(data.disbursedLoansByWeek, weekLabels);
              this.setChart(weekLabels, businessesTrained, disbursedLoans);
              this.hideOutput = weekLabels.length === 0;
            });
            break;
          case 'MONTHLY':
            const trainedParticipantsByMonth = this.dashboardService.getBusinessesTrainedByTimeScale(this.dashBoardFilters, this.selectedDashboardMode);
            const disbursedLoansByMonth = this.dashboardService.getLoanDisbursedByTimeScale(this.dashBoardFilters, this.selectedDashboardMode);
            forkJoin({trainedParticipantsByMonth, disbursedLoansByMonth}).subscribe((data: any) => {
              const monthLabels = this.getLabels(data.trainedParticipantsByMonth);
              const businessesTrained = this.getCounts(data.trainedParticipantsByMonth, monthLabels);
              const disbursedLoans = this.getCounts(data.disbursedLoansByMonth, monthLabels);
              this.setChart(monthLabels, businessesTrained, disbursedLoans);
              this.hideOutput = monthLabels.length === 0;
            });
            break;
            case 'YEARLY':
            const trainedParticipantsByYear = this.dashboardService.getBusinessesTrainedByTimeScale(this.dashBoardFilters, this.selectedDashboardMode);
            const disbursedLoansByYear = this.dashboardService.getLoanDisbursedByTimeScale(this.dashBoardFilters, this.selectedDashboardMode);
            forkJoin({trainedParticipantsByYear, disbursedLoansByYear}).subscribe((data: any) => {
              const yearLabels = this.getLabels(data.trainedParticipantsByYear);
              const businessesTrained = this.getCounts(data.trainedParticipantsByYear, yearLabels);
              const disbursedLoans = this.getCounts(data.disbursedLoansByYear, yearLabels);
              this.setChart(yearLabels, businessesTrained, disbursedLoans);
              this.hideOutput = yearLabels.length === 0;
            });
            break;
        }
    });
  }

  /**
   * Gets Abscissa Labels.
   * @param {string} data User's timescale choice.
   */
  getLabels(data: any) {
    return data.map((entry: any) => entry.name);
  }

  /**
   * Get bar heights for clients/loans trends.
   * @param {any[]} data API response array.
   * @param {any[]} labels Abscissa Labels.
   */
  getCounts(data: any[], labels: any[]) {
    let counts: number[]  = [];
    labels.forEach((label: any) => {
          const dataKey = data.find((entry: any) => entry.name === label);
          counts.push(dataKey ? dataKey.value : 0);
        });
    return counts;
  }

  /**
   * Creates an instance of Chart.js multi-bar chart.
   * Refer: https://www.chartjs.org/docs/latest/charts/bar.html for configuration details.
   * @param {any[]} labels Abscissa Labels.
   * @param {number[]} businessesTrained Businesses Trained Ordinate.
   * @param {number[]} disbursedLoans Loans Ordinate.
   */
  setChart(labels: any[], businessesTrained: number[], disbursedLoans: number[]) {
    if (!this.chart) {
      this.chart = new Chart('participant-trends-bar', {
        type: 'bar',
        data: {
          labels: labels,
          datasets: [
            {
              label: 'Businesses Trained',
              backgroundColor: 'dodgerblue',
              data: businessesTrained
            },
            {
              label: 'Loans Disbursed',
              backgroundColor: 'green',
              data: disbursedLoans
            }
          ]
        },
        options: {
          layout: {
            padding: {
              top: 5,
              left: 10,
              right: 10
            }
          }
        }
      });
    } else {
      this.chart.data.labels = labels;
      this.chart.data.datasets[0].data = businessesTrained;
      this.chart.data.datasets[1].data = disbursedLoans;
      this.chart.update();
    }
  }

}
