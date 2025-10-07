import { CommonModule } from '@angular/common';
import { AfterViewInit, Component, DestroyRef, effect, ElementRef, inject, Input, OnChanges, signal, SimpleChanges, ViewChild } from '@angular/core';
import { BaseChartDirective } from 'ng2-charts';
import { Chart, ChartConfiguration, ChartType, ChartOptions } from 'chart.js';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { PieChartConfig } from '../models/pie-chart-config.model';
import ChartDataLabels from 'chartjs-plugin-datalabels'; 


@Component({
  selector: 'app-doughnut',
  imports: [
    CommonModule, 
    BaseChartDirective,
    FlexLayoutModule,
    MatCardModule,
    MatIconModule
  ],
  templateUrl: './doughnut.component.html',
  styleUrl: './doughnut.component.scss'
})
export class DoughnutComponent implements AfterViewInit, OnChanges{

  @Input() data: { name: string, value: number }[] = [];
  @Input() chartTitle: string = 'Test Doughnut Chart';
  @Input() chartSColorScheme: any;
  @Input({ required: true }) config!: PieChartConfig;
  @ViewChild('chartCanvas') private chartCanvas!: ElementRef<HTMLCanvasElement>;
  private chartInstance: Chart | undefined;

 // Use a signal internally to manage the config (since @Input is not yet a signal)
  configSignal = signal<PieChartConfig | undefined>(undefined);

  constructor() {
    Chart.register(ChartDataLabels); 
    const destroyRef = inject(DestroyRef);
    
    // Auto-destroy the chart instance when the component is destroyed
    destroyRef.onDestroy(() => {
        this.chartInstance?.destroy();
    });

    // Use effect to automatically update the chart when the config signal changes
    effect(() => {
        const currentConfig = this.configSignal();
        if (currentConfig && this.chartInstance) {
            this.updateChart(currentConfig);
        } else if (currentConfig) {
            // If instance doesn't exist yet, it will be created in ngAfterViewInit
        }
    });
  }
  


  /**
   * Updates the internal signal when the @Input() changes.
   */
  ngOnChanges(changes: SimpleChanges): void {
    if (changes['config'] && this.config) {
      this.configSignal.set(this.config);
    }
  }

  /**
   * Initializes the chart after the view has been initialized.
   */
  ngAfterViewInit(): void {
    if (this.configSignal()) {
      this.createChart(this.configSignal()!);
    }
  }

  /**
   * Creates a new Chart.js instance.
   */
  private createChart(currentConfig: PieChartConfig): void {
    if (!this.chartCanvas) return;

    // Destroy existing chart to prevent memory leaks/multiple renders
    this.chartInstance?.destroy();

    const chartConfig: ChartConfiguration<'doughnut' | 'pie'> = {
      type: currentConfig.type,
      data: {
        labels: currentConfig.labels,
        datasets: currentConfig.datasets,
      },
      options: currentConfig.options as ChartOptions<'doughnut' | 'pie'>
    };

    this.chartInstance = new Chart(this.chartCanvas.nativeElement, chartConfig);
  }

  /**
   * Updates the chart with new data without full re-creation.
   */
  private updateChart(currentConfig: PieChartConfig): void {
    if (this.chartInstance) {
      this.chartInstance.data.labels = currentConfig.labels;
      this.chartInstance.data.datasets = currentConfig.datasets;
      // Optional: Deep merge options if needed, but for simplicity, we just update data
      this.chartInstance.update();
    } else {
      // Recreate if no instance exists (shouldn't happen after init, but safe to check)
      this.createChart(currentConfig);
    }
  }

  
}
