import { AfterViewInit, Component, DestroyRef, ElementRef, inject, Input, OnChanges, signal, SimpleChanges, ViewChild } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ChartConfiguration, ChartOptions, Chart } from 'chart.js';
import { BarChartConfig } from '../models/bar-chart-config.model';

@Component({
  selector: 'app-bar-chart',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './bar-chart.component.html',
  styleUrls: ['./bar-chart.component.scss']
})
export class BarChartComponent implements AfterViewInit, OnChanges{
  @Input({ required: true }) config!: BarChartConfig;

  @ViewChild('chartCanvas') private chartCanvas!: ElementRef<HTMLCanvasElement>;

  private chartInstance: Chart | undefined;

  // Internal signal to manage the config and trigger reactive updates
  configSignal = signal<BarChartConfig>({} as BarChartConfig);
  
  private destroyRef = inject(DestroyRef);

   constructor() {
    this.destroyRef.onDestroy(() => {
        this.chartInstance?.destroy();
    });
  }

  /**
   * Updates the internal signal when the @Input() changes.
   */
  ngOnChanges(changes: SimpleChanges): void {
    if (changes['config'] && this.config) {
      this.configSignal.set(this.config);
      if (this.chartInstance) {
        this.updateChart();
      }
    }
  }

  /**
   * Initializes the chart after the view has been initialized.
   */
  ngAfterViewInit(): void {
    if (this.configSignal().labels) {
      this.createChart();
    }
  }

  /**
   * Determines the Chart.js axis configuration based on the orientation flag.
   */
  private getChartOptions(config: BarChartConfig): ChartOptions<'bar'> {
    const isHorizontal = config.orientation === 'horizontal';
    
    // Default options (can be overridden by config.options)
    const defaultOptions: ChartOptions<'bar'> = {
      responsive: true,
      maintainAspectRatio: false,
      indexAxis: isHorizontal ? 'y' : 'x', // KEY for horizontal/vertical flip
      plugins: {
        legend: { 
          position: 'top', display: config.showLegend ?? true
        },
        title: { display: false }, // Title is displayed in H2 tag
      },
      scales: {
        x: {
          beginAtZero: true,
          title: {
            display: !isHorizontal,
            text: isHorizontal ? 'Value' : 'Category'
          }
        },
        y: {
          beginAtZero: true,
          title: {
            display: isHorizontal,
            text: isHorizontal ? 'Category' : 'Value'
          }
        }
      }
    };
    
    // Merge provided options with defaults
    return { ...defaultOptions, ...config.options } as ChartOptions<'bar'>;
  }

  /**
   * Creates a new Chart.js instance.
   */
  private createChart(): void {
    const config = this.configSignal();
    if (!this.chartCanvas || !config.labels) {
      return;
    }
    
    this.chartInstance?.destroy();

    const chartConfig: ChartConfiguration<'bar'> = {
      type: 'bar',
      data: {
        labels: config.labels,
        datasets: config.datasets.map(ds => ({
          ...ds,
          type: 'bar', // Ensure Chart.js knows the type
        })),
      },
      options: this.getChartOptions(config)
    };

    this.chartInstance = new Chart(this.chartCanvas.nativeElement, chartConfig);
  }

  /**
   * Updates the chart with new data and options.
   */
  private updateChart(): void {
    if (this.chartInstance) {
      const config = this.configSignal();
      this.chartInstance.data.labels = config.labels;
      this.chartInstance.data.datasets = config.datasets;
      
      // Update orientation by changing indexAxis in options
      this.chartInstance.options.indexAxis = config.orientation === 'horizontal' ? 'y' : 'x';
      
      this.chartInstance.update();
    } else {
      this.createChart();
    }
  }
}

