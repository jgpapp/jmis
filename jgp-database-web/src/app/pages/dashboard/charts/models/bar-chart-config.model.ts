import { ChartOptions } from 'chart.js';

/**
 * Interface for the data passed to the BarChartComponent
 */
export interface BarChartConfig {
  /** The title displayed above the chart. */
  title: string;
  /** Labels for the X or Y axis categories. */
  labels: string[];
  /** Orientation: 'vertical' or 'horizontal'. */
  orientation: 'vertical' | 'horizontal';
  /** The data sets for the chart. */
  datasets: {
    label: string;
    data: number[];
    backgroundColor: string[];
    borderColor: string;
    borderWidth: number;
  }[];
  /** Optional: Chart.js options to override defaults. */
  options?: ChartOptions<'bar'>;
  showLegend?: boolean ;
}