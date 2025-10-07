import { ChartOptions } from 'chart.js';

/**
 * Interface for the data passed to the ChartCardComponent
 */
export interface PieChartConfig {
  /** The title displayed in the card header. */
  title: string;
  /** The type of chart (e.g., 'doughnut' or 'pie'). */
  type: 'doughnut' | 'pie';
  /** Labels for each segment. */
  labels: string[];
  /** The actual data sets for the chart. */
  datasets: {
    label: string;
    data: number[];
    backgroundColor: string[];
    borderColor: string[];
    borderWidth: number;
  }[];
  /** Optional: Chart.js options to override defaults. */
  options?: ChartOptions<'doughnut' | 'pie'>;
}