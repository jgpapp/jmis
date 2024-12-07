import { AfterViewInit, Component, ElementRef, Inject, ViewChild } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { PieChartComponent } from '../dashboard/pie-chart/pie-chart.component';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { MatIconModule } from '@angular/material/icon';
import { MatCardModule } from '@angular/material/card';
import { MatButtonModule } from '@angular/material/button';
import html2canvas from 'html2canvas';
import jsPDF from 'jspdf';
import { ExportAsService, ExportAsConfig, ExportAsModule } from 'ngx-export-as';
import { KeMapComponent } from "../dashboard/ke-map/ke-map.component";

@Component({
  selector: 'app-chart-dialog',
  standalone: true,
  imports: [
    PieChartComponent,
    FlexLayoutModule,
    MatCardModule,
    MatIconModule,
    NgxChartsModule,
    MatButtonModule,
    ExportAsModule,
    KeMapComponent
],
  templateUrl: './chart-dialog.component.html',
  styleUrl: './chart-dialog.component.scss'
})
export class ChartDialogComponent implements AfterViewInit {
  @ViewChild('dialogContentContainer', { static: true }) dialogContentContainer!: ElementRef;

  exportAsConfig: ExportAsConfig = {
    type: 'png', // the file format: PDF, PNG, etc.
    elementIdOrContent: 'chartDiv', // The ID of the div or component you want to export
    options: { // jsPDF options for PDF format
      jsPDF: {
        orientation: 'portrait',
        unit: 'px',
        format: 'a4' // You can change this to fit your chart dimensions
      }
    }
  };

  constructor(
    private exportAsService: ExportAsService,
    public dialogRef: MatDialogRef<ChartDialogComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngAfterViewInit(): void {
    // Append the original content from the passed `div` to the dialog content container
    //this.dialogContentContainer.nativeElement.appendChild(this.data.content);
  }

  onCloseClick(): void {
    this.dialogRef.close();
  }

  downloadPDF() {
    const chartDiv = document.getElementById('chartDiv'); // The id of the div you want to capture

    // Use html2canvas to capture the div as an image
    html2canvas(chartDiv!).then((canvas) => {
      const imgData = canvas.toDataURL('image/png');

      // Initialize jsPDF, with orientation, unit of measure, and format
      const pdf = new jsPDF({
        orientation: 'portrait',
        unit: 'px',
        format: [canvas.width, canvas.height]
      });

      // Add the image to the PDF
      pdf.addImage(imgData, 'PNG', 0, 0, canvas.width, canvas.height);

      // Download the PDF
      pdf.save('chart.pdf');
    });
  }

  downloadChart() {
    // Trigger the export using ngx-export-as
    this.exportAsService.save(this.exportAsConfig, 'chart').subscribe(() => {
      console.log('Chart exported successfully');
    });
  }

  public onSelect(event: any) {
    console.log(event);
  }

}
