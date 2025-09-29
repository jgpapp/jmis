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
import domtoimage from 'dom-to-image';
import { ExportAsService, ExportAsConfig, ExportAsModule } from 'ngx-export-as';
import { KenyanMapComponent } from "../dashboard/kenyan-map/kenyan-map.component";
import { KeLeafletMapComponent } from '../ke-leaflet-map/ke-leaflet-map.component';

@Component({
    selector: 'app-chart-dialog',
    imports: [
        PieChartComponent,
        FlexLayoutModule,
        MatCardModule,
        MatIconModule,
        NgxChartsModule,
        MatButtonModule,
        ExportAsModule,
        KenyanMapComponent,
        KeLeafletMapComponent
    ],
    templateUrl: './chart-dialog.component.html',
    styleUrl: './chart-dialog.component.scss'
})
export class ChartDialogComponent implements AfterViewInit {
  @ViewChild('dialogContentContainer', { static: true }) dialogContentContainer!: ElementRef;
  @ViewChild('chartContainer', { static: true }) chartContainerRef!: ElementRef;

  exportPNGConfig: ExportAsConfig = {
    type: 'png', // the file format: PDF, PNG, etc.
    elementIdOrContent: 'chartDiv', // The ID of the div or component you want to export
    options: { // jsPDF options for PDF format
      jsPDF: {
        orientation: 'landscape',
        unit: 'mm',
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


  downloadChart() {
    // Trigger the export using ngx-export-as
    this.exportAsService.save(this.exportPNGConfig, 'chart').subscribe(() => {
      console.log('Chart exported successfully');
    });
  }


  exportAsPDF() {
    const chartDiv = document.getElementById('chartDiv'); // The id of the div you want to capture

    const dashboardHeight = chartDiv!.clientHeight;
    const dashboardWidth = chartDiv!.clientWidth;
    const options = { background: 'white', width: dashboardWidth, height: dashboardHeight };

    domtoimage.toPng(chartDiv!, options).then((imgData: any) => {
         const doc = new jsPDF(dashboardWidth > dashboardHeight ? 'l' : 'p', 'mm', [dashboardWidth, dashboardHeight]);
         const imgProps = doc.getImageProperties(imgData);
         const pdfWidth = doc.internal.pageSize.getWidth();
         const pdfHeight = (imgProps.height * pdfWidth) / imgProps.width;

         doc.addImage(imgData, 'PNG', 0, 0, pdfWidth, pdfHeight);
         doc.save('Dashboard_Chart.pdf');
    });
}

exportAsPNG() {
  const chartDiv = document.getElementById('chartDiv'); // The id of the div you want to capture

  html2canvas(chartDiv!, { scale: 2 }).then(function(canvas) {
    // Convert the canvas to blob
    canvas.toBlob(function(blob: any){
        // To download directly on browser default 'downloads' location
        let link = document.createElement("a");
        link.download = "chart.png";
        const url = window.URL.createObjectURL(blob);
        link.href = url;
        link.click();

        setTimeout(() => {
          window.URL.revokeObjectURL(url);
          document.body.removeChild(link);
        }, 0);

    },'image/png');
});
}

  public onSelect(event: any) {
    console.log(event);
  }

}
