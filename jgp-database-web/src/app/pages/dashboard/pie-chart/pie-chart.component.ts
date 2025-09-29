import { Component, ElementRef, HostListener, Input, ViewChild, ViewEncapsulation } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { MatCheckboxModule } from '@angular/material/checkbox';

@Component({
    selector: 'app-pie-chart',
    imports: [
        FlexLayoutModule,
        MatCardModule,
        NgxChartsModule,
        MatCheckboxModule
    ],
    templateUrl: './pie-chart.component.html',
    styleUrl: './pie-chart.component.scss',
    encapsulation: ViewEncapsulation.None
})
export class PieChartComponent {

  @Input('data') data: any[];
  @Input('showLegend') showLegend: boolean;
  public gradient = true;
  @Input('colorScheme') colorScheme: any;
  @Input('showLabels') showLabels: boolean;
  @Input('explodeSlices') explodeSlices: boolean;
  @Input('doughnut') doughnut: boolean;
  @Input('chartTitle') chartTitle: string;
  @Input('chartView') chartView: [number, number];
  @Input('chartFormatLabel') chartFormatLabel: any;
  @Input('labelFormatting') labelFormatting: any;
  //@ViewChild('resizedDiv') resizedDiv: ElementRef;
  //public previousWidthOfResizedDiv: number = 0;


  public onSelect(event: any) {
    console.log(event);
    this.setChartView(); // Set initial chart size
  }

   // Dynamically adjust the chart view size based on the window size
   @HostListener('window:resize', ['$event'])
   onResize(event?: any) {
     this.setChartView();
   }

    // Set chart size based on container or window size
  setChartView(): void {
    const container = document.querySelector('.doughnut-chart-container');
    if (container) {
      const width = container.clientWidth;
      const height = container.clientHeight;
      this.chartView = [width, height]; // Set chart size to fill container
    }
  }

  ngAfterViewChecked() {
    /*if (this.previousWidthOfResizedDiv != this.resizedDiv.nativeElement.clientWidth) {
      //setTimeout(() => this.data = [...this.data]);
    }
    this.previousWidthOfResizedDiv = this.resizedDiv.nativeElement.clientWidth;*/
  }
}

