import { Component, ElementRef, Input, OnChanges, OnDestroy, OnInit, SimpleChanges, ViewChild } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatDialog } from '@angular/material/dialog';
import { analytics } from '@data/dashboard-data';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { GlobalService } from '@services/shared/global.service';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { Subject } from 'rxjs';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';

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
export class AnalyticsComponent implements OnInit, OnChanges, OnDestroy {
  @Input('dashBoardFilters') dashBoardFilters: any;
  public chartSColorScheme: any = {
    domain: ['#2F3E9E', '#D22E2E', '#378D3B', '#7f7f7f', '#c4a678', '#6a7b6a', '#191919', '#3d144c', '#f0e1dc', '#a04324', '#00ffff', '#0e5600', '#0e9697']
  };
  public autoScale = true;
  public roundDomains = true;
  public gradient = false;
  

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
  subs = new SubscriptionsContainer();

  constructor(private dashBoardService: DashboardService, private gs: GlobalService, private dialog: MatDialog){
    
  }
  ngOnDestroy(): void {
    this.subs.dispose();
  }

  ngOnChanges(changes: SimpleChanges): void {
    this.dashBoardFilters = changes['dashBoardFilters']['currentValue']
    this.reloadData();
  }

  ngOnInit() {
    this.reloadData();
  }

  reloadData() {
    this.analytics = analytics;
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

}
