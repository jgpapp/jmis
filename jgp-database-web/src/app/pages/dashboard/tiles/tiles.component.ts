import { Component, Input, OnChanges, OnDestroy, OnInit, SimpleChanges } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { HighLevelSummaryDto } from '../dto/highLevelSummaryDto';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { Subject, takeUntil } from 'rxjs';
import { DashboardTypeFilter } from '../../../dto/dashboard-type-filter';

@Component({
  selector: 'app-tiles',
  standalone: true,
  imports: [
    FlexLayoutModule,
    MatCardModule,
    MatIconModule
  ],
  templateUrl: './tiles.component.html',
  styleUrl: './tiles.component.scss'
})
export class TilesComponent implements OnInit, OnDestroy, OnChanges {

  @Input('dashBoardFilters') dashBoardFilters: any;
  @Input({required: true, alias: 'dashboardTypeFilter'}) dashboardTypeFilter: DashboardTypeFilter;
  highLevelSummary: HighLevelSummaryDto = {businessesTrained: '0', businessesLoaned: '0', amountDisbursed: '0', amountDisbursedByTranches: '0', businessesMentored: '0'};
  private unsubscribe$ = new Subject<void>();
  constructor(private dashBoardService: DashboardService){

  }

  ngOnChanges(changes: SimpleChanges): void {
    
    if (changes['dashboardTypeFilter']) {
      this.dashboardTypeFilter = changes['dashboardTypeFilter']['currentValue']
    }
    if (changes['dashBoardFilters']) {
      this.dashBoardFilters = changes['dashBoardFilters']['currentValue']
    this.getHighLevelSummary();
    }
  }

  getHighLevelSummary() {
    this.dashBoardService.getHighLevelSummary(this.dashBoardFilters)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.highLevelSummary = this.dashBoardService.getFormattedTileData(response);
        },
        error: (error) => { }
      });
  }

  ngOnInit(): void {
    this.getHighLevelSummary();
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

}
