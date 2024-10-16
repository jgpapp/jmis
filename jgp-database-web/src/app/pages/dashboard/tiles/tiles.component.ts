import { Component, OnDestroy, OnInit } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { HighLevelSummaryDto } from '../dto/highLevelSummaryDto';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { Subject, takeUntil } from 'rxjs';

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
export class TilesComponent implements OnInit, OnDestroy {

  highLevelSummary: HighLevelSummaryDto = {businessesTrained: 0, businessesLoaned: 0, amountDisbursed: 0, outStandingAmount: 0}
  private unsubscribe$ = new Subject<void>();
  constructor(private dashBoardService: DashboardService){

  }

  getHighLevelSummary() {
    this.dashBoardService.getHighLevelSummary()
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.highLevelSummary = response;
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
