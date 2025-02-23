import { FlatTreeControl } from '@angular/cdk/tree';
import { Component, Input, OnInit } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { MatTableModule } from '@angular/material/table';
import { MatTreeFlattener, MatTreeFlatDataSource } from '@angular/material/tree';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { GlobalService } from '@services/shared/global.service';
import { Subject, takeUntil } from 'rxjs';
import { CommonModule, DecimalPipe } from '@angular/common';
import { AuthService } from '@services/users/auth.service';

interface PerformanceSummaryDto {
  year: number | undefined;
  month: number | undefined;
  partner: string | undefined;
  quarter: string | undefined;
  category: string | undefined;
  businessesTrained: number | undefined;
  businessesLoaned: number | undefined;
  amountDisbursed: number | undefined;
  outStandingAmount: number | undefined;
  children: PerformanceSummaryDto[];
}

interface PerformanceSummaryFlatNode {
  expandable: boolean;
  year: number | undefined;
  month: number | undefined;
  partner: string | undefined;
  quarter: string | undefined;
  category: string | undefined;
  businessesTrained: number | undefined;
  businessesLoaned: number | undefined;
  amountDisbursed: number | undefined;
  outStandingAmount: number | undefined;
  level: number;
}


@Component({
  selector: 'app-performance-summary',
  standalone: true,
  imports: [
    MatTableModule,
    MatIconModule,
    FlexLayoutModule,
    MatCardModule,
    CommonModule
  ],
  templateUrl: './performance-summary.component.html',
  styleUrl: './performance-summary.component.scss'
})
export class PerformanceSummaryComponent implements OnInit {

  @Input({alias: 'selectedDashboardView', required: true}) selectedDashboardView: string

  fiDisplayedColumns: string[] = ['category',  'businessesLoaned', 'amountDisbursed', 'outStandingAmount' ];
  taDisplayedColumns: string[] = ['category', 'businessesTrained'];
  private transformer = (node: PerformanceSummaryDto, level: number) => {
    return {
      expandable: !!node.children && node.children.length > 0,
      year: node.year,
      month: node.month,
      partner: node.partner,
      quarter: node.quarter,
      category: node.category,
      businessesTrained: node.businessesTrained,
      businessesLoaned: node.businessesLoaned,
      amountDisbursed: node.amountDisbursed,
      outStandingAmount: node.outStandingAmount,
      level: level
    };
  }
  treeControl = new FlatTreeControl<PerformanceSummaryFlatNode>(node => node.level, node => node.expandable); 

  treeFlattener = new MatTreeFlattener(
    this.transformer, 
    node => node.level, 
    node => node.expandable, 
    node => node.children
  );

  dataSource = new MatTreeFlatDataSource(this.treeControl, this.treeFlattener);

  private unsubscribe$ = new Subject<void>();
  constructor(private dashBoardService: DashboardService, public gs: GlobalService, private authService: AuthService){}

  ngOnInit(): void {
    this.getPerformanceSummary();
  }

  getPerformanceSummary() {
    this.dashBoardService.getPerformanceSummary(undefined, this.authService.currentUser()?.partnerId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.dataSource.data = response; // Set the hierarchical data
        },
        error: (error) => { }
      });
  }

  isFinancialDashboard(): boolean {
    return 'FI' === this.selectedDashboardView;
  }

  isTADashboard(): boolean {
    return 'TA' === this.selectedDashboardView;
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

}
