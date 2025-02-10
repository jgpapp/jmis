import { FlatTreeControl } from '@angular/cdk/tree';
import { Component, OnInit } from '@angular/core';
import { MatIconModule } from '@angular/material/icon';
import { MatTableModule } from '@angular/material/table';
import { MatTreeFlattener, MatTreeFlatDataSource } from '@angular/material/tree';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { Subject, takeUntil } from 'rxjs';

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
    MatIconModule
  ],
  templateUrl: './performance-summary.component.html',
  styleUrl: './performance-summary.component.scss'
})
export class PerformanceSummaryComponent implements OnInit {

  //treeFlattener: MatTreeFlattener<PerformanceSummaryDto, PerformanceSummaryFlatNode>;
  //dataSource: MatTreeFlatDataSource<PerformanceSummaryDto, PerformanceSummaryFlatNode>;
  displayedColumns: string[] = ['category', 'businessesTrained', 'businessesLoaned', 'amountDisbursed', 'outStandingAmount' ];
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
  constructor(private dashBoardService: DashboardService){}

  //hasChild = (_: number, node: PerformanceSummaryFlatNode) => node.expandable;

  ngOnInit(): void {
    this.getPerformanceSummary();
  }

  getPerformanceSummary() {
    this.dashBoardService.getPerformanceSummary(undefined, undefined)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          console.log(response);
          this.dataSource.data = response; // Set the hierarchical data
        },
        error: (error) => { }
      });
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

}
