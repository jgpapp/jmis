import { Component } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { AsyncPipe } from '@angular/common';
import { MatDividerModule } from '@angular/material/divider';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { HasPermissionDirective } from '../../../directives/has-permission.directive';
import { map, Observable, Subject, takeUntil } from 'rxjs';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { LoanService } from '@services/data-management/loan.service';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';

@Component({
    selector: 'app-loan-details',
    imports: [
        MatCardModule,
        ContentHeaderComponent,
        FlexLayoutModule,
        MatButtonModule,
        MatIconModule,
        MatFormFieldModule,
        AsyncPipe,
        MatDividerModule,
        RouterModule,
        MatTableModule,
        MatPaginatorModule,
        HasPermissionDirective
    ],
    templateUrl: './loan-details.component.html',
    styleUrl: './loan-details.component.scss'
})
export class LoanDetailsComponent {

  public loansTransactionsDisplayedColumns = ['tranch', 'transactionType', 'transactionDate', 'amount', 'dateUploaded', 'uploadedBy', 'dateApproved', 'approvedBy'];
    selectedLoan: Observable<any>;
    loanTransactions: any;
    dataSource: any;
    loanId: number;
    pageSize = 10;
    pageIndex = 0;
    totalItems = 0;
    subs = new SubscriptionsContainer();
    constructor(private activatedRoute: ActivatedRoute, private dashBoardService: DashboardService, private loanService: LoanService){}

    ngOnInit(): void {
        this.selectedLoan = this.activatedRoute.data.pipe(map(data => data['selectedLoan']));
        this.getLoanTransactions();
      }

      getLoanTransactions() {
        this.selectedLoan.subscribe({
          next: (response) => {
            this.loanId = response.id;
            if (this.loanId) {
              this.subs.add = this.loanService.getLoanTransactions(this.pageIndex, this.pageSize, false, undefined, this.loanId)
                .subscribe({
                  next: (response) => {
                    this.loanTransactions = response.content;
                    this.dataSource = new MatTableDataSource(this.loanTransactions);
                    this.totalItems = response.totalElements;
                  },
                  error: (error) => { }
                });
              }
          },
          error: (error) => { }
        });;
        
        }

        onPageChange(event: PageEvent) {
              this.pageIndex = event.pageIndex;
              this.pageSize = event.pageSize;
              this.getLoanTransactions();
            }
        
        
          ngOnDestroy(): void {
            this.subs.dispose();
          }
}
