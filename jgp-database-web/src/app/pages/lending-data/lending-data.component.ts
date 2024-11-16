import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginator, MatPaginatorModule } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatSort } from '@angular/material/sort';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { DataUploadComponent } from './data-upload/data-upload.component';
import { LoanService } from '@services/data-management/loan.service';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { GlobalService } from '@services/shared/global.service';
import { SelectionModel } from '@angular/cdk/collections';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { HasPermissionDirective } from '../../directives/has-permission.directive';
import { Subject, takeUntil } from 'rxjs';

@Component({
  selector: 'app-lending-data',
  standalone: true,
  imports: [
    ContentHeaderComponent,
    FlexLayoutModule,
    MatCardModule,
    MatButtonModule,
    MatButtonToggleModule,
    MatIconModule,
    DataUploadComponent,
    MatTableModule,
    MatPaginatorModule,
    NoPermissionComponent,
    MatCheckboxModule,
    HasPermissionDirective
  ],
  templateUrl: './lending-data.component.html',
  styleUrl: './lending-data.component.scss'
})
export class LendingDataComponent implements OnDestroy, OnInit {

  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort!: MatSort;
  public displayedColumns = [
    'select', 'participantName', 'loanNumber', 'pipeLineSource', 
    'loanAmountAccessed', 'loanOutStandingAmount', 
    'loanDuration', 'dateApplied', 'loanStatus'
  ];
  public dataSource: any;

  newLoansData: any
  public selection = new SelectionModel<any>(true, []);

  private unsubscribe$ = new Subject<void>();
  constructor(private loanService: LoanService, public authService: AuthService, private gs: GlobalService) { }

  getAvailableNewLendingData() {
    this.loanService.getAvailableLendingData(false, this.authService.currentUser()?.partnerId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.newLoansData = response;
          this.dataSource = new MatTableDataSource(this.newLoansData);
        },
        error: (error) => { }
      });
  }

  
  ngAfterViewInit() {
    if(this.dataSource){
      this.dataSource.paginator = this.paginator;
      this.dataSource.sort = this.sort;
    }
  }

  ngOnInit(): void {
    this.getAvailableNewLendingData();
  }

  isAllSelected() {
    const numSelected = this.selection.selected.length;
    const numRows = this.dataSource.data.length;
    return numSelected === numRows;
  }

  masterToggle() {
    this.isAllSelected() ?
      this.selection.clear() :
      this.dataSource.data.forEach((row: any) => this.selection.select(row));
  }

  approveLoansData(loanIds: number[]){
    this.loanService.approveLoansData(loanIds)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.gs.openSnackBar(response.message, "Dismiss");
          this.getAvailableNewLendingData();
        },
        error: (error) => { }
      });
  }

  approveSelectedRows(){
    this.approveLoansData(this.selection.selected.map(row => row.id));
  }

  approveAllPartnerLoansData(){
    this.approveLoansData([]);
  }


  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
