import { Component, OnDestroy, ViewChild } from '@angular/core';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatSort } from '@angular/material/sort';
import { BMOClientDataService } from '@services/data-management/bmo-client-data.service';
import { NoPermissionComponent } from '../../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { SelectionModel } from '@angular/cdk/collections';
import { HasPermissionDirective } from '../../../directives/has-permission.directive';
import { GlobalService } from '@services/shared/global.service';
import { Subject, takeUntil } from 'rxjs';
import { ConfirmDialogModel } from '../../../dto/confirm-dialog-model';
import { ConfirmDialogComponent } from '../../confirm-dialog/confirm-dialog.component';
import { MatDialog } from '@angular/material/dialog';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';

@Component({
    selector: 'app-data-list',
    imports: [
        ContentHeaderComponent,
        FlexLayoutModule,
        MatCardModule,
        MatButtonModule,
        MatButtonToggleModule,
        MatIconModule,
        MatTableModule,
        MatPaginatorModule,
        NoPermissionComponent,
        MatCheckboxModule,
        HasPermissionDirective
    ],
    templateUrl: './data-list.component.html'
})
export class DataListComponent implements OnDestroy{

  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort!: MatSort;
  public displayedColumns = ['select', 'participantName', 'participantJGPID', 'tasAttended', 'taSessionsAttended', 'isRecommendedForFinance', 'dateFormSubmitted', 'decisionDate', 'dateUploaded', 'uploadedBy'];
  public dataSource: any;

  pageSize = 10;
  pageIndex = 0;
  totalItems = 0;

  bmoClientsData: any
  public selection = new SelectionModel<any>(true, []);

  subs = new SubscriptionsContainer();
  constructor(private bmoClientDataService: BMOClientDataService, public authService: AuthService, private gs: GlobalService, private dialog: MatDialog) { }

  getAvailableBMOClientData() {
    this.subs.add = this.bmoClientDataService.getAvailableBMOClientData(this.pageIndex, this.pageSize, false, this.authService.currentUser()?.partnerId)
      .subscribe({
        next: (response) => {
          this.bmoClientsData = response.content;
          this.dataSource = new MatTableDataSource(this.bmoClientsData);
          this.totalItems = response.totalElements;
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
    this.getAvailableBMOClientData();
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

  approveTAData(bmoIds: number[]): void {
    const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to approve TA data?`, '');
    const dialogRef = this.dialog.open(ConfirmDialogComponent, {
      maxWidth: "400px",
      data: dialogData
    });

    dialogRef.afterClosed().subscribe(dialogResult => {
      if(dialogResult > 0){ // 0 means dialog was dismissed
        this.subs.add = this.bmoClientDataService.approveBMOClientData(bmoIds)
          .subscribe({
            next: (response) => {
              this.gs.openSnackBar(response.message, "Dismiss");
              this.getAvailableBMOClientData();
            },
            error: (error) => { }
        });
      }
    });
  }

  approveSelectedRows(){
    this.approveTAData(this.selection.selected.map(row => row.id));
  }

  approveAllPartnerTAData(){
    this.approveTAData([]);
  }

  rejectTAData(bmoIds: number[]): void {
    const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to reject & delete TA data?`, '');
    const dialogRef = this.dialog.open(ConfirmDialogComponent, {
      maxWidth: "400px",
      data: dialogData
    });

    dialogRef.afterClosed().subscribe(dialogResult => {
      if(dialogResult > 0){ // 0 means dialog was dismissed
        this.subs.add = this.bmoClientDataService.rejectBMOClientData(bmoIds)
          .subscribe({
            next: (response) => {
              this.gs.openSnackBar(response.message, "Dismiss");
              this.getAvailableBMOClientData();
            },
            error: (error) => { }
        });
      }
    });
  }

  rejectSelectedRows(){
    this.rejectTAData(this.selection.selected.map(row => row.id));
  }

  rejectAllPartnerTAData(){
    this.rejectTAData([]);
  }

  onPageChange(event: PageEvent) {
      this.pageIndex = event.pageIndex;
      this.pageSize = event.pageSize;
      this.getAvailableBMOClientData();
    }

  ngOnDestroy(): void {
    this.subs.dispose();
  }
}
