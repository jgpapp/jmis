import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { HasPermissionDirective } from '../../directives/has-permission.directive';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatCardModule } from '@angular/material/card';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { SelectionModel } from '@angular/cdk/collections';
import { MatSort } from '@angular/material/sort';
import { Subject, takeUntil } from 'rxjs';
import { MatDialog } from '@angular/material/dialog';
import { GlobalService } from '@services/shared/global.service';
import { AuthService } from '@services/users/auth.service';
import { ConfirmDialogModel } from '../../dto/confirm-dialog-model';
import { ConfirmDialogComponent } from '../confirm-dialog/confirm-dialog.component';
import { MonitoringServiceService } from '@services/monitoring.service.service';

@Component({
  selector: 'app-outcome-monitoring',
  standalone: true,
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
  templateUrl: './outcome-monitoring.component.html',
  styleUrl: './outcome-monitoring.component.scss'
})
export class OutcomeMonitoringComponent implements OnDestroy, OnInit {

    @ViewChild(MatPaginator) paginator: MatPaginator;
    @ViewChild(MatSort) sort!: MatSort;
    public displayedColumns = [
      'select', 'surveyDate', 'surveyLanguage', 'consented', 'gender', 
      'countyName', 'businessAge', 'dateUploaded', 'uploadedBy'
    ];
    public dataSource: any;
  
    pageSize = 10;
    pageIndex = 0;
    totalItems = 0;
  
    newMonitoringData: any
    public selection = new SelectionModel<any>(true, []);
  
    private unsubscribe$ = new Subject<void>();

    constructor(private monitoringService: MonitoringServiceService, public authService: AuthService, private gs: GlobalService, private dialog: MatDialog) { }
    
      getOutComeMonitoringDataRecords() {
        const partnerId = this.authService.currentUser()?.partnerId;
        if (partnerId) {
        this.monitoringService.getOutComeMonitoringDataRecords(this.pageIndex, this.pageSize, false)
        .pipe(takeUntil(this.unsubscribe$))
          .subscribe({
            next: (response) => {
              this.newMonitoringData = response.content;
              this.dataSource = new MatTableDataSource(this.newMonitoringData);
              this.totalItems = response.page.totalElements;
            },
            error: (error) => { }
          });
        }
      }
    
      
      ngAfterViewInit() {
        if(this.dataSource){
          this.dataSource.paginator = this.paginator;
          this.dataSource.sort = this.sort;
        }
      }
    
      ngOnInit(): void {
        this.getOutComeMonitoringDataRecords();
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
    
    
      approvedMonitoringData(monitoringIds: number[]): void {
        const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to approve monitorng data?`);
        const dialogRef = this.dialog.open(ConfirmDialogComponent, {
          maxWidth: "400px",
          data: dialogData
        });
    
        dialogRef.afterClosed().subscribe(dialogResult => {
          if(dialogResult){
            this.monitoringService.approvedMonitoringData(monitoringIds)
            .pipe(takeUntil(this.unsubscribe$))
              .subscribe({
                next: (response) => {
                  this.gs.openSnackBar(response.message, "Dismiss");
                  this.getOutComeMonitoringDataRecords();
                },
                error: (error) => { }
              });
          }
        });
      }
    
      approveSelectedRows(){
        this.approvedMonitoringData(this.selection.selected.map(row => row.id));
      }
    
      rejectMonitoringData(monitoringIds: number[]): void {
        const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to reject & remove monitoring data?`);
        const dialogRef = this.dialog.open(ConfirmDialogComponent, {
          maxWidth: "400px",
          data: dialogData
        });
    
        dialogRef.afterClosed().subscribe(dialogResult => {
          if(dialogResult){
            this.monitoringService.rejectMonitoringData(monitoringIds)
            .pipe(takeUntil(this.unsubscribe$))
              .subscribe({
                next: (response) => {
                  this.gs.openSnackBar(response.message, "Dismiss");
                  this.getOutComeMonitoringDataRecords();
                },
                error: (error) => { }
              });
          }
        });
      }
    
      rejectSelectedRows(){
        this.rejectMonitoringData(this.selection.selected.map(row => row.id));
      }
    
      onPageChange(event: PageEvent) {
          this.pageIndex = event.pageIndex;
          this.pageSize = event.pageSize;
          this.getOutComeMonitoringDataRecords();
        }
    
    
      ngOnDestroy(): void {
        this.unsubscribe$.next();
        this.unsubscribe$.complete();
      }
    }