import { Component, OnDestroy, ViewChild } from '@angular/core';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { DataUploadComponent } from "../data-upload/data-upload.component";
import { MatPaginator, MatPaginatorModule } from '@angular/material/paginator';
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

@Component({
  selector: 'app-data-list',
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
  templateUrl: './data-list.component.html'
})
export class DataListComponent implements OnDestroy{

  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort!: MatSort;
  public displayedColumns = ['select', 'participantName', 'tasAttended', 'taSessionsAttended', 'isRecommendedForFinance', 'dateFormSubmitted', 'decisionDate'];
  public dataSource: any;

  bmoClientsData: any
  public selection = new SelectionModel<any>(true, []);

  private unsubscribe$ = new Subject<void>();
  constructor(private bmoClientDataService: BMOClientDataService, public authService: AuthService, private gs: GlobalService) { }

  getAvailableBMOClientData() {
    this.bmoClientDataService.getAvailableBMOClientData(false, this.authService.currentUser()?.partnerId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.bmoClientsData = response;
          this.dataSource = new MatTableDataSource(this.bmoClientsData);
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

  approveSelectedRows(){
    this.bmoClientDataService.approveBMOClientData(this.selection.selected.map(row => row.id))
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.gs.openSnackBar(response.message, "Dismiss");
          this.getAvailableBMOClientData();
        },
        error: (error) => { }
      });
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
