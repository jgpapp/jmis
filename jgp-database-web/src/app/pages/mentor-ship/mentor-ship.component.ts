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
import { MentorShipService } from '@services/data-management/mentor-ship.service';
import { SubscriptionsContainer } from '../../theme/utils/subscriptions-container';

@Component({
  selector: 'app-mentor-ship',
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
  templateUrl: './mentor-ship.component.html',
  styleUrl: './mentor-ship.component.scss'
})
export class MentorShipComponent implements OnDestroy, OnInit {

    @ViewChild(MatPaginator) paginator: MatPaginator;
    @ViewChild(MatSort) sort!: MatSort;
    public displayedColumns = [
      'select', 'participantName', 'businessName', 'participantJGPID', 'mentorShipDate', 
      'mentorShipOrganization', 'bmoMemberShip', 'dateUploaded', 'uploadedBy'
    ];
    public dataSource: any;
  
    pageSize = 10;
    pageIndex = 0;
    totalItems = 0;
  
    newMentorShipData: any
    public selection = new SelectionModel<any>(true, []);
  
    subs = new SubscriptionsContainer();

    constructor(private mentorshipService: MentorShipService, public authService: AuthService, private gs: GlobalService, private dialog: MatDialog) { }
    
      getAvailableNewMentorShipData() {
        const partnerId = this.authService.currentUser()?.partnerId;
        if (partnerId) {
          this.subs.add = this.mentorshipService.getAvailableMentorshipData(this.pageIndex, this.pageSize, false, partnerId)
            .subscribe({
              next: (response) => {
                this.newMentorShipData = response.content;
                this.dataSource = new MatTableDataSource(this.newMentorShipData);
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
        this.getAvailableNewMentorShipData();
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
    
    
      approvedMentorShipData(mentorshipIds: number[]): void {
        const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to approve mentorship data?`);
        const dialogRef = this.dialog.open(ConfirmDialogComponent, {
          maxWidth: "400px",
          data: dialogData
        });
    
        dialogRef.afterClosed().subscribe(dialogResult => {
          if(dialogResult){
            this.subs.add = this.mentorshipService.approvedMentorShipData(mentorshipIds)
              .subscribe({
                next: (response) => {
                  this.gs.openSnackBar(response.message, "Dismiss");
                  this.getAvailableNewMentorShipData();
                },
                error: (error) => { }
              });
          }
        });
      }
    
      approveSelectedRows(){
        this.approvedMentorShipData(this.selection.selected.map(row => row.id));
      }
    
      approveAllPartnerMentorShipData(){
        this.approvedMentorShipData([]);
      }
    
      rejectMentorShipData(mentorshipIds: number[]): void {
        const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to reject & remove mentorship data?`);
        const dialogRef = this.dialog.open(ConfirmDialogComponent, {
          maxWidth: "400px",
          data: dialogData
        });
    
        dialogRef.afterClosed().subscribe(dialogResult => {
          if(dialogResult){
            this.subs.add = this.mentorshipService.rejectMentorShipData(mentorshipIds)
              .subscribe({
                next: (response) => {
                  this.gs.openSnackBar(response.message, "Dismiss");
                  this.getAvailableNewMentorShipData();
                },
                error: (error) => { }
              });
          }
        });
      }
    
      rejectSelectedRows(){
        this.rejectMentorShipData(this.selection.selected.map(row => row.id));
      }
    
      rejectAllPartnerMentorShipData(){
        this.rejectMentorShipData([]);
      }
    
      onPageChange(event: PageEvent) {
          this.pageIndex = event.pageIndex;
          this.pageSize = event.pageSize;
          this.getAvailableNewMentorShipData();
        }
    
    
      ngOnDestroy(): void {
        this.subs.dispose();
      }
}
