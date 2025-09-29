import { Component, ViewChild } from '@angular/core';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { RouterModule } from '@angular/router';
import { NoPermissionComponent } from '../../errors/no-permission/no-permission.component';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { FormBuilder, FormGroup, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSort } from '@angular/material/sort';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';
import { MomentDateAdapter, MAT_MOMENT_DATE_ADAPTER_OPTIONS } from '@angular/material-moment-adapter';
import { DateAdapter, MAT_DATE_FORMATS, MAT_DATE_LOCALE, MatOptionModule } from '@angular/material/core';
import { AuthService } from '@services/users/auth.service';
import { GlobalService } from '@services/shared/global.service';
import { UserService } from '@services/users/user.service';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { DatePipe } from '@angular/common';
import { MatSelectModule } from '@angular/material/select';

export const MY_FORMATS = {
  parse: {
    dateInput: 'YYYY-MM-DDTHH:mm:ss', // This must match the Java format
  },
  display: {
    dateInput: 'DD/MM/YYYY',
    monthYearLabel: 'MMM YYYY',
    dateA11yLabel: 'LL',
    monthYearA11yLabel: 'MMMM YYYY',
  },
};

@Component({
    selector: 'app-user-audit-logs',
    imports: [
        FlexLayoutModule,
        MatTableModule,
        MatPaginatorModule,
        ContentHeaderComponent,
        RouterModule,
        NoPermissionComponent,
        MatFormFieldModule,
        MatInputModule,
        FormsModule,
        ReactiveFormsModule,
        MatDatepickerModule,
        MatButtonModule,
        MatOptionModule,
        MatSelectModule,
        MatIconModule
    ],
    providers: [
        DatePipe,
        {
            provide: DateAdapter,
            useClass: MomentDateAdapter,
            deps: [MAT_DATE_LOCALE, MAT_MOMENT_DATE_ADAPTER_OPTIONS]
        },
        { provide: MAT_DATE_FORMATS, useValue: MY_FORMATS }
    ],
    templateUrl: './user-audit-logs.component.html',
    styleUrl: './user-audit-logs.component.scss'
})
export class UserAuditLogsComponent {

    @ViewChild(MatPaginator) paginator: MatPaginator;
    @ViewChild(MatSort) sort!: MatSort;
    public displayedColumns = ['logTime', 'username', 'action', 'resourceId', 'details'];
    public dataSource: any;
  
    userLogs: any
    auditableOperations: any[]
    pageSize = 10;
    pageIndex = 0;
    totalItems = 0;
    subs = new SubscriptionsContainer();
    public userLogsFilterForm: FormGroup;
    constructor(
      private userService: UserService, 
      private fb: FormBuilder,
      public authService: AuthService, 
      public gs: GlobalService) {

      this.userLogsFilterForm = this.fb.group({
        fromDate: [undefined], 
        toDate: [undefined],
        action: [undefined],
        userName: [undefined]
      });
    }

    getUserAuditLogs() {
        this.subs.add = this.userService.getUserAuditLogs(
          this.userLogsFilterForm.controls['userName'].value,
          this.userLogsFilterForm.controls['action'].value,
          this.userLogsFilterForm.controls['fromDate'].value,
          this.userLogsFilterForm.controls['toDate'].value,
          this.pageIndex,
          this.pageSize
        ).subscribe({
            next: (response) => {
              this.userLogs = response.content;
              this.dataSource = new MatTableDataSource(this.userLogs);
              this.totalItems = response.page.totalElements;
            },
            error: (error) => { }
          });
    }

    getUserAuditableOperations() {
        this.subs.add = this.userService.getUserAuditableOperations().subscribe({
            next: (response) => {
                this.auditableOperations = response;
            },
            error: (error) => { }
        });
    }

      refresh() {
        this.getUserAuditLogs();
      }


    onPageChange(event: PageEvent) {
        this.pageIndex = event.pageIndex;
        this.pageSize = event.pageSize;
        this.getUserAuditLogs();
      }
    
      ngOnInit(): void {
        this.getUserAuditLogs();
        this.getUserAuditableOperations();
      }
    
      ngOnDestroy(): void {
        this.subs.dispose();
      }

}
