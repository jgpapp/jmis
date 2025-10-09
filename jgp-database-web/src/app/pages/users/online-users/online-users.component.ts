import { Component, OnDestroy, OnInit } from '@angular/core';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';
import { UserService } from '@services/users/user.service';
import { AuthService } from '@services/users/auth.service';
import { ContentHeaderComponent } from "../../../theme/components/content-header/content-header.component";
import { NoPermissionComponent } from "../../errors/no-permission/no-permission.component";
import { DashboardTypeFilter } from '../../../dto/dashboard-type-filter';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { BarChartModule, NgxChartsModule } from "@swimlane/ngx-charts";
import { FormBuilder, FormGroup, ReactiveFormsModule } from '@angular/forms';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { DatePipe } from '@angular/common';
import { MatInputModule } from '@angular/material/input';

@Component({
    selector: 'app-online-users',
    imports: [
    ContentHeaderComponent,
    ReactiveFormsModule,
    NoPermissionComponent,
    FlexLayoutModule,
    MatCardModule,
    MatIconModule,
    BarChartModule,
    NgxChartsModule,
    MatInputModule,
    MatDatepickerModule,
    MatNativeDateModule
],
    providers: [DatePipe], // Add DatePipe to providers
    templateUrl: './online-users.component.html'
})
export class OnlineUsersComponent implements OnInit, OnDestroy {

  subs = new SubscriptionsContainer();
  dashBoardFilters: any;
  currentDashBoardTypeFilters: DashboardTypeFilter = {};
  onlineUsersCount: number = 0;

  public userAccessSummary: any[];
  public userAccessSummaryShowXAxis: boolean = true;
  public userAccessSummaryShowYAxis: boolean = true;
  public userAccessSummaryShowLegend: boolean = false;
  public userAccessSummaryShowXAxisLabel: boolean = true;
  public userAccessSummaryShowYAxisLabel: boolean = true;
  public userAccessSummaryXAxisLabel: string = 'Access Count';
  public userAccessSummaryYAxisLabel: string = 'Time';
  public userAccessSummaryChartTitle: string = 'User Access Summary';

  public chartSColorScheme: any = {
    domain: [
      '#FF671B',
      '#8DB92E',
      '#4FCDB0',
      '#DE3C95',
      '#F38B00',
      '#2F7B6B',
      '#D22A2F',
      '#FFC81F',
      '#2F3E9E', 
      '#D22E2E', 
      '#378D3B', 
      '#7f7f7f', 
      '#c4a678', 
      '#6a7b6a', 
      '#191919', 
      '#3d144c', 
      '#f0e1dc', 
      '#a04324', 
      '#00ffff', 
      '#0e5600', 
      '#0e9697'
    ]
  };

  public disableToDate: boolean = true;
  fromMaxDate: Date = new Date();
  toDateMinValue = new Date();
  toDateMaxValue = new Date();
  public filterForm: FormGroup;

  constructor(
     private userService: UserService, 
     public fb: FormBuilder, 
     public authService: AuthService,
     private datePipe: DatePipe
    ) {

      this.filterForm = this.fb.group({
      selectedDateFrom: [null],
      selectedDateTo: [null]
      });
    }

  ngOnInit(): void {
    this.updateCurrentDashBoardTypeFilters();
    this.updateGraphData();
    this.subscribeToOnlineUsersCount();
  }

  updateCurrentDashBoardTypeFilters() {
  this.currentDashBoardTypeFilters = {
    isPartnerDashboard: false,
    currentUserPartnerId: 0,
    partnerType: 'None',
    isFinancialDashboard: false,
    isTADashboard: false,
    isMentorShipDashboard: false,
    isMonitoringDashBoard: false
  };
}

  setDashBoardFilters(currentDashBoardFilters: any){
    this.dashBoardFilters = currentDashBoardFilters;
    this.updateGraphData();
  }

  updateGraphData(): void {
      this.subs.add = this.userService.getSystemUserLoginSummary(this.filterForm.value)
      .subscribe({
        next: (response) => {
          this.userAccessSummary = response;
        },
        error: (error) => { }
      });
    }

    subscribeToOnlineUsersCount(): void {
    this.subs.add = this.userService.getOnlineUsersCount()
      .subscribe({
        next: (response) => {
          this.onlineUsersCount = response ? response.measurements[0].value : 0;
        },
        error: (error) => { }
      });
  }

  filterFromDateChanged() {
    this.disableToDate = false;
    let startDate = this.filterForm.controls['selectedDateFrom'].value;
    let endDate = this.filterForm.controls['selectedDateTo'].value;
    if(endDate && endDate != null){
      this.filterForm.patchValue({
        'selectedDateTo': null
      });
    }
    this.toDateMinValue = startDate && startDate != null ? new Date(startDate) : new Date();
    const minDate = this.toDateMinValue;
    const maxDate = new Date(minDate);
    maxDate.setMonth(maxDate.getMonth() + 1);
    this.toDateMaxValue = maxDate > new Date() ? new Date() : maxDate;
  
}

  filterFormChanged() {
    let startDate = this.filterForm.controls['selectedDateFrom'].value;
    let endDate = this.filterForm.controls['selectedDateTo'].value;
    if(endDate && endDate != null && startDate && startDate != null){
      this.filterForm.patchValue({
        'selectedDateFrom': this.forMatDateToISO(startDate),
        'selectedDateTo': this.forMatDateToISO(endDate)
      });
      this.updateGraphData();
    }

  }

  forMatDateToISO(selectedDate: any): any {
    return this.datePipe.transform(selectedDate, 'yyyy-MM-dd');
  }

  public onSelect(event: any) {
    console.log(event);
  }

    ngOnDestroy(): void {
    this.subs.dispose();
  }

}
