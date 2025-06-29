import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output, SimpleChanges } from '@angular/core';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatNativeDateModule, MatOptionModule } from '@angular/material/core';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { PartnerDto } from '../../../dto/Partner';
import { PartnerService } from '@services/data-management/partners.service';
import { Subject, takeUntil } from 'rxjs';
import { GlobalService } from '@services/shared/global.service';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { AuthService } from '@services/users/auth.service';
import { DatePipe } from '@angular/common';

@Component({
  selector: 'app-dashboard-filters',
  standalone: true,
  imports: [
    FlexLayoutModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatOptionModule,
    MatDatepickerModule,
    MatNativeDateModule
  ],
  providers: [DatePipe], // Add DatePipe to providers
  templateUrl: './dashboard-filters.component.html',
  styleUrl: './dashboard-filters.component.scss'
})
export class DashboardFiltersComponent implements OnDestroy, OnInit, OnChanges{

  @Output() dashBoardFilters: EventEmitter<any> = new EventEmitter();
  @Input({required: true, alias: 'isPartnerDashBoard'}) isPartnerDashBoard: boolean;
  @Input({required: true, alias: 'showTADashboard'}) showTADashboard: boolean;
  @Input('partnerId') partnerId: number;
  @Input({required: true, alias: 'fieldFlex'}) fieldFlex: number;
  @Input({required: true, alias: 'resetDashBoardFilters'}) resetDashBoardFilters: boolean;
  @Input({required: true, alias: 'isMonitoringDashBoard'}) isMonitoringDashBoard: boolean;
  public disableToDate: boolean = true;
  maxDate: Date = new Date();
  toDateMinValue = new Date();
  public dashFilterForm: FormGroup;
  partners: PartnerDto[] = [];
  counties: any[]
  private unsubscribe$ = new Subject<void>();
  constructor(
    public fb: FormBuilder, 
    private partnerService: PartnerService,
    private gs: GlobalService,
    private dashBoardService: DashboardService,
    public authService: AuthService, 
    private datePipe: DatePipe){

    this.dashFilterForm = this.fb.group({
      selectedPartnerId: [null],
      selectedTrainingPartner: [null],
      selectedCountyCode: [null],
      selectedDateFrom: [null],
      selectedDateTo: [null],
      partnerName: [null],
      ageGroup: [null],
      gender: [null],
      jgpIntervention: [null],
      region: [null]
      });
  }

  ngOnChanges(changes: SimpleChanges): void {
    if(changes['resetDashBoardFilters'] && true === changes['resetDashBoardFilters']['currentValue']){
      this.dashFilterForm = this.fb.group({
        selectedPartnerId: [null],
        selectedTrainingPartner: [null],
        selectedCountyCode: [null],
        selectedDateFrom: [null],
        selectedDateTo: [null],
        partnerName: [null],
        ageGroup: [null],
        gender: [null],
        jgpIntervention: [null],
        region: [null]
        });
    }
  }

  ngOnInit(): void {
    this.getAvailablePartners();
    this.getAvailableCounties();
  }
  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

  getAvailablePartners() {
    this.partnerService.getAvailablePartners(0, 400)
    .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
            this.partners = response.content
          },
          error: (error) => {
            this.gs.openSnackBar(`An error occured ${error.error.detail}`, "Dismiss");
          }
        });
  }

  getAvailableCounties() {
    this.dashBoardService.getKenyanCounties()
    .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
            this.counties = response
          },
          error: (error) => {
            this.gs.openSnackBar(`An error occured ${error.error.detail}`, "Dismiss");
          }
        });
  }

  filterFromDateChanged() {
    this.disableToDate = false;
    this.toDateMinValue = this.dashFilterForm.controls['selectedDateFrom'].value
    if(this.dashFilterForm.controls['selectedDateTo'].value){
      this.filterFormChanged();
    }
  }

  filterFormChanged() {
    if(this.isPartnerDashBoard){
      this.dashFilterForm.patchValue({
        'selectedPartnerId': this.partnerId
      });
    }
    if(this.dashFilterForm.controls['selectedPartnerId'].value == '0'){
      this.dashFilterForm.patchValue({
        'selectedPartnerId': null
      });
    }
    if(this.dashFilterForm.controls['selectedCountyCode'].value == '0'){
      this.dashFilterForm.patchValue({
        'selectedCountyCode': null
      });
    }
    if(this.dashFilterForm.controls['selectedTrainingPartner'].value == 'OO'){
      this.dashFilterForm.patchValue({
        'selectedTrainingPartner': null
      });
    }

    if(this.dashFilterForm.controls['selectedDateTo'].value && this.dashFilterForm.controls['selectedDateFrom'].value){
      this.dashFilterForm.patchValue({
        'selectedDateFrom': this.forMatDateToISO(this.dashFilterForm.controls['selectedDateFrom'].value),
        'selectedDateTo': this.forMatDateToISO(this.dashFilterForm.controls['selectedDateTo'].value)
      });
    }
    
    this.dashBoardFilters.emit(this.dashFilterForm.value);
  }

  forMatDateToISO(selectedDate: any): any {
    return this.datePipe.transform(selectedDate, 'yyyy-MM-dd');
  }

}
