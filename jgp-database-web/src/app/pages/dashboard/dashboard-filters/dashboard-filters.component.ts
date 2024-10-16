import { Component, OnDestroy, OnInit } from '@angular/core';
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
  templateUrl: './dashboard-filters.component.html',
  styleUrl: './dashboard-filters.component.scss'
})
export class DashboardFiltersComponent implements OnDestroy, OnInit{

  public dashFilterForm: FormGroup;
  partners: PartnerDto[] = [];
  private unsubscribe$ = new Subject<void>();
  constructor(
    public fb: FormBuilder, 
    private partnerService: PartnerService,
    private gs: GlobalService){

    this.dashFilterForm = this.fb.group({
      selectedPartnerId: [null],
      selectedTANeeds: [null],
      selectedSector: [null],
      selectedDateFrom: [null],
      selectedDateTo: [null]
      });
  }
  ngOnInit(): void {
    this.getAvailablePartners();
  }
  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

  getAvailablePartners() {
    this.partnerService.getAvailablePartners()
    .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
            this.partners = response
          },
          error: (error) => {
            this.gs.openSnackBar(`An error occured ${error.error.detail}`, "Dismiss");
          }
        });
  }

  filterFormChanged() {
    console.log(this.dashFilterForm.value);
  }
}
