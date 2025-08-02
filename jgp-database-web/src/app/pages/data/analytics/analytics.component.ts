import { Component, OnDestroy, OnInit } from '@angular/core';
import { MatTabsModule } from '@angular/material/tabs';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatRadioModule } from '@angular/material/radio';
import { MatNativeDateModule, MatOptionModule } from '@angular/material/core';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatSelectModule } from '@angular/material/select';
import { MatDialogModule } from '@angular/material/dialog';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { GlobalService } from '@services/shared/global.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { MatCardModule } from '@angular/material/card';
import { Subject, takeUntil } from 'rxjs';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { AuthService } from '@services/users/auth.service';
import { DatePipe } from '@angular/common';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';

@Component({
  selector: 'app-analytics',
  standalone: true,
  imports: [ReactiveFormsModule,
      ContentHeaderComponent,
      FlexLayoutModule,
      MatTabsModule,
      MatFormFieldModule,
      MatInputModule,
      MatIconModule,
      MatDatepickerModule,
      MatNativeDateModule,
      MatRadioModule,
      MatDialogModule,
      MatButtonModule,
      MatSelectModule,
      MatOptionModule,
      MatCardModule],
  providers: [DatePipe],
  templateUrl: './analytics.component.html',
  styleUrl: './analytics.component.scss'
})
export class AnalyticsComponent implements OnInit, OnDestroy{

  public analyticsForm: FormGroup;
  subs = new SubscriptionsContainer();

  constructor(
    public fb: FormBuilder, 
    private authService: AuthService,
    private dashBoardService: DashboardService,
    private gs: GlobalService, 
    private datePipe: DatePipe
  ){
    this.analyticsForm = this.fb.group({
      fromDate: [null, Validators.compose([Validators.required])],
      toDate: [null, Validators.compose([Validators.required])],
      });
  }


  ngOnInit(): void {
  }


  onSubmitUpdateAnalytics(): void {
    if (this.analyticsForm.valid) {
      if(this.analyticsForm.controls['fromDate'].value && this.analyticsForm.controls['toDate'].value){
        this.analyticsForm.patchValue({
          'fromDate': this.forMatDateToISO(this.analyticsForm.controls['fromDate'].value),
          'toDate': this.forMatDateToISO(this.analyticsForm.controls['toDate'].value)
        });
      }
        this.subs.add = this.dashBoardService.updateAnalyticsDataSummary(this.analyticsForm.value, this.authService.currentUser()?.partnerId)
        .subscribe({
          next: (response) => {
            this.gs.openSnackBar("Started Updating Analytics!!", "Dismiss");
          },
          error: (error) => {
            this.gs.openSnackBar(`An error occured ${error.error.detail}`, "Dismiss");
          }
        });
      
    }
  }

  forMatDateToISO(selectedDate: any): any {
    return this.datePipe.transform(selectedDate, 'yyyy-MM-dd');
  }

  ngOnDestroy(): void {
    this.subs.dispose();
  }

}
