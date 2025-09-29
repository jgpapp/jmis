import { Component, OnDestroy } from '@angular/core';
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
import { PartnerService } from '@services/data-management/partners.service';
import { GlobalService } from '@services/shared/global.service';
import { Router } from '@angular/router';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { MatCardModule } from '@angular/material/card';
import { Subject, takeUntil } from 'rxjs';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';

@Component({
    selector: 'app-create-partner',
    imports: [
        ReactiveFormsModule,
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
        MatCardModule
    ],
    templateUrl: './create-partner.component.html',
    styleUrl: './create-partner.component.scss'
})
export class CreatePartnerComponent implements OnDestroy{

  public newPartnerForm: FormGroup;
  subs = new SubscriptionsContainer();
  constructor(
    public fb: FormBuilder, 
    private partnerService: PartnerService,
    private gs: GlobalService,
    private router: Router
  ){
    this.newPartnerForm = this.fb.group({
      id: null,
      partnerName: [null, Validators.compose([Validators.required])],
      type: [null, Validators.compose([Validators.required])],
      });
  }

  onSubmitEditPartner(): void {
    if (this.newPartnerForm.valid) {
        this.subs.add = this.partnerService.createPartner(this.newPartnerForm.value)
        .subscribe({
          next: (response) => {
            this.gs.openSnackBar("Done sucessfully!!", "Dismiss");
            this.router.navigateByUrl('/partners');
          },
          error: (error) => {
            this.gs.openSnackBar(`An error occured ${error.error.detail}`, "Dismiss");
          }
        });
      
    }
  }


  ngOnDestroy(): void {
    this.subs.dispose();
  }
}
