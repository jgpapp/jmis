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
import { PartnerDto } from '../../../dto/Partner';
import { PartnerService } from '@services/data-management/partners.service';
import { GlobalService } from '@services/shared/global.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { MatCardModule } from '@angular/material/card';
import { ActivatedRoute, Router } from '@angular/router';
import { map, Subject, takeUntil } from 'rxjs';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';

@Component({
    selector: 'app-edit-partner',
    imports: [
        ReactiveFormsModule,
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
        ContentHeaderComponent,
        MatCardModule
    ],
    templateUrl: './edit-partner.component.html',
    styleUrl: './edit-partner.component.scss'
})
export class EditPartnerComponent implements OnDestroy{

  public editPartnerForm: FormGroup;

  selectedPartner: PartnerDto;
  subs = new SubscriptionsContainer();
  constructor(
    public fb: FormBuilder, 
    private partnerService: PartnerService,
    private gs: GlobalService,
    private router: Router,
    private activatedRoute: ActivatedRoute) {
    this.editPartnerForm = this.fb.group({
    id: null,
    partnerName: [null, Validators.compose([Validators.required])],
    type: [null, Validators.compose([Validators.required])],
    });
    }

    ngOnInit(): void {
      this.subs.add = this.activatedRoute.data.pipe(map(data => data['selectedPartner']))
      .subscribe({
        next: (response) => {
          this.selectedPartner = response;
          this.editPartnerForm.patchValue({
            'partnerName': this.selectedPartner.partnerName,
            'type': this.selectedPartner.typeEnum
          });
        }
      });
    }

    onSubmitEditPartner(): void {
      if (this.editPartnerForm.valid && this.selectedPartner.id) {
          this.subs.add = this.partnerService.updatePartner(this.selectedPartner.id, this.editPartnerForm.value)
          .subscribe({
            next: (response) => {
              this.gs.openSnackBar("Done sucessfully!!", "Dismiss");
              this.router.navigateByUrl(`/partners/${this.selectedPartner.id}/details`);
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
