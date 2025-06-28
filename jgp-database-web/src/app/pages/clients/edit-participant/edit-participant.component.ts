import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatNativeDateModule, MatOptionModule } from '@angular/material/core';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatRadioModule } from '@angular/material/radio';
import { MatSelectModule } from '@angular/material/select';
import { MatTabsModule } from '@angular/material/tabs';
import { ActivatedRoute, Router } from '@angular/router';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { map, Subject, takeUntil } from 'rxjs';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { MatCardModule } from '@angular/material/card';
import { GlobalService } from '@services/shared/global.service';
import { ClientService } from '@services/data-management/clients.service';
import { DashboardService } from '@services/dashboard/dashboard.service';

@Component({
  selector: 'app-edit-participant',
  standalone: true,
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
  templateUrl: './edit-participant.component.html',
  styleUrl: './edit-participant.component.scss'
})
export class EditParticipantComponent implements OnInit {

  partners: any;
  public editParticipantForm: FormGroup;
    allUserRoles: any;
    counties: any[]
    selectedParticipant: any;
    private unsubscribe$ = new Subject<void>();
    constructor(
      public fb: FormBuilder, 
      private participantService: ClientService, 
      private gs: GlobalService,
      private dashBoardService: DashboardService,
      private router: Router,
      private activatedRoute: ActivatedRoute) {
      this.editParticipantForm = this.fb.group({
        id: null,
        businessName: [null],
        participantName: [null],
        jgpId: [null, Validators.compose([Validators.required])],
        phoneNumber: [null, Validators.compose([Validators.required])],
        alternativePhoneNumber: [null],
        ownerGender: [null, Validators.compose([Validators.required])],
        ownerAge: [null, Validators.compose([Validators.required])],
        locationCountyCode: [null, Validators.compose([Validators.required])],
        industrySector: [null],
        businessSegment: [null],
        bestMonthlyRevenue: [null],
        worstMonthlyRevenue: [null],
        totalRegularEmployees: [null],
        youthRegularEmployees: [null],
        totalCasualEmployees: [null],
        youthCasualEmployees: [null],
        sampleRecords: [null],
        personWithDisability: [null],
        refugeeStatus: [null]
      });
      }

  
      ngOnInit(): void {
        this.getAvailableCounties();
        this.activatedRoute.data.pipe(map(data => data['selectedParticipant']))
        .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
            this.selectedParticipant = response;
            this.editParticipantForm.patchValue({
              'businessName': this.selectedParticipant.businessName,
              'participantName': this.selectedParticipant.participantName,
              'jgpId': this.selectedParticipant.jgpId,
              'phoneNumber': this.selectedParticipant.phoneNumber,
              'alternativePhoneNumber': this.selectedParticipant.alternativePhoneNumber,
              'ownerGender': this.selectedParticipant.ownerGender,
              'ownerAge': this.selectedParticipant.ownerAge,
              'locationCountyCode': this.selectedParticipant.locationCountyCode,
              'industrySector': this.selectedParticipant.industrySector,
              'businessSegment': this.selectedParticipant.businessSegment,
              'bestMonthlyRevenue': this.selectedParticipant.bestMonthlyRevenue,
              'worstMonthlyRevenue': this.selectedParticipant.worstMonthlyRevenue,
              'totalRegularEmployees': this.selectedParticipant.totalRegularEmployees,
              'youthRegularEmployees': this.selectedParticipant.youthRegularEmployees,
              'totalCasualEmployees': this.selectedParticipant.totalCasualEmployees,
              'youthCasualEmployees': this.selectedParticipant.youthCasualEmployees,
              'sampleRecords': this.selectedParticipant.sampleRecords,
              'personWithDisability': this.selectedParticipant.personWithDisability,
              'refugeeStatus': this.selectedParticipant.refugeeStatus
            });
          }
        });
      }
  
      onSubmitEditParticipant(): void {
        if (this.editParticipantForm.valid && this.selectedParticipant.id) {
            this.participantService.updateParticipant(this.selectedParticipant.id, this.editParticipantForm.value)
            .pipe(takeUntil(this.unsubscribe$))
            .subscribe({
              next: () => {
                this.gs.openSnackBar("Done sucessfully!!", "Dismiss");
                this.router.navigateByUrl(`/participants/${this.selectedParticipant.id}/details`);
              },
              error: (error: any) => {
                this.gs.openSnackBar(`An error occured ${error.error.detail}`, "Dismiss");
              }
            });
        }
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
  
  
      ngOnDestroy(): void {
        this.unsubscribe$.next();
        this.unsubscribe$.complete();
      }

}
