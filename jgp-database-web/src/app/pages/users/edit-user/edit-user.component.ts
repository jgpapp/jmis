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
import { UserRoleService } from '@services/users/userroles.service';
import { UserService } from '@services/users/user.service';
import { User } from '../../../common/models/user.model';

@Component({
  selector: 'app-edit-user',
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
  templateUrl: './edit-user.component.html',
  styleUrl: './edit-user.component.scss'
})
export class EditUserComponent implements OnDestroy{

  public editUserForm: FormGroup;
  partners: PartnerDto[] = [];
  allUserRoles: any;
  selectedUser: User;
  private unsubscribe$ = new Subject<void>();
  constructor(
    public fb: FormBuilder, 
    private userService: UserService, 
    private partnerService: PartnerService, 
    private userRolesService: UserRoleService,
    private gs: GlobalService,
    private router: Router,
    private activatedRoute: ActivatedRoute) {
    this.editUserForm = this.fb.group({
      id: null,
      firstName: [null, Validators.compose([Validators.required])],
      lastName: [null, Validators.compose([Validators.required])],
      gender: [null, Validators.compose([Validators.required])],
      userRoles: [null, Validators.compose([Validators.required])],
      username: [null, Validators.compose([Validators.email])],
      partnerId: [null],
      designation: [null],
      cellPhone: [null],
      town: [null],
    });
    }

    getAvailableUserRoles() {
      this.userRolesService.getAvailableUserRoles()
      .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
            this.allUserRoles = response;
          },
          error: (error) => { }
        });
    }
    
    getAvailablePartners() {
      this.partnerService.getAvailablePartners(0, 400)
      .pipe(takeUntil(this.unsubscribe$))
          .subscribe({
            next: (response) => {
              this.partners = response.content
              this.partners = this.partners.concat([{
                id: 0,
                partnerName: 'GLOBAL PARTNER',
                type: 'NONE',
                typeEnum: 'NONE'
            }])
            },
            error: (error) => {
              this.gs.openSnackBar(`An error occured ${error.error.detail}`, "Dismiss");
            }
          });
    }
   

    ngOnInit(): void {
      this.getAvailablePartners();
      this.getAvailableUserRoles();
      this.activatedRoute.data.pipe(map(data => data['selectedUser']))
      .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.selectedUser = response;
          this.editUserForm.patchValue({
            'firstName': this.selectedUser.firstName,
            'lastName': this.selectedUser.lastName,
            'gender': this.selectedUser.gender,
            'userRoles': this.selectedUser.userRoles,
            'partnerId': this.selectedUser.partnerId,
            'designation': this.selectedUser.designation,
            'cellPhone': this.selectedUser.cellPhone,
            'town': this.selectedUser.town
          });
        }
      });
    }

    onSubmitEditUserer(): void {
      if (this.editUserForm.valid && this.selectedUser.id) {
          this.userService.updateUser(this.selectedUser.id, this.editUserForm.value)
          .pipe(takeUntil(this.unsubscribe$))
          .subscribe({
            next: () => {
              this.gs.openSnackBar("Done sucessfully!!", "Dismiss");
              this.router.navigateByUrl(`/users/${this.selectedUser.id}/details`);
            },
            error: (error) => {
              this.gs.openSnackBar(`An error occured ${error.error.detail}`, "Dismiss");
            }
          });
      }
    }


    ngOnDestroy(): void {
      this.unsubscribe$.next();
      this.unsubscribe$.complete();
    }
}
