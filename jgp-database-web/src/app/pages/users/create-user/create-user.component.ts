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
import { MatStepperModule } from '@angular/material/stepper';
import { UserService } from '@services/users/user.service';
import { UserRoleService } from '@services/users/userroles.service';
import { PartnerDto } from '../../../dto/Partner';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';


@Component({
    selector: 'app-create-user',
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
        MatCardModule,
        MatStepperModule
    ],
    templateUrl: './create-user.component.html',
    styleUrl: './create-user.component.scss'
})
export class CreateUserComponent implements OnDestroy {

  public createUserForm: FormGroup;

  partners: PartnerDto[] = [];
  allUserRoles: any;
  subs = new SubscriptionsContainer();
  constructor(
    public fb: FormBuilder, private gs: GlobalService, 
    private userService: UserService, 
    private partnerService: PartnerService, 
    private userRolesService: UserRoleService,
    private router: Router) {
      this.createUserForm = this.fb.group({
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
  this.subs.add = this.userRolesService.getAvailableUserRoles()
    .subscribe({
      next: (response) => {
        this.allUserRoles = response;
      },
      error: (error) => { }
    });
}

getAvailablePartners() {
  this.subs.add = this.partnerService.getAvailablePartners(0, 400)
      .subscribe({
        next: (response) => {
          this.partners = response.content
        },
        error: (error) => {
          this.gs.openSnackBar(`An error occured ${error.error.detail}`, "Dismiss");
        }
      });
}

ngOnInit() {
  this.getAvailablePartners();
  this.getAvailableUserRoles();
}


onSubmitCreateUser(): void {
  if (this.createUserForm.valid) {
      this.subs.add = this.userService.createUser(this.createUserForm.value)
      .subscribe({
        next: (response) => {
          this.gs.openSnackBar("Done sucessfully!!", "Dismiss");
          this.router.navigateByUrl('/users');
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
