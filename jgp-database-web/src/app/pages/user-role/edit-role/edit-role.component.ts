import { Component, Inject, OnDestroy, OnInit } from '@angular/core';
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
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { GlobalService } from '@services/shared/global.service';
import { UserRoleDto } from '../../../dto/UserRoleDto';
import { UserRoleService } from '@services/users/userroles.service';
import { PermissionsService } from '@services/users/permissions.service';
import { PartnerDto } from '../../../dto/Partner';
import { PartnerService } from '@services/data-management/partners.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { MatCardModule } from '@angular/material/card';
import { ActivatedRoute, Router } from '@angular/router';
import { map, Subject, takeUntil } from 'rxjs';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';

@Component({
    selector: 'app-edit-role',
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
    templateUrl: './edit-role.component.html',
    styleUrl: './edit-role.component.scss'
})
export class EditRoleComponent implements OnInit, OnDestroy {

  public editUserRoleForm: FormGroup;

  allPermissions: any
  selectedUserRole: UserRoleDto;
  subs = new SubscriptionsContainer();
  constructor(
    public fb: FormBuilder, 
    private userRoleServive: UserRoleService,
    private permissionsServive: PermissionsService,
    private gs: GlobalService,
    private router: Router,
    private activatedRoute: ActivatedRoute) {
      this.editUserRoleForm = this.fb.group({
      id: null,
      roleName: [null, Validators.compose([Validators.required])],
      description: [null, Validators.compose([Validators.required])],
      permissions: [null, Validators.compose([Validators.required])],
      });
    }
    
  ngOnInit(): void {
    this.getAvailablePermissions();
    this.subs.add = this.activatedRoute.data.pipe(map(data => data['selectedUserRole']))
      .subscribe({
        next: (response) => {
          this.selectedUserRole = response;
          this.editUserRoleForm.patchValue({
            'roleName': this.selectedUserRole.roleName,
            'description': this.selectedUserRole.description,
            'permissions': this.selectedUserRole.permissions
          });
        }
      });
  }

    getAvailablePermissions() {
      this.subs.add = this.permissionsServive.getAvailablePermissions()
        .subscribe({
          next: (response) => {
            this.allPermissions = response;
          },
          error: (error) => { }
        });
    }

    onSubmitEditUserRole(): void {
      if (this.editUserRoleForm.valid && this.selectedUserRole.id) {
          this.subs.add = this.userRoleServive.updateUserRole(this.selectedUserRole.id, this.editUserRoleForm.value)
          .subscribe({
            next: (response) => {
              this.gs.openSnackBar("Done sucessfully!!", "Dismiss");
              this.router.navigateByUrl(`/user-roles/${this.selectedUserRole.id}/details`);
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
