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
import { PermissionsService } from '@services/users/permissions.service';
import { UserRoleService } from '@services/users/userroles.service';
import { Subject, takeUntil } from 'rxjs';

@Component({
  selector: 'app-create-role',
  standalone: true,
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
  templateUrl: './create-role.component.html',
  styleUrl: './create-role.component.scss'
})
export class CreateRoleComponent implements OnDestroy{

  allPermissions: any
  public newUserRoleForm: FormGroup;
  private unsubscribe$ = new Subject<void>();
  constructor(
    public fb: FormBuilder, 
    private partnerService: PartnerService,
    private userRoleServive: UserRoleService,
    private permissionsServive: PermissionsService,
    private gs: GlobalService,
    private router: Router
  ){
    this.newUserRoleForm = this.fb.group({
      id: null,
      roleName: [null, Validators.compose([Validators.required])],
      description: [null, Validators.compose([Validators.required])],
      permissions: [null, Validators.compose([Validators.required])],
      });
  }

  ngOnInit(): void {
    this.getAvailablePermissions();
  }

    getAvailablePermissions() {
      this.permissionsServive.getAvailablePermissions()
      .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
            this.allPermissions = response;
          },
          error: (error) => { }
        });
    }

    onSubmitCreateUserRole(): void {
      if (this.newUserRoleForm.valid) {
          this.userRoleServive.createUserRole(this.newUserRoleForm.value)
          .pipe(takeUntil(this.unsubscribe$))
          .subscribe({
            next: (response) => {
              this.gs.openSnackBar("Done sucessfully!!", "Dismiss");
              this.router.navigateByUrl('/user-roles');
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
