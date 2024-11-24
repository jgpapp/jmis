import { Component, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { Router, RouterModule } from '@angular/router';
import { Settings, SettingsService } from '@services/settings.service';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { MatInputModule } from '@angular/material/input';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { UserService } from '@services/users/user.service';
import { GlobalService } from '@services/shared/global.service';
import { AuthService } from '@services/users/auth.service';
import { Subject, takeUntil } from 'rxjs';

@Component({
  selector: 'app-change-password',
  standalone: true,
  imports: [
    RouterModule,
    FlexLayoutModule,
    MatSidenavModule,
    ReactiveFormsModule,
    MatButtonModule,
    MatIconModule,
    MatInputModule,
    MatSlideToggleModule,
    ContentHeaderComponent
  ],
  templateUrl: './change-password.component.html',
  styleUrl: './change-password.component.scss'
})
export class ChangePasswordComponent implements OnDestroy {

  public settings: Settings;
  passwordChangeForm: FormGroup = this.fb.group({});
  msg: string = "";
  hide = true;
  authResponse: any
  private unsubscribe$ = new Subject<void>();
  constructor(public settingsService: SettingsService, public fb: FormBuilder, public router: Router, private userService: UserService, private gs: GlobalService, private authService: AuthService) {
    this.settings = this.settingsService.settings;
    this.passwordChangeForm = this.fb.group({
      'password': [null, Validators.required],
      'newPass': [null, Validators.required],
      'passConfirm': [null, Validators.required]
    });
  }

  public onSubmit(): void {
    this.userService.updateUserPassword(this.passwordChangeForm.value)
    .pipe(takeUntil(this.unsubscribe$))
    .subscribe({
      next: (response) => {
        this.gs.openSnackBar(response.message, '')
        this.authService.doLogout();

      }
    });
  }

  ngAfterViewInit() {
    setTimeout(() => {
      this.settings.loadingSpinner = false;
    });
  }

  ngOnInit(): void {

  }

  togglePasswordVisibility() {
    this.hide = !this.hide;
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }

}
