import { Component, OnDestroy } from '@angular/core';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { Router, RouterModule } from '@angular/router';
import { Settings, SettingsService } from '@services/settings.service';
import { emailValidator } from '../../theme/utils/app-validators';
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
  selector: 'app-login',
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
  templateUrl: './login.component.html'
})
export class LoginComponent implements OnDestroy {
  public settings: Settings;
  userloginForm: FormGroup = this.fb.group({});
  msg: string = "";
  hide = true;
  authResponse: any
  authResponse2?: { success: boolean, message: string, authToken: string }
  private unsubscribe$ = new Subject<void>();
  constructor(public settingsService: SettingsService, public fb: FormBuilder, public router: Router, private us: UserService, private authService: AuthService) {
    this.settings = this.settingsService.settings;
    this.userloginForm = this.fb.group({
      'username': [null, Validators.compose([Validators.required, emailValidator])],
      'password': [null, Validators.compose([Validators.required, Validators.minLength(1)])]
    });
  }

  public onSubmit(): void {
    this.authService.login(this.userloginForm.value)
    .pipe(takeUntil(this.unsubscribe$))
    .subscribe({
      next: (response) => {
        this.authResponse = response
        this.authService.storeUserDetails(this.authResponse?.authToken);
        this.authService.userRedirection();

      }
    });
  }

  ngAfterViewInit() {
    setTimeout(() => {
      this.settings.loadingSpinner = false;
    });
  }

  ngOnInit(): void {
    this.authService.userRedirection();
  }

  togglePasswordVisibility() {
    this.hide = !this.hide;
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}