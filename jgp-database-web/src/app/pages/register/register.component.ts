import { Component } from '@angular/core';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatSidenavModule } from '@angular/material/sidenav';
import { Router, RouterModule } from '@angular/router';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { Settings, SettingsService } from '@services/settings.service';
import { emailValidator, matchingPasswords } from '../../theme/utils/app-validators';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';

@Component({
  selector: 'app-register',
  standalone: true,
  imports: [
    RouterModule,
    ReactiveFormsModule,
    FlexLayoutModule,
    MatSidenavModule,
    MatButtonModule,
    MatIconModule,
    MatInputModule,
    ContentHeaderComponent
  ],
  templateUrl: './register.component.html',
  styleUrl: './register.component.scss'
})
export class RegisterComponent {
  public form: FormGroup;
  public settings: Settings;
  constructor(public settingsService: SettingsService, public fb: FormBuilder, public router: Router) {
    this.settings = this.settingsService.settings;
    this.form = this.fb.group({
      'name': [null, Validators.compose([Validators.required, Validators.minLength(3)])],
      'email': [null, Validators.compose([Validators.required, emailValidator])],
      'password': ['', Validators.required],
      'confirmPassword': ['', Validators.required]
    }, { validator: matchingPasswords('password', 'confirmPassword') });
  }

  public onSubmit(values: Object): void {
    if (this.form.valid) {
      this.router.navigate(['/login']);
    }
  }

  ngAfterViewInit() {
    setTimeout(() => {
      this.settings.loadingSpinner = false;
    });
  }
}