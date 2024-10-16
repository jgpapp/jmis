import { Component } from '@angular/core';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatSidenavModule } from '@angular/material/sidenav';
import { Router, RouterModule } from '@angular/router';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { Settings, SettingsService } from '@services/settings.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { MatIconModule } from '@angular/material/icon';

@Component({
  selector: 'app-not-found',
  standalone: true,
  imports: [
    RouterModule,
    FlexLayoutModule,
    ReactiveFormsModule,
    MatSidenavModule,
    MatInputModule,
    MatButtonModule,
    MatIconModule,
    ContentHeaderComponent
  ],
  templateUrl: './not-found.component.html'
})
export class NotFoundComponent {
  public form: FormGroup;
  public settings: Settings;
  constructor(public settingsService: SettingsService, public router: Router, public fb: FormBuilder) {
    this.settings = this.settingsService.settings;
  }

  ngOnInit() {
    this.form = this.fb.group({
      'param': [null, Validators.required]
    });
  }

  public onSubmit(values: any): void {
    if (this.form.valid) {
      this.router.navigate(['/search', values['param']]);
    }
  }

  public goHome(): void {
    this.router.navigate(['/']);
  }

  ngAfterViewInit() {
    setTimeout(() => {
      this.settings.loadingSpinner = false;
    });
  }

}
