import { Component } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatSidenavModule } from '@angular/material/sidenav';
import { Router, RouterModule } from '@angular/router';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { Settings, SettingsService } from '@services/settings.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
  selector: 'app-no-permission',
  standalone: true,
  imports: [
    RouterModule,
    FlexLayoutModule,
    MatSidenavModule,
    MatButtonModule,
    ContentHeaderComponent
  ],
  templateUrl: './no-permission.component.html',
  styleUrl: './no-permission.component.scss'
})
export class NoPermissionComponent {

  public settings: Settings;
  constructor(public settingsService: SettingsService, public router: Router) {
    this.settings = this.settingsService.settings; 
  }

  goHome(): void {
    this.router.navigate(['/']);
  }

  ngAfterViewInit(){
    setTimeout(() => {
      this.settings.loadingSpinner = false; 
    });  
  }

}
