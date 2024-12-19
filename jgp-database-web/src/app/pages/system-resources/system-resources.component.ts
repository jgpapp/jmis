import { Component } from '@angular/core';
import { ContentHeaderComponent } from "../../theme/components/content-header/content-header.component";
import { AuthService } from '@services/users/auth.service';
import { HasPermissionDirective } from '../../directives/has-permission.directive';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';

@Component({
  selector: 'app-system-resources',
  standalone: true,
  imports: [
    ContentHeaderComponent,
    NoPermissionComponent,
    HasPermissionDirective,
    FlexLayoutModule,
    MatButtonModule,
    MatIconModule
  ],
  templateUrl: './system-resources.component.html',
  styleUrl: './system-resources.component.scss'
})
export class SystemResourcesComponent {

  constructor(public authService: AuthService) { }
}
