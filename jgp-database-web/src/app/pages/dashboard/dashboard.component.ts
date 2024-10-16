import { Component } from '@angular/core';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { MatCardModule } from '@angular/material/card';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { TilesComponent } from './tiles/tiles.component';
import { InfoCardsComponent } from './info-cards/info-cards.component';
import { AnalyticsComponent } from './analytics/analytics.component';
import { DashboardService } from '@services/dashboard/dashboard.service';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { DashboardFiltersComponent } from './dashboard-filters/dashboard-filters.component';

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [
    ContentHeaderComponent,
    FlexLayoutModule,
    MatCardModule,
    MatProgressBarModule,
    TilesComponent,
    InfoCardsComponent,
    AnalyticsComponent,
    NoPermissionComponent,
    DashboardFiltersComponent
  ],
  templateUrl: './dashboard.component.html',
  styleUrl: './dashboard.component.scss'
})
export class DashboardComponent {

  constructor(private dashBoardService: DashboardService, public authService: AuthService){
    
  }
}
