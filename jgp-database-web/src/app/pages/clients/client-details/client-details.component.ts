import { Component  } from '@angular/core';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { map, Observable } from 'rxjs';
import { AsyncPipe } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatDividerModule } from '@angular/material/divider';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { HasPermissionDirective } from '../../../directives/has-permission.directive';
import { DashboardService } from '@services/dashboard/dashboard.service';

@Component({
  selector: 'app-client-details',
  standalone: true,
  imports: [
    MatCardModule,
    ContentHeaderComponent,
    FlexLayoutModule,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    AsyncPipe,
    MatDividerModule,
    RouterModule,
    MatTableModule,
    MatPaginatorModule,
    HasPermissionDirective
  ],
  templateUrl: './client-details.component.html',
  styleUrl: './client-details.component.scss'
})
export class ClientDetailsComponent {

  public bmoDisplayedColumns = ['partnerName', 'dateFormSubmitted', 'isApplicantEligible', 'taSessionsAttended', 'isRecommendedForFinance', 'decisionDate', 'fiBusinessReferred', 'dateUploaded', 'uploadedBy', 'dateApproved', 'approvedBy'];
  public loansDisplayedColumns = ['partnerName', 'loanNumber', 'pipeLineSource', 'loanAmountAccessed', 'loanOutStandingAmount', 'loanDuration', 'dateApplied', 'dateDisbursed', 'dateUploaded', 'uploadedBy', 'dateApproved', 'approvedBy'];
  public mentorshipDisplayedColumns = ['partnerName', 'mentorShipDate', 'mentorShipOrganization', 'bmoMemberShip','msmeSessionsCovered', 'smeSessionsCovered', 'dateUploaded', 'uploadedBy', 'dateApproved', 'approvedBy'];
  public outcomeMonitoringDisplayedColumns = ['surveyDate', 'surveyLanguage', 'partner', 'region', 'countyName', 'businessSetting', 'dateUploaded', 'uploadedBy', 'dateApproved', 'approvedBy'];

  selectedParticipant: Observable<any>;
  constructor(private activatedRoute: ActivatedRoute, private dashBoardService: DashboardService){}

  ngOnInit(): void {
    this.selectedParticipant = this.activatedRoute.data.pipe(map(data => data['selectedParticipant']));
  }

}
