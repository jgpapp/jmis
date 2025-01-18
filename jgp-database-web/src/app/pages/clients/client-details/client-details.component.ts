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
    MatPaginatorModule
  ],
  templateUrl: './client-details.component.html',
  styleUrl: './client-details.component.scss'
})
export class ClientDetailsComponent {

  public bmoDisplayedColumns = ['partnerName', 'dateFormSubmitted', 'isApplicantEligible', 'taSessionsAttended', 'isRecommendedForFinance', 'decisionDate', 'fiBusinessReferred'];
  public loansDisplayedColumns = ['partnerName', 'loanNumber', 'pipeLineSource', 'loanAmountAccessed', 'loanOutStandingAmount', 'loanDuration', 'dateApplied', 'dateDisbursed'];
  selectedParticipant: Observable<any>;
  constructor(private activatedRoute: ActivatedRoute){}

  ngOnInit(): void {
    this.selectedParticipant = this.activatedRoute.data.pipe(map(data => data['selectedParticipant']));
  }

}
