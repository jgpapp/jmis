import { Component  } from '@angular/core';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { map, Observable, Subject } from 'rxjs';
import { PartnerDto } from '../../../dto/Partner';
import { AsyncPipe } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatDividerModule } from '@angular/material/divider';
import { HasPermissionDirective } from '../../../directives/has-permission.directive';

@Component({
  selector: 'app-partner-details',
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
    HasPermissionDirective
  ],
  templateUrl: './partner-details.component.html',
  styleUrl: './partner-details.component.scss'
})
export class PartnerDetailsComponent {

  selectedPartner: Observable<PartnerDto>;
  constructor(private activatedRoute: ActivatedRoute){}

  ngOnInit(): void {
    this.selectedPartner = this.activatedRoute.data.pipe(map(data => data['selectedPartner']));
  }

}
