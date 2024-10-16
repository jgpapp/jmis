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
import { User } from '../../../common/models/user.model';
import { HasPermissionDirective } from '../../../directives/has-permission.directive';

@Component({
  selector: 'app-user-details',
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
  templateUrl: './user-details.component.html',
  styleUrl: './user-details.component.scss'
})
export class UserDetailsComponent {

  selectedUser: Observable<User>;
  constructor(private activatedRoute: ActivatedRoute){}

  ngOnInit(): void {
    this.selectedUser = this.activatedRoute.data.pipe(map(data => data['selectedUser']));
  }
}
