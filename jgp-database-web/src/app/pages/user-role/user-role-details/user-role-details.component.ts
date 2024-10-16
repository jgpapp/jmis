import { Component, OnInit } from '@angular/core';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { map, Observable } from 'rxjs';
import { AsyncPipe } from '@angular/common';
import { UserRoleDto } from '../../../dto/UserRoleDto';
import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatDividerModule } from '@angular/material/divider';
import { HasPermissionDirective } from '../../../directives/has-permission.directive';

@Component({
  selector: 'app-user-role-details',
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
  templateUrl: './user-role-details.component.html',
  styleUrl: './user-role-details.component.scss'
})
export class UserRoleDetailsComponent implements OnInit {

  selectedUserRole: Observable<UserRoleDto>;
  constructor(private activatedRoute: ActivatedRoute){}

  ngOnInit(): void {
    this.selectedUserRole = this.activatedRoute.data.pipe(map(data => data['selectedUserRole']));
  }

}
