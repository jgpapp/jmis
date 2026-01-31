import { Component, OnInit, ViewEncapsulation } from '@angular/core';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatMenuModule } from '@angular/material/menu';
import { MatToolbarModule } from '@angular/material/toolbar';
import { RouterModule } from '@angular/router';
import { MatDividerModule } from '@angular/material/divider';
import { AuthService } from '@services/users/auth.service';

@Component({
    selector: 'app-user-menu',
    standalone: true,
    imports: [
        RouterModule,
        FlexLayoutModule,
        MatButtonModule,
        MatIconModule,
        MatMenuModule,
        MatToolbarModule,
        MatDividerModule
    ],
    templateUrl: './user-menu.component.html',
    styleUrls: ['./user-menu.component.scss'],
    encapsulation: ViewEncapsulation.None
})
export class UserMenuComponent implements OnInit {
  public userImage = "img/users/default-user.jpg";
  currentUser: any = {}
  constructor(private authService: AuthService) { }

  doLogout(): void {
    this.authService.doLogout()
  }

  ngOnInit() {
    this.currentUser = this.authService.currentUser()
  }

}
