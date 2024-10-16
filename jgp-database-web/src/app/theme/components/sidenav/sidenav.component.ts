import { Component, OnInit, ViewEncapsulation } from '@angular/core';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MenuService } from '@services/menu.service';
import { Settings, SettingsService } from '@services/settings.service';
import { NgScrollbarModule } from 'ngx-scrollbar';
import { MatDividerModule } from '@angular/material/divider';
import { VerticalMenuComponent } from '../menu/vertical-menu/vertical-menu.component';
import { AuthService } from '@services/users/auth.service';

@Component({
  selector: 'app-sidenav',
  standalone: true,
  imports: [
    FlexLayoutModule,
    NgScrollbarModule,
    MatDividerModule, 
    VerticalMenuComponent
  ],
  templateUrl: './sidenav.component.html',
  styleUrls: ['./sidenav.component.scss'],
  encapsulation: ViewEncapsulation.None
})
export class SidenavComponent implements OnInit {
  public menuItems: Array<any>;
  public settings: Settings;
  currentUser: any = {}
  constructor(public settingsService: SettingsService, public menuService: MenuService, private authService: AuthService) {
    this.settings = this.settingsService.settings;
  }

  ngOnInit() {
    this.menuItems = this.menuService.getVerticalMenuItems();
    this.currentUser = this.authService.currentUser()
  }

  public closeSubMenus() {
    let menu = document.getElementById("vertical-menu");
    if (menu) {
      for (let i = 0; i < menu.children[0].children.length; i++) {
        let child = menu.children[0].children[i];
        if (child) {
          if (child.children[0].classList.contains('expanded')) {
            child.children[0].classList.remove('expanded');
            child.children[1].classList.remove('show');
          }
        }
      }
    }
  }

}
