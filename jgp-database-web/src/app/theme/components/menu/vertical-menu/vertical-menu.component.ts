import { Component, OnInit, Input, ViewEncapsulation } from '@angular/core';
import { MenuService } from '@services/menu.service';
import { Settings, SettingsService } from '@services/settings.service';
import { Menu } from '../../../../common/models/menu.model';
import { RouterModule } from '@angular/router';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';
import { AuthService } from '@services/users/auth.service';

@Component({
    selector: 'app-vertical-menu',
    standalone: true,
    imports: [
        RouterModule,
        FlexLayoutModule,
        MatButtonModule,
        MatIconModule,
        MatTooltipModule
    ],
    templateUrl: './vertical-menu.component.html',
    styleUrls: ['./vertical-menu.component.scss'],
    encapsulation: ViewEncapsulation.None
})
export class VerticalMenuComponent implements OnInit {
  @Input('menuItems') menuItems: Menu[];
  @Input('menuParentId') menuParentId: any;
  parentMenu: Array<any>;
  public settings: Settings;
  currentUser: any = {}
  myDashboardMenu?: Menu; 
  constructor(public settingsService: SettingsService, public menuService: MenuService, private authService: AuthService) {
    this.settings = this.settingsService.settings;
  }


  ngOnInit() {
    this.parentMenu = this.menuItems.filter(item => item.parentId == this.menuParentId);
    this.currentUser = this.authService.currentUser();  
    this.myDashboardMenu = this.menuItems.find(item => item.id === 2);
    if(this.myDashboardMenu && this.currentUser){
      this.myDashboardMenu.title = `${this.currentUser.partnerName} Dashboard`;
    }else {
      this.parentMenu = this.parentMenu.filter(item => item.id !== 2);
    }
  }

  onClick(menuId: any) {
    this.menuService.toggleMenuItem(menuId);
    this.menuService.closeOtherSubMenus(this.menuItems, menuId);
  }

}
