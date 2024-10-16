import { Component, HostListener, OnInit, ViewChild } from '@angular/core';
import { MatDrawerContent } from '@angular/material/sidenav';
import { Settings, SettingsService } from '@services/settings.service';
import { SubscriptionsContainer } from '../theme/utils/subscriptions-container';
import { NavigationEnd, Router, RouterOutlet } from '@angular/router';
import { MenuService } from '@services/menu.service';
import { rotate } from '../theme/utils/app-animation';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { NgScrollbarModule } from 'ngx-scrollbar';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatRadioModule } from '@angular/material/radio';
import { FormsModule } from '@angular/forms';
import { SidenavComponent } from '../theme/components/sidenav/sidenav.component';
import { HorizontalMenuComponent } from '../theme/components/menu/horizontal-menu/horizontal-menu.component';
import { FullScreenComponent } from '../theme/components/fullscreen/fullscreen.component';
import { FlagsMenuComponent } from '../theme/components/flags-menu/flags-menu.component';
import { FavoritesComponent } from '../theme/components/favorites/favorites.component';
import { ApplicationsComponent } from '../theme/components/applications/applications.component';
import { MessagesComponent } from '../theme/components/messages/messages.component';
import { UserMenuComponent } from '../theme/components/user-menu/user-menu.component';
import { TopInfoContentComponent } from '../theme/components/top-info-content/top-info-content.component';

@Component({
  selector: 'app-pages',
  standalone: true,
  imports: [
    RouterOutlet,
    FormsModule,
    FlexLayoutModule,
    NgScrollbarModule,
    MatSidenavModule,
    MatToolbarModule,
    MatIconModule,
    MatButtonModule,
    MatSlideToggleModule,
    MatRadioModule,
    SidenavComponent,
    HorizontalMenuComponent,
    FullScreenComponent,
    FlagsMenuComponent,
    FavoritesComponent,
    ApplicationsComponent,
    MessagesComponent,
    UserMenuComponent,
    TopInfoContentComponent
  ],
  templateUrl: './pages.component.html',
  styleUrl: './pages.component.scss',
  animations: [ rotate ]
})
export class PagesComponent implements OnInit {
  @ViewChild('sidenav') sidenav: any;
  @ViewChild('backToTop') backToTop: any;
  @ViewChild('mainContent') mainContent: MatDrawerContent;
  public settings: Settings;
  public showSidenav: boolean = false;
  public showInfoContent: boolean = false;
  public toggleSearchBar: boolean = false;
  private defaultMenu: string; //declared for return default menu when window resized 
  public menus = ['vertical', 'horizontal'];
  public menuOption: string;
  public menuTypes = ['default', 'compact', 'mini'];
  public menuTypeOption: string;
  subs = new SubscriptionsContainer();

  constructor(public settingsService: SettingsService, public router: Router, private menuService: MenuService) {
    this.settings = this.settingsService.settings;
  }

  ngOnInit() {
    if (window.innerWidth <= 960) {
      this.settings.menu = 'vertical';
      this.settings.sidenavIsOpened = false;
      this.settings.sidenavIsPinned = false;
    }
    this.menuOption = this.settings.menu;
    this.menuTypeOption = this.settings.menuType;
    this.defaultMenu = this.settings.menu;
  }

  ngAfterViewInit() {
    setTimeout(() => { this.settings.loadingSpinner = false }, 300);
    this.backToTop.nativeElement.style.display = 'none';
    this.subs.add = this.router.events.subscribe(event => {
      if (event instanceof NavigationEnd) {
        this.scrollToTop();
      }
      if (window.innerWidth <= 960) {
        this.sidenav.close();
      }
    });
    if (this.settings.menu == "vertical")
      this.menuService.expandActiveSubMenu(this.menuService.getVerticalMenuItems());
  }

  ngOnDestroy() {
    this.subs.dispose();
  }

  public toggleSidenav() {
    this.sidenav.toggle();
  }

  public chooseMenu() {
    this.settings.menu = this.menuOption;
    this.defaultMenu = this.menuOption;
    if (this.menuOption == 'horizontal') {
      this.settings.fixedSidenav = false;
    }
    this.router.navigate(['/']);
  }

  public chooseMenuType() {
    this.settings.menuType = this.menuTypeOption;
  }

  public changeTheme(theme: string) {
    this.settings.theme = theme;
  }

  public closeInfoContent(showInfoContent: boolean) {
    this.showInfoContent = !showInfoContent;
  }

  @HostListener('window:resize') onWindowResize(): void {
    if (window.innerWidth <= 960) {
      this.settings.sidenavIsOpened = false;
      this.settings.sidenavIsPinned = false;
      this.settings.menu = 'vertical'
    }
    else {
      (this.defaultMenu == 'horizontal') ? this.settings.menu = 'horizontal' : this.settings.menu = 'vertical'
      this.settings.sidenavIsOpened = true;
      this.settings.sidenavIsPinned = true;
    }
  }

  public scrollToTop() {
    if (this.settings.fixedHeader && this.settings.fixedSidenav) {
      this.mainContent.scrollTo({
        behavior: 'smooth',
        top: 0
      });
    }
    else {
      window.scrollTo({
        behavior: 'smooth',
        top: 0
      })
    }
  }

  public closeSubMenus() {
    if (this.settings.menu == "vertical") {
      this.menuService.closeAllSubMenus();
    }
  }

  onMainContentScroll(event: any) {
    this.toggleBackToTop(event.target.scrollTop);
  }

  @HostListener('window:scroll') onWindowScroll(): void {
    this.toggleBackToTop(window.scrollY);
  }

  toggleBackToTop(value: number) {
    value > 300 ? this.backToTop.nativeElement.style.display = 'flex' : this.backToTop.nativeElement.style.display = 'none';
  }

}