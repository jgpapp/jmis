import { Injectable } from '@angular/core';
import { Location } from '@angular/common';
import { verticalMenuItems, horizontalMenuItems } from '../common/data/menu';
import { Menu } from '../common/models/menu.model';

@Injectable({
  providedIn: 'root'
})
export class MenuService {

  constructor(private location: Location) { }

  getVerticalMenuItems(): Menu[] {
    return verticalMenuItems;
  }

  getHorizontalMenuItems(): Menu[] {
    return horizontalMenuItems;
  }

  expandActiveSubMenu(menu: Menu[]): void {
    const url = this.location.path();
    const routerLink = url;
    const activeMenuItem = menu.find((item) => item.routerLink === routerLink);
    if (!activeMenuItem) {
      return;
    }
    let menuItem = activeMenuItem;
    while (menuItem.parentId !== 0) {
      const parentMenuItem = menu.find((item) => item.id === menuItem.parentId);
      if (!parentMenuItem) {
        break;
      }
      menuItem = parentMenuItem;
      this.toggleMenuItem(String(menuItem.id));
    }
  }

  toggleMenuItem(menuId: string): void {
    const menuItem = document.getElementById(`menu-item-${menuId}`);
    const subMenu = document.getElementById(`sub-menu-${menuId}`);
    if (!subMenu) {
      return;
    }
    subMenu.classList.toggle('show');
    menuItem?.classList.toggle('expanded');
  }

  closeOtherSubMenus(menu: Menu[], menuId: number): void {
    const currentMenuItem = menu.find((item) => item.id === menuId);
    if (!currentMenuItem || currentMenuItem.parentId !== 0 || currentMenuItem.target) {
      return;
    }
    menu.forEach((item) => {
      if (item.id !== menuId) {
        const subMenu = document.getElementById(`sub-menu-${item.id}`);
        const menuItem = document.getElementById(`menu-item-${item.id}`);
        if (subMenu?.classList.contains('show')) {
          subMenu.classList.remove('show');
          menuItem?.classList.remove('expanded');
        }
      }
    });
  }

  closeAllSubMenus() {
    const menu = document.querySelector("#vertical-menu .ng-scroll-content");
    if (!menu) return;
    const submenuItems = Array.from(menu.children[0].children);
    for (const item of submenuItems) {
      const submenu = item.children[1];
      if (!submenu) continue;
      const isExpanded = submenu.previousElementSibling?.classList.contains("expanded");
      if (isExpanded) {
        submenu.previousElementSibling?.classList.remove("expanded");
        submenu.classList.remove("show");
      }
    }
  }


}
