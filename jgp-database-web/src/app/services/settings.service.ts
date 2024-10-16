import { Injectable } from '@angular/core';

export class Settings {
  constructor(public name: string,
    public loadingSpinner: boolean,
    public fixedHeader: boolean,
    public fixedSidenav: boolean,
    public fixedFooter: boolean,
    public sidenavIsOpened: boolean,
    public sidenavIsPinned: boolean,
    public menu: string,
    public menuType: string,
    public theme: string,
    public rtl: boolean) { }
}

@Injectable({
  providedIn: 'root'
})
export class SettingsService {

  public settings = new Settings(
    'JGP',      //theme name
    true,           //loadingSpinner
    true,           //fixedHeader
    true,           //fixedSidenav 
    false,          //fixedFooter
    true,           //sidenavIsOpened
    true,           //sidenavIsPinned  
    'vertical',     //horizontal , vertical
    'default',      //default, compact, mini
    'indigo-light', //indigo-light, teal-light, red-light, gray-light, blue-dark, green-dark, pink-dark, gray-dark
    false           // true = rtl, false = ltr
  )

  constructor() { }
}
