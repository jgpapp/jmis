import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { JwtHelperService } from '@auth0/angular-jwt';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';
import { GlobalService } from '@services/shared/global.service';
import { CurrentUserCredentials } from '../../dto/CurrentUserCredentials';

@Injectable({
  providedIn: 'root'
})
export class AuthService {

  CURRENT_USER_CREDENTIALS: string = 'current_user_credentials';
  AUTH_TOKEN_KEY: string = 'auth_token';
  USER_FULL_NAME: string = 'user_full_name';
  USER_ROLES: string = 'user_roles';
  USER_PERMISSIONS: string = 'user_permissions';
  USER_EMAIL: string = 'user_email';
  USER_POSITION: string = 'user_position';
  USER_PARTNER_ID: string = 'user_partner_id';
  USER_PARTNER_NAME: string = 'user_partner_name';
  USER_PARTNER_TYPE: string = 'user_partner_type';
  USER_REGISTRATION: string = 'user_registration';
  FORCE_PASS_CHANGE: string = 'force_change_password';
  jwtService: JwtHelperService = new JwtHelperService();

  constructor(private httpClient: HttpClient, private globalService: GlobalService, private router: Router) { }

  login = (authRequest: {username: string, password: string}): Observable<any> => this.httpClient.post(`${this.globalService.BASE_API_URL}/users/authenticate`, JSON.stringify(authRequest))

  public setLocalStorageValue = (key : string, value: any): void => localStorage.setItem(key, value)

  public getLocalStorageValue = (key: string): any => localStorage.getItem(key);

  public getAuthToken = (): string | null => this.getLocalStorageValue(this.AUTH_TOKEN_KEY);

  public clearLocalStorageValue = (): void => localStorage.clear();
  
  public decodeAuthToken = (): any =>  this.getAuthToken() === null ? null : this.jwtService.decodeToken(this.getAuthToken()!);

  public storeUserDetails = (token?: string) : void => {
    if(token){
      this.setLocalStorageValue(this.AUTH_TOKEN_KEY, token);
      const userCredentials: CurrentUserCredentials = {
        accessToken: token,
        desgnation: this.decodeAuthToken()[this.USER_POSITION],
        permissions: JSON.stringify(this.decodeAuthToken()[this.USER_PERMISSIONS]),
        registration: this.decodeAuthToken()[this.USER_REGISTRATION],
        roles: JSON.stringify(this.decodeAuthToken()[this.USER_ROLES]),
        username: this.decodeAuthToken()[this.USER_EMAIL],
        email: this.decodeAuthToken()[this.USER_EMAIL],
        partnerId: this.decodeAuthToken()[this.USER_PARTNER_ID],
        partnerName: this.decodeAuthToken()[this.USER_PARTNER_NAME],
        partnerType: this.decodeAuthToken()[this.USER_PARTNER_TYPE],
        rememberMe: false,
        userFullName: this.decodeAuthToken()[this.USER_FULL_NAME],
        forceChangePassword: this.decodeAuthToken()[this.FORCE_PASS_CHANGE]
      }
      console.log(userCredentials)
      this.setLocalStorageValue(this.CURRENT_USER_CREDENTIALS, JSON.stringify(userCredentials));
    }
  };

  public isAuthenticated = () : boolean => this.getAuthToken() !== null && !this.jwtService.isTokenExpired(this.getAuthToken());

  public isUserAdmin(): boolean {
    return (this.decodeAuthToken()['roles'] as Array<string>).includes('Admin');
  }

  userRedirection(): void {
    if(this.isAuthenticated()){
      this.router.navigateByUrl("/");
    }else {
      this.doLogout();
    }
  }

  redirectToChangePassword(): void{
    this.router.navigateByUrl("/change-password");
  }


  doLogout(): void {
    this.clearLocalStorageValue();
    this.router.navigateByUrl("/login");
  }

  currentUser(): CurrentUserCredentials | null {
    if(undefined == this.getLocalStorageValue(this.CURRENT_USER_CREDENTIALS) ){
      this.doLogout();
    }
    return JSON.parse(this.getLocalStorageValue(this.CURRENT_USER_CREDENTIALS))
  }

  hasPermission(permission: string): boolean {
    const userPermissions = this.currentUser()?.permissions
    if(!userPermissions){
      return false;
    }
    permission = permission.trim();
    if (userPermissions.includes('ALL_FUNCTIONS')) {
      return true;
    } else if (permission !== '') {
        if (permission.substring(0, 5) === 'READ_' && userPermissions.includes('ALL_FUNCTIONS_READ')) {
          return true;
        } else if (userPermissions.includes(permission)) {
          return true;
        } else {
          return false;
        }
    } else {
      return false;
    }
  }

}
