import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { JwtHelperService } from '@auth0/angular-jwt';
import { Router } from '@angular/router';
import { BehaviorSubject, catchError, Observable, tap, throwError } from 'rxjs';
import { CurrentUserCredentials } from '../../dto/CurrentUserCredentials';

@Injectable({
  providedIn: 'root'
})
export class AuthService {

  CURRENT_USER_CREDENTIALS: string = 'current_user_credentials';
  ACCESS_TOKEN_KEY: string = 'auth_token';
  REFRESH_TOKEN_KEY: string = 'refresh_token';
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
  private loggedIn = new BehaviorSubject<boolean>(this.hasTokens());
  redirectUrl: string | null = null;
  isLoggingOut = false;

  constructor(private httpClient: HttpClient, private router: Router, private jwtHelper: JwtHelperService) { 
    this.refreshTokenIfNeeded();
  }

  get isLoggedIn(): Observable<boolean> {
    return this.loggedIn.asObservable();
  }

  login(authRequest: {username: string, password: string}): Observable<any> {
    return this.httpClient.post<any>(`/users/authenticate`, JSON.stringify(authRequest)).pipe(
      tap(response => {
        this.storeUserDetails(response.accessToken, response.refreshToken);
        this.loggedIn.next(this.isAuthenticated());
        this.userRedirection(this.redirectUrl || '/');
        this.redirectUrl = null; // Reset redirect URL after successful login
      }),
      catchError(error => {
        return throwError(() => new Error(error.error.detail || 'Login failed'));
      })
    );
  }

  refreshToken(): Observable<any> {
    const refreshToken = this.getRefreshToken();
    if (!refreshToken) {
      this.doLogout();
      return throwError(() => new Error('No refresh token available. Logging out.'));
    }

    const refreshRequest: any = { refreshToken: refreshToken };
    return this.httpClient.post<any>(`/users/refresh-token`, JSON.stringify(refreshRequest)).pipe(
      tap(response => {
        this.storeUserDetails(response.accessToken, response.refreshToken);
      }),
      catchError(error => {
        console.error('Refresh token failed:', error);
        // If refresh token itself is invalid or expired, force logout
        this.doLogout();
        return throwError(() => new Error('Refresh token invalid or expired. Logging out.'));
      })
    );
  }

  refreshTokenIfNeeded(): boolean {
    if (this.isAccessTokenAboutToExpire()) {
      const refreshRequest: any = { refreshToken: this.getRefreshToken() };
      let isRefreshed: boolean = false;
      this.httpClient.post<any>(`/users/refresh-token`, JSON.stringify(refreshRequest))
      .subscribe({
          next: (response) => {
            this.storeUserDetails(response.accessToken, response.refreshToken);
          isRefreshed = true;
          this.loggedIn.next(this.isAuthenticated());
          },
          error: (error) => {
            isRefreshed = false;
          }
        });
      return isRefreshed;
    } else {
      return false;
      }
    }

  private hasTokens(): boolean {
    return !!this.getAccessToken() && !!this.getRefreshToken();
  }

  getAccessToken(): string | null {
    return localStorage.getItem(this.ACCESS_TOKEN_KEY);
  }

  getRefreshToken(): string | null {
    return localStorage.getItem(this.REFRESH_TOKEN_KEY);
  }

  private checkTokenStatus(): void {
    const accessToken = this.getAccessToken();
    if (accessToken && !this.jwtHelper.isTokenExpired(accessToken)) {
      this.loggedIn.next(this.isAuthenticated());
    } else {
      this.refreshToken().subscribe({
        next: () => this.loggedIn.next(this.isAuthenticated()),
        error: () => {
          this.loggedIn.next(false);
          this.doLogout();
        }
      });
    }
  }

  // Proactive check: Check if access token is about to expire
  isAccessTokenAboutToExpire(thresholdSeconds: number = 60): boolean {
    const accessToken = this.getAccessToken();
    if (!accessToken) {
      return true; // No token, consider it expired
    }
    const expirationDate = this.jwtHelper.getTokenExpirationDate(accessToken);
    if (!expirationDate) {
      return true; // Cannot determine expiration, treat as expired
    }
    const now = new Date();
    const expiresInMs = expirationDate.getTime() - now.getTime();
    return expiresInMs / 1000 < thresholdSeconds;
  }


  saveTokens(accessToken: string, refreshToken: string): void {
    localStorage.setItem(this.ACCESS_TOKEN_KEY, accessToken);
    localStorage.setItem(this.REFRESH_TOKEN_KEY, refreshToken);
  }
  
  public decodeAuthToken = (): any =>  this.getAccessToken() === null ? null : this.jwtHelper.decodeToken(this.getAccessToken()!);

  public storeUserDetails = (token?: string, refreshToken?: string) : void => {
    if(token && refreshToken){
      localStorage.setItem(this.ACCESS_TOKEN_KEY, token);
      localStorage.setItem(this.REFRESH_TOKEN_KEY, refreshToken);
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
      localStorage.setItem(this.CURRENT_USER_CREDENTIALS, JSON.stringify(userCredentials));
    }
  };

  public isAuthenticated = () : boolean => this.getAccessToken() !== null && !this.jwtHelper.isTokenExpired(this.getAccessToken());

  public isUserAdmin(): boolean {
    return (this.decodeAuthToken()['roles'] as Array<string>).includes('Admin');
  }

  userRedirection(redirectUrl: string | undefined = undefined): void {
    if(this.hasTokens()){
      this.router.navigateByUrl(redirectUrl || '/');
    }else {
      this.router.navigateByUrl('/login');
    }
  }

  redirectToChangePassword(): void{
    this.router.navigateByUrl('/change-password');
  }


  doLogout(): void {
    if(this.isLoggingOut) {
      return; // Prevent multiple logout calls
    }
    this.isLoggingOut = true;
    localStorage.clear();
    this.loggedIn.next(false);
    this.router.navigateByUrl('/login');
    this.isLoggingOut = false;
  }

  currentUser(): CurrentUserCredentials | null {
    
    let userDetails = localStorage.getItem(this.CURRENT_USER_CREDENTIALS);
    if(null === userDetails || undefined === userDetails){
      this.doLogout();
      return null;
    }
    return JSON.parse(userDetails)
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
