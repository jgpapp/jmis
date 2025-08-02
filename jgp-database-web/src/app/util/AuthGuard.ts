import { inject } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivateFn, Router, RouterStateSnapshot, UrlTree } from '@angular/router';
import { AuthService } from '@services/users/auth.service';
import { map, Observable, take } from 'rxjs';

export const AuthGuard: CanActivateFn = (
    _route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot
  ):
    Observable<boolean | UrlTree> 
    | Promise<boolean | UrlTree> 
    | boolean 
    | UrlTree=> {
    
      const router = inject(Router);
      const authService = inject(AuthService);

      return authService.isLoggedIn.pipe(
      take(1),
      map((isLoggedIn: boolean) => {
        if (!isLoggedIn) {
          authService.redirectUrl = state.url;
          authService.refreshToken();
        }
        return true;
      })
    );
  
  };