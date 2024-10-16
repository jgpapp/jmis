import { inject } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivateFn, Router, RouterStateSnapshot, UrlTree } from '@angular/router';
import { AuthService } from '@services/users/auth.service';
import { Observable } from 'rxjs';

export const AuthGuard: CanActivateFn = (
    route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot
  ):
    Observable<boolean | UrlTree> 
    | Promise<boolean | UrlTree> 
    | boolean 
    | UrlTree=> {
    
      const router = inject(Router);
      const userIsAuthorized: boolean = inject(AuthService).isAuthenticated()

      if(userIsAuthorized){
        return true;
      } else {
        router.navigateByUrl('/login');
        return false;
      }
  
  };