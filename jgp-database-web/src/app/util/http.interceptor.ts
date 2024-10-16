import { HttpErrorResponse, HttpInterceptorFn } from '@angular/common/http';
import { inject } from '@angular/core';
import { catchError, throwError } from 'rxjs';
import { GlobalService } from '@services/shared/global.service';
import { AuthService } from '@services/users/auth.service';

export const httpInterceptor: HttpInterceptorFn = (req, next) => {
  const globalService = inject(GlobalService);
  const token: string = localStorage.getItem("auth_token")!;
  const authService: AuthService = inject(AuthService)
  const forceChangePassword: boolean | undefined = authService.currentUser()?.forceChangePassword;
  if(forceChangePassword && true === forceChangePassword){
    authService.redirectToChangePassword()
  }
  if (token != null) {
    req = req.clone({
      setHeaders: { Authorization: `Bearer ${token}`}
    });
  }
  if(!req.url.includes('upload')) {
    req = req.clone({ headers: req.headers.set('Content-Type', 'application/json') });
    req = req.clone({ headers: req.headers.set('Accept', 'application/json') });
  }
  

  return next(req).pipe(
    catchError((err: any) => {
      if (err instanceof HttpErrorResponse) {
        // Handle HTTP errors
        if (err.status === 401) {
          // Specific handling for unauthorized errors         
          globalService.openSnackBar(`${err.error.detail}`, "Dismiss");
          // You might trigger a re-authentication flow or redirect the user here
        } else {
          // Handle other HTTP error codes
          globalService.openSnackBar(`${err.error.detail}`, "Dismiss");
        }
      } else {
        // Handle non-HTTP errors
        globalService.openSnackBar(`${err}`, "Dismiss");
      }

      // Re-throw the error to propagate it further
      return throwError(() => err); 
    })
  );
};
