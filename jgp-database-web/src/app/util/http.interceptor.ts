import { HttpErrorResponse, HttpInterceptorFn, HttpRequest } from '@angular/common/http';
import { inject } from '@angular/core';
import { BehaviorSubject, catchError, filter, finalize, switchMap, take, throwError, timeout } from 'rxjs';
import { GlobalService } from '@services/shared/global.service';
import { AuthService } from '@services/users/auth.service';

// A simple utility to add the Authorization header
function addToken(request: HttpRequest<unknown>, token: string): HttpRequest<unknown> {
  return request.clone({
    setHeaders: {
      Authorization: `Bearer ${token}`
    }
  });
}

// Global variable to manage the refresh token state
// In a real app, consider a more robust state management solution
// or ensure this subject's lifecycle is tied to the application.
let isRefreshing = false;
let refreshTokenSubject: BehaviorSubject<any> = new BehaviorSubject<any>(null);

export const httpInterceptor: HttpInterceptorFn = (req, next) => {
  const authService = inject(AuthService);
  const token: string = authService.getAccessToken()!;

  // 1. Proactive Check: If token exists and is about to expire, try to refresh
  if (token && authService.isAccessTokenAboutToExpire() && !isRefreshing) {
    // Don't refresh auth/login or auth/refresh requests
    if (!req.url.includes('/authenticate') && !req.url.includes('/refresh-token')) {
      isRefreshing = true;
      refreshTokenSubject.next(null); // Clear previous token

      return authService.refreshToken().pipe(
        switchMap((response: any) => {
          isRefreshing = false;
          refreshTokenSubject.next(response.accessToken);
          // Retry the original request with the new token
          return next(addToken(req, response.accessToken));
        }),
        catchError((error) => {
          isRefreshing = false;
          authService.doLogout(); // If refresh token fails, log out
          return throwError(() => error);
        }),
        finalize(() => {
          isRefreshing = false; // Ensure flag is reset
        })
      );
    }
  }


  const forceChangePassword: boolean | undefined = authService.currentUser()?.forceChangePassword;
  if(forceChangePassword && true === forceChangePassword){
    authService.redirectToChangePassword()
  }
  if (token != null && !req.url.includes('/authenticate')) {
    req = addToken(req, token);
  }
  if(!req.url.includes('upload')) {
    req = req.clone({ headers: req.headers.set('Content-Type', 'application/json') });
    req = req.clone({ headers: req.headers.set('Accept', 'application/json') });
  }
  

  return next(req).pipe(
    catchError((error: HttpErrorResponse) => {
       // THIS IS THE KEY DETECTION POINT FOR 401
      if (error.status === 401 && !req.url.includes('/authenticate') && !req.url.includes('/refresh-token')) {
        // Handle 401 error: Token expired or invalid
        if (!isRefreshing) {
          isRefreshing = true;
          refreshTokenSubject.next(null); // Clear previous token

          return authService.refreshToken().pipe(
            timeout(10000), // Optional: Set a timeout for the refresh token request
            switchMap(response => {
              isRefreshing = false;
              refreshTokenSubject.next(response.accessToken); // Set new access token
              // Retry the original request with the new access token
              return next(addToken(req, response.accessToken));
            }),
            catchError((refreshError) => {
              // If refresh token fails (e.g., refresh token expired/invalid), force logout
              isRefreshing = false;
              refreshTokenSubject.next(null); // Clear the subject
              authService.doLogout(); // AuthService handles navigation to login
              return throwError(() => refreshError); // Propagate the error
            })
          );
        } else {
          // If refresh is already in progress, wait for it to complete
          return refreshTokenSubject.pipe(
            filter(token => token !== null), // Wait until a new token is emitted
            take(1), // Take only the first emitted token
            switchMap(token => {
              // Retry the original request with the newly refreshed access token
              return next(addToken(req, token));
            })
          );
        }
      }
      // Re-throw the error to propagate it further
      return throwError(() => error); 
    })
  );
};
