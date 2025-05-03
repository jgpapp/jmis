/** Angular Imports */
import { HttpErrorResponse, HttpInterceptorFn } from '@angular/common/http';
import { inject } from '@angular/core';

/** rxjs Imports */
import { GlobalService } from '@services/shared/global.service';
import { catchError, throwError } from 'rxjs';


/**
 * Http request interceptor to prefix a request with `serverUrl`.
 */

export const apiPrefixInterceptor: HttpInterceptorFn = (req, next) => {
  const globalService = inject(GlobalService);
  if (!req.url.includes('http:') && !req.url.includes('https:')) {
    req = req.clone({ url: globalService.baseApiUrl + req.url });
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
