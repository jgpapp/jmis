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

  return next(req);
};
