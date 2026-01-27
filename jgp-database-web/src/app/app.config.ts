import { ApplicationConfig, importProvidersFrom, inject } from '@angular/core';
import { provideRouter } from '@angular/router';

import { routes } from './app.routes';
import { provideAnimationsAsync } from '@angular/platform-browser/animations/async';
import { provideHttpClient, withInterceptors } from '@angular/common/http';
import { OverlayContainer } from '@angular/cdk/overlay';
import { CustomOverlayContainer } from './theme/utils/custom-overlay-container';
import { CalendarModule, DateAdapter } from 'angular-calendar';
import { adapterFactory } from 'angular-calendar/date-adapters/date-fns'; 
import { httpInterceptor } from './util/http.interceptor';
import { apiPrefixInterceptor } from './util/api-prefix.interceptor';
import { JWT_OPTIONS, JwtHelperService } from '@auth0/angular-jwt';
import { AuthService } from '@services/users/auth.service';
import { provideCharts, withDefaultRegisterables } from 'ng2-charts';
import { DatePipe } from '@angular/common';

export function tokenGetter() {
  return inject(AuthService).getAccessToken();
}

export const appConfig: ApplicationConfig = {
  providers: [
    //provideZoneChangeDetection({ eventCoalescing: true }), 
    provideRouter(
      routes
      //withPreloading(PreloadAllModules),  // comment this line for enable lazy-loading
    ), 
    provideAnimationsAsync(),
    provideHttpClient(withInterceptors([apiPrefixInterceptor, httpInterceptor])),
    {
      provide: JWT_OPTIONS,
      useValue: {
        tokenGetter: tokenGetter,
      },
    },
    JwtHelperService,
    DatePipe,
   // importProvidersFrom(InMemoryWebApiModule.forRoot(UsersData, { delay: 1000 })),
    importProvidersFrom(CalendarModule.forRoot({
      provide: DateAdapter,
      useFactory: adapterFactory
    })),
    { provide: OverlayContainer, useClass: CustomOverlayContainer },
    provideCharts(withDefaultRegisterables())
  ]
};
