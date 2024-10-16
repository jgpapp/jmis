import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';
import { GlobalService } from './shared/global.service';

@Injectable({
  providedIn: 'root'
})
export class ClientService {

    constructor(private httpClient: HttpClient, private globalService: GlobalService, private router: Router) { }


    getAvailableClients(): Observable<any> {
        return this.httpClient.get(`${this.globalService.BASE_API_URL}/clients`);
      }
}