import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class ClientService {

    constructor(private httpClient: HttpClient) { }


    getAvailableClients(): Observable<any> {
        return this.httpClient.get(`/clients`);
      }
}