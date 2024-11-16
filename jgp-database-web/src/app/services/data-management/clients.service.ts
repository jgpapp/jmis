import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { GlobalService } from '../shared/global.service';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class ClientService {

    constructor(private httpClient: HttpClient, private globalService: GlobalService, private router: Router) { }


    getAvailableClients(searchText: string | null, page: number, size: number): Observable<any> {
      console.log(searchText)
      let params = new HttpParams()
      .set('pageNumber', page.toString())
      .set('pageSize', size.toString());
      if(searchText && null !== searchText){
        params = new HttpParams()
        .set('pageNumber', page.toString())
        .set('pageSize', size.toString())
        .set('searchText', searchText);
      }
        return this.httpClient.get(`${this.globalService.BASE_API_URL}/participants`, { params });
    }

    getParticipantById(participantId: number | string | null): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/participants/${participantId}?includeAccounts=true`);
    }
}