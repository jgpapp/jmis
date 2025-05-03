import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class ClientService {

    constructor(private httpClient: HttpClient) { }


    getAvailableClients(searchText: string | null, page: number, size: number): Observable<any> {
      let params = new HttpParams()
      .set('pageNumber', page.toString())
      .set('pageSize', size.toString());
      if(searchText && null !== searchText){
        params = new HttpParams()
        .set('pageNumber', page.toString())
        .set('pageSize', size.toString())
        .set('searchText', searchText);
      }
        return this.httpClient.get(`/participants`, { params });
    }

    getParticipantById(participantId: number | string | null): Observable<any> {
      return this.httpClient.get(`/participants/${participantId}?includeAccounts=true`);
    }

    updateParticipant(participantId: number, participantDto: any): Observable<any> {
        return this.httpClient.put(`/participants/${participantId}`, JSON.stringify(participantDto));
      }
}