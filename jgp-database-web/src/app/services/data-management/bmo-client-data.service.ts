import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class BMOClientDataService {

    constructor(private httpClient: HttpClient) { }

      getAvailableBMOClientData(page: number, size: number, dataStatus: string, partnerId: number | undefined): Observable<any> {
        const params = new HttpParams()
              .set('pageNumber', page.toString())
              .set('pageSize', size.toString());
        if(partnerId){
          return this.httpClient.get(`/bmos?partnerId=${partnerId}&dataStatus=${dataStatus}`, { params });
        }
        return this.httpClient.get(`/bmos?dataStatus=${dataStatus}`, { params });
      }

      approveBMOClientData(bmoIds: number[]): Observable<any> {
        return this.httpClient.post(`/bmos/approve-or-reject?approved=true`, JSON.stringify(bmoIds));
      }

      rejectBMOClientData(bmoIds: number[]): Observable<any> {
        return this.httpClient.post(`/bmos/approve-or-reject?approved=false`, JSON.stringify(bmoIds));
      }
}