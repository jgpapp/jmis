import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class MonitoringServiceService {

   constructor(private httpClient: HttpClient) { }
  
        getOutComeMonitoringDataRecords(page: number, size: number, approved: Boolean): Observable<any> {
          const params = new HttpParams()
                .set('pageNumber', page.toString())
                .set('pageSize', size.toString());
          return this.httpClient.get(`/monitoring?approved=${approved}`, { params });
        }
  
        approvedMonitoringData(monitoringIds: number[]): Observable<any> {
          return this.httpClient.post(`/monitoring/approve-or-reject?approved=true`, JSON.stringify(monitoringIds));
        }
  
        rejectMonitoringData(monitoringIds: number[]): Observable<any> {
          return this.httpClient.post(`/monitoring/approve-or-reject?approved=false`, JSON.stringify(monitoringIds));
        }
}
