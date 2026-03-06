import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class MentorShipService {

    constructor(private httpClient: HttpClient) { }

      getAvailableMentorshipData(page: number, size: number, dataStatus: string, partnerId: number | undefined): Observable<any> {
        const params = new HttpParams()
              .set('pageNumber', page.toString())
              .set('pageSize', size.toString());
        if(partnerId){
          return this.httpClient.get(`/mentorship?partnerId=${partnerId}&dataStatus=${dataStatus}`, { params });
        }
        return this.httpClient.get(`/mentorship?dataStatus=${dataStatus}`, { params });
      }

      approvedMentorShipData(mentorshipIds: number[]): Observable<any> {
        return this.httpClient.post(`/mentorship/approve-or-reject?approved=true`, JSON.stringify(mentorshipIds));
      }

      rejectMentorShipData(mentorshipIds: number[]): Observable<any> {
        return this.httpClient.post(`/mentorship/approve-or-reject?approved=false`, JSON.stringify(mentorshipIds));
      }
}