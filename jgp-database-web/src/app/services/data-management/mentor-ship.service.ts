import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class MentorShipService {

    constructor(private httpClient: HttpClient) { }

      getAvailableMentorshipData(page: number, size: number, approvedByPartner: Boolean, partnerId: number | undefined): Observable<any> {
        const params = new HttpParams()
              .set('pageNumber', page.toString())
              .set('pageSize', size.toString());
        if(partnerId){
          return this.httpClient.get(`/mentorship?partnerId=${partnerId}&approvedByPartner=${approvedByPartner}`, { params });
        }
        return this.httpClient.get(`/mentorship?approvedByPartner=${approvedByPartner}`, { params });
      }

      approvedMentorShipData(mentorshipIds: number[]): Observable<any> {
        return this.httpClient.post(`/mentorship/approve-or-reject?approved=true`, JSON.stringify(mentorshipIds));
      }

      rejectMentorShipData(mentorshipIds: number[]): Observable<any> {
        return this.httpClient.post(`/mentorship/approve-or-reject?approved=false`, JSON.stringify(mentorshipIds));
      }
}