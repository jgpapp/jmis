import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { GlobalService } from '../shared/global.service';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class LoanService {

    constructor(private httpClient: HttpClient, private gs: GlobalService, private router: Router) { }


    uploadLendingData(file: File): Observable<any> {
        const formData = new FormData();
        formData.append('excelFile', file, file.name);
        return this.httpClient.post(`${this.gs.BASE_API_URL}/loans/upload-template`, formData);
      }

      getAvailableLendingData(page: number, size: number, approvedByPartner: Boolean, partnerId: number | undefined): Observable<any> {
        const params = new HttpParams()
              .set('pageNumber', page.toString())
              .set('pageSize', size.toString());
        if(partnerId){
          return this.httpClient.get(`${this.gs.BASE_API_URL}/loans?partnerId=${partnerId}&approvedByPartner=${approvedByPartner}`, { params });
        }
        return this.httpClient.get(`${this.gs.BASE_API_URL}/loans?approvedByPartner=${approvedByPartner}`, { params });
      }

      approveLoansData(loanIds: number[]): Observable<any> {
        return this.httpClient.post(`${this.gs.BASE_API_URL}/loans/approve-or-reject?approved=true`, JSON.stringify(loanIds));
      }

      disapproveLoansData(loanIds: number[]): Observable<any> {
        return this.httpClient.post(`${this.gs.BASE_API_URL}/loans/approve-or-reject?approved=false`, JSON.stringify(loanIds));
      }
}