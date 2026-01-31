import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class LoanService {

    constructor(private httpClient: HttpClient) { }

      getAvailableLendingData(page: number, size: number, dataStatus: string, partnerId: number | undefined): Observable<any> {
        const params = new HttpParams()
              .set('pageNumber', page.toString())
              .set('pageSize', size.toString());
        if(partnerId){
          return this.httpClient.get(`/loans?partnerId=${partnerId}&dataStatus=${dataStatus}`, { params });
        }
        return this.httpClient.get(`/loans?dataStatus=${dataStatus}`, { params });
      }

      getLoanTransactions(page: number, size: number, dataStatus: string, partnerId: number | undefined, loanId: number | undefined = undefined): Observable<any> {
        let params = new HttpParams()
              .set('pageNumber', page.toString())
              .set('pageSize', size.toString())
              .set('dataStatus', dataStatus);
              if(loanId){
                params = params.set('loanId', loanId.toString());
              }
              if(partnerId){
                params = params.set('partnerId', partnerId.toString());
              }
        return this.httpClient.get(`/loans/transactions`, { params });
      }

      getLoanById(loanId: number | string | null): Observable<any> {
        return this.httpClient.get(`/loans/${loanId}`);
      }

      approveLoanTransactions(transactionsIds: number[]): Observable<any> {
        return this.httpClient.post(`/loans/approve-or-reject-transactions?approved=true`, JSON.stringify(transactionsIds));
      }

      rejectLoanTransactions(transactionsIds: number[]): Observable<any> {
        return this.httpClient.post(`/loans/approve-or-reject-transactions?approved=false`, JSON.stringify(transactionsIds));
      }
}