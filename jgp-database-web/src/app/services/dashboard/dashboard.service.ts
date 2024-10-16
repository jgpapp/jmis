import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { GlobalService } from '@services/shared/global.service';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class DashboardService {

    constructor(private httpClient: HttpClient, private globalService: GlobalService, private router: Router) { }


    getHighLevelSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/high-level-summary${queryParam}`);
    }

    getLoansDisbursedByGenderSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-by-gender${queryParam}`);
    }

    getBusinessesTrainedByGenderSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/businesses-trained-by-gender${queryParam}`);
    }

    getLoansDisbursedByPipelineSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-by-pipeline${queryParam}`);
    }

    getLoansDisbursedByStatusSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-by-quality${queryParam}`);
    }

    getTaNeedsByGenderSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/ta-needs-by-gender${queryParam}`);
    }

    getTaTrainingBySectorSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/ta-training-by-sector${queryParam}`);
    }

    getTaTrainingBySegmentSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/ta-training-by-segment${queryParam}`);
    }

    getTrainingByPartnerByGenderSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/training-by-partner-by-gender${queryParam}`);
    }

    getLastThreeYearsAccessedLoanPerPartnerSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loan-accessed-per-partner-for-last-three-years${queryParam}`);
    }

    getLoansAccessedVsOutStandingByPartnerSummary(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-accessed-vs-out-standing-per-partner${queryParam}`);
    }

    getCountySummaryMap(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/county-summary-map${queryParam}`);
    }

}