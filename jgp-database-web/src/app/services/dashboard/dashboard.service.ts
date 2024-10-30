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


    getHighLevelSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/high-level-summary?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansDisbursedByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanDisbursedByIndustrySectorSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-by-sector?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanDisbursedByIndustrySegmentSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-by-segment?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanDisbursedTopFourPartnersSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-top-four-partners?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanDisbursedTopFourCountiesSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-top-four-counties?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getBusinessTrainedTopFourCountiesSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/businesses-trained-top-four-counties?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getBusinessesTrainedByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/businesses-trained-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansDisbursedByPipelineSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-by-pipeline?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansDisbursedByStatusSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-disbursed-by-quality?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getTaNeedsByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/ta-needs-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getTaTrainingBySectorSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/ta-training-by-sector?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getTaTrainingBySegmentSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/ta-training-by-segment?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getTrainingByPartnerByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/training-by-partner-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLastThreeYearsAccessedLoanPerPartnerSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loan-accessed-per-partner-for-last-three-years?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansAccessedVsOutStandingByPartnerSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-accessed-vs-out-standing-per-partner?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansAccessedVsOutStandingByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-accessed-vs-out-standing-per-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getCountySummaryMap(partnerId: number | undefined = undefined): Observable<any> {
      const queryParam = (partnerId ? `?partner-id=${partnerId}` : '');
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/county-summary-map${queryParam}`);
    }

    getKenyanCounties(): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/kenyan-counties`);
    }


    getDashBoardQueryParams(dashBoardFilters: any): string {
      const currentDate = new Date();
      let dateFrom = new Date(`${currentDate.getFullYear()}-01-01`);
      let dateTo = new Date(`${currentDate.getFullYear()}-12-31`);
      let queryParam = `from-date=${dateFrom.toISOString().split('T')[0]}&to-date=${dateTo.toISOString().split('T')[0]}`
      if(!dashBoardFilters){
        return queryParam;
      }
      
      if(dashBoardFilters.selectedDateFrom && dashBoardFilters.selectedDateTo) {
        const currentDate = new Date();
        dateFrom = dashBoardFilters.selectedDateFrom;
        dateTo = dashBoardFilters.selectedDateTo;
      }
      queryParam = `from-date=${dateFrom.toISOString().split('T')[0]}&to-date=${dateTo.toISOString().split('T')[0]}`
      if(dashBoardFilters.selectedPartnerId){
        queryParam = `${queryParam}&partner-id=${dashBoardFilters.selectedPartnerId}`
      }
      if(dashBoardFilters.selectedCountyCode){
        queryParam = `${queryParam}&county-code=${dashBoardFilters.selectedCountyCode}`
      }
        return queryParam;
    }
}