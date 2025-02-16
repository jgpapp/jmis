import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { GlobalService } from '@services/shared/global.service';
import { Observable } from 'rxjs';
import { HighLevelSummaryDto } from '../../pages/dashboard/dto/highLevelSummaryDto';

@Injectable({
  providedIn: 'root'
})
export class DashboardService {

    constructor(private httpClient: HttpClient, private globalService: GlobalService, private router: Router) { }


    updateAnalyticsDataSummary(analyticsUpdate: {partnerId: any, fromDate: any, toDate: any}, partnerId: any = undefined): Observable<any> {
      if(partnerId){
        analyticsUpdate.partnerId = partnerId;
      }
      return this.httpClient.post(`${this.globalService.BASE_API_URL}/reports/analytics-update`, JSON.stringify(analyticsUpdate));
    }

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

    getLastThreeYearsAccessedLoanPerPartnerYearly(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loan-accessed-per-partner-yearly?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLastThreeYearsAccessedLoansCountPerPartnerYearly(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/accessed-loans-count-per-partner-yearly?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLastThreeYearsTrainedBusinessesPerPartnerYearly(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/trained_businesses-per-partner-yearly?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }


    getLoansAccessedVsOutStandingByPartnerSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-accessed-vs-out-standing-per-partner?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansAccessedVsOutStandingByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/loans-accessed-vs-out-standing-per-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getCountySummaryMap(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/county-summary-map?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getPerformanceSummary(year: string | undefined, partnerId: number | undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/performance-summary${this.getPerformanceSummaryQueryParams(year, partnerId)}`);
    }

    getKenyanCounties(): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/kenyan-counties`);
    }

    getPerformanceSummaryQueryParams(year: string | undefined, partnerId: number | undefined): string {
      let queryParam = ``
      if(!year && !partnerId){
        return queryParam;
      }

      queryParam = '?'
      if(!year && partnerId) {
        return `${queryParam}partner-id=${partnerId}`
      }
      
      if(year && !partnerId){
        return `${queryParam}year=${year}`
      }
        return `${queryParam}partner-id=${partnerId}&year=${year}`;
    }


    getDashBoardQueryParams(dashBoardFilters: any): string {
      //const currentDate = new Date();
      //let dateFrom = new Date(`${currentDate.getFullYear()}-01-01`);
      //let dateTo = new Date(`${currentDate.getFullYear()}-12-31`);
      //let queryParam = `from-date=${dateFrom.toISOString().split('T')[0]}&to-date=${dateTo.toISOString().split('T')[0]}`
      let queryParam = ``
      if(!dashBoardFilters){
        return queryParam;
      }

      if(dashBoardFilters.selectedDateFrom && dashBoardFilters.selectedDateTo) {
        const currentDate = new Date();
        let dateFrom = dashBoardFilters.selectedDateFrom;
        let dateTo = dashBoardFilters.selectedDateTo;
        queryParam = `from-date=${dateFrom.toISOString().split('T')[0]}&to-date=${dateTo.toISOString().split('T')[0]}`
      }
      
      if(dashBoardFilters.selectedPartnerId){
        queryParam = `${queryParam}&partner-id=${dashBoardFilters.selectedPartnerId}`
      }
      if(dashBoardFilters.selectedCountyCode){
        queryParam = `${queryParam}&county-code=${dashBoardFilters.selectedCountyCode}`
      }
        return queryParam;
    }



    formatNumberToShortForm(number: any): string {
      // Use the toLocaleString method to add suffixes to the number
      return number && null !== number ? number.toLocaleString('en-US', {
        // add suffixes for thousands, millions, and billions
        // the maximum number of decimal places to use
        maximumFractionDigits: 2,
        // specify the abbreviations to use for the suffixes
        notation: 'compact',
        compactDisplay: 'short'
      }) : '0';
    }

    getFormattedTileData(highLevelSummary: any): HighLevelSummaryDto {
      return {
        businessesTrained: this.formatNumberToShortForm(highLevelSummary.businessesTrained), 
        businessesLoaned: this.formatNumberToShortForm(highLevelSummary.businessesLoaned), 
        amountDisbursed: this.formatNumberToShortForm(highLevelSummary.amountDisbursed), 
        outStandingAmount: this.formatNumberToShortForm(highLevelSummary.outStandingAmount)
      }
    }


}