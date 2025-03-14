import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { GlobalService } from '@services/shared/global.service';
import { Observable } from 'rxjs';
import { HighLevelSummaryDto } from '../../pages/dashboard/dto/highLevelSummaryDto';
import { ChartDialogComponent } from '../../pages/chart-dialog/chart-dialog.component';
import { MatDialog } from '@angular/material/dialog';

@Injectable({
  providedIn: 'root'
})
export class DashboardService {

    constructor(private httpClient: HttpClient, private globalService: GlobalService, private router: Router) { }


    updateAnalyticsDataSummary(analyticsUpdate: {partnerId: any, fromDate: any, toDate: any}, partnerId: any = undefined): Observable<any> {
      if(partnerId){
        analyticsUpdate.partnerId = partnerId;
      }
      analyticsUpdate.fromDate.setMinutes(analyticsUpdate.fromDate.getMinutes() - analyticsUpdate.fromDate.getTimezoneOffset());
      analyticsUpdate.toDate.setMinutes(analyticsUpdate.toDate.getMinutes() - analyticsUpdate.toDate.getTimezoneOffset());
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

    getDisabledBusinessOwnersTrainedByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/disabled-businesses-trained-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getRefugeeBusinessOwnersTrainedByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/refugee-businesses-trained-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
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

    getParticipantsEmployeesSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/reports/employees-summary?${this.getDashBoardQueryParams(dashBoardFilters)}`);
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
      let queryParam = ``
      if(!dashBoardFilters){
        return queryParam;
      }

      if(dashBoardFilters.selectedDateFrom && dashBoardFilters.selectedDateTo) {
        let dateFrom = dashBoardFilters.selectedDateFrom;
        dateFrom.setMinutes(dateFrom.getMinutes() - dateFrom.getTimezoneOffset());
        let dateTo = dashBoardFilters.selectedDateTo;
        dateTo.setMinutes(dateTo.getMinutes() - dateTo.getTimezoneOffset());
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


    openExpandedChartDialog(dialog: MatDialog, chartData: any): void {
      // Dynamically calculate dialog size
      const dialogWidth = window.innerWidth;
      const dialogHeight = window.innerHeight;
      const dialogRef = dialog.open(ChartDialogComponent, {
        width: `${dialogWidth}px`,
        height: `${dialogHeight}px`,
        data: chartData,
        panelClass: 'custom-dialog-container',
      });
  
      dialogRef.afterClosed().subscribe(result => {
        console.log('Dialog was closed');
      });
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