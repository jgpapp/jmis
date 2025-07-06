import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HighLevelSummaryDto } from '../../pages/dashboard/dto/highLevelSummaryDto';
import { ChartDialogComponent } from '../../pages/chart-dialog/chart-dialog.component';
import { MatDialog } from '@angular/material/dialog';

@Injectable({
  providedIn: 'root'
})
export class DashboardService {

    constructor(private httpClient: HttpClient) { }


    updateAnalyticsDataSummary(analyticsUpdate: {partnerId: any, fromDate: any, toDate: any}, partnerId: any = undefined): Observable<any> {
      if(partnerId){
        analyticsUpdate.partnerId = partnerId;
      }
      return this.httpClient.post(`/reports/analytics-update`, JSON.stringify(analyticsUpdate));
    }

    getHighLevelSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/high-level-summary?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansDisbursedByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-disbursed-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanDisbursedByIndustrySectorSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-disbursed-by-sector?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanDisbursedByIndustrySegmentSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-disbursed-by-segment?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanDisbursedTopFourPartnersSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-disbursed-top-four-partners?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanDisbursedTopFourCountiesSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-disbursed-top-four-counties?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getBusinessTrainedTopFourCountiesSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/businesses-trained-top-four-counties?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getBusinessesTrainedByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/businesses-trained-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanedBusinessesByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loaned-businesses-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getDisabledBusinessOwnersTrainedByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/disabled-businesses-trained-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getRefugeeBusinessOwnersTrainedByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/refugee-businesses-trained-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/refugee-and-plwd-businesses-trained-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansDisbursedByPipelineSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-disbursed-by-pipeline?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getMentorshipGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/mentorship-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansDisbursedByStatusSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-disbursed-by-quality?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getTaNeedsByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/ta-needs-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getTaTrainingBySectorSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/ta-training-by-sector?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansDisbursedByLoanProductSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-disbursed-by-product?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getParticipantsEmployeesSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/employees-summary?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getParticipantsMentorshipDeliveryModeSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/mentorship-by-delivery-mode?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }
    getParticipantsMentorshipBusiCategoryByCountySummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/business-category-by-county?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getTaTrainingBySegmentSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/ta-training-by-segment?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getTrainingByPartnerByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/training-by-partner-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoanDisbursedByLoanProductByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/disbursed-by-product-by-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLastThreeYearsAccessedLoanPerPartnerSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loan-accessed-per-partner-for-last-three-years?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLastThreeYearsAccessedLoanPerPartnerYearly(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loan-accessed-per-partner-yearly?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLastThreeYearsAccessedLoansCountPerPartnerYearly(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/accessed-loans-count-per-partner-yearly?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLastThreeYearsTrainedBusinessesPerPartnerYearly(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/trained_businesses-per-partner-yearly?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getTaTypeTrainedBusinesses(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/trained_businesses-per-ta-type?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }


    getLoansAccessedVsOutStandingByPartnerSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-accessed-vs-out-standing-per-partner?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getLoansAccessedVsOutStandingByGenderSummary(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/loans-accessed-vs-out-standing-per-gender?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getCountySummaryMap(dashBoardFilters: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/data-summary-map?${this.getDashBoardQueryParams(dashBoardFilters)}`);
    }

    getPerformanceSummary(year: string | undefined, partnerId: number | undefined): Observable<any> {
      return this.httpClient.get(`/reports/performance-summary${this.getPerformanceSummaryQueryParams(year, partnerId)}`);
    }

    getOutcomeMonitoringSummary(dashBoardFilters: any = undefined, summarizingColumn: any = undefined): Observable<any> {
      return this.httpClient.get(`/reports/outcome-monitoring-summary?${this.getDashBoardQueryParams(dashBoardFilters, summarizingColumn)}`);
    }

    getKenyanCounties(): Observable<any> {
      return this.httpClient.get(`/reports/kenyan-counties`);
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


    getDashBoardQueryParams(dashBoardFilters: any, summarizingColumn: any = undefined): string {
      let queryParam = ``
      if(!dashBoardFilters && !summarizingColumn){
        return queryParam;
      }

      if(!dashBoardFilters && summarizingColumn){
        return `${queryParam}&summarizing-column=${summarizingColumn}`;
      }

      if(dashBoardFilters.selectedDateFrom && dashBoardFilters.selectedDateTo) {
        queryParam = `from-date=${dashBoardFilters.selectedDateFrom}&to-date=${dashBoardFilters.selectedDateTo}`
      }
      
      if(dashBoardFilters.selectedPartnerId){
        queryParam = `${queryParam}&partner-id=${dashBoardFilters.selectedPartnerId}`
      }
      if(dashBoardFilters.selectedCountyCode){
        queryParam = `${queryParam}&county-code=${dashBoardFilters.selectedCountyCode}`
      }
      if(dashBoardFilters.selectedTrainingPartner){
        queryParam = `${queryParam}&training-partner=${dashBoardFilters.selectedTrainingPartner}`
      }

      if(dashBoardFilters.partnerName){
        queryParam = `${queryParam}&partner=${dashBoardFilters.partnerName}`
      }
      if(dashBoardFilters.ageGroup){
        queryParam = `${queryParam}&age-group=${dashBoardFilters.ageGroup}`
      }
      if(dashBoardFilters.gender){
        queryParam = `${queryParam}&gender=${dashBoardFilters.gender}`
      }
      if(dashBoardFilters.jgpIntervention){
        queryParam = `${queryParam}&jgp-intervention=${dashBoardFilters.jgpIntervention}`
      }
      if(dashBoardFilters.region){
        queryParam = `${queryParam}&region=${dashBoardFilters.region}`
      }

      if(summarizingColumn){
        queryParam = `${queryParam}&summarizing-column=${summarizingColumn}`
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

    formatNumberWithThousandSeparatorForm(number: any): string {
      // Use the toLocaleString method to add suffixes to the number
      return number && null !== number ? number.toLocaleString('en-US', {
        minimumFractionDigits: 2,
        maximumFractionDigits: 2
      }) : '0.00';
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
        amountDisbursedByTranches: this.formatNumberToShortForm(highLevelSummary.amountDisbursedByTranches),
        businessesMentored: this.formatNumberToShortForm(highLevelSummary.businessesMentored)
      }
    }


}