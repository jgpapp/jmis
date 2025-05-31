package com.jgp.dashboard.api;

import com.jgp.dashboard.dto.AnalyticsUpdateRequestDto;
import com.jgp.dashboard.dto.CountyDto;
import com.jgp.dashboard.dto.DataSummaryDto;
import com.jgp.dashboard.dto.DashboardSearchCriteria;
import com.jgp.dashboard.dto.DataPointDto;
import com.jgp.dashboard.dto.HighLevelSummaryDto;
import com.jgp.dashboard.dto.PartnerYearlyDataDto;
import com.jgp.dashboard.dto.PerformanceSummaryDto;
import com.jgp.dashboard.dto.SeriesDataPointDto;
import com.jgp.dashboard.dto.TaTypeTrainedBusinessDto;
import com.jgp.dashboard.service.DashboardService;
import com.jgp.shared.dto.ApiResponseDto;
import com.jgp.util.CommonUtil;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/reports")
public class DashboardController {

    private final DashboardService dashboardService;

    @PostMapping("analytics-update")
    public ResponseEntity<ApiResponseDto> updateAnalyticsData(@Valid @RequestBody AnalyticsUpdateRequestDto analyticsUpdateRequestDto){
        this.dashboardService.updateAnalyticsData(analyticsUpdateRequestDto);
        return new ResponseEntity<>(new ApiResponseDto(true, CommonUtil.RESOURCE_CREATED), HttpStatus.CREATED);
    }

    @GetMapping("high-level-summary")
    public ResponseEntity<HighLevelSummaryDto> getHighLevelSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                   @RequestParam(value = "county-code", required = false) String countyCode,
                                                                   @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                   @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                   @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getHighLevelSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-gender")
    public ResponseEntity<List<DataPointDto>> getLoansDisbursedByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                               @RequestParam(value = "county-code", required = false) String countyCode,
                                                                               @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                               @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedByGenderSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("loaned-businesses-by-gender")
    public ResponseEntity<List<DataPointDto>> getLoanedBusinessesByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                               @RequestParam(value = "county-code", required = false) String countyCode,
                                                                               @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                               @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoanedBusinessesByGenderSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-sector")
    public ResponseEntity<List<DataPointDto>> getLoanDisbursedByIndustrySectorSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                      @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                      @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                      @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedByIndustrySectorSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-segment")
    public ResponseEntity<List<DataPointDto>> getLoanDisbursedByIndustrySegmentSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                       @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                      @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                      @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedByIndustrySegmentSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-top-four-partners")
    public ResponseEntity<List<DataPointDto>> getLoanDisbursedTopFourPartnersSummary(@RequestParam(value = "county-code", required = false) String countyCode,
                                                                             @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                             @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedTopFourPartnersSummary(new DashboardSearchCriteria(fromDate, toDate, null, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-top-four-counties")
    public ResponseEntity<List<DataPointDto>> getLoanDisbursedTopFourCountiesSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                     @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                     @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedTopFourCountiesSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, null, null)), HttpStatus.OK);
    }

    @GetMapping("businesses-trained-top-four-counties")
    public ResponseEntity<List<DataPointDto>> getBusinessTrainedTopFourCountiesSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                     @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                     @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getBusinessTrainedTopFourCountiesSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, null, null)), HttpStatus.OK);
    }

    @GetMapping("businesses-trained-by-gender")
    public ResponseEntity<List<DataPointDto>> getBusinessesByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                           @RequestParam(value = "county-code", required = false) String countyCode,
                                                                           @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getBusinessOwnersTrainedByGenderSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("disabled-businesses-trained-by-gender")
    public ResponseEntity<List<DataPointDto>> getDisabledBusinessOwnersTrainedByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                              @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                              @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                              @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                              @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getDisabledBusinessOwnersTrainedByGenderSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("refugee-businesses-trained-by-gender")
    public ResponseEntity<List<DataPointDto>> getRefugeeBusinessOwnersTrainedByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                             @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                             @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                             @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                             @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getRefugeeBusinessOwnersTrainedByGenderSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("refugee-and-plwd-businesses-trained-by-gender")
    public ResponseEntity<List<DataPointDto>> getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                             @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                             @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                             @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                             @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-pipeline")
    public ResponseEntity<List<DataPointDto>> getLoansDisbursedByPipelineSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                 @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                 @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                 @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedByPipelineSourceSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-quality")
    public ResponseEntity<List<DataPointDto>> getLoansDisbursedByQualitySummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoansDisbursedByQualitySummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("ta-needs-by-gender")
    public ResponseEntity<List<SeriesDataPointDto>> getTaNeedsByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                              @RequestParam(value = "county-code", required = false) String countyCode,
                                                                              @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                              @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                              @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getTaNeedsByGenderSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("ta-training-by-sector")
    public ResponseEntity<List<DataPointDto>> getTaTrainingBySectorSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                           @RequestParam(value = "county-code", required = false) String countyCode,
                                                                           @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getTaTrainingBySectorSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-product")
    public ResponseEntity<List<DataPointDto>> getLoansDisbursedByLoanProductSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                           @RequestParam(value = "county-code", required = false) String countyCode,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoansDisbursedByLoanProductSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("employees-summary")
    public ResponseEntity<List<DataPointDto>> getParticipantsEmployeesSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getParticipantsEmployeesSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, null, null)), HttpStatus.OK);
    }

    @GetMapping("ta-training-by-segment")
    public ResponseEntity<List<DataPointDto>> getTaTrainingBySegmentSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                            @RequestParam(value = "county-code", required = false) String countyCode,
                                                                            @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getTaTrainingBySegmentSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("training-by-partner-by-gender")
    public ResponseEntity<List<SeriesDataPointDto>> getTrainingByPartnerByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                        @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                        @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                        @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                        @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getTrainingByPartnerByGenderSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("loan-accessed-per-partner-for-last-three-years")
    public ResponseEntity<List<SeriesDataPointDto>> getLastThreeYearsAccessedLoanPerPartnerSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                   @RequestParam(value = "county-code", required = false) String countyCode){
        return new ResponseEntity<>(this.dashboardService.getLastThreeYearsAccessedLoanPerPartnerSummary(new DashboardSearchCriteria(null, null, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("loan-accessed-per-partner-yearly")
    public ResponseEntity<List<PartnerYearlyDataDto>> getLastThreeYearsAccessedLoanPerPartnerYearly(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                    @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                    @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                    @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLastThreeYearsAccessedLoanAmountPerPartnerYearly(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("accessed-loans-count-per-partner-yearly")
    public ResponseEntity<List<PartnerYearlyDataDto>> getLastThreeYearsAccessedLoansCountPerPartnerYearly(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                          @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                          @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                          @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLastThreeYearsAccessedLoansCountPerPartnerYearly(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("trained_businesses-per-partner-yearly")
    public ResponseEntity<List<PartnerYearlyDataDto>> getLastThreeYearsTrainedBusinessesPerPartnerYearly(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                         @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                         @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                                         @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                         @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLastThreeYearsTrainedBusinessesPerPartnerYearly(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("trained_businesses-per-ta-type")
    public ResponseEntity<List<TaTypeTrainedBusinessDto>> getTaTypeTrainedBusinesses(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                             @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                             @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                                             @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                             @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getTaTypeTrainedBusinesses(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, trainingPartner)), HttpStatus.OK);
    }

    @GetMapping("loans-accessed-vs-out-standing-per-partner")
    public ResponseEntity<List<SeriesDataPointDto>> getLoansAccessedVsOutStandingByPartnerSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                  @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                  @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                  @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoansAccessedVsOutStandingByPartnerSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("loans-accessed-vs-out-standing-per-gender")
    public ResponseEntity<List<SeriesDataPointDto>> getLoansAccessedVsOutStandingByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                 @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                  @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                  @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getLoansAccessedVsOutStandingByGenderSummary(new DashboardSearchCriteria(fromDate, toDate, partnerId, countyCode, null)), HttpStatus.OK);
    }

    @GetMapping("data-summary")
    public ResponseEntity<List<DataSummaryDto>> getDataSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                 @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                 @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getDataSummary(fromDate, toDate, partnerId), HttpStatus.OK);
    }

    @GetMapping("data-summary-map")
    public ResponseEntity<Map<String, DataSummaryDto>> getDataSummaryMap(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getDataSummaryMap(fromDate, toDate, partnerId), HttpStatus.OK);
    }

    @GetMapping("kenyan-counties")
    public ResponseEntity<List<CountyDto>> getKenyanCounties(){
        return new ResponseEntity<>(new ArrayList<>(EnumSet.allOf(CommonUtil.KenyanCounty.class))
                .stream().map(county -> new CountyDto(county.getCountyCode(), county.getCountyName()))
                .sorted(Comparator.comparing(CountyDto::countyName))
                .toList(), HttpStatus.OK);
    }

    @GetMapping("performance-summary")
    public ResponseEntity<List<PerformanceSummaryDto>> getPerformanceSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                             @RequestParam(value = "year", required = false) String year){
        return new ResponseEntity<>(this.dashboardService.getPerformanceSummary(year, partnerId), HttpStatus.OK);
    }
}
