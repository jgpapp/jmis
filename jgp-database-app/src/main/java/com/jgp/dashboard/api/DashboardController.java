package com.jgp.dashboard.api;

import com.jgp.dashboard.dto.AnalyticsUpdateRequestDto;
import com.jgp.dashboard.dto.CountyDataSummaryResponseDto;
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
import com.jgp.monitoring.domain.predicate.OutComeMonitoringSearchCriteria;
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
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getHighLevelSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("county-summary")
    public ResponseEntity<List<CountyDataSummaryResponseDto>> getCountySummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                  @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                  @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                  @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                  @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getCountySummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-gender")
    public ResponseEntity<List<DataPointDto>> getLoansDisbursedByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                               @RequestParam(value = "county-code", required = false) String countyCode,
                                                                               @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                               @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedByGenderSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loaned-businesses-by-gender")
    public ResponseEntity<List<DataPointDto>> getLoanedBusinessesByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                               @RequestParam(value = "county-code", required = false) String countyCode,
                                                                               @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                               @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoanedBusinessesByGenderSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-sector")
    public ResponseEntity<List<DataPointDto>> getLoanDisbursedByIndustrySectorSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                      @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                      @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                      @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedByIndustrySectorSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-segment")
    public ResponseEntity<List<DataPointDto>> getLoanDisbursedByIndustrySegmentSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                       @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                      @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                      @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedByIndustrySegmentSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-top-four-partners")
    public ResponseEntity<List<DataPointDto>> getLoanDisbursedTopFourPartnersSummary(@RequestParam(value = "county-code", required = false) String countyCode,
                                                                             @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                             @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedTopFourPartnersSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-top-four-counties")
    public ResponseEntity<List<DataPointDto>> getLoanDisbursedTopFourCountiesSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                     @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                     @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedTopFourCountiesSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("businesses-trained-top-four-counties")
    public ResponseEntity<List<DataPointDto>> getBusinessTrainedTopFourCountiesSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                     @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                     @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .build();
        return new ResponseEntity<>(this.dashboardService.getBusinessTrainedTopFourCountiesSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("businesses-trained-and-took-loan-count")
    public ResponseEntity<List<DataPointDto>> getParticipantCountForTrainedAndTookLoanSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                              @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                              @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                              @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                              @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .build();
        return new ResponseEntity<>(this.dashboardService.getParticipantCountForTrainedAndTookLoanSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("businesses-trained-by-gender")
    public ResponseEntity<List<DataPointDto>> getBusinessesByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                           @RequestParam(value = "county-code", required = false) String countyCode,
                                                                           @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getBusinessOwnersTrainedByGenderSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("disabled-businesses-trained-by-gender")
    public ResponseEntity<List<DataPointDto>> getDisabledBusinessOwnersTrainedByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                              @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                              @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                              @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                              @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getDisabledBusinessOwnersTrainedByGenderSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("refugee-businesses-trained-by-gender")
    public ResponseEntity<List<DataPointDto>> getRefugeeBusinessOwnersTrainedByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                             @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                             @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                             @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                             @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getRefugeeBusinessOwnersTrainedByGenderSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("refugee-and-plwd-businesses-trained-by-gender")
    public ResponseEntity<List<DataPointDto>> getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                             @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                             @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                             @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                             @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-pipeline")
    public ResponseEntity<List<DataPointDto>> getLoansDisbursedByPipelineSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                 @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                 @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                 @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedByPipelineSourceSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("mentorship-by-given-field")
    public ResponseEntity<List<DataPointDto>> getMentorshipGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                 @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                 @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                 @RequestParam(value = "to-date", required = false) LocalDate toDate,
                                                                         @RequestParam(value = "given-field") String givenField){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getMentorshipByGivenFieldSummary(searchCriteria, givenField), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-quality")
    public ResponseEntity<List<DataPointDto>> getLoansDisbursedByQualitySummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoansDisbursedByQualitySummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("ta-needs-by-gender")
    public ResponseEntity<List<SeriesDataPointDto>> getTaNeedsByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                              @RequestParam(value = "county-code", required = false) String countyCode,
                                                                              @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                              @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                              @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getTaNeedsByGenderSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("business-category-by-county")
    public ResponseEntity<List<DataPointDto>> getBusinessCategoryByCountySummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                              @RequestParam(value = "county-code", required = false) String countyCode,
                                                                              @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                              @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getBusinessCategoryByCountySummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("ta-training-by-sector")
    public ResponseEntity<List<DataPointDto>> getTaTrainingBySectorSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                           @RequestParam(value = "county-code", required = false) String countyCode,
                                                                           @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getTaTrainingBySectorSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loans-disbursed-by-product")
    public ResponseEntity<List<DataPointDto>> getLoansDisbursedByLoanProductSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                           @RequestParam(value = "county-code", required = false) String countyCode,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoansDisbursedByLoanProductSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("outcome-monitoring-summary")
    public ResponseEntity<List<DataPointDto>> getOutcomeMonitoringSummary(@RequestParam(value = "partner", required = false) String partner,
                                                                          @RequestParam(value = "county-code", required = false) String countyCode,
                                                                          @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                          @RequestParam(value = "to-date", required = false) LocalDate toDate,
                                                                          @RequestParam(value = "age-group", required = false) String ageGroup,
                                                                          @RequestParam(value = "gender", required = false) String gender,
                                                                          @RequestParam(value = "gender-category", required = false) String genderCategory,
                                                                          @RequestParam(value = "jgp-intervention", required = false) String jgpIntervention,
                                                                          @RequestParam(value = "region", required = false) String region,
                                                                          @RequestParam(value = "summarizing-column", required = false) String summarizingColumn){
        final var searchCriteria = OutComeMonitoringSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .participantAgeGroup(ageGroup)
                .partner(partner)
                .countyCode(countyCode)
                .gender(gender)
                .genderCategory(genderCategory)
                .jgpIntervention(jgpIntervention)
                .region(region)
                .summarizingColumn(summarizingColumn)
                .build();
        return new ResponseEntity<>(this.dashboardService.getOutcomeMonitoringSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("employees-summary")
    public ResponseEntity<List<DataPointDto>> getParticipantsEmployeesSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .build();
        return new ResponseEntity<>(this.dashboardService.getParticipantsEmployeesSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("ta-training-by-segment")
    public ResponseEntity<List<DataPointDto>> getTaTrainingBySegmentSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                            @RequestParam(value = "county-code", required = false) String countyCode,
                                                                            @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                           @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                           @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getTaTrainingBySegmentSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("mentorship-by-delivery-mode")
    public ResponseEntity<List<DataPointDto>> getParticipantsMentorshipDeliveryModeSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                            @RequestParam(value = "county-code", required = false) String countyCode,
                                                                            @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                            @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getParticipantsMentorshipDeliveryModeSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("training-by-partner-by-gender")
    public ResponseEntity<List<SeriesDataPointDto>> getTrainingByPartnerByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                        @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                        @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                        @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                        @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getTrainingByPartnerByGenderSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("disbursed-by-product-by-gender")
    public ResponseEntity<List<SeriesDataPointDto>> getLoanDisbursedByLoanProductByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                        @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                        @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                        @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoanDisbursedByLoanProductByGenderSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loan-accessed-per-partner-for-last-three-years")
    public ResponseEntity<List<SeriesDataPointDto>> getLastThreeYearsAccessedLoanPerPartnerSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                   @RequestParam(value = "county-code", required = false) String countyCode){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLastThreeYearsAccessedLoanPerPartnerSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loan-accessed-per-partner-yearly")
    public ResponseEntity<List<PartnerYearlyDataDto>> getLastThreeYearsAccessedLoanPerPartnerYearly(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                    @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                    @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                    @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLastThreeYearsAccessedLoanAmountPerPartnerYearly(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("accessed-loans-count-per-partner-yearly")
    public ResponseEntity<List<PartnerYearlyDataDto>> getLastThreeYearsAccessedLoansCountPerPartnerYearly(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                          @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                          @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                          @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLastThreeYearsAccessedLoansCountPerPartnerYearly(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("trained_businesses-per-partner-yearly")
    public ResponseEntity<List<PartnerYearlyDataDto>> getLastThreeYearsTrainedBusinessesPerPartnerYearly(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                         @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                         @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                                         @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                         @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLastThreeYearsTrainedBusinessesPerPartnerYearly(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("trained_businesses-per-ta-type")
    public ResponseEntity<List<TaTypeTrainedBusinessDto>> getTaTypeTrainedBusinesses(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                             @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                             @RequestParam(value = "training-partner", required = false) String trainingPartner,
                                                                                                             @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                             @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .trainingPartner(trainingPartner)
                .build();
        return new ResponseEntity<>(this.dashboardService.getTaTypeTrainedBusinesses(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loans-accessed-vs-out-standing-per-partner")
    public ResponseEntity<List<SeriesDataPointDto>> getLoansAccessedVsOutStandingByPartnerSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                  @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                  @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                  @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoansAccessedVsOutStandingByPartnerSummary(searchCriteria), HttpStatus.OK);
    }

    @GetMapping("loans-accessed-vs-out-standing-per-gender")
    public ResponseEntity<List<SeriesDataPointDto>> getLoansAccessedVsOutStandingByGenderSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                                                 @RequestParam(value = "county-code", required = false) String countyCode,
                                                                                                  @RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                                                  @RequestParam(value = "to-date", required = false) LocalDate toDate){
        final var searchCriteria = DashboardSearchCriteria.builder()
                .fromDate(fromDate)
                .toDate(toDate)
                .partnerId(partnerId)
                .countyCode(countyCode)
                .build();
        return new ResponseEntity<>(this.dashboardService.getLoansAccessedVsOutStandingByGenderSummary(searchCriteria), HttpStatus.OK);
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
        return new ResponseEntity<>(this.dashboardService.getOperationalCounties(), HttpStatus.OK);
    }

    @GetMapping("performance-summary")
    public ResponseEntity<List<PerformanceSummaryDto>> getPerformanceSummary(@RequestParam(value = "partner-id", required = false) Long partnerId,
                                                                             @RequestParam(value = "year", required = false) String year){
        return new ResponseEntity<>(this.dashboardService.getPerformanceSummary(year, partnerId), HttpStatus.OK);
    }

    @GetMapping("system-user-login-summary")
    public ResponseEntity<List<DataPointDto>> getSystemUserLoginSummary(@RequestParam(value = "from-date", required = false) LocalDate fromDate,
                                                                         @RequestParam(value = "to-date", required = false) LocalDate toDate){
        return new ResponseEntity<>(this.dashboardService.getSystemUserLoginSummary(fromDate, toDate), HttpStatus.OK);
    }
}
