package com.jgp.dashboard.service;

import com.jgp.dashboard.dto.AnalyticsUpdateRequestDto;
import com.jgp.dashboard.dto.DataSummaryDto;
import com.jgp.dashboard.dto.DashboardSearchCriteria;
import com.jgp.dashboard.dto.DataPointDto;
import com.jgp.dashboard.dto.HighLevelSummaryDto;
import com.jgp.dashboard.dto.PartnerYearlyDataDto;
import com.jgp.dashboard.dto.PerformanceSummaryDto;
import com.jgp.dashboard.dto.SeriesDataPointDto;
import com.jgp.dashboard.dto.TaTypeTrainedBusinessDto;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

public interface DashboardService {

    HighLevelSummaryDto getHighLevelSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getLoanDisbursedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getLoanedBusinessesByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getLoanDisbursedByIndustrySectorSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getLoanDisbursedByIndustrySegmentSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getLoanDisbursedTopFourPartnersSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getLoanDisbursedTopFourCountiesSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getBusinessTrainedTopFourCountiesSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getBusinessOwnersTrainedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getDisabledBusinessOwnersTrainedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getRefugeeBusinessOwnersTrainedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getLoanDisbursedByPipelineSourceSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getLoansDisbursedByQualitySummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<SeriesDataPointDto> getTaNeedsByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getTaTrainingBySectorSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getLoansDisbursedByLoanProductSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getParticipantsEmployeesSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataPointDto> getTaTrainingBySegmentSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<SeriesDataPointDto> getTrainingByPartnerByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<SeriesDataPointDto> getLastThreeYearsAccessedLoanPerPartnerSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<PartnerYearlyDataDto> getLastThreeYearsAccessedLoanAmountPerPartnerYearly(DashboardSearchCriteria dashboardSearchCriteria);

    List<PartnerYearlyDataDto> getLastThreeYearsAccessedLoansCountPerPartnerYearly(DashboardSearchCriteria dashboardSearchCriteria);

    List<PartnerYearlyDataDto> getLastThreeYearsTrainedBusinessesPerPartnerYearly(DashboardSearchCriteria dashboardSearchCriteria);

    List<TaTypeTrainedBusinessDto> getTaTypeTrainedBusinesses(DashboardSearchCriteria dashboardSearchCriteria);

    List<SeriesDataPointDto> getLoansAccessedVsOutStandingByPartnerSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<SeriesDataPointDto> getLoansAccessedVsOutStandingByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria);

    List<DataSummaryDto> getDataSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    Map<String, DataSummaryDto> getDataSummaryMap(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<PerformanceSummaryDto> getPerformanceSummary(String year, Long partnerId);

    void updateAnalyticsData(AnalyticsUpdateRequestDto analyticsUpdateRequestDto);
}
