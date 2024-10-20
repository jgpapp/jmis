package com.jgp.dashboard.service;

import com.jgp.dashboard.dto.CountySummaryDto;
import com.jgp.dashboard.dto.DataPointDto;
import com.jgp.dashboard.dto.HighLevelSummaryDto;
import com.jgp.dashboard.dto.SeriesDataPointDto;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

public interface DashboardService {

    HighLevelSummaryDto getHighLevelSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getLoanDisbursedByGenderSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getLoanDisbursedByIndustrySectorSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getLoanDisbursedByIndustrySegmentSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getLoanDisbursedTopFourSummary(LocalDate fromDate, LocalDate toDate);

    List<DataPointDto> getLoanDisbursedTopFourCountiesSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getBusinessTrainedTopFourCountiesSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getBusinessOwnersTrainedByGenderSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getLoanDisbursedByPipelineSourceSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getLoansDisbursedByQualitySummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<SeriesDataPointDto> getTaNeedsByGenderSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getTaTrainingBySectorSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<DataPointDto> getTaTrainingBySegmentSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<SeriesDataPointDto> getTrainingByPartnerByGenderSummary(LocalDate fromDate, LocalDate toDate);

    List<SeriesDataPointDto> getLastThreeYearsAccessedLoanPerPartnerSummary(Long partnerId);

    List<SeriesDataPointDto> getLoansAccessedVsOutStandingByPartnerSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<SeriesDataPointDto> getLoansAccessedVsOutStandingByGenderSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<CountySummaryDto> getCountySummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    Map<String, CountySummaryDto> getCountySummaryMap(LocalDate fromDate, LocalDate toDate, Long partnerId);
}
