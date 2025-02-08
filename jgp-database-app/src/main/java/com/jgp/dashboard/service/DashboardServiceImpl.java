package com.jgp.dashboard.service;

import com.jgp.dashboard.dto.CountySummaryDto;
import com.jgp.dashboard.dto.DashboardSearchCriteria;
import com.jgp.dashboard.dto.DataPointDto;
import com.jgp.dashboard.dto.HighLevelSummaryDto;
import com.jgp.dashboard.dto.PartnerYearlyDataDto;
import com.jgp.util.CommonUtil;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.jgp.dashboard.dto.SeriesDataPointDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class DashboardServiceImpl implements DashboardService {

    private final NamedParameterJdbcTemplate namedParameterJdbcTemplate;
    private static final String INTEGER_DATA_POINT_TYPE = "INTEGER";
    private static final String DECIMAL_DATA_POINT_TYPE = "DECIMAL";
    private static final String PARTNER_ID_PARAM = "partnerId";
    private static final String FROM_DATE_PARAM = "fromDate";
    private static final String TO_DATE_PARAM = "toDate";
    private static final String COUNTY_CODE_PARAM = "countyCode";
    private static final String DATA_VALUE_PARAM = "dataValue";
    private static final String DATA_PERCENTAGE_VALUE_PARAM = "percentage";
    private static final String LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM = "WHERE l.date_disbursed between :fromDate and :toDate  and l.data_is_approved = true ";
    private static final String BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM = "WHERE bpd.date_partner_recorded between :fromDate and :toDate and bpd.data_is_approved = true ";
    private static final String LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM = "%s and l.partner_id = :partnerId ";
    private static final String BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM = "%s and bpd.partner_id = :partnerId ";
    private static final String WHERE_CLAUSE_BY_COUNTY_CODE_PARAM = "and cl.location_county_code = :countyCode";

    @Value("${jgp.dashboard.default.view.period.in.months}")
    private Integer jgpDashboardDefaultViewPeriodInMonths;

    @Override
    public HighLevelSummaryDto getHighLevelSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var highLevelSummaryMapper = new HighLevelSummaryMapper();
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var bpdWhereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);

        if (Objects.nonNull(dashboardSearchCriteria.partnerId())) {
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            bpdWhereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, bpdWhereClause);
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            bpdWhereClause = String.format("%s%s", bpdWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(HighLevelSummaryMapper.SCHEMA, bpdWhereClause, loanWhereClause);
        return this.namedParameterJdbcTemplate.queryForObject(sqlQuery, parameters, highLevelSummaryMapper);
    }

    @Override
    public List<DataPointDto> getLoanDisbursedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.LOANS_DISBURSED_BY_GENDER_SCHEMA, loanWhereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getLoanDisbursedByIndustrySectorSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.LOANS_DISBURSED_BY_SECTOR_SCHEMA, loanWhereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getLoanDisbursedByIndustrySegmentSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.LOANS_DISBURSED_BY_SEGMENT_SCHEMA, loanWhereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getLoanDisbursedTopFourPartnersSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.LOANS_DISBURSED_TOP_FOUR_PARTNERS_SCHEMA, loanWhereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getLoanDisbursedTopFourCountiesSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        var sqlQuery = String.format(DataPointMapper.LOANS_DISBURSED_TOP_FOUR_LOCATIONS_SCHEMA, loanWhereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getBusinessTrainedTopFourCountiesSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var whereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_TOP_FOUR_LOCATIONS_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getBusinessOwnersTrainedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var whereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_BY_GENDER_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getLoanDisbursedByPipelineSourceSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.LOANS_DISBURSED_BY_PIPELINE_SCHEMA, loanWhereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getLoansDisbursedByQualitySummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.LOANS_DISBURSED_BY_QUALITY_SCHEMA, loanWhereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<SeriesDataPointDto> getTaNeedsByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final SeriesDataPointMapper rm = new SeriesDataPointMapper();
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var whereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(SeriesDataPointMapper.TA_NEEDS_BY_GENDER_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getTaTrainingBySectorSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var whereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_BY_SECTOR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getTaTrainingBySegmentSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final DataPointMapper rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var whereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_BY_SEGMENT_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<SeriesDataPointDto> getTrainingByPartnerByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final SeriesDataPointMapper rm = new SeriesDataPointMapper();
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var whereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(SeriesDataPointMapper.TRAINING_BY_PARTNER_BY_GENDER_SCHEMA, whereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<SeriesDataPointDto> getLastThreeYearsAccessedLoanPerPartnerSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final SeriesDataPointMapper rm = new SeriesDataPointMapper();
        MapSqlParameterSource parameters = new MapSqlParameterSource();
        var whereClause = "";
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s  and l.data_is_approved = true", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        whereClause = String.format("%s  and l.data_is_approved = true", whereClause);
        var sqlQuery = String.format(SeriesDataPointMapper.ACCESSED_AMOUNT_BY_PARTNER_BY_YEAR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<PartnerYearlyDataDto> getLastThreeYearsAccessedLoanAmountPerPartnerYearly(DashboardSearchCriteria dashboardSearchCriteria) {
        final PartnerYearlyDataMapper rm = new PartnerYearlyDataMapper(DECIMAL_DATA_POINT_TYPE);
        MapSqlParameterSource parameters = new MapSqlParameterSource();
        var whereClause = "";
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s  and l.data_is_approved = true", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        whereClause = String.format("%s  and l.data_is_approved = true", whereClause);
        var sqlQuery = String.format(PartnerYearlyDataMapper.ACCESSED_AMOUNT_BY_PARTNER_BY_YEAR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<PartnerYearlyDataDto> getLastThreeYearsAccessedLoansCountPerPartnerYearly(DashboardSearchCriteria dashboardSearchCriteria) {
        final PartnerYearlyDataMapper rm = new PartnerYearlyDataMapper(INTEGER_DATA_POINT_TYPE);
        MapSqlParameterSource parameters = new MapSqlParameterSource();
        var whereClause = "";
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        whereClause = String.format("%s  and l.data_is_approved = true", whereClause);
        var sqlQuery = String.format(PartnerYearlyDataMapper.ACCESSED_LOAN_COUNT_BY_PARTNER_BY_YEAR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<PartnerYearlyDataDto> getLastThreeYearsTrainedBusinessesPerPartnerYearly(DashboardSearchCriteria dashboardSearchCriteria) {
        final PartnerYearlyDataMapper rm = new PartnerYearlyDataMapper(INTEGER_DATA_POINT_TYPE);
        MapSqlParameterSource parameters = new MapSqlParameterSource();
        var whereClause = "";
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s and bpd.data_is_approved = true", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        whereClause = String.format("%s and bpd.data_is_approved = true", whereClause);
        var sqlQuery = String.format(PartnerYearlyDataMapper.BUSINESSES_TRAINED_COUNT_BY_PARTNER_BY_YEAR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<SeriesDataPointDto> getLoansAccessedVsOutStandingByPartnerSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final SeriesDataPointMapper rm = new SeriesDataPointMapper();
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(SeriesDataPointMapper.LOAN_AMOUNT_ACCESSED_VS_OUTSTANDING_PER_PARTNER_BY_YEAR_SCHEMA, loanWhereClause, loanWhereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<SeriesDataPointDto> getLoansAccessedVsOutStandingByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final SeriesDataPointMapper rm = new SeriesDataPointMapper();
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(SeriesDataPointMapper.LOAN_AMOUNT_ACCESSED_VS_OUTSTANDING_PER_GENDER_SCHEMA, loanWhereClause, loanWhereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<CountySummaryDto> getCountySummary(LocalDate fromDate, LocalDate toDate, Long partnerId) {
        final var countySummaryMapper = new CountySummaryDataMapper();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var bpdWhereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);

        if (Objects.nonNull(partnerId)) {
            parameters.addValue(PARTNER_ID_PARAM, partnerId);
            bpdWhereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, bpdWhereClause);
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
        }
        var sqlQuery = String.format(CountySummaryDataMapper.COUNTY_SUMMARY_SCHEMA, bpdWhereClause, loanWhereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, countySummaryMapper);
    }

    @Override
    public Map<String, CountySummaryDto> getCountySummaryMap(LocalDate fromDate, LocalDate toDate, Long partnerId) {
        return getCountySummary(fromDate, toDate, partnerId).stream().collect(Collectors.toMap(CountySummaryDto::countyCode, s -> s));
    }

    private static final class HighLevelSummaryMapper implements RowMapper<HighLevelSummaryDto> {

        public static final String SCHEMA = """
                    with highLevelSummary as (\s
                    select count(*) as businessesTrained,\s
                    0 as businessesLoaned, 0 as amountDisbursed,\s
                    0 as outStandingAmount from bmo_participants_data bpd\s
                    inner join participants cl on bpd.participant_id = cl.id %s \s
                    union
                    select 0 as businessesTrained, count(*) as businessesLoaned,\s
                    sum(loan_amount_accessed) as amountDisbursed, sum(loan_outstanding_amount) as outStandingAmount from loans l\s
                    inner join participants cl on l.participant_id = cl.id %s\s
                    )
                    select sum(businessesTrained) as businessesTrained, sum(businessesLoaned) as businessesLoaned,\s
                    sum(amountDisbursed) as amountDisbursed, sum(outStandingAmount) as outStandingAmount
                    from highLevelSummary;
                   \s""";

        @Override
        public HighLevelSummaryDto mapRow(ResultSet rs, int rowNum) throws SQLException {
            final var businessesTrained = rs.getInt("businessesTrained");
            final var businessesLoaned = rs.getInt("businessesLoaned");
            final var amountDisbursed = rs.getBigDecimal("amountDisbursed");
            final var outStandingAmount = rs.getBigDecimal("outStandingAmount");
            return new HighLevelSummaryDto(businessesTrained, businessesLoaned, amountDisbursed, outStandingAmount);
        }
    }

    private static final class DataPointMapper implements ResultSetExtractor<List<DataPointDto>> {

        public static final String LOANS_DISBURSED_BY_GENDER_SCHEMA = """
                select cl.gender_category as dataKey, sum(l.loan_amount_accessed) as dataValue,\s
                SUM(l.loan_amount_accessed) * 100.0 / SUM(SUM(l.loan_amount_accessed)) OVER () AS percentage\s
                from loans l left join participants cl on l.participant_id = cl.id %s  group by 1;\s
                """;

        public static final String LOANS_DISBURSED_BY_SECTOR_SCHEMA = """
                select cl.industry_sector as dataKey, sum(l.loan_amount_accessed) as dataValue,\s
                SUM(l.loan_amount_accessed) * 100.0 / SUM(SUM(l.loan_amount_accessed)) OVER () AS percentage\s
                from loans l left join participants cl on l.participant_id = cl.id %s  group by 1;\s
                """;

        public static final String LOANS_DISBURSED_BY_SEGMENT_SCHEMA = """
                select cl.business_segment as dataKey, sum(l.loan_amount_accessed) as dataValue,\s
                SUM(l.loan_amount_accessed) * 100.0 / SUM(SUM(l.loan_amount_accessed)) OVER () AS percentage\s
                from loans l left join participants cl on l.participant_id = cl.id %s  group by 1;\s
                """;

        public static final String LOANS_DISBURSED_TOP_FOUR_PARTNERS_SCHEMA = """
                select p.partner_name as dataKey, sum(l.loan_amount_accessed) as dataValue,
                sum(l.loan_amount_accessed) * 100.0 / sum(sum(l.loan_amount_accessed)) OVER () AS percentage
                from loans l inner join partners p on l.partner_id = p.id\s
                inner join participants cl on l.participant_id = cl.id %s group by 1 order by 2 DESC limit 4;
               \s""";

        public static final String LOANS_DISBURSED_TOP_FOUR_LOCATIONS_SCHEMA = """
                select p.business_location as dataKey, sum(l.loan_amount_accessed) as dataValue,
                sum(l.loan_amount_accessed) * 100.0 / sum(sum(l.loan_amount_accessed)) OVER () AS percentage
                from loans l inner join participants p on l.participant_id = p.id %s group by 1 order by 2 DESC limit 4;
                """;

        public static final String BUSINESSES_TRAINED_TOP_FOUR_LOCATIONS_SCHEMA = """
                select p.business_location as dataKey, count(p.id) as dataValue,
                count(p.id) * 100.0 / sum(count(p.id)) OVER () AS percentage
                from bmo_participants_data bpd inner join participants p on bpd.participant_id = p.id %s group by 1 order by 2 DESC limit 4;
                """;

        public static final String BUSINESSES_TRAINED_BY_GENDER_SCHEMA = """
                select cl.gender_category as dataKey, count(cl.id) as dataValue,\s
                count(cl.id) * 100.0 / count(count(cl.id)) OVER () AS percentage\s
                from participants cl inner join bmo_participants_data bpd on bpd.participant_id = cl.id\s
                inner join partners p on p.id = bpd.partner_id %s  group by 1;\s
                """;

        public static final String LOANS_DISBURSED_BY_PIPELINE_SCHEMA = """
                select l.pipeline_source as dataKey, sum(l.loan_amount_accessed) as dataValue,\s
                SUM(l.loan_amount_accessed) * 100.0 / SUM(SUM(l.loan_amount_accessed)) OVER () AS percentage\s
                from loans l inner join participants cl on l.participant_id = cl.id %s group by 1;\s
                """;

        public static final String LOANS_DISBURSED_BY_QUALITY_SCHEMA = """
                select l.loan_quality as dataKey, sum(l.loan_amount_accessed) as dataValue,\s
                SUM(l.loan_amount_accessed) * 100.0 / SUM(SUM(l.loan_amount_accessed)) OVER () AS percentage\s
                from loans l inner join participants cl on l.participant_id = cl.id %s group by 1;\s
                """;

        public static final String BUSINESSES_TRAINED_BY_SECTOR_SCHEMA = """
                select cl.industry_sector as dataKey, count(cl.id) as dataValue,\s
                count(cl.id) * 100.0 / count(count(cl.id)) OVER () AS percentage\s
                from participants cl inner join bmo_participants_data bpd on bpd.participant_id = cl.id\s
                inner join partners p on p.id = bpd.partner_id %s group by 1;\s
                """;

        public static final String BUSINESSES_TRAINED_BY_SEGMENT_SCHEMA = """
                select cl.business_segment as dataKey, count(cl.id) as dataValue,\s
                count(cl.id) * 100.0 / count(count(cl.id)) OVER () AS percentage\s
                from participants cl inner join bmo_participants_data bpd on bpd.participant_id = cl.id\s
                inner join partners p on p.id = bpd.partner_id %s group by 1;\s
                """;

        private final String valueDataType;

        public DataPointMapper(String valueDataType) {
            this.valueDataType = valueDataType;
        }


        @Override
        public List<DataPointDto> extractData(ResultSet rs) throws SQLException, DataAccessException {
            var dataPoints = new ArrayList<DataPointDto>();
            while (rs.next()){
                final var dataKey = rs.getString("dataKey");
                final var nullableDataKey = CommonUtil.defaultToOtherIfStringIsNull(dataKey);
                if (DashboardServiceImpl.INTEGER_DATA_POINT_TYPE.equals(this.valueDataType)){
                    dataPoints.add(new DataPointDto(StringUtils.capitalize(nullableDataKey), String.valueOf(rs.getInt(DATA_VALUE_PARAM)), String.valueOf(rs.getBigDecimal(DATA_PERCENTAGE_VALUE_PARAM))));
                } else if (DECIMAL_DATA_POINT_TYPE.equals(this.valueDataType)) {
                    dataPoints.add(new DataPointDto(StringUtils.capitalize(nullableDataKey), String.valueOf(rs.getBigDecimal(DATA_VALUE_PARAM)), String.valueOf(rs.getBigDecimal(DATA_PERCENTAGE_VALUE_PARAM))));
                }else {
                    dataPoints.add(new DataPointDto(StringUtils.capitalize(nullableDataKey), rs.getString(DATA_VALUE_PARAM), String.valueOf(rs.getBigDecimal(DATA_PERCENTAGE_VALUE_PARAM))));
                }
            }
            return dataPoints;
        }
    }

private static final class SeriesDataPointMapper implements ResultSetExtractor<List<SeriesDataPointDto>> {

    public static final String TA_NEEDS_BY_GENDER_SCHEMA = """
                SELECT unnest(string_to_array(bpd.ta_needs, ',')) AS name, cl.gender_category as seriesName, COUNT(*) AS value\s
                FROM participants cl inner join bmo_participants_data bpd on bpd.participant_id = cl.id %s group by 1, 2;\s
               \s""";

    public static final String TRAINING_BY_PARTNER_BY_GENDER_SCHEMA = """
                SELECT p.partner_name AS name, cl.gender_category as seriesName, COUNT(*) AS value
                FROM participants cl inner join bmo_participants_data bpd on bpd.participant_id = cl.id\s
                inner join partners p on p.id  = bpd.partner_id %s group by 1, 2;\s
               \s""";

    public static final String ACCESSED_AMOUNT_BY_PARTNER_BY_YEAR_SCHEMA = """
             SELECT p.partner_name as name,\s
             EXTRACT(YEAR FROM l.date_disbursed) AS seriesName,\s
             SUM(l.loan_amount_accessed) AS value\s
             FROM loans l inner join partners p on p.id = l.partner_id\s
             inner join participants cl on l.participant_id = cl.id\s
             WHERE EXTRACT(YEAR FROM l.date_disbursed) >= EXTRACT(YEAR FROM current_date) - 2 %s\s
             GROUP BY 1, 2\s
             ORDER BY 2 ASC;
           \s""";


    public static final String LOAN_AMOUNT_ACCESSED_VS_OUTSTANDING_PER_PARTNER_BY_YEAR_SCHEMA = """
             SELECT p.partner_name AS name,\s
             'ACCESSED' as seriesName, SUM(l.loan_amount_accessed) AS value
              FROM loans l\s
              inner join partners p on p.id  = l.partner_id \s
              inner join participants cl on l.participant_id = cl.id %s
              group by 1, 2
              union\s
              SELECT p.partner_name AS name,\s
             'OUT-STANDING' as seriesName, SUM(l.loan_outstanding_amount) AS value
              FROM loans l\s
              inner join partners p on p.id  = l.partner_id\s
              inner join participants cl on l.participant_id = cl.id %s
              group by 1, 2;
           \s""";

    public static final String LOAN_AMOUNT_ACCESSED_VS_OUTSTANDING_PER_GENDER_SCHEMA = """
             SELECT cl.owner_gender AS name,
              'ACCESSED' as seriesName, SUM(l.loan_amount_accessed) AS value
               FROM loans l
               inner join participants cl on cl.id  = l.participant_id %s\s
               group by 1, 2
               union
               SELECT cl.owner_gender AS name,
              'OUT-STANDING' as seriesName, SUM(l.loan_outstanding_amount) AS value
               FROM loans l
               inner join participants cl on cl.id  = l.participant_id %s\s
               group by 1, 2;
           \s""";

    @Override
    public List<SeriesDataPointDto> extractData(ResultSet rs) throws SQLException, DataAccessException {
        var dataPoints = new ArrayList<SeriesDataPointDto>();
        var dataPointsMap = new HashMap<String, Map<String, Integer>>();
        while (rs.next()){
            final var taName = rs.getString("name");
            final var seriesName = rs.getString("seriesName");
            final var nullableTaName = CommonUtil.defaultToOtherIfStringIsNull(taName);
            final var nullableSeriesName = CommonUtil.defaultToOtherIfStringIsNull(seriesName);

            final var value = rs.getInt("value");

            if (dataPointsMap.containsKey(nullableTaName)){
                var genderMap = dataPointsMap.get(nullableTaName);
                if (genderMap.containsKey(nullableSeriesName)){
                    genderMap.put(nullableSeriesName, value + genderMap.get(nullableSeriesName));
                }else {
                    genderMap.put(nullableSeriesName, value);
                }
            }else {
                var genderMap = new HashMap<String, Integer>();
                genderMap.put(nullableSeriesName, value);
                dataPointsMap.put(nullableTaName, genderMap);
            }
        }
        for (Map.Entry<String, Map<String, Integer>> entry: dataPointsMap.entrySet()){
            var series = new HashSet<DataPointDto>();
            for (Map.Entry<String, Integer> seriesEntry: entry.getValue().entrySet()){
                series.add(new DataPointDto(StringUtils.capitalize(CommonUtil.defaultToOtherIfStringIsNull(seriesEntry.getKey())), seriesEntry.getValue().toString(), ""));
            }
            dataPoints.add(new SeriesDataPointDto(StringUtils.capitalize(CommonUtil.defaultToOtherIfStringIsNull(entry.getKey())), series));
        }
        return dataPoints;
    }
}


    private static final class PartnerYearlyDataMapper implements ResultSetExtractor<List<PartnerYearlyDataDto>> {

        public static final String ACCESSED_AMOUNT_BY_PARTNER_BY_YEAR_SCHEMA = """
             SELECT p.partner_name as partnerName,\s
             EXTRACT(YEAR FROM l.date_disbursed) AS year,\s
             cl.gender_category as genderName,\s
             SUM(l.loan_amount_accessed) as value\s
             FROM loans l inner join partners p on p.id = l.partner_id\s
             inner join participants cl on l.participant_id = cl.id\s
             WHERE EXTRACT(YEAR FROM l.date_disbursed) >= EXTRACT(YEAR FROM current_date) - 2 %s\s
             GROUP BY 1, 2, 3\s
             ORDER BY 2 ASC;
           \s""";

        public static final String ACCESSED_LOAN_COUNT_BY_PARTNER_BY_YEAR_SCHEMA = """
             SELECT p.partner_name as partnerName,\s
             EXTRACT(YEAR FROM l.date_disbursed) AS year,\s
             cl.gender_category as genderName,\s
             COUNT(*) AS value\s
             FROM loans l inner join partners p on p.id = l.partner_id\s
             inner join participants cl on l.participant_id = cl.id\s
             WHERE EXTRACT(YEAR FROM l.date_disbursed) >= EXTRACT(YEAR FROM current_date) - 2 %s\s
             GROUP BY 1, 2, 3\s
             ORDER BY 2 ASC;
           \s""";

        public static final String BUSINESSES_TRAINED_COUNT_BY_PARTNER_BY_YEAR_SCHEMA = """
             SELECT p.partner_name as partnerName,\s
             EXTRACT(YEAR FROM bpd.date_partner_recorded) AS year,\s
             cl.gender_category as genderName,\s
             COUNT(*) AS value\s
             FROM bmo_participants_data bpd inner join partners p on p.id = bpd.partner_id\s
             inner join participants cl on bpd.participant_id = cl.id\s
             WHERE EXTRACT(YEAR FROM bpd.date_partner_recorded) >= EXTRACT(YEAR FROM current_date) - 2 %s\s
             GROUP BY 1, 2, 3\s
             ORDER BY 2 ASC;
           \s""";

        private final String valueDataType;

        public PartnerYearlyDataMapper(String valueDataType) {
            this.valueDataType = valueDataType;
        }


        @Override
        public List<PartnerYearlyDataDto> extractData(ResultSet rs) throws SQLException, DataAccessException {
            var dataPoints = new ArrayList<PartnerYearlyDataDto>();
            while (rs.next()){
                final var partnerName = rs.getString("partnerName");
                final var genderName = rs.getString("genderName");
                final var year = rs.getInt("year");
                final var value = INTEGER_DATA_POINT_TYPE.equals(valueDataType) ? rs.getInt("value") : rs.getBigDecimal("value");
                dataPoints.add(new PartnerYearlyDataDto(StringUtils.capitalize(CommonUtil.defaultToOtherIfStringIsNull(partnerName)), StringUtils.capitalize(CommonUtil.defaultToOtherIfStringIsNull(genderName)), year, CommonUtil.NUMBER_FORMAT.format(value)));

            }
            return dataPoints;
        }
    }

    private static final class CountySummaryDataMapper implements ResultSetExtractor<List<CountySummaryDto>> {

        public static final String COUNTY_SUMMARY_SCHEMA = """
                with highLevelSummary as (
                                     select p.location_county_code as county, count(*) as businessesTrained,
                                     0 as businessesLoaned, 0 as amountDisbursed,
                                     0 as outStandingAmount from bmo_participants_data bpd\s
                                     inner join participants p on p.id = bpd.participant_id %s
                                     group by 1
                                     union
                                     select p.location_county_code as county, 0 as businessesTrained, count(*) as businessesLoaned,
                                     sum(loan_amount_accessed) as amountDisbursed, sum(loan_outstanding_amount) as outStandingAmount from loans l\s
                                     inner join participants p on p.id = l.participant_id %s\s
                                     group by 1
                                     )
                                     select county, sum(businessesTrained) as businessesTrained, sum(businessesLoaned) as businessesLoaned,
                                     sum(amountDisbursed) as amountDisbursed, sum(outStandingAmount) as outStandingAmount
                                     from highLevelSummary group by 1;
               \s""";


        @Override
        public List<CountySummaryDto> extractData(ResultSet rs) throws SQLException, DataAccessException {
            var dataPoints = new ArrayList<CountySummaryDto>();
            while (rs.next()){
                final var countyCode = rs.getString("county");
                final var businessesTrained = rs.getInt("businessesTrained");
                final var businessesLoaned = rs.getInt("businessesLoaned");
                final var amountDisbursed = rs.getBigDecimal("amountDisbursed");
                final var outStandingAmount = rs.getBigDecimal("outStandingAmount");

                final var county = CommonUtil.KenyanCounty.getKenyanCountyFromCode(countyCode);
                final var kenyaCounty = county.orElse(CommonUtil.KenyanCounty.UNKNOWN);
                dataPoints.add(new CountySummaryDto(CommonUtil.defaultToOtherIfStringIsNull(countyCode), kenyaCounty.getCountyName(), businessesTrained, businessesLoaned, amountDisbursed, outStandingAmount, 2025, 2));
            }
            return dataPoints;
        }
    }


private Pair<LocalDate, LocalDate> getDefaultQueryDates(){
        final var dateToday = LocalDate.now();
        return new ImmutablePair<>(LocalDate.now(ZoneId.systemDefault()).minusMonths(jgpDashboardDefaultViewPeriodInMonths), dateToday);
}

}
