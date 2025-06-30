package com.jgp.dashboard.service;

import com.jgp.dashboard.dto.AnalyticsUpdateRequestDto;
import com.jgp.dashboard.dto.DataSummaryDto;
import com.jgp.dashboard.dto.DashboardSearchCriteria;
import com.jgp.dashboard.dto.DataPointDto;
import com.jgp.dashboard.dto.EmployeesSummaryDto;
import com.jgp.dashboard.dto.HighLevelSummaryDto;
import com.jgp.dashboard.dto.PartnerYearlyDataDto;
import com.jgp.dashboard.dto.PerformanceSummaryDto;
import com.jgp.dashboard.dto.TaTypeTrainedBusinessDto;
import com.jgp.infrastructure.bulkimport.event.DataApprovedEvent;
import com.jgp.monitoring.domain.predicate.OutComeMonitoringSearchCriteria;
import com.jgp.patner.domain.Partner;
import com.jgp.patner.domain.PartnerRepository;
import com.jgp.util.CommonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.jgp.dashboard.dto.SeriesDataPointDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
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
    private final ApplicationContext applicationContext;
    private final PartnerRepository partnerRepository;
    private static final String INTEGER_DATA_POINT_TYPE = "INTEGER";
    private static final String DECIMAL_DATA_POINT_TYPE = "DECIMAL";
    private static final String PARTNER_ID_PARAM = "partnerId";
    private static final String FROM_DATE_PARAM = "fromDate";
    private static final String TO_DATE_PARAM = "toDate";
    private static final String COUNTY_CODE_PARAM = "countyCode";
    private static final String TRAINING_PARTNER_PARAM = "trainingPartner";
    private static final String DATA_VALUE_PARAM = "dataValue";
    private static final String DATA_PERCENTAGE_VALUE_PARAM = "percentage";
    private static final String LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM = "WHERE lt.transaction_date between :fromDate and :toDate  and l.data_is_approved = true and lt.is_approved = true ";
    private static final String BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM = "WHERE bpd.date_partner_recorded between :fromDate and :toDate and bpd.data_is_approved = true ";
    private static final String MENTORSHIP_WHERE_CLAUSE_BY_MENTORSHIP_DATE_PARAM = "WHERE m.mentor_ship_date between :fromDate and :toDate and m.is_approved = true ";
    private static final String LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM = "%s and l.partner_id = :partnerId ";
    private static final String BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM = "%s and bpd.partner_id = :partnerId ";
    private static final String MENTORSHIP_WHERE_CLAUSE_BY_PARTNER_ID_PARAM = "%s and m.partner_id = :partnerId ";
    private static final String WHERE_CLAUSE_BY_COUNTY_CODE_PARAM = "and cl.location_county_code = :countyCode ";
    private static final String WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM = "and LOWER(bpd.training_partner) = :trainingPartner";
    private static final String BUSINESSES_TRAINED = "businessesTrained";
    private static final String BUSINESSES_LOANED = "businessesLoaned";
    private static final String AMOUNT_DISBURSED = "amountDisbursed";
    private static final String OUT_STANDING_AMOUNT = "outStandingAmount";
    private static final String GENDER_CATEGORY_PARAM = "genderCategory";
    private static final String PARTNER_NAME_PARAM = "partnerName";
    private static final String VALUE_PARAM = "value";
    private static final String TOTAL_PARAM_PARAM = "%s Totals";

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
        var mentorShipWhereClause = MENTORSHIP_WHERE_CLAUSE_BY_MENTORSHIP_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);

        if (Objects.nonNull(dashboardSearchCriteria.partnerId())) {
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            bpdWhereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, bpdWhereClause);
            loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
            mentorShipWhereClause = String.format(MENTORSHIP_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, mentorShipWhereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            bpdWhereClause = String.format("%s%s", bpdWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
            loanWhereClause = String.format("%s%s", loanWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
            mentorShipWhereClause = String.format("%s%s", mentorShipWhereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            bpdWhereClause = String.format("%s%s", bpdWhereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        var sqlQuery = String.format(HighLevelSummaryMapper.SCHEMA, bpdWhereClause, loanWhereClause, loanWhereClause.concat(" and lt.is_given_in_tranches = true"), mentorShipWhereClause);
        return this.namedParameterJdbcTemplate.queryForObject(sqlQuery, parameters, highLevelSummaryMapper);
    }

    @Override
    public List<DataPointDto> getLoanDisbursedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
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
    public List<DataPointDto> getLoanedBusinessesByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
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
        var sqlQuery = String.format(DataPointMapper.LOANED_BUSINESSES_BY_GENDER_SCHEMA, loanWhereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getLoanDisbursedByIndustrySectorSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
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
        final var rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
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
        final var rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
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
        final var rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
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
        final var rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
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
        final var rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
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
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_BY_GENDER_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getPLWDAndRefugeeBusinessOwnersTrainedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
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
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        whereClause = String.format("%s and (LOWER(person_with_disability) = LOWER('YES') or LOWER(refugee_status) = LOWER('YES'))", whereClause);
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_BY_GENDER_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getDisabledBusinessOwnersTrainedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
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
            whereClause = String.format("%s%s ", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        whereClause = String.format("%s and LOWER(person_with_disability) = LOWER('YES')", whereClause);
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_BY_GENDER_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getRefugeeBusinessOwnersTrainedByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
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
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        whereClause = String.format("%s and LOWER(refugee_status) = LOWER('YES')", whereClause);
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_BY_GENDER_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getLoanDisbursedByPipelineSourceSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
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
        final var rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
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
        final var rm = new SeriesDataPointMapper();
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
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        var sqlQuery = String.format(SeriesDataPointMapper.TA_NEEDS_BY_GENDER_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getTaTrainingBySectorSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
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
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_BY_SECTOR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getLoansDisbursedByLoanProductSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var whereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.LOANS_DISBURSED_BY_LOAN_PRODUCT_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getOutcomeMonitoringSummary(OutComeMonitoringSearchCriteria searchCriteria) {
        final var rm = new DataPointMapper(INTEGER_DATA_POINT_TYPE);
        var fromDate = searchCriteria.fromDate();
        var toDate = searchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var whereClause = "WHERE mon.survey_date between :fromDate and :toDate  and mon.is_approved = true ";
        var parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(searchCriteria.partner())){
            parameters.addValue("partner", searchCriteria.partner());
            whereClause = String.format("%s and mon.partner = :partner ", whereClause);
        }
        if (Objects.nonNull(searchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, searchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, "and mon.county_code = :countyCode ");
        }
        if (Objects.nonNull(searchCriteria.region())) {
            parameters.addValue("region", searchCriteria.region());
            whereClause = String.format("%s%s", whereClause, "and mon.region = :region ");
        }
        if (Objects.nonNull(searchCriteria.participantAgeGroup())) {
            final var ageRange = CommonUtil.getAgeRangeFromAgeGroup(searchCriteria.participantAgeGroup());
            parameters.addValue("participantAgeGroupFrom", ageRange.getLeft());
            parameters.addValue("participantAgeGroupTo", ageRange.getRight());
            whereClause = String.format("%s%s", whereClause, "and mon.age between :participantAgeGroupFrom and :participantAgeGroupTo ");
        }
        if (Objects.nonNull(searchCriteria.gender())) {
            parameters.addValue("gender", searchCriteria.gender());
            whereClause = String.format("%s%s", whereClause, "and mon.gender = :gender ");
        }
        if (Objects.nonNull(searchCriteria.genderCategory())) {
            parameters.addValue(GENDER_CATEGORY_PARAM, searchCriteria.genderCategory());
            whereClause = String.format("%s%s", whereClause, "and mon.genderCategory = :genderCategory ");
        }
        if (Objects.nonNull(searchCriteria.jgpIntervention())) {
            parameters.addValue("jgpIntervention", "%"+searchCriteria.jgpIntervention()+"%");
            whereClause = String.format("%s%s", whereClause, "and mon.jgp_interventions LIKE :jgpIntervention ");
        }

        var sqlQuery = String.format(DataPointMapper.OUT_COME_MONITORING_SCHEMA, searchCriteria.summarizingColumn(), whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<DataPointDto> getParticipantsEmployeesSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var employeeMapper = new EmployeesSummaryMapper();
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
        var sqlQuery = String.format(EmployeesSummaryMapper.EMPLOYEES_SCHEMA, whereClause);
        final var employeeSummary = this.namedParameterJdbcTemplate.queryForObject(sqlQuery, parameters, employeeMapper);
        if (Objects.isNull(employeeSummary)){
            return List.of();
        }
        return List.of(
                new DataPointDto("Regular Above 35", employeeSummary.totalRegularEmployeesAbove35()+"", BigDecimal.ZERO.toString()),
                new DataPointDto("Regular 18-35", employeeSummary.youthRegularEmployees()+"", BigDecimal.ZERO.toString()),
                new DataPointDto("Casual Above 35", employeeSummary.totalCasualEmployeesAbove35()+"", BigDecimal.ZERO.toString()),
                new DataPointDto("Casual 18-35", employeeSummary.youthCasualEmployees()+"", BigDecimal.ZERO.toString())
        );
    }

    @Override
    public List<DataPointDto> getTaTrainingBySegmentSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new DataPointMapper(DECIMAL_DATA_POINT_TYPE);
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
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        var sqlQuery = String.format(DataPointMapper.BUSINESSES_TRAINED_BY_SEGMENT_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<SeriesDataPointDto> getTrainingByPartnerByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new SeriesDataPointMapper();
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
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        var sqlQuery = String.format(SeriesDataPointMapper.TRAINING_BY_PARTNER_BY_GENDER_SCHEMA, whereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<SeriesDataPointDto> getLoanDisbursedByLoanProductByGenderSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new SeriesDataPointMapper();
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        var whereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(SeriesDataPointMapper.DISBURSED_BY_PRODUCT_BY_GENDER_SCHEMA, whereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<SeriesDataPointDto> getLastThreeYearsAccessedLoanPerPartnerSummary(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new SeriesDataPointMapper();
        MapSqlParameterSource parameters = new MapSqlParameterSource();
        var whereClause = "";
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s  and l.data_is_approved = true and lt.is_approved = true", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        whereClause = String.format("%s  and l.data_is_approved = true and lt.is_approved = true", whereClause);
        var sqlQuery = String.format(SeriesDataPointMapper.ACCESSED_AMOUNT_BY_PARTNER_BY_YEAR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<PartnerYearlyDataDto> getLastThreeYearsAccessedLoanAmountPerPartnerYearly(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new PartnerYearlyDataMapper(DECIMAL_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        var whereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(PartnerYearlyDataMapper.ACCESSED_AMOUNT_BY_PARTNER_BY_YEAR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<PartnerYearlyDataDto> getLastThreeYearsAccessedLoansCountPerPartnerYearly(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new PartnerYearlyDataMapper(INTEGER_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        var whereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        var sqlQuery = String.format(PartnerYearlyDataMapper.ACCESSED_LOAN_COUNT_BY_PARTNER_BY_YEAR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<PartnerYearlyDataDto> getLastThreeYearsTrainedBusinessesPerPartnerYearly(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new PartnerYearlyDataMapper(INTEGER_DATA_POINT_TYPE);
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        var whereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        var sqlQuery = String.format(PartnerYearlyDataMapper.BUSINESSES_TRAINED_COUNT_BY_PARTNER_BY_YEAR_SCHEMA, whereClause);

        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, rm);
    }

    @Override
    public List<TaTypeTrainedBusinessDto> getTaTypeTrainedBusinesses(DashboardSearchCriteria dashboardSearchCriteria) {
        final var rm = new TaTypeTrainedBusinessMapper();
        LocalDate fromDate = dashboardSearchCriteria.fromDate();
        LocalDate toDate = dashboardSearchCriteria.toDate();
        if (Objects.isNull(fromDate) || Objects.isNull(toDate)){
            fromDate = getDefaultQueryDates().getLeft();
            toDate = getDefaultQueryDates().getRight();
        }
        MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, fromDate);
        parameters.addValue(TO_DATE_PARAM, toDate);
        var whereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
        if (Objects.nonNull(dashboardSearchCriteria.partnerId())){
            parameters.addValue(PARTNER_ID_PARAM, dashboardSearchCriteria.partnerId());
            whereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, whereClause);
        }
        if (Objects.nonNull(dashboardSearchCriteria.countyCode())) {
            parameters.addValue(COUNTY_CODE_PARAM, dashboardSearchCriteria.countyCode());
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_COUNTY_CODE_PARAM);
        }
        if (Objects.nonNull(dashboardSearchCriteria.trainingPartner())) {
            parameters.addValue(TRAINING_PARTNER_PARAM, dashboardSearchCriteria.trainingPartner().toLowerCase(Locale.getDefault()));
            whereClause = String.format("%s%s", whereClause, WHERE_CLAUSE_BY_TRAINING_PARTNER_PARAM);
        }
        whereClause = String.format("%s  and bpd.ta_type IS NOT NULL", whereClause);
        var sqlQuery = String.format(TaTypeTrainedBusinessMapper.BUSINESSES_TRAINED_COUNT_BY_TA_TYPE_SCHEMA, whereClause);

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
    public List<DataSummaryDto> getDataSummary(LocalDate fromDate, LocalDate toDate, Long partnerId) {
        final var countySummaryMapper = new DataSummaryDataMapper();
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
        var sqlQuery = String.format(DataSummaryDataMapper.COUNTY_SUMMARY_SCHEMA, bpdWhereClause, loanWhereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, countySummaryMapper);
    }

    @Override
    public Map<String, DataSummaryDto> getDataSummaryMap(LocalDate fromDate, LocalDate toDate, Long partnerId) {
        return getDataSummary(fromDate, toDate, partnerId).stream().collect(Collectors.toMap(DataSummaryDto::genderCategory, s -> s));
    }

    @Override
    public List<PerformanceSummaryDto> getPerformanceSummary(String year, Long partnerId) {
        final var performanceSummaryMapper = new PerformanceSummaryMapper();
        final var today = LocalDate.now(ZoneId.systemDefault());
        final var thisYear = Objects.nonNull(year) ? Integer.parseInt(year) : today.getYear();
        var whereClause = Objects.nonNull(year) ? "where ds.data_year = :fromYear " : "where ds.data_year between :fromYear and :toYear ";
        var parameters = new MapSqlParameterSource();
        parameters.addValue("fromYear", (thisYear - 3));
        parameters.addValue("toYear", thisYear);

        if (Objects.nonNull(partnerId)) {
            parameters.addValue(PARTNER_ID_PARAM, partnerId);
            whereClause = "and ds.partner_id = :partnerId ";
        }
        var sqlQuery = String.format(PerformanceSummaryMapper.PERFORMANCE_SUMMARY_SCHEMA, whereClause);
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, performanceSummaryMapper);
    }

    @Override
    public void updateAnalyticsData(AnalyticsUpdateRequestDto analyticsUpdateRequestDto) {
        var partnerIds = Objects.nonNull(analyticsUpdateRequestDto.partnerId()) ? Set.of(analyticsUpdateRequestDto.partnerId()) : this.partnerRepository.findAll().stream().map(Partner::getId).collect(Collectors.toSet());
        final var dataDates = Set.of(analyticsUpdateRequestDto.fromDate(), analyticsUpdateRequestDto.toDate());
        this.applicationContext.publishEvent(new DataApprovedEvent(partnerIds, dataDates));
    }

    private static final class PerformanceSummaryMapper implements ResultSetExtractor<List<PerformanceSummaryDto>> {

        public static final String PERFORMANCE_SUMMARY_SCHEMA = """
                 SELECT p.partner_name as partnerName, ds.data_year as dataYear, ds.data_month as dataMonth,\s
                 ds.gender_category as genderCategory, sum(ds.businesses_trained) as businessesTrained,\s
                 sum(ds.businesses_loaned) as businessesLoaned, sum(ds.amount_disbursed) as amountDisbursed,\s
                 sum(ds.out_standing_amount) as outStandingAmount\s
                 FROM data_summary ds \s
                 inner join partners p on ds.partner_id = p.id\s
                 %s group by 1, 2, 3, 4 order by 2, 3;
                """;

        @Override
        public List<PerformanceSummaryDto> extractData(ResultSet rs) throws SQLException, DataAccessException {
            var dataPoints = new ArrayList<PerformanceSummaryDto>();
            while (rs.next()){
                final var businessesTrained = rs.getInt(BUSINESSES_TRAINED);
                final var businessesLoaned = rs.getInt(BUSINESSES_LOANED);
                final var genderCategory = rs.getString(GENDER_CATEGORY_PARAM);
                final var amountDisbursed = rs.getBigDecimal(AMOUNT_DISBURSED);
                final var outStandingAmount = rs.getBigDecimal(OUT_STANDING_AMOUNT);
                final var year = rs.getInt("dataYear");
                final var month = rs.getInt("dataMonth");
                final var partner = rs.getString(PARTNER_NAME_PARAM);
                final var quarter = getQuarterFromMonth(month);
                dataPoints.add(new PerformanceSummaryDto(year, month, genderCategory, partner, quarter, StringUtils.capitalize(genderCategory)+" Totals", businessesTrained, businessesLoaned, amountDisbursed, outStandingAmount, new ArrayList<>()));
            }
            return groupAndSummarizeByPartner(dataPoints);
        }

        private static String getQuarterFromMonth(Integer month) {
            return switch (month){
                case 1, 2, 3 -> "Qtr 1";
                case 4, 5, 6 -> "Qtr 2";
                case 7, 8, 9 -> "Qtr 3";
                case 10, 11, 12 -> "Qtr 4";
                default -> "";
            };
        }

        public List<PerformanceSummaryDto> groupAndSummarizeByPartner(List<PerformanceSummaryDto> dataSummary) {
            // Group the data by year
            Map<String, List<PerformanceSummaryDto>> groupedByPartner = dataSummary.stream()
                    .collect(Collectors.groupingBy(PerformanceSummaryDto::partner));

            // Process each group
            var perPartnerSummary =  groupedByPartner.entrySet().stream()
                    .map(entry -> {
                        String partner = entry.getKey();
                        List<PerformanceSummaryDto> children = entry.getValue();

                        // Calculate the totals for this year
                        Integer totalBusinessesTrained = children.stream()
                                .mapToInt(PerformanceSummaryDto::businessesTrained)
                                .sum();

                        Integer totalBusinessesLoaned = children.stream()
                                .mapToInt(PerformanceSummaryDto::businessesLoaned)
                                .sum();

                        BigDecimal totalAmountDisbursed = children.stream()
                                .map(PerformanceSummaryDto::amountDisbursed)
                                .reduce(BigDecimal.ZERO, BigDecimal::add);

                        BigDecimal totalOutstandingAmount = children.stream()
                                .map(PerformanceSummaryDto::outStandingAmount)
                                .reduce(BigDecimal.ZERO, BigDecimal::add);

                        // Create a parent PerformanceSummaryDto for this year
                        return new PerformanceSummaryDto(
                                null,
                                null,
                                null,
                                null,
                                partner,
                                String.format(TOTAL_PARAM_PARAM, partner),
                                totalBusinessesTrained,
                                totalBusinessesLoaned,
                                totalAmountDisbursed,
                                totalOutstandingAmount,
                                children
                        );
                    }).toList();

            for (PerformanceSummaryDto dto: perPartnerSummary) {
                var newChildren = groupAndSummarizeByYear(dto.children());
                dto.children().clear();
                dto.children().addAll(newChildren);
            }

            return perPartnerSummary;
        }

        public List<PerformanceSummaryDto> groupAndSummarizeByYear(List<PerformanceSummaryDto> partnerDataSummary) {
            // Group the data by year
            Map<Integer, List<PerformanceSummaryDto>> groupedByYear = partnerDataSummary.stream()
                    .collect(Collectors.groupingBy(PerformanceSummaryDto::year));

            // Process each group
            var yearlySummary =  groupedByYear.entrySet().stream()
                    .map(entry -> {
                        Integer year = entry.getKey();
                        List<PerformanceSummaryDto> children = entry.getValue();

                        // Calculate the totals for this year
                        Integer totalBusinessesTrained = children.stream()
                                .mapToInt(PerformanceSummaryDto::businessesTrained)
                                .sum();

                        Integer totalBusinessesLoaned = children.stream()
                                .mapToInt(PerformanceSummaryDto::businessesLoaned)
                                .sum();

                        BigDecimal totalAmountDisbursed = children.stream()
                                .map(PerformanceSummaryDto::amountDisbursed)
                                .reduce(BigDecimal.ZERO, BigDecimal::add);

                        BigDecimal totalOutstandingAmount = children.stream()
                                .map(PerformanceSummaryDto::outStandingAmount)
                                .reduce(BigDecimal.ZERO, BigDecimal::add);

                        // Create a parent PerformanceSummaryDto for this year
                        return new PerformanceSummaryDto(
                                year,
                                null,
                                null,
                                null,
                                null,
                                String.format("Year %d Totals", year),
                                totalBusinessesTrained,
                                totalBusinessesLoaned,
                                totalAmountDisbursed,
                                totalOutstandingAmount,
                                children
                        );
                    }).toList();

            for (PerformanceSummaryDto dto: yearlySummary) {
                var newChildren = groupAndSummarizeYearDataPerQuarter(dto.children());
                dto.children().clear();
                dto.children().addAll(newChildren);
            }

            return yearlySummary;
        }

        public List<PerformanceSummaryDto> groupAndSummarizeYearDataPerQuarter(List<PerformanceSummaryDto> singleYearlyDataSummary) {
            // Group the data by year
                Map<String, List<PerformanceSummaryDto>> groupedByQuarter = singleYearlyDataSummary.stream()
                        .collect(Collectors.groupingBy(PerformanceSummaryDto::quarter));

            // Process each group
            var quarterlySummary = groupedByQuarter.entrySet().stream()
                    .map(entry -> {
                        String quarter = entry.getKey();
                        List<PerformanceSummaryDto> children = entry.getValue();

                        // Calculate the totals for this year
                        Integer totalBusinessesTrained = children.stream()
                                .mapToInt(PerformanceSummaryDto::businessesTrained)
                                .sum();

                        Integer totalBusinessesLoaned = children.stream()
                                .mapToInt(PerformanceSummaryDto::businessesLoaned)
                                .sum();

                        BigDecimal totalAmountDisbursed = children.stream()
                                .map(PerformanceSummaryDto::amountDisbursed)
                                .reduce(BigDecimal.ZERO, BigDecimal::add);

                        BigDecimal totalOutstandingAmount = children.stream()
                                .map(PerformanceSummaryDto::outStandingAmount)
                                .reduce(BigDecimal.ZERO, BigDecimal::add);

                        // Create a parent PerformanceSummaryDto for this year
                        return new PerformanceSummaryDto(
                                null,
                                null,
                                null,
                                null,
                                quarter,
                                String.format(TOTAL_PARAM_PARAM, quarter),
                                totalBusinessesTrained,
                                totalBusinessesLoaned,
                                totalAmountDisbursed,
                                totalOutstandingAmount,
                                children
                        );
                    }).toList();

            for (PerformanceSummaryDto dto: quarterlySummary) {
                var newChildren = groupAndSummarizeYearDataPerMonth(dto.children());
                dto.children().clear();
                dto.children().addAll(newChildren);
            }

            return quarterlySummary;
        }

        public List<PerformanceSummaryDto> groupAndSummarizeYearDataPerMonth(List<PerformanceSummaryDto> singleQuarterDataSummary) {
            // Group the data by year
            Map<Integer, List<PerformanceSummaryDto>> groupedByQuarter = singleQuarterDataSummary.stream()
                    .collect(Collectors.groupingBy(PerformanceSummaryDto::month));

            // Process each group
            return groupedByQuarter.entrySet().stream()
                    .map(entry -> {
                        final var theMonth = entry.getKey();
                        List<PerformanceSummaryDto> children = entry.getValue();

                        // Calculate the totals for this year
                        Integer totalBusinessesTrained = children.stream()
                                .mapToInt(PerformanceSummaryDto::businessesTrained)
                                .sum();

                        Integer totalBusinessesLoaned = children.stream()
                                .mapToInt(PerformanceSummaryDto::businessesLoaned)
                                .sum();

                        BigDecimal totalAmountDisbursed = children.stream()
                                .map(PerformanceSummaryDto::amountDisbursed)
                                .reduce(BigDecimal.ZERO, BigDecimal::add);

                        BigDecimal totalOutstandingAmount = children.stream()
                                .map(PerformanceSummaryDto::outStandingAmount)
                                .reduce(BigDecimal.ZERO, BigDecimal::add);

                        // Create a parent PerformanceSummaryDto for this year
                        var monthEnum = Month.of(theMonth);
                        var monthName = Objects.nonNull(monthEnum) ? StringUtils.capitalize(monthEnum.name().toLowerCase(Locale.getDefault())) : "";
                        return new PerformanceSummaryDto(
                                null,
                                null,
                                null,
                                null,
                                null,
                                String.format(TOTAL_PARAM_PARAM, monthName),
                                totalBusinessesTrained,
                                totalBusinessesLoaned,
                                totalAmountDisbursed,
                                totalOutstandingAmount,
                                children
                        );
                    }).toList();
        }
    }


    private static final class HighLevelSummaryMapper implements RowMapper<HighLevelSummaryDto> {

        public static final String SCHEMA = """
                with highLevelSummary as (\s
                select count(bpd.*) as businessesTrained,\s
                0 as businessesLoaned, 0 as amountDisbursed,\s
                0 as amountDisbursedByTranches, 0 as businessesMentored from bmo_participants_data bpd inner join participants cl on bpd.participant_id = cl.id %s \s
                union
                select 0 as businessesTrained, count(distinct l.*) as businessesLoaned,\s
                sum(lt.amount) as amountDisbursed, 0 as amountDisbursedByTranches, 0 as businessesMentored from loan_transactions lt\s
                inner join loans l on lt.loan_id = l.id inner join participants cl on l.participant_id = cl.id %s\s
                union
                 select 0 as businessesTrained, 0 as businessesLoaned,\s
                 0 as amountDisbursed, sum(lt.amount) as amountDisbursedByTranches, 0 as businessesMentored from loan_transactions lt\s
                 inner join loans l on lt.loan_id = l.id inner join participants cl on l.participant_id = cl.id %s\s
                 union
                 select 0 as businessesTrained, 0 as businessesLoaned,\s
                 0 as amountDisbursed, 0 as amountDisbursedByTranches, count(distinct m.*) as businessesMentored from mentor_ships m\s
                 inner join participants cl on m.participant_id = cl.id %s\s
                    )
                    select sum(businessesTrained) as businessesTrained, sum(businessesLoaned) as businessesLoaned,\s
                    sum(amountDisbursed) as amountDisbursed, sum(amountDisbursedByTranches) as amountDisbursedByTranches,\s
                    sum(businessesMentored) as businessesMentored
                    from highLevelSummary;
                   \s""";

        @Override
        public HighLevelSummaryDto mapRow(ResultSet rs, int rowNum) throws SQLException {
            final var businessesTrained = rs.getInt(BUSINESSES_TRAINED);
            final var businessesLoaned = rs.getInt(BUSINESSES_LOANED);
            final var amountDisbursed = rs.getBigDecimal(AMOUNT_DISBURSED);
            final var amountDisbursedByTranches = rs.getBigDecimal("amountDisbursedByTranches");
            final var businessesMentored = rs.getBigDecimal("businessesMentored");
            return new HighLevelSummaryDto(CommonUtil.NUMBER_FORMAT.format(businessesTrained), CommonUtil.NUMBER_FORMAT.format(businessesLoaned), amountDisbursed, amountDisbursedByTranches, CommonUtil.NUMBER_FORMAT.format(businessesMentored));
        }
    }


    private static final class EmployeesSummaryMapper implements RowMapper<EmployeesSummaryDto> {

        public static final String EMPLOYEES_SCHEMA = """
                with employees as (\s
                select sum(cl.total_regular_employees) as totalRegularEmployees,\s
                sum(cl.youth_regular_employees ) as youthRegularEmployees,\s
                sum(cl.total_casual_employees ) as totalCasualEmployees,\s
                sum(cl.youth_casual_employees ) as youthCasualEmployees\s
                from participants cl inner join bmo_participants_data bpd on cl.id = bpd.participant_id %s\s
                )
                select (totalRegularEmployees - youthRegularEmployees) as totalRegularEmployeesAbove35,
                youthRegularEmployees,
                (totalCasualEmployees - youthCasualEmployees) as totalCasualEmployeesAbove35,\s
                youthCasualEmployees from employees;
               """;

        @Override
        public EmployeesSummaryDto mapRow(ResultSet rs, int rowNum) throws SQLException {
            final var totalRegularEmployeesAbove35 = rs.getInt("totalRegularEmployeesAbove35");
            final var youthRegularEmployees = rs.getInt("youthRegularEmployees");
            final var totalCasualEmployeesAbove35 = rs.getInt("totalCasualEmployeesAbove35");
            final var youthCasualEmployees = rs.getInt("youthCasualEmployees");
            return new EmployeesSummaryDto(totalRegularEmployeesAbove35, youthRegularEmployees, totalCasualEmployeesAbove35, youthCasualEmployees);
        }
    }

    private static final class DataPointMapper implements ResultSetExtractor<List<DataPointDto>> {

        public static final String LOANS_DISBURSED_BY_GENDER_SCHEMA = """
                select cl.gender_category as dataKey, sum(lt.amount) as dataValue,\s
                SUM(lt.amount) * 100.0 / SUM(SUM(lt.amount)) OVER () AS percentage\s
                from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
                inner join participants cl on l.participant_id = cl.id %s  group by 1;\s
                """;

        public static final String LOANED_BUSINESSES_BY_GENDER_SCHEMA = """
                select cl.gender_category as dataKey, count(distinct l.id) as dataValue,\s
                count(distinct l.id) * 100.0 / count(count(distinct l.id)) OVER () AS percentage\s
                from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
                inner join participants cl on l.participant_id = cl.id %s  group by 1;\s
                """;

        public static final String LOANS_DISBURSED_BY_SECTOR_SCHEMA = """
                select cl.industry_sector as dataKey, sum(lt.amount) as dataValue,\s
                SUM(lt.amount) * 100.0 / SUM(SUM(lt.amount)) OVER () AS percentage\s
                from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
                inner join participants cl on l.participant_id = cl.id %s  group by 1;\s
                """;

        public static final String LOANS_DISBURSED_BY_SEGMENT_SCHEMA = """
                select cl.business_segment as dataKey, sum(lt.amount) as dataValue,\s
                SUM(lt.amount) * 100.0 / SUM(SUM(lt.amount)) OVER () AS percentage\s
                from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
                inner join participants cl on l.participant_id = cl.id %s  group by 1;\s
                """;

        public static final String LOANS_DISBURSED_TOP_FOUR_PARTNERS_SCHEMA = """
                select p.partner_name as dataKey, sum(lt.amount) as dataValue,
                sum(lt.amount) * 100.0 / sum(sum(lt.amount)) OVER () AS percentage\s
                from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
                inner join participants cl on l.participant_id = cl.id\s
                inner join partners p on l.partner_id = p.id %s group by 1 order by 2 DESC limit 4;
               \s""";

        public static final String LOANS_DISBURSED_TOP_FOUR_LOCATIONS_SCHEMA = """
                select cl.business_location as dataKey, sum(lt.amount) as dataValue,
                sum(lt.amount) * 100.0 / sum(sum(lt.amount)) OVER () AS percentage
                from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
                inner join participants cl on l.participant_id = cl.id %s group by 1 order by 2 DESC limit 4;
                """;

        public static final String BUSINESSES_TRAINED_TOP_FOUR_LOCATIONS_SCHEMA = """
                select cl.business_location as dataKey, count(cl.id) as dataValue,
                count(cl.id) * 100.0 / sum(count(cl.id)) OVER () AS percentage
                from bmo_participants_data bpd inner join participants cl on bpd.participant_id = cl.id %s group by 1 order by 2 DESC limit 4;
                """;

        public static final String BUSINESSES_TRAINED_BY_GENDER_SCHEMA = """
                select cl.gender_category as dataKey, count(cl.id) as dataValue,\s
                count(cl.id) * 100.0 / sum(count(cl.id)) OVER () AS percentage\s
                from participants cl inner join bmo_participants_data bpd on bpd.participant_id = cl.id\s
                %s  group by 1;\s
                """;

        public static final String LOANS_DISBURSED_BY_PIPELINE_SCHEMA = """
                select l.pipeline_source as dataKey, sum(lt.amount) as dataValue,\s
                SUM(lt.amount) * 100.0 / SUM(SUM(lt.amount)) OVER () AS percentage\s
                from loan_transactions lt inner join loans l on lt.loan_id = l.id inner join participants cl on l.participant_id = cl.id %s group by 1;\s
                """;

        public static final String LOANS_DISBURSED_BY_QUALITY_SCHEMA = """
                select l.loan_quality as dataKey, sum(lt.amount) as dataValue,\s
                SUM(lt.amount) * 100.0 / SUM(SUM(lt.amount)) OVER () AS percentage\s
                from loan_transactions lt inner join loans l on lt.loan_id = l.id inner join participants cl on l.participant_id = cl.id %s group by 1;\s
                """;

        public static final String LOANS_DISBURSED_BY_LOAN_PRODUCT_SCHEMA = """
                select l.loan_product as dataKey, sum(lt.amount) as dataValue,\s
                SUM(lt.amount) * 100.0 / SUM(SUM(lt.amount)) OVER () AS percentage\s
                from loan_transactions lt inner join loans l on lt.loan_id = l.id inner join participants cl on l.participant_id = cl.id %s group by 1;\s
                """;

        public static final String OUT_COME_MONITORING_SCHEMA = """
                select unnest(string_to_array(mon.%s::TEXT, ',')) as dataKey, count(mon.id) as dataValue,\s
                count(mon.id) * 100.0 / count(count(mon.id)) OVER () AS percentage\s
                from outcome_monitoring mon inner join participants cl on mon.participant_id = cl.id %s group by 1;\s
                """;

        public static final String BUSINESSES_TRAINED_BY_SECTOR_SCHEMA = """
                select cl.industry_sector as dataKey, count(cl.id) as dataValue,\s
                count(cl.id) * 100.0 / sum(count(cl.id)) OVER () AS percentage\s
                from participants cl inner join bmo_participants_data bpd on bpd.participant_id = cl.id\s
                %s group by 1;\s
                """;

        public static final String BUSINESSES_TRAINED_BY_SEGMENT_SCHEMA = """
                select cl.business_segment as dataKey, count(cl.id) as dataValue,\s
                count(cl.id) * 100.0 / sum(count(cl.id)) OVER () AS percentage\s
                from participants cl inner join bmo_participants_data bpd on bpd.participant_id = cl.id\s
                %s group by 1;\s
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
                final var percentageVal = String.valueOf(rs.getBigDecimal(DATA_PERCENTAGE_VALUE_PARAM).setScale(2, RoundingMode.HALF_UP));
                final var nameAndPercentage = String.format("%s (%s", StringUtils.capitalize(nullableDataKey), percentageVal)+ "%)";
                if (DashboardServiceImpl.INTEGER_DATA_POINT_TYPE.equals(this.valueDataType)){
                    dataPoints.add(new DataPointDto(nameAndPercentage, String.valueOf(rs.getInt(DATA_VALUE_PARAM)), percentageVal));
                } else if (DECIMAL_DATA_POINT_TYPE.equals(this.valueDataType)) {
                    dataPoints.add(new DataPointDto(nameAndPercentage, String.valueOf(rs.getBigDecimal(DATA_VALUE_PARAM)), percentageVal));
                }else {
                    dataPoints.add(new DataPointDto(nameAndPercentage, rs.getString(DATA_VALUE_PARAM), percentageVal));
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
                SELECT p.partner_name AS name, cl.gender_category as seriesName, COUNT(cl.*) AS value
                FROM participants cl inner join bmo_participants_data bpd on bpd.participant_id = cl.id\s
                inner join partners p on p.id  = bpd.partner_id %s group by 1, 2;\s
               \s""";

    public static final String DISBURSED_BY_PRODUCT_BY_GENDER_SCHEMA = """
                SELECT l.loan_product AS name, cl.gender_category as seriesName, SUM(lt.amount) AS value
                from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
                inner join participants cl on l.participant_id = cl.id %s group by 1, 2;\s
               \s""";

    public static final String ACCESSED_AMOUNT_BY_PARTNER_BY_YEAR_SCHEMA = """
             SELECT p.partner_name as name,\s
             EXTRACT(YEAR FROM lt.transaction_date) AS seriesName,\s
             SUM(lt.amount) AS value\s
             from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
             inner join partners p on p.id = l.partner_id inner join participants cl on l.participant_id = cl.id\s
             WHERE EXTRACT(YEAR FROM lt.transaction_date) >= EXTRACT(YEAR FROM current_date) - 2 %s\s
             GROUP BY 1, 2\s
             ORDER BY 2 ASC;
           \s""";


    public static final String LOAN_AMOUNT_ACCESSED_VS_OUTSTANDING_PER_PARTNER_BY_YEAR_SCHEMA = """
             SELECT p.partner_name AS name,\s
             'ACCESSED' as seriesName, SUM(lt.amount) AS value
              from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
              inner join partners p on p.id  = l.partner_id inner join participants cl on l.participant_id = cl.id %s\s
              group by 1, 2
              union\s
              SELECT p.partner_name AS name,\s
             'OUT-STANDING' as seriesName, SUM(lt.out_standing_amount) AS value
              from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
              inner join partners p on p.id  = l.partner_id inner join participants cl on l.participant_id = cl.id %s\s
              group by 1, 2;
           \s""";

    public static final String LOAN_AMOUNT_ACCESSED_VS_OUTSTANDING_PER_GENDER_SCHEMA = """
             SELECT cl.owner_gender AS name,
              'ACCESSED' as seriesName, SUM(lt.amount) AS value
               from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
               inner join participants cl on cl.id  = l.participant_id %s\s
               group by 1, 2
               union
               SELECT cl.owner_gender AS name,
              'OUT-STANDING' as seriesName, SUM(lt.out_standing_amount) AS value
               from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
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

            final var value = rs.getInt(VALUE_PARAM);

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
             EXTRACT(YEAR FROM lt.transaction_date) AS year,\s
             cl.gender_category as genderName,\s
             SUM(lt.amount) as value\s
             from loan_transactions lt\s
             inner join loans l on lt.loan_id = l.id\s
             inner join partners p on p.id = l.partner_id\s
             inner join participants cl on l.participant_id = cl.id %s\s
             GROUP BY 1, 2, 3\s
             ORDER BY 2 ASC;
           \s""";

        public static final String ACCESSED_LOAN_COUNT_BY_PARTNER_BY_YEAR_SCHEMA = """
             SELECT p.partner_name as partnerName,\s
             EXTRACT(YEAR FROM lt.transaction_date) AS year,\s
             cl.gender_category as genderName,\s
             COUNT(distinct l.*) AS value\s
             from loan_transactions lt\s
             inner join loans l on lt.loan_id = l.id\s
             inner join partners p on p.id = l.partner_id\s
             inner join participants cl on l.participant_id = cl.id %s\s
             GROUP BY 1, 2, 3\s
             ORDER BY 2 ASC;
           \s""";

        public static final String BUSINESSES_TRAINED_COUNT_BY_PARTNER_BY_YEAR_SCHEMA = """
             SELECT p.partner_name as partnerName,\s
             EXTRACT(YEAR FROM bpd.date_partner_recorded) AS year,\s
             cl.gender_category as genderName,\s
             COUNT(distinct cl.*) AS value\s
             FROM bmo_participants_data bpd inner join partners p on p.id = bpd.partner_id\s
             inner join participants cl on bpd.participant_id = cl.id %s\s
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
                final var partnerName = rs.getString(PARTNER_NAME_PARAM);
                final var genderName = rs.getString("genderName");
                final var year = rs.getInt("year");
                final var value = INTEGER_DATA_POINT_TYPE.equals(valueDataType) ? rs.getInt(VALUE_PARAM) : rs.getBigDecimal(VALUE_PARAM);
                dataPoints.add(new PartnerYearlyDataDto(StringUtils.capitalize(CommonUtil.defaultToOtherIfStringIsNull(partnerName)), StringUtils.capitalize(CommonUtil.defaultToOtherIfStringIsNull(genderName)), year, CommonUtil.NUMBER_FORMAT.format(value)));

            }
            return dataPoints;
        }
    }

    private static final class TaTypeTrainedBusinessMapper implements ResultSetExtractor<List<TaTypeTrainedBusinessDto>> {

        public static final String BUSINESSES_TRAINED_COUNT_BY_TA_TYPE_SCHEMA = """
             select p2.partner_name as partnerName , bpd.ta_type as taType, cl.gender_category as genderCategory, count(cl.id) as businessesTrained\s
             from participants cl inner join bmo_participants_data bpd on cl.id = bpd.participant_id\s
             inner join partners p2 on p2.id = bpd.partner_id %s\s
             GROUP BY 1, 2, 3\s
             ORDER BY 1 ASC, 2, 3;
           \s""";


        @Override
        public List<TaTypeTrainedBusinessDto> extractData(ResultSet rs) throws SQLException, DataAccessException {
            var dataPoints = new ArrayList<TaTypeTrainedBusinessDto>();
            while (rs.next()){
                final var partnerName = rs.getString(PARTNER_NAME_PARAM);
                final var taType = rs.getString("taType");
                final var genderCategory = rs.getString(GENDER_CATEGORY_PARAM);
                final var businessesTrained = rs.getInt(BUSINESSES_TRAINED);
                dataPoints.add(new TaTypeTrainedBusinessDto(StringUtils.capitalize(CommonUtil.defaultToOtherIfStringIsNull(partnerName)), taType, genderCategory, CommonUtil.NUMBER_FORMAT.format(businessesTrained)));
            }
            return dataPoints;
        }
    }

    private static final class DataSummaryDataMapper implements ResultSetExtractor<List<DataSummaryDto>> {

        public static final String COUNTY_SUMMARY_SCHEMA = """
                with highLevelSummary as (
                                     select cl.gender_category as genderCategory, count(bpd.*) as businessesTrained,
                                     0 as businessesLoaned, 0 as amountDisbursed,
                                     0 as outStandingAmount from bmo_participants_data bpd\s
                                     inner join participants cl on cl.id = bpd.participant_id %s
                                     group by 1
                                     union
                                     select cl.gender_category as genderCategory, 0 as businessesTrained, count(distinct l.*) as businessesLoaned,
                                     sum(lt.amount) as amountDisbursed, sum(lt.out_standing_amount) as outStandingAmount\s
                                     from loan_transactions lt inner join loans l on lt.loan_id = l.id\s
                                     inner join participants cl on cl.id = l.participant_id %s\s
                                     group by 1
                                     )
                                     select genderCategory, sum(businessesTrained) as businessesTrained, sum(businessesLoaned) as businessesLoaned,
                                     sum(amountDisbursed) as amountDisbursed, sum(outStandingAmount) as outStandingAmount
                                     from highLevelSummary group by 1;
               \s""";


        @Override
        public List<DataSummaryDto> extractData(ResultSet rs) throws SQLException, DataAccessException {
            var dataPoints = new ArrayList<DataSummaryDto>();
            while (rs.next()){
                final var genderCategory = rs.getString(GENDER_CATEGORY_PARAM);
                final var businessesTrained = rs.getInt(BUSINESSES_TRAINED);
                final var businessesLoaned = rs.getInt(BUSINESSES_LOANED);
                final var amountDisbursed = rs.getBigDecimal(AMOUNT_DISBURSED);
                final var outStandingAmount = rs.getBigDecimal(OUT_STANDING_AMOUNT);

                dataPoints.add(new DataSummaryDto(genderCategory, businessesTrained, businessesLoaned, amountDisbursed, outStandingAmount, 2025, 2));
            }
            return dataPoints;
        }
    }


private Pair<LocalDate, LocalDate> getDefaultQueryDates(){
        final var dateToday = LocalDate.now();
        return new ImmutablePair<>(LocalDate.now(ZoneId.systemDefault()).minusMonths(jgpDashboardDefaultViewPeriodInMonths), dateToday);
}

}
