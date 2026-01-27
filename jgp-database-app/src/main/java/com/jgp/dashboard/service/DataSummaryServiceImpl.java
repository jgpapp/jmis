package com.jgp.dashboard.service;

import com.jgp.dashboard.domain.DataSummaryRepository;
import com.jgp.dashboard.dto.DataSummaryDto;
import com.jgp.infrastructure.core.domain.JdbcSupport;
import com.jgp.patner.domain.PartnerRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
public class DataSummaryServiceImpl implements DataSummaryService {


    private final NamedParameterJdbcTemplate namedParameterJdbcTemplate;
    private final DataSummaryRepository countySummaryRepository;
    private final PartnerRepository partnerRepository;
    private static final String PARTNER_ID_PARAM = "partnerId";
    private static final String FROM_DATE_PARAM = "fromDate";
    private static final String TO_DATE_PARAM = "toDate";
    private static final String LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM = "WHERE l.date_disbursed between :fromDate and :toDate  and l.data_is_approved = true and lt.is_deleted = false and l.is_deleted = false ";
    private static final String BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM = "WHERE bpd.date_partner_recorded between :fromDate and :toDate and bpd.data_is_approved = true and bpd.is_deleted = false ";
    private static final String LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM = "%s and l.partner_id = :partnerId ";
    private static final String BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM = "%s and bpd.partner_id = :partnerId ";

    @PersistenceContext
    private EntityManager entityManager;

    @Transactional
    @Override
    public void updateDataSummary(LocalDate startDate, LocalDate endDate, Long partnerId) {
        log.info("Started Updating summary between  {} and {}", startDate, endDate);

        var partner = this.partnerRepository.findById(partnerId).filter(t -> Boolean.FALSE.equals(t.getIsDeleted())).orElse(null);
        if (Objects.isNull(partner)) {
            log.error("Partner with ID {} not found.", partnerId);
            return;
        }

        // Iterate while the current month is before or equal to the end date's month
        while (!startDate.isAfter(endDate)) {
            // Get the start date of the month
            LocalDate monthStartDate = startDate;
            // Get the last day of the month
            LocalDate monthEndDate = startDate.with(TemporalAdjusters.lastDayOfMonth());

            final var dataIncludingMissingDates = fillMissingDatesWithDefaultValues(
                    getDataSummaryForDateRangeAndPartner(partnerId, monthStartDate, monthEndDate),
                    monthStartDate,
                    monthEndDate
            );
            for (var dto: dataIncludingMissingDates){
                this.countySummaryRepository.upsertDataSummary(partnerId, dto.genderCategory(), dto.summaryDate(),
                        dto.businessesTrained(), dto.businessesLoaned(), dto.amountDisbursed(), dto.outStandingAmount(), dto.amountRepaid());
            }

            // Move to the next month
            startDate = startDate.plusMonths(1);
        }
        log.info("Finished Updating summary between  {} and {}", startDate, endDate);
    }

    /**
     * Fetches data summary for a given date range and partner.
     *
     * @param partnerId the ID of the partner (can be null)
     * @param fromDate  the start date of the range
     * @param toDate    the end date of the range
     * @return a list of DataSummaryDto objects containing the summary data
     */
    private List<DataSummaryDto> getDataSummaryForDateRangeAndPartner(Long partnerId, LocalDate fromDate, LocalDate toDate) {

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
        return this.namedParameterJdbcTemplate.query(sqlQuery, parameters, new DataSummaryDataMapper());
    }

    /**
     * Fills in missing dates in the data summaries with default values.
     *
     * @param dataSummaries the list of existing data summaries
     * @param fromDate      the start date of the range
     * @param toDate        the end date of the range
     * @return a complete list of DataSummaryDto objects with missing dates filled in
     */
    private List<DataSummaryDto> fillMissingDatesWithDefaultValues(List<DataSummaryDto> dataSummaries, LocalDate fromDate, LocalDate toDate) {
        // Group existing summaries by date for O(1) lookup
        var summariesByDate = dataSummaries.stream()
                .collect(Collectors.groupingBy(DataSummaryDto::summaryDate));

        var completeDataSummaries = new ArrayList<DataSummaryDto>();
        var datePointer = fromDate;

        while (!datePointer.isAfter(toDate)) {
            var summariesForDate = summariesByDate.get(datePointer);
            if (summariesForDate == null || summariesForDate.isEmpty()) {
                completeDataSummaries.add(
                        new DataSummaryDto("na", 0, 0, java.math.BigDecimal.ZERO,
                                java.math.BigDecimal.ZERO, java.math.BigDecimal.ZERO, datePointer)
                );
            } else {
                completeDataSummaries.addAll(summariesForDate);
            }
            datePointer = datePointer.plusDays(1);
        }
        return completeDataSummaries;
    }

    private static final class DataSummaryDataMapper implements ResultSetExtractor<List<DataSummaryDto>> {

        public static final String COUNTY_SUMMARY_SCHEMA = """
                with highLevelSummary as (
                                     select p.gender_category as genderCategory, bpd.date_partner_recorded as summaryDate, count(distinct p.id) as businessesTrained,
                                     0 as businessesLoaned, 0 as amountDisbursed,
                                     0 as outStandingAmount, 0 as amountRepaid from ta_participants_data bpd\s
                                     inner join participants p on p.id = bpd.participant_id %s
                                     group by 1, 2
                                     union
                                     select p.gender_category as genderCategory, lt.transaction_date as summaryDate, 0 as businessesTrained, count(l.id) as businessesLoaned,
                                     sum(lt.amount) as amountDisbursed, sum(lt.out_standing_amount) as outStandingAmount,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       \s
                                     sum(l.loan_amount_repaid) as amountRepaid from loan_transactions lt\s
                                     inner join loans l on lt.loan_id = l.id\s
                                     inner join participants p on p.id = l.participant_id %s\s
                                     group by 1, 2
                                     )
                                     select genderCategory, summaryDate, sum(businessesTrained) as businessesTrained, sum(businessesLoaned) as businessesLoaned,
                                     sum(amountDisbursed) as amountDisbursed, sum(outStandingAmount) as outStandingAmount, sum(amountRepaid) as amountRepaid
                                     from highLevelSummary group by 1, 2;
               \s""";


        @Override
        public List<DataSummaryDto> extractData(ResultSet rs) throws SQLException, DataAccessException {
            var dataPoints = new ArrayList<DataSummaryDto>();
            while (rs.next()){
                final var summaryDate = JdbcSupport.getLocalDate(rs, "summaryDate");
                final var genderCategory = rs.getString("genderCategory");
                final var businessesTrained = rs.getInt("businessesTrained");
                final var businessesLoaned = rs.getInt("businessesLoaned");
                final var amountDisbursed = rs.getBigDecimal("amountDisbursed");
                final var outStandingAmount = rs.getBigDecimal("outStandingAmount");
                final var amountRepaid = rs.getBigDecimal("amountRepaid");

                dataPoints.add(
                        new DataSummaryDto(genderCategory, businessesTrained, businessesLoaned, amountDisbursed,
                                outStandingAmount, amountRepaid, summaryDate)
                );
            }
            return dataPoints;
        }
    }

}
