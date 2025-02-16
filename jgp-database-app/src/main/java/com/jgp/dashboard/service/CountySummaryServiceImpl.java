package com.jgp.dashboard.service;

import com.jgp.dashboard.domain.CountySummaryRepository;
import com.jgp.dashboard.dto.CountySummaryDto;
import com.jgp.util.CommonUtil;
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
import java.time.Month;
import java.time.ZoneId;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
@Slf4j
@RequiredArgsConstructor
public class CountySummaryServiceImpl implements CountySummaryService {


    private final NamedParameterJdbcTemplate namedParameterJdbcTemplate;
    private final CountySummaryRepository countySummaryRepository;
    private static final String PARTNER_ID_PARAM = "partnerId";
    private static final String FROM_DATE_PARAM = "fromDate";
    private static final String TO_DATE_PARAM = "toDate";
    private static final String LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM = "WHERE l.date_disbursed between :fromDate and :toDate  and l.data_is_approved = true ";
    private static final String BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM = "WHERE bpd.date_partner_recorded between :fromDate and :toDate and bpd.data_is_approved = true ";
    private static final String LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM = "%s and l.partner_id = :partnerId ";
    private static final String BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM = "%s and bpd.partner_id = :partnerId ";

    @PersistenceContext
    private EntityManager entityManager;

    @Transactional
    @Override
    public void updateCountySummary(LocalDate fromDate, LocalDate toDate, Long partnerId) {
        LocalDate startDate = LocalDate.of(fromDate.getYear(), Month.JANUARY, 1);
        LocalDate endDate = LocalDate.of(toDate.getYear(), Month.DECEMBER, 31);
        LocalDate currentMonth = startDate.withDayOfMonth(1); // Set the currentMonth to the first day of the start month

        var countySummaries = new ArrayList<CountySummaryDto>();
        // Iterate while the current month is before or equal to the end date's month
        while (!currentMonth.isAfter(endDate)) {
            // Get the start date of the month
            LocalDate monthStartDate = currentMonth;
            // Get the last day of the month
            LocalDate monthEndDate = currentMonth.with(TemporalAdjusters.lastDayOfMonth());

            final var summaryMapper = new CountySummaryDataMapper(monthStartDate.getYear(), monthStartDate.getMonthValue());
            var bpdWhereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
            var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
            MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, monthStartDate);
            parameters.addValue(TO_DATE_PARAM, monthEndDate);

            if (Objects.nonNull(partnerId)) {
                parameters.addValue(PARTNER_ID_PARAM, partnerId);
                bpdWhereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, bpdWhereClause);
                loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
            }
            var sqlQuery = String.format(CountySummaryDataMapper.COUNTY_SUMMARY_SCHEMA, bpdWhereClause, loanWhereClause);
            countySummaries.addAll(this.namedParameterJdbcTemplate.query(sqlQuery, parameters, summaryMapper));

            // Move to the next month
            currentMonth = currentMonth.plusMonths(1);
        }

        for (CountySummaryDto dto: countySummaries){
            this.countySummaryRepository.insertOrUpdateCountySummary(partnerId, dto.countyCode(), dto.dataYear(), dto.dataMonth(),
                    dto.businessesTrained(), dto.businessesLoaned(), dto.amountDisbursed(), dto.outStandingAmount(), LocalDate.now(ZoneId.systemDefault()));
        }
    }

    @Override
    public List<CountySummaryDto> getCountySummary(LocalDate fromDate, LocalDate toDate, Long partnerId) {
            return List.of();
    }


    private static final class CountySummaryDataMapper implements ResultSetExtractor<List<CountySummaryDto>> {

        private final Integer dataYear;
        private final Integer dataMonth;

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

        private CountySummaryDataMapper(Integer dataYear, Integer dataMonth) {
            this.dataYear = dataYear;
            this.dataMonth = dataMonth;
        }


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
                dataPoints.add(new CountySummaryDto(CommonUtil.defaultToOtherIfStringIsNull(countyCode), kenyaCounty.getCountyName(), businessesTrained, businessesLoaned, amountDisbursed, outStandingAmount, this.dataYear, this.dataMonth));
            }
            return dataPoints;
        }
    }

}
