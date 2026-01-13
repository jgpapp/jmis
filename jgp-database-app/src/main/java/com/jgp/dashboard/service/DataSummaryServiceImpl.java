package com.jgp.dashboard.service;

import com.jgp.dashboard.domain.DataSummary;
import com.jgp.dashboard.domain.DataSummaryRepository;
import com.jgp.dashboard.dto.DataSummaryDto;
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

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

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
    private static final BigDecimal ZERO = BigDecimal.ZERO;

    @PersistenceContext
    private EntityManager entityManager;

    @Transactional
    @Override
    public void updateDataSummary(LocalDate fromDate, LocalDate toDate, Long partnerId) {
        LocalDate startDate = fromDate.with(TemporalAdjusters.firstDayOfMonth());
        LocalDate endDate = toDate.with(TemporalAdjusters.lastDayOfMonth());
        log.info("Updating summary between  {} and {}", startDate, endDate);

        var countySummaries = new ArrayList<DataSummaryDto>();
        // Iterate while the current month is before or equal to the end date's month
        while (!startDate.isAfter(endDate)) {
            // Get the start date of the month
            LocalDate monthStartDate = startDate;
            // Get the last day of the month
            LocalDate monthEndDate = startDate.with(TemporalAdjusters.lastDayOfMonth());

            final var summaryMapper = new DataSummaryDataMapper(monthStartDate.getYear(), monthStartDate.getMonthValue());
            var bpdWhereClause = BMO_WHERE_CLAUSE_BY_PARTNER_RECORDED_DATE_PARAM;
            var loanWhereClause = LOAN_WHERE_CLAUSE_BY_DISBURSED_DATE_PARAM;
            MapSqlParameterSource parameters = new MapSqlParameterSource(FROM_DATE_PARAM, monthStartDate);
            parameters.addValue(TO_DATE_PARAM, monthEndDate);

            if (Objects.nonNull(partnerId)) {
                parameters.addValue(PARTNER_ID_PARAM, partnerId);
                bpdWhereClause = String.format(BMO_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, bpdWhereClause);
                loanWhereClause = String.format(LOAN_WHERE_CLAUSE_BY_PARTNER_ID_PARAM, loanWhereClause);
            }
            var sqlQuery = String.format(DataSummaryDataMapper.COUNTY_SUMMARY_SCHEMA, bpdWhereClause, loanWhereClause);
            countySummaries.addAll(this.namedParameterJdbcTemplate.query(sqlQuery, parameters, summaryMapper));

            // Move to the next month
            startDate = startDate.plusMonths(1);
        }

        List<DataSummary> dataSummaries = new ArrayList<>();
        var partner = this.partnerRepository.findById(partnerId).filter(t -> Boolean.FALSE.equals(t.getIsDeleted())).orElse(null);
        for (DataSummaryDto dto: countySummaries){
            this.countySummaryRepository.deleteDataSummary(partnerId, dto.dataYear(), dto.dataMonth());
            if (Objects.nonNull(partner) && (0 < dto.businessesTrained() || 0 < dto.businessesLoaned() || ZERO.compareTo(dto.amountDisbursed()) < 0 || ZERO.compareTo(dto.outStandingAmount()) < 0)) {
                dataSummaries.add(DataSummary.createDataSummary(dto, partner));
            }
        }
        this.countySummaryRepository.saveAll(dataSummaries);
    }

    private static final class DataSummaryDataMapper implements ResultSetExtractor<List<DataSummaryDto>> {

        private final Integer dataYear;
        private final Integer dataMonth;

        public static final String COUNTY_SUMMARY_SCHEMA = """
                with highLevelSummary as (
                                     select p.gender_category as genderCategory, count(*) as businessesTrained,
                                     0 as businessesLoaned, 0 as amountDisbursed,
                                     0 as outStandingAmount from ta_participants_data bpd\s
                                     inner join participants p on p.id = bpd.participant_id %s
                                     group by 1
                                     union
                                     select p.gender_category as genderCategory, 0 as businessesTrained, count(l.*) as businessesLoaned,
                                     sum(lt.amount) as amountDisbursed, sum(lt.out_standing_amount) as outStandingAmount from loan_transactions lt\s
                                     inner join loans l on lt.loan_id = l.id\s
                                     inner join participants p on p.id = l.participant_id %s\s
                                     group by 1
                                     )
                                     select genderCategory, sum(businessesTrained) as businessesTrained, sum(businessesLoaned) as businessesLoaned,
                                     sum(amountDisbursed) as amountDisbursed, sum(outStandingAmount) as outStandingAmount
                                     from highLevelSummary group by 1;
               \s""";

        private DataSummaryDataMapper(Integer dataYear, Integer dataMonth) {
            this.dataYear = dataYear;
            this.dataMonth = dataMonth;
        }


        @Override
        public List<DataSummaryDto> extractData(ResultSet rs) throws SQLException, DataAccessException {
            var dataPoints = new ArrayList<DataSummaryDto>();
            while (rs.next()){
                final var genderCategory = rs.getString("genderCategory");
                final var businessesTrained = rs.getInt("businessesTrained");
                final var businessesLoaned = rs.getInt("businessesLoaned");
                final var amountDisbursed = rs.getBigDecimal("amountDisbursed");
                final var outStandingAmount = rs.getBigDecimal("outStandingAmount");

                dataPoints.add(new DataSummaryDto(genderCategory, businessesTrained, businessesLoaned, amountDisbursed, outStandingAmount, this.dataYear, this.dataMonth));
            }
            return dataPoints;
        }
    }

}
