package com.jgp.dashboard.domain;

import com.jgp.dashboard.dto.DataSummaryDto;
import jakarta.persistence.QueryHint;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.jpa.repository.QueryHints;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;


@Repository
public interface DataSummaryRepository extends JpaRepository<DataSummary, Long>, JpaSpecificationExecutor<DataSummary>, QuerydslPredicateExecutor<DataSummary> {

    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @QueryHints(@QueryHint(name = "jakarta.persistence.query.timeout", value = "5000"))
    @Query(value = """
        INSERT INTO data_summary (partner_id, gender_category, summary_date, businesses_trained, 
        businesses_loaned, amount_disbursed, out_standing_amount, amount_repaid, summary_week, 
        summary_month, summary_year, week_number, year_quarter, quarter_number, month_number, year_number) 
        VALUES (:#{#dto.partnerId}, :#{#dto.genderCategory}, :#{#dto.summaryDate}, :#{#dto.businessesTrained}, 
        :#{#dto.businessesLoaned}, :#{#dto.amountDisbursed}, :#{#dto.outStandingAmount}, :#{#dto.amountRepaid}, 
        :#{#dto.summaryWeek}, :#{#dto.summaryMonth}, :#{#dto.summaryYear}, :#{#dto.weekNumber}, 
        :#{#dto.summaryQuarter}, :#{#dto.quarterNumber}, :#{#dto.monthNumber}, :#{#dto.yearNumber})
        ON CONFLICT (partner_id, gender_category, summary_date) 
        DO UPDATE SET 
            businesses_trained = EXCLUDED.businesses_trained, 
            businesses_loaned = EXCLUDED.businesses_loaned, 
            amount_disbursed = EXCLUDED.amount_disbursed, 
            out_standing_amount = EXCLUDED.out_standing_amount, 
            amount_repaid = EXCLUDED.amount_repaid
        WHERE data_summary.businesses_trained <> EXCLUDED.businesses_trained
           OR data_summary.businesses_loaned <> EXCLUDED.businesses_loaned
           OR data_summary.amount_disbursed <> EXCLUDED.amount_disbursed
           OR data_summary.out_standing_amount <> EXCLUDED.out_standing_amount
           OR data_summary.amount_repaid <> EXCLUDED.amount_repaid
        """, nativeQuery = true)
    void upsertDataSummary(@Param("dto") DataSummaryDto dto);

}