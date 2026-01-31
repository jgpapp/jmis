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

import java.math.BigDecimal;
import java.time.LocalDate;


@Repository
public interface DataSummaryRepository extends JpaRepository<DataSummary, Long>, JpaSpecificationExecutor<DataSummary>, QuerydslPredicateExecutor<DataSummary> {

    @Transactional
    @Modifying
    @Query(value = "DELETE FROM data_summary WHERE  partner_id = :partnerId AND summary_date BETWEEN :startDate AND :endDate", nativeQuery = true)
    void deleteDataSummary(@Param("partnerId") Long partnerId, @Param("startDate") LocalDate startDate, @Param("endDate") LocalDate endDate);

    @Transactional
    @Modifying
    @Query(value = """
            INSERT INTO data_summary (partner_id, gender_category, summary_date, businesses_trained, businesses_loaned, amount_disbursed, 
            out_standing_amount, amount_repaid, summary_week, summary_month, summary_year, week_number, year_quarter, quarter_number, month_number, year_number) 
            VALUES (:partnerId, :genderCategory, :summaryDate, :businessesTrained, :businessesLoaned, :amountDisbursed, :outStandingAmount, :amountRepaid, 
            :summaryWeek, :summaryMonth, :summaryYear, :weekNumber, :yearQuarter, :quarterNumber, :monthNumber, :yearNumber)
            ON CONFLICT (partner_id, gender_category, summary_date) 
                        DO UPDATE set businesses_trained = EXCLUDED.businesses_trained, businesses_loaned = EXCLUDED.businesses_loaned, 
                                    amount_disbursed = EXCLUDED.amount_disbursed, out_standing_amount = EXCLUDED.out_standing_amount, 
                                    amount_repaid = EXCLUDED.amount_repaid
            """, nativeQuery = true)
    void upsertDataSummary2(
            @Param("partnerId") Long partnerId, @Param("genderCategory") String genderCategory,
            @Param("businessesTrained") Integer businessesTrained, @Param("businessesLoaned") Integer businessesLoaned,
            @Param("amountDisbursed") BigDecimal amountDisbursed, @Param("outStandingAmount") BigDecimal outStandingAmount,
            @Param("amountRepaid") BigDecimal amountRepaid, @Param("summaryDate") LocalDate summaryDate,
            @Param("summaryWeek") String summaryWeek, @Param("weekNumber") int weekNumber,
            @Param("summaryMonth") String summaryMonth, @Param("monthNumber") int monthNumber,
            @Param("yearQuarter") String yearQuarter, @Param("quarterNumber") int quarterNumber,
            @Param("summaryYear") String summaryYear, @Param("yearNumber") int yearNumber
            );

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