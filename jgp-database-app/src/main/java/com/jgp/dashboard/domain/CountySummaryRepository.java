package com.jgp.dashboard.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDate;

@Repository
public interface CountySummaryRepository extends JpaRepository<CountySummary, Long>, JpaSpecificationExecutor<CountySummary>, QuerydslPredicateExecutor<CountySummary> {

    @Modifying
    @Query(value = "INSERT INTO county_summary (partner_id, county_code, data_date, businesses_trained, businesses_loaned, amount_disbursed, out_standing_amount) " +
            "VALUES (:partnerId, :countyCode, :dataDate, :businessesTrained, :businessesLoaned, :amountDisbursed, :outStandingAmount) " +
            "ON CONFLICT (partner_id, county_code, data_date) " +
            "DO UPDATE SET businesses_trained = :businessesTrained, businesses_loaned = :businessesLoaned, amount_disbursed = :amountDisbursed, out_standing_amount = :outStandingAmount", nativeQuery = true)
    void insertOrUpdateCountySummary(@Param("partnerId") Long partnerId,
                                     @Param("countyCode") String countyCode,
                                     @Param("dataDate") LocalDate dataDate,
                                     @Param("businessesTrained") Integer businessesTrained,
                                     @Param("businessesLoaned") Integer businessesLoaned,
                                     @Param("amountDisbursed") BigDecimal amountDisbursed,
                                     @Param("outStandingAmount") Integer outStandingAmount);
}
