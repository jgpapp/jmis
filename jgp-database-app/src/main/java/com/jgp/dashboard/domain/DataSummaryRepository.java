package com.jgp.dashboard.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;


@Repository
public interface DataSummaryRepository extends JpaRepository<DataSummary, Long>, JpaSpecificationExecutor<DataSummary>, QuerydslPredicateExecutor<DataSummary> {

    @Transactional
    @Modifying
    @Query(value = "DELETE FROM data_summary WHERE  partner_id = :partnerId AND data_year = :dataYear AND data_month = :dataMonth", nativeQuery = true)
    void deleteDataSummary(@Param("partnerId") Long partnerId, @Param("dataYear") Integer dataYear, @Param("dataMonth") Integer dataMonth);

}
