package com.jgp.monitoring.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface OutComeMonitoringRepository extends JpaRepository<OutComeMonitoring, Long>, JpaSpecificationExecutor<OutComeMonitoring>, QuerydslPredicateExecutor<OutComeMonitoring> {

    @Transactional
    @Modifying
    @Query(value = "delete from outcome_monitoring mon where mon.id in ?1", nativeQuery = true)
    void deleteOutComeMonitoringsByIds(@NonNull List<Long> monitoringIds);
}
