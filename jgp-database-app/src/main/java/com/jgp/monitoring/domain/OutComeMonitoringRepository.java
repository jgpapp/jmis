package com.jgp.monitoring.domain;

import org.jspecify.annotations.NonNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface OutComeMonitoringRepository extends JpaRepository<OutComeMonitoring, Long>, JpaSpecificationExecutor<OutComeMonitoring>, QuerydslPredicateExecutor<OutComeMonitoring> {

    @Transactional
    @Modifying
    @Query(value = "update outcome_monitoring mon set is_deleted = true where mon.id in ?1", nativeQuery = true)
    void deleteOutComeMonitoringByIds(@NonNull List<Long> monitoringIds);

    List<OutComeMonitoring> findByIsDeletedFalse();

    List<OutComeMonitoring> findByDocumentId(@NonNull Long documentId);


}
