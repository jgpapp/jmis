package com.jgp.monitoring.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OutComeMonitoringRepository extends JpaRepository<OutComeMonitoring, Long> {
    List<OutComeMonitoring> findByPartner(String partner);
}
