package com.jgp.monitoring.service;

import com.jgp.monitoring.domain.OutComeMonitoring;
import java.util.List;

public interface OutComeMonitoringService {
    void createOutComeMonitoring(OutComeMonitoring outComeMonitoring);
    OutComeMonitoring findOneById(Long id);
    List<OutComeMonitoring> findAllByPartner(String partner);
    void deleteByIds(List<Long> ids);
}

