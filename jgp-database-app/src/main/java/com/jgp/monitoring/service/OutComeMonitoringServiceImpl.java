package com.jgp.monitoring.service;

import com.jgp.monitoring.domain.OutComeMonitoring;
import com.jgp.monitoring.domain.OutComeMonitoringRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
public class OutComeMonitoringServiceImpl implements OutComeMonitoringService {
    private final OutComeMonitoringRepository outComeMonitoringRepository;

    @Transactional
    @Override
    public void createOutComeMonitoring(OutComeMonitoring outComeMonitoring) {
        outComeMonitoringRepository.save(outComeMonitoring);
    }

    @Override
    public OutComeMonitoring findOneById(Long id) {
        return outComeMonitoringRepository.findById(id).orElse(null);
    }

    @Override
    public List<OutComeMonitoring> findAllByPartner(String partner) {
        return outComeMonitoringRepository.findByPartner(partner);
    }

    @Override
    public void deleteByIds(List<Long> ids) {
        outComeMonitoringRepository.deleteAllById(ids);
    }
}
