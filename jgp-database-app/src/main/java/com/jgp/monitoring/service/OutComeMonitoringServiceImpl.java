package com.jgp.monitoring.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.monitoring.domain.OutComeMonitoring;
import com.jgp.monitoring.domain.OutComeMonitoringRepository;
import com.jgp.monitoring.domain.predicate.OutComeMonitoringPredicateBuilder;
import com.jgp.monitoring.domain.predicate.OutComeMonitoringSearchCriteria;
import com.jgp.monitoring.dto.OutComeMonitoringResponseDto;
import com.jgp.monitoring.mapper.OutComeMonitoringMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class OutComeMonitoringServiceImpl implements OutComeMonitoringService {

    private final OutComeMonitoringRepository outComeMonitoringRepository;
    private final OutComeMonitoringMapper outComeMonitoringMapper;
    private final OutComeMonitoringPredicateBuilder outComeMonitoringPredicateBuilder;
    private final PlatformSecurityContext platformSecurityContext;
    private final ApplicationContext applicationContext;

    @Transactional
    @Override
    public void createOutComeMonitoring(OutComeMonitoring outComeMonitoring) {
        outComeMonitoringRepository.save(outComeMonitoring);
    }

    @Override
    public OutComeMonitoringResponseDto findOneById(Long id) {
        return outComeMonitoringRepository.findById(id).map(outComeMonitoringMapper::toDto).orElse(null);
    }

    @Override
    public void approvedOutComeMonitoringData(List<Long> dataIds, Boolean approval) {
        var outComeMonitoringData = this.outComeMonitoringRepository.findAllById(dataIds);
        if (dataIds.isEmpty()) {
            outComeMonitoringData = this.outComeMonitoringRepository.findAll();
        }
        var currentUser = this.platformSecurityContext.getAuthenticatedUserIfPresent();

        if (Boolean.TRUE.equals(approval)) {
            int count = 0;
            var mentorshipsToSave = new ArrayList<OutComeMonitoring>();
            for (OutComeMonitoring outComeMonitoring : outComeMonitoringData) {
                outComeMonitoring.approveData(true, currentUser);
                mentorshipsToSave.add(outComeMonitoring);
                count++;
                if (count % 20 == 0) { // Flush and clear the session every 20 entities
                    this.outComeMonitoringRepository.saveAllAndFlush(mentorshipsToSave);
                    count = 0;
                    mentorshipsToSave = new ArrayList<>();
                }
            }
            this.outComeMonitoringRepository.saveAllAndFlush(mentorshipsToSave);
        }else {
            rejectAndDeleteBMOParticipantsData(outComeMonitoringData);
        }
    }

    private void rejectAndDeleteBMOParticipantsData(List<OutComeMonitoring> outComeMonitoringList){
        final var mentorshipIds = outComeMonitoringList.stream()
                .map(OutComeMonitoring::getId).toList();
        this.outComeMonitoringRepository.deleteOutComeMonitoringsByIds(mentorshipIds);
    }

    @Override
    public Page<OutComeMonitoringResponseDto> getOutComeMonitoringDataRecords(OutComeMonitoringSearchCriteria searchCriteria, Pageable pageable) {
        final var bmoData = this.outComeMonitoringRepository.findAll(this.outComeMonitoringPredicateBuilder.buildPredicateForSearchOutComeMonitorings(searchCriteria), pageable);
        return new PageImpl<>(this.outComeMonitoringMapper.toDto(bmoData.stream().toList()), pageable, bmoData.getTotalElements());
    }

}
