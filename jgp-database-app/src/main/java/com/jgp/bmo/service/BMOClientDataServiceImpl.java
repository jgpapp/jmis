package com.jgp.bmo.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.bmo.domain.BMOParticipantData;
import com.jgp.bmo.domain.BMOClientDataRepository;
import com.jgp.bmo.domain.predicate.BMOPredicateBuilder;
import com.jgp.bmo.dto.BMOClientDto;
import com.jgp.bmo.dto.BMOParticipantSearchCriteria;
import com.jgp.bmo.mapper.BMOClientMapper;
import com.jgp.infrastructure.bulkimport.event.DataApprovedEvent;
import com.jgp.util.CommonUtil;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

@Service
@RequiredArgsConstructor
@Slf4j
public class BMOClientDataServiceImpl implements BMOClientDataService {

    private final BMOClientDataRepository bmoDataRepository;
    private final BMOClientMapper bmoClientMapper;
    private final BMOPredicateBuilder bmoPredicateBuilder;
    private final PlatformSecurityContext platformSecurityContext;
    private final ApplicationContext applicationContext;

    @Override
    public void createBMOData(List<BMOParticipantData> bmoDataListRequest) {
        this.bmoDataRepository.saveAll(bmoDataListRequest);
    }

    @Transactional
    @Override
    public void approvedBMOParticipantsData(List<Long> dataIds, Boolean approval) {
        var bmoData = this.bmoDataRepository.findAllById(dataIds);
        var currentUser = this.platformSecurityContext.getAuthenticatedUserIfPresent();
        var currentUserPartner = Objects.nonNull(currentUser) ? currentUser.getPartner() : null;
        if (dataIds.isEmpty() && Objects.nonNull(currentUserPartner)) {
                bmoData = this.bmoDataRepository.findAll(this.bmoPredicateBuilder.buildPredicateForSearchTAData(new BMOParticipantSearchCriteria(currentUserPartner.getId(), null, false)), Pageable.unpaged()).getContent();
            }

        int count = 0;
        var bmoToSave = new ArrayList<BMOParticipantData>();
        Set<LocalDate> dataDates = new HashSet<>();
        for(BMOParticipantData bmo : bmoData) {
            bmo.approveData(approval);
            var participant = bmo.getParticipant();
            if (Boolean.TRUE.equals(approval) && Boolean.FALSE.equals(participant.getIsActive())){
                participant.activateParticipant();
            }
            bmoToSave.add(bmo);
            count++;
            if (count % 20 == 0) { // Flush and clear the session every 20 entities
                this.bmoDataRepository.saveAllAndFlush(bmoToSave);
                count = 0;
                bmoToSave = new ArrayList<>();
            }
            dataDates.add(bmo.getDateRecordedByPartner());
        }
        this.bmoDataRepository.saveAllAndFlush(bmoToSave);
        this.applicationContext.publishEvent(new DataApprovedEvent(currentUserPartner.getId(), dataDates));
    }

    @Override
    public List<BMOClientDto> getBMODataRecords(BMOParticipantSearchCriteria searchCriteria, Pageable pageable) {
        return this.bmoClientMapper.toDto(this.bmoDataRepository.findAll(this.bmoPredicateBuilder.buildPredicateForSearchTAData(searchCriteria), pageable).stream().toList());
    }

    @Override
    public BMOClientDto findBMODataById(Long bmoId) {
        return this.bmoDataRepository.findById(bmoId).map(this.bmoClientMapper::toDto).orElseThrow(() -> new RuntimeException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));
    }
}
