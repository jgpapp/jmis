package com.jgp.bmo.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.bmo.domain.BMOParticipantData;
import com.jgp.bmo.domain.BMOClientDataRepository;
import com.jgp.bmo.domain.predicate.BMOPredicateBuilder;
import com.jgp.bmo.dto.BMOClientDto;
import com.jgp.bmo.dto.BMOParticipantSearchCriteria;
import com.jgp.bmo.mapper.BMOClientMapper;
import com.jgp.infrastructure.bulkimport.event.DataApprovedEvent;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.domain.ParticipantRepository;
import com.jgp.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
    private final ParticipantRepository participantRepository;

    @Transactional
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

        if (Boolean.TRUE.equals(approval)) {
            int count = 0;
            var bmoToSave = new ArrayList<BMOParticipantData>();
            Set<LocalDate> dataDates = new HashSet<>();
            for (BMOParticipantData bmo : bmoData) {
                bmo.approveData(true, currentUser);
                var participant = bmo.getParticipant();
                if (Boolean.FALSE.equals(participant.getIsActive())) {
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
            if (Objects.nonNull(currentUserPartner)) {
                this.applicationContext.publishEvent(new DataApprovedEvent(Set.of(currentUserPartner.getId()), dataDates));
            }
        }else {
            rejectAndDeleteBMOParticipantsData(bmoData);
        }
    }

    private void rejectAndDeleteBMOParticipantsData(List<BMOParticipantData> bmoParticipantDataList){
        final var bmoIds = bmoParticipantDataList.stream()
                .map(BMOParticipantData::getId).toList();
        this.bmoDataRepository.deleteTADataByIds(bmoIds);
        final var participantsToDeleteIds = bmoParticipantDataList.stream().map(BMOParticipantData::getParticipant)
                .filter(pt -> Boolean.FALSE.equals(pt.getIsActive()))
                .map(Participant::getId).toList();

        this.participantRepository.deleteParticipantsByIds(participantsToDeleteIds);

    }

    @Override
    public Page<BMOClientDto> getBMODataRecords(BMOParticipantSearchCriteria searchCriteria, Pageable pageable) {
        final var bmoData = this.bmoDataRepository.findAll(this.bmoPredicateBuilder.buildPredicateForSearchTAData(searchCriteria), pageable);
        return new PageImpl<>(this.bmoClientMapper.toDto(bmoData.stream().toList()), pageable, bmoData.getTotalElements());
    }

    @Override
    public BMOClientDto findBMODataById(Long bmoId) {
        return this.bmoDataRepository.findById(bmoId).map(this.bmoClientMapper::toDto).orElseThrow(() -> new RuntimeException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));
    }
}
