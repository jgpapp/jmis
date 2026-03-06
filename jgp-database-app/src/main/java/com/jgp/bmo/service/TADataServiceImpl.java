package com.jgp.bmo.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.bmo.domain.TAData;
import com.jgp.bmo.domain.TADataRepository;
import com.jgp.bmo.domain.predicate.TAPredicateBuilder;
import com.jgp.bmo.dto.TAResponseDto;
import com.jgp.bmo.dto.TAParticipantSearchCriteria;
import com.jgp.bmo.mapper.TAMapper;
import com.jgp.infrastructure.bulkimport.event.DataApprovedEvent;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.domain.ParticipantRepository;
import com.jgp.shared.domain.DataStatus;
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
public class TADataServiceImpl implements TADataService {

    private final TADataRepository bmoDataRepository;
    private final TAMapper taMapper;
    private final TAPredicateBuilder bmoPredicateBuilder;
    private final PlatformSecurityContext platformSecurityContext;
    private final ApplicationContext applicationContext;
    private final ParticipantRepository participantRepository;

    @Transactional
    @Override
    public void createBMOData(TAData bmoDataListRequest) {
        this.bmoDataRepository.save(bmoDataListRequest);
    }

    @Transactional
    @Override
    public void approvedBMOParticipantsData(List<Long> dataIds, Boolean approval) {
        var bmoData = this.bmoDataRepository.findAllById(dataIds);
        var currentUser = this.platformSecurityContext.getAuthenticatedUserIfPresent();
        var currentUserPartner = Objects.nonNull(currentUser) ? currentUser.getPartner() : null;
        if (dataIds.isEmpty() && Objects.nonNull(currentUserPartner)) {
                bmoData = this.bmoDataRepository.findAll(this.bmoPredicateBuilder.buildPredicateForSearchTAData(new TAParticipantSearchCriteria(currentUserPartner.getId(), null, DataStatus.PENDING_APPROVAL.name())), Pageable.unpaged()).getContent();
            }

        if (Objects.nonNull(approval)) {
            int count = 0;
            var bmoToSave = new ArrayList<TAData>();
            Set<LocalDate> dataDates = new HashSet<>();
            for (TAData bmo : bmoData) {
                bmo.approveData(approval, currentUser);
                var participant = bmo.getParticipant();
                if (DataStatus.PENDING_APPROVAL.equals(participant.getDataStatus())) {
                    participant.activateParticipant(approval);
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
            if (Boolean.TRUE.equals(approval) && Objects.nonNull(currentUserPartner)) {
                this.applicationContext.publishEvent(new DataApprovedEvent(Set.of(currentUserPartner.getId()), dataDates));
            }
        }
    }

    @Override
    public void deleteBMOParticipantsDataByIds(List<Long> dataIds) {
        var bmoData = this.bmoDataRepository.findAllById(dataIds);
        deleteBMOParticipantsData(bmoData);
    }

    private void deleteBMOParticipantsData(List<TAData> bmoParticipantDataList){
        int count = 0;
        var bmoToDelete = new ArrayList<TAData>();
        for (TAData bmo : bmoParticipantDataList) {
            bmoToDelete.add(bmo);
            count++;
            if (count % 50 == 0) { // Flush and clear the session every 50 entities
                deleteSelectedRecords(bmoToDelete);
                count = 0;
                bmoToDelete = new ArrayList<>();
            }

        }
        deleteSelectedRecords(bmoToDelete);

    }


    private void deleteSelectedRecords(ArrayList<TAData> bmoToDelete) {
        if (bmoToDelete.isEmpty()) {
            return;
        }
        this.bmoDataRepository.deleteTADataByIds(bmoToDelete.stream().map(TAData::getId).toList());
        final var participantsToDeleteIds = bmoToDelete.stream().map(TAData::getParticipant)
                .map(Participant::getId).toList();

        this.participantRepository.deleteParticipantsByIds(participantsToDeleteIds);
    }

    @Override
    public Page<TAResponseDto> getBMODataRecords(TAParticipantSearchCriteria searchCriteria, Pageable pageable) {
        final var bmoData = this.bmoDataRepository.findAll(this.bmoPredicateBuilder.buildPredicateForSearchTAData(searchCriteria), pageable);
        return new PageImpl<>(this.taMapper.toDto(bmoData.stream().toList()), pageable, bmoData.getTotalElements());
    }

    @Override
    public TAResponseDto findBMODataById(Long bmoId) {
        return this.bmoDataRepository.findById(bmoId).map(this.taMapper::toDto).orElseThrow(() -> new RuntimeException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));
    }

    @Override
    public List<TAData> findByDocumentId(Long documentId) {
        return bmoDataRepository.findByDocumentId(documentId);
    }
}
