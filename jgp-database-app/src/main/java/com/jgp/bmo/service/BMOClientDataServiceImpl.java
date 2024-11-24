package com.jgp.bmo.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.bmo.domain.BMOParticipantData;
import com.jgp.bmo.domain.BMOClientDataRepository;
import com.jgp.bmo.domain.predicate.BMOPredicateBuilder;
import com.jgp.bmo.dto.BMOClientDto;
import com.jgp.bmo.dto.BMOParticipantSearchCriteria;
import com.jgp.bmo.mapper.BMOClientMapper;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.participant.domain.ParticipantRepository;
import com.jgp.util.CommonUtil;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class BMOClientDataServiceImpl implements BMOClientDataService {

    private final BMOClientDataRepository bmoDataRepository;
    private final ApplicationEventPublisher publisher;
    private final BMOClientMapper bmoClientMapper;
    private final BMOPredicateBuilder bmoPredicateBuilder;
    private final ParticipantRepository participantRepository;
    private final PlatformSecurityContext platformSecurityContext;

    @Override
    public void createBMOData(List<BMOParticipantData> bmoDataListRequest) {
        this.bmoDataRepository.saveAll(bmoDataListRequest);
    }

    @Transactional
    @Override
    public void approvedBMOParticipantsData(List<Long> dataIds, Boolean approval) {
        var bmoData = this.bmoDataRepository.findAllById(dataIds);
        if (dataIds.isEmpty()) {
            var currentUser = this.platformSecurityContext.getAuthenticatedUserIfPresent();
            var currentUserPartner = Objects.nonNull(currentUser) ? currentUser.getPartner() : null;
            if(Objects.nonNull(currentUserPartner)) {
                bmoData = this.bmoDataRepository.findAll(this.bmoPredicateBuilder.buildPredicateForSearchTAData(new BMOParticipantSearchCriteria(currentUserPartner.getId(), null, false)), Pageable.unpaged()).getContent();
            }
        }
        bmoData.forEach(bmo -> {
            bmo.approveData(approval);
            var participant = bmo.getParticipant();
            if (Boolean.TRUE.equals(approval) && Boolean.FALSE.equals(participant.getIsActive())){
                participant.activateParticipant();
                this.participantRepository.save(participant);
            }
        });
        this.bmoDataRepository.saveAll(bmoData);
    }

    @Override
    public void uploadBMOData(MultipartFile file) {
        try {
            this.publisher.publishEvent(new BulkImportEvent(new XSSFWorkbook(file.getInputStream()),  TemplatePopulateImportConstants.BMO_ENTITY));
        }  catch (Exception e){
            throw new RuntimeException("Error while importing BMO Data: "+ e.getMessage());
        }
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
