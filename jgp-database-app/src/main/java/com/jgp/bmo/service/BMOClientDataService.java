package com.jgp.bmo.service;

import com.jgp.bmo.domain.BMOParticipantData;
import com.jgp.bmo.dto.BMOClientDto;
import com.jgp.bmo.dto.BMOParticipantSearchCriteria;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface BMOClientDataService {

    void createBMOData(List<BMOParticipantData> bmoDataListRequest);

    void approvedBMOParticipantsData(List<Long> dataIds, Boolean approval);

    Page<BMOClientDto> getBMODataRecords(BMOParticipantSearchCriteria searchCriteria, Pageable pageable);

    BMOClientDto findBMODataById(Long bmoId);

    List<BMOParticipantData> findByDocumentId(Long documentId);
}
