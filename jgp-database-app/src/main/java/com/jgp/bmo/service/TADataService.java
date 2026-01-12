package com.jgp.bmo.service;

import com.jgp.bmo.domain.TAData;
import com.jgp.bmo.dto.TAResponseDto;
import com.jgp.bmo.dto.TAParticipantSearchCriteria;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface TADataService {

    void createBMOData(TAData bmoDataListRequest);

    void approvedBMOParticipantsData(List<Long> dataIds, Boolean approval);

    Page<TAResponseDto> getBMODataRecords(TAParticipantSearchCriteria searchCriteria, Pageable pageable);

    TAResponseDto findBMODataById(Long bmoId);

    List<TAData> findByDocumentId(Long documentId);
}
