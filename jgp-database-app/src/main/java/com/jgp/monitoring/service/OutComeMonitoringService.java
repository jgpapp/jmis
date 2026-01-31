package com.jgp.monitoring.service;

import com.jgp.monitoring.domain.OutComeMonitoring;
import com.jgp.monitoring.domain.predicate.OutComeMonitoringSearchCriteria;
import com.jgp.monitoring.dto.OutComeMonitoringResponseDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface OutComeMonitoringService {
    /**
     * Creates or updates an OutComeMonitoring record.
     *
     * @param outComeMonitoring the OutComeMonitoring record to create or update
     */
    void createOutComeMonitoring(OutComeMonitoring outComeMonitoring);
    /**
     * Finds an OutComeMonitoring record by its ID.
     *
     * @param id the ID of the OutComeMonitoring record to find
     * @return the OutComeMonitoring record with the specified ID, or null if not found
     */
    OutComeMonitoringResponseDto findOneById(Long id);
    /**
     * Approves or rejects a list of OutComeMonitoring records based on their IDs.
     *
     * @param dataIds the list of IDs of the OutComeMonitoring records to approve or reject
     * @param approval true to approve, false to reject
     */
    void approvedOutComeMonitoringData(List<Long> dataIds, Boolean approval);

    /**
     * Deletes a list of OutComeMonitoring records based on their IDs.
     *
     * @param dataIds the list of IDs of the OutComeMonitoring records to delete
     */
    void deleteOutComeMonitoringDataByIds(List<Long> dataIds);

    /**
     * Retrieves a paginated list of OutComeMonitoring records based on search criteria.
     *
     * @param searchCriteria the criteria to filter the OutComeMonitoring records
     * @param pageable pagination information
     * @return a paginated list of OutComeMonitoring records matching the search criteria
     */
    Page<OutComeMonitoringResponseDto> getOutComeMonitoringDataRecords(OutComeMonitoringSearchCriteria searchCriteria, Pageable pageable);

    /**
     * Retrieves OutComeMonitoring records associated with a specific document ID.
     *
     * @param documentId the ID of the document whose associated OutComeMonitoring records should be retrieved
     */
    List<OutComeMonitoring> findByDocumentId(Long documentId);
}

