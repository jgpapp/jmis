package com.jgp.monitoring.api;

import com.jgp.monitoring.domain.predicate.OutComeMonitoringSearchCriteria;
import com.jgp.monitoring.dto.OutComeMonitoringResponseDto;
import com.jgp.monitoring.service.OutComeMonitoringService;
import com.jgp.shared.dto.ApiResponseDto;
import com.jgp.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/monitoring")
public class OutComeMonitoringController {

    private final OutComeMonitoringService outComeMonitoringService;

    @GetMapping
    public ResponseEntity<Page<OutComeMonitoringResponseDto>> getOutComeMonitoringDataRecords(@RequestParam(name = "dataStatus", required = false) String dataStatus,
                                                                                              @RequestParam(name = "pageNumber", defaultValue = "0") Integer pageNumber,
                                                                                              @RequestParam(name = "pageSize", defaultValue = "10") Integer pageSize){
        final var sortedByDateCreated =
                PageRequest.of(pageNumber, pageSize, Sort.by("dateCreated").descending());
        final var searchCriteria = OutComeMonitoringSearchCriteria.builder()
                .dataStatus(dataStatus)
                .build();
        return new ResponseEntity<>(this.outComeMonitoringService.getOutComeMonitoringDataRecords(searchCriteria, sortedByDateCreated), HttpStatus.OK);
    }

    @GetMapping("{outcomeId}")
    public ResponseEntity<OutComeMonitoringResponseDto> findOneById(@PathVariable("outcomeId") Long outcomeId){
        return new ResponseEntity<>(this.outComeMonitoringService.findOneById(outcomeId), HttpStatus.OK);
    }

    @PostMapping("approve-or-reject")
    public ResponseEntity<ApiResponseDto> approvedMonitoringData(@RequestBody List<Long> monitoringIds, @RequestParam(name = "approved") Boolean approved) {
        this.outComeMonitoringService.approvedOutComeMonitoringData(monitoringIds, approved);
        return new ResponseEntity<>(new ApiResponseDto(true, CommonUtil.RESOURCE_UPDATED), HttpStatus.OK);
    }
}
