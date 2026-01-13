package com.jgp.bmo.api;

import com.jgp.bmo.dto.TAResponseDto;
import com.jgp.bmo.dto.TAParticipantSearchCriteria;
import com.jgp.bmo.service.TADataService;
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
@RequestMapping("api/v1/bmos")
public class BMOClientController {

    private final TADataService bmoDataService;

    @GetMapping
    public ResponseEntity<Page<TAResponseDto>> getAvailableBMODataRecords(@RequestParam(name = "partnerId", required = false) Long partnerId,
                                                                          @RequestParam(name = "participantId", required = false) Long participantId,
                                                                          @RequestParam(name = "approvedByPartner", required = false) Boolean approvedByPartner,
                                                                          @RequestParam(name = "pageNumber", defaultValue = "0") Integer pageNumber,
                                                                          @RequestParam(name = "pageSize", defaultValue = "10") Integer pageSize){
        final var sortedByDateCreated =
                PageRequest.of(pageNumber, pageSize, Sort.by("dateCreated").descending());
        return new ResponseEntity<>(this.bmoDataService.getBMODataRecords(new TAParticipantSearchCriteria(partnerId, participantId, approvedByPartner), sortedByDateCreated), HttpStatus.OK);
    }

    @GetMapping("{bmoId}")
    public ResponseEntity<TAResponseDto> getBMOParticipantData(@PathVariable("bmoId") Long bmoId){
        return new ResponseEntity<>(this.bmoDataService.findBMODataById(bmoId), HttpStatus.OK);
    }

    @PostMapping("approve-or-reject")
    public ResponseEntity<ApiResponseDto> approvedBMOParticipantsData(@RequestBody List<Long> loanIds, @RequestParam(name = "approved") Boolean approved) {
        this.bmoDataService.approvedBMOParticipantsData(loanIds, approved);
        return new ResponseEntity<>(new ApiResponseDto(true, CommonUtil.RESOURCE_UPDATED), HttpStatus.OK);
    }

}
