package com.jgp.patner.api;

import com.jgp.patner.dto.PartnerDto;
import com.jgp.patner.service.PartnerService;
import com.jgp.shared.dto.ApiResponseDto;
import com.jgp.util.CommonUtil;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/partners")
public class PartnerController {

    private final PartnerService partnerService;

    @GetMapping
    public ResponseEntity<List<PartnerDto>> getAvailablePartners(){
        return new ResponseEntity<>(this.partnerService.getAllPartners(), HttpStatus.OK);
    }

    @PostMapping
    public ResponseEntity<ApiResponseDto> createPartner(@Valid @RequestBody PartnerDto partnerDto){
        this.partnerService.createPartner(partnerDto);
        return new ResponseEntity<>(new ApiResponseDto(true, CommonUtil.RESOURCE_CREATED), HttpStatus.CREATED);
    }

    @PutMapping("{partnerId}")
    public ResponseEntity<ApiResponseDto> createPartner(@PathVariable("partnerId") Long partnerId, @Valid @RequestBody PartnerDto partnerDto){
        this.partnerService.updatePartner(partnerId, partnerDto);
        return new ResponseEntity<>(new ApiResponseDto(true, CommonUtil.RESOURCE_UPDATED), HttpStatus.OK);
    }

    @GetMapping("{partnerId}")
    public ResponseEntity<PartnerDto> getPartner(@PathVariable("partnerId") Long partnerId){
        return new ResponseEntity<>(this.partnerService.findPartnerById(partnerId), HttpStatus.OK);
    }
}
