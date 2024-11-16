package com.jgp.patner.service;

import com.jgp.patner.dto.PartnerDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface PartnerService {

    void createPartner(PartnerDto partnerDto);

    void updatePartner(Long userId, PartnerDto partnerDto);

    PartnerDto findPartnerById(Long userId);

    Page<PartnerDto> getAllPartners(Pageable pageable);

}
