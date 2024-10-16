package com.jgp.patner.service;

import com.jgp.patner.dto.PartnerDto;

import java.util.List;

public interface PartnerService {

    void createPartner(PartnerDto partnerDto);

    void updatePartner(Long userId, PartnerDto partnerDto);

    PartnerDto findPartnerById(Long userId);

    List<PartnerDto> getAllPartners();

}
