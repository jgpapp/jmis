package com.jgp.patner.service;


import com.jgp.authentication.aop.AuditTrail;
import com.jgp.authentication.domain.UserAuditOperationConstants;
import com.jgp.patner.domain.Partner;
import com.jgp.patner.domain.PartnerRepository;
import com.jgp.patner.dto.PartnerDto;
import com.jgp.patner.exception.PartnerNotFoundException;
import com.jgp.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class PartnerServiceImpl implements PartnerService {

    private final PartnerRepository partnerRepository;


    @AuditTrail(operation = UserAuditOperationConstants.CREATE_PARTNER)
    @Transactional
    @Override
    public void createPartner(PartnerDto partnerDto) {
        try {
            this.partnerRepository.save(Partner.createPartner(partnerDto));
        }catch (Exception e){
            throw new IllegalArgumentException(e);
        }

    }

    @Override
    public Partner findPartnerById(Long partnerId) {
        return this.partnerRepository.findById(partnerId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .orElseThrow(() -> new PartnerNotFoundException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));
    }

    @AuditTrail(operation = UserAuditOperationConstants.UPDATE_PARTNER, bodyIndex = 1, entityIdIndex = 0)
    @Transactional
    @Override
    public void updatePartner(Long partnerId, PartnerDto partnerDto) {
        var currentPartner = this.partnerRepository.findById(partnerId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .orElseThrow(() -> new PartnerNotFoundException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));
        try {
            currentPartner.updatePartner(partnerDto);
            this.partnerRepository.save(currentPartner);
        }catch (Exception e){
            throw new IllegalArgumentException(e);
        }

    }

    @Override
    public PartnerDto findPartnerDtoById(Long userId) {
        return this.partnerRepository.findById(userId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .map(p -> new PartnerDto(p.getId(), p.getPartnerName(), p.getType().getName(), p.getType().name()))
                .orElseThrow(() -> new PartnerNotFoundException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));
    }

    @Override
    public Page<PartnerDto> getAllPartners(Pageable pageable) {
        return new PageImpl<>(this.partnerRepository.findAll(pageable).stream().filter(t -> Boolean.FALSE.equals(t.getIsDeleted())).map(p -> new PartnerDto(p.getId(), p.getPartnerName(), p.getType().getName(), p.getType().name())).toList(), pageable, this.partnerRepository.findAll().size());
    }

}
