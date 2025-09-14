package com.jgp.participant.service;

import com.jgp.authentication.aop.AuditTrail;
import com.jgp.authentication.domain.UserAuditOperationConstants;
import com.jgp.bmo.dto.BMOParticipantSearchCriteria;
import com.jgp.bmo.dto.MentorshipSearchCriteria;
import com.jgp.bmo.service.BMOClientDataService;
import com.jgp.bmo.service.MentorshipService;
import com.jgp.finance.dto.LoanSearchCriteria;
import com.jgp.finance.service.LoanService;
import com.jgp.monitoring.domain.predicate.OutComeMonitoringSearchCriteria;
import com.jgp.monitoring.service.OutComeMonitoringService;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.domain.ParticipantRepository;
import com.jgp.participant.domain.QParticipant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.dto.ParticipantResponseDto;
import com.jgp.participant.exception.ParticipantNotFoundException;
import com.jgp.participant.mapper.ParticipantMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.annotation.Validated;

import java.util.Optional;

@Service
@RequiredArgsConstructor
@Validated
public class ParticipantServiceImpl implements ParticipantService {

    private final ParticipantRepository participantRepository;
    private final ParticipantMapper participantMapper;
    private final LoanService loanService;
    private final BMOClientDataService bmoClientDataService;
    private final MentorshipService mentorshipService;
    private final OutComeMonitoringService outComeMonitoringService;

    @AuditTrail(operation = UserAuditOperationConstants.CREATE_PARTICIPANT)
    @Override
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public Participant createParticipant(ParticipantDto participantDto) {
        return this.participantRepository.save(new Participant(participantDto));
    }

    @AuditTrail(operation = UserAuditOperationConstants.UPDATE_PARTICIPANT, bodyIndex = 1, entityIdIndex = 0)
    @Override
    @Transactional
    public void updateParticipant(Long participantId, ParticipantDto participantDto) {
        var participant =  this.participantRepository.findById(participantId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .orElseThrow(() -> new ParticipantNotFoundException(participantId));
        participant.updateParticipant(participantDto);
        this.participantRepository.save(participant);
    }

    @Override
    public Optional<Participant> findOneParticipantByJGPID(String jgpId) {
        return this.participantRepository.findByJgpId(jgpId);
    }

    @Override
    public ParticipantResponseDto findParticipantById(Long participantId, boolean includeAccounts) {
        var participant =  this.participantRepository.findById(participantId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .map(this.participantMapper::toDto)
                .orElseThrow(() -> new ParticipantNotFoundException(participantId));

        if (includeAccounts){
            participant.setLoanDtos(this.loanService.getLoans(LoanSearchCriteria.builder().participantId(participantId).approvedByPartner(true).build(), Pageable.unpaged()).stream().toList());
            participant.setBmoClientDtos(this.bmoClientDataService.getBMODataRecords(BMOParticipantSearchCriteria.builder().approvedByPartner(true).participantId(participantId).build(), Pageable.unpaged()).stream().toList());
            participant.setMentorshipResponseDtos(this.mentorshipService.getMentorshipDataRecords(MentorshipSearchCriteria.builder().approvedByPartner(true).participantId(participantId).build(), Pageable.unpaged()).stream().toList());
            participant.setMonitoringResponseDtos(this.outComeMonitoringService.getOutComeMonitoringDataRecords(OutComeMonitoringSearchCriteria.builder()
                            .approved(true)
                            .participantId(participantId)
                            .build(), Pageable.unpaged()).stream().toList());
        }

        return participant;
    }

    @Override
    public Page<Participant> availableParticipants(String searchText, Pageable pageable) {
        final var qParticipant = QParticipant.participant;
        var isActive = qParticipant.isActive.eq(true).and(qParticipant.isDeleted.isFalse());
        if (null != searchText) {
            var partnerNamePredicate = qParticipant.participantName.containsIgnoreCase(searchText);
            var businessNamePredicate = qParticipant.businessName.containsIgnoreCase(searchText);
            var jgpPredicate = qParticipant.jgpId.containsIgnoreCase(searchText);
            var phoneNumberPredicate = qParticipant.phoneNumber.containsIgnoreCase(searchText);
            return this.participantRepository.findAll(isActive.and(businessNamePredicate.or(jgpPredicate).or(phoneNumberPredicate).or(partnerNamePredicate)), pageable);
        }
        return this.participantRepository.findAll(isActive, pageable);
    }
}
