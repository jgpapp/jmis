package com.jgp.bmo.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.bmo.domain.Mentorship;
import com.jgp.bmo.domain.MentorshipRepository;
import com.jgp.bmo.domain.predicate.MentorshipPredicateBuilder;
import com.jgp.bmo.dto.MentorshipResponseDto;
import com.jgp.bmo.dto.MentorshipSearchCriteria;
import com.jgp.bmo.mapper.MentorshipMapper;
import com.jgp.infrastructure.bulkimport.event.DataApprovedEvent;
import com.jgp.participant.domain.Participant;
import com.jgp.participant.domain.ParticipantRepository;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

@Service
@Slf4j
@RequiredArgsConstructor
public class MentorshipServiceImpl implements MentorshipService {

    private final MentorshipRepository mentorshipRepository;
    private final MentorshipMapper mentorshipMapper;
    private final MentorshipPredicateBuilder mentorshipPredicateBuilder;
    private final PlatformSecurityContext platformSecurityContext;
    private final ApplicationContext applicationContext;
    private final ParticipantRepository participantRepository;

    @Transactional
    @Override
    public void saveMentorshipWithParticipant(Mentorship mentorship, Boolean updateParticipantInfo, Map<Long, ParticipantDto> participantDtoMap) {
        Participant participant = mentorship.getParticipant();
        ParticipantDto participantDto = participantDtoMap.get(participant.getId());
        if (Boolean.TRUE.equals(updateParticipantInfo)) {
            participant.updateParticipant(participantDto);
        }
        participant.updateBusinessLocation(participantDto);
        this.participantRepository.save(participant);
        this.mentorshipRepository.save(mentorship);
    }

    @Override
    public void approvedMentorShipData(List<Long> dataIds, Boolean approval) {
        var mentorshipData = this.mentorshipRepository.findAllById(dataIds);
        var currentUser = this.platformSecurityContext.getAuthenticatedUserIfPresent();
        var currentUserPartner = Objects.nonNull(currentUser) ? currentUser.getPartner() : null;
        if (dataIds.isEmpty() && Objects.nonNull(currentUserPartner)) {
            mentorshipData = this.mentorshipRepository.findAll(this.mentorshipPredicateBuilder.buildPredicateForSearchMentorshipData(new MentorshipSearchCriteria(currentUserPartner.getId(), null, false)), Pageable.unpaged()).getContent();
        }

        if (Boolean.TRUE.equals(approval)) {
            int count = 0;
            var mentorshipsToSave = new ArrayList<Mentorship>();
            Set<LocalDate> dataDates = new HashSet<>();
            for (Mentorship mentorship : mentorshipData) {
                mentorship.approveData(true, currentUser);
                mentorshipsToSave.add(mentorship);
                count++;
                if (count % 20 == 0) { // Flush and clear the session every 20 entities
                    this.mentorshipRepository.saveAllAndFlush(mentorshipsToSave);
                    count = 0;
                    mentorshipsToSave = new ArrayList<>();
                }
                dataDates.add(mentorship.getMentorShipDate());
            }
            this.mentorshipRepository.saveAllAndFlush(mentorshipsToSave);
            if (Objects.nonNull(currentUserPartner)) {
                this.applicationContext.publishEvent(new DataApprovedEvent(Set.of(currentUserPartner.getId()), dataDates));
            }
        }else {
            rejectAndDeleteBMOParticipantsData(mentorshipData);
        }
    }

    private void rejectAndDeleteBMOParticipantsData(List<Mentorship> mentorships){
        final var mentorshipIds = mentorships.stream()
                .map(Mentorship::getId).toList();
        this.mentorshipRepository.deleteMentorshipDataByIds(mentorshipIds);
    }

    @Override
    public Page<MentorshipResponseDto> getMentorshipDataRecords(MentorshipSearchCriteria searchCriteria, Pageable pageable) {
        final var bmoData = this.mentorshipRepository.findAll(this.mentorshipPredicateBuilder.buildPredicateForSearchMentorshipData(searchCriteria), pageable);
        return new PageImpl<>(this.mentorshipMapper.toDto(bmoData.stream().toList()), pageable, bmoData.getTotalElements());
    }

    @Override
    public MentorshipResponseDto findMentorshipDataById(Long mentorshipId) {
        return this.mentorshipRepository.findById(mentorshipId).map(this.mentorshipMapper::toDto).orElseThrow(() -> new RuntimeException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));
    }
}
