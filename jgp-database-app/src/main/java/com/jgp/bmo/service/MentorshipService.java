package com.jgp.bmo.service;

import com.jgp.bmo.domain.Mentorship;
import com.jgp.bmo.dto.MentorshipResponseDto;
import com.jgp.bmo.dto.MentorshipSearchCriteria;
import com.jgp.participant.dto.ParticipantDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Map;


public interface MentorshipService {

    void saveMentorshipWithParticipant(Mentorship mentorship, Boolean updateParticipantInfo, Map<Long, ParticipantDto> participantDtoMap);

    void approvedMentorShipData(List<Long> dataIds, Boolean approval);

    Page<MentorshipResponseDto> getMentorshipDataRecords(MentorshipSearchCriteria searchCriteria, Pageable pageable);

    MentorshipResponseDto findMentorshipDataById(Long mentorshipId);

    List<Mentorship> findByDocumentId(Long documentId);
}
