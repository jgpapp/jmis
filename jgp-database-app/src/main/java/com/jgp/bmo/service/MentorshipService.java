package com.jgp.bmo.service;

import com.jgp.bmo.domain.Mentorship;
import com.jgp.bmo.dto.MentorshipResponseDto;
import com.jgp.bmo.dto.MentorshipSearchCriteria;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;


public interface MentorshipService {

    void createMentorship(Mentorship mentorship);

    void approvedMentorShipData(List<Long> dataIds, Boolean approval);

    void deleteMentorShipDataByIds(List<Long> dataIds);

    Page<MentorshipResponseDto> getMentorshipDataRecords(MentorshipSearchCriteria searchCriteria, Pageable pageable);

    MentorshipResponseDto findMentorshipDataById(Long mentorshipId);

    List<Mentorship> findByDocumentId(Long documentId);
}
