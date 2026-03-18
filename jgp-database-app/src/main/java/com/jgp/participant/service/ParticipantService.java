package com.jgp.participant.service;

import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantRequestDto;
import com.jgp.participant.dto.ParticipantResponseDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Map;

public interface ParticipantService {

    void updateParticipant(Long participantId, ParticipantRequestDto participantRequestDto);

    Participant createOrUpdateParticipant(ParticipantRequestDto participantRequestDto, Map<String, Participant> existingParticipants, boolean updateParticipant);

    Map<String, Participant> findParticipantsByJGPIDs(List<String> jgpIds);

    ParticipantResponseDto findParticipantById(Long participantId, boolean includeAccounts);

    Page<Participant> availableParticipants(String searchText, Pageable pageable);

}
