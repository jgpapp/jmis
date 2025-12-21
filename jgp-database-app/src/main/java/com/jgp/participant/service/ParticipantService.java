package com.jgp.participant.service;

import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.dto.ParticipantResponseDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface ParticipantService {

    Participant createParticipant(ParticipantDto participantDto);

    List<Participant> createParticipants(List<ParticipantDto> participantDtos);

    void updateParticipant(Long participantId, ParticipantDto participantDto);

    List<Participant> updateParticipants(Map<Long, ParticipantDto> participantUpdates);

    Participant createOrUpdateParticipant(ParticipantDto participantDto, Map<String, Participant> existingParticipants, boolean updateParticipant);

    Optional<Participant> findOneParticipantByJGPID(String jgpId);

    Map<String, Participant> findParticipantsByJGPIDs(List<String> jgpIds);

    ParticipantResponseDto findParticipantById(Long participantId, boolean includeAccounts);

    Page<Participant> availableParticipants(String searchText, Pageable pageable);

}
