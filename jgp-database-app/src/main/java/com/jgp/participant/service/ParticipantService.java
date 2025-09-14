package com.jgp.participant.service;

import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.dto.ParticipantResponseDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.Optional;

public interface ParticipantService {

    Participant createParticipant(ParticipantDto participantDto);

    void updateParticipant(Long participantId, ParticipantDto participantDto);

    Optional<Participant> findOneParticipantByJGPID(String jgpId);

    ParticipantResponseDto findParticipantById(Long participantId, boolean includeAccounts);

    Page<Participant> availableParticipants(String searchText, Pageable pageable);

}
