package com.jgp.participant.service;

import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.dto.ParticipantResponseDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.lang.NonNull;

import java.util.Optional;

public interface ParticipantService {

    Participant createClient(ParticipantDto clientDto);

    Optional<Participant> findOneByJGPID(@NonNull String jgpId);

    ParticipantResponseDto findParticipantById(Long participantId, boolean includeAccounts);

    Page<Participant> availableClients(String searchText, Pageable pageable);
}
