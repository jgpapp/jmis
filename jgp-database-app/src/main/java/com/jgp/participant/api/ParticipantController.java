package com.jgp.participant.api;

import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantDto;
import com.jgp.participant.dto.ParticipantResponseDto;
import com.jgp.participant.service.ParticipantService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/participants")
public class ParticipantController {

    private final ParticipantService participantService;

    @GetMapping
    public ResponseEntity<List<Participant>> getAvailableClients(@RequestParam(name = "pageNumber", defaultValue = "1") Integer pageNumber,
                                                                 @RequestParam(name = "pageSize", defaultValue = "200") Integer pageSize){
        final var sortedByDateCreated =
                PageRequest.of(pageNumber - 1, pageSize, Sort.by("dateCreated").descending());
        return new ResponseEntity<>(this.participantService.availableClients(sortedByDateCreated), HttpStatus.OK);
    }

    @GetMapping("{participantId}")
    public ResponseEntity<ParticipantResponseDto> getParticipantDto(@PathVariable("participantId") Long participantId, @RequestParam(name = "includeAccounts", defaultValue = "false") boolean includeAccounts){
        return new ResponseEntity<>(this.participantService.findParticipantById(participantId, includeAccounts), HttpStatus.OK);
    }
}
