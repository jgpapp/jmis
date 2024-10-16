package com.jgp.participant.mapper;

import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantResponseDto;
import java.util.ArrayList;
import java.util.List;
import org.springframework.stereotype.Component;

@Component
public class ParticipantMapper {

    public ParticipantResponseDto toDto(Participant participant) {

        ParticipantResponseDto participantResponseDto = new ParticipantResponseDto();

        if ( participant != null ) {
            if ( participant.getBusinessName() != null ) {
                participantResponseDto.setBusinessName( participant.getBusinessName() );
            }
            if ( participant.getJgpId() != null ) {
                participantResponseDto.setJgpId( participant.getJgpId() );
            }
            if ( participant.getPhoneNumber() != null ) {
                participantResponseDto.setPhoneNumber( participant.getPhoneNumber() );
            }
            if ( participant.getOwnerAge() != null ) {
                participantResponseDto.setOwnerAge( participant.getOwnerAge() );
            }
            if ( participant.getBusinessLocation() != null ) {
                participantResponseDto.setBusinessLocation( participant.getBusinessLocation() );
            }
            if ( participant.getIndustrySector() != null ) {
                participantResponseDto.setIndustrySector( participant.getIndustrySector() );
            }
            if ( participant.getBusinessSegment() != null ) {
                participantResponseDto.setBusinessSegment( participant.getBusinessSegment() );
            }
            if ( participant.getIsBusinessRegistered() != null ) {
                participantResponseDto.setIsBusinessRegistered( participant.getIsBusinessRegistered() );
            }
            if ( participant.getRegistrationNumber() != null ) {
                participantResponseDto.setRegistrationNumber( participant.getRegistrationNumber() );
            }
            if ( participant.getHasBMOMembership() != null ) {
                participantResponseDto.setHasBMOMembership( participant.getHasBMOMembership() );
            }
            if ( participant.getBmoMembership() != null ) {
                participantResponseDto.setBmoMembership( participant.getBmoMembership() );
            }
            if ( participant.getBestMonthlyRevenue() != null ) {
                participantResponseDto.setBestMonthlyRevenue( participant.getBestMonthlyRevenue() );
            }
            if ( participant.getWorstMonthlyRevenue() != null ) {
                participantResponseDto.setWorstMonthlyRevenue( participant.getWorstMonthlyRevenue() );
            }
            if ( participant.getTotalRegularEmployees() != null ) {
                participantResponseDto.setTotalRegularEmployees( participant.getTotalRegularEmployees() );
            }
            if ( participant.getYouthRegularEmployees() != null ) {
                participantResponseDto.setYouthRegularEmployees( participant.getYouthRegularEmployees() );
            }
            if ( participant.getTotalCasualEmployees() != null ) {
                participantResponseDto.setTotalCasualEmployees( participant.getTotalCasualEmployees() );
            }
            if ( participant.getYouthCasualEmployees() != null ) {
                participantResponseDto.setYouthCasualEmployees( participant.getYouthCasualEmployees() );
            }
            if ( participant.getSampleRecords() != null ) {
                participantResponseDto.setSampleRecords( participant.getSampleRecords() );
            }
            if ( participant.getPersonWithDisability() != null ) {
                participantResponseDto.setPersonWithDisability( participant.getPersonWithDisability() );
            }
            if ( participant.getRefugeeStatus() != null ) {
                participantResponseDto.setRefugeeStatus( participant.getRefugeeStatus() );
            }
            participantResponseDto.setOwnerGender( null != participant.getOwnerGender() ? participant.getOwnerGender().getName() : null );
        }


        return participantResponseDto;
    }

    public List<ParticipantResponseDto> toDto(List<Participant> participants) {
        if ( participants == null ) {
            return new ArrayList<>();
        }

        var list = new ArrayList<ParticipantResponseDto>( participants.size() );
        for ( Participant participant : participants ) {
            list.add( toDto( participant ) );
        }

        return list;
    }
}
