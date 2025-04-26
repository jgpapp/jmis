package com.jgp.participant.mapper;

import com.jgp.participant.domain.Participant;
import com.jgp.participant.dto.ParticipantResponseDto;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.jgp.util.CommonUtil;
import org.springframework.stereotype.Component;

@Component
public class ParticipantMapper {

    public ParticipantResponseDto toDto(Participant participant) {

        ParticipantResponseDto participantResponseDto = new ParticipantResponseDto();

        if ( participant != null ) {
            participantResponseDto.setId( participant.getId() );
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
            if ( participant.getLocationCountyCode() != null ) {
                participantResponseDto.setLocationCountyCode( participant.getLocationCountyCode() );
            }
            if ( participant.getIndustrySector() != null ) {
                participantResponseDto.setIndustrySector( participant.getIndustrySector() );
            }
            if ( participant.getBusinessSegment() != null ) {
                participantResponseDto.setBusinessSegment( participant.getBusinessSegment() );
            }
            if ( participant.getRegistrationNumber() != null ) {
                participantResponseDto.setRegistrationNumber( participant.getRegistrationNumber() );
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
                participantResponseDto.setSampleRecords( Arrays.stream(participant.getSampleRecords().split(",")).map(String::trim).toList() );
            }
            if ( participant.getPersonWithDisability() != null ) {
                participantResponseDto.setPersonWithDisability( participant.getPersonWithDisability() );
            }
            if ( participant.getRefugeeStatus() != null ) {
                participantResponseDto.setRefugeeStatus( participant.getRefugeeStatus() );
            }
            participantResponseDto.setSavings( null != participant.getPrePayment() ? CommonUtil.NUMBER_FORMAT.format(participant.getPrePayment()) : "0.00");
            participantResponseDto.setOwnerGender( null != participant.getOwnerGender() ? participant.getOwnerGender().getName() : null );
            participantResponseDto.setPassport( null != participant.getPassport() ? participant.getPassport() : null );
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
