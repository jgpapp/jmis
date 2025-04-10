package com.jgp.bmo.mapper;

import com.jgp.bmo.domain.BMOParticipantData;
import com.jgp.bmo.dto.BMOClientDto;
import java.util.List;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.NullValueCheckStrategy;
import org.mapstruct.NullValueMappingStrategy;

@Mapper(componentModel = "spring", nullValueMappingStrategy = NullValueMappingStrategy.RETURN_DEFAULT, nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface BMOClientMapper {

    @Mapping(target = "id", expression = "java(bmoClientData.getId())")
    @Mapping(target = "partnerId", expression = "java(null != bmoClientData.getPartner() ? bmoClientData.getPartner().getId() : null)")
    @Mapping(target = "partnerName", expression = "java(null != bmoClientData.getPartner() ? bmoClientData.getPartner().getPartnerName() : null)")
    @Mapping(target = "participantId", expression = "java(null != bmoClientData.getParticipant() ? bmoClientData.getParticipant().getId() : null)")
    @Mapping(target = "participantJGPID", expression = "java(null != bmoClientData.getParticipant() ? bmoClientData.getParticipant().getJgpId() : null)")
    @Mapping(target = "participantName", expression = "java(null != bmoClientData.getParticipant() ? bmoClientData.getParticipant().getParticipantName() : null)")
    @Mapping(target = "tasAttended", expression = "java(null != bmoClientData.getTasAttended() ? bmoClientData.getTasAttended() : null)")
    @Mapping(target = "taSessionsAttended", expression = "java(null != bmoClientData.getTaSessionsAttended() ? bmoClientData.getTaSessionsAttended() : null)")
    @Mapping(target = "isRecommendedForFinance", expression = "java(null != bmoClientData.getIsRecommendedForFinance() ? bmoClientData.getIsRecommendedForFinance() : null)")
    @Mapping(target = "decisionDate", expression = "java(null != bmoClientData.getDecisionDate() ? bmoClientData.getDecisionDate() : null)")
    @Mapping(target = "dateFormSubmitted", expression = "java(null != bmoClientData.getDateFormSubmitted() ? bmoClientData.getDateFormSubmitted() : null)")
    @Mapping(target = "isApplicantEligible", expression = "java(null != bmoClientData.getIsApplicantEligible() ? bmoClientData.getIsApplicantEligible() : null)")
    @Mapping(target = "fiBusinessReferred", expression = "java(null != bmoClientData.getFiBusinessReferred() ? bmoClientData.getFiBusinessReferred() : null)")
    @Mapping(target = "dateRecordedByPartner", ignore = true)
    @Mapping(target = "dateRecordedToJGPDB", expression = "java(null != bmoClientData.getDateCreated() ? bmoClientData.getDateCreated() : null)")
    @Mapping(target = "taNeeds", expression = "java(null != bmoClientData.getTaNeeds() ? bmoClientData.getTaNeeds() : null)")
    BMOClientDto toDto(BMOParticipantData bmoClientData);

    List<BMOClientDto> toDto(List<BMOParticipantData> bmoClientDataList);
}
