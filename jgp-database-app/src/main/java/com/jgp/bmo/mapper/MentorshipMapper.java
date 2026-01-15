package com.jgp.bmo.mapper;

import com.jgp.bmo.domain.Mentorship;
import com.jgp.bmo.dto.MentorshipResponseDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.NullValueCheckStrategy;
import org.mapstruct.NullValueMappingStrategy;

import java.util.List;

@Mapper(componentModel = "spring", nullValueMappingStrategy = NullValueMappingStrategy.RETURN_DEFAULT, nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface MentorshipMapper {

    @Mapping(target = "id", expression = "java(mentorship.getId())")
    @Mapping(target = "partnerId", expression = "java(null != mentorship.getPartner() ? mentorship.getPartner().getId() : null)")
    @Mapping(target = "partnerName", expression = "java(null != mentorship.getPartner() ? mentorship.getPartner().getPartnerName() : null)")
    @Mapping(target = "participantId", expression = "java(null != mentorship.getParticipant() ? mentorship.getParticipant().getId() : null)")
    @Mapping(target = "participantJGPID", expression = "java(null != mentorship.getParticipant() ? mentorship.getParticipant().getJgpId() : null)")
    @Mapping(target = "participantName", expression = "java(null != mentorship.getParticipant() ? mentorship.getParticipant().getParticipantName() : null)")
    @Mapping(target = "businessName", expression = "java(null != mentorship.getParticipant() ? mentorship.getParticipant().getBusinessName() : null)")
    @Mapping(target = "mentorShipDate", expression = "java(null != mentorship.getMentorShipDate() ? mentorship.getMentorShipDate() : null)")
    @Mapping(target = "mentorShipOrganization", expression = "java(null != mentorship.getMentorShipOrganization() ? mentorship.getMentorShipOrganization() : null)")
    @Mapping(target = "bmoMemberShip", expression = "java(null != mentorship.getBmoMemberShip() ? mentorship.getBmoMemberShip() : null)")
    @Mapping(target = "msmeSessionsCovered", expression = "java(null != mentorship.getMsmeSessionsCovered() ? mentorship.getMsmeSessionsCovered() : null)")
    @Mapping(target = "smeSessionsCovered", expression = "java(null != mentorship.getSmeSessionsCovered() ? mentorship.getSmeSessionsCovered() : null)")
    @Mapping(target = "uploadedBy", expression = "java(null != mentorship.getCreatedBy() ? mentorship.getCreatedBy().getUserFullName() : null)")
    @Mapping(target = "dateUploaded", expression = "java(null != mentorship.getDateCreated() ? com.jgp.util.CommonUtil.getNairobiISODATEDateFormatter().format(mentorship.getDateCreated()) : null)")
    @Mapping(target = "approvedBy", expression = "java(null != mentorship.getApprovalBy() ? mentorship.getApprovalBy().getUserFullName() : null)")
    @Mapping(target = "dateApproved", expression = "java(null != mentorship.getDateApproved() ? mentorship.getDateApproved() : null)")
    MentorshipResponseDto toDto(Mentorship mentorship);

    List<MentorshipResponseDto> toDto(List<Mentorship> mentorshipList);
}
