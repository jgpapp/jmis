package com.jgp.finance.mapper;

import com.jgp.finance.domain.Loan;
import com.jgp.finance.dto.LoanDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.NullValueCheckStrategy;
import org.mapstruct.NullValueMappingStrategy;

import java.util.List;

@Mapper(componentModel = "spring", nullValueMappingStrategy = NullValueMappingStrategy.RETURN_DEFAULT, nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface LoanMapper {

    @Mapping(target = "id", expression = "java(loan.getId())")
    @Mapping(target = "partnerId", expression = "java(null != loan.getPartner() ? loan.getPartner().getId() : null)")
    @Mapping(target = "partnerName", expression = "java(null != loan.getPartner() ? loan.getPartner().getPartnerName() : null)")
    @Mapping(target = "participantName", expression = "java(null != loan.getParticipant() ? loan.getParticipant().getParticipantName() : null)")
    @Mapping(target = "businessName", expression = "java(null != loan.getParticipant() ? loan.getParticipant().getBusinessName() : null)")
    @Mapping(target = "participantJGPID", expression = "java(null != loan.getParticipant() ? loan.getParticipant().getJgpId() : null)")
    @Mapping(target = "loanNumber", expression = "java(null != loan.getLoanNumber() ? loan.getLoanNumber() : null)")
    @Mapping(target = "loanStatus", expression = "java(null != loan.getLoanStatus() ? loan.getLoanStatus().getName() : null)")
    @Mapping(target = "loanQuality", expression = "java(null != loan.getLoanQuality() ? loan.getLoanQuality().getName() : null)")
    @Mapping(target = "pipeLineSource", expression = "java(null != loan.getPipeLineSource() ? loan.getPipeLineSource() : null)")
    @Mapping(target = "loanAmountAccessed", expression = "java(null != loan.getLoanAmount() ? loan.getLoanAmount() : null)")
    @Mapping(target = "loanDuration", expression = "java(null != loan.getLoanDuration() ? loan.getLoanDuration() : null)")
    @Mapping(target = "dateApplied", expression = "java(null != loan.getDateApplied() ? loan.getDateApplied() : null)")
    @Mapping(target = "dateDisbursed", expression = "java(null != loan.getDateDisbursed() ? loan.getDateDisbursed() : null)")
    @Mapping(target = "loanAmountApplied", ignore = true)
    @Mapping(target = "loanAmountApproved", ignore = true)
    @Mapping(target = "dateRecordedByPartner", ignore = true)
    @Mapping(target = "dateAddedToDB", expression = "java(null != loan.getDateCreated() ? loan.getDateCreated() : null)")
    @Mapping(target = "uploadedBy", expression = "java(null != loan.getCreatedBy() ? loan.getCreatedBy().getUserFullName() : null)")
    @Mapping(target = "dateUploaded", expression = "java(null != loan.getDateCreated() ? loan.getDateCreated() : null)")
    @Mapping(target = "approvedBy", expression = "java(null != loan.getApprovalBy() ? loan.getApprovalBy().getUserFullName() : null)")
    @Mapping(target = "dateApproved", expression = "java(null != loan.getDateApproved() ? loan.getDateApproved() : null)")
    LoanDto toDto(Loan loan);

    List<LoanDto> toDto(List<Loan> loan);
}
