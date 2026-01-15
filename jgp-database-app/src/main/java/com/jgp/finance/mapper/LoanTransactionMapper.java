package com.jgp.finance.mapper;

import com.jgp.finance.domain.LoanTransaction;
import com.jgp.finance.dto.LoanTransactionResponseDto;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.NullValueCheckStrategy;
import org.mapstruct.NullValueMappingStrategy;

import java.util.List;

@Mapper(componentModel = "spring", nullValueMappingStrategy = NullValueMappingStrategy.RETURN_DEFAULT, nullValueCheckStrategy = NullValueCheckStrategy.ALWAYS)
public interface LoanTransactionMapper {

    @Mapping(target = "id", expression = "java(loanTransaction.getId())")
    @Mapping(target = "tranch", expression = "java(loanTransaction.getTranchName())")
    @Mapping(target = "transactionType", expression = "java(null != loanTransaction.getTransactionType() ? loanTransaction.getTransactionType().getName() : null)")
    @Mapping(target = "transactionDate", expression = "java(null != loanTransaction.getTransactionDate() ? loanTransaction.getTransactionDate() : null)")
    @Mapping(target = "amount", expression = "java(null != loanTransaction.getAmount() ? loanTransaction.getAmount() : null)")
    @Mapping(target = "participantJGPID", expression = "java(null != loanTransaction.getLoan().getParticipant() ? loanTransaction.getLoan().getParticipant().getJgpId() : null)")
    @Mapping(target = "participantName", expression = "java(null != loanTransaction.getLoan().getParticipant() ? loanTransaction.getLoan().getParticipant().getParticipantName() : null)")
    @Mapping(target = "businessName", expression = "java(null != loanTransaction.getLoan().getParticipant() ? loanTransaction.getLoan().getParticipant().getBusinessName() : null)")
    @Mapping(target = "pipeLineSource", expression = "java(null != loanTransaction.getLoan() ? loanTransaction.getLoan().getPipeLineSource() : null)")
    @Mapping(target = "loanDuration", expression = "java(null != loanTransaction.getLoan() ? loanTransaction.getLoan().getLoanDuration() : null)")
    @Mapping(target = "uploadedBy", expression = "java(null != loanTransaction.getCreatedBy() ? loanTransaction.getCreatedBy().getUserFullName() : null)")
    @Mapping(target = "dateUploaded", expression = "java(null != loanTransaction.getDateCreated() ? com.jgp.util.CommonUtil.getNairobiISODATEDateFormatter().format(loanTransaction.getDateCreated()) : null)")
    @Mapping(target = "approvedBy", expression = "java(null != loanTransaction.getApprovalBy() ? loanTransaction.getApprovalBy().getUserFullName() : null)")
    @Mapping(target = "dateApproved", expression = "java(null != loanTransaction.getDateApproved() ? loanTransaction.getDateApproved() : null)")
     LoanTransactionResponseDto toDto(LoanTransaction loanTransaction);

    List<LoanTransactionResponseDto> toDto(List<LoanTransaction> loanTransactions);
}
