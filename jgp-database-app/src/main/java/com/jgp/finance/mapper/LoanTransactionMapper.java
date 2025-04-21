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

    @Mapping(target = "transactionType", expression = "java(null != loanTransaction.getTransactionType() ? loanTransaction.getTransactionType().getName() : null)")
    @Mapping(target = "transactionDate", expression = "java(null != loanTransaction.getTransactionDate() ? loanTransaction.getTransactionDate() : null)")
    @Mapping(target = "amount", expression = "java(null != loanTransaction.getAmount() ? loanTransaction.getAmount() : null)")
     LoanTransactionResponseDto toDto(LoanTransaction loanTransaction);

    List<LoanTransactionResponseDto> toDto(List<LoanTransaction> loanTransactions);
}
