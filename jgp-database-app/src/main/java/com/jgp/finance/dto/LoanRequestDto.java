package com.jgp.finance.dto;

import com.jgp.authentication.domain.AppUser;
import com.jgp.finance.domain.Loan;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.participant.dto.ParticipantRequestDto;
import com.jgp.patner.domain.Partner;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;

@Builder
public record LoanRequestDto(
        ParticipantRequestDto participantRequestDto,

        String loanNumber,

        @NotBlank(message = "Pipeline source is required !!")
        String pipeLineSource,

        @NotNull(message = "Loan Amount is required !!")
        @Min(value = 0, message = "Loan Amount is required !!")
        BigDecimal loanAmount,

        @NotNull(message = "Outstanding Loan Amount is required !!")
        BigDecimal loanOutStandingAmount,

        BigDecimal loanAmountRepaid,

        String loanerType,

        String loanProduct,

        @NotNull(message = "Loan duration is required !!")
        Integer loanDuration,

        @NotNull(message = "Application Date is required !!")
        LocalDate dateApplied,

        LocalDate dateRecordedByPartner,

        @NotNull(message = "Date of record added to JGP database is required !!")
        LocalDate dateAddedToDB,

        @NotNull(message = "Disbursement Date is required !!")
        LocalDate dateDisbursed,

        Loan.LoanStatus loanStatus,

        @NotNull(message = "Loan Quality is required !!")
        Loan.LoanQuality loanQuality,

        String uniqueValues,

        Integer rowIndex,

        String rowErrorMessage,

        Partner partner,

        AppUser createdBy,

        Document document

) {
}
