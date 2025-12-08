package com.jgp.finance.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.finance.domain.LoanTransaction;
import com.jgp.finance.domain.LoanTransactionRepository;
import com.jgp.finance.domain.predicate.LoanPredicateBuilder;
import com.jgp.finance.dto.LoanSearchCriteria;
import com.jgp.finance.dto.LoanTransactionResponseDto;
import com.jgp.finance.mapper.LoanMapper;
import com.jgp.finance.domain.Loan;
import com.jgp.finance.domain.LoanRepository;
import com.jgp.finance.dto.LoanDto;
import com.jgp.finance.mapper.LoanTransactionMapper;
import com.jgp.infrastructure.bulkimport.event.DataApprovedEvent;
import com.jgp.participant.domain.ParticipantRepository;
import com.jgp.shared.exception.DataRulesViolationException;
import com.jgp.shared.exception.ResourceNotFound;
import com.jgp.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Service
@RequiredArgsConstructor
@Slf4j
public class LoanServiceImpl implements LoanService {

    private final LoanRepository loanRepository;
    private final LoanTransactionRepository loanTransactionRepository;
    private final LoanTransactionMapper loanTransactionMapper;
    private final LoanMapper loanMapper;
    private final LoanPredicateBuilder loanPredicateBuilder;
    private final PlatformSecurityContext platformSecurityContext;
    private final ApplicationContext applicationContext;
    private final ParticipantRepository participantRepository;

    @Transactional
    @Override
    public void createOrUpdateLoan(Loan loan) {
        final var existingLoanOptional = null != loan.getLoanNumber() ? this.loanRepository.findByParticipantAndLoanNumber(loan.getParticipant(), loan.getLoanNumber()).filter(l -> Boolean.FALSE.equals(l.getIsDeleted())) : Optional.<Loan>empty();
        if (existingLoanOptional.isPresent()){
            var existingLoan = existingLoanOptional.get();
            var loanTransaction = loan.getLoanTransactions().stream().findFirst().orElse(null);
            if (Objects.nonNull(loanTransaction)){
                checkSingleDisbursementPerDayValidation(existingLoan, loanTransaction);
            }
            existingLoan.addLoanTransaction(loan.getLoanTransactions().stream().findFirst().orElse(null));
            this.loanRepository.save(existingLoan);
        }else {
            this.loanRepository.save(loan);
        }
    }

    @Transactional
    @Override
    public void approvedTransactionsLoans(List<Long> transactionsIds, Boolean approval) {
        var loanTransactions = this.loanTransactionRepository.findAllById(transactionsIds);
        var currentUser = this.platformSecurityContext.getAuthenticatedUserIfPresent();
        var currentUserPartner = Objects.nonNull(currentUser) ? currentUser.getPartner() : null;
        if (transactionsIds.isEmpty() && Objects.nonNull(currentUserPartner)) {
            loanTransactions =  this.loanTransactionRepository.getLoanTransactions(currentUserPartner.getId(), false, Pageable.unpaged()).getContent();
        }

        if (Boolean.TRUE.equals(approval)) {
            int count = 0;
            var loansToSave = new ArrayList<Loan>();
            Set<LocalDate> dataDates = new HashSet<>();
            for (var loanTransaction : loanTransactions) {
                var loan = loanTransaction.getLoan();
                if (!loan.isDataApprovedByPartner()) {
                    loan.approveData(true, currentUser);
                }
                var participant = loan.getParticipant();
                if (Boolean.FALSE.equals(participant.getIsActive())) {
                    participant.activateParticipant();
                }
                participant.incrementPrePaidAmount(loanTransaction.getOutStandingAmount().compareTo(BigDecimal.ZERO) < 0 ? loanTransaction.getOutStandingAmount().negate() : BigDecimal.ZERO);


                var transaction = loan.getLoanTransactions().stream()
                        .filter(txn -> loanTransaction.getId().equals(txn.getId())).findFirst().orElse(null);
                if (Objects.nonNull(transaction) && !transaction.isApproved()) {
                    transaction.approveData(approval, currentUser);
                }
                loansToSave.add(loan);
                count++;
                if (count % 20 == 0) { // Flush and clear the session every 20 entities
                    this.loanRepository.saveAllAndFlush(loansToSave);
                    count = 0;
                    loansToSave = new ArrayList<>();
                }
                dataDates.add(loan.getDateDisbursed());
            }
            this.loanRepository.saveAllAndFlush(loansToSave);
            if (Objects.nonNull(currentUserPartner)) {
                this.applicationContext.publishEvent(new DataApprovedEvent(Set.of(currentUserPartner.getId()), dataDates));
            }
        }else {
            rejectAndDeleteTransactions(loanTransactions);
        }
    }

    private void rejectAndDeleteTransactions(List<LoanTransaction> transactions){
        int count = 0;
        var loanTransactionsToDelete = new ArrayList<LoanTransaction>();
        for (LoanTransaction loanTransaction : transactions) {
            loanTransactionsToDelete.add(loanTransaction);
            count++;
            if (count % 50 == 0) { // Flush and clear the session every 50 entities
                deleteSelectedRecords(loanTransactionsToDelete);
                count = 0;
                loanTransactionsToDelete = new ArrayList<>();
            }

        }
        deleteSelectedRecords(loanTransactionsToDelete);

    }


    private void deleteSelectedRecords(List<LoanTransaction> loanTransactionsToDelete) {
        if (loanTransactionsToDelete.isEmpty()) {
            return;
        }
        final var loanTransactionsIds = loanTransactionsToDelete.stream().map(LoanTransaction::getId).toList();
        final var loansIds = loanTransactionsToDelete.stream().map(lt -> lt.getLoan().getId()).toList();
        final var participantsIds = loanTransactionsToDelete.stream().map(lt -> lt.getLoan().getParticipant().getId()).toList();
        this.loanTransactionRepository.deleteLoanTransactionsByIds(loanTransactionsIds);
        this.loanRepository.deleteLoansByIds(loansIds);
        this.participantRepository.deleteParticipantsByIds(participantsIds);
    }

    @Override
    public Page<LoanDto> getLoans(LoanSearchCriteria searchCriteria, Pageable pageable) {
        final var loans = this.loanRepository.findAll(loanPredicateBuilder.buildPredicateForSearchLoans(searchCriteria), pageable);
        return new PageImpl<>(this.loanMapper.toDto(loans.stream().toList()), pageable, loans.getTotalElements());
    }

    @Override
    public LoanDto findLoanById(Long loanId) {
        return this.loanRepository.findById(loanId).filter(t -> Boolean.FALSE.equals(t.getIsDeleted())).map(this.loanMapper::toDto).orElseThrow(() -> new RuntimeException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));

    }

    @Override
    public Page<LoanTransactionResponseDto> getAvailableLoanTransactions(Long partnerId, Long loanId, Boolean isApproved, Pageable pageable) {
        Page<LoanTransaction> transactions;
        if (Objects.nonNull(loanId)){
            final var loan = this.loanRepository.findById(loanId).filter(t -> Boolean.FALSE.equals(t.getIsDeleted())).orElseThrow(() -> new ResourceNotFound(HttpStatus.NOT_FOUND));
            final var txns = loan.getLoanTransactions().stream().toList();
            transactions = new PageImpl<>(txns, pageable, txns.size());
        }else {
            transactions =  this.loanTransactionRepository.getLoanTransactions(partnerId, isApproved, pageable);
        }
        return new PageImpl<>(this.loanTransactionMapper.toDto(transactions.stream().toList()), pageable, transactions.getTotalElements());
    }

    private void checkSingleDisbursementPerDayValidation(Loan loan, LoanTransaction loanTransaction){
        if (!LoanTransaction.TransactionType.DISBURSEMENT.equals(loanTransaction.getTransactionType())){
            return;
        }
        var duplicateDisbursementExist = loan.getLoanTransactions().stream()
                .filter(lt -> lt.getTransactionDate().equals(loanTransaction.getTransactionDate()))
                .anyMatch(lt -> lt.getTransactionType().equals(loanTransaction.getTransactionType()));
        if (duplicateDisbursementExist){
            throw new DataRulesViolationException("Duplicate Disbursement On Same Day!!!");
        }
    }

}
