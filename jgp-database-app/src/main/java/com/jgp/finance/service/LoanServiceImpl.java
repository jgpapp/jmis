package com.jgp.finance.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.finance.domain.predicate.LoanPredicateBuilder;
import com.jgp.finance.dto.LoanSearchCriteria;
import com.jgp.finance.mapper.LoanMapper;
import com.jgp.finance.domain.Loan;
import com.jgp.finance.domain.LoanRepository;
import com.jgp.finance.dto.LoanDto;
import com.jgp.util.CommonUtil;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Slf4j
public class LoanServiceImpl implements LoanService {

    private final LoanRepository loanRepository;
    private final LoanMapper loanMapper;
    private final LoanPredicateBuilder loanPredicateBuilder;
    private final PlatformSecurityContext platformSecurityContext;

    @Override
    public void createLoans(List<Loan> loans) {
        this.loanRepository.saveAll(loans);
    }

    @Transactional
    @Override
    public void approvedParticipantsLoansData(List<Long> dataIds, Boolean approval) {
        var loans = this.loanRepository.findAllById(dataIds);
        if (dataIds.isEmpty()) {
            var currentUser = this.platformSecurityContext.getAuthenticatedUserIfPresent();
            var currentUserPartner = Objects.nonNull(currentUser) ? currentUser.getPartner() : null;
            if(Objects.nonNull(currentUserPartner)) {
                loans = this.loanRepository.findAll(this.loanPredicateBuilder.buildPredicateForSearchLoans(new LoanSearchCriteria(currentUserPartner.getId(), null, null, null,  false, null, null)), Pageable.unpaged()).getContent();
            }
        }
        int count = 0;
        var loansToSave = new ArrayList<Loan>();
        for(Loan loan : loans) {
            loan.approveData(approval);
            var participant = loan.getParticipant();
            if (Boolean.TRUE.equals(approval) && Boolean.FALSE.equals(participant.getIsActive())){
                participant.activateParticipant();
            }
            loansToSave.add(loan);
            count++;
            if (count % 20 == 0) { // Flush and clear the session every 20 entities
                this.loanRepository.saveAllAndFlush(loansToSave);
                count = 0;
                loansToSave = new ArrayList<>();
            }
        }
        this.loanRepository.saveAllAndFlush(loansToSave);
    }

    @Override
    public List<LoanDto> getLoans(LoanSearchCriteria searchCriteria, Pageable pageable) {
        return this.loanMapper.toDto(this.loanRepository.findAll(loanPredicateBuilder.buildPredicateForSearchLoans(searchCriteria), pageable).stream().toList());
    }

    @Override
    public LoanDto findLoanById(Long loanId) {
        return this.loanRepository.findById(loanId).map(this.loanMapper::toDto).orElseThrow(() -> new RuntimeException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));

    }
}
