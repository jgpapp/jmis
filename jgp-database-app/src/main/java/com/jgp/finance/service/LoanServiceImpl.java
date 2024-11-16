package com.jgp.finance.service;

import com.jgp.authentication.service.PlatformSecurityContext;
import com.jgp.bmo.dto.BMOParticipantSearchCriteria;
import com.jgp.finance.domain.predicate.LoanPredicateBuilder;
import com.jgp.finance.dto.LoanSearchCriteria;
import com.jgp.finance.mapper.LoanMapper;
import com.jgp.finance.domain.Loan;
import com.jgp.finance.domain.LoanRepository;
import com.jgp.finance.dto.LoanDto;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.participant.domain.ParticipantRepository;
import com.jgp.util.CommonUtil;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class LoanServiceImpl implements LoanService {

    private final LoanRepository loanRepository;
    private final ApplicationEventPublisher publisher;
    private final LoanMapper loanMapper;
    private final LoanPredicateBuilder loanPredicateBuilder;
    private final ParticipantRepository participantRepository;
    private final PlatformSecurityContext platformSecurityContext;

    @Override
    public void createLoans(List<Loan> loans) {
        this.loanRepository.saveAll(loans);
    }

    @Override
    public void uploadBulkLoanData(MultipartFile file) {
        try {
            this.publisher.publishEvent(new BulkImportEvent(new XSSFWorkbook(file.getInputStream()), TemplatePopulateImportConstants.LOAN_ENTITY));
        }  catch (Exception e){
            throw new RuntimeException("Error while importing Loan Data: "+ e.getMessage());
        }
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
        loans.forEach(loan -> {
            loan.approveData(approval);
            var participant = loan.getParticipant();
            if (Boolean.TRUE.equals(approval) && Boolean.FALSE.equals(participant.getIsActive())){
                participant.activateParticipant();
                this.participantRepository.save(participant);
            }
        });
        this.loanRepository.saveAll(loans);
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
