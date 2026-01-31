package com.jgp.infrastructure.bulkimport.listener;

import com.jgp.bmo.service.TADataService;
import com.jgp.bmo.service.MentorshipService;
import com.jgp.finance.service.LoanService;
import com.jgp.finance.service.LoanTransactionService;
import com.jgp.infrastructure.bulkimport.data.GlobalEntityType;
import com.jgp.infrastructure.bulkimport.event.DataImportDocumentDeletedEvent;
import com.jgp.monitoring.service.OutComeMonitoringService;
import com.jgp.shared.domain.BaseEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@RequiredArgsConstructor
@Slf4j
public class DataImportDocumentDeletedEventListener {

    private final LoanService loanService;
    private final LoanTransactionService loanTransactionService;
    private final TADataService bmoClientDataService;
    private final MentorshipService mentorshipService;
    private final OutComeMonitoringService outComeMonitoringService;

    @EventListener
    @Async
    public void handleDataImportDocumentDeletedEvent(DataImportDocumentDeletedEvent event) {
        final var document = event.document();
        if (Objects.nonNull(document) && event.deleteAssociatedData()) {
            final var documentImportType = document.getGlobalEntityType();
            if(Objects.isNull(documentImportType)){
                log.info("Document Import Type is null, skipping associated data deletion");
                return;
            }
            switch (documentImportType) {
                case GlobalEntityType.LOAN_IMPORT_TEMPLATE -> {
                    final var loanTransactionIds = this.loanTransactionService.getLoanTransactions(document.getId())
                            .stream()
                            .map(BaseEntity::getId)
                            .toList();
                    this.loanService.deleteTransactionsLoans(loanTransactionIds);
                }
                case GlobalEntityType.TA_IMPORT_TEMPLATE -> {
                    final var bmoClientDataIds = this.bmoClientDataService.findByDocumentId(document.getId())
                            .stream()
                            .map(BaseEntity::getId)
                            .toList();
                    this.bmoClientDataService.deleteBMOParticipantsDataByIds(bmoClientDataIds);
                }
                case GlobalEntityType.MENTORSHIP_IMPORT_TEMPLATE -> {
                    final var mentorshipIds = this.mentorshipService.findByDocumentId(document.getId())
                            .stream()
                            .map(BaseEntity::getId)
                            .toList();
                    this.mentorshipService.deleteMentorShipDataByIds(mentorshipIds);
                }
                case GlobalEntityType.MONITORING_IMPORT_TEMPLATE -> {
                    final var outComeMonitoringIds = this.outComeMonitoringService.findByDocumentId(document.getId())
                            .stream()
                            .map(BaseEntity::getId)
                            .toList();
                    this.outComeMonitoringService.deleteOutComeMonitoringDataByIds(outComeMonitoringIds);
                }
                default -> {
                    return;
                }
            }
            log.info("Data deleted successfully");
        }
    }
}
