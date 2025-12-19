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
    public void handleDataUploadedEvent(DataImportDocumentDeletedEvent event) {
        final var document = event.document();
        if (Objects.nonNull(document) && event.deleteAssociatedData()) {
            final var documentImportType = document.getGlobalEntityType();
            switch (documentImportType) {
                case GlobalEntityType.LOAN_IMPORT_TEMPLATE -> {
                    final var loanTransactionIds = this.loanTransactionService.getLoanTransactions(document.getId())
                            .stream()
                            .map(BaseEntity::getId)
                            .toList();
                    this.loanService.approvedTransactionsLoans(loanTransactionIds, Boolean.FALSE);
                }
                case GlobalEntityType.TA_IMPORT_TEMPLATE -> {
                    final var bmoClientDataIds = this.bmoClientDataService.findByDocumentId(document.getId())
                            .stream()
                            .map(BaseEntity::getId)
                            .toList();
                    this.bmoClientDataService.approvedBMOParticipantsData(bmoClientDataIds, Boolean.FALSE);
                }
                case GlobalEntityType.MENTORSHIP_IMPORT_TEMPLATE -> {
                    final var mentorshipIds = this.mentorshipService.findByDocumentId(document.getId())
                            .stream()
                            .map(BaseEntity::getId)
                            .toList();
                    this.mentorshipService.approvedMentorShipData(mentorshipIds, Boolean.FALSE);
                }
                case GlobalEntityType.MONITORING_IMPORT_TEMPLATE -> {
                    final var outComeMonitoringIds = this.outComeMonitoringService.findByDocumentId(document.getId())
                            .stream()
                            .map(BaseEntity::getId)
                            .toList();
                    this.outComeMonitoringService.approvedOutComeMonitoringData(outComeMonitoringIds, Boolean.FALSE);
                }
                default -> {
                    return;
                }
            }
            log.info("Data deleted successfully");
        }
    }
}
