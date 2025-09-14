package com.jgp.authentication.listener;

import com.jgp.authentication.event.UserAuditEvent;
import com.jgp.authentication.service.UserAuditLogService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

@Component
@RequiredArgsConstructor
public class UserAuditEventListener {

    private final UserAuditLogService userAuditLogService;

    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onUserAuditEvent(UserAuditEvent event) {
        this.userAuditLogService.logUserAction(event.username(), event.operation(), event.resourceId(), event.jsonDetails());
    }
}
