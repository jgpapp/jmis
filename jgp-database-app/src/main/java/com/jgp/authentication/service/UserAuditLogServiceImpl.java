package com.jgp.authentication.service;

import com.jgp.authentication.domain.UserAuditLog;
import com.jgp.authentication.domain.UserAuditLogRepository;
import com.jgp.authentication.domain.predicate.UserAuditLogPredicateBuilder;
import com.jgp.authentication.dto.UserAuditLogSearchCriteria;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;


@Service
@RequiredArgsConstructor
public class UserAuditLogServiceImpl implements UserAuditLogService {

    private  final UserAuditLogRepository userAuditLogRepository;
    private final UserAuditLogPredicateBuilder userAuditLogPredicateBuilder;

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    @Override
    public void logUserAction(String userName, String action, Long resourceId, String details) {
        this.userAuditLogRepository.save(new UserAuditLog(userName, action, resourceId, details));
    }

    @Override
    public Page<UserAuditLog> findAvailableUserAuditLogs(UserAuditLogSearchCriteria userAuditLogSearchCriteria, Pageable pageable) {
        return this.userAuditLogRepository.findAll(this.userAuditLogPredicateBuilder.buildPredicateForSearchUserLogs(userAuditLogSearchCriteria), pageable);
    }

}
