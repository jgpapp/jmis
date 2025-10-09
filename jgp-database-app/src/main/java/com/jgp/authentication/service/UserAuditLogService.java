package com.jgp.authentication.service;

import com.jgp.authentication.domain.UserAuditLog;
import com.jgp.authentication.dto.UserAuditLogSearchCriteria;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * Service interface for managing user audit logs.
 */
public interface UserAuditLogService {

    void logUserAction(String userName, String action, Long resourceId, String details);

    void logUserLogin(String username, String ipAddress) throws DataIntegrityViolationException;

    Page<UserAuditLog> findAvailableUserAuditLogs(UserAuditLogSearchCriteria userAuditLogSearchCriteria, Pageable pageable);
}
