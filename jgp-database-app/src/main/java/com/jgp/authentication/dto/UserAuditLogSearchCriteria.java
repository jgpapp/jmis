package com.jgp.authentication.dto;

import java.time.LocalDateTime;

/**
 * DTO for searching user audit logs based on criteria.
 *
 * @param userName    the username to filter by (optional)
 * @param action      the action to filter by (optional)
 * @param logTimeFrom the start of the log time range (optional)
 * @param logTimeTo   the end of the log time range (optional)
 */
public record UserAuditLogSearchCriteria(String userName, String action, LocalDateTime logTimeFrom, LocalDateTime logTimeTo) {
}
