package com.jgp.authentication.api;

import com.jgp.authentication.domain.UserAuditLog;
import com.jgp.authentication.domain.UserAuditOperationConstants;
import com.jgp.authentication.dto.UserAuditLogSearchCriteria;
import com.jgp.authentication.service.UserAuditLogService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDateTime;

@RestController
@RequestMapping("api/v1/user-audit-logs")
@RequiredArgsConstructor
public class UserAuditLogController {

    private  final UserAuditLogService userAuditLogService;

    @GetMapping
    public ResponseEntity<Page<UserAuditLog>> findAvailableUserAuditLogs(@RequestParam(value = "user-name", required = false) String userName,
                                                                         @RequestParam(value = "action", required = false) String action,
                                                                         @RequestParam(value = "from-date", required = false) LocalDateTime fromDate,
                                                                         @RequestParam(value = "to-date", required = false) LocalDateTime toDate,
                                                                         @RequestParam(value = "pageNumber", defaultValue = "0") int pageNumber,
                                                                         @RequestParam(value = "pageSize", defaultValue = "10") int pageSize) {
        final var sortedByLogTime =
                PageRequest.of(pageNumber, pageSize, Sort.by("logTime").descending());
        return new ResponseEntity<>(this.userAuditLogService.findAvailableUserAuditLogs(new UserAuditLogSearchCriteria(userName, action, fromDate, toDate), sortedByLogTime), HttpStatus.OK);
    }

    @GetMapping("/auditable-operations")
    public ResponseEntity<String> findAvailableUserAuditOperations() {
        return new ResponseEntity<>(UserAuditOperationConstants.getOperationsJsonList(), HttpStatus.OK);
    }


    /**
     * Endpoint to manually log an Excel upload activity.
     * This demonstrates how to log a specific action that isn't handled by the AOP aspect.
     * @param userName The ID of the user performing the upload.
     * @param details A detailed description of the upload.
     */
    @PostMapping("/excel-upload")
    public void logExcelUpload(@RequestParam String userName, @RequestParam String details) {
        userAuditLogService.logUserAction(userName, "EXCEL_UPLOAD", null, details);
    }
}
