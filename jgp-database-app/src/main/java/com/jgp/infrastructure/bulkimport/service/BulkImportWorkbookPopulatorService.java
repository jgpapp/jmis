package com.jgp.infrastructure.bulkimport.service;

import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.ResponseEntity;

public interface BulkImportWorkbookPopulatorService {

    ResponseEntity<?> getTemplate(String entityType, HttpServletResponse response);
}
