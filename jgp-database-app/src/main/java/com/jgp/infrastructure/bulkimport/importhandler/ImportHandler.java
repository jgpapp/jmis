package com.jgp.infrastructure.bulkimport.importhandler;

import com.jgp.authentication.service.UserService;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.data.ExcelTemplateProcessingResult;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;
import com.jgp.infrastructure.bulkimport.service.ImportProgressService;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.slf4j.Logger;

import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

public interface ImportHandler {

    String PARTICIPANT_ASSOCIATION_ERROR = "Cannot associate data to a participant!";
    String PARTNER_ASSOCIATION_ERROR = "Cannot associate data to a partner!";
    int CHUNK_SIZE = 100; // Default chunk size for parallel processing
    int BULK_SIZE_FOR_PROGRESS_UPDATE = 500; // Default bulk size for batch operations
    ExecutorService IMPORT_EXECUTOR =
            Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

    /**
     * Process the bulk import event asynchronously
     */
    CompletableFuture<Count> process(BulkImportEvent bulkImportEvent);

    /**
     * Writes error message to the specified cells in the workbook
     */
    default void writeGroupErrorMessage(String errorMessage, Workbook workbook, Cell statusCell, Cell errorReportCell) {
        String status = TemplatePopulateImportConstants.STATUS_CREATION_FAILED;
        statusCell.setCellValue(status);
        statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(workbook, IndexedColors.RED));
        errorReportCell.setCellValue(errorMessage);
    }

    /**
     * Sets the report headers in the specified sheet
     */
    default void setReportHeaders(Sheet currentSheet, int statusColIndex, int failureColIndex) {
        Row headerRow = currentSheet.getRow(0);
        if (headerRow == null) {
            headerRow = currentSheet.createRow(0);
        }

        Cell statusHeaderCell = headerRow.createCell(statusColIndex);
        statusHeaderCell.setCellValue("Status");
        statusHeaderCell.setCellStyle(ImportHandlerUtils.getCellStyle(currentSheet.getWorkbook(), IndexedColors.LIGHT_BLUE));

        Cell failureHeaderCell = headerRow.createCell(failureColIndex);
        failureHeaderCell.setCellValue("Failure Report");
        failureHeaderCell.setCellStyle(ImportHandlerUtils.getCellStyle(currentSheet.getWorkbook(), IndexedColors.LIGHT_BLUE));
    }

    /**
     * Writes processing result to workbook (must be called sequentially, not thread-safe)
     */
    default void writeResultToWorkbook(ExcelTemplateProcessingResult result, int statusColIndex, int failureColIndex) {
        Row row = result.row();
        Cell errorReportCell = row.createCell(failureColIndex);
        Cell statusCell = row.createCell(statusColIndex);

        if (result.success()) {
            statusCell.setCellValue(TemplatePopulateImportConstants.STATUS_CELL_IMPORTED);
            statusCell.setCellStyle(ImportHandlerUtils.getCellStyle(row.getSheet().getWorkbook(), IndexedColors.LIGHT_GREEN));
        } else {
            writeGroupErrorMessage(result.errorMessage(), row.getSheet().getWorkbook(), statusCell, errorReportCell);
        }
    }

    /**
     * Sleeps the current thread for a short duration to avoid tight loops
     */
    default void sleep(Logger log) {
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.warn("Sleep interrupted", e);
        }
    }

    /**
     * Normalizes TA needs by trimming whitespace around comma-separated values
     */
    default String normalizeStringValues(String stringValues) {
        return Objects.nonNull(stringValues)
                ? Arrays.stream(stringValues.split(","))
                .map(String::trim)
                .filter(s -> !s.isEmpty())
                .collect(Collectors.joining(","))
                : null;
    }


    /**
     * Updates import progress in bulk to reduce overhead
     */
    default void updateProgressInBulk(ImportProgressService importProgressService, String documentImportProgressUUId, int processedCount) {
        if (processedCount % BULK_SIZE_FOR_PROGRESS_UPDATE == 0) {
            importProgressService.sendProgressUpdate(documentImportProgressUUId, processedCount);
        }
    }

    default Long getCurrentPartnerId(UserService userService) {
        final var currentUser = userService.currentUser();
        if (Objects.nonNull(currentUser) && Objects.nonNull(currentUser.getPartner())) {
            return currentUser.getPartner().getId();
        }
        return null;
    }

}
