
package com.jgp.infrastructure.bulkimport.data;

import java.time.LocalDate;

public final class ImportData {

    @SuppressWarnings("unused")
    private Long importId;
    @SuppressWarnings("unused")
    private Long documentId;
    @SuppressWarnings("unused")
    private String name;
    @SuppressWarnings("unused")
    private LocalDate importTime;
    @SuppressWarnings("unused")
    private LocalDate endTime;
    @SuppressWarnings("unused")
    private Boolean completed;
    @SuppressWarnings("unused")
    private Long createdBy;
    @SuppressWarnings("unused")
    private Integer totalRecords;
    @SuppressWarnings("unused")
    private Integer successCount;
    @SuppressWarnings("unused")
    private Integer failureCount;

    public static ImportData instance(final Long importId, final Long documentId, final LocalDate importTime, final LocalDate endTime,
            final Boolean completed, final String name, final Long createdBy, final Integer totalRecords, final Integer successCount,
            final Integer failureCount) {
        return new ImportData(importId, documentId, importTime, endTime, completed, name, createdBy, totalRecords, successCount,
                failureCount);
    }

    public static ImportData instance(final Long importId) {
        return new ImportData(importId, null, null, null, null, null, null, null, null, null);
    }

    private ImportData(final Long importId, final Long documentId, final LocalDate importTime, final LocalDate endTime,
                       final Boolean completed, final String name, final Long createdBy, final Integer totalRecords, final Integer successCount,
                       final Integer failureCount) {
        this.importId = importId;
        this.documentId = documentId;
        this.name = name;
        this.importTime = importTime;
        this.endTime = endTime;
        this.completed = completed;
        this.createdBy = createdBy;
        this.totalRecords = totalRecords;
        this.successCount = successCount;
        this.failureCount = failureCount;
    }

}
