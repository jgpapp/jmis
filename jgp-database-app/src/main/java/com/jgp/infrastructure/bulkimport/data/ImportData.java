
package com.jgp.infrastructure.bulkimport.data;

import java.time.LocalDateTime;

public record ImportData(
        Long importId,
        Long documentId,
        String name,
        LocalDateTime importTime,
        LocalDateTime endTime,
        Boolean completed,
        Long createdBy,
        Integer totalRecords,
        Integer successCount,
        Integer failureCount){}

