package com.jgp.infrastructure.bulkimport.importhandler;

import com.jgp.infrastructure.bulkimport.data.Count;
import com.jgp.infrastructure.bulkimport.event.BulkImportEvent;

public interface ImportHandler {

    Count process(BulkImportEvent bulkImportEvent);

    void updateImportProgress(Long importId, boolean updateTotal, int total);

    void markImportAsFinished(Long importId);
}
