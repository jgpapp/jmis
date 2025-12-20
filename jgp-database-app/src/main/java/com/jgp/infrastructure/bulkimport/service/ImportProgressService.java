package com.jgp.infrastructure.bulkimport.service;

import com.jgp.infrastructure.bulkimport.data.ImportProgress;

public interface ImportProgressService {

    void updateStepAndSendProgress(String importUUId, String step);

    void updateTotal(String importUUId, int total);

    void resetEveryThingToZero(String importUUId);

    ImportProgress getImportProgress(String importUUId);

    void sendProgressUpdate(String importUUId);

    void incrementAndSendProgressUpdate(String importUUId);
}
