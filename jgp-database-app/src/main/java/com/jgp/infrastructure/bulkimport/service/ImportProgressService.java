package com.jgp.infrastructure.bulkimport.service;

import com.jgp.infrastructure.bulkimport.data.ImportProgress;

public interface ImportProgressService {

    void updateImportDocumentIdProgress(Long importId, int total) throws  java.util.concurrent.ExecutionException;

    ImportProgress getImportProgress(Long importId) throws  java.util.concurrent.ExecutionException;
}
