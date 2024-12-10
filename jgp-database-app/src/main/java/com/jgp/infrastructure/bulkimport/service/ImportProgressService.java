package com.jgp.infrastructure.bulkimport.service;

import com.jgp.infrastructure.bulkimport.data.ImportProgress;

import java.util.concurrent.ExecutionException;

public interface ImportProgressService {

    void updateTotal(Long importId, int total) throws ExecutionException;

    void incrementProcessedProgress(Long importId) throws  ExecutionException;

    void markImportAsFinished(Long importId) throws  ExecutionException;

    ImportProgress getImportProgress(Long importId) throws  ExecutionException;
}
