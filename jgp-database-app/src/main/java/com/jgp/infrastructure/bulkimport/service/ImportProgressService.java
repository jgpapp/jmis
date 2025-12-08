package com.jgp.infrastructure.bulkimport.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.jgp.infrastructure.bulkimport.data.ImportProgress;

import java.util.concurrent.ExecutionException;

public interface ImportProgressService {

    void updateTotal(String importUUId, int total) throws ExecutionException;

    void incrementProcessedProgress(String importUUId) throws  ExecutionException;

    void resetEveryThingToZero(String importUUId) throws  ExecutionException;

    ImportProgress getImportProgress(String importUUId) throws  ExecutionException;

    void sendProgressUpdate(String importUUId) throws ExecutionException, JsonProcessingException;

    void incrementAndSendProgressUpdate(String importUUId) throws ExecutionException, JsonProcessingException;
}
