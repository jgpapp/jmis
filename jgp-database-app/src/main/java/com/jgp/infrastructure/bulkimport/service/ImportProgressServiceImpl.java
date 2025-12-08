package com.jgp.infrastructure.bulkimport.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.jgp.infrastructure.bulkimport.data.ImportProgress;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

@Service
@Slf4j
@RequiredArgsConstructor
public class ImportProgressServiceImpl implements ImportProgressService {


    private final SimpMessagingTemplate simpMessagingTemplate;
    private final ObjectMapper objectMapper;

    private final LoadingCache<String, ImportProgress> requestCountsPerIdIdempotencyCache = CacheBuilder.newBuilder()
            .expireAfterAccess(7, TimeUnit.DAYS).build(new CacheLoader<>() {
                @Override
                public ImportProgress load(String importUUId) {
                    return new ImportProgress();
                }
            });


    @Override
    public void updateTotal(String importUUId, int total) throws ExecutionException{
        var newProgress = this.getImportProgress(importUUId);
        if (Objects.nonNull(newProgress)) {
            newProgress.setTotal(total);
        }
    }

    @Override
    public void incrementProcessedProgress(String importUUId) throws ExecutionException {
        var progress = this.getImportProgress(importUUId);
        if (Objects.nonNull(progress)) {
            progress.incrementProcessed();
        }
    }

    @Override
    public void resetEveryThingToZero(String importUUId) throws ExecutionException {
        var newProgress = this.getImportProgress(importUUId);
        if (Objects.nonNull(newProgress)) {
            newProgress.reset();
        }

    }

    @Override
    public ImportProgress getImportProgress(String importUUId) throws ExecutionException {
        return requestCountsPerIdIdempotencyCache.get(importUUId);
    }

    @Override
    public void sendProgressUpdate(String importUUId) throws ExecutionException, JsonProcessingException {
        // Send progress to the WebSocket
        var progress = this.getImportProgress(importUUId);
        if (Objects.nonNull(progress)) {
            simpMessagingTemplate.convertAndSend(String.format("/topic/progress/%s", importUUId), this.objectMapper.writeValueAsString(progress));
        }
    }

    @Override
    public void incrementAndSendProgressUpdate(String importUUId) throws ExecutionException, JsonProcessingException {
        //Update & Send progress to the WebSocket
        var progress = this.getImportProgress(importUUId);
        if (Objects.nonNull(progress)) {
            progress.incrementProcessed();
            simpMessagingTemplate.convertAndSend(String.format("/topic/progress/%s", importUUId), this.objectMapper.writeValueAsString(progress));
        }
    }
}
