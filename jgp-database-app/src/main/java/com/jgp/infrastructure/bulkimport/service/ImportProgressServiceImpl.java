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

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

@Service
@Slf4j
@RequiredArgsConstructor
public class ImportProgressServiceImpl implements ImportProgressService {


    private final SimpMessagingTemplate simpMessagingTemplate;
    private final ObjectMapper objectMapper;

    private final LoadingCache<Long, ImportProgress> requestCountsPerIdIdempotencyCache = CacheBuilder.newBuilder()
            .expireAfterAccess(7, TimeUnit.DAYS).build(new CacheLoader<>() {
                @Override
                public ImportProgress load(Long importId) {
                    return new ImportProgress();
                }
            });


    @Override
    public void updateTotal(Long importId, int total) throws ExecutionException{
        var newProgress = this.getImportProgress(importId);
        newProgress.setTotal(total);
    }

    @Override
    public void incrementProcessedProgress(Long importId) throws ExecutionException {
        var progress = this.getImportProgress(importId);
        progress.incrementProcessed();
    }

    @Override
    public void markImportAsFinished(Long importId) throws ExecutionException {
        var progress = this.getImportProgress(importId);
        progress.setProgressAsFinished(1);
    }

    @Override
    public ImportProgress getImportProgress(Long importId) throws ExecutionException {
        return requestCountsPerIdIdempotencyCache.get(importId);
    }

    @Override
    public void sendProgressUpdate(Long importId) throws ExecutionException, JsonProcessingException {
        // Send progress to the WebSocket
        var progress = this.getImportProgress(importId);
        log.info(this.objectMapper.writeValueAsString(progress));
        simpMessagingTemplate.convertAndSend(String.format("/topic/progress/%d", importId), this.objectMapper.writeValueAsString(progress));
        simpMessagingTemplate.convertAndSend("/topic/progress", this.objectMapper.writeValueAsString(progress));
    }
}
