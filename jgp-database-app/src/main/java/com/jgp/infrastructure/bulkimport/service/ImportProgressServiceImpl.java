package com.jgp.infrastructure.bulkimport.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.jgp.infrastructure.bulkimport.constants.TemplatePopulateImportConstants;
import com.jgp.infrastructure.bulkimport.data.ImportProgress;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.concurrent.CountDownLatch;
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
    public void updateStepAndSendProgress(String importUUId, String step) {
        try {
            var progress = this.getImportProgress(importUUId);
            if (Objects.nonNull(progress)) {
                progress.updateProgressStep(step);
                sendProgressSync(importUUId, progress);
            }
        } catch (Exception e) {
            log.error("Problem Sending  Progress After Updating Step: {}", e.getMessage());
        }

    }

    @Override
    public void updateTotal(String importUUId, int total) {
        var newProgress = this.getImportProgress(importUUId);
        if (Objects.nonNull(newProgress)) {
            newProgress.setTotal(total);
        }
    }

    @Override
    public void resetEveryThingToZero(String importUUId) {
        var newProgress = this.getImportProgress(importUUId);
        if (Objects.nonNull(newProgress)) {
            newProgress.reset();
        }

    }

    @Override
    public ImportProgress getImportProgress(String importUUId) {
        try {
            return requestCountsPerIdIdempotencyCache.get(importUUId);
        } catch (ExecutionException e) {
            log.error("Problem getting  Progress: {}", e.getMessage());
        }
        return null;
    }

    @Override
    public void sendProgressUpdate(String importUUId) {
        // Send progress to the WebSocket
        try {
            var progress = this.getImportProgress(importUUId);
            if (Objects.nonNull(progress)) {
                sendProgressSync(importUUId, progress);
            }
        } catch (Exception e) {
            log.error("Problem Sending  Progress: {}", e.getMessage());
        }
    }

    @Override
    public void incrementAndSendProgressUpdate(String importUUId) {
        //Update & Send progress to the WebSocket
        try {
            var progress = this.getImportProgress(importUUId);
            if (Objects.nonNull(progress)) {
                progress.incrementProcessed();
                sendProgressSync(importUUId, progress);
            }
        } catch (Exception e) {
            log.error("Problem Updating & Sending Progress: {}", e.getMessage());
        }

    }

    /**
     * Sends the current progress synchronously, ensuring the message is sent before returning.
     *
     * @param importUUId      the unique identifier for the import process
     * @param currentProgress the current progress to be sent
     * @throws Exception if there is an error during message sending
     */
    public void sendProgressSync(String importUUId, ImportProgress currentProgress) throws Exception {
        // Use CountDownLatch to ensure message is sent
        CountDownLatch latch = new CountDownLatch(1);

        simpMessagingTemplate.convertAndSend(String.format(TemplatePopulateImportConstants.WEB_SOCKET_EXCEL_UPLOAD_PROGRESS_ENDPOINT, importUUId), this.objectMapper.writeValueAsString(currentProgress),
                headers -> {
                    latch.countDown();
                    return headers;
                });

        try {
            latch.await(1, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.warn("Progress update wait interrupted", e);
        }
    }
}
