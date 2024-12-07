package com.jgp.infrastructure.bulkimport.service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.jgp.infrastructure.bulkimport.data.ImportProgress;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

@Service
@Slf4j
@RequiredArgsConstructor
public class ImportProgressServiceImpl implements ImportProgressService {

    private final LoadingCache<Long, ImportProgress> requestCountsPerIdIdempotencyCache = CacheBuilder.newBuilder()
            .expireAfterAccess(7, TimeUnit.DAYS).build(new CacheLoader<>() {
                @Override
                public ImportProgress load(Long importId) {
                    return new ImportProgress(0, 0);
                }
            });


    @Override
    public void updateTotal(Long importId, int total) {
        requestCountsPerIdIdempotencyCache.put(importId, new ImportProgress(0, total));
    }

    @Override
    public void updateImportDocumentIdProgress(Long importId) throws ExecutionException {
        var progress = this.getImportProgress(importId);
        progress.incrementProcessed();
    }

    @Override
    public ImportProgress getImportProgress(Long importId) throws ExecutionException {
        return requestCountsPerIdIdempotencyCache.get(importId);
    }
}
