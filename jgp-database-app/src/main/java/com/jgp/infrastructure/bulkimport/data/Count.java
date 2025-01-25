package com.jgp.infrastructure.bulkimport.data;

import lombok.Getter;

@Getter
public final class Count {

    private final Integer successCount;
    private final Integer errorCount;
    private final Integer totalCount;

    public static Count instance(final Integer totalCount, final Integer successCount, final Integer errorCount) {
        return new Count(totalCount, successCount, errorCount);
    }

    private Count(final Integer totalCount, final Integer successCount, final Integer errorCount) {
        this.successCount = successCount;
        this.errorCount = errorCount;
        this.totalCount = totalCount;
    }

}
