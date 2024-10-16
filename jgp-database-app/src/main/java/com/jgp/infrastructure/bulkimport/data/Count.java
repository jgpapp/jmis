package com.jgp.infrastructure.bulkimport.data;

import lombok.Getter;

@Getter
public final class Count {

    private Integer successCount;
    private Integer errorCount;

    public static Count instance(final Integer successCount, final Integer errorCount) {
        return new Count(successCount, errorCount);
    }

    private Count(final Integer successCount, final Integer errorCount) {
        this.successCount = successCount;
        this.errorCount = errorCount;
    }

}
