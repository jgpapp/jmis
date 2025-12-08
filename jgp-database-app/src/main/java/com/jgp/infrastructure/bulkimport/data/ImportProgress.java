package com.jgp.infrastructure.bulkimport.data;

public class ImportProgress {

    private int processed;
    private int total;

    public synchronized void incrementProcessed() {
        this.processed++;
    }

    public synchronized int getProcessed() {
        return processed;
    }

    public synchronized int getTotal() {
        return total;
    }

    public synchronized void reset() {
        this.processed = 0;
        this.total = 0;
    }

    public synchronized void setTotal(int total) {
        this.total = total;
    }
}
