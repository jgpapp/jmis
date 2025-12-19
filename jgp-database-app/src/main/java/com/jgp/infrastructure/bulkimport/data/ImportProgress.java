package com.jgp.infrastructure.bulkimport.data;


public class ImportProgress {

    private String step;
    private int processed;
    private int total;

    public synchronized void updateProgressStep(String step) {
        this.step = step;
    }

    public synchronized void incrementProcessed() {
        this.processed++;
    }

    public synchronized void reset() {
        this.processed = 0;
    }

    public synchronized void setTotal(int total) {
        this.total = total;
    }
}
