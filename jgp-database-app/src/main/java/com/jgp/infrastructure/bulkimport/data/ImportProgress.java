package com.jgp.infrastructure.bulkimport.data;


public class ImportProgress {

    private String step;
    private int processed;
    private int total;

    public synchronized void updateProgressStep(String step) {
        this.step = step;
    }

    public synchronized void setProcessed(int processed) {
        this.processed = processed;
    }

    public synchronized void reset() {
        this.processed = 0;
    }

    public synchronized void setTotal(int total) {
        this.total = total;
    }

    // Add these getters for Jackson serialization
    public synchronized String getStep() {
        return step;
    }

    public synchronized int getProcessed() {
        return processed;
    }

    public synchronized int getTotal() {
        return total;
    }
}
