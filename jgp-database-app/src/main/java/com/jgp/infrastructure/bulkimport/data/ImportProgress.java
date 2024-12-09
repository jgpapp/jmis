package com.jgp.infrastructure.bulkimport.data;

public class ImportProgress {

    private int processed;
    private int total;
    private int finished;

    public ImportProgress() {
        this.total = 1;
    }

    public synchronized void incrementProcessed() {
        this.processed++;
    }

    public synchronized int getProcessed() {
        return processed;
    }

    public synchronized int getTotal() {
        return total;
    }

    public synchronized void setTotal(int total) {
        this.total = total;
    }

    public synchronized int isFinished() {
        return finished;
    }

    public synchronized void setProgressAsFinished(int end) {
        this.finished = end;
    }
}
