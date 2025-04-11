package com.jgp.dashboard.service;

import java.time.LocalDate;

public interface CountySummaryService {

    void updateCountySummary(LocalDate fromDate, LocalDate toDate, Long partnerId);
}
