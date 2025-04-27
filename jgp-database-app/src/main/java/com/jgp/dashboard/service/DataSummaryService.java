package com.jgp.dashboard.service;

import java.time.LocalDate;

public interface DataSummaryService {

    void updateDataSummary(LocalDate fromDate, LocalDate toDate, Long partnerId);
}
