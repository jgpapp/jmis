package com.jgp.dashboard.service;

import com.jgp.dashboard.dto.CountySummaryDto;

import java.time.LocalDate;
import java.util.List;

public interface CountySummaryService {

    void updateCountySummary(LocalDate fromDate, LocalDate toDate, Long partnerId);

    List<CountySummaryDto> getCountySummary(LocalDate fromDate, LocalDate toDate, Long partnerId);
}
