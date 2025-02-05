package com.jgp.dashboard.service;

import com.jgp.dashboard.dto.CountySummaryDto;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;

@Service
@Slf4j
@RequiredArgsConstructor
public class CountySummaryServiceImpl implements CountySummaryService {


    private final NamedParameterJdbcTemplate namedParameterJdbcTemplate;

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public void updateCountySummary(LocalDate fromDate, LocalDate toDate, Long partnerId) {
        log.info("Optimization coming !!!!!!!");
    }

    @Override
    public List<CountySummaryDto> getCountySummary(LocalDate fromDate, LocalDate toDate, Long partnerId) {
            return List.of();
    }

}
