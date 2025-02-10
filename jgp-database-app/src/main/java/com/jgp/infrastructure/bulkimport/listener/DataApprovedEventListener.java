package com.jgp.infrastructure.bulkimport.listener;

import com.jgp.dashboard.service.CountySummaryService;
import com.jgp.infrastructure.bulkimport.event.DataApprovedEvent;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Slf4j
public class DataApprovedEventListener {

    private final CountySummaryService countySummaryService;

    @EventListener
    @Async
    public void handleBulkImportEvent(DataApprovedEvent dataApprovedEvent){
        if (Objects.isNull(dataApprovedEvent.dates()) || dataApprovedEvent.dates().isEmpty()){
            return;
        }
        var fromDateOptional = dataApprovedEvent.dates().stream().findFirst();
        var fromDate = fromDateOptional.orElse(LocalDate.now(ZoneId.systemDefault()).plusYears(10));
        var toDate = fromDate;

        for (LocalDate localDate: dataApprovedEvent.dates()){
            if (localDate.isBefore(fromDate)){
                fromDate = localDate;
            }
            if (localDate.isAfter(toDate)){
                toDate = localDate;
            }
        }
        this.countySummaryService.updateCountySummary(fromDate, toDate, dataApprovedEvent.partnerId());

    }
}
