package com.jgp.infrastructure.bulkimport.listener;

import com.jgp.dashboard.service.DataSummaryService;
import com.jgp.infrastructure.bulkimport.event.DataApprovedEvent;
import com.jgp.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.time.temporal.TemporalAdjusters;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Slf4j
public class DataApprovedEventListener {

    private final DataSummaryService countySummaryService;

    @EventListener
    @Async
    public void handleDataApprovedEvent(DataApprovedEvent dataApprovedEvent){
        if (Objects.isNull(dataApprovedEvent.dates()) || dataApprovedEvent.dates().isEmpty()){
            return;
        }
        final var dateRange = CommonUtil.getMinMaxDates(dataApprovedEvent.dates());

        for (var partnerId: dataApprovedEvent.partnerIds()){
            this.countySummaryService.updateDataSummary(
                    dateRange.getLeft().with(TemporalAdjusters.firstDayOfMonth()),
                    dateRange.getRight().with(TemporalAdjusters.lastDayOfMonth()),
                    partnerId
            );
        }

    }
}
