package com.jgp.authentication.dto;

import java.util.List;

public record AppUserSearchCriteria(
        Long userId,
        List<Long> userIds,
        Long partnerId
) {
}
