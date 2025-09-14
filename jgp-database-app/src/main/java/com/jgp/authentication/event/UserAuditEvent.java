package com.jgp.authentication.event;

import java.io.Serializable;

public record UserAuditEvent(String username, String operation, Long resourceId, String jsonDetails) implements Serializable {
}
