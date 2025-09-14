package com.jgp.authentication.aop;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jgp.authentication.event.UserAuditEvent;
import com.jgp.authentication.service.UserService;
import com.jgp.shared.domain.BaseEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

@Aspect
@Component
@RequiredArgsConstructor
@Slf4j
public class UserAuditAspect {

    private final ObjectMapper objectMapper;
    private final UserService userService;
    private final ApplicationEventPublisher publisher;
    private static final String WITH_ID_LITERAL = " entity with ID: ";
    private static final String DELETE_OPERATION_LITERAL = "DELETE";
    private static final String CREATE_OPERATION_LITERAL = "CREATE";


    @Around("@annotation(auditTrail)")
    public Object logAudit(ProceedingJoinPoint joinPoint, AuditTrail auditTrail) throws Throwable {
        Object result = joinPoint.proceed();

        Object[] args = joinPoint.getArgs();
        Object arg = (auditTrail.bodyIndex() < args.length) ? args[auditTrail.bodyIndex()] : null;
        String details;
        Long resourceId = (auditTrail.entityIdIndex() < 99 && args[auditTrail.entityIdIndex()] instanceof Long id) ? id : null;
        if (auditTrail.operation().startsWith(DELETE_OPERATION_LITERAL)) {
            details = auditTrail.operation()+WITH_ID_LITERAL + auditTrail.entityIdIndex();
        }else {
            details = objectMapper.writeValueAsString(arg);
        }
        if (result instanceof BaseEntity resource && auditTrail.operation().startsWith(CREATE_OPERATION_LITERAL)) {
            resourceId = resource.getId();
        }
        publisher.publishEvent(new UserAuditEvent(userService.currentUser().getUsername(), auditTrail.operation(), resourceId, details));
        return result;
    }

}
