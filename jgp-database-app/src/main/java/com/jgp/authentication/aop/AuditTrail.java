package com.jgp.authentication.aop;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface AuditTrail {
    String operation(); // e.g. "CREATE", "UPDATE", "DELETE"
    int bodyIndex() default 0; // index of argument to log
    int entityIdIndex() default 99 ; // index of entity id to log
}
