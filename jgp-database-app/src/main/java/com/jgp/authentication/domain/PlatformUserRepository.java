package com.jgp.authentication.domain;

public interface PlatformUserRepository {
    PlatformUser findByUsernameAndIsActive(String username, boolean isActive);
}
