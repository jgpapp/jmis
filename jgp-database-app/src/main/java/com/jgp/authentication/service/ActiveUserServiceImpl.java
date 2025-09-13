package com.jgp.authentication.service;

import org.springframework.stereotype.Service;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

@Service
public class ActiveUserServiceImpl implements ActiveUserService {

    private final ConcurrentMap<String, Long> activeUsers = new ConcurrentHashMap<>();

    @Override
    public void userActivity(String username) {
        activeUsers.put(username, System.currentTimeMillis());
    }

    @Override
    public void removeExpiredUsers(long expirationMillis) {
        long now = System.currentTimeMillis();
        activeUsers.entrySet().removeIf(entry -> now - entry.getValue() > expirationMillis);
    }

    @Override
    public int getActiveUserCount() {
        return activeUsers.size();
    }
}
