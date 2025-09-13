package com.jgp.authentication.service;

public interface ActiveUserService {

    /**
     * Records activity for a user, updating their last active timestamp.
     *
     * @param username the username of the user
     */
    void userActivity(String username);

    /**
     * Removes users who have been inactive for longer than the specified expiration time.
     *
     * @param expirationMillis the expiration time in milliseconds
     */
    void removeExpiredUsers(long expirationMillis);

    /**
     * Gets the count of currently active users.
     *
     * @return the number of active users
     */
    int getActiveUserCount();
}
