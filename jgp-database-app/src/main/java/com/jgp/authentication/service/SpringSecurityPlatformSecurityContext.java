package com.jgp.authentication.service;

import com.jgp.authentication.domain.AppUser;
import com.jgp.authentication.exception.ResetPasswordException;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.time.LocalDate;

@Service
@RequiredArgsConstructor
public class SpringSecurityPlatformSecurityContext implements PlatformSecurityContext {

    private final UserService userService;

    @Override
    public AppUser getAuthenticatedUserIfPresent() {
        AppUser currentUser = getCurrentUserFromContext();

        if (currentUser == null) {
            return null;
        }

        if (this.doesPasswordHasToBeRenewed(currentUser)) {
            throw new ResetPasswordException(currentUser.getId());
        }

        return currentUser;
    }

    @Override
    public boolean doesPasswordHasToBeRenewed(AppUser currentUser) {
        return currentUser.isForceChangePass();
    }

    /**
     * Get current user from the security context
     *
     * @return {@link AppUser}
     */
    private AppUser getCurrentUserFromContext() {
        return userService.currentUser();
    }
}
