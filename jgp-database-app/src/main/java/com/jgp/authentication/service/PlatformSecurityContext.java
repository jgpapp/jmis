package com.jgp.authentication.service;

import com.jgp.authentication.domain.AppUser;

public interface PlatformSecurityContext {

    AppUser getAuthenticatedUserIfPresent();

    boolean doesPasswordHasToBeRenewed(AppUser currentUser);


}
