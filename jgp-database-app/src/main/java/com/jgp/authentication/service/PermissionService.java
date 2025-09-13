package com.jgp.authentication.service;

import com.jgp.authentication.dto.PermissionDto;

import java.util.Collection;

public interface PermissionService {

    Collection<PermissionDto> retrieveAllPermissions();
}
