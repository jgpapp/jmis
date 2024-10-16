package com.jgp.authentication.api;

import com.jgp.authentication.dto.PermissionDto;
import com.jgp.authentication.service.PermissionService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collection;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/permissions")
public class PermissionController {

    private final PermissionService permissionService;
    @GetMapping
    public ResponseEntity<Collection<PermissionDto>> getAvailablePermissions(){
        return new ResponseEntity<>(this.permissionService.retrieveAllPermissions(), HttpStatus.OK);
    }
}
