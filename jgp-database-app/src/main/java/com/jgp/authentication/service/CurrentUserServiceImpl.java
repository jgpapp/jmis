package com.jgp.authentication.service;

import com.jgp.authentication.domain.AppUserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.HashSet;

@Service
@RequiredArgsConstructor
public class CurrentUserServiceImpl implements UserDetailsService {

    private final AppUserRepository userRepository;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException{
        final var appUser = userRepository.findByUsername(username)
                .orElseThrow(() -> new UsernameNotFoundException("Bad credentials"));
        final var grantedAuthorities = new HashSet<GrantedAuthority>(Collections.singleton(new SimpleGrantedAuthority("USER")));
        return new org.springframework.security.core.userdetails.User(appUser.getUsername(), appUser.getPassword(), grantedAuthorities);
    }


}
