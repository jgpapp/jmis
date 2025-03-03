package com.jgp.dashboard.dto;

public record EmployeesSummaryDto(
    int totalRegularEmployeesAbove35,
    int youthRegularEmployees,
    int totalCasualEmployeesAbove35,
    int youthCasualEmployees

) {
}
