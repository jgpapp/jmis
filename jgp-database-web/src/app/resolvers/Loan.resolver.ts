import { inject } from "@angular/core";
import { ResolveFn } from "@angular/router";
import { LoanService } from "@services/data-management/loan.service";

export const LoanResolver: ResolveFn<Object> = (route, state) => {
    const loanId = route.paramMap.get('id');
    return inject(LoanService).getLoanById(loanId);
}