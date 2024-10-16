import { inject } from "@angular/core";
import { ResolveFn } from "@angular/router";
import { PartnerService } from "@services/data-management/partners.service";

export const PartnerResolver: ResolveFn<Object> = (route, state) => {
    const partnerId = route.paramMap.get('id');
    return inject(PartnerService).getPartnerById(partnerId)
}