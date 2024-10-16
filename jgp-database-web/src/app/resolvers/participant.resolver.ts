import { inject } from "@angular/core";
import { ResolveFn } from "@angular/router";
import { ClientService } from "@services/data-management/clients.service";

export const ParticipantResolver: ResolveFn<Object> = (route, state) => {
    const participantId = route.paramMap.get('id');
    return inject(ClientService).getParticipantById(participantId)
}