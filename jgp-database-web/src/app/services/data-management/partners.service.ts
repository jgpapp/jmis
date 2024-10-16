import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { GlobalService } from '../shared/global.service';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';
import { PartnerDto } from '../../dto/Partner';

@Injectable({
  providedIn: 'root'
})
export class PartnerService {

    constructor(private httpClient: HttpClient, private globalService: GlobalService, private router: Router) { }


    createPartner(partner: PartnerDto): Observable<any> {
        return this.httpClient.post(`${this.globalService.BASE_API_URL}/partners`, JSON.stringify(partner));
    }

    updatePartner(partnerId: number, partner: PartnerDto): Observable<any> {
      return this.httpClient.put(`${this.globalService.BASE_API_URL}/partners/${partnerId}`, JSON.stringify(partner));
    }

    getAvailablePartners(): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/partners`);
    }

    getPartnerById(partnerId: number | string | null): Observable<any> {
      return this.httpClient.get(`${this.globalService.BASE_API_URL}/partners/${partnerId}`);
    }
}