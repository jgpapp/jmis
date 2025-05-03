import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class PermissionsService {

  constructor(private httpClient: HttpClient) { }

  getAvailablePermissions(): Observable<any> {
    return this.httpClient.get(`/permissions`);
  }
}