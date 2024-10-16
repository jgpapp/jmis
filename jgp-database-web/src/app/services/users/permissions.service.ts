import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { GlobalService } from '../shared/global.service';
import { Observable } from 'rxjs';
import { UserDto } from '../../dto/UserDto';
import { UserRoleDto } from '../../dto/UserRoleDto';

@Injectable({
  providedIn: 'root'
})
export class PermissionsService {

  constructor(private httpClient: HttpClient, private globalService: GlobalService) { }

  getAvailablePermissions(): Observable<any> {
    return this.httpClient.get(`${this.globalService.BASE_API_URL}/permissions`);
  }
}