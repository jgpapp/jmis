import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { User } from '../../common/models/user.model';

@Injectable({
  providedIn: 'root'
})
export class UserService {

  constructor(private httpClient: HttpClient) { }

  getAvailableUsers(): Observable<any> {
    return this.httpClient.get(`/users`);
  }

  getOnlineUsersCount(): Observable<any> {
    return this.httpClient.get(`/actuator/metrics/app.active.users`);
  }

  getSystemUserLoginSummary(dashBoardFilters: any): Observable<any> {
    let params = new HttpParams();
    if(dashBoardFilters && dashBoardFilters.selectedDateFrom && dashBoardFilters.selectedDateTo){
      params = new HttpParams()
        .set('from-date', dashBoardFilters.selectedDateFrom)
        .set('to-date', dashBoardFilters.selectedDateTo);
    }
    return this.httpClient.get(`/reports/system-user-login-summary`, { params });
  }

  createUser(user: User): Observable<any> {
    return this.httpClient.post(`/users`, JSON.stringify(user));
  }

  updateUser(userId: number, user: User): Observable<any> {
    return this.httpClient.put(`/users/${userId}`, JSON.stringify(user));
  }

  lockOrUnlockUser(selectedUser: any): Observable<any> {
    const newUserStatus = selectedUser.isActive ? 'INACTIVE' : 'ACTIVE';
    return this.httpClient.put(`/users/${selectedUser.id}/change-user-status/${newUserStatus}`, '');
  }

  updateUserPassword(userPassDto: {password: string, newPass: string, passConfirm: string}): Observable<any> {
    return this.httpClient.put(`/users/change-password`, JSON.stringify(userPassDto));
  }

  resetUserPassword(userId: number): Observable<any> {
    return this.httpClient.put(`/users/reset-user-password/${userId}`, JSON.stringify({}));
  }

  getUserById(userId: number | string | null): Observable<any> {
    return this.httpClient.get(`/users/${userId}`);
  }

  getUserAuditLogs(userName: string | undefined, action: string | undefined, fromDate: any, toDate: any, page: number, size: number): Observable<any> {
        let params = new HttpParams()
        .set('pageNumber', page.toString())
        .set('pageSize', size.toString());
        if(userName){
            params = params.set('user-name', userName.toString());
          }
          if(action){
            params = params.set('action', action.toString());
          }
          if(fromDate){
            params = params.set('from-date', fromDate.format("YYYY-MM-DD") + 'T00:00:00');
          }
          if(toDate){
            params = params.set('to-date', toDate.format("YYYY-MM-DD") + 'T23:59:59');
          }
        return this.httpClient.get(`/user-audit-logs`, { params });
      }

  getUserAuditableOperations(): Observable<any> {
    return this.httpClient.get(`/user-audit-logs/auditable-operations`);
  }
}
