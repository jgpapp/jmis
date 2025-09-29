import { Component, OnDestroy, OnInit } from '@angular/core';
import { ConfirmDialogModel } from '../../../dto/confirm-dialog-model';
import { ConfirmDialogComponent } from '../../confirm-dialog/confirm-dialog.component';
import { MatDialog, MatDialogRef } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';
import { UserService } from '@services/users/user.service';
import { AuthService } from '@services/users/auth.service';

@Component({
    selector: 'app-online-users',
    imports: [],
    template: ''
})
export class OnlineUsersComponent implements OnInit, OnDestroy {

  dialogRef: MatDialogRef<ConfirmDialogComponent>;
  subs = new SubscriptionsContainer();
  constructor(private dialog: MatDialog, private userService: UserService, private router: Router, public authService: AuthService) {}

  ngOnInit(): void {
    this.subs.add = this.userService.getOnlineUsersCount().subscribe({
      next: (response) => {
        const onlineUsersCount = response.measurements[0].value;
        this.showOnlineUsers(onlineUsersCount);
      },
      error: (error) => { }
    });
  
    this.subs.add = this.dialogRef.afterClosed().subscribe(() => {
      this.router.navigate(['../']); // Navigate back to the parent route
    });
  }

  showOnlineUsers(onlineUsersCount: any): void {
    if(this.authService.hasPermission('ADMINISTRATION_ADMIN')){
      const dialogData = new ConfirmDialogModel("Online Users", `We have ${onlineUsersCount} online user(s), including you`, '', 'Cancel', 'Close');
      this.dialogRef = this.dialog.open(ConfirmDialogComponent, {
        maxWidth: "400px",
        data: dialogData
      });
      }else{
      const dialogData = new ConfirmDialogModel("No Permission", `You do not have permission to view online users`, '', 'Cancel', 'Close');
      this.dialogRef = this.dialog.open(ConfirmDialogComponent, {
        maxWidth: "400px",
        data: dialogData
      });
      }
      

      this.subs.add = this.dialogRef.afterClosed().subscribe(() => {
        this.router.navigate(['../']); // Navigate back to the parent route
      });
    }

    ngOnDestroy(): void {
    if (this.dialogRef) {
      this.dialogRef.close();
    }
    this.subs.dispose();
  }

}
