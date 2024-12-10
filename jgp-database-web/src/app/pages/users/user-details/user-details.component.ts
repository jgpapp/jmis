import { Component, OnDestroy, OnInit  } from '@angular/core';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { map, Observable, Subject, takeUntil } from 'rxjs';
import { AsyncPipe } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatDividerModule } from '@angular/material/divider';
import { User } from '../../../common/models/user.model';
import { HasPermissionDirective } from '../../../directives/has-permission.directive';
import { ConfirmDialogModel } from '../../../dto/confirm-dialog-model';
import { ConfirmDialogComponent } from '../../confirm-dialog/confirm-dialog.component';
import { MatDialog } from '@angular/material/dialog';
import { GlobalService } from '@services/shared/global.service';
import { UserService } from '@services/users/user.service';

@Component({
  selector: 'app-user-details',
  standalone: true,
  imports: [
    MatCardModule,
    ContentHeaderComponent,
    FlexLayoutModule,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    AsyncPipe,
    MatDividerModule,
    RouterModule,
    HasPermissionDirective
  ],
  templateUrl: './user-details.component.html',
  styleUrl: './user-details.component.scss'
})
export class UserDetailsComponent implements OnDestroy, OnInit{

  selectedUser: Observable<User>;
  private unsubscribe$ = new Subject<void>();
  constructor(private activatedRoute: ActivatedRoute, private gs: GlobalService, private dialog: MatDialog, private userService: UserService){}

  ngOnInit(): void {
    this.selectedUser = this.activatedRoute.data.pipe(map(data => data['selectedUser']));
  }


  resetUserPassword(userId: number): void {
    const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to approve loans data?`);
    const dialogRef = this.dialog.open(ConfirmDialogComponent, {
      maxWidth: "400px",
      data: dialogData
    });

    dialogRef.afterClosed().subscribe(dialogResult => {
      if(dialogResult){
        this.userService.resetUserPassword(userId)
        .pipe(takeUntil(this.unsubscribe$))
          .subscribe({
            next: (response) => {
              this.gs.openSnackBar(response.message, "X");
            },
            error: (error) => { }
          });
      }
    });
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
