import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { ConfirmDialogModel } from '../../dto/confirm-dialog-model';
import { MatButtonModule } from '@angular/material/button';


@Component({
    selector: 'app-confirm-dialog',
    imports: [
    MatDialogModule,
    MatButtonModule
],
    templateUrl: './confirm-dialog.component.html',
    styleUrl: './confirm-dialog.component.scss'
})
export class ConfirmDialogComponent {
  title: string;
  message: string;
  cancelButtonLabel: string;
  confirmButtonLabel: string;
  secondConfirmButtonLabel: string;
  public includeSecondConfirmBtn: boolean = false

  constructor(public dialogRef: MatDialogRef<ConfirmDialogComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ConfirmDialogModel) {
    // Update view with given values
    this.title = data.title;
    this.message = data.message;
    this.cancelButtonLabel = data.cancelButtonLabel;
    this.confirmButtonLabel = data.confirmButtonLabel;
    this.secondConfirmButtonLabel = data.secondConfirmButtonLabel;
    this.includeSecondConfirmBtn = data.includeSecondConfirmBtn;
  }

  onConfirm(): void {
    // Close the dialog, return 1
    this.dialogRef.close(1);
  }

  onConfirmSecond(): void {
    // Close the dialog, return 2
    this.dialogRef.close(2);
  }

  onDismiss(): void {
    // Close the dialog, return 0
    this.dialogRef.close(0);
  }
}
