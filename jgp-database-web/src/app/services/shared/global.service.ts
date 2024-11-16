import { HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { ChartDialogComponent } from '../../pages/chart-dialog/chart-dialog.component';

@Injectable({ providedIn: 'root' })
export class GlobalService {
  //API_HOST = '68.183.21.53';
  API_HOST = '20.242.125.65';
  //API_HOST = 'localhost';
  API_PORT = '8082';
  BASE_API_URL: string = `http://${this.API_HOST}:${this.API_PORT}/jgp-app/api/v1`;  
 
  HTTP_OPTIONS = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };
  
  constructor(public _snackBar: MatSnackBar) { }

  openSnackBar(message: string, action: string) {
    this._snackBar.open(message, 'X', {
      duration: 6000,
      verticalPosition: 'top'
    });
  }

  openExpandedChartDialog(chartData: any, dialog: MatDialog): void {
    // Dynamically calculate dialog size
    const dialogWidth = window.innerWidth;
    const dialogHeight = window.innerHeight;
    const dialogRef = dialog.open(ChartDialogComponent, {
      width: `${dialogWidth}px`,
      height: `${dialogHeight}px`,
      data: chartData,
      panelClass: 'custom-dialog-container', // Custom styles can be added
    });

    dialogRef.afterClosed().subscribe(result => {
      console.log('Dialog was closed');
    });
  }

}