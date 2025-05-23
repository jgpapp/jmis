import { HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { ChartDialogComponent } from '../../pages/chart-dialog/chart-dialog.component';
import { ConfirmDialogModel } from '../../dto/confirm-dialog-model';
import { ConfirmDialogComponent } from '../../pages/confirm-dialog/confirm-dialog.component';
import * as XLSX from 'xlsx';

@Injectable({ providedIn: 'root' })
export class GlobalService {
 
  HTTP_OPTIONS = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };


  // Define the structure of your expected environment variables
  public config: {
    baseApiUrl?: string;
    baseWebSocketUrl?: string;
  } = {};
  
  
  constructor(public _snackBar: MatSnackBar) {

     // Attempt to load config from the global window object
    // The 'env' property will be populated by assets/env.js
    const browserWindow = window || {};
    const browserWindowEnv = (browserWindow as any)['__env'] || {};

    // Assign default values or values from window.__env
    this.config = {
      baseApiUrl: browserWindowEnv.baseApiUrl || 'http://localhost:8082/jgp-app/api/v1',
      baseWebSocketUrl: browserWindowEnv.baseWebSocketUrl || 'http://localhost:8082/jgp-app/ws',
    };
   }

   get baseApiUrl(): string | undefined {
    return this.config.baseApiUrl;
  }

  get baseWebSocketUrl(): string | undefined {
    return this.config.baseWebSocketUrl;
  }

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

  confirmDialog(dialog: MatDialog, message: string): string {
    const dialogData = new ConfirmDialogModel("Confirm Action", message);
    const dialogRef = dialog.open(ConfirmDialogComponent, {
      maxWidth: "400px",
      data: dialogData
    });
    let res = '';
    dialogRef.afterClosed().subscribe(dialogResult => {
      res = dialogResult;
    });

    return res;
  }


  exportToExcel(dataSource: any, columns: any, fileName: string): void {
    const data = dataSource.data; // Get the table data

    // Convert data to a worksheet
    const worksheet: XLSX.WorkSheet = XLSX.utils.json_to_sheet(data, {
      header: columns,
    });

    // Create a workbook
    const workbook: XLSX.WorkBook = XLSX.utils.book_new();
    XLSX.utils.book_append_sheet(workbook, worksheet, 'Table Data');

    // Export the workbook
    XLSX.writeFile(workbook, fileName+'.xlsx');
  }

}