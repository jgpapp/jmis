import { Component } from '@angular/core';
import { ContentHeaderComponent } from "../../theme/components/content-header/content-header.component";
import { AuthService } from '@services/users/auth.service';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { MatOptionModule } from '@angular/material/core';
import { MatSelectModule } from '@angular/material/select';
import { MatCardModule } from '@angular/material/card';
import { FormBuilder, ReactiveFormsModule, UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { FileUploadComponent } from "../file-upload/file-upload.component";
import { Subject, Subscription, takeUntil } from 'rxjs';
import { DataUploadService } from '@services/shared/data-upload.service';
import { GlobalService } from '@services/shared/global.service';
import { ConfirmDialogModel } from '../../dto/confirm-dialog-model';
import { ConfirmDialogComponent } from '../confirm-dialog/confirm-dialog.component';
import { MatDialog } from '@angular/material/dialog';
import { HasPermissionDirective } from '../../directives/has-permission.directive';

@Component({
  selector: 'app-system-resources',
  standalone: true,
  imports: [
    ContentHeaderComponent,
    FlexLayoutModule,
    MatButtonModule,
    MatIconModule,
    MatTableModule,
    MatPaginatorModule,
    MatTooltipModule,
    MatProgressBarModule,
    MatOptionModule,
    MatSelectModule,
    MatCardModule,
    ReactiveFormsModule,
    FileUploadComponent,
    HasPermissionDirective
],
  templateUrl: './system-resources.component.html',
  styleUrl: './system-resources.component.scss'
})
export class SystemResourcesComponent {

  legalFormTypeForm: UntypedFormGroup;
  public displayedColumns = ['name', 'endTime','actions'];
  public dataSource: any;
  pageSize = 10;
  pageIndex = 0;
  totalItems = 0;
  entityType: any;
  documents: any[]
  private unsubscribe$ = new Subject<void>();
  private subscription: Subscription | null = null;
  bulkImport: any = {};
  legalFormType: string | undefined;
  template: File | null = null;
  fileName: string | undefined;
  constructor(
    public authService: AuthService, 
    public fb: FormBuilder,
    private gs: GlobalService,
    private formBuilder: UntypedFormBuilder,
    private dataUploadService: DataUploadService, 
    private dialog: MatDialog) { 
      this.createBulkImportForm();
    }


     createBulkImportForm() {
        this.legalFormTypeForm = this.formBuilder.group({
          legalFormType: [''],
        });
      }
    
      ngOnInit() {
        this.getAvailableDocuments();
      }


      getAvailableDocuments() {
            this.dataUploadService.getAvailableDocuments(undefined, 'RESOURCES_IMPORT', this.pageIndex, this.pageSize)
            .pipe(takeUntil(this.unsubscribe$))
              .subscribe({
                next: (response) => {
                  this.documents = response.content;
                  this.dataSource = new MatTableDataSource(this.documents);
                  this.totalItems = response.page.totalElements;
                },
                error: (error) => { }
              });
        }

        onPageChange(event: PageEvent) {
            this.pageIndex = event.pageIndex;
            this.pageSize = event.pageSize;
            this.getAvailableDocuments();
          }
  /**
   * Sets file form control value.
   * @param {any} $event file change event.
   */
  onFileSelect($event: any) {
    if ($event.target.files.length > 0) {
      this.template = $event.target.files[0];
    }
  }

  uploadResourceFile() {
    if(this.legalFormType && this.template) {
    this.dataUploadService.uploadResourceFile(this.template, this.legalFormType)
      .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.gs.openSnackBar(`${response.message}`, 'X');
          this.template = null;
          this.legalFormType = undefined;
          this.getAvailableDocuments();
        },
        error: (error) => {
          console.error(error);
        }
      });
    }
  }

  
  viewDocument(row: any) {
    this.dataUploadService.downloadDataImportedFile(row, 'PDF')
        .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
              this.dataUploadService.openPdfFileOnNewBrowserTab(response);
          }
        });
  }

  downloadDocument(row: any) {
    this.dataUploadService.downloadFile(row)
      .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
            this.dataUploadService.downloadFileFromAPIResponse(response);
        },
        error: (error) => {
          console.error('Error downloading template:', error);
          this.gs.openSnackBar('Error downloading template', "Dismiss");
        }
      });
  }



  deleteResourceFile(importId: number): void {
      const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to delete resource?`);
      const dialogRef = this.dialog.open(ConfirmDialogComponent, {
        maxWidth: "400px",
        data: dialogData
      });
  
      dialogRef.afterClosed().subscribe(dialogResult => {
        if(dialogResult){
          this.dataUploadService.deleteResourceFile(importId)
        .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
            console.log(response);
              this.gs.openSnackBar('Document successfully deleted!!', 'X');
              this.getAvailableDocuments();
          },
          error: (error) => {
              console.error(error);
          }
        });
        }
      });
    }


  get buttonIsDisabled(): boolean {
    return !this.template || !this.legalFormType;
  }

  isPDFFile(file: File): boolean {
    if (!file || !file.name) {
      return false;
    }
    // Check if the file name ends with .pdf
    const fileName = file.name;
    const lowerCaseFileName = fileName.toLowerCase();
    return lowerCaseFileName.endsWith('.pdf');
  }

  ngOnDestroy(): void {
    if (this.subscription) {
      this.subscription.unsubscribe();
    }
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
