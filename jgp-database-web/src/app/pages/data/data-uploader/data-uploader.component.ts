import { Component, OnDestroy } from '@angular/core';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { FileUploadComponent } from "../../file-upload/file-upload.component";
import { MatButtonModule } from '@angular/material/button';
import { MatSelectModule } from '@angular/material/select';
import { MatOptionModule } from '@angular/material/core';
import { MatCardModule } from '@angular/material/card';
import { DataUploadService } from '@services/shared/data-upload.service';
import { FormBuilder, FormGroup, ReactiveFormsModule, UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { GlobalService } from '@services/shared/global.service';
import { NoPermissionComponent } from '../../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { Subject, Subscription, takeUntil } from 'rxjs';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { v4 as uuidv4 } from 'uuid';

@Component({
  selector: 'app-data-uploader',
  standalone: true,
  imports: [
    ContentHeaderComponent,
    FileUploadComponent,
    MatButtonModule,
    MatSelectModule,
    MatCardModule,
    ReactiveFormsModule,
    FlexLayoutModule,
    MatFormFieldModule,
    MatInputModule,
    MatIconModule,
    MatButtonModule,
    MatOptionModule,
    NoPermissionComponent,
    MatTableModule,
    MatPaginatorModule,
    MatTooltipModule,
    MatProgressBarModule
],
  templateUrl: './data-uploader.component.html',
  styleUrl: './data-uploader.component.scss'
})
export class DataUploaderComponent implements OnDestroy {

  bulkImport: any = {};
  template: File;
  bulkImportForm: UntypedFormGroup;
  partnerType: string | undefined = 'NONE';
  public docsFilterForm: FormGroup;
  progress: { processed: number; total: number; finished: number } | null = null;
  private subscription: Subscription | null = null;

  public displayedColumns = ['name', 'importTime', 'endTime', 'completed', 'totalRecords', 'successCount', 'failureCount', 'actions'];
  public dataSource: any;
  pageSize = 10;
  pageIndex = 0;
  totalItems = 0;
  entityType: any;
  documents: any[]
  importDocumentId: any;

  private unsubscribe$ = new Subject<void>();
  constructor(
    public fb: FormBuilder,
    private dataUploadService: DataUploadService, 
    private gs: GlobalService,
    private formBuilder: UntypedFormBuilder,
    public authService: AuthService){

      this.docsFilterForm = this.fb.group({
        selectedEntityType: [null]
        });
        this.dataUploadService.initializeStompClient();
  }


  createBulkImportForm() {
    this.bulkImportForm = this.formBuilder.group({
      countryId: [''],
      officeId: [''],
      staffId: [''],
      legalForm: ['', Validators.required],
    });
  }

  ngOnInit() {
    this.partnerType = this.authService.currentUser()?.partnerType === '-' ? 'NONE' : this.authService.currentUser()?.partnerType;
    this.createBulkImportForm();
  }

  subscribeToUploadProgress(documentId: any): void {
    this.dataUploadService.subscribeToUploadProgress(documentId)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.progress = JSON.parse(response);
        },
        error: (error) => { }
      });
  }


  getAvailableDocuments() {
    let partnerId = this.authService.currentUser()?.partnerId;
    if(this.importDocumentId){
      this.dataUploadService.getDocumentsById(this.importDocumentId, this.entityType)
      .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
            this.documents = response.content;
            this.dataSource = new MatTableDataSource(this.documents);
            this.totalItems = response.page.totalElements;
          },
          error: (error) => { }
        });
    } else if(partnerId){
      this.dataUploadService.getAvailableDocuments(partnerId, this.entityType, this.pageIndex, this.pageSize)
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
    this.importDocumentId = undefined;
  }

  filterEntityTypeChanged() {
      this.entityType = this.docsFilterForm.controls['selectedEntityType'].value;
      this.pageIndex = 0;
      this.getAvailableDocuments();
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

  uploadTemplate() {
    let legalFormType = '';
    let entityType = '';
    /** Only for Client Bulk Imports */
    if(this.authService.currentUser()?.partnerId){
      if (this.template.name.toUpperCase().includes('LOAN_IMPORT_TEMPLATE')) {
        legalFormType = 'LOAN_IMPORT_TEMPLATE';
        entityType = 'loans';
      } else if (this.template.name.toUpperCase().includes('TA_IMPORT_TEMPLATE')) {
        legalFormType = 'TA_IMPORT_TEMPLATE';
        entityType = 'bmos';
      }else {
        this.gs.openSnackBar('Invalid Template', "Dismiss");
      }
    }else{
      this.gs.openSnackBar('User must be assigned to a patner!!', "Dismiss");
    }

    if('' !== legalFormType){
      let uploadProgressID = uuidv4();
      this.subscribeToUploadProgress(uploadProgressID);
    this.dataUploadService.uploadDataTemplate(this.template, legalFormType, uploadProgressID)
      .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.importDocumentId = response.message;
          this.entityType = legalFormType;
          this.getAvailableDocuments();
        }
      });
    }
  }

  
  downloadTemplate() {
    if(this.bulkImportForm.valid){
    this.dataUploadService.downloadDataTemplate(this.bulkImportForm.value.legalForm)
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
  }

  downLoadDocument(row: any){
      this.dataUploadService.downloadDataImportedFile(row)
        .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
              this.dataUploadService.downloadFileFromAPIResponse(response);
          }
        });
  }

  ngOnDestroy(): void {
    this.dataUploadService.disconnectWebSocket()
    if (this.subscription) {
      this.subscription.unsubscribe();
    }
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
