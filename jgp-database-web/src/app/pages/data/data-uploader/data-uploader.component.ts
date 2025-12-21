import { Component, OnDestroy } from '@angular/core';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { FileUploadComponent } from "../../file-upload/file-upload.component";
import { MatButtonModule } from '@angular/material/button';
import { MatSelectModule } from '@angular/material/select';
import { MatOptionModule } from '@angular/material/core';
import { MatCardModule } from '@angular/material/card';
import { DataUploadService } from '@services/shared/data-upload.service';
import { FormBuilder, FormGroup, FormsModule, ReactiveFormsModule, UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { GlobalService } from '@services/shared/global.service';
import { NoPermissionComponent } from '../../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { v4 as uuidv4 } from 'uuid';
import { MatRadioModule } from '@angular/material/radio';
import { ConfirmDialogModel } from '../../../dto/confirm-dialog-model';
import { ConfirmDialogComponent } from '../../confirm-dialog/confirm-dialog.component';
import { MatDialog } from '@angular/material/dialog';
import { HasPermissionDirective } from '../../../directives/has-permission.directive';
import { SubscriptionsContainer } from '../../../theme/utils/subscriptions-container';
import { ActivatedRoute } from '@angular/router';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';

// Define interfaces at the top of the file
interface UploadProgress {
  processed: number;
  total: number;
  step: string;
}

interface ImportDocument {
  id: number;
  name: string;
  importTime: Date;
  endTime: Date;
  completed: boolean;
  totalRecords: number;
  successCount: number;
  failureCount: number;
}

interface PagedResponse<T> {
  content: T[];
  page: {
    totalElements: number;
    totalPages: number;
    size: number;
    number: number;
  };
}

@Component({
    selector: 'app-data-uploader',
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
        MatSlideToggleModule,
        MatTableModule,
        MatPaginatorModule,
        MatTooltipModule,
        MatProgressBarModule,
        MatRadioModule,
        FormsModule,
        HasPermissionDirective
    ],
    templateUrl: './data-uploader.component.html',
    styleUrl: './data-uploader.component.scss'
})
export class DataUploaderComponent implements OnDestroy {

  template: File | null = null;
  legalFormType: string | undefined;
  bulkImportForm: UntypedFormGroup;
  partnerType: string | undefined = 'NONE';
  public docsFilterForm: FormGroup;
  progress: UploadProgress | null = null;
  updateParticipantInfo: boolean = false;
  uploadProgressID: string | null = null;

  public displayedColumns = ['name', 'importTime', 'endTime', 'completed', 'totalRecords', 'successCount', 'failureCount', 'actions'];
  public dataSource: MatTableDataSource<ImportDocument>;
  pageSize = 10;
  pageIndex = 0;
  totalItems = 0;
  entityType: any;
  documents: ImportDocument[]
  importDocumentId: any;

  private uploadCheckInterval: any = null;
  private uploadTimeoutId: any = null;

  subs = new SubscriptionsContainer();
  constructor(
    public fb: FormBuilder,
    private dataUploadService: DataUploadService, 
    private gs: GlobalService,
    private formBuilder: UntypedFormBuilder,
    public authService: AuthService, 
    private route: ActivatedRoute,
    private dialog: MatDialog){

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
    const importDocId = this.route.snapshot.paramMap.get('id');
    const importDocEntityType = this.route.snapshot.paramMap.get('entityType');
    this.getDocumentsById(importDocId, importDocEntityType);
  }

  subscribeToUploadProgress(documentId: any): void {
    this.subs.add = this.dataUploadService.subscribeToUploadProgress(documentId)
      .subscribe({
        next: (response) => {
          this.progress = JSON.parse(response);
        },
        error: (error) => { }
      });
  }


  getDocumentsById(importDocumentId: string | null, entityType: string | null): void {
    if (importDocumentId && entityType) {
      this.subs.add = this.dataUploadService.getDocumentsById(Number(importDocumentId), entityType)
        .subscribe({
          next: (response) => {
            this.documents = response.content;
            this.dataSource = new MatTableDataSource(this.documents);
            this.totalItems = response.page.totalElements;
          },
          error: (error) => { }
        });
      }
  }

  getAvailableDocuments() {
    let partnerId = this.authService.currentUser()?.partnerId;
    // Clear any existing intervals/timeouts first
  if (this.uploadCheckInterval) {
    clearInterval(this.uploadCheckInterval);
    this.uploadCheckInterval = null;
  }
  if (this.uploadTimeoutId) {
    clearTimeout(this.uploadTimeoutId);
    this.uploadTimeoutId = null;
  }
  
  if(this.importDocumentId){
    // Poll for upload completion
    this.uploadCheckInterval = setInterval(() => {
      if (this.progress && this.progress.step === 'Upload Completed') {
        clearInterval(this.uploadCheckInterval);
        this.uploadCheckInterval = null;
        this.getDocumentsById(this.importDocumentId, this.entityType);
      }
    }, 1000); // Check every second

    // Timeout after 10 minutes
    this.uploadTimeoutId = setTimeout(() => {
      if (this.uploadCheckInterval) {
        clearInterval(this.uploadCheckInterval);
        this.uploadCheckInterval = null;
      }
      this.getDocumentsById(this.importDocumentId, this.entityType);
    }, 600000);
    } else if(partnerId){
      this.subs.add = this.dataUploadService.getAvailableDocuments(partnerId, this.entityType, this.pageIndex, this.pageSize)
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
  onFileSelect2($event: any) {
    if ($event.target.files.length > 0) {
      this.template = $event.target.files[0];
      this.uploadProgressID = null; // to reset progress bar
      this.progress = null; // to reset progress bar
      if (this.template && this.template.name.toUpperCase().includes('LOAN_IMPORT_TEMPLATE') && this.authService.hasPermission('LOAN_UPLOAD')) {
        this.legalFormType = 'LOAN_IMPORT_TEMPLATE';
      } else if (this.template && this.template.name.toUpperCase().includes('TA_IMPORT_TEMPLATE') && this.authService.hasPermission('BMO_PARTICIPANTS_DATA_UPLOAD')) {
        this.legalFormType = 'TA_IMPORT_TEMPLATE';
      }else if (this.template && this.template.name.toUpperCase().includes('MENTORSHIP_IMPORT_TEMPLATE') && this.authService.hasPermission('MENTOR_SHIP_UPLOAD')) {
        this.legalFormType = 'MENTORSHIP_IMPORT_TEMPLATE';
      }else if (this.template && this.template.name.toUpperCase().includes('MONITORING_IMPORT_TEMPLATE') && this.authService.hasPermission('MONITORING_OUTCOME_UPLOAD')) {
        this.legalFormType = 'MONITORING_IMPORT_TEMPLATE';
      }
    }
  }

  onFileSelect($event: Event) {
  const input = $event.target as HTMLInputElement;
  
  if (!input.files || input.files.length === 0) {
    return;
  }
  
  this.template = input.files[0];
  this.uploadProgressID = null;
  this.progress = null;
  this.legalFormType = undefined;
  
  const fileName = this.template.name.toUpperCase();
  
  // Define template mappings
  const templateMappings: Record<string, { keyword: string; permission: string }> = {
    'LOAN_IMPORT_TEMPLATE': { keyword: 'LOAN_IMPORT_TEMPLATE', permission: 'LOAN_UPLOAD' },
    'TA_IMPORT_TEMPLATE': { keyword: 'TA_IMPORT_TEMPLATE', permission: 'BMO_PARTICIPANTS_DATA_UPLOAD' },
    'MENTORSHIP_IMPORT_TEMPLATE': { keyword: 'MENTORSHIP_IMPORT_TEMPLATE', permission: 'MENTOR_SHIP_UPLOAD' },
    'MONITORING_IMPORT_TEMPLATE': { keyword: 'MONITORING_IMPORT_TEMPLATE', permission: 'MONITORING_OUTCOME_UPLOAD' }
  };
  
  // Find matching template type
  for (const [templateType, config] of Object.entries(templateMappings)) {
    if (fileName.includes(config.keyword) && this.authService.hasPermission(config.permission)) {
      this.legalFormType = templateType;
      break;
    }
  }
  
  if (!this.legalFormType) {
    this.gs.openSnackBar('Invalid template file or insufficient permissions', 'Dismiss');
  }
}

  uploadTemplate() {
    if(!this.authService.currentUser()?.partnerId){
      this.gs.openSnackBar('User must be assigned to a patner!!', "Dismiss");
      return;
    }
    if(!this.legalFormType){
      this.gs.openSnackBar('Invalid Template or You have no required permissions!', "Dismiss");
      return;
    }
    if(!this.template){
      this.gs.openSnackBar('No file selected!', "Dismiss");
      return;
    }
    this.uploadProgressID = uuidv4();
    this.subscribeToUploadProgress(this.uploadProgressID);
    this.subs.add = this.dataUploadService.uploadDataTemplate(
      this.template, 
      this.legalFormType, 
      this.uploadProgressID, 
      this.updateParticipantInfo ? 'YES' : 'NO'
    ).subscribe({
        next: (response) => {
          this.importDocumentId = response.message;
          this.entityType = this.legalFormType;
          this.getAvailableDocuments();
          this.gs.openSnackBar('Upload initiated successfully', 'Dismiss');
        
        // Reset form after successful upload initiation
        this.resetUploadForm();
        },
      error: (error) => {
        this.gs.openSnackBar('Error uploading file: ' + (error.message || 'Unknown error'), 'Dismiss');
        this.uploadProgressID = null;
        this.progress = null;
      }
      });
  }

  private resetUploadForm(): void {
  this.template = null;
  this.legalFormType = undefined;
  this.updateParticipantInfo = false;
  
  // Reset file input element
  const fileInput = document.querySelector('input[type="file"]') as HTMLInputElement;
  if (fileInput) {
    fileInput.value = '';
  }
}

  
  downloadTemplate() {
    if(this.bulkImportForm.valid){
      this.subs.add = this.dataUploadService.downloadDataTemplate(this.bulkImportForm.value.legalForm)
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
      this.subs.add = this.dataUploadService.downloadDataImportedFile(row, 'EXCEL')
        .subscribe({
          next: (response) => {
              this.dataUploadService.downloadFileFromAPIResponse(response);
          }
        });
  }

  deleteImportedTemplateFile(importId: number): void {
        const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to continue?`, 'Yes, Delete Template plus all associated data', 'No', 'Yes, But Only The Template', true);
        const dialogRef = this.dialog.open(ConfirmDialogComponent, {
          maxWidth: "400px",
          data: dialogData
        });
    
        dialogRef.afterClosed().subscribe(dialogResult => {
          if(dialogResult > 0){ // 0 means dialog was dismissed
          this.subs.add = this.dataUploadService.deleteResourceFile(importId, dialogResult === 2) // 2 means delete associated data too
          .subscribe({
            next: (response) => {
                this.gs.openSnackBar('Imported Template successfully deleted!!', 'X');
                this.getAvailableDocuments();
            },
            error: (error) => {
                console.error(error);
            }
          });
          }
        });
      }

  get uploadButtonIsDisabled(): boolean {
  return !this.template 
    || !this.isExcelFile(this.template) 
    || !this.authService.currentUser()?.partnerId 
    || this.uploadProgressID !== null
    || !this.legalFormType;
}

get isUploading(): boolean {
  return this.uploadProgressID !== null && this.progress !== null;
}

get uploadComplete(): boolean {
  return this.progress?.step === 'Upload Completed';
}

get uploadPercentage(): number {
  if (!this.progress || this.progress.total === 0) {
    return 0;
  }
  return Math.round((this.progress.processed / this.progress.total) * 100);
}

get progressText(): string {
  if (!this.progress) {
    return '';
  }
  
  if (this.uploadComplete) {
    return `Completed Processing ${this.progress.total} rows`;
  }
  
  return `${this.progress.step} ${this.progress.processed} of ${this.progress.total} rows`;
}
 

  isExcelFile(file: File | null): boolean {
  if (!file || !file.name) {
    return false;
  }
  
  const validExtensions = ['.xls', '.xlsx'];
  const fileName = file.name.toLowerCase();
  
  return validExtensions.some(ext => fileName.endsWith(ext));
}

  ngOnDestroy(): void {
     // Clear intervals/timeouts on destroy
  if (this.uploadCheckInterval) {
    clearInterval(this.uploadCheckInterval);
  }
  if (this.uploadTimeoutId) {
    clearTimeout(this.uploadTimeoutId);
  }
    this.dataUploadService.disconnectWebSocket();
    this.subs.dispose();
  }
}
