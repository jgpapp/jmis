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
import { interval, Subject, Subscription, takeUntil } from 'rxjs';
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
    MatProgressBarModule,
    MatRadioModule,
    FormsModule,
    HasPermissionDirective
],
  templateUrl: './data-uploader.component.html',
  styleUrl: './data-uploader.component.scss'
})
export class DataUploaderComponent implements OnDestroy {

  bulkImport: any = {};
  template: File;
  legalFormType: string | undefined;
  bulkImportForm: UntypedFormGroup;
  partnerType: string | undefined = 'NONE';
  public docsFilterForm: FormGroup;
  progress: { processed: number; total: number; finished: number } | null = null;
  private subscription: Subscription | null = null;
  updateParticipantInfo: string;
  dotCount: number = 0;
  preparingText: string = 'Preparing the template';
  private readonly animationInterval = 500; // milliseconds
  uploadProgressID: string | null = null;

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
    public authService: AuthService, 
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
      this.uploadProgressID = null; // to reset progress bar
      this.progress = null; // to reset progress bar
      if (this.template.name.toUpperCase().includes('LOAN_IMPORT_TEMPLATE')) {
        this.legalFormType = 'LOAN_IMPORT_TEMPLATE';
      } else if (this.template.name.toUpperCase().includes('TA_IMPORT_TEMPLATE')) {
        this.legalFormType = 'TA_IMPORT_TEMPLATE';
      }
    }
  }

  uploadTemplate() {
    if(!this.authService.currentUser()?.partnerId){
      this.gs.openSnackBar('User must be assigned to a patner!!', "Dismiss");
    }else {
    if(this.legalFormType){
      this.uploadProgressID = uuidv4();
      this.subscribeToUploadProgress(this.uploadProgressID);
    this.dataUploadService.uploadDataTemplate(this.template, this.legalFormType, this.uploadProgressID, this.updateParticipantInfo)
      .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.importDocumentId = response.message;
          this.entityType = this.legalFormType;
          this.getAvailableDocuments();
        }
      });
    }
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
      this.dataUploadService.downloadDataImportedFile(row, 'EXCEL')
        .pipe(takeUntil(this.unsubscribe$))
        .subscribe({
          next: (response) => {
              this.dataUploadService.downloadFileFromAPIResponse(response);
          }
        });
  }

  deleteImportedTemplateFile(importId: number): void {
        const dialogData = new ConfirmDialogModel("Confirm", `Are you sure you want to delete Imported Template?`);
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

  get buttonIsDisabled(): boolean {
    return !this.template || !this.isExcelFile(this.template) || !this.authService.currentUser()?.partnerId || !this.legalFormType || this.uploadProgressID !== null;
  }

  get progressText(): string {
    if (this.progress) {
      this.progressDots();
      if (this.progress.processed > 0 && this.progress.total !== this.progress.processed) {
        return `Processed ${this.progress.processed} of ${this.progress.total} rows`;
      } else if (this.progress.processed > 0 && this.progress.total === this.progress.processed) {
        return `Completed Processing All ${this.progress.total} rows`;
      } else {
        return this.preparingText;
      }
    }
    return '';
  }

  progressDots(){
    interval(this.animationInterval)
      .pipe(takeUntil(this.unsubscribe$))
      .subscribe(() => {
        this.dotCount = (this.dotCount + 1) % 4; // Cycle through 0, 1, 2, 3
        this.preparingText = 'Preparing the template' + '.'.repeat(this.dotCount);
      });
  }
 

  isExcelFile(file: File): boolean {
    if (!file || !file.name) {
      return false;
    }
    // Check if the file name ends with .xls or .xlsx (case insensitive)
    const fileName = file.name;
    const lowerCaseFileName = fileName.toLowerCase();
    return lowerCaseFileName.endsWith('.xls') || lowerCaseFileName.endsWith('.xlsx');
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
