import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { GlobalService } from '../shared/global.service';
import { Observable, of } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class DataUploadService {

    constructor(private httpClient: HttpClient, private gs: GlobalService) { }


    uploadDataTemplate(file: File, templateName: string): Observable<any> {
        const formData = new FormData();
        formData.append('excelFile', file, file.name);
        if(templateName.toUpperCase().includes('TA_IMPORT_TEMPLATE')){
            return this.httpClient.post(`${this.gs.BASE_API_URL}/bmos/upload-template`, formData);
        }else if(templateName.toUpperCase().includes('LOAN_IMPORT_TEMPLATE')){
            return this.httpClient.post(`${this.gs.BASE_API_URL}/loans/upload-template`, formData);
        }
        return of(null);
      }

      trackTemplateUploadProgress(entityType: string, importDocumentId: any): Observable<any> {
        return this.httpClient.get<{ processed: number; total: number }>(`${this.gs.BASE_API_URL}/${entityType}/import-progress/${importDocumentId}`);
      }

      downloadDataTemplate(templateName: string): Observable<any> {
        if(templateName.toUpperCase().includes('TA_IMPORT_TEMPLATE')){
            return this.httpClient.get(`${this.gs.BASE_API_URL}/bmos/template/download`, {
              responseType: 'arraybuffer',
              observe: 'response',
            });
        }else if(templateName.toUpperCase().includes('LOAN_IMPORT_TEMPLATE')){
            return this.httpClient.get(`${this.gs.BASE_API_URL}/loans/template/download`, {
              responseType: 'arraybuffer',
              observe: 'response',
            });
        }
        return of();
      }


      downloadDataImportedFile(row: any): Observable<any> {
            return this.httpClient.get(`${this.gs.BASE_API_URL}/imports/downloadOutputTemplate?importDocumentId=${row.documentId}`, {
              responseType: 'arraybuffer',
              observe: 'response',
            });
      }


      /**
   * Download file from API response
   *
   * @param res
   */
  downloadFileFromAPIResponse(res: any) {
    const headers = res.headers;
    const contentType = headers.get('Content-Type');
    const blob = new Blob([res.body], { type: contentType });
    const fileName = this.getFileNameFromHttpHeaders(headers);
    let fileLink = document.createElement('a');
    document.body.appendChild(fileLink);
    fileLink.style.display = 'none';
    const url = window.URL.createObjectURL(blob);
    fileLink.href = url;
    fileLink.download = fileName;
    fileLink.click();
    setTimeout(() => {
      window.URL.revokeObjectURL(url);
      document.body.removeChild(fileLink);
    }, 0);
  }

  /**
   * Get file name from HTTP headers
   * @param headers the HTTP headers
   * @returns the file name found in the headers
   */
  getFileNameFromHttpHeaders(headers: any): string {
    const contentDispositionHeader = headers.get('Content-Disposition');
    let result = contentDispositionHeader.split(';')[1].trim().split('=')[1];
    return result.replace(/"/g, '');
  }


  getAvailableDocuments(partnerId: number, entityType: string, page: number, size: number): Observable<any> {
    let params = new HttpParams()
    .set('pageNumber', page.toString())
    .set('pageSize', size.toString())
    .set('entityType', entityType)
    .set('partnerId', partnerId);
    return this.httpClient.get(`${this.gs.BASE_API_URL}/imports`, { params });
  }

  getDocumentsById(importDocumentId: number, entityType: string): Observable<any> {
    let params = new HttpParams()
    .set('entityType', entityType)
    .set('importDocumentId', importDocumentId);
    return this.httpClient.get(`${this.gs.BASE_API_URL}/imports`, { params });
  }
}