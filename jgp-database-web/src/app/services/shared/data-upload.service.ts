import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { GlobalService } from '../shared/global.service';
import { Observable, of } from 'rxjs';
import { Client, StompSubscription} from '@stomp/stompjs'; 
import SockJS from 'sockjs-client';


@Injectable({
  providedIn: 'root'
})
export class DataUploadService {

  private webSocketClient: Client;
    constructor(private httpClient: HttpClient, private gs: GlobalService) { 
      
    }


    uploadDataTemplate(file: File, templateName: string, uploadProgressID: string, updateParticipantInfo: string = 'NO'): Observable<any> {
        const appDomain = `${window.location.origin}/data-upload/`;
        const formData = new FormData();
        formData.append('excelFile', file, file.name);
        formData.append('appDomain', appDomain);
        let templateEntityType = 'INVALID';
        if(templateName.toUpperCase().includes('TA_IMPORT_TEMPLATE')){
            templateEntityType = 'TA_IMPORT_TEMPLATE';
        }else if(templateName.toUpperCase().includes('LOAN_IMPORT_TEMPLATE')){
            templateEntityType = 'LOAN_IMPORT_TEMPLATE';
        }else if(templateName.toUpperCase().includes('MENTORSHIP_IMPORT_TEMPLATE')){
          templateEntityType = 'MENTORSHIP_IMPORT_TEMPLATE';
        }else if(templateName.toUpperCase().includes('MONITORING_IMPORT_TEMPLATE')){
            templateEntityType = 'MONITORING_IMPORT_TEMPLATE';
        }
        formData.append('entityType', templateEntityType);
        formData.append('documentProgressId', uploadProgressID);
        formData.append('updateParticipantInfo', updateParticipantInfo);
        return this.httpClient.post(`/imports/upload-excel-template`, formData);
      }

      uploadResourceFile(file: File, legalFormType: string): Observable<any> {
        const formData = new FormData();
        formData.append('fileDetail', file, file.name);
        return this.httpClient.post(`/imports/upload-resource-file/${legalFormType}`, formData);
      }

      downloadDataTemplate(templateName: string): Observable<any> {
        let templateEntityType = 'INVALID';
        if(templateName.toUpperCase().includes('TA_IMPORT_TEMPLATE')){
            templateEntityType = 'TA_IMPORT_TEMPLATE';
        }else if(templateName.toUpperCase().includes('LOAN_IMPORT_TEMPLATE')){
            templateEntityType = 'LOAN_IMPORT_TEMPLATE';
        } else if(templateName.toUpperCase().includes('MENTORSHIP_IMPORT_TEMPLATE')){
            templateEntityType = 'MENTORSHIP_IMPORT_TEMPLATE';
        } else if(templateName.toUpperCase().includes('MONITORING_IMPORT_TEMPLATE')){
            templateEntityType = 'MONITORING_IMPORT_TEMPLATE';
        }  else {
          return of();
        }
        
        return this.httpClient.get(`/imports/template/download/${templateEntityType}`, {
          responseType: 'arraybuffer',
          observe: 'response',
        });
      }


      downloadDataImportedFile(row: any, fileType: string): Observable<any> {
            return this.httpClient.get(`/imports/downloadOutputTemplate/${fileType}?importDocumentId=${row.documentId}`, {
              responseType: 'arraybuffer',
              observe: 'response',
            });
      }

      downloadFile(row: any): Observable<any> {
            return this.httpClient.get(`/imports/downloadFile/${row.documentId}`, {
              responseType: 'arraybuffer',
              observe: 'response',
            });
      }

      deleteResourceFile(importId: number, deleteAssociatedData: Boolean = false): Observable<any> {
        return this.httpClient.delete(`/imports/delete-resource-file/${importId}?delete-associated-data=${deleteAssociatedData}`);
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

  openPdfFileOnNewBrowserTab(res: any) {
    const headers = res.headers;
    const contentType = headers.get('Content-Type');
    const blob = new Blob([res.body], { type: contentType });
    const fileURL = URL.createObjectURL(blob);

    // Open the Blob URL in a new tab
    const pdfWindow = window.open(fileURL, '_blank');

    // Optional: Revoke the object URL after a short delay
    // This frees up memory, but the browser needs a moment to load the content.
    // Adjust the delay if needed.
    if (pdfWindow) {
        pdfWindow.onload = () => {
          setTimeout(() => URL.revokeObjectURL(fileURL), 100);
        };
    } else {
        // Handle cases where the pop-up was blocked
        console.error('Pop-up blocked. Please allow pop-ups for this site.');
        URL.revokeObjectURL(fileURL); // Clean up immediately if window didn't open
    }
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


  getAvailableDocuments(partnerId: number | undefined, entityType: string, page: number, size: number): Observable<any> {
    let params = new HttpParams()
    .set('pageNumber', page.toString())
    .set('pageSize', size.toString())
    .set('entityType', entityType);
    if(undefined !== partnerId){
      params = params.set('partnerId', partnerId);
    }
    return this.httpClient.get(`/imports`, { params });
  }

  getDocumentsById(importDocumentId: number, entityType: string): Observable<any> {
    let params = new HttpParams()
    .set('entityType', entityType)
    .set('importDocumentId', importDocumentId);
    return this.httpClient.get(`/imports`, { params });
  }

  initializeStompClient(): void {
    // Initialize the STOMP client
    this.webSocketClient = new Client({
      webSocketFactory: () => new SockJS(this.gs.baseWebSocketUrl || ''), // WebSocket URL for the backend
      reconnectDelay: 5000,
      heartbeatIncoming: 4000,
      heartbeatOutgoing: 4000,
      connectHeaders: {},
      debug: (str) => {
        //console.log(str); // Debugging logs for STOMP. Enable in case of issues
      },
      onConnect: () => {
        console.log('Connected to WebSocket');
      },
      onDisconnect: () => {
        console.log('Disconnected from WebSocket');
      }
    });

    this.webSocketClient.activate();
  }
  

   // Method to subscribe to progress updates
   subscribeToUploadProgress(documentId: string): Observable<string> {
    return new Observable(observer => {
      // Wait for the connection to be established
      if (this.webSocketClient.connected) {
        this.webSocketClient.subscribe(`/topic/progress/${documentId}`, (message) => {
          observer.next(message.body); // Send progress updates to observers
        });
      }
    });
  }

  // Method to subscribe to online users count updates
   subscribeToOnlineUsersCount(): Observable<string> {
    return new Observable(observer => {
      // Wait for the connection to be established
      if (this.webSocketClient.connected) {
        this.webSocketClient.subscribe(`/topic/online-users`, (message) => {
          observer.next(message.body); // Send online users count updates to observers
        });
      }
    });
  }

  disconnectWebSocket(): void {
    if(this.webSocketClient.connected){
      this.webSocketClient.deactivate();
    }
  }
}