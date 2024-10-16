import { Component } from '@angular/core';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { GlobalService } from '@services/shared/global.service';
import { MatIconModule } from '@angular/material/icon';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { LoanService } from '@services/data-management/loan.service';

@Component({
  selector: 'app-data-upload',
  standalone: true,
  imports: [
    ReactiveFormsModule, 
    FormsModule, 
    MatButtonModule, 
    MatInputModule, 
    MatProgressSpinnerModule,
    MatIconModule,
    MatProgressBarModule
  ],
  templateUrl: './data-upload.component.html',
  styleUrl: './data-upload.component.scss'
})
export class DataUploadComponent {

  file: any;
  uploadProgress: number | null = null;

  constructor(private loanService: LoanService, private gs: GlobalService) { }

  onFileSelected(event: any) {
    this.file = event.target.files[0];
  }

  uploadExcel(){
    this.loanService.uploadLendingData(this.file).subscribe({
      next: (response) => {
        this.gs.openSnackBar(response.message, "Dismiss");
      }
    });

  }
}
