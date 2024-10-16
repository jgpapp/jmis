import { Component, inject } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSnackBar } from '@angular/material/snack-bar';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
  selector: 'app-snackbar',
  standalone: true,
  imports: [
    MatCardModule,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule,
    ContentHeaderComponent
  ],
  templateUrl: './snackbar.component.html'
})
export class SnackbarComponent {
  snackBar = inject(MatSnackBar);

  openSnackBar(message: string, action: string) {
    this.snackBar.open(message, action, {
      duration: 2000
    });
  }
}
