import { Component } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
    selector: 'app-progress',
    imports: [
        FlexLayoutModule,
        MatCardModule,
        MatProgressSpinnerModule,
        MatProgressBarModule,
        ContentHeaderComponent
    ],
    templateUrl: './progress.component.html'
})
export class ProgressComponent {

}
