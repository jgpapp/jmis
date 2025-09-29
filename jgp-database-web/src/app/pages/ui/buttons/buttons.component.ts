import { Component } from '@angular/core';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';

@Component({
    selector: 'app-buttons',
    imports: [
        ContentHeaderComponent,
        FlexLayoutModule,
        MatCardModule,
        MatButtonModule,
        MatButtonToggleModule,
        MatIconModule
    ],
    templateUrl: './buttons.component.html'
})
export class ButtonsComponent {

}
