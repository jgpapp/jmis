import { Component } from '@angular/core';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';
import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatDividerModule } from '@angular/material/divider';
import { MatIconModule } from '@angular/material/icon';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';

@Component({
    selector: 'app-cards',
    imports: [
        ContentHeaderComponent,
        FlexLayoutModule,
        MatCardModule,
        MatButtonModule,
        MatIconModule,
        MatDividerModule
    ],
    templateUrl: './cards.component.html',
    styleUrl: './cards.component.scss'
})
export class CardsComponent {
  longText = `The Shiba Inu is the smallest of the six original and distinct spitz breeds of dog
  from Japan. A small, agile dog that copes very well with mountainous terrain, the Shiba Inu was
  originally bred for hunting.`;
}
