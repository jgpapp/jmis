import { Component } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { MatTabsModule } from '@angular/material/tabs';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
    selector: 'app-tabs',
    imports: [
        FlexLayoutModule,
        MatCardModule,
        MatTabsModule,
        MatIconModule,
        ContentHeaderComponent
    ],
    templateUrl: './tabs.component.html'
})
export class TabsComponent {

}
