import { Component, ViewEncapsulation } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatMenuModule } from '@angular/material/menu';

@Component({
    selector: 'app-flags-menu',
    imports: [
        MatButtonModule,
        MatIconModule,
        MatMenuModule
    ],
    templateUrl: './flags-menu.component.html',
    styleUrls: ['./flags-menu.component.scss'],
    encapsulation: ViewEncapsulation.None
})
export class FlagsMenuComponent { 

}
