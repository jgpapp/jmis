import { Component } from '@angular/core';
import { MatTabsModule } from '@angular/material/tabs';
import { MonthlyComponent } from './monthly/monthly.component';
import { YearlyComponent } from './yearly/yearly.component';

@Component({
    selector: 'app-pricing',
    imports: [
        MatTabsModule,
        MonthlyComponent,
        YearlyComponent
    ],
    templateUrl: './pricing.component.html'
})
export class PricingComponent {

}
