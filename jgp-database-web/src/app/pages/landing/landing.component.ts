import { Component, ViewEncapsulation } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatToolbarModule } from '@angular/material/toolbar';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { LandingService } from '@services/landing.service';
import { Settings, SettingsService } from '@services/settings.service';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { FeaturesComponent } from './features/features.component';
import { OurWorksComponent } from './our-works/our-works.component';
import { OurServicesComponent } from './our-services/our-services.component';
import { TestimonialsComponent } from './testimonials/testimonials.component';
import { PricingComponent } from './pricing/pricing.component';
import { ContactUsComponent } from './contact-us/contact-us.component';
import { RouterModule } from '@angular/router';

@Component({
    selector: 'app-landing',
    imports: [
        RouterModule,
        FlexLayoutModule,
        MatToolbarModule,
        MatSidenavModule,
        MatButtonModule,
        MatIconModule,
        ContentHeaderComponent,
        FeaturesComponent,
        OurWorksComponent,
        OurServicesComponent,
        TestimonialsComponent,
        PricingComponent,
        ContactUsComponent
    ],
    templateUrl: './landing.component.html',
    styleUrl: './landing.component.scss',
    encapsulation: ViewEncapsulation.None,
    providers: [LandingService]
})
export class LandingComponent {
  public menuItems: any;  
  public settings: Settings;  
  constructor(public settingsService: SettingsService, private landingService: LandingService) {
    this.settings = this.settingsService.settings; 
  }

  ngOnInit(){
    this.menuItems = this.landingService.getMenuItems();    
  }

  ngAfterViewInit(){
    setTimeout(() => {
      this.settings.loadingSpinner = false; 
    }); 
  }
  
  scrollToSection(id: string): void {  
    const element = document.getElementById(id);
    if (element) {
      const elementPosition = element.offsetTop;
      const container = document.querySelector('.landing-container .mat-drawer-content');
      if (container instanceof HTMLElement) {
        container.scrollTo({
          top: elementPosition - 2,
          behavior: "smooth" 
        });
      }
    }
  }
  
}