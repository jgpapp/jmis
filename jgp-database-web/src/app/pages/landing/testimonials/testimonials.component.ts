import { ChangeDetectionStrategy, Component, OnInit } from '@angular/core';
import { LandingService } from '@services/landing.service';
import { Settings, SettingsService } from '@services/settings.service';
import {
  NguCarousel,
  NguCarouselConfig,
  NguCarouselDefDirective,
  NguCarouselNextDirective,
  NguCarouselPrevDirective,
  NguItemComponent,
  NguTileComponent
} from '@ngu/carousel';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';

@Component({
    selector: 'app-testimonials',
    imports: [
        NguCarousel,
        NguTileComponent,
        NguCarousel,
        NguCarouselDefDirective,
        NguCarouselNextDirective,
        NguCarouselPrevDirective,
        NguItemComponent,
        FlexLayoutModule,
        MatCardModule,
        MatButtonModule,
        MatIconModule
    ],
    templateUrl: './testimonials.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
    providers: [LandingService]
})
export class TestimonialsComponent implements OnInit {
  public testimonials: any[];
  public testimonialsCarouselConfig: NguCarouselConfig;
  public clients: any[];
  public clientsCarouselConfig: NguCarouselConfig;
  public settings: Settings;
  constructor(public settingsService: SettingsService, private landingService: LandingService) {
    this.settings = this.settingsService.settings;
  }

  ngOnInit() {
    this.clients = this.landingService.getClients();
    this.testimonials = this.landingService.getTestimonials();
    this.clientsCarouselConfig = {
      grid: { xs: 3, sm: 4, md: 5, lg: 6, all: 0 },
      slide: 1,
      speed: 500,
      interval: {
        timing: 4000
      },
      point: {
        visible: false
      },
      loop: true,
      touch: true,
      custom: 'banner',
      RTL: this.settings.rtl
    };
    this.testimonialsCarouselConfig = {
      grid: { xs: 1, sm: 2, md: 3, lg: 3, all: 0 },
      slide: 1,
      speed: 500,
      interval: {
        timing: 4000
      },
      point: {
        visible: true
      },
      loop: true,
      touch: true,
      custom: 'banner',
      RTL: this.settings.rtl
    };
  }

}