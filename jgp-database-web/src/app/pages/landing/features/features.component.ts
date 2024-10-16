import { Component, OnInit } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { LandingService } from '@services/landing.service';

@Component({
  selector: 'app-features',
  standalone: true,
  imports: [
    FlexLayoutModule,
    MatCardModule,
    MatIconModule
  ],
  templateUrl: './features.component.html',
  providers: [LandingService]
})
export class FeaturesComponent implements OnInit {
  public features: any; 
  constructor(private landingService: LandingService) { }

  ngOnInit() {
    this.features = this.landingService.getFeatures();
  }

}

