import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { emailValidator } from '../../../theme/utils/app-validators';
import { GoogleMapsModule } from '@angular/google-maps'; 
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatCardModule } from '@angular/material/card';

@Component({
    selector: 'app-contact-us',
    imports: [
        FlexLayoutModule,
        ReactiveFormsModule,
        MatCardModule,
        MatInputModule,
        MatButtonModule,
        MatIconModule,
        GoogleMapsModule
    ],
    templateUrl: './contact-us.component.html',
    styleUrl: './contact-us.component.scss'
})
export class ContactUsComponent implements OnInit {
  center: google.maps.LatLngLiteral = { lat: 45.421530, lng: -75.697193 };
  zoom = 7;
  markerOptions: google.maps.MarkerOptions = { draggable: false };
  markerPositions: google.maps.LatLngLiteral[] = [
    { lat: 45.421530, lng: -75.697193 }
  ];
  contactForm: FormGroup;

  constructor(public formBuilder: FormBuilder) { }

  ngOnInit() {
    this.contactForm = this.formBuilder.group({
      name: ['', Validators.required],
      email: ['', Validators.compose([Validators.required, emailValidator])],
      phone: ['', Validators.required],
      message: ['', Validators.required]
    });
  }

  public onContactFormSubmit(values: Object): void {
    if (this.contactForm.valid) {
      console.log(values);
    }
  }

}
