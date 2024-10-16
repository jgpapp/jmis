import { Injectable } from '@angular/core';
import { OverlayContainer } from '@angular/cdk/overlay';

@Injectable() 
export class CustomOverlayContainer extends OverlayContainer {
  override _createContainer(): void {
    const container = document.createElement('div');
    container.classList.add('cdk-overlay-container');
    const appElement = document.getElementById('app');
    if (appElement) {
      appElement.appendChild(container);
    } 
    this._containerElement = container;
  }
}