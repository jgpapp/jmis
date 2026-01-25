import { DatePipe } from '@angular/common';
import { Injectable } from '@angular/core';
import moment from 'moment';

@Injectable({
  providedIn: 'root'
})
export class Dates {

  constructor(private datePipe: DatePipe) {}

  public formatDate(timestamp: any, dateFormat: string): string | null {
    return this.datePipe.transform(timestamp, dateFormat);
  }

  public parseDate(value: any): Date {
    if (value instanceof Array) {
      return moment(value.join('-'), 'YYYY-MM-DD').toDate();
    } else {
      return moment(value).toDate();
    }
  }
}