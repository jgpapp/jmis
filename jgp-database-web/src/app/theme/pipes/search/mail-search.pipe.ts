import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
    name: 'MailSearch',
    standalone: false
})
export class MailSearchPipe implements PipeTransform {
  transform(value: any, args?: any): any {
    const searchText = new RegExp(args, 'ig');
    if (value) {
      return value.filter((mail: any) => {
        return mail.sender?.search(searchText) !== -1 || mail.subject?.search(searchText) !== -1;
      })
    }
  }
}
