import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
    name: 'ChatPersonSearchPipe',
    standalone: false
})
export class ChatPersonSearchPipe implements PipeTransform {
  transform(value: any, args?: any): any {
    const searchText = new RegExp(args, 'ig');
    if (value) {
      return value.filter((message: any) => {      
        return message.author?.search(searchText) !== -1;         
      });
    }
  }
}
