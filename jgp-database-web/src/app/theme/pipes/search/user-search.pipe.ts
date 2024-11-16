import { Pipe, PipeTransform } from '@angular/core';

@Pipe({ 
  name: 'UserSearchPipe', 
  pure: false 
})
export class UserSearchPipe implements PipeTransform {
  transform(value: any, args?: any): any {
    const searchText = new RegExp(args, 'ig');
    if (value) {
      return value.filter((user: any) => {
        const name = user.firstName || user.lastName || user.username;
        return name.search(searchText) !== -1;
      });
    }
  }
}