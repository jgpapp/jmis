import { Injectable } from '@angular/core';

@Injectable()
export class TodoService {

  private _todoList = [
    { text: 'Dummy' },
    { text: 'Dummy' },
    { text: 'Dummy' },
    { text: 'Dummy' },
    { text: 'Dummy' },
    { text: 'Dummy' },
    { text: 'Dummy' },
    { text: 'Dummy' }
  ];

  getTodoList() {
    return this._todoList;
  }
}