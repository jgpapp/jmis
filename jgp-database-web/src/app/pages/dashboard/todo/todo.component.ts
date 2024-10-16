import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatListModule } from '@angular/material/list';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { TodoService } from '@services/todo.service';
import { NgScrollbarModule } from 'ngx-scrollbar';

@Component({
  selector: 'app-todo',
  standalone: true,
  imports: [
    FlexLayoutModule,
    FormsModule,
    MatCardModule,
    MatInputModule,
    MatButtonModule,
    MatIconModule,
    MatListModule,
    NgScrollbarModule
  ],
  templateUrl: './todo.component.html',
  styleUrl: './todo.component.scss',
  providers: [TodoService]
})
export class TodoComponent {
  public todoList: Array<any>;
  public newTodoText: string = '';

  constructor(private _todoService: TodoService) {
    this.todoList = this._todoService.getTodoList();
  }

  public getNotDeleted() {
    return this.todoList.filter((item: any) => {
      return !item.deleted
    })
  }

  public addToDoItem(event: any) {
    if ((event.which === 1 || event.which === 13) && this.newTodoText.trim() != '') {
      this.todoList.unshift({
        text: this.newTodoText
      });
      this.newTodoText = '';
    }
  }

}