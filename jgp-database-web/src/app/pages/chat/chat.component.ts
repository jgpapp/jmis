import { Component, HostListener, OnInit, ViewChild, ViewEncapsulation } from '@angular/core';
import { Chat } from '../../common/models/chat.model';
import { ChatService } from '@services/chat.service';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatListModule } from '@angular/material/list';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatMenuModule } from '@angular/material/menu';
import { NgScrollbarModule } from 'ngx-scrollbar';
import { MatDividerModule } from '@angular/material/divider';
import { MatInputModule } from '@angular/material/input';
import { DatePipe } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';

@Component({
    selector: 'app-chat',
    imports: [
        DatePipe,
        FormsModule,
        FlexLayoutModule,
        MatCardModule,
        MatSidenavModule,
        MatToolbarModule,
        MatListModule,
        MatButtonModule,
        MatIconModule,
        MatMenuModule,
        MatDividerModule,
        MatInputModule,
        NgScrollbarModule,
        ContentHeaderComponent
    ],
    templateUrl: './chat.component.html',
    styleUrl: './chat.component.scss',
    encapsulation: ViewEncapsulation.None,
    providers: [
        ChatService
    ]
})
export class ChatComponent implements OnInit {
  @ViewChild('sidenav') sidenav: any;
  public userImage = 'img/users/user.jpg';
  public chats: Array<Chat>;
  public talks: Array<Chat>;
  public sidenavOpen: boolean = true;
  public currentChat: Chat;
  public newMessage: string;

  constructor(private chatService: ChatService) { }

  ngOnInit() {
    this.chats = this.chatService.getChats();
    if (window.innerWidth <= 768) {
      this.sidenavOpen = false;
    }
  }

  @HostListener('window:resize')
  public onWindowResize(): void {
    (window.innerWidth <= 768) ? this.sidenavOpen = false : this.sidenavOpen = true;
  }

  public getChat(obj: any) {
    if (this.talks) {
      this.talks.length = 2;
    }
    this.talks = this.chatService.getTalk();
    this.talks.push(obj);
    this.currentChat = obj;
    this.talks.forEach(talk => {
      if (!talk.my) {
        talk.image = obj.image;
      }
    });
    if (window.innerWidth <= 768) {
      this.sidenav.close();
    }
  }

  public sendMessage($event: any) {
    if (($event.which === 1 || $event.which === 13) && this.newMessage.trim() != '') {
      if (this.talks) {
        this.talks.push(
          new Chat(
            'assets/img/users/user.jpg',
            'Emilio Verdines',
            'online',
            this.newMessage,
            new Date(),
            true)
        )
        this.newMessage = '';
        let chatContainer = document.querySelector('.chat-content');
        if (chatContainer) {
          setTimeout(() => {
            var nodes = chatContainer!.querySelectorAll('.mat-mdc-list-item');
            let newChatTextHeight = nodes[nodes.length - 1];
            chatContainer!.scrollTop = chatContainer!.scrollHeight + newChatTextHeight.clientHeight;
          });
        }
      }
    }
  }

  public ngOnDestroy() {
    if (this.talks)
      this.talks.length = 2;
  }

}