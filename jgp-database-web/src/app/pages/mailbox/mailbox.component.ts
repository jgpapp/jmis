import { Component, HostListener, OnInit, ViewChild, ViewEncapsulation } from '@angular/core';
import { MailboxService } from '@services/mailbox.service';
import { Mail } from '../../common/models/mail.model';
import { FormBuilder, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatSnackBar } from '@angular/material/snack-bar';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatCardModule } from '@angular/material/card';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatMenuModule } from '@angular/material/menu';
import { MatListModule } from '@angular/material/list';
import { MatDividerModule } from '@angular/material/divider';
import { MatInputModule } from '@angular/material/input';
import { NgScrollbarModule } from 'ngx-scrollbar';
import { QuillModule } from 'ngx-quill'
import { PipesModule } from '../../theme/pipes/pipes.module';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';

@Component({
    selector: 'app-mailbox',
    imports: [
        FormsModule,
        ReactiveFormsModule,
        FlexLayoutModule,
        MatCardModule,
        MatSidenavModule,
        MatToolbarModule,
        MatButtonModule,
        MatIconModule,
        MatMenuModule,
        MatListModule,
        MatDividerModule,
        MatInputModule,
        NgScrollbarModule,
        QuillModule,
        PipesModule,
        ContentHeaderComponent
    ],
    templateUrl: './mailbox.component.html',
    styleUrl: './mailbox.component.scss',
    encapsulation: ViewEncapsulation.None,
    providers: [
        MailboxService
    ]
})
export class MailboxComponent implements OnInit {
  @ViewChild('sidenav') sidenav: any;
  public sidenavOpen: boolean = true;
  public mails: Array<Mail>;
  public mail: any;
  public newMail: boolean;
  public type: string = 'all';
  public showSearch: boolean = false;
  public searchText: string;
  public form: FormGroup;

  constructor(public formBuilder: FormBuilder, public snackBar: MatSnackBar, private mailboxService: MailboxService) { }

  ngOnInit() {
    this.getMails();
    if (window.innerWidth <= 992) {
      this.sidenavOpen = false;
    }
    this.form = this.formBuilder.group({
      'to': ['', Validators.required],
      'cc': null,
      'subject': null,
      'message': null
    });
  }

  @HostListener('window:resize')
  public onWindowResize(): void {
    (window.innerWidth <= 992) ? this.sidenavOpen = false : this.sidenavOpen = true;
  }

  public getMails() {
    switch (this.type) {
      case 'all':
        this.mails = this.mailboxService.getAllMails();
        break;
      case 'starred':
        this.mails = this.mailboxService.getStarredMails();
        break;
      case 'sent':
        this.mails = this.mailboxService.getSentMails();
        break;
      case 'drafts':
        this.mails = this.mailboxService.getDraftMails();
        break;
      case 'trash':
        this.mails = this.mailboxService.getTrashMails();
        break;
      default:
        this.mails = this.mailboxService.getDraftMails();
    }
  }

  public viewDetail(mail: any) {
    this.mail = this.mailboxService.getMail(mail.id);
    this.mails.forEach(m => m.selected = false);
    this.mail.selected = true;
    this.mail.unread = false;
    this.newMail = false;
    if (window.innerWidth <= 992) {
      this.sidenav.close();
    }
  }

  public compose() {
    this.mail = null;
    this.newMail = true;
  }

  public setAsRead() {
    this.mail.unread = false;
  }

  public setAsUnRead() {
    this.mail.unread = true;
  }

  public delete() {
    this.mail.trash = true;
    this.mail.sent = false;
    this.mail.draft = false;
    this.mail.starred = false;
    this.getMails();
    this.mail = null;
  }

  public changeStarStatus() {
    this.mail.starred = !this.mail.starred;
    this.getMails();
  }

  public restore() {
    this.mail.trash = false;
    this.type = 'all';
    this.getMails();
    this.mail = null;
  }

  public onSubmit(mail: any) {
    console.log(mail)
    if (this.form.valid) {
      this.snackBar.open('Mail sent to ' + mail.to + ' successfully!', undefined, {
        duration: 2000,
      });
      this.form.reset();
    }
  }

}
