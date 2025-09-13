export class ConfirmDialogModel {

    constructor(public title: string, public message: string, public secondConfirmButtonLabel: string, public cancelButtonLabel: string = 'No', public confirmButtonLabel: string = 'Yes', public includeSecondConfirmBtn: boolean = false) {
    }
  }