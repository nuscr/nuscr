len(s) should give be implemented as an operator to take the length of the
string.

  $ nuscr PasswordManager.nuscr --project C@PasswordManager
  rec X [(silent) stored_pass<S>: string = "pass"] {
    Login(entered_pass: string) to S;
    choice at S {
      WrongPass(wrong: wrong:unit{(entered_pass)<>(stored_pass)}) from S;
      continue X;
    } or {
      Authenticated(correct: correct:unit{(entered_pass)=(stored_pass)}) from S;
      NewPass(new_pass: new_pass:string{(len(new_pass))>=(6)}) to S;
      continue X;
    }
  }
  
