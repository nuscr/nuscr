len(s) should give be implemented as an operator to take the length of the
string.

  $ nuscr PasswordManager.nuscr --project C@PasswordManager --show-solver-queries
  (declare-const freshvar$0 String)
  (declare-const stored_pass String)
  (assert (not (>= (str.len stored_pass) 6)))
  (assert (= freshvar$0 "password"))
  (assert (= freshvar$0 stored_pass))
  (check-sat)
  
  (declare-const correct Int)
  (declare-const entered_pass String)
  (declare-const new_pass String)
  (declare-const stored_pass String)
  (assert (not (>= (str.len stored_pass) 6)))
  (assert (>= (str.len new_pass) 6))
  (assert (= new_pass stored_pass))
  (assert (>= (str.len stored_pass) 6))
  (assert (>= (str.len new_pass) 6))
  (assert (= entered_pass stored_pass))
  (check-sat)
  
  rec X [(silent) stored_pass<S>: stored_pass:string{(len(stored_pass))>=(6)} = "password"] {
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
  

PasswordManager2 has a too short initial password, and an error is expected.

  $ nuscr PasswordManager2.nuscr --project C@PasswordManager --show-solver-queries
  (declare-const freshvar$0 String)
  (declare-const stored_pass String)
  (assert (not (>= (str.len stored_pass) 6)))
  (assert (= freshvar$0 "pass"))
  (assert (= freshvar$0 stored_pass))
  (check-sat)
  
  nuscr: User error: Type Error: Expression "pass" should be of type stored_pass:string{(len(stored_pass))>=(6)}
  [1]
