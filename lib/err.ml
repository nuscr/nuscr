

(** UserError is a user error and should be reported back so it can be fixed *)
exception UserError of string

(** A Violation is reported when an impossible state was reached. It
   has to be considered a bug even when the fix is to change the
   Violation to a user error *)
exception Violation of string
