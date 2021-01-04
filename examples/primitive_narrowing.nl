fn strToNat(s: string): nat {
  90
}

fn dupBool(b: bool): bool {
  b
}

fn dupNat(n: nat): nat {
  n
}

fn narrow(p: string|nat|bool): nat|bool {
  if p is nat
    then dupNat(p)
    else if p is bool
         then dupBool(p)
         else strToNat(p)
}

narrow(8)
