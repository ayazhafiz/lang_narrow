fn a(p: {a: nat, b: nat}|{a: bool, b: bool}): bool {
  if p is {a: bool, b: bool}
     then p.a
     else false
}

a({a: true, b: false})
