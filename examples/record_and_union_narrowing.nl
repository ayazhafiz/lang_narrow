fn defaultNat(): nat {
  1729
}

fn readNat(n: nat|string): nat {
  if n is nat
     then n
     else defaultNat()
}

fn narrowB(p: nat|{a: bool, b: nat}|{b: string, c: nat}|{noBInMe: nat}): nat {
  if b in p
     then readNat(p.b)
     else if noBInMe in p
          then p.noBInMe
          else p
}

fn asdasdasd(): {a: nat} { {a: 1} }

narrowB(
  { explanationA: "This record gains admission to narrowB as it is a subtype of"
  , explanationB: "{b: string, c: nat}"
  , explanationC: "Through a series of type narrowings the call lands at emission"
  , explanationD: "of 1729 via defaultNat() because b is a string."

  , b: "not a nat"
  , c: 9
  })
