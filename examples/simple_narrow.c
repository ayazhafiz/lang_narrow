#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NAT "nat"
#define STRING "string"
#define BOOL "bool"

union any {
  int nat;
  const char* string;
  int bool;
} any;

typedef struct {
  const char* tag;
  union any val;
} tagged_any;

tagged_any make_nat(int val) {
  union any nat;
  nat.nat = val;
  tagged_any result = {.tag = NAT, .val = nat};
  return result;
}

tagged_any make_string(const char* val) {
  union any string;
  string.string = val;
  tagged_any result = {.tag = STRING, .val = string};
  return result;
}

tagged_any make_bool(int val) {
  union any bol;
  bol.bool = val;
  tagged_any result = {.tag = BOOL, .val = bol};
  return result;
}

int is(tagged_any val, const char* tag) {
  return (strcmp(val.tag, tag) == 0) ? 1 : 0;
}

void print(tagged_any any) {
  if (is(any, NAT)) {
    printf("%d\n", any.val.nat);
  } else if (is(any, STRING)) {
    printf("%s\n", any.val.string);
  } else {
    fprintf(stderr, "Runtime error: no matching tag %s\n", any.tag);
    exit(1);
  }
}

// User code
tagged_any _strToNat(tagged_any _s) {
  return make_nat(90);
}
tagged_any _dupBool(tagged_any _b) {
  return _b;
}
tagged_any _dupNat(tagged_any _n) {
  return _n;
}
tagged_any _narrow(tagged_any _p) {
  tagged_any _fresh_0;
    if (is(_p, NAT)) {
      _fresh_0 = _dupNat(_p);
  } else {
      tagged_any _fresh_1;
          if (is(_p, BOOL)) {
          _fresh_1 = _dupBool(_p);
    } else {
          _fresh_1 = _strToNat(_p);
    }
      _fresh_0 = _fresh_1;
  }
  return _fresh_0;
}
int main() {
  tagged_any __main_result = _narrow(make_nat(8));
  print(__main_result);
}