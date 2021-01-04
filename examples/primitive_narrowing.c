#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NAT "nat"
#define STRING "string"
#define BOOL "bool"
#define RECORD "record"

typedef struct tagged_any tagged_any;
void print(tagged_any);

typedef struct record {
  int numFields;
  const char** fields;
  tagged_any* values;
} record;

union any {
  int nat;
  const char* string;
  int bool;
  record record;
} any;

struct tagged_any {
  const char* tag;
  union any val;
};

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

tagged_any make_record(int numFields, ...) {
  record rcd;
  rcd.numFields = numFields;
  rcd.fields = malloc(numFields * sizeof(const char*));
  rcd.values = malloc(numFields * sizeof(tagged_any));

  va_list lst;
  va_start(lst, numFields);
  for (int i = 0; i < numFields; ++i) {
    rcd.fields[i] = va_arg(lst, const char*);
    rcd.values[i] = va_arg(lst, tagged_any);
  }

  union any r;
  r.record = rcd;
  tagged_any result = {.tag = RECORD, .val = r};
  return result;
}

int is(tagged_any val, const char* tag) {
  return (strcmp(val.tag, tag) == 0) ? 1 : 0;
}

int in(tagged_any rcd, const char* field) {
  if (!is(rcd, RECORD)) {
    return 0;
  }
  record r = rcd.val.record;
  for (int i = 0; i < r.numFields; ++i) {
    if (strcmp(r.fields[i], field) == 0) {
      return 1;
    }
  }
  return 0;
}

tagged_any record_proj(tagged_any rcd, const char* field) {
  if (!is(rcd, RECORD)) {
    fprintf(stderr, "Runtime error: attempting to project %s\n", rcd.tag);
    exit(1);
  }
  record r = rcd.val.record;
  for (int i = 0; i < r.numFields; ++i) {
    if (strcmp(r.fields[i], field) == 0) {
      return r.values[i];
    }
  }
  fprintf(stderr, "Runtime error: no field %s in record\n", field);
  exit(1);
}

void _print(tagged_any any) {
  if (is(any, NAT)) {
    printf("%d", any.val.nat);
  } else if (is(any, STRING)) {
    printf("%s", any.val.string);
  } else if (is(any, BOOL)) {
    printf("%s", any.val.bool == 1 ? "true" : "false");
  } else if (is(any, RECORD)) {
    record r = any.val.record;
    printf("{");
    for (int i = 0; i < r.numFields; ++i) {
      printf("%s: ", r.fields[i]);
      _print(r.values[i]);
      if (i != r.numFields - 1) {
        printf(", ");
      }
    }
    printf("}");
  } else {
    fprintf(stderr, "Runtime error: no matching tag %s\n", any.tag);
    exit(1);
  }
}

void print(tagged_any any) {
  _print(any);
  printf("\n");
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