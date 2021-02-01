#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * An arbitrary value in the runtime.
 * A `tagged_any` consists of a `tag` field tagging the concrete type of the
 * value and a `val` field holding the concrete value.
 */
typedef struct tagged_any tagged_any;

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
  int is_rcd;
  const char* tag;
  union any val;
};

static const char *NAT = "nat", *STRING = "string", *BOOL = "bool";

tagged_any make_nat(int val) {
  union any nat;
  nat.nat = val;
  tagged_any result = {.is_rcd = 0, .tag = NAT, .val = nat};
  return result;
}

tagged_any make_string(const char* val) {
  union any string;
  string.string = val;
  tagged_any result = {.is_rcd = 0, .tag = STRING, .val = string};
  return result;
}

tagged_any make_bool(int val) {
  union any bol;
  bol.bool = val;
  tagged_any result = {.is_rcd = 0, .tag = BOOL, .val = bol};
  return result;
}

tagged_any make_record(const char* tag, int numFields, ...) {
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
  tagged_any result = {.is_rcd = 1, .tag = tag, .val = r};
  return result;
}

int is1(tagged_any val, const char* tag) {
  return strcmp(val.tag, tag) == 0 ? 1 : 0;
}

int is(tagged_any val, const char* const* any_of, int num_opts) {
  for (int i = 0; i < num_opts; ++i) {
    if (is1(val, any_of[i])) return 1;
  }
  return 0;
}

int in(tagged_any rcd, const char* field) {
  if (!rcd.is_rcd) {
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
  if (!rcd.is_rcd) {
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
  if (is1(any, NAT)) {
    printf("%d", any.val.nat);
  } else if (is1(any, STRING)) {
    printf("%s", any.val.string);
  } else if (is1(any, BOOL)) {
    printf("%s", any.val.bool == 1 ? "true" : "false");
  } else if (any.is_rcd) {
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
tagged_any _a(tagged_any _p) {
  tagged_any _fresh_0;
  const char* _fresh_1[] = {STRING, BOOL};
  if (is(_p, _fresh_1, 2)) {
    _fresh_0 = make_nat(0);
  } else {
    _fresh_0 = _p;
  }
  return _fresh_0;
}
int main() {
  tagged_any __main_result = _a(make_string("h"));
  print(__main_result);
}