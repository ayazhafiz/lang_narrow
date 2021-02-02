#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * An arbitrary value in the runtime.
 * A `_tagged_any` consists of a `tag` field tagging the concrete type of the
 * value and a `val` field holding the concrete value.
 */
typedef struct _tagged_any _tagged_any;

typedef struct _record {
  int numFields;
  const char** fields;
  _tagged_any* values;
} _record;

union _any {
  int nat;
  const char* string;
  int bool;
  _record _record;
} _any;

typedef struct _tag {
  unsigned v;
} _tag;

struct _tagged_any {
  int is_rcd;
  _tag tag;
  union _any val;
};

static const _tag _NAT = {.v = 0}, _STRING = {.v = 1}, _BOOL = {.v = 2};

_tagged_any _make_nat(int val) {
  union _any nat;
  nat.nat = val;
  _tagged_any result = {.is_rcd = 0, .tag = _NAT, .val = nat};
  return result;
}

_tagged_any _make_string(const char* val) {
  union _any string;
  string.string = val;
  _tagged_any result = {.is_rcd = 0, .tag = _STRING, .val = string};
  return result;
}

_tagged_any _make_bool(int val) {
  union _any bol;
  bol.bool = val;
  _tagged_any result = {.is_rcd = 0, .tag = _BOOL, .val = bol};
  return result;
}

_tagged_any _make_record(const _tag tag, int numFields, ...) {
  _record rcd;
  rcd.numFields = numFields;
  rcd.fields = malloc(numFields * sizeof(const char*));
  rcd.values = malloc(numFields * sizeof(_tagged_any));

  va_list lst;
  va_start(lst, numFields);
  for (int i = 0; i < numFields; ++i) {
    rcd.fields[i] = va_arg(lst, const char*);
    rcd.values[i] = va_arg(lst, _tagged_any);
  }

  union _any r;
  r._record = rcd;
  _tagged_any result = {.is_rcd = 1, .tag = tag, .val = r};
  return result;
}

int _is1(_tagged_any val, const _tag tag) { return val.tag.v == tag.v; }

int _is(_tagged_any val, const _tag* _any_of, int num_opts) {
  for (int i = 0; i < num_opts; ++i) {
    if (_is1(val, _any_of[i])) return 1;
  }
  return 0;
}

int _in(_tagged_any rcd, const char* field) {
  if (!rcd.is_rcd) {
    return 0;
  }
  _record r = rcd.val._record;
  for (int i = 0; i < r.numFields; ++i) {
    if (strcmp(r.fields[i], field) == 0) {
      return 1;
    }
  }
  return 0;
}

_tagged_any _record_proj(_tagged_any rcd, const char* field) {
  if (!rcd.is_rcd) {
    fprintf(stderr, "Runtime error: attempting to project %d\n", rcd.tag.v);
    exit(1);
  }
  _record r = rcd.val._record;
  for (int i = 0; i < r.numFields; ++i) {
    if (strcmp(r.fields[i], field) == 0) {
      return r.values[i];
    }
  }
  fprintf(stderr, "Runtime error: no field %s in record\n", field);
  exit(1);
}

void _print1(_tagged_any _any) {
  if (_is1(_any, _NAT)) {
    printf("%d", _any.val.nat);
  } else if (_is1(_any, _STRING)) {
    printf("%s", _any.val.string);
  } else if (_is1(_any, _BOOL)) {
    printf("%s", _any.val.bool == 1 ? "true" : "false");
  } else if (_any.is_rcd) {
    _record r = _any.val._record;
    printf("{");
    for (int i = 0; i < r.numFields; ++i) {
      printf("%s: ", r.fields[i]);
      _print1(r.values[i]);
      if (i != r.numFields - 1) {
        printf(", ");
      }
    }
    printf("}");
  } else {
    fprintf(stderr, "Runtime error: no matching tag %d\n", _any.tag.v);
    exit(1);
  }
}

void _print(_tagged_any _any) {
  _print1(_any);
  printf("\n");
}

// User code
const _tag ty_tag = {.v = 100};
_tagged_any defaultNat() {
  return _make_nat(1729);
}
_tagged_any readNat(_tagged_any n) {
  _tagged_any tmp;
  const _tag tags[] = {_NAT};
  if (_is(n, tags, 1)) {
    tmp = n;
  } else {
    tmp = defaultNat();
  }
  return tmp;
}
_tagged_any narrowB(_tagged_any p) {
  _tagged_any tmp;
  if (_in(p, "b")) {
    tmp = readNat(_record_proj(p, "b"));
  } else {
    _tagged_any tmp1;
    if (_in(p, "noBInMe")) {
      tmp1 = _record_proj(p, "noBInMe");
    } else {
      tmp1 = p;
    }
    tmp = tmp1;
  }
  return tmp;
}
int main() {
  _tagged_any result = narrowB(_make_record(ty_tag, 6, "b", _make_string("not a nat"), "c", _make_nat(9), "explanationA", _make_string("This record gains admission to narrowB as it is a subtype of"), "explanationB", _make_string("{b: string, c: nat}"), "explanationC", _make_string("Through a series of type narrowings the call lands at emission"), "explanationD", _make_string("of 1729 via defaultNat() because b is a string.")));
  _print(result);
}