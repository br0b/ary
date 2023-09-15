#include "ary.h"
#include <stdlib.h>
#include <math.h>

// Definicje struktur pomocniczych

// Struktura nice_wartosc reprezentuje to samo co wartosc,
// ale pozwala na łatwiejsze mnożenie i dzielenie.
typedef struct {
  wartosc ranges[3];
} nice_wartosc;

// Defnicje stałych

const wartosc EMPTY_SET = { .left = 1, .right = -1, .is_single_range = false };
const wartosc ZERO = { .left = 0, .right = 0, .is_single_range = true };
const wartosc REALS = { .left = -HUGE_VAL, .right = HUGE_VAL, .is_single_range = true };

// Deklaracje funkcji pomocniczych

bool is_zero(double x);
bool is_wartosc_zero(wartosc war);
bool is_empty(wartosc war);
int compare_ranges(const void *wa, const void *wb);
double min(double x, double y);
double max(double x, double y);
double razy_bounds (double x, double y);
wartosc get_left_range(wartosc war);
wartosc get_right_range(wartosc war);
wartosc get_opposite(wartosc war);
wartosc range_plus_range_pair(wartosc r, wartosc rp);
wartosc razy_nice(nice_wartosc nwa, nice_wartosc nwb);
wartosc razy_range(wartosc wa, wartosc wb);
wartosc inverse_range(wartosc range);
wartosc range_union(wartosc wa, wartosc wb);
nice_wartosc get_nice_wartosc(wartosc war);
nice_wartosc inverse_nice(nice_wartosc nwar);

// Definicje funkcji z ary.h

wartosc wartosc_dokladnosc(double x, double p) {
  wartosc war = EMPTY_SET;
  x = (is_zero(x) ? 0 : x);
  p = (is_zero(p) ? 0 : p);
  war.left = x - (p / 100) * fabs(x);
  war.right = x + (p / 100) * fabs(x);
  war.is_single_range = true;
  return war;
}

wartosc wartosc_od_do(double x, double y) {
  wartosc war = EMPTY_SET;
  war.left = (is_zero(x) ? 0 : x);
  war.right = (is_zero(y) ? 0 : y);
  war.is_single_range = true;
  return war;
}

wartosc wartosc_dokladna(double x) {
  wartosc war = EMPTY_SET;
  x = (is_zero(x) ? 0 : x);
  war.left = x;
  war.right = x;
  war.is_single_range = true;
  return war;
}

bool in_wartosc(wartosc w, double x) {
  if (is_empty(w)) {
    return false;
  }
  else if (w.is_single_range) {
    return w.left <= x && x <= w.right;
  }
  else {
    return x <= w.left || w.right <= x;
  }
}

double min_wartosc(wartosc w) {
  if (is_empty(w)) {
    return NAN;
  }
  else if (w.is_single_range) {
    return w.left;
  }
  else {
    return -HUGE_VAL;
  }
}

double max_wartosc(wartosc w) {
  if (is_empty(w)) {
    return NAN;
  }
  else if (w.is_single_range) {
    return w.right;
  }
  else {
    return HUGE_VAL;
  }
}

double sr_wartosc(wartosc w) {
  if (isinf(w.left) || isinf(w.right)) {
    return NAN;
  }
  return (min_wartosc(w) + max_wartosc(w)) / 2;
}

wartosc plus(wartosc a, wartosc b) {
  if (is_empty(a) || is_empty(b)) {
    return EMPTY_SET;
  }
  wartosc sum = EMPTY_SET;
  if (a.is_single_range && b.is_single_range) {
    sum.left = a.left + b.left;
    sum.right = a.right + b.right;
    sum.is_single_range = true; 
  }
  else if (a.is_single_range && !b.is_single_range) {
    sum = range_plus_range_pair(a, b);
  }
  else if (!a.is_single_range && b.is_single_range) {
    sum = range_plus_range_pair(b, a);
  }
  else {
    sum = REALS;
  } 
  return sum;
}

wartosc minus(wartosc a, wartosc b) {
  if (is_empty(a) || is_empty(b)) {
    return EMPTY_SET;
  }
  return plus(a, get_opposite(b));
}

wartosc razy(wartosc a, wartosc b) {
  if (is_empty(a) || is_empty(b)) {
    return EMPTY_SET;
  }
  nice_wartosc na = get_nice_wartosc(a);
  nice_wartosc nb = get_nice_wartosc(b);
  return razy_nice(na, nb);
}

wartosc podzielic(wartosc a, wartosc b) {
  if (is_wartosc_zero(b) || is_empty(a) || is_empty(b)) {
    return EMPTY_SET;
  }
  else {
    nice_wartosc na = get_nice_wartosc(a);
    nice_wartosc nb = inverse_nice(get_nice_wartosc(b));
    return razy_nice(na, nb);
  }
}

// Definicje funkcji pomocniczych

bool is_zero(double x) { return fabs(x) < 1e-10; }

// Funkcja sprawdza, czy zbiór wartości reprezentowany przez war to {0}.
bool is_wartosc_zero(wartosc war) {
  return is_zero(war.left)
         && is_zero(war.right);
}

// Funkcja sprawdza, czy war jest zbiorem pustym.
bool is_empty(wartosc war) {
  return war.left > war.right;
}

// Funkcja porównuje dwa obiekty typu wartosc i zwraca -1, 0 lub 1
// wtedy gdy *ra jest odpowiednio mniejsze, równe lub większe niż *rb.
// Zbiór pusty jest traktowany jako mniejszy od wszystkich innych zbiorów i
// równy sam sobie.
int compare_ranges(const void *ra, const void *rb) {
  wartosc _ra = *(wartosc *)ra;
  wartosc _rb = *(wartosc *)rb;
  if (is_empty(_ra) || is_empty(_rb)) {
    if (!is_empty(_rb)) {
      return -1;
    }
    else if (!is_empty(_ra)) {
      return 1;
    }
    else {
      return 0;
    }
  }
  else {
    if (_ra.left < _rb.left) {
      return -1;
    }
    else if (_ra.left > _rb.left) {
      return 1;
    }
    else {
      return 0;
    }
  }
}

double min(double x, double y) {return x < y ? x : y;}
double max(double x, double y) {return x >= y ? x : y;}

// Mnożenie kresów dwóch różnych przedziałów
// uwzględniające x lub y równe (-)HUGE_VAL
double razy_bounds (double x, double y) {
  if (is_zero(x) || is_zero(y)) {
    return 0;
  }
  return x * y;
}

// Funkcja przyjmuje war, dla którego war.is_single_range == false.
wartosc get_left_range(wartosc war) {
  wartosc range = { .left = -HUGE_VAL,
                    .right = war.left,
                    .is_single_range = true };
  return range;
}

// Funkcja przyjmuje war, dla którego war.is_single_range == false.
wartosc get_right_range(wartosc war) {
  wartosc range = { .left = war.right, 
                    .right = HUGE_VAL, 
                    .is_single_range = true };
  return range;
}

// Funkcja zwraca obiekt typu wartosc zawierający liczby przeciwne do
// liczb należących do zbioru reprezentowanego przez war.
wartosc get_opposite(wartosc war) {
  if (is_empty(war)) {
    return war;
  }
  wartosc owar = { .left = -war.right,
                   .right = -war.left,
                   .is_single_range = war.is_single_range };
  return owar;
}

// Funkcja zwraca sumę r i rp, dla których r.is_single_range == true
// oraz rp.is_single_range == false.
wartosc range_plus_range_pair(wartosc r, wartosc rp) {
  double sum_left = rp.left + r.right;
  double sum_right = rp.right + r.left;
  if (sum_left < sum_right) {
    wartosc sum = { .left = sum_left,
                    .right = sum_right,
                    .is_single_range = false };
    return sum;
  }
  else {
    return REALS;
  }
}

wartosc razy_nice(nice_wartosc nwa, nice_wartosc nwb) {
  wartosc range_products[9];
  for (int i = 0; i < 3; ++i) {
    for (int j = 0; j < 3; ++j) {
      range_products[3 * i + j] = razy_range(nwa.ranges[i], nwb.ranges[j]);
    }
  }
  qsort(range_products, 9, sizeof(*range_products), compare_ranges);
  wartosc product = range_products[0];
  for (int i = 1; product.is_single_range && i < 9; ++i) {
    product = range_union(product, range_products[i]);
  }
  return product;
}

// Obiekty wa i wb to przedziały liczb albo tylko nieujemnych
// albo tylko niedodatnich. Funkcja zwraca przedział.
wartosc razy_range(wartosc wa, wartosc wb) {
  if (is_empty(wa) || is_empty(wb)) {
    return EMPTY_SET;
  }
  double products[4] = { razy_bounds(wa.left, wb.left),
                         razy_bounds(wa.left, wb.right),
                         razy_bounds(wa.right, wb.left),
                         razy_bounds(wa.right, wb.right) };
  double left = min(min(min(products[0], products[1]), products[2]), products[3]);
  double right = max(max(max(products[0], products[1]), products[2]), products[3]);
  wartosc product = { .left = left,
                      .right = right,
                      .is_single_range = true };
  return product;
}

// Funkcja zwraca przedział liczb odwrotnych do liczb należących do przedziału
// [range.left, range.right] \ {0}.
wartosc inverse_range(wartosc range) {
  if (is_empty(range)) {
    return range;
  }
  if (range.left < 0) {
    wartosc oprange = get_opposite(range);
    return get_opposite(inverse_range(oprange));
  }
  if (is_zero(range.right)) {
    return EMPTY_SET;
  }
  wartosc inversed_range = EMPTY_SET;
  if (is_zero(range.left)) {
    inversed_range.right = HUGE_VAL;
  }
  else {
    inversed_range.right = 1 / range.left;
  }
  inversed_range.left = 1 / range.right;
  inversed_range.is_single_range = true;
  return inversed_range;
}

// Funkcja przyjmnuje wa i wb reprezentujące przedziały,
// dla których wa.left <= wb.left. Jeżeli zbiory reprezentowane przez wa i wb
// 1. mają częśc wspólną, to funkcja zwraca ich sumę w sensie teorii mnogości.
// 2. są rozłączne, to funkcja zwraca obiekt typu wartosc, który reprezentuje
// sumę mnogościową przedziałów (-\infty, wa.right] oraz [wb.left, \infty).
wartosc range_union(wartosc wa, wartosc wb) {
  if (!is_empty(wa) && !is_empty(wb)) {
    wartosc waru = EMPTY_SET;
    if (wa.right < wb.left) {
      waru.left = wa.right;
      waru.right = wb.left;
      waru.is_single_range = false;
    }
    else {
      waru.left = wa.left;
      waru.right = max(wa.right, wb.right);
      waru.is_single_range = true;
    }
    return waru;
  }
  else if (is_empty(wa)) {
    return wb;
  }
  else return wa;
}

// Funkcja tworzy obiekt typu nice_wartosc reprezentujący ten sam zbiór liczb
// co war.
nice_wartosc get_nice_wartosc(wartosc war) {
  nice_wartosc nwar = { .ranges[0] = EMPTY_SET,
                        .ranges[1] = EMPTY_SET,
                        .ranges[2] = EMPTY_SET };
  wartosc *ra = &nwar.ranges[0];
  wartosc *rb = &nwar.ranges[1];
  wartosc *rc = &nwar.ranges[2];
  if (war.is_single_range) {
    if (war.left < 0 && 0 < war.right) {
      ra->left = war.left,
      ra->right = 0,
      ra->is_single_range = true;
      rb->left = 0,
      rb->right = war.right,
      rb->is_single_range = true;
      *rc = EMPTY_SET;
    }
    else {
      *ra = war;
      *rb = EMPTY_SET;
      *rc = EMPTY_SET;
    }
  }
  else {
    wartosc rl = get_left_range(war);
    wartosc rr = get_right_range(war);
    nice_wartosc rln = get_nice_wartosc(rl);
    nice_wartosc rrn = get_nice_wartosc(rr);
    if (is_empty(rln.ranges[1])) {
      *ra = rln.ranges[0];
      *rb = rrn.ranges[0];
      *rc = rrn.ranges[1];
    }
    else {
      *ra = rln.ranges[0];
      *rb = rln.ranges[1];
      *rc = rrn.ranges[0];
    }
  }
  return nwar;
}

// Funkcja zwraca obiekt typu nice_wartosc, który reprezentuje zbiór
// zawierający wszystkie liczby odwrotne do liczb należących do zbioru
// reprezentowanego przez nwar, z wyłączeniem zera.
nice_wartosc inverse_nice(nice_wartosc nwar) {
  nice_wartosc inversed = { .ranges[0] = inverse_range(nwar.ranges[0]),
                            .ranges[1] = inverse_range(nwar.ranges[1]),
                            .ranges[2] = inverse_range(nwar.ranges[2]) };
  return inversed;
}
