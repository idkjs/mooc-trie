open Prelude
let rec children_from_char = (m, c) =>
  switch m {
  | [] => None
  | [(c', t), ...xs] =>
    if (c' == c) {
      Some(t);
    } else {
      children_from_char(xs, c);
    }
  };

let update_children = (m, c, t) =>
  if (children_from_char(m, c) == None) {
    m @ [(c, t)];
  } else {
    let rec help = (ml) =>
      switch ml {
      | [] => []
      | [(c', t'), ...xs] =>
        if (c' == c) {
          [(c, t), ...xs];
        } else {
          [(c', t'), ...help(xs)];
        }
      };
    help(m);
  };

let lookup = (trie, w) => {
  let rec find = (t, idx) => {
    let [@implicit_arity] Trie(value, children) = t;
    if (idx == String.length(w)) {
      value;
    } else {
      switch (children_from_char(children, w.[idx])) {
      | None => None
      | Some(t') => find(t', idx + 1)
      };
    };
  };
  find(trie, 0);
};

let rec insert = (trie, w, v) => {
  let slen = String.length(w)
  and [@implicit_arity] Trie(v', m) = trie;
  if (slen == 0) {
    [@implicit_arity] Trie(Some(v), m);
  } else {
    let c = w.[0]
    and s = String.sub(w, 1, slen - 1);
    let ot = children_from_char(m, c);
    switch ot {
    | Some(t) => [@implicit_arity] Trie(v', update_children(m, c, insert(t, s, v)))
    | None => [@implicit_arity] Trie(v', update_children(m, c, insert(empty, s, v)))
    };
  };
};

let insert2 = (trie, w, v) => {
  let len = String.length(w);
  let rec help = (ctc, pos) => {
    let ch = w.[pos];
    switch (children_from_char(ctc, ch)) {
    | Some(t) =>
      let [@implicit_arity] Trie(x, ctc') = t;
      if (pos == len - 1) {
        update_children(ctc, ch, [@implicit_arity] Trie(Some(v), ctc'));
      } else {
        update_children(ctc, ch, [@implicit_arity] Trie(x, help(ctc', pos + 1)));
      };
    | None =>
      let rec loop = (i) =>
        if (i == len - 1) {
          [(w.[i], [@implicit_arity] Trie(Some(v), []))];
        } else {
          [(w.[i], [@implicit_arity] Trie(None, loop(i + 1)))];
        };
      if (pos == len - 1) {
        update_children(ctc, ch, [@implicit_arity] Trie(Some(v), []));
      } else {
        update_children(ctc, ch, [@implicit_arity] Trie(None, loop(1 + pos)));
      };
    };
  };
  let [@implicit_arity] Trie(num, ctc) = trie;
  if (len == 0) {
    [@implicit_arity] Trie(Some(v), ctc);
  } else {
    [@implicit_arity] Trie(num, help(ctc, 0));
  };
};
