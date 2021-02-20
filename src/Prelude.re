type trie =
  | Trie(option(int), char_to_children)
and char_to_children = list((char, trie));

let empty = [@implicit_arity] Trie(None, []);

let example =
  [@implicit_arity]
  Trie(
    None,
    [
      (
        'i',
        [@implicit_arity]
        Trie(
          Some(11),
          [('n', [@implicit_arity] Trie(Some(5), [('n', [@implicit_arity] Trie(Some(9), []))]))]
        )
      ),
      (
        't',
        [@implicit_arity]
        Trie(
          None,
          [
            (
              'e',
              [@implicit_arity]
              Trie(
                None,
                [
                  ('n', [@implicit_arity] Trie(Some(12), [])),
                  ('d', [@implicit_arity] Trie(Some(4), [])),
                  ('a', [@implicit_arity] Trie(Some(3), []))
                ]
              )
            ),
            ('o', [@implicit_arity] Trie(Some(7), []))
          ]
        )
      ),
      ('A', [@implicit_arity] Trie(Some(15), []))
    ]
  );
