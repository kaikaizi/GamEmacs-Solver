This folder contains my attempts to solve classical (X)Emacs games.

* xmine-solver:
To run, `eval-buffer` and `xmine-solve`. Customable options contain:
   slowness, max-step and from xmine.el: xmine-width, xmine-height, %-of-mines.

* CYJL:
It's a Perl game but you can certainly run it under Emacs whatever. CYJL (成语接龙) chains Chinese proverb by searching one item whose leading character has same pronounciation as the trailing character of a given item. Character matching is prioritized, and if no item with given character is in database or has not already been used, it searches for characters having same pronounciation, ignoring accent. Note that many a character has more than one pronounciation (ignoring accent), and when searching such characters, other character matching any of potential pronounciation can be chosen. When no chaining is possible, it backtracks one step to retry.

To run, `perl CYJL.pl 200` will try to chain 200 pieces from a random starter; `perl CYJL 50 abcd` will start from Chengyu "abcd". Interpretation is given when queried with an item's index.

"ChineseChengyu.txt" and "ChinesePinyin.txt" are the database files needed.
