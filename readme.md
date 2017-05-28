[online](https://tonyday567.github.io/online/index.html) [![Build Status](https://travis-ci.org/tonyday567/online.png)](https://travis-ci.org/tonyday567/online)
===

online turns a statistic (a summary or fold of data) into an online
algorithm.

See https://tonyday567.github.io/online/index.html for motivation.

recipe
---

    stack build --copy-bins --exec  "online-examples" --exec "pandoc -f markdown -t html -i examples/examples.md -o index.html --mathjax --filter pandoc-include"
