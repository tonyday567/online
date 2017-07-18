[online](https://tonyday567.github.io/online/index.html) [![Build Status](https://travis-ci.org/tonyday567/online.png)](https://travis-ci.org/tonyday567/online)
===

online turns a statistic (a summary or fold of data) into an online
algorithm.

See https://tonyday567.github.io/online/index.html for motivation.

recipe
---

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/online-examples" --exec "$(stack path --local-bin)/pandoc -f markdown -i example/examples.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
