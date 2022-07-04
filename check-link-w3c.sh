#!/bin/sh
## https://validator.w3.org/checklink/docs/checklink.html

checklink --quiet --summary --depth 2  --exclude='^mailto:' https://www.i3s.unice.fr/~malapert/R/
