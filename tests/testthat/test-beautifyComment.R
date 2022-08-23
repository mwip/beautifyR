test_that("don't beautify one word", {
  inputstring <- "# test"
  outputstring <- "# test"
  expect_equal(beautifyComment(inputstring), outputstring)
})

test_that("Beautify lorem ipsum", {
  inputstring <- "# Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  outputstring <- "# Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
# tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
# quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
# consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
# cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
# non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  expect_equal(beautifyComment(inputstring), outputstring)
})

test_that("beautify comment with `code`", {
  inputstring <- "# This is my dash-dash word. And this right here is inline code `r mean(c(1,2))` that is
# separated into two lines when beautifying this comment."
  outputstring <- "# This is my dash-dash word. And this right here is inline code
# `r mean(c(1,2))` that is separated into two lines when beautifying this
# comment."
  expect_equal(beautifyComment(inputstring), outputstring)
})

test_that("beautify comment with `backticked` words", {
  inputstring <- "#' Bzgl. der Behandlungseffekte ist der Versuch prinzipiell 3 faktoriell, allerdings ist `Sorte` geschachtelt im Effekt `Typ`, da jede Sorte nur
#' genau zu einem Typ gehört und ein Sorten-übergreifender Effekt des Typs
#' explizit untersucht werden soll. Somit gibt es die festen Effekte
#' `Intensität`, `Saatzeit`, `Typ` und `Typ:Sorte` und all deren Wechselwirkungen. Bzgl. der Designeffekte werden alle oben genannten
#' Effekte als zufällig ins Modell aufgenommen: Wiederholung (`Wdh`),
#' Großteilstück (`GT`), Kleinteilstück (`KT`), und Doppelplot (`DP`)."
  outputstring <- "#' Bzgl. der Behandlungseffekte ist der Versuch prinzipiell 3 faktoriell,
#' allerdings ist `Sorte` geschachtelt im Effekt `Typ`, da jede Sorte nur
#' genau zu einem Typ gehört und ein Sorten-übergreifender Effekt des Typs
#' explizit untersucht werden soll. Somit gibt es die festen Effekte
#' `Intensität`, `Saatzeit`, `Typ` und `Typ:Sorte` und all deren
#' Wechselwirkungen. Bzgl. der Designeffekte werden alle oben genannten
#' Effekte als zufällig ins Modell aufgenommen: Wiederholung (`Wdh`),
#' Großteilstück (`GT`), Kleinteilstück (`KT`), und Doppelplot (`DP`)."
  expect_equal(beautifyComment(inputstring), outputstring)
})

test_that("beautify comment with code and links", {
  inputstring <- "#' Sidenote: One can also use the `dispformula = ~ 0` argument to set the error
#' variance to 0. Technically it is not being set to exactly 0, but to a very
#' small value defined as `sqrt(.Machine$double.eps)` (find more on this
#' [here](https://github.com/glmmTMB/glmmTMB/issues/653) and [here](https://schmidtpaul.github.io/MMFAIR/glmmtmbdispformula0.html)). As a consequence,
#' this can be seen as an alternative way of formulating the model for the special case of setting 
#' the error variance to 0. It was shown [here](https://github.com/glmmTMB/glmmTMB/issues/833#issuecomment-1182840849) that the resulting model is
#' indeed equivalent to fixing the error variance via the method shown above to
#' `start = list(betad = log(sqrt(.Machine$double.eps)))`."
  outputstring <- "#' Sidenote: One can also use the `dispformula = ~ 0` argument to set the
#' error variance to 0. Technically it is not being set to exactly 0, but to a
#' very small value defined as `sqrt(.Machine$double.eps)` (find more on this
#' [here](https://github.com/glmmTMB/glmmTMB/issues/653) and
#' [here](https://schmidtpaul.github.io/MMFAIR/glmmtmbdispformula0.html)). As
#' a consequence, this can be seen as an alternative way of formulating the
#' model for the special case of setting the error variance to 0. It was shown
#' [here](https://github.com/glmmTMB/glmmTMB/issues/833#issuecomment-1182840849)
#' that the resulting model is indeed equivalent to fixing the error variance
#' via the method shown above to
#' `start = list(betad = log(sqrt(.Machine$double.eps)))`."
  expect_equal(beautifyComment(inputstring), outputstring)
})
