# shinyasync (development version)

* `status_async()` now starts a `reactiveAsync()` that has not run yet, the
  same way reading its value does. Previously a UI that read the value only
  once the status was `"success"` never started the task (#1).

# shinyasync 0.1.0

First release.

* `reactiveAsync()`, `observeAsync()` and `reactiveValAsync()`: async
  counterparts of `reactive()`, `observe()` and `reactiveVal()` that evaluate
  on `mirai` daemons, track reactive dependencies, cancel superseded work and
  return the previous value while a new one is computed.
* `status_async()` and `cancel_async()` for spinners, disabled buttons and
  cancel buttons.
* `pool_async()` starts a daemon pool shared by every session in the process,
  reference-counted and shut down with the last session.
* `bindEvent()` methods so button-triggered async reactives update outputs.
* Dependency capture respects `isolate()`, keeps `req()`/`validate()` in the
  worker when they may not run, hoists reads inside formulas, ships helper
  closures with their own environments, and reports assignment to reactive
  values as an error.
