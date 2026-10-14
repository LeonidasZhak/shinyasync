# shinyasync

> Async Shiny reactives on `mirai`, with the standard Shiny API.

`shinyasync` lets you move slow work off the Shiny session thread without changing how you write Shiny. Add `Async` to the reactive primitives you already use and the rest of your app stays the same.

| Shiny            | shinyasync            |
|------------------|-----------------------|
| `reactive()`     | `reactiveAsync()`     |
| `observe()`      | `observeAsync()`      |
| `reactiveVal()`  | `reactiveValAsync()`  |

Under the hood each one is a cancellable task, modelled on `ExtendedTask`, backed by a `mirai` daemon pool. You never see either.

## Why

Shiny's `ExtendedTask` makes an app responsive within a session and across concurrent users, but using it today means wiring three things by hand: an `ExtendedTask$new()` with a `mirai()` inside, argument capture via `environment()`, and a reference to the running mirai if you ever want to cancel. The result is boilerplate that looks nothing like ordinary Shiny code.

`shinyasync` collapses that into one call and keeps the reactive contract:

- dependencies are tracked like a normal reactive;
- when a dependency changes the in-flight task is cancelled and re-run;
- while a task runs, reading the reactive returns the previous value (stale-while-revalidate) or silently halts like `req()` when there is none;
- errors surface on the next read, as they would from any reactive.

Because they are real reactives, `bindEvent()`, `debounce()`, `req()`, `observe()` and modules all work unchanged.

## Installation

```r
# install.packages("pak")
pak::pak("dawiegriesel/shinyasync")
```

Requires R ≥ 4.1, shiny ≥ 1.8.1 and mirai ≥ 2.0.

## Quick start

```r
library(shiny)
library(shinyasync)

ui <- fluidPage(
  sliderInput("n", "Rows", 1e3, 1e7, 1e5),
  plotOutput("p"),
  textOutput("status")
)

server <- function(input, output, session) {
  pool_async(workers = 4)             # starts daemons; stopped with the session

  r <- reactiveAsync({
    x <- rnorm(input$n)               # runs in a worker, not the session
    density(x)
  })

  output$p      <- renderPlot(plot(r()))
  output$status <- renderText(status_async(r))
}

shinyApp(ui, server)
```

Drag the slider: the previous density stays on screen while the new one computes, and other users of the app are never blocked.

### Button-triggered

Use `bindEvent()` exactly as you would with `reactive()`:

```r
r <- reactiveAsync({ fit_model(input$formula, data()) }) |>
  bindEvent(input$go)
```

### Debounced input

```r
q <- debounce(reactive(input$query), 400)

r <- reactiveAsync({ search_db(q()) })
```

### Side effects

```r
observeAsync({
  write_audit_log(input$user, results())
})
```

### Background refresh

```r
r <- reactiveAsync({
  invalidateLater(60e3)
  fetch_dashboard_data()
})
```

### Manual control

```r
v <- reactiveValAsync()

observe({
  v(mirai::mirai(slow(x), x = input$x))   # resolves in the background
})

output$t <- renderTable(v())
```

## API

### `reactiveAsync(expr, mode = c("latest", "queue"))`

Returns a reactive. `expr` is evaluated in a worker. Reactive dependencies are captured on the session thread and their resolved values shipped to the worker.

- `mode = "latest"` (default): a new invalidation cancels the running task.
- `mode = "queue"`: tasks run in order, matching `ExtendedTask`'s default.

### `observeAsync(expr, ...)`

Like `observe()`, but `expr` runs in a worker. Returns an observer handle with `$status()` and `$cancel()`.

### `reactiveValAsync(value = NULL)`

A `reactiveVal` whose setter accepts a `mirai` (or a `promise`). The value updates when the task resolves.

### `status_async(r)`

Reactive status of an async reactive: `"idle"`, `"running"`, `"success"` or `"error"`. Use it for spinners and disabled buttons.

### `cancel_async(r)`

Stops the running task, if any.

### `pool_async(workers = NULL, ...)`

Starts a `mirai` daemon pool for the current session (or app, if called outside a session) and registers cleanup on `onStop()` / `session$onSessionEnded()`. Arguments pass through to `mirai::daemons()`, so remote or TLS-backed pools work as usual. If you already manage `daemons()` yourself, skip this.

## Deployment

`shinyasync` is deployment-agnostic. Each R process gets its own daemon pool, so under a multi-process load balancer such as [faucet](https://github.com/ixpantia/faucet) or Posit Connect you should size `workers` so that `processes × workers` fits the host, or point every process at one shared remote daemon set:

```r
pool_async(url = "tcp://daemon-host:5555")
```

## Design notes

- **Standard API first.** No new verbs. If a Shiny idiom works with `reactive()`, it should work here.
- **mirai only.** `crew` already covers autoscaling and heterogeneous workers; keeping to mirai keeps the package small.
- **Pure R.** The dispatch path is already native (nanonext/NNG, later); the time goes to the user's code in the worker. Rust or C++ would add install friction without measurable gain.
- **Cancellation is built in.** `ExtendedTask` has no cancel method; `shinyasync` holds the mirai reference and calls `stop_mirai()` for you.

## Status

Pre-alpha. The API above is the target; expect changes before 0.1.0.

## Related

- [mirai](https://mirai.r-lib.org/) — the async engine
- [Shiny ExtendedTask](https://shiny.posit.co/r/reference/shiny/latest/extendedtask.html) — the mechanism this wraps
- [crew](https://wlandau.github.io/crew/) — autoscaling worker pools, if you outgrow this
- [faucet](https://github.com/ixpantia/faucet) — multi-process load balancing for Shiny and Plumber

## License

MIT
