# Blockade

## Table of contents
<!-- TOC -->
* [Blockade](#blockade)
  * [Description](#description)
  * [Setup](#setup)
    * [Elixir](#elixir)
    * [Erlang](#erlang)
  * [Example usage](#example-usage)
    * [Elixir](#elixir-1)
    * [Erlang](#erlang-1)
    * [Using priority option](#using-priority-option)
  * [Distributed environment dispatching](#distributed-environment-dispatching)
  * [Development](#development)
    * [Setup with Docker](#setup-using-docker)
    * [Running tests](#running-tests)

## Description
Blockade provides a simple way to queue and dispatch events to subscribers.

Blockade enables you to set a priority level on the event queue. This allows you to handle events later or discard them completely when dispatched with lower priority than the current priority level on the event queue. 
This can be handy in situations where you want to lock down the system and not process any events for some duration of time. In such cases, you can set a high priority level. Later, when the system is ready to handle events again, you can lower the priority level, and the queued events will be automatically dispatched (unless the `discard_events` option is set to `true`).

Blockade is built on top of the Erlang `pg` module, which means it will work in a distributed environment. Each node starts the `blockade` supervisor with the same name parameter. When dispatching events, the events will be received by all subscribers across the cluster.

Please read the documentation for the `blockade` module for more information on the available functions.

## Setup
Blockade can be started under supervision tree in your application.
When starting `blockade` you need to provide a name for the event queue. This name is used when dispatching events to the queue.

### Elixir

1. Add the following to your `mix.exs` file:
```elixir
{:blockade, "~> 0.1"},
```

2. Start `blockade` under your supervision tree
```elixir
alias :blockade, as: Blockade

  def init(_) do
    children = [
      {Blockade, %{name: :my_event_queue}},
    ]

    opts = [strategy: :one_for_one]
    Supervisor.init(children, opts)
  end
```

### Erlang
Add the following to your `rebar.config` file:

1. Add `blockade` as a dependency
```erlang
{deps, [{blockade, "0.1.0"}]}.
```

2. Start `blockade` under your supervision tree
```erlang
init(Args) ->
    Children = [
        #{id => blockade, start => {blockade, start_link, [#{name => my_event_queue}]}}
    ],

    {ok, {{one_for_one, 5, 10}, Children}}.
```


## Example usage

> **Note**
> When adding handlers to the event queue, the current process will be subscribed to the event queue. If subscribed multiple times, the process will receive the event multiple times.

### Elixir
```elixir
> alias :blockade, as: Blockade
> Blockade.add_handler(:my_event_queue, :event_key)
:ok
> Blockade.dispatch(:my_event_queue, :event_key, "event_payload")
:ok
> Blockade.dispatch(:my_event_queue, :event_key, "something_else")
:ok
> flush()
{:event_key, "event_payload"}
{:event_key, "something_else"}
```

### Erlang
```erlang
> blockade:add_handler(my_event_queue, event_key).
ok
> blockade:dispatch(my_event_queue, event_key, "event_payload").
ok
> blockade:dispatch(my_event_queue, event_key, "something_else").
ok
> flush().
{event_key, "event_payload"}
{event_key, "something_else"}
```

### Using priority option
```elixir
> alias :blockade, as: Blockade

# Subscribe current process to event with the key :some_event_key
> Blockade.add_handler(:my_event_queue, :some_event_key)
:ok

# Set priority to 100 and reset back to 0 (default) after 30 seconds. Do not discard any queued events. 
# If discard events is set to true, events with lower priority than the current priority will be discarded immediately after dispatching the event.
> Blockade.set_priority(:my_event_queue, 100, %{reset_after: 30000, discard_events: false}) 
:ok

# Immediately dispatch event with priority 100 and check local mailbox for any events. 
# There should be none if checked within 30 seconds after setting priority.
> Blockade.dispatch(:my_event_queue, :some_event_key, "priority_test")
:ok
> flush()
:ok

# Wait 30 seconds and check local mailbox again. There should be one event now.
> flush()
{:some_event_key, "priority_test"}

# Now lets try dispatching events with priority option.

# Set priority to 150 and discard any queued events with lower priority.
# This time the event queue will keep the priority and does not reset back to 0 because we did not provide the reset_after option.
> Blockade.set_priority(:my_event_queue, 150, %{discard_events: true})
:ok

> Blockade.dispatch(:my_event_queue, :some_event_key, "will_be_discarded", %{priority: 149})
:ok
> Blockade.dispatch(:my_event_queue, :some_event_key, "will_be_received prio 150", %{priority: 150})
:ok
> Blockade.dispatch(:my_event_queue, :some_event_key, {:my_data, "will_be_received prio 151"}, %{priority: 151})
:ok
> flush()
# We can see that the dispatched event with lower priority than 150 was discarded.
{:some_event_key, "will_be_received prio 150"}
{:some_event_key, {:my_data, "will_be_received prio 151"}}
```
We can also set the `discard_event` option to true per dispatch call meaning
the event will be discarded if priority level on the event queue is higher than the priority level on the dispatch call:
```elixir
> Blockade.set_priority(:my_event_queue, 150, %{discard_events: false})
> Blockade.dispatch(:my_event_queue, :some_event_key, "will_be_discarded", %{priority: 149, discard_event: true})
:ok
> flush()
# The event will be discarded and wont be received by the handler.
```

## Distributed environment dispatching
By default `blockade` will dispatch events to all subscribers across the cluster. If you want to dispatch to local handlers only, you can set the members option to local when dispatching the event:

```elixir
# This event will be received by all handlers across the cluster.
> Blockade.dispatch(:my_event_queue, :some_event_key, "global_dispatch", %{members: :global})
:ok

# This event will be received by local node handlers only.
> Blockade.dispatch(:my_event_queue, :some_event_key, "local_dispatch", %{members: :local})
:ok
```

> **Note**
> If dispatching events to all members across the cluster is not desired, you can start `blockade` with different names on different nodes. This way you can have multiple event queues across the cluster each completly isolated from each other. For example you can use the local node name as the event queue name which is unique across the cluster.

## Development

### Setup with Docker
- `docker-compose up -d`
- `docker exec -it blockade_blockade_1 sh`
- `rebar3 compile`

### Running tests
```bash
$ rebar3 compile && ct_run -dir test -logdir test_logs -pa ./_build/default/lib/blockade/ebin -setcookie cookievalue
```
