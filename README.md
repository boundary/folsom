### emetrics

emetrics is an Erlang based metrics system. It's purpose is to collect realtime metrics from various systems via REST and Erlang API's. The basic idea is that you create a metric via either API and then alert the metric with new values as you receive them. You can then retrieve information about these metrics, such as a histogram and the mean. The metric data is stored in memory in one of two kinds of samples, uniform and expotentially decaying, "exdec". Additionally, emetrics can produce Erlang VM metrics as JSON.

#### _metrics API Usage

Here are some examples in both HTTP and Erlang.

Create a exdec metric named "a" with a sample size of 5 values and an alpha of 1:

       > emetrics_event:add_handler(a, exdec, 5, 1).

       $ curl -X PUT http://localhost:5555/_metrics -d '{"id": "a", "size": 5, "type": "exdec", "alpha": 1}' -H 'Content-Type: application/json'

Create a uniform metric named "b" with a sample size of 5 values:

       > emetrics_event:add_handler(b, uniform, 5).

       $ curl -X PUT http://localhost:5555/_metrics -d '{"id": "b", "size": 5, "type": "uniform"}' -H 'Content-Type: application/json'

Query available metrics:

      > emetrics_event:get_handlers().

      $ curl -X GET http://localhost:5555/_metrics

Query a specific metric:

      > emetrics_event:get_all(a).

      $ curl -X GET http://localhost:5555/_metrics/a

Notify a metric of an event:

      > emetrics_event:notify({a, 1}).

      $ curl -X PUT http://localhost:5555/_metrics/a -d '{"value": 1}' -H 'Content-Type: application/json'

Delete a metric:

       > emetrics_event:delete_handler(a).

       $ curl -X DELETE http://localhost:5555/_metrics/a

#### _events API Usage

Create a event handler named "d" with a max size of 5 and two tags:

       > emetrics_events_event:add_handler(d, [test, blah], 5).

       $ curl -X PUT http://localhost:5555/_events -d '{"id": "d", "size": 5, "tags": ["test", "blah"]}' -H 'Content-Type: application/json'

Query available event handlers:

      > emetrics_events_event:get_handlers().

      $ curl http://localhost:5555/_events

Query available event handlers with additional info:

      > emetrics_events_event:get_handlers_info().

      $ curl http://localhost:5555/_events?info=true

Query handlers that have a specific tag assigned:

      > emetrics_events_event:get_tagged_handlers(test).

      $ curl http://localhost:5555/_events?tag=test

Query a specific event handler for events:

      > emetrics_events_event:get_events(d).

      $ curl http://localhost:5555/_events/d

Query a specific event handler for events with a limit:

      > emetrics_events_event:get_events(d, 1).

      $ curl http://localhost:5555/_events/d?limit=1

Query a specific event_handler for events with a tag:

      > emetrics_events_event:get_events(d, test).

      $ curl http://localhost:5555/_events/d?tag=test

Notify an event handler of an event:

      > emetrics_events_event:notify({d, [test], "something"}).

      $ curl -X PUT http://localhost:5555/_events/d -d '{"event": "something", "tags": ["test", "blah"]}' -H 'Content-Type: application/json'

Delete a event handler:

       > emetrics_events_event:delete_handler(d).

       $ curl -X DELETE http://localhost:5555/_events/d

#### Erlang VM Metrics:

The result of `erlang:memory/0`:

       > emetrics_vm_metrics:get_memory().

       $ curl http://localhost:5555/_memory

The result of `erlang:system_info/1`:

       > emetrics_vm_metrics:get_system_info().

       $ curl http://localhost:5555/_system

The result of `erlang:statistics/1`:

       > emetrics_vm_metrics:get_statistics().

       $ curl http://localhost:5555/_statistics
