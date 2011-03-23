### emetrics

emetrics is an Erlang based metrics system. It's purpose is to collect realtime metrics from various systems via REST and Erlang API's. The basic idea is that you create a metric via either API and then alert the metric with new values as you receive them. You can then retrieve information about these metrics, such as a histogram and the mean. The metric data is stored in memory in one of two kinds of samples, uniform and expotentially decaying, "exdec". Additionally, emetrics can produce Erlang VM metrics as JSON.

#### Usage

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

Erlang VM Metrics:

> emetrics_vm_metrics:get_memory().
$ curl http://localhost:5555/_memory

> emetrics_vm_metrics:get_system_info().
$ curl http://localhost:5555/_system

> emetrics_vm_metrics:get_statistics().
$ curl http://localhost:5555/_statistics
