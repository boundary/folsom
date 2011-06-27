### folsom

Folsom is an Erlang based metrics system inspired by Coda Hale's metrics (https://github.com/codahale/metrics/). The metrics API's purpose is to collect realtime metrics from your Erlang applications and publish them via HTTP+JSON. folsom is *not* a persistent store. There are 5 types of metrics, counters, gauges, histograms (and timers), histories and meters. Metrics can be created, read, and updated via the `folsom_metrics` module.

#### Building and running

You need a (preferably recent) version of Erlang installed but that should be it.

       ./rebar get-deps
       ./rebar compile

* Note that ibrowse is only a dep for the test suite.

folsom can be run standalone or embedded in an Erlang application.

       $ erl -pa ebin deps/*/ebin

       > folsom:start(). % or start_link ...

By default folsom will listen on localhost, port 5565 and log to 'priv/log'. All three can be adjusted by changing environment variables FOLSOM_IP, FOLSOM_PORT and FOLSOM_LOG_DIR.

#### Metrics API

folsom_metrics.erl is the API module you will need to use most of the time.

Retreive a list of current installed metrics:

      > folsom_metrics:get_metrics().

Query a specific metric:

      > folsom_metrics:get_metric_value(Name).

##### Counters

Counter metrics provide increment and decrement capabilities for a single scalar value.

      > folsom_metrics:new_counter(Name).
      > folsom_metrics:notify({Name, {inc, Value}}).
      > folsom_metrics:notify({Name, {dec, Value}}).

##### Gauges

Gauges are point-in-time single value metrics.

      > folsom_metrics:new_gauge(Name).
      > folsom_metrics:notify({Name, Value}).

##### Histograms (and Timers)

Histograms are collections of values that have statistical analysis done to them, such as mean, min, max, kurtosis and percentile. The can be used like "timers" as well with the timed update functions.

      > folsom_metrics:new_histogram(Name).
      > folsom_metrics:histogram_timed_update(Name, Mod, Fun, Args).
      > folsom_metrics:histogram_timed_update(Name, Fun, Args).
      > folsom_metrics:histogram_timed_update(Name, Fun).
      > folsom_metrics:notify({Name, Value}).

##### Histories

Histories are a collection of past events, such as errors or log messages.

      > folsom_metrics:new_history(Name).
      > folsom_metrics:get_history_values(Name, Count). % get more than the default number of history items back
      > folsom_metrics:notify({Name, Value}).

##### Meters

Meters are increment only counters with mean rates and exponentially weighted moving averages applied to them, similar to a unix load average.

      > folsom_metrics:new_meter(Name).
      > folsom_metrics:notify({Name, Value}).

##### Erlang VM

folsom also produces Erlang VM statistics.

The result of `erlang:memory/0`:

       > folsom_vm_metrics:get_memory().

The result of `erlang:system_info/1`:

       > folsom_vm_metrics:get_system_info().

The result of `erlang:statistics/1`:

       > folsom_vm_metrics:get_statistics().
