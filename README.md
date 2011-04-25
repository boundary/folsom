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

      $ curl http://localhost:5565/_metrics

Query a specific metric:

      > folsom_metrics:get_metric_value(Name).

      $ curl http://localhost:5565/_metrics/name

##### Counters

Counter metrics provide increment and decrement capabilities for a single scalar value.

      > folsom_metrics:new_counter(Name).
      > folsom_metrics:notify({Name, {inc, Value}}).
      > folsom_metrics:notify({Name, {dec, Value}}).

##### Gauges

Gauges are "point in time" single value metrics.

      > folsom_metrics:new_gauge(Name).
      > folsom_metrics:notify({Name, Value}).

##### Histograms (and Timers)

Histograms are collections of values that have statistical analysis done to them, such as mean, min, max, kurtosis and percentile. One can use use these like "timers" as well with the timed update functions.

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

Meters are increment only counters with mean rates and exponentially-weighted moving averages applied to them, similar to a unix load average.

      > folsom_metrics:new_meter(Name).
      > folsom_metrics:notify({Name, Value}).

##### Erlang VM

folsom also produces Erlang VM statistics.

The result of `erlang:memory/0`:

       > folsom_vm_metrics:get_memory().

       $ curl http://localhost:5565/_memory

The result of `erlang:system_info/1`:

       > folsom_vm_metrics:get_system_info().

       $ curl http://localhost:5565/_system

The result of `erlang:statistics/1`:

       > folsom_vm_metrics:get_statistics().

       $ curl http://localhost:5565/_statistics

#### Sample Output

Stats for metric 'a':

      $ curl http://localhost:5565/_metrics/a

      {"min":1,"max":1000,"mean":322.2,"median":100,"variance":185259.19999999998,"standard_deviation":430.4174717643325,"skewness":1.2670136514902162,"kurtosis":-1.2908313302242205,"percentile":{"75":500,"95":1000,"99":1000,"999":1000},"histogram":{"10":2,"20":0,"30":0,"50":0,"100":1,"200":0,"300":0,"400":0,"500":1,"1000":1,"99999999999999":0}}

Results of history metric 'test':

       $ curl http://localhost:5565/_metrics/test

       {"1303483997384193":{"event":"asdfasdf"}}

Erlang VM memory metrics:

       $ curl http://localhost:5565/_memory

       {"total":11044608,"processes":3240936,"processes_used":3233888,"system":7803672,"atom":532137,"atom_used":524918,"binary":696984,"code":4358030,"ets":385192}
