### folsom

folsom is an Erlang based metrics system inspired by Coda Hale's metrics (https://github.com/codahale/metrics/). The metrics API's purpose is to collect realtime metrics from your Erlang applications and publish them via HTTP+JSON. folsom is *not* a persistent store, it uses ETS and state to hold values in memory. Generally an external service should be used to consume folsom's data and calculations.

Lastly, folsom as both erlang and REST API's for Erlang VM statistics.

#### Building and running

You need a (preferably recent) version of Erlang installed but that should be it.

       ./rebar get-deps
       ./rebar compile

* Note that ibrowse is only a dep for the test suite.

folsom can be run standalone or embedded in an Erlang application.

       $ erl -pa ebin deps/*/ebin

       > folsom:start(). % or start_link ...

#### _metrics API


#### Erlang VM Metrics:

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

Erlang VM memory metrics:

       $ curl http://localhost:5565/_memory

       {"total":11044608,"processes":3240936,"processes_used":3233888,"system":7803672,"atom":532137,"atom_used":524918,"binary":696984,"code":4358030,"ets":385192}
