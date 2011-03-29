-define(DEFAULT_LIMIT, 5).

-define(SYSTEM_INFO, [
                   allocated_areas,
                   allocator,
                   alloc_util_allocators,
                   build_type,
                   c_compiler_used,
                   check_io,
                   compat_rel,
                   cpu_topology,
                   creation,
                   debug_compiled,
                   dist,
                   dist_ctrl,
                   driver_version,
                   elib_malloc,
                   dist_buf_busy_limit,
                   %fullsweep_after, % included in garbage_collection
                   garbage_collection,
                   global_heaps_size,
                   heap_sizes,
                   heap_type,
                   info,
                   kernel_poll,
                   loaded,
                   logical_processors,
                   logical_processors_available,
                   logical_processors_online,
                   machine,
                   %min_heap_size, % included in garbage_collection
                   %min_bin_vheap_size, % included in garbage_collection
                   modified_timing_level,
                   multi_scheduling,
                   multi_scheduling_blockers,
                   otp_release,
                   process_count,
                   process_limit,
                   scheduler_bind_type,
                   scheduler_bindings,
                   scheduler_id,
                   schedulers,
                   schedulers_online,
                   smp_support,
                   system_version,
                   system_architecture,
                   threads,
                   thread_pool_size,
                   trace_control_word,
                   update_cpu_info,
                   version,
                   wordsize
                  ]).

-define(STATISTICS, [
                     context_switches,
                     %exact_reductions, % use reductions instead
                     garbage_collection,
                     io,
                     reductions,
                     run_queue,
                     runtime,
                     wall_clock
                    ]).
