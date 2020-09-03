{application,psql,
             [{description,"An example application"},
              {vsn,"0.1"},
              {applications,[kernel,stdlib,sasl]},
              {modules,[psql,psql_worker]},
              {registered,[psql]},
              {mod,{psql,[]}},
              {env,[]}
]}.


