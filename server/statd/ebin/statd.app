{application,statd,
             [{vsn,"1.0"},
              {description,"fitap status management daemon"},
              {modules,[statd,statd_app,statd_ctl,statd_hub,statd_sup]},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{statd_app,[]}},
              {env,[]}]}.
