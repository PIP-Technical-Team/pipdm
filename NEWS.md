# pipdm (development version)
* Add function `db_create_pipeline_inventory`
* add function `delete_old_output_file`
* Add function `adjust_population`


# pipdm 0.0.7
* remove deprecated files from cache inventory
* Allow the creation dist  statistics for aggregate group data with only one area, like ARG 1980

# pipdm 0.0.6
* Change argument `mean` for `mean_table` in `db_compute_dist_stats()`. Now, it does not receive a vector of means but a dataframe with the means of all countries and it is filtered using cache_id. In this way, we can select any variable and, more importantly, use mean PPP values. 

# pipdm 0.0.5
* fix big bug on the selection of alternative welfare

# pipdm 0.0.4
* add variable `reporting_level` to dist_stat. 
* remove `problem` from frame dist_stat.
* change class of reporting_year in ref_year_table

# pipdm 0.0.3
  * Add variable`max_domain` to all scripts. 
  * add `save_*` functions from pipeline. 
  * fix bugs

# pipdm 0.0.2
* load global variables in `gls` using `pipload::pip_create_globals()` and `pipload::add_gls_to_env()`
* bring pip_update_cache_inventory to pipdm. It was before in the pipeline

## pipdm 0.0.1

* change completely the way `db_compute_dist_stats()` works. 

## pipdm 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
