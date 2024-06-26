
filter_run_scens = function(scenario_selection, scens, scen_list) {
  
  scen_id_list_sub <- scen_list
  
  sel_scenarios_dt <- scens
  
  ## filter for tax scenarios
  
  if (scenario_selection == 'tax_scens') {

    sel_scenarios_dt = sel_scenarios_dt[(oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario %in% c('medium CCS cost', 'no ccs') &
                                           # excise_tax_scenario == 'no tax' & ## all tax
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'no quota') |
                                          (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           ccs_scenario %in% c('medium CCS cost', 'no ccs') &
                                           excise_tax_scenario == 'no tax' &
                                           # setback_scenario == 'no_setback' & ## all setback
                                           prod_quota_scenario == 'no quota')]
  }

  
  if (scenario_selection == 'carbon_scens') {
    
    sel_scenarios_dt = sel_scenarios_dt[(oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           # carbon_price_scenario == 'price floor' &
                                           ccs_scenario %in% c('medium CCS cost', 'no ccs') &
                                           excise_tax_scenario == 'no tax' & ## all tax
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'no quota') |
                                          (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario %in% c('medium CCS cost', 'no ccs') &
                                           excise_tax_scenario == 'no tax' &
                                           # setback_scenario == 'no_setback' & ## all setback
                                           prod_quota_scenario == 'no quota')]
    
  }
  
  ## carbon x setback scens
  if (scenario_selection == 'carbon_setback_scens') {
    
    sel_scenarios_dt = sel_scenarios_dt[(oil_price_scenario == 'reference case' &
                                         innovation_scenario == 'low innovation' &
                                         # carbon_price_scenario == 'price floor' &
                                         ccs_scenario %in% c('medium CCS cost', 'no ccs') &
                                         excise_tax_scenario == 'no tax' & ## all tax
                                         setback_scenario != 'no_setback' &
                                         prod_quota_scenario == 'no quota')]
    
  }
  
  
  
  # keep diagnostics only (if that is input) ------

  if (scenario_selection == 'diagnostic') {

    sel_scenarios_dt = sel_scenarios_dt[(oil_price_scenario == 'reference case' &
                                         innovation_scenario == 'low innovation' &
                                         carbon_price_scenario == 'price floor' &
                                         ccs_scenario == 'medium CCS cost' &
                                         excise_tax_scenario == 'no tax' &
                                         setback_scenario == 'no_setback' &
                                         prod_quota_scenario == 'no quota') |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'quota_20') |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'setback_2500ft' &
                                           prod_quota_scenario == 'quota_20')]
  }

  ## filter for benchmark scenarios
  
  if (scenario_selection == 'benchmark') {

    sel_scenarios_dt = sel_scenarios_dt[(innovation_scenario == 'low innovation' &
                                         carbon_price_scenario == 'price floor' &
                                         ccs_scenario == 'medium CCS cost' &
                                         excise_tax_scenario == 'no tax' &
                                         setback_scenario == 'no_setback' &
                                         prod_quota_scenario == 'no quota') | ## all oil scenarios, hold everything else BAU
                                        (oil_price_scenario == 'reference case' &
                                           # innovation_scenario == 'low innovation' &  ## all innovation scenarios, everything else BAU
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'no quota') |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           # carbon_price_scenario == 'price floor' & ## all carbon scenarios, everything else BAU
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'no quota') |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'central SCC' &
                                           # ccs_scenario == 'medium CCS cost' & ## all CCS
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'no quota') |
                                      (oil_price_scenario == 'reference case' &
                                          innovation_scenario == 'low innovation' &
                                          carbon_price_scenario == 'price floor' &
                                          ccs_scenario == 'medium CCS cost' &
                                         # excise_tax_scenario == 'no tax' & ## all tax
                                          setback_scenario == 'no_setback' &
                                          prod_quota_scenario == 'no quota')  |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           # setback_scenario == 'setback_2500ft' & ## all setback
                                           prod_quota_scenario == 'no quota') |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'no_setback') ] ## all quota
  }


  
    if (scenario_selection == 'full_run') {
      
      sel_scenarios_dt = sel_scenarios_dt[scen_id %in% scen_id_list[, scen_id]]
    }
  
    
    if (scenario_selection == 'full_run_subset') {
      
      subset_scens_df <- scen_id_list_sub[BAU_scen == 1 | subset_scens == 1]
      
      sel_scenarios_dt = sel_scenarios_dt[scen_id %in% subset_scens_df[, scen_id]]
      
      
      
    }
  
  
  
  return(sel_scenarios_dt)
  
  
  
}