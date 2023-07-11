extensions [ rnd csv ]

breed [ farmers farmer ]
breed [ chiefs chief ]
breed [ researchers researcher ]
undirected-link-breed [intra_village_friends intra_village_friend]
undirected-link-breed [inter_village_friends inter_village_friend]
undirected-link-breed [members member]
undirected-link-breed [research-links research-link]

globals [

  ;; -----------------------------------------
  ;; other variable parameters
  farmgroup_meeting_attendance_percentage     ;; defines the percentage of farmers who attend a farmgroup meeting


  ;; -----------------------------------------
  ;; parameters influencing mentioning percentage

  max_influence_adopter_type_on_mention              ;; defines max influence of topic mention during interaction of adopter type of participants
  max_influence_prev_interactions_on_mention         ;; defines max influence of topic mention during interaction of previous interactions of participants
  max_influence_adoption_state_on_mention            ;; defines max influence of topic mention during interaction of adoption state of participants
  optimal_nr_of_interactions_for_mention             ;; defines optimal value of previous interaction that the topic is mentioned

  farmgroup_meeting_mention_probability


  ;; -----------------------------------------
  ;; parameters affecting adoption-decision

  avg_check_adoption_interval                       ;; defines the interval in which every farmer checks whether he wants to adopt or not
  max_influence_adopter_type_on_adoption            ;; defines how heavilgy adoption decision is influenced by adopter type of farmer
  max_influence_attitude_on_adoption                ;; defines how heavily adoption decision is influenced by attitude of the farmer
  max_influence_adopter_friends_on_adoption         ;; defines how heavilty the adoption decision is influenced by the amoung of friends/neighbors already adopted

  ;; -----------------------------------------
  ;; parameters affecting attitude change after interaction


  ;; influence counterpart
  base_influence_counterpart                        ;; defines base influece on attitude during topic related interaction
  chief_influence                                   ;; defines the influence of a chief during a farmgroup meeting

  max_influence_adopter_counterpart_on_attitude     ;; defines influence of a farmer which already adopted to another one
  max_influence_adopter_type_on_attitude            ;; defines the influence of the adopter type on the attitude change during an interaction
  base_attitude_change                              ;; defines by what value the attitude is changed during an interaction




  ;; -----------------------------------------
  ;; paramters affecting attitude after intervetion

  direct_ad_influence                              ;; defines influence of intervetion targetted directly to farmers
  train_chiefs_influence                           ;; defines influence of ToT on chiefs


  attitude_decrease_per_tick                       ;; defines how much the attitude decreases per tick (stops at 0 --> no bad attitude can be created by not talking)



  ;; -----------------------------------------
  ;; -----------------------------------------
  ;; other values

  ;; calculated values
  village_id_counter
  village_size_multiply_factor
  total_nr_of_interactions
  total_nr_of_topic_related_interactions


  all_village_ids ;; list with all village ids
  research_team_agent ;; research team variable for direct access
  average_farmer_dummy ;; dummy farmer of whom all values are average

  villages_per_neighborhood ;; calculated number of villages in one neighborhood (upper)
  start_counter ;; start of sublist
  end_counter ;; end of sublist
  counter
  neighborhood_list ;; list for keeping track of villages in one neighborhood (temp)

  ;; -----------------------------------------
  ;; simulation parameters
  direct_ad_frequency
  train_chiefs_frequency

  ;; -----------------------------------------
  ;; export lists
  list_turtles_topic_interaction_per_tick

  ;; counting values for results (uncomment function in go() to calc them)
  nr_of_adopters
  nr_of_considerers
  nr_of_unknown
  nr_villages_with_adopters
  nr_villages_with_considerers

]

turtles-own [
  ;; intra village link variables
  nr_of_intra_village_interaction_this_tick
  next_intra_village_interaction_tick_nr


  ;; inter village link variables
  nr_of_inter_village_interaction_this_tick
  next_inter_village_interaction_tick_nr
  my_inter_vill_link_nr
  my_remaining_inter_vill_link_nr

  ;; adoption variable
  adopter_type ;; 1=innovator, 2=early adopter, 3=early majority, 4=late majority, 5=laggards
  adoption_state ;; 0=not aware of innovation, 1=consideration, 2=adopter
  next_adoption_decision_tick_nr ;; states the tick nr of this agent when he decides about adopting again
  innovation_adoption_attitude ;; defines the attitude of the agent towards the innovation (can be positive or negative; declines over time by attitude_decrease_per_tick)

  ;; generic interaction variables
  nr_total_interactions
  nr_topic_related_interactions
  nr_of_council_interactions
  nr_of_inter_village_interactions
  nr_of_intra_village_interactions


  ref_village_id
  ref_neighborhood_id


]

farmers-own [
   ]

chiefs-own [
  next_chief_farmer_meeting_tick_nr
]

patches-own [

  village_id
  neighborhood_id

]

links-own [

  last_executed_tick

]


;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; initialization
to setup

  ;; reset
  clear-all
  reset-ticks

  set total_nr_of_interactions 0
  set total_nr_of_topic_related_interactions 0

  ;; variables
  init_parameters

  ;; agents and links
  grow-villages
  grow-neighborhoods
  create_villages

  init-research-team

  link_farmers_in_same_village
  link_farmers_inter_village

  init_agents_general
  init_links_general

  init_export_lists
end

;; inititalizes lists which can be exported
to init_export_lists
  ;; general
  let agent_names (map [x -> [(word breed " " who)] of x] sort turtles with [breed != researchers])

  ;; init list_turtles_topic_interaction_per_tick
  set list_turtles_topic_interaction_per_tick []
  set list_turtles_topic_interaction_per_tick lput agent_names list_turtles_topic_interaction_per_tick
end

;; initializes all semi-fixed parameters used in the model
to init_parameters
  set farmgroup_meeting_attendance_percentage 90 ;; in %

  set max_influence_adopter_type_on_mention 25 ;; in %
  set max_influence_prev_interactions_on_mention 20 ;; in %
  set max_influence_adoption_state_on_mention 10 ;; in %
  set optimal_nr_of_interactions_for_mention 10

  set farmgroup_meeting_mention_probability 5 ;; in %


  set base_attitude_change 1
  set base_influence_counterpart 100 ;; in %
  set chief_influence 175 ;; in %
  set max_influence_adopter_counterpart_on_attitude 20 ;; in %
  set max_influence_adopter_type_on_attitude 25 ;; in %


  set direct_ad_influence 150 ;; in %
  set train_chiefs_influence 200 ;; in %


  set avg_check_adoption_interval 5
  set max_influence_adopter_type_on_adoption  10 ;; in %
  set max_influence_attitude_on_adoption 50 ;; in %
  set max_influence_adopter_friends_on_adoption 20 ;; in %

  set attitude_decrease_per_tick 0.05

  ;; simulation variables
  set nr_of_adopters 0
  set nr_of_considerers 0
  set nr_of_unknown 0
  set nr_villages_with_adopters 0
  set nr_villages_with_considerers 0
end


;; inititalizes the research and dummy agents used in the model
to init-research-team
  create-researchers 1 [
      create-research-links-with other turtles
    ]

  set research_team_agent one-of researchers

  create-researchers 1 [
    set adopter_type 3
    set adoption_state 0
    set nr_topic_related_interactions 0
  ]

  set average_farmer_dummy one-of researchers with [adopter_type = 3]
end

;; inititalizes the general properties of the agents
to init_agents_general
  ask chiefs [
    set color red
    set shape "flag"
    set next_chief_farmer_meeting_tick_nr calc_next_chief_farmer_meeting_interaction
  ]

  ask farmers [
    set color black
    set shape "dot" ;;"person farmer"
  ]

  ask researchers [
    set shape "uzh"
    hide-turtle
  ]

  ask turtles [
    set adoption_state 0
    set innovation_adoption_attitude 0
    set size 5
    set nr_total_interactions 0
    set next_adoption_decision_tick_nr 0
    set next_inter_village_interaction_tick_nr calc_next_inter_village_interaction
    set next_intra_village_interaction_tick_nr calc_next_intra_village_interaction
    set adopter_type get_adopter_type
  ]
end

;; returns an adopter type according Roger's probability
to-report get_adopter_type
  let items [ 1 2 3 4 5 ]
  let weights (list 0.025 0.135 0.34 0.34 0.16)
  report (choose_with_probability items weights)
end

;; inititalizes all link breed with general values
to init_links_general
  ask intra_village_friends [
    set color blue
    hide-link
    set last_executed_tick ticks
  ]

  ask inter_village_friends [
    set color black
    hide-link
    set last_executed_tick ticks
  ]

  ask members [
    set color red
    hide-link
    set last_executed_tick ticks
  ]

  ask research-links [
    set color orange
    hide-link
    set last_executed_tick ticks
  ]
end

;; links all farmers and chiefs in same village (for each village)
to link_farmers_in_same_village
  ; for each village, get all farmers and link them
  foreach all_village_ids [
    x ->
    let cur_farmers (turtles with [ref_village_id = x and breed != researchers])

    ask cur_farmers [
      create-intra_village_friends-with other cur_farmers
    ]
  ]
end

;; connects random farmers and chiefs between villages
to link_farmers_inter_village

  ask turtles [
    set my_inter_vill_link_nr (random-poisson nr_default_friends_inter_village)
    set my_remaining_inter_vill_link_nr my_inter_vill_link_nr
  ]

  ask turtles with [breed != researchers] [

    let friends at-most-n-of-more-from-same-neighborhood my_remaining_inter_vill_link_nr other turtles with [ ;;TODO: make it more likely to befriend someone from same neighborhood
      breed != researchers and
      ref_village_id != [ref_village_id] of myself and
        my_remaining_inter_vill_link_nr > 0 ] ref_neighborhood_id

    if friends != nobody [
        create-inter_village_friends-with friends
        ask friends [ set my_remaining_inter_vill_link_nr (my_remaining_inter_vill_link_nr - 1) ]
        set my_remaining_inter_vill_link_nr (my_remaining_inter_vill_link_nr - count friends)
    ]
]
end

to grow-neighborhoods
   ask patches [
    set neighborhood_id -1 ;; set all neighborhood ids to -1 to keep track of mistakes
  ]

  if nr_of_neighborhoods != 0 [ ;; only do this if there are more than 0 neighborhoods

    set villages_per_neighborhood (nr_of_villages / nr_of_neighborhoods)
    set start_counter 0
    set end_counter (round villages_per_neighborhood)
    set counter 0

    loop [ ;; loop for every neighborhood
      if counter = nr_of_neighborhoods [ stop ] ;; stop if we have set number of neighborhoods
      if start_counter > nr_of_villages [ stop ] ;; stop if we reach end of list
      set counter (counter + 1)
      if end_counter >= (length all_village_ids) [ set end_counter (length all_village_ids) ] ;; reset end counter if it is larger than length of list
      set neighborhood_list sublist all_village_ids start_counter end_counter ;; define sublist of villages belonging to new neighborhood ;; end counter is not included
      ask patches with [ member? village_id neighborhood_list ] [ ;; all patches of villages which are in the sublist
        set neighborhood_id counter + 1 ;; set neighborhood id
      ]

      ;; update start and end counter
      set start_counter (start_counter + round villages_per_neighborhood)
      set end_counter (end_counter + round villages_per_neighborhood)
    ]
  ]
end

;; generates the defined number of villages in a random fashion
to grow-villages
  ;; init patch counter
  ask patches [
    set village_id -1
    set pcolor white
  ]

  set village_id_counter 0
  ;; set initial number of villages
  ask n-of nr_of_villages patches [
    set village_id village_id_counter
    set village_id_counter (village_id_counter + 1)
  ]

  while [ any? patches with [ village_id = -1 ] ] [
    ask patches with [ village_id != -1 ] [
      ask neighbors with [ village_id = -1 ] [
        set village_id [village_id] of myself
      ]
    ]
  ]
  ;; add borders around villages
  ensure-village-min-size
  fill_village_nr_list
  ensure-village-borders
  calc_village_size_multiply_factor
end

;; gets all village ids
to fill_village_nr_list
  set all_village_ids remove-duplicates [village_id] of patches
end

;; calculates the factor between avg. patch amount per village and defined avg. number of villagers
to calc_village_size_multiply_factor
  let all_village_sizes []
  foreach all_village_ids [
    x ->
    set all_village_sizes lput (get-village-size x) all_village_sizes
  ]

  ;; divide average nr of inhabitants by mean/median village size to norm
  let median_village_patch_size (mean all_village_sizes)
  set village_size_multiply_factor (avg_nr_of_farmers_per_village / median_village_patch_size)
end


;; patches at the border of a village should become black
to ensure-village-borders
  ask patches [
    if any? neighbors4 with [village_id != [village_id] of myself] [
      set pcolor black
    ]
  ]
end


;; if villages are smaller than minimal size, merge them with next village in sequence
to ensure-village-min-size
  let i 0
  while [i < village_id_counter] [
    let nr_of_patches (count patches with [village_id = i])
    if nr_of_patches < 10 [
      ask patches with [village_id = i] [
        set village_id ( (i + 1) mod (village_id_counter - 1 ))
      ]
    ]
    set i (i + 1)
  ]
end

;; adds all villagers and chiefs to the villages
to create_villages
  foreach all_village_ids [
    x -> let coord (find_middle_of_village x)

    let cur_patch one-of patches with [village_id = x ] ;; get one of the patches in the village in order to set the neighborhood_id of the chief correctly

    create-chiefs 1 [
      setxy (item 0 coord) (item 1 coord)
      set ref_village_id x
      set ref_neighborhood_id ([neighborhood_id] of cur_patch) ;; set neighborhood_id by taking it from one of the patches from the village
    ]

    let cur_chief chiefs with [xcor = (item 0 coord) and ycor = (item 1 coord)]
    let nr_of_villagers (determine-nr-of-village-inhabitants x)
    repeat nr_of_villagers [
      add_farmer_to_village x cur_chief ]
  ]
end


;; maps the village patch size to the according number of residents
to-report determine-nr-of-village-inhabitants [vill_id]
  let nr_of_patches (count patches with [village_id = vill_id])
  report (max list 2 ((ceiling (nr_of_patches * village_size_multiply_factor)) - 1 )) ;; chief is also inhabitant
end

;; adds a single farmer to a village
to add_farmer_to_village [vill_id this_chief]

  let selected_empty_patch one-of patches with [
    village_id = vill_id and
    not any? turtles-here and
    pcolor != black ;; no farmers on border (UI)
  ]

  if selected_empty_patch != nobody [
    create-farmers 1 [
      create-members-with this_chief
      set ref_village_id ([ref_village_id] of one-of this_chief)
      set ref_neighborhood_id ([ref_neighborhood_id] of one-of this_chief)
      move-to selected_empty_patch
    ]
  ]
end

;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; while running
to go
  ;; check interventions
  check_for_interventions

  ;; pre-execution
  prepare_agents_for_interaction

  ;; exec interaction
  farmer-intra-and-inter-village-interaction
  chief-farmer-meeting-interaction

  ;; check if some farmers want to adopt
  check_adoption


  ;; update UI according to tick changes
  if is_visible_update_activated [
    update_village_visuals
  ]

  calc_simulation_variables ; uncomment if values are needed for analysis

  ;; export data for analysis
  ;prepare_exports

  ;; check finishing condition
  if check_finished_condition and check-simulation-finished [
    stop
  ]

  if  run_until_day_x > 0 and run_until_day_x <= ticks [stop]

  tick
end

;; caculates variables for simulation to be exported
to calc_simulation_variables
  let adopters turtles with [adoption_state = 2]
  let considerers turtles with [adoption_state = 1]
  let unknowns turtles with [adoption_state = 0 and breed != researchers]

  set nr_of_adopters count adopters
  set nr_of_considerers count considerers
  set nr_of_unknown count unknowns
  set nr_villages_with_adopters length (remove-duplicates [ref_village_id] of adopters)
  set nr_villages_with_considerers length (remove-duplicates [ref_village_id] of considerers)
end

;; checks whether simulation should be finished when everyone adopter
to-report check-simulation-finished
  if ticks mod 1 = 0
  [
    if all? farmers [ check_finished_farmer self ]  [
      report true
    ]
  ]

  report false
end

to-report check_finished_farmer [farmer_agent]
  report [adoption_state] of farmer_agent = 2
end


;; set interaction amount for each agent which interacts this tick
to prepare_agents_for_interaction
  ask turtles [
    let b_interacts_this_tick false
    if next_inter_village_interaction_tick_nr = ticks [
      set b_interacts_this_tick true
      ;; get random value for actual interactions this interval
      set nr_of_inter_village_interaction_this_tick (ifelse-value
        avg_inter_village_interaction_frequency >= 1 [ 1 ]
                                                     [ round (1 / avg_inter_village_interaction_frequency) ]) ;; e.g. 0.5 means 2 interactions each tick
    ]

    if next_intra_village_interaction_tick_nr = ticks [
      set b_interacts_this_tick true
      ;; get random value for actual interactions this interval
      set nr_of_intra_village_interaction_this_tick (ifelse-value
        avg_intra_village_interaction_frequency >= 1 [ 1 ]
                                                     [ round (1 / avg_intra_village_interaction_frequency) ]) ;; e.g. 0.5 means 2 interactions each tick
    ]

    ;; decrease attitude if no interactions happen
    if not b_interacts_this_tick [
      if innovation_adoption_attitude > 0 [set innovation_adoption_attitude ( max list (innovation_adoption_attitude - attitude_decrease_per_tick) 0)]
    ]
 ]
end

;; let farmers interact with each other
to farmer-intra-and-inter-village-interaction

  ask turtles with [(nr_of_inter_village_interaction_this_tick + nr_of_intra_village_interaction_this_tick) > 0 and breed != researchers] [

    if next_inter_village_interaction_tick_nr = ticks [

      ;; select links for this interaction
      let chosen_inter_links at-most-n-of nr_of_inter_village_interaction_this_tick my-inter_village_friends with [ last_executed_tick < ticks]
      if chosen_inter_links != nobody [
        let chosen_inter_village_farmers ([other-end] of chosen_inter_links)

        ;; mark these links as used for this tick
        ask chosen_inter_links [
          set last_executed_tick ticks
        ]

        ;; increase the interactions counter for this farmer by number of interactions
        set nr_of_inter_village_interactions (nr_of_inter_village_interactions + (length chosen_inter_village_farmers))


        ;; interact with inter village friends
        foreach chosen_inter_village_farmers [
          x -> ask x [
            set nr_of_inter_village_interactions (nr_of_inter_village_interactions + 1)
            interact self base_influence_counterpart true myself base_influence_counterpart true false true
        ]]
      ]
      set next_inter_village_interaction_tick_nr calc_next_inter_village_interaction
    ]

    if next_intra_village_interaction_tick_nr = ticks [
      ;; select links for this interaction
      let chosen_intra_links at-most-n-of nr_of_intra_village_interaction_this_tick my-intra_village_friends with [ last_executed_tick < ticks]
      if chosen_intra_links != nobody [
        let chosen_intra_village_farmers ([other-end] of chosen_intra_links)

        ;; mark these links as used for this tick
        ask chosen_intra_links [
          set last_executed_tick ticks
        ]

        ;; increase the interactions counter for this farmer by number of interactions
        set nr_of_intra_village_interactions (nr_of_intra_village_interactions + (length chosen_intra_village_farmers))


        ;; interact with inter village friends
        foreach chosen_intra_village_farmers [
          x -> ask x [
            set nr_of_intra_village_interactions (nr_of_intra_village_interactions + 1)
            interact self base_influence_counterpart true myself base_influence_counterpart true false true
        ]]
      ]
      ;; set next interaction timepoint
      set next_intra_village_interaction_tick_nr calc_next_intra_village_interaction
    ]
  ]
end

;; farm group meeting
to chief-farmer-meeting-interaction
  ask chiefs with [ next_chief_farmer_meeting_tick_nr = ticks ] [

    ;; increase the interactions counter for this farmer by number of interactions
    set nr_of_council_interactions (nr_of_council_interactions + 1)

    let village_size (count farmers with [ref_village_id = [ref_village_id] of myself])

    ;; consider that not all farmers may attend
    let nr_of_attendants floor ((farmgroup_meeting_attendance_percentage / 100) * village_size) ;; floor as half-person cannot attend

    ;; get all farmers in this village
    let village_farmers (at-most-n-of nr_of_attendants member-neighbors)

    if any? village_farmers
    [
      set nr_total_interactions (nr_total_interactions + 1)

      ;; calculate if chief mentions the innovation in current meeting
      let mention_topic_prop (calc_mention_topic_probability self average_farmer_dummy false)
      set mention_topic_prop (mention_topic_prop * (farmgroup_meeting_mention_probability / avg_mention_percentage )) ;; multply result by ratio between probabilities

      let mention_innovation choose_boolean_with_probability mention_topic_prop

      ifelse mention_innovation [
        ask village_farmers [
          set nr_of_council_interactions (nr_of_council_interactions + 1)
          interact self 0 true myself chief_influence false true true
      ]
      ] [ ;; just an unrelated interaction
       ask village_farmers [
          set nr_total_interactions (nr_total_interactions + 1)
          set nr_of_council_interactions (nr_of_council_interactions + 1)
        ]
      ]
    ]

    set next_chief_farmer_meeting_tick_nr calc_next_chief_farmer_meeting_interaction
  ]
end


;; returns tick value for next inter village interaction
to-report calc_next_inter_village_interaction
  report (ticks + truncate_value_lower random-poisson avg_inter_village_interaction_frequency 1 )
end

;; returns tick value for next intra village interaction
to-report calc_next_intra_village_interaction
  report (ticks + truncate_value_lower random-poisson avg_intra_village_interaction_frequency 1 )
end

;; returns tick value for next chief farmer meeting interaction
to-report calc_next_chief_farmer_meeting_interaction
  report (ticks + truncate_value_lower random-poisson avg_chief_farmer_meeting_frequency 1 )
end

;; returns tick value for next chief farmer meeting interaction
to-report calc_next_adoption_decision_tick
  report (ticks + truncate_value_lower random-poisson avg_check_adoption_interval 1 )
end


;; interaction logic between 2 agents
to interact [ participant1 part1level b_increase_part1_level participant2 part2level b_increase_part2_level b_force_mention b_negative_WoM_allowed ]

  ;; incerease default interaction variables
  if b_increase_part1_level [
    ask participant1 [
      set nr_total_interactions (nr_total_interactions + 1)
  ]]

  if b_increase_part2_level [
    ask participant2 [
      set nr_total_interactions (nr_total_interactions + 1)
  ]]

  set total_nr_of_interactions total_nr_of_interactions + 1


  ;; check if topic is mentioned in this interaction
  if (calc_mention_topic participant1 participant2 b_force_mention) [
    set total_nr_of_topic_related_interactions total_nr_of_topic_related_interactions + 1
    update_adoption_attributes participant1 part1level b_increase_part1_level participant2 part2level b_increase_part2_level b_negative_WoM_allowed
  ]
end

;; calculates whether the topic is talked about in current interaction
to-report calc_mention_topic [ participant1 participant2 bforceMention ]
  let final_interaction_probability (calc_mention_topic_probability participant1 participant2 bforceMention)

  let btalk (choose_boolean_with_probability final_interaction_probability)

  report btalk
end

;; calculates whether the topic is talked about in current interaction
to-report calc_mention_topic_probability [ participant1 participant2 bforceMention ]
 if bforceMention [
    report 1
  ]

  ;; cant talk about topic you never heard of
  if [adoption_state] of participant1 = 0 and [adoption_state] of participant2 = 0 [
    report 0
  ]

  ;; calculate influence of adopter types
  ;; compare adopter types, the smaller the average of both, the likelier they talk about the innovation
  let adopter_type_influence (calc_adopter_type_influence_on_mention ((([adopter_type] of participant1 ) + ([adopter_type] of participant2)) / 2))

  ;; calculate influence of previous interaction
  let prev_topic_interactions_influence (calc_prev_interaction_influence_on_mention participant1 participant2)

  ;; calculate influence of adoption state
  let adopter_interaction_influence (calc_adoption_state_influence_on_mention participant1 participant2)

  ;; calc final interaction probability
  let final_interaction_probability (avg_mention_percentage / 100)
  set final_interaction_probability (final_interaction_probability + (adopter_type_influence * (avg_mention_percentage / 100))) ;; influence of adopter type
  set final_interaction_probability (final_interaction_probability + (prev_topic_interactions_influence * (avg_mention_percentage / 100))) ;; influence of previous interaction
  set final_interaction_probability (final_interaction_probability + (adopter_interaction_influence * (avg_mention_percentage / 100))) ;; influence of adoption phase

  ;; percentage between 0 and 1
  set final_interaction_probability truncate_value final_interaction_probability 1 0
  report final_interaction_probability
end



to-report calc_adopter_type_influence_on_mention [n_adopter_type]
  ;; arbitrarly defined values based on adopter type curve
  let influence (ifelse-value
  n_adopter_type <= 1.5 [max_influence_adopter_type_on_mention]
  n_adopter_type <= 2.5 [max_influence_adopter_type_on_mention * 0.5]
  n_adopter_type <= 3.5 [max_influence_adopter_type_on_mention * 0.2]
  n_adopter_type <= 4.5 [max_influence_adopter_type_on_mention * -0.5]
  [max_influence_adopter_type_on_mention * -1])
  report influence / 100
end

to-report calc_prev_interaction_influence_on_mention [participant1 participant2]
  let b_is_participant1_adopter (([adoption_state] of participant1) = 2)
  let b_is_participant2_adopter (([adoption_state] of participant2) = 2)
  let b_both_participants_adopted (b_is_participant1_adopter and b_is_participant2_adopter)
  let prev_topic_interactions_influence 0
  if (not b_both_participants_adopted) [
    let prob1 0
    let prob2 0
    if (not b_is_participant1_adopter and [nr_topic_related_interactions] of participant1 != 0) [
      ;; poisson with mean=optimal_nr_of_interactions_for_adoption and value=nr_topic_related_interactions of part1
      set prob1 (calculate_poisson_probability ([nr_topic_related_interactions] of participant1)  optimal_nr_of_interactions_for_mention)
      ;; map this value in relation to maximal influence of prev topic (so it is in bound of [-max_influence_previous_topic_interactions,max_influence_previous_topic_interactions])
      set prob1 (((prob1 / find_max_of_poisson optimal_nr_of_interactions_for_mention) * 2 * max_influence_prev_interactions_on_mention) - max_influence_prev_interactions_on_mention )
    ]
    if (not b_is_participant2_adopter and [nr_topic_related_interactions] of participant2 != 0) [
      ;; poisson with mean=optimal_nr_of_interactions_for_adoption and value=nr_topic_related_interactions of part2
      set prob2 (calculate_poisson_probability ([nr_topic_related_interactions] of participant2)  optimal_nr_of_interactions_for_mention)
      ;; map this value in relation to maximal influence of prev topic (so it is in bound of [-max_influence_previous_topic_interactions,max_influence_previous_topic_interactions])
      set prob2 (((prob2 / find_max_of_poisson optimal_nr_of_interactions_for_mention) * 2 * max_influence_prev_interactions_on_mention) - max_influence_prev_interactions_on_mention )
    ]

    ;; take the average of both influence (in case one particpant is an adopter, the influence gets halfed)
    set prev_topic_interactions_influence (prob1 + prob2) / 2
  ]
  report prev_topic_interactions_influence / 100
end

to-report calc_adoption_state_influence_on_mention [participant1 participant2]
  let b_is_participant1_adopter (([adoption_state] of participant1) = 2)
  let b_is_participant2_adopter (([adoption_state] of participant2) = 2)
  let b_no_participants_adopted ( not b_is_participant1_adopter and  not b_is_participant2_adopter)
  let b_both_participants_adopted (b_is_participant1_adopter and b_is_participant2_adopter)
  let adopter_interaction_influence 0
  ;; at least one adopter

  (ifelse (b_both_participants_adopted) [
    set adopter_interaction_influence (max_influence_adoption_state_on_mention / 100)
  ]
  not b_no_participants_adopted [
    set adopter_interaction_influence (max_influence_adoption_state_on_mention / 100) / 2
  ])

  report adopter_interaction_influence
end

to update_adoption_attributes [ participant1 part1level b_increase_part1_level participant2 part2level b_increase_part2_level b_negative_WoM_allowed ]
  let b_is_negative_wom (b_negative_WoM_allowed and choose_boolean_with_probability (percentage_negative_WoM / 100))

  ;; update participant 1
  update_attitude_single_agent participant1 b_increase_part1_level participant2 part2level b_is_negative_wom

  ;; update participant 2
  update_attitude_single_agent participant2 b_increase_part2_level participant1 part1level b_is_negative_wom
end


to update_attitude_single_agent [agent b_update_this_agent other_agent other_agent_influence b_is_negative_wom ]
  if b_update_this_agent [
    let final_attitude_change base_attitude_change

    ;; attitude change is influenced by type of adopter, influence of other agent, whether other agent already adopted innovation
    ;; -----------

    ;; adopter type influence (laggers tend to accept negative information more than positiv and vice versa for innovators)
    let adopter_type_influence (calc_adopter_type_influence_on_attitude [adopter_type] of agent b_is_negative_wom)


    ;; adoption state influence - if other agent is adopter, his opinion is more valuable
    let adoption_state_influence (calc_adopter_influence_on_attitude other_agent)


    ;; change attitude change
    set final_attitude_change (final_attitude_change * (other_agent_influence / 100)) ;; influence of counterpart (a.k.a. trustworthiness)
    set final_attitude_change (final_attitude_change + adopter_type_influence) ;; influence of adopter type
    set final_attitude_change (final_attitude_change + adoption_state_influence) ;; influence of adoption state
    set final_attitude_change (final_attitude_change * (ifelse-value (b_is_negative_wom) [-1] [1])) ;; influence of negative WoM (negate)



    ;; check that influence cannot be negative for positive WoM and vice versa (security check)
    ifelse (b_is_negative_wom) [
      set final_attitude_change (truncate_value_upper final_attitude_change 0)
    ]
    [ set final_attitude_change (truncate_value_lower final_attitude_change 0) ]


    ask agent [
      set nr_topic_related_interactions (nr_topic_related_interactions + 1)
      set innovation_adoption_attitude (innovation_adoption_attitude + final_attitude_change)
      if (nr_topic_related_interactions = 1) [set adoption_state 1] ;; switch adoption state
    ]
  ]
end

to-report calc_adopter_influence_on_attitude [other_agent]
  if ([adoption_state] of other_agent = 2) [
    report max_influence_adopter_counterpart_on_attitude / 100
   ]
  report 0
end

to-report calc_adopter_type_influence_on_attitude [input_adopter_type b_is_negative_wom]
  let type_influence 0
  ifelse b_is_negative_wom [
      set type_influence (ifelse-value
        input_adopter_type = 1 [ max_influence_adopter_type_on_attitude * -1 ]
        input_adopter_type = 2 [ max_influence_adopter_type_on_attitude * -0.5]
        input_adopter_type = 3 [ max_influence_adopter_type_on_attitude * -0.2]
        input_adopter_type = 4 [ max_influence_adopter_type_on_attitude * 0.5 ]
        input_adopter_type = 5 [ max_influence_adopter_type_on_attitude ]
      )
    ] [
      set type_influence (ifelse-value
        input_adopter_type = 1 [ max_influence_adopter_type_on_attitude ]
        input_adopter_type = 2 [ max_influence_adopter_type_on_attitude * 0.5 ]
        input_adopter_type = 3 [ max_influence_adopter_type_on_attitude * 0.2 ]
        input_adopter_type = 4 [ max_influence_adopter_type_on_attitude * -0.5 ]
        input_adopter_type = 5 [ max_influence_adopter_type_on_attitude * -1 ])
    ]

  report type_influence / 100
end

to update_village_visuals
  foreach all_village_ids [
    x ->
    let all_finished (all? (farmers with [ref_village_id = x]) [check_finished_farmer self])
    if all_finished [
      ask patches with [village_id = x and pcolor = white] [
        set pcolor green
      ]
    ]
  ]
  ask turtles with [ check_finished_farmer self ] [ set color green ]
end


;; check if any farmer wants to adopt the innovation
to check_adoption

  ;; don't check if already adopted
  ask turtles with [next_adoption_decision_tick_nr = ticks and adoption_state != 2] [

    ;; make adoption decision when in consideration phase
    if (adoption_state = 1 and nr_topic_related_interactions > 2) [
      ;; decision is based on base_adoption_percentage, own attitude, own adopter_type and adoption rate of friends/neighbors)
      let final_probability base_adoption_probability / 100

      set final_probability (final_probability + (calc_adopter_type_influence_on_decision self * (base_adoption_probability / 100))) ;; influence of adopter type
      set final_probability (final_probability + (calc_adopter_friends_influence_on_decision self * (base_adoption_probability / 100))) ;; influence of adoption rate of friends
      set final_probability (final_probability + (calc_attitude_influence_on_decision self * (base_adoption_probability / 100))) ;; influence of attitude towards innovation

      ;; truncate negative or >100% values
      set final_probability (truncate_value final_probability 1 0)


      ;; check if agent wants to adopt
      let b_wants_to_adopt (choose_boolean_with_probability final_probability)
      if b_wants_to_adopt [
        set adoption_state 2
      ]
    ]
    ;; set next adoption decision tick nr
    set next_adoption_decision_tick_nr calc_next_adoption_decision_tick
  ]
end

to-report calc_adopter_type_influence_on_decision [agent]
  ;; arbitrarly defined values based on adopter type curve
  let temp_adopter_type [adopter_type] of agent
  let influence (ifelse-value
  temp_adopter_type = 1 [max_influence_adopter_type_on_adoption]
  temp_adopter_type = 2 [max_influence_adopter_type_on_adoption * 0.5]
  temp_adopter_type = 3 [max_influence_adopter_type_on_adoption * 0.2]
  temp_adopter_type = 4 [max_influence_adopter_type_on_adoption * -0.5]
  temp_adopter_type = 5 [max_influence_adopter_type_on_adoption * -1])
  report influence / 100
end

to-report calc_adopter_friends_influence_on_decision [agent]
  let intra_friends [intra_village_friend-neighbors] of agent
  let inter_friends [inter_village_friend-neighbors] of agent

  if intra_friends = nobody and inter_friends = nobody [report 0]

  let perc_intra 0
  let perc_inter 0
  if any? intra_friends [
    set perc_intra (count intra_friends with [adoption_state = 2]) / (count intra_friends)
  ]
  if any? inter_friends [
    set perc_inter (count inter_friends with [adoption_state = 2]) / (count inter_friends)
  ]

  let percentage_adopted (perc_intra + perc_inter) / 2 ;; 50/50 influence intra and inter friends adoption rate
  ;; there is no negative consequence if none of your friend hasn't adopted yet
  report max_influence_adopter_friends_on_adoption * percentage_adopted / 100
end

to-report calc_attitude_influence_on_decision [agent]

  if ([innovation_adoption_attitude] of agent = 0) [report 0]

  ;; compare attitude with nr of prev. interaction to check whether he has a genrally positive attitude or negative
  let nr_of_rel_int [nr_topic_related_interactions] of agent
  let attitude_of_agent [innovation_adoption_attitude] of agent

  let result 0
  (ifelse
  attitude_of_agent < (0.85 * nr_of_rel_int) [set result max_influence_attitude_on_adoption * -1 / 100]
  attitude_of_agent > (0.95 * nr_of_rel_int) [set result max_influence_attitude_on_adoption / 100]
  [set result 0])

  report result
end



;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; interventions

to direct_village_intervention
  contact_farmers direct_ad_nr_of_villages percentage_of_villagers_addressed / 100
end

to contact_farmers [ nr_of_villages_selected percentage_of_farmers ]
  let chosen_village_ids (at-most-n-of-list nr_of_villages_selected all_village_ids)
;  let chosen_village_ids [ 1 ]

  foreach chosen_village_ids [
    x ->
    let village_farmers  farmers with [ref_village_id = x]
    ;; get percentage of villagers and only reachable
    ;; todo maybe more randomness??
    set village_farmers (at-most-n-of (floor ((count village_farmers) *  percentage_of_farmers)) village_farmers )

    if any? village_farmers [
      ask village_farmers [
      interact research_team_agent direct_ad_influence false self 0 true true false
      ]
    ]
  ]
end


to train_chiefs
  let chosen_chiefs at-most-n-of train_chiefs_nr chiefs

  if (any? chosen_chiefs) [

    ask chosen_chiefs [
      interact research_team_agent train_chiefs_influence false self 0 true true false
    ]
  ]
end

to check_for_interventions
  intervention_strategy_sample
end



to intervention_strategy_sample
  if direct_ad_frequency > 0 and ticks mod direct_ad_frequency = 0 [
    direct_village_intervention
  ]
  if train_chiefs_frequency > 0 and ticks mod train_chiefs_frequency = 0 [
    train_chiefs
  ]
end

;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; export

to prepare_exports
  let list_this_tick (map [x -> [nr_topic_related_interactions] of x] sort turtles with [breed != researchers])
  set list_turtles_topic_interaction_per_tick lput list_this_tick list_turtles_topic_interaction_per_tick
end


to export
  let file_name "pathToCSVFile"
  csv:to-file file_name list_turtles_topic_interaction_per_tick
end


;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------
;; helper

to-report choose_boolean_with_probability [probability_true]
  let items [ true false ]
  let weights list probability_true (1 - probability_true)
  report (choose_with_probability items weights)
end


to-report choose_with_probability [items weights]
  let pairs (map list items weights)
  report first rnd:weighted-one-of-list pairs [ [p] -> last p ]
end

to-report find_middle_of_village [ id ]
  let selected_patches patches with [village_id = id ]
  let avg_xcor (sum [pxcor] of selected_patches) / (count selected_patches)
  let avg_ycor (sum [pycor] of selected_patches) / (count selected_patches)
  report list avg_xcor avg_ycor
end

to-report get-village-size [vill_id]
  report count patches with [village_id = vill_id]
end

to-report at-most-n-of [ n agentset ]
  ifelse count agentset > n [
    report n-of n agentset
  ] [
    report agentset
  ]
end

to-report at-most-n-of-list [ n lst ]
  ifelse  length lst > n [
    report n-of n lst
  ] [
    report lst
  ]
end

;; returns agentset containing n agents of given agentset
;; returned agentset contains more agents from given neighborhood_id than not
to-report at-most-n-of-more-from-same-neighborhood [ n agentset id ]

  ;; create two agentsets from input agentset: one containing all agents from same neighborhood and one with all other agents
  let same_neighborhood_friends agentset with [ ref_neighborhood_id = [ref_neighborhood_id] of myself ]
  let different_neighborhood_friends agentset with [ ref_neighborhood_id != [ref_neighborhood_id] of myself ]

  ;; 80% of friends are from same neighborhood
  let num_friends_same_neighborhood ceiling n * 0.8
  let num_friends_different_neighborhood n - num_friends_same_neighborhood

  ;; take number of friends from corresponding agentsets ;; if agentset is smaller than n, take whole agentset
  if count same_neighborhood_friends > num_friends_same_neighborhood [
    set same_neighborhood_friends n-of num_friends_same_neighborhood same_neighborhood_friends
  ]

  if count different_neighborhood_friends > num_friends_different_neighborhood [
    set different_neighborhood_friends n-of num_friends_different_neighborhood different_neighborhood_friends
  ]

  ;; return combined agentset of friends
  let combined_friends (turtle-set same_neighborhood_friends different_neighborhood_friends)
  report combined_friends
end

to-report truncate_value [value maxVal minVal]
  report max (list minVal (min (list maxVal value)))
end

to-report truncate_value_lower [value minVal]
  report max (list minVal value)
end

to-report truncate_value_upper [value maxVal]
  report min (list maxVal value)
end

to-report calculate_poisson_probability [value mean_poisson]
  report (exp (- 1 * mean_poisson)) * (mean_poisson ^ value) / (factorial value 0)
end

to-report factorial [n m]
  if (n = m) [report 1]
  report (n * factorial (n - 1) (m))
end

to-report find_max_of_poisson [mean_poisson]
  let ceil_value ceiling mean_poisson
  let floor_value floor mean_poisson
  let result max list (calculate_poisson_probability ceil_value mean_poisson) (calculate_poisson_probability floor_value mean_poisson)
  report result
end



to reset
  reset-ticks

  ask turtles [
    ;; intra village link variables
    set nr_of_intra_village_interaction_this_tick 0
    set next_intra_village_interaction_tick_nr 0


    ;; inter village link variables
    set nr_of_inter_village_interaction_this_tick 0
    set next_inter_village_interaction_tick_nr 0

    ;; generic interaction variables
    set nr_total_interactions 0
    set nr_topic_related_interactions 0
    set nr_of_council_interactions 0
    set nr_of_inter_village_interactions 0
    set nr_of_intra_village_interactions 0
    set adoption_state 0
    set innovation_adoption_attitude 0
    set color white
    set next_adoption_decision_tick_nr 0
  ]


  ask chiefs [
    set color red
    set next_chief_farmer_meeting_tick_nr 0
  ]

  ask links [
    set last_executed_tick ticks
  ]

  ask patches with [pcolor = green] [
    set pcolor white
  ]

  init_parameters
  init_export_lists
  clear-all-plots
  setup-plots
end

to export_world
  let path "pathToCSVFile"
  export-world path
end

to import_world
  let path "pathToCSVFile"
  import-world path
end

to reset_simulation
  if count turtles = 0 [
    import_world
  ]
  random-seed new-seed
  reset
end
@#$#@#$#@
GRAPHICS-WINDOW
353
11
1438
1097
-1
-1
4.191
1
10
1
1
1
0
0
0
1
-128
128
-128
128
1
1
1
ticks
30.0

BUTTON
21
44
98
77
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
117
83
197
116
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1480
51
1730
84
avg_nr_of_farmers_per_village
avg_nr_of_farmers_per_village
2
50
10.0
1
1
NIL
HORIZONTAL

SLIDER
1479
95
1729
128
nr_of_villages
nr_of_villages
2
500
100.0
1
1
NIL
HORIZONTAL

SLIDER
1480
139
1729
172
nr_default_friends_inter_village
nr_default_friends_inter_village
1
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
1480
282
1733
315
avg_mention_percentage
avg_mention_percentage
0
100
1.0
1
1
%
HORIZONTAL

BUTTON
117
43
195
77
Go Once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1480
417
1735
450
avg_inter_village_interaction_frequency
avg_inter_village_interaction_frequency
1
10
5.0
1
1
days
HORIZONTAL

SLIDER
1480
378
1734
411
avg_intra_village_interaction_frequency
avg_intra_village_interaction_frequency
1
10
4.0
1
1
days
HORIZONTAL

SLIDER
1480
457
1736
490
avg_chief_farmer_meeting_frequency
avg_chief_farmer_meeting_frequency
1
50
7.0
1
1
days
HORIZONTAL

BUTTON
22
84
98
117
Reset
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1482
12
1609
31
Setup Parameter
15
0.0
1

TEXTBOX
123
427
273
446
Interventions
15
0.0
1

BUTTON
19
483
108
516
Direct Ad
direct_village_intervention
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1480
637
1705
670
is_visible_update_activated
is_visible_update_activated
1
1
-1000

BUTTON
17
617
107
650
Train Chiefs
train_chiefs
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1479
675
1706
708
check_finished_condition
check_finished_condition
1
1
-1000

SLIDER
1480
322
1734
355
percentage_negative_WoM
percentage_negative_WoM
0
100
10.0
1
1
%
HORIZONTAL

PLOT
21
134
338
325
Number of Adopters over Time
days
Nr. of Adopters
0.0
370.0
0.0
1000.0
true
false
"" ""
PENS
"default" 1.0 0 -14730904 true "" "plot count turtles with [adoption_state = 2]"

INPUTBOX
232
48
336
108
run_until_day_x
-1.0
1
0
Number

SLIDER
1480
509
1738
542
base_adoption_probability
base_adoption_probability
0.1
100
1.0
1
1
%
HORIZONTAL

MONITOR
23
337
123
382
Nr. of Adopters
count turtles with [adoption_state = 2]
17
1
11

MONITOR
248
336
339
381
Total Nr. of Farmers
count turtles - 2
0
1
11

MONITOR
134
337
239
382
Aware Farmers
count turtles with [adoption_state = 1]
17
1
11

SLIDER
127
542
335
575
percentage_of_villagers_addressed
percentage_of_villagers_addressed
0.0
100
53.0
1
1
%
HORIZONTAL

SLIDER
127
484
331
517
direct_ad_nr_of_villages
direct_ad_nr_of_villages
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
126
616
336
649
train_chiefs_nr
train_chiefs_nr
0
100
80.0
1
1
NIL
HORIZONTAL

TEXTBOX
1484
247
1675
285
Simulation Parameter
15
0.0
1

TEXTBOX
1483
603
1633
622
UI Settings
15
0.0
1

SLIDER
1480
182
1689
215
nr_of_neighborhoods
nr_of_neighborhoods
0
nr_of_villages
20.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## Agent-based Model of Innovation Diffusion among Smallholder Farmer Households

This is an agent-based model simulation innovation diffusion amongst smallholder farmer households in Tanzania. It illustrates the social system in these regions with a focus on interactions.
The evironment is structured in villages which contain farmers and chiefs which toghether form farm groups. Furthermore, there are links between farmers/chiefs from different villages to facilite diffusion across village border.

The model is implemented in a modular way in order that it is easy adjustable or extendable. Its usage is straighforward: 1. set the values of the most important parameter on the UI, 2. click "Setup", 3. Click "Go" and intervene with farmers to introduce the innovation to the system, 4. enjoy monitoring the diffusion accross the network. After a simulation, "Setup" creates a new environment, while "Reset" only resets simulation (environment stays the same).

A detailed explanation of all features and the functionality of this model is included in the accompanying master thesis. 

### Credits

This model was developed in the course of the master thesis of Marc Zwimpfer at the University of Zurich under supervision of Prof. Dr. Lorenz Hilty and Dr. Matthias Huss in 2022.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person farmer
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -1 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -6459832 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -16777216 false 225 90 270 90
Line -16777216 false 225 15 225 90
Line -16777216 false 270 15 270 90
Line -16777216 false 247 15 247 90
Rectangle -6459832 true false 240 90 255 300

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

uzh
false
0
Rectangle -1 true false 15 75 30 195
Rectangle -1 true false 75 75 90 195
Rectangle -1 true false 105 75 195 90
Rectangle -1 true false 180 90 195 105
Rectangle -1 true false 165 105 180 120
Rectangle -1 true false 150 120 165 135
Rectangle -1 true false 135 135 150 150
Rectangle -1 true false 120 150 135 165
Rectangle -1 true false 105 165 120 180
Rectangle -1 true false 105 180 195 195
Rectangle -1 true false 210 75 225 195
Rectangle -1 true false 270 75 285 195
Rectangle -1 true false 225 135 270 150
Rectangle -1 true false 30 180 75 195

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment neg WoM" repetitions="10" runMetricsEveryStep="true">
    <setup>reset_direct</setup>
    <go>go</go>
    <timeLimit steps="365"/>
    <metric>count turtles with [adoption_state = 2]</metric>
    <enumeratedValueSet variable="percentage_negative_WoM">
      <value value="10"/>
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max_influence_attitude_on_decision">
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="direct_ad_experiment" repetitions="50" runMetricsEveryStep="true">
    <setup>reset_simulation</setup>
    <go>go</go>
    <timeLimit steps="365"/>
    <metric>count turtles with [adoption_state = 2]</metric>
    <metric>count turtles with [adoption_state = 1]</metric>
    <metric>count turtles with [adoption_state = 0]</metric>
    <enumeratedValueSet variable="direct_ad_frequency">
      <value value="7"/>
      <value value="14"/>
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="direct_ad_nr_of_villages">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage_of_villagers_addressed">
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="final_experiment_regression" repetitions="50" runMetricsEveryStep="false">
    <setup>reset_simulation</setup>
    <go>go</go>
    <timeLimit steps="365"/>
    <metric>count turtles with [adoption_state = 2]</metric>
    <enumeratedValueSet variable="direct_ad_frequency">
      <value value="7"/>
      <value value="14"/>
      <value value="21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="direct_ad_nr_of_villages">
      <value value="8"/>
      <value value="13"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage_of_villagers_addressed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="train_chiefs_nr">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="train_chiefs_frequency">
      <value value="7"/>
      <value value="14"/>
      <value value="21"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="qual_fiinal_experiment" repetitions="12" runMetricsEveryStep="true">
    <setup>reset_simulation</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>nr_of_adopters</metric>
    <metric>nr_of_considerers</metric>
    <metric>nr_of_unknown</metric>
    <metric>nr_villages_with_adopters</metric>
    <metric>nr_villages_with_considerers</metric>
    <enumeratedValueSet variable="direct_ad_frequency">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="direct_ad_nr_of_villages">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage_of_villagers_addressed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="train_chiefs_nr">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="train_chiefs_frequency">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
