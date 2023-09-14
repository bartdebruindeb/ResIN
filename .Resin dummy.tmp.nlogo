extensions[table matrix csv nw palette stats]

globals [
  global-attitude-correlation-matrix
  #nodes
  show-people?
  show-correlations?
  show-neighborships?
  correlation-correction
  #louvain-communities
  whole-world?
  #no-change-in-attitude
  correlation-corrected?

]

breed [people person]
breed [attitudes attitude]
undirected-link-breed [attitude-links attitude-link]
undirected-link-breed [neighborship-links neighborship-link]

attitude-links-own [
  weight
]

people-own [
  neighbors-attitude-correlation-matrix
  item-list
  level-list
  mean-neighbors-internal-coherence
  mean-global-internal-coherence
  reference-group
  adjusted-attitude?
]

attitudes-own [
  key_item
  level
  popularity
  lv-community
]

to setup
  clear-all
  reset-ticks
  if static-seed? [random-seed seed ]
  setup-attitudes                                              ;creating the attitude nodes for visualisation
  setup-people                                                 ;create the people and assign them with the preset items and levels in an item and level list
  compute-global-attitude-matrix                               ;based on the the level list of all the agent, the attitude-correlation-matrix of the whole population is computed
  compute-neighbors-attitude-matrix                            ;based on the the level list of the neighbouring agents, each agents compute their own neighbor attitude matrix
  setup-internal-coherence                                    ; agents compute the internal coherence of the attitudes the hold by retrieving the correlation coefficient from both matrices
  compute-global-attitude-matrix                                ;For situations when there are 3 levels or more, the disbution of internal coherence and attitude correlation matrixes needs..
  compute-neighbors-attitude-matrix                           ; ... to be corrected due to having more (0) than (1).
  update-internal-coherence                                 ;
  compute-attitude-network                                    ;The weights of the attitude-links equals the correlation coefficients from the global attitude corelation matrix. Only attitudes with correlation coefficient above zero are given an attitude-link above 0
  setup-world                                                 ; some visualisation global metrics functions
  update-appearance
end


to go
  write-attitudes-to-file
  set #no-change-in-attitude 0
  alter-attitude-system
  compute-global-attitude-matrix
  compute-neighbors-attitude-matrix
  update-internal-coherence
  update-appearance
 ; if remainder ticks 2 = 0 [export-view (word #levels ticks ".png")]
  tick
end

to setup-parameters
  set degree-of-cognition "whole-world"         ; "whole-world" "neighbors" "world-over-neighbors"
  set rb 0
 ; set #levels 5
  set seed 4
  set #subgroups 1
  set average-node-degree 30
  set world-over-neighbors 50
  set rewiring-proportion 0
  set degree-of-homophily 0
  set update-appearance? true
  set hide-people? true
  set color-communities-louvain? false
 ; set k 100
  set static-seed? false
 ; set ordinal-behavior? true
  set write-attitudes-to-file? false
 ; set #items 7
  set population-size 500                    ; float number
end

to setup-attitudes
  let i 0
  let l 0
  repeat (#levels * #items) [
    create-attitudes 1 [
      set key_item i
      set level l
      set size 3
      set color palette:scale-scheme "Divergent" "Spectral" #items key_item 0 #items
     ; palette:set-brightness palette:brightness * (0.4 + (0.6 / #levels * (level + 1)))
      set shape "dot"
      setxy 0 0]
    ifelse l = (#levels - 1) [
      set l 0
      set i i + 1
    ][
      set l l + 1]
  ]
  set #nodes #items * #levels
end

to adjust-attitudes-due-to-homophily
  let sub-group-size (population-size - remainder population-size #subgroups) / #subgroups
  let leftovers remainder population-size #subgroups
  let counter 1
  ask people [
    set adjusted-attitude? false]
  repeat #subgroups [
    let reference-person person (#nodes + floor (sub-group-size * counter ) - floor (sub-group-size / 2))
    let reference-list [item-list] of reference-person
    let lo 0
    if leftovers > 0 [set lo 1]
    ask people with [who < (sub-group-size * counter + #nodes + lo ) and adjusted-attitude? = false ] [
      let counter2 0
      foreach reference-list [
        y -> if random-float 1 < degree-of-homophily [
          set item-list replace-item counter2 item-list y
        ]

        set counter2 counter2 + 1
      ]
      set level-list (list)
      foreach item-list [
      r ->
        let l 0
        repeat #levels [
          ifelse r = l [
            set level-list lput 1 level-list
          ][
            set level-list lput 0 level-list ]
          set l l  + 1
        ]
      ]
      set adjusted-attitude? true
      set color palette:scale-gradient-hsb [[100 80 80] [360 80 80]] counter 1 #subgroups
    ]
    set counter counter + 1
    set leftovers leftovers - 1 ]




  ; print sub-group-size
end


to setup-people
  create-people population-size [
    setxy random-xcor random-ycor
    set size 2

  ]
  setup-peoples-attitudes
  setup-network
end

to setup-peoples-attitudes
  ask people [
    set item-list (list)
    set level-list (list)
    repeat #items [
      set item-list lput random #levels  item-list
    ]
  ]
  ask people [
    foreach item-list [
      r ->
      let l 0
      repeat #levels [
        ifelse r = l [
          set level-list lput 1 level-list
        ][
          set level-list lput 0 level-list ]
        set l l  + 1
      ]
    ]
  ]
end

to compute-global-attitude-matrix
  let dummy-matrix matrix:make-constant population-size #nodes 0
  ;print #nodes
  let row 0                                                                                 ; make dummy matrix
  ask people [
    matrix:set-row dummy-matrix row level-list
    set row row + 1
  ]
  let attitude-correlation-matrix matrix:make-constant #nodes #nodes 0                     ; correlation matrix with item respones as rows and columns
  let counter 1
  let counter2 0
  let counter3 #levels - 1
  repeat #nodes [                                                                          ; for every calculate the correlation between the other columns, ones calculated, column is removed to reduce
    let n 1 + counter3
    let empty-list (list)
    repeat counter + counter3 [                                                                       ; loop over all columns
      set empty-list lput 0 empty-list]
    repeat (#nodes - (counter + counter3)) [
      ; compute correlation for each column with the other columns on the right of the given column
      let #equal 0
      let r 0
      let list1 reduce sentence (list matrix:get-column dummy-matrix 0)
      let list2  reduce sentence (list matrix:get-column dummy-matrix n)                    ; create matrix containing two columns for which correlations are calculated
      (foreach list1 list2 [ [x y] -> if x = y [set #equal #equal + 1] ])
      set r  2 * (#equal / length list1 - 0.5) - correlation-correction
      set empty-list lput r  empty-list                                                  ; get correlation coefficient (r) for the two columns and put them in list
      set n n + 1
    ]
   ; output-print empty-list
    matrix:set-column attitude-correlation-matrix counter2 empty-list                                                          ; append correlation column into correlation matrix
    if (counter2 + 1) != #nodes [
      set dummy-matrix matrix:submatrix dummy-matrix 0 1 (population-size - 1) (#nodes - counter2) ]
    ; first column is removed to prevend double calculations
    set counter counter + 1
    set counter2 counter2 + 1
    ifelse counter3 = 0 [set counter3 #levels - 1] [ set counter3 counter3 - 1]
  ]
  set global-attitude-correlation-matrix attitude-correlation-matrix
end



to compute-neighbors-attitude-matrix
  ifelse degree-of-cognition = "whole-world" [
    ask people [set neighbors-attitude-correlation-matrix global-attitude-correlation-matrix]
    ][
    ask people [
      let dummy-matrix 0
      let #reference-group count neighborship-link-neighbors
        set dummy-matrix matrix:make-constant count reference-group #nodes 0
        let row 0                                                                                 ; make dummy matrix
        ask reference-group [
          matrix:set-row [dummy-matrix] of myself row level-list
        set row row + 1]                         ; 0 1 matrix with people as rows and item responses as columns
      let attitude-correlation-matrix matrix:make-constant #nodes #nodes 0                     ; correlation matrix with item respones as rows and columns
      let counter 1
      let counter2 0
      let counter3 #levels - 1
      repeat #nodes [                                                                          ; for every calculate the correlation between the other columns, ones calculated, column is removed to reduce
        let n 1 + counter3
        let empty-list (list)
        repeat counter + counter3 [                                                                       ; loop over all columns
          set empty-list lput 0 empty-list]
        repeat (#nodes - (counter + counter3)) [
          ; compute correlation for each column with the other columns on the right of the given column
          let #equal 0
          let r 0
          let list1 reduce sentence (list matrix:get-column dummy-matrix 0)
          let list2  reduce sentence (list matrix:get-column dummy-matrix n)                                  ; create matrix containing two columns for which correlations are calculated
          (foreach list1 list2 [ [x y] -> if x = y [set #equal #equal + 1] ])
          set r  2 * (#equal / length list1 - 0.5) - correlation-correction
          set empty-list lput r  empty-list                                                                              ; get correlation coefficient (r) for the two columns and put them in list
          set n n + 1
        ]
        matrix:set-column attitude-correlation-matrix counter2 empty-list                                                          ; append correlation column into correlation matrix
        if (counter2 + 1) != #nodes [
          set dummy-matrix matrix:submatrix dummy-matrix 0 1 (#reference-group - 1) (#nodes - counter2) ]
        ; first column is removed to prevend double calculations
        set counter counter + 1
        set counter2 counter2 + 1
        ifelse counter3 = 0 [set counter3 #levels - 1] [ set counter3 counter3 - 1]
      ]

      set neighbors-attitude-correlation-matrix attitude-correlation-matrix
    ]
  ]
end

to compute-attitude-network
  ask attitude-links [die]
  let column-counter 0
  repeat #nodes [
    let row-counter 0
    let c matrix:get-column global-attitude-correlation-matrix column-counter
    foreach c [
      x ->
      if row-counter != column-counter [
        if x > 0 [
          ask attitude column-counter [
            create-attitude-link-with attitude row-counter
            ask attitude-link-with attitude row-counter [set weight x]
          ]
        ]
      ]
      set row-counter row-counter + 1
    ]
    set column-counter column-counter + 1
  ]


end

to setup-internal-coherence
  ask people [
    set mean-neighbors-internal-coherence compute-internal-coherence neighbors-attitude-correlation-matrix
    set mean-global-internal-coherence compute-internal-coherence global-attitude-correlation-matrix
  ]
  set correlation-correction mean [mean-global-internal-coherence] of people
  set correlation-corrected? true
  adjust-attitudes-due-to-homophily
  ask people [
    set mean-neighbors-internal-coherence compute-internal-coherence neighbors-attitude-correlation-matrix
    set mean-global-internal-coherence compute-internal-coherence global-attitude-correlation-matrix
  ]
end

to update-internal-coherence
  ask people [
    set mean-neighbors-internal-coherence compute-internal-coherence neighbors-attitude-correlation-matrix
    set mean-global-internal-coherence compute-internal-coherence global-attitude-correlation-matrix
  ]
 ; set correlation-correction mean [mean-global-internal-coherence] of people
end

to-report compute-internal-coherence [attitude-correlation-matrix]
  let counter 0
  let position-list (list)
  let internal-coherence-list (list)
  foreach level-list [
    x -> if x > 0 [ set position-list lput counter position-list]
    set counter counter + 1]
  let counter2 0
  foreach position-list [
    x ->
    set position-list but-first position-list
    foreach position-list [ y ->
      set internal-coherence-list lput item y matrix:get-column attitude-correlation-matrix x internal-coherence-list
    ]
  ]
  report mean internal-coherence-list / (1 - correlation-correction)
end

to alter-attitude-system
  if degree-of-cognition = "whole-world" [
    ask people [compute-new-attitude global-attitude-correlation-matrix]
  ]
  if degree-of-cognition = "neighbors" [
    ask people [compute-new-attitude neighbors-attitude-correlation-matrix]
  ]
  if degree-of-cognition = "whole-world-AND-neighbors" [
    ifelse random-float 1 > world-over-neighbors [
      ask people [compute-new-attitude neighbors-attitude-correlation-matrix]
    ][
      ask people [compute-new-attitude global-attitude-correlation-matrix]
    ]
  ]

end


to compute-new-attitude [attitude-correlation-matrix]
    let item-in-dispute random #items
    let level-in-dispute item item-in-dispute item-list
    let attitude-in-dispute 0
    ifelse item-in-dispute = 0
    [set attitude-in-dispute item-in-dispute * #levels + level-in-dispute]
    [set attitude-in-dispute item-in-dispute * #levels + level-in-dispute ]
    let candidate-nodes (list)
  ifelse ordinal-behavior? [
    (ifelse
      level-in-dispute = 0 [
        set candidate-nodes (list attitude-in-dispute (attitude-in-dispute + 1))
      ]
      level-in-dispute = (#levels - 1) [
        set candidate-nodes (list (attitude-in-dispute - 1) attitude-in-dispute)
      ] [
        set candidate-nodes (list (attitude-in-dispute - 1) attitude-in-dispute (attitude-in-dispute + 1))
    ])

  ][
    let begin max (list 0 ( item-in-dispute * #levels))
    repeat #levels [
      set candidate-nodes lput begin candidate-nodes
      set begin begin + 1
    ]
  ]
    let counter 0
    let position-list (list)
    foreach level-list [
      x -> if x = 1 [
        set position-list lput counter position-list]
      set counter counter + 1]
    let c position attitude-in-dispute position-list
    set position-list remove attitude-in-dispute position-list                                                                 ; remove attitude-in-dispute to compute alternative correlations
    let candidate-correlation-list (list)
    foreach candidate-nodes [ n ->
      let dummy-list position-list
      let new-correlation (list)
      repeat (c) [
        let cor item n matrix:get-column attitude-correlation-matrix item 0 dummy-list
        set new-correlation lput cor new-correlation
        set dummy-list but-first dummy-list
      ]
      if length dummy-list > 0 [
        foreach dummy-list [
          z  ->
          set new-correlation lput (item z matrix:get-column attitude-correlation-matrix n) new-correlation]
      ]
      set candidate-correlation-list lput sum (new-correlation) candidate-correlation-list
    ]
    let pos length candidate-correlation-list
    let cdt-min min candidate-correlation-list

  ifelse cdt-min < 0 [
    set cdt-min abs cdt-min ^ k * -1
  ][
    set cdt-min cdt-min ^ k]

    let normalised-list (list)
    let probability-list (list)
  foreach candidate-correlation-list [
    x ->
    ifelse x < 0 [
      set x abs x ^ k * -1
    ][
      set x x ^ k]
    set normalised-list lput (x - cdt-min) normalised-list
  ]
    let sm-nl sum normalised-list

    foreach normalised-list [
      x ->
      let p 0
      ifelse x != 0 [
        set p rb / pos + (1 - rb) * (x / sm-nl )] [
        set p (rb / pos)]
      set probability-list lput p probability-list]
    let counter2 0
    let cum-probability-list (list)

    foreach probability-list [
      x -> ifelse counter2 = 0 [
        set cum-probability-list lput x cum-probability-list]
      [set cum-probability-list lput (item (counter2 - 1) cum-probability-list + x) cum-probability-list]
      set counter2 counter2 + 1]
    let prob random-float last cum-probability-list
    let new-attitude-node 0
    let found-it? false

    foreach cum-probability-list [
      x ->
      if prob = 0 [
        set new-attitude-node item (position x cum-probability-list) candidate-nodes ]
      if prob < x and found-it? = false [
        set new-attitude-node item (position x cum-probability-list) candidate-nodes
        set found-it? true
      ]
    ]



    (ifelse new-attitude-node = attitude-in-dispute [
      set #no-change-in-attitude #no-change-in-attitude + 1]
      new-attitude-node < attitude-in-dispute [
        set item-list replace-item item-in-dispute item-list ((item item-in-dispute item-list) - 1)
        set level-list replace-item attitude-in-dispute level-list 0
        set level-list replace-item (attitude-in-dispute - 1) level-list 1
        ;   print (word "prob: " prob)
        ;   print (word "old item list: " item-list)
        ;   print (word "old level list: " level-list)
        ;   print (word "item in dispute: " item-in-dispute)
        ;   print (word "level in dispute: " level-in-dispute)
        ;   print (word "attitude in dispute: " attitude-in-dispute)
        ;   print (word "candidate attitudes: " candidate-nodes)
        ;   print (word "position list: " position-list)
        ;  print (word "correlation list of alternative attitudes: " candidate-correlation-list)
        ;  print (word "normalised list: " normalised-list)
        ;  print (word "probability list: " probability-list)
        ;  print (word "cum probabality list: " cum-probability-list)
        ;  print (word "new-attitude: " new-attitude-node)
        ;  print (word "new-attitude: " attitude-in-dispute)

      ]
      new-attitude-node > attitude-in-dispute [
        set item-list replace-item item-in-dispute item-list ((item item-in-dispute item-list) + 1)
        set level-list replace-item attitude-in-dispute level-list 0
        set level-list replace-item (attitude-in-dispute + 1) level-list 1
        ; print (word "prob: " prob)
        ;  print (word "old item list: " item-list)
        ;  print (word "old level list: " level-list)
        ;  print (word "item in dispute: " item-in-dispute)
        ;  print (word "level in dispute: " level-in-dispute)
        ;  print (word "attitude in dispute: " attitude-in-dispute)
        ;  print (word "candidate attitudes: " candidate-nodes)
        ;  print (word "position list: " position-list)
        ;  print (word "correlation list of alternative attitudes: " candidate-correlation-list)
        ;  print (word "normalised list: " normalised-list)
        ;  print (word "probability list: " probability-list)
        ;  print (word "cum probabality list: " cum-probability-list)
        ;  print (word "new-attitude: " new-attitude-node)
        ;  print (word "new-attitude: " attitude-in-dispute)
    ])
    let counter4 0

    if level-in-dispute = 0 [
      if new-attitude-node < attitude-in-dispute [
        ;   print (word "new item list" item-list)
        ;  print (word "new level list" level-list)
      ]
    ]

end

to setup-world
  set show-people? true
  set show-correlations? true
  set show-neighborships? true
end


to update-appearance
  if update-appearance? [
    compute-attitude-network
    if hide-people? [
      set show-people? true
    show/hide-people]
    ask patches [ set pcolor white]
    size-attitude-nodes-on-popularity
    ; ask attitudes [ask patch-at 0 1 [ set plabel [level] of myself]]
    repeat 3 [layout-spring attitudes attitude-links  0.5 1 #nodes * 10]
    ask attitudes [set label level
    set label-color black]

    ask attitude-links [
      set color scale-color green weight 0 1.5
      set color 255 - color
      set thickness 0.12
      if show-correlations?  = false [hide-link]
    ]
    color-communities-louvain
    ask patch 25 29 [
      set plabel  (word "k = " k)
      set plabel-color black ]
        ask patch 25 27 [
      set plabel  (word "rb = " rb)
      set plabel-color black ]
            ask patch 25 25 [
      set plabel  (word "ticks = " ticks)
      set plabel-color black ]
            ask patch 25 23 [
      set plabel  (word "#levels = " #levels)
      set plabel-color black ]
            ask patch 25 21 [
      set plabel  (word "mean internal coherence = " precision mean [mean-global-internal-coherence] of people 3)
      set plabel-color black ]
  ]

end

to size-attitude-nodes-on-popularity
  ask attitudes [set popularity 0]
  ask people [
    let counter 0
    foreach level-list [x ->
      if x > 0 [
        ask attitude counter [set popularity popularity + 1 ]
      ]
      set counter counter + 1 ]
  ]
  ask attitudes [
    set size 1 + 30 * (popularity / population-size) ]
end

to show/hide-people
  ifelse show-people? = true [
    ask people [
      hide-turtle]
    set show-people? false
  ][
    ask people [
      show-turtle ]
    set show-people? true
  ]
end

to show/hide-correlations
  ifelse show-correlations? = true [
    ask attitude-links [
      hide-link
      set show-correlations? false]
  ][
    ask attitude-links [
      show-link]
    set show-correlations? true
  ]
end

to show/hide-neighborships
  ifelse show-neighborships? = true [
    ask neighborship-links [
      hide-link
      set show-neighborships? false]
  ][
    ask neighborship-links [
      show-link]
    set show-neighborships? true
  ]

end

to setup-network
  if degree-of-cognition = "neighbors" or degree-of-cognition = "whole-world-AND-neighbors" [
    layout-circle sort people 25.5
    ask people with [remainder who 2 = 0] [ fd 6 ]
    ask people [
      let max-iterations average-node-degree / 2
      let iterations 0
      let new-neighbor who + 1
      while [iterations <= max-iterations] [
        if new-neighbor = (population-size + #items * #levels) [
          set new-neighbor new-neighbor - population-size
        ]
        create-neighborship-link-with person new-neighbor
        set iterations iterations + 1
        set new-neighbor new-neighbor + 1
      ]
    ]
    if (rewiring-proportion > 0) [MS-rewiring]
    ask neighborship-links [ set thickness 1.5 * 1 / (0.035 * population-size)
      set color [255 0 0 25]]
  ]
  ask people [set reference-group neighborship-link-neighbors]

end;setup-network2


to MS-rewiring
  let A-who 99999
  let B-who 99999
  let C-who 99999
  let D-who 99999
  let successful-needed (rewiring-proportion * count neighborship-links)
  let successful 0

  while [successful < successful-needed]
  [;;first step: pick two pairs that might be rewired
    ask one-of people [
      set A-who who
      ask one-of neighborship-link-neighbors [ set B-who who ]
    ]
    ask one-of people [
      set C-who who
      ask one-of neighborship-link-neighbors [ set D-who who ]
    ]

    ;;second step: rewire if there aren't links already
    if ( (A-who != D-who) and (B-who != C-who) and (A-who != C-who) and (B-who != D-who)
      and not (is-link? link A-who D-who) and not (is-link? link B-who C-who) ) [
      ask person A-who [ ask neighborship-link-with person B-who [ die ] ]
      ask person A-who [ create-neighborship-link-with person D-who ]
      ask person C-who [ ask neighborship-link-with person D-who [ die ] ]
      ask person B-who [ create-neighborship-link-with person C-who ]
      set successful successful + 1
    ]
  ];while
end;MS_rewiring

to color-communities-louvain
  ifelse color-communities-louvain? = true [
    nw:set-context attitudes attitude-links
    ask patches [set pcolor white]
    let communities nw:louvain-communities
    set #louvain-communities length communities
    let d 1
    foreach communities [ [comm] ->
      ask comm [
        set lv-community d
        let c palette:scale-gradient-hsb [[100 80 80] [360 80 80]] d 1 #louvain-communities
        ask patches in-radius 2 [set pcolor c ]
        ask patches in-radius 1 [set pcolor white]
      ]
      set d d + 1

    ]
  ]
  [ ask patches [set pcolor white]]
end



to write-attitudes-to-file
  if write-attitudes-to-file? [
    if ticks <= 150 [
      ; we use the `of` primitive to make a list of lists and then
      ; use the csv extension to write that list of lists to a file.
      let counter 0
      let column-names (list (word "ticks"))
      repeat #items [ set column-names lput (word "item" counter) column-names
        set counter counter + 1]
      let complete-list (list column-names)
      ask people [
        ;let k fput ticks item-list
        set complete-list lput item-list complete-list ]


      csv:to-file (word "#items" #items ", " "#levels" #levels ", " "#ticks" ticks ", " "rb" rb ".csv") complete-list
    ]
  ]
end

to-report adjacency-matrix

  let aj-string (word)
  let counter 0
  let aj-list matrix:to-row-list  global-attitude-correlation-matrix
  foreach aj-list [ x ->
    foreach x [ y ->
      set aj-string (word aj-string " " y) ]
    set aj-string (word aj-string "\n")]
  report aj-string



end





to-report TRANSFORM-LIST! [#list #sep]
  if  #list = 0 [report #list]
  if not empty? #list [report (map [x -> (word x #sep)] #list)]
  report #list
end
@#$#@#$#@
GRAPHICS-WINDOW
298
45
740
488
-1
-1
6.113
1
10
1
1
1
0
0
0
1
-35
35
-35
35
1
1
1
ticks
30.0

SWITCH
0
212
116
245
static-seed?
static-seed?
1
1
-1000

INPUTBOX
54
248
117
308
seed
4.0
1
0
Number

BUTTON
5
111
68
144
NIL
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
5
178
68
211
NIL
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

BUTTON
2
145
75
178
go once
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
174
178
296
211
#levels
#levels
2
10
7.0
1
1
NIL
HORIZONTAL

SLIDER
176
141
297
174
#items
#items
1
12
6.0
1
1
NIL
HORIZONTAL

SLIDER
181
107
296
140
population-size
population-size
0
1000
500.0
20
1
NIL
HORIZONTAL

BUTTON
1
79
145
112
NIL
inspect one-of people
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
175
215
297
248
rb
rb
0
1
0.0
0.01
1
NIL
HORIZONTAL

PLOT
746
45
1200
482
Internal coherence distribution
NIL
NIL
-1.2
1.2
0.0
10.0
true
true
"" ""
PENS
"global" 0.05 1 -16449023 true "" "histogram [mean-global-internal-coherence] of people"
"Neighbors" 0.05 1 -2674135 true "" "histogram [mean-neighbors-internal-coherence] of people"

SWITCH
145
10
307
43
update-appearance?
update-appearance?
0
1
-1000

CHOOSER
118
281
296
326
degree-of-cognition
degree-of-cognition
"whole-world" "neighbors" "whole-world-AND-neighbors"
0

BUTTON
466
11
591
44
show/hide people
show/hide-people
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
591
10
743
43
NIL
show/hide-correlations
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
161
345
298
378
average-node-degree
average-node-degree
5
30
30.0
1
1
NIL
HORIZONTAL

SLIDER
150
379
297
412
rewiring-proportion
rewiring-proportion
0
1
0.0
0.01
1
NIL
HORIZONTAL

MONITOR
1209
62
1405
107
mean global-internal-coherence
mean [mean-global-internal-coherence] of people
17
1
11

BUTTON
312
10
475
43
NIL
show/hide-neighborships
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1210
111
1418
156
mean neighbor-internal-coherence
mean [mean-neighbors-internal-coherence] of people
17
1
11

PLOT
1207
161
1407
311
Mean internal coherence
NIL
NIL
0.0
10.0
-0.5
0.5
true
true
"" ""
PENS
"Mean" 1.0 0 -2674135 true "" "plot mean [mean-neighbors-internal-coherence] of people"

TEXTBOX
221
325
371
343
Small world
14
0.0
1

SWITCH
746
10
943
43
color-communities-louvain?
color-communities-louvain?
1
1
-1000

PLOT
33
572
298
692
Louvain communities
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot #louvain-communities"

TEXTBOX
111
420
295
441
Whole-world-AND-neighbors
14
0.0
1

MONITOR
1209
15
1343
60
NIL
correlation-correction
17
1
11

SLIDER
0
339
119
372
#subgroups
#subgroups
1
15
1.0
1
1
NIL
HORIZONTAL

SLIDER
0
379
138
412
degree-of-homophily
degree-of-homophily
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
118
442
290
475
world-over-neighbors
world-over-neighbors
0
1
50.0
0.01
1
NIL
HORIZONTAL

PLOT
1204
314
1404
464
#no-change-in-attitude
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot #no-change-in-attitude"

BUTTON
0
10
143
43
NIL
write-attitudes-to-file
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
2
42
149
75
write-attitudes-to-file?
write-attitudes-to-file?
1
1
-1000

SWITCH
947
10
1067
43
hide-people?
hide-people?
0
1
-1000

TEXTBOX
4
320
154
338
Subgroups
14
0.0
1

SLIDER
177
250
296
283
k
k
0
200
100.0
0.01
1
NIL
HORIZONTAL

SWITCH
150
44
294
77
ordinal-behavior?
ordinal-behavior?
0
1
-1000

PLOT
508
574
708
724
plot 1
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot nw:modularity(nw:louvain-communities)"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment ordinal true" repetitions="5" runMetricsEveryStep="true">
    <setup>setup-parameters
setup</setup>
    <go>go</go>
    <timeLimit steps="150"/>
    <metric>adjacency-matrix</metric>
    <enumeratedValueSet variable="k">
      <value value="0.5"/>
      <value value="1"/>
      <value value="3"/>
      <value value="20"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ordinal-behavior?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="#levels">
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="#items">
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="degree-of-cognition">
      <value value="&quot;whole-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rb">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="#levels">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="#subgroups">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-over-neighbors">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degree-of-homophily">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-appearance?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hide-people?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="color-communities-louvain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ordinal-behavior?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-attitudes-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="#items">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-size">
      <value value="500"/>
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
