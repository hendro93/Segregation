extensions [ matrix rnd gis profiler]
breed [districts district]
breed [staticempiricals staticempirical]

globals [ townshp
;  admshp jakbarshp jakpusshp jakselshp jaktimshp jakutshp ; shapefiles
          ethnicities religions ; lists of names
          town-popdata town-ethnicity-counts town-rel-counts town-totalpop all-thresholds ; lists of constants for statistical purposes
          decisions-count forced-moves-count searches-count moves-count
          b
]
districts-own [ id popdata ethnicity-counts rel-counts totalpop maxpop rel-maxpop
                perc-sim-eth perc-sim-rel
                indivs]
staticempiricals-own [ id popdata ethnicity-counts rel-counts totalpop maxpop rel-maxpop indivs] ; mirrors districts for statistical purposes and on the fly comparison


;; SETUP PROCEDURES

to setup
  clear-all
  load-gisdataset
;  let vars [ "WHTB_HG" "WHTB_MD" "WHTB_LW" "ASN_HGH" "ASIN_MD" "ASIN_LW" "BLCK_HG" "BLCK_MD" "BLCK_LW" "OTHR_HG" "OTHR_MD" "OTHR_LW" ]
;  let vars ["ISL_OTH" "ISL_CHN" "ISL_GRP" "CRS_OTH" "CRS_CHN" "CRS_GRP" "BUD_OTH" "BUD_CHN" "BUD_GRP" "OTH_OTH" "OTH_CHN" "OTH_GRP"]
  let vars ["GM" "GC" "GO" "CM" "CC" "CO" "BM" "BC" "BO" "OM" "OC" "OO"]
;  let vars ["GLO" "GMI" "GHI" "CLO" "CMI" "CHI" "BLO" "BMI" "BHI" "OLO" "OMI" "OHI"]
;  set ethnicities [ "WHITEB" "ASIAN" "BLACK" "OTHER" ]
  set ethnicities ["EGJ" "CHINESE" "EGS" "OTHER"]
  set religions [ "MUSLIM" "CHRISTIAN" "OTHER" ]
  foreach gis:feature-list-of townshp [ x ->
    let centroid gis:location-of gis:centroid-of x
    create-districts 1 [
      setxy item 0 centroid item 1 centroid
      set id gis:property-value x "NAMOBJ"
      let pops map [y -> round ((gis:property-value x y) / scale-down-pop)] vars
      set popdata (list ( sublist pops 0 3) ( sublist pops 3 6) ( sublist pops 6 9) ( sublist pops 9 12))
      setup-indivs-popdata-subcounts
      set maxpop round (totalpop / (1 - free-space))
      set rel-maxpop map [y -> round (y / (1 - free-space))] rel-counts
      hatch-staticempiricals 1 [ set size 0 ]
    ]
  ]
  ask districts [ create-district-neighbor-links ]
  ask staticempiricals [ create-district-neighbor-links ]
  set town-popdata matrix:to-row-list reduce matrix:plus [matrix:from-row-list popdata] of districts
  set town-ethnicity-counts count-ethnicities town-popdata
  set town-rel-counts count-rels town-popdata
  set town-totalpop count-totalpop town-popdata
  print-town-data
  carefully [visualize] [print error-message]


  reset-ticks
end

to load-gisdataset
  set townshp gis:load-dataset (word town "/" town ".shp")
;  set admshp gis:load-dataset (word town "/" town "merge.shp")
;  set jakbarshp gis:load-dataset (word town "/" "jakbar.shp")
;  set jakpusshp gis:load-dataset (word town "/" "jakpus.shp")
;  set jakselshp gis:load-dataset (word town "/" "jaksel.shp")
;  set jaktimshp gis:load-dataset (word town "/" "jaktim.shp")
;  set jakutshp gis:load-dataset (word town "/" "jakut.shp")
  gis:set-world-envelope (gis:envelope-union-of (gis:envelope-of townshp))
end

to create-district-neighbor-links
  let list-of-neighbors map [x -> one-of turtles with [breed = [breed] of myself and id = gis:property-value x "NAMOBJ"]]
                          (filter [x -> gis:intersects? x gis:find-one-feature townshp "NAMOBJ" id ] gis:feature-list-of townshp)
  create-links-with other (turtle-set list-of-neighbors)
end

to shuffle-population
  ifelse tie-houses-to-religion [
    ; The following sets popdata in each district such that ethnicity counts in each religious group are proportional to town-wide ethnicity counts in each religious groups.
    ; The rel counts in each district remain as original. This makes the ethnic mean local Simpson index minimal (entropy maximal) while keeping religion structure as in reality.
    let townrelfrac map [x -> normalize-list x]  matrix:to-column-list matrix:from-row-list town-popdata
    ask districts [ set popdata matrix:to-row-list matrix:map round matrix:from-column-list (map [ [x vec] -> map [y -> x * y] vec] rel-counts townrelfrac) ]
  ][
    let towntotalfrac matrix:to-row-list matrix:map [x -> x / sum [totalpop] of districts] matrix:from-row-list town-popdata
    ask districts [ set popdata matrix:to-row-list matrix:map round (matrix:from-row-list towntotalfrac matrix:*  totalpop) ]
  ]
  ask districts [ setup-indivs-popdata-subcounts ]
  carefully [visualize] [print error-message]
  clear-all-plots
  reset-ticks
end

to setup-indivs-popdata-subcounts
  set ethnicity-counts count-ethnicities popdata
  set rel-counts count-rels popdata
  set totalpop count-totalpop popdata

  set indivs reduce sentence map [z -> reduce sentence map [y -> n-values item y item z popdata [x -> (list z y random-beta-musigma threshold-mean threshold-sd)]] range length item z popdata] range length popdata
  set all-thresholds reduce sentence [map [x -> item 2 x] indivs] of districts

end


;; GO PROCEDURES

to go
  reset-timer
  set decisions-count 0 set forced-moves-count 0 set searches-count 0 set moves-count 0
  ask districts [compute-percentage-similar]
  repeat town-totalpop [ ask one-of districts [
    let nummoves totalpop / (town-totalpop / count districts)
    repeat floor (nummoves) [individual-decides]
    if random-float 1 < (nummoves - floor (nummoves)) [individual-decides] ; to make the expected number of moves per tick equal to the number of individuals
  ]]
  carefully [visualize] [
    print error-message
    stop
  ]
  print (word
    ifelse-value (always-search) [""] [(word decisions-count " decisions, ")]
    ifelse-value (turnover > 0) [(word forced-moves-count " randomly replaced via turnover (" precision (100 * forced-moves-count / decisions-count) 1 "%), ")] [""]
    searches-count " searches (" precision (100 * searches-count / decisions-count) 1 "%), "
    moves-count " moves (" precision (100 * moves-count / decisions-count) 1 "%) in  " timer " seconds")
  tick
;  if ticks = stop-tick or (moves-count / decisions-count < 0.01) [visualize stop]
  if ticks = stop-tick [visualize stop]
end

to compute-percentage-similar
  set perc-sim-eth (list percent-similar-ethnicity-neighborhood 0  percent-similar-ethnicity-neighborhood 1  percent-similar-ethnicity-neighborhood 2  percent-similar-ethnicity-neighborhood 3)
  set perc-sim-rel (list percent-similar-rel-neighborhood 0  percent-similar-rel-neighborhood 1  percent-similar-rel-neighborhood 2)
end

to individual-decides ; in a districts select a "virtual" person and let this decide to move
  set decisions-count decisions-count + 1
  let indiv-ind random length indivs
  let indiv item indiv-ind indivs
  let ethn-ind item 0 indiv
  let rel-ind item 1 indiv
  ifelse random-float 1 < turnover [
    set forced-moves-count forced-moves-count + 1
    let option ifelse-value (tie-houses-to-religion) [random-rel-option ethn-ind rel-ind] [random-option ethn-ind rel-ind]
    individual-moves option ethn-ind rel-ind indiv-ind indiv
  ] [
    let thresh item 2 indiv
    let U_home utility ethn-ind rel-ind thresh
    if U_home < 0 or always-search [
      set searches-count searches-count + 1
      let option ifelse-value (tie-houses-to-religion) [random-rel-option ethn-ind rel-ind] [random-option ethn-ind rel-ind]
      let U_option [utility ethn-ind rel-ind thresh] of option
      if (U_option - U_home > 0) or (always-move) [
        set moves-count moves-count + 1
        individual-moves option ethn-ind rel-ind indiv-ind indiv
      ]
    ]
  ]
end

to individual-moves [option ethn-ind rel-ind indiv-ind indiv]
  alter-popdata ethn-ind rel-ind -1
  set indivs remove-item indiv-ind indivs
  ask option [
    alter-popdata ethn-ind rel-ind 1
    set indivs fput indiv indivs
  ]
end

to alter-popdata [ethn-ind rel-ind change] ; change should be 1 or -1
  set popdata replace-item ethn-ind popdata (replace-item rel-ind (item ethn-ind popdata) (item rel-ind item ethn-ind popdata + change))
  set ethnicity-counts replace-item ethn-ind ethnicity-counts (item ethn-ind ethnicity-counts + change)
  set rel-counts replace-item rel-ind rel-counts (item rel-ind rel-counts + change)
  set totalpop totalpop + change
end

;; REPORTERS DECISIONS TO SEARCH / MOVE
; For selection of "a random person"
to-report random-option [ethn-ind rel-ind]
  report rnd:weighted-one-of districts [max list 0 (maxpop - totalpop) * ifelse-value ethn-rel-recommendations [recommendation-probability ethn-ind rel-ind] [1]]
end
to-report random-rel-option [ethn-ind rel-ind]
report rnd:weighted-one-of districts [max list 0 (item rel-ind rel-maxpop - item rel-ind rel-counts) *
                                                 ifelse-value ethn-rel-recommendations [recommendation-probability ethn-ind rel-ind] [1]]
end
; For utility computation
to-report utility [ethn-ind rel-ind thresh] report  (observable-utility ethn-ind rel-ind thresh) + random-gumbel end
to-report observable-utility [ethn-ind rel-ind thresh] ; the utility concept here is linear in similarity fractions, shifted such that 0 divides favorable and non-favorable
  report (ifelse-value (others-ignore-ethn and ethn-ind = 3) [0] [beta-eth * (item ethn-ind perc-sim-eth - thresh)]) +
         beta-rel * (item rel-ind perc-sim-rel - thresh)
end
to-report recommendation-probability [ethn-ind rel-ind]
  ifelse (((ifelse-value (others-ignore-ethn and ethn-ind = 3) [0] [beta-eth]) + beta-rel) = 0)
    [report 1]
    [report ((ifelse-value (others-ignore-ethn and ethn-ind = 3) [0] [beta-eth * item ethn-ind perc-sim-eth]) +
                      beta-rel * (item rel-ind perc-sim-rel)) / ((ifelse-value (others-ignore-ethn and ethn-ind = 3) [0] [beta-eth]) + beta-rel) / 2]
end
to-report percent-similar-ethnicity-neighborhood [ethn-ind]
  report (item ethn-ind ethnicity-counts + neighbor-weight * sum [item ethn-ind ethnicity-counts] of link-neighbors) /
         (totalpop + neighbor-weight * sum [totalpop] of link-neighbors)
end
to-report percent-similar-rel-neighborhood [rel-ind]
  report (item rel-ind rel-counts + neighbor-weight * sum [item rel-ind rel-counts] of link-neighbors) /
         (totalpop + neighbor-weight * sum [totalpop] of link-neighbors)
end

;; VISUALIZATION AND PRINT OUTPUT

to visualize

  ask patches [set pcolor ifelse-value (data-source = "simulation (dynamic)") [77] [white]]
  ask turtles [set size 0 set label ""]
  foreach gis:feature-list-of townshp [ x ->
    let dist ifelse-value (data-source = "empirical (static)")

      [one-of staticempiricals with [id = (gis:property-value x "NAMOBJ")]]
      [one-of districts with [id = (gis:property-value x "NAMOBJ")]]
    let val value-for-monitoring dist
    gis:set-drawing-color ifelse-value (is-number? val) [ifelse-value (val >= 0) [scale-color red val color-axis-max 0] [scale-color blue (0 - val) color-axis-max 0]] [gray]
    gis:fill x 0
;    ask dist [ set size 0 set label ifelse-value is-number? val [precision val 2] [val] set label-color 114  set hidden? not show-labels ]
  ]
  ask links [hide-link]
  gis:set-drawing-color grey
  gis:draw townshp 1
  gis:set-drawing-color 130
;  gis:draw admshp 5
;  gis:set-drawing-color red
;  gis:draw jakbarshp 2
;  gis:set-drawing-color blue
;
;  gis:draw jakselshp 2
;  gis:set-drawing-color cyan
;  gis:draw jaktimshp 2
;  gis:set-drawing-color green
;  gis:draw jakutshp 2
;  gis:set-drawing-color orange
;  gis:draw jakpusshp 2

end

to toggle-color-axis-max
  set color-axis-max ifelse-value (color-axis-max = 1) [precision max fput 0.1 [abs value-for-monitoring self] of ifelse-value (data-source = "empirical (static)") [staticempiricals] [districts] 1] [1]
;  set color-axis-max ifelse-value (color-axis-max = 1) [precision max (list 0.1 [abs value-for-monitoring self] of ifelse-value (data-source = "empirical (static)") [staticempiricals] [districts]) 1] [1]
end

to print-town-data
  clear-output
  let all1674 (map [x -> gis:property-value x "TOTAL_S"] gis:feature-list-of townshp)
  output-print (word town ": demographic data used")
  output-print (word "Population (religion): " (sum all1674))
  output-print (word "Sub-districts (Kelurahan): " (length all1674))
  output-print (word "  Pop mean " round (sum all1674 / length all1674) ", min " (min all1674) ", max " (max all1674) )
  output-print ""
  output-print "Ethnicities (%)"
  output-print ethnicities
  output-print map [x -> precision ((100 / town-totalpop) * x) 1] (town-ethnicity-counts)
  foreach range length religions [x -> output-print (word "  " item x religions ": " rounded-percentages matrix:get-column (matrix:from-row-list town-popdata) x)]
  output-print ""
  output-print "Religious adherents (%)"
  output-print religions
  output-print map [x -> precision ((100 / town-totalpop) * x) 1] (town-rel-counts)
  foreach range length ethnicities [x -> output-print (word "  " item x ethnicities ": " rounded-percentages (item x town-popdata)
                                                       " AvRel " precision average-rel-from-list (item x town-popdata) 2)]
  output-print ""
  output-print "All subgroups (rows Ethn, cols Rel)"
  output-print matrix:pretty-print-text matrix:map [x -> precision x 1]
     matrix:times-scalar (matrix:from-row-list town-popdata) (100 / town-totalpop)
  output-print ""
  output-print "Ethnic segregation measures"
  output-print (word " Avg local Simpson index: " precision (sum [totalpop * ethnic-simpson] of districts / sum [totalpop] of districts) 3)
  output-print (word " Town Simpson index: " precision town-ethnic-simpson 3)
  output-print (word " Excess avg local Simpson index: " precision (sum [totalpop * (ethnic-simpson - town-ethnic-simpson)] of districts / sum [totalpop] of districts) 3)
;  foreach range length ethnicities [x -> output-print (word " Dissimilarity " item x ethnicities ": " precision (sum [totalpop * dissimilarity x "all"] of districts / sum [totalpop] of districts) 3)]
  foreach range length ethnicities [x -> output-print (word " Dissimilarity " item x ethnicities ": " precision (sum [dissimilarity x "all"] of districts / (2 * town-totalpop * item x town-ethnicity-counts / town-totalpop * (1 - item x town-ethnicity-counts / town-totalpop ))) 3)]

  ; Pi = item ethn-ind ethnicity-counts / totalpop
  ; P = item ethn-ind town-ethnicity-counts / town-totalpop
  ; ti = totalpop
  ; T = town-totalpop

;  foreach range length ethnicities [x -> output-print (word " Dissimilarity " item x ethnicities ": " precision
;    (1 - (item x town-ethnicity-counts / town-totalpop / (1 - item x town-ethnicity-counts / town-totalpop)) * ((1 / ((item x town-ethnicity-counts / town-totalpop) * town-totalpop) * sum [dissimilarity x "all"] of districts)) ^ (1 / (1 - b))) 3)]
end

;; REPORTER FOR MONITORING

to-report color-explain-string
  report (word measure
    ifelse-value (substring measure 0 9 = "ethnicity") [word " " ethnicity] [""]
    ifelse-value (substring measure 0 3 = "religion" or member? "-religion" measure ) [word " " religion] [""]
    ifelse-value (data-source = "empirical (static)") [" (emp)"] [" (sim)"] )
end
to-report value-for-monitoring [dist]
  report (ifelse-value
    (measure = "Simpson index") [ [ethnic-simpson] of dist ]
    (measure = "entropy index") [ [ethnic-entropy] of dist]
    (measure = "excess Simpson index") [ [ethnic-simpson] of dist - town-ethnic-simpson]
    (measure = "ethnic entropy") [ [ethnic-entropy] of dist]
    (measure = "loss ethnic entropy") [ town-ethnic-entropy - [ethnic-entropy] of dist]
    (measure = "pop / max pop") [ [totalpop / maxpop] of dist ]
    (measure = "pop / mean pop") [ [totalpop ] of dist / town-totalpop * count districts ]
    (measure = "ethnicity fraction") [ [item (position (ethnicity) ethnicities) ethnicity-counts] of dist / [totalpop] of dist ]
    (measure = "ethnicity dissimilarity") [ [dissimilarity (position (ethnicity) ethnicities) dissimilarity-religion] of dist ]
    (measure = "ethnicity location quotient") [ [location-quotient (position (ethnicity) ethnicities)] of dist ]
    (measure = "ethnicity-religion obs utility")  [ [observable-utility (position (ethnicity) ethnicities) (position (religion) religions) threshold-mean ] of dist ]
    (measure = "ethnicity-religion fraction") [ [ item (position (religion) religions) (item (position (ethnicity) ethnicities) popdata) ] of dist / [totalpop] of dist ]
    (measure = "ethnicity-religion loc. quo.") [ [location-quotient-rel (position (ethnicity) ethnicities) (position (religion) religions)] of dist ]
    (measure = "avg threshold") [ mean-threshold [indivs] of dist ]
    (measure = "ethnicity avg threshold") [ mean-threshold [filter [y -> item 0 y = position (ethnicity) ethnicities] indivs] of dist ]
    (measure = "religion avg threshold") [ mean-threshold [filter [y -> item 1 y = position (religion) religions] indivs] of dist ]
    (measure = "ethnicity-religion avg thres") [ mean-threshold [filter [y -> (item 1 y = position (religion) religions) and (item 0 y = position (ethnicity) ethnicities)] indivs] of dist ]
    (measure = "avg religion") [ [average-rel] of dist ]
    (measure = "ethnicity avg religion") [[ethnicity-average-rel position (ethnicity) ethnicities] of dist ]
    (measure = "religion fraction") [ [item (position (religion) religions) rel-counts] of dist / [totalpop] of dist ]
      [1])
end
to-report simpson [p] report sum (map [x -> x ^ 2] p) end
to-report entropy [p] report 0 - 1 / ln (length p) * (sum (map [x -> x * ifelse-value (x = 0) [0] [ln x]] p)) end
to-report ethnic-simpson report simpson sublist (normalize-list ethnicity-counts) 0 (ifelse-value (others-ignore-ethn) [3] [4]) end
to-report ethnic-entropy report entropy normalize-list ethnicity-counts end
to-report rel-simpson report simpson normalize-list rel-counts end
to-report rel-entropy report entropy normalize-list rel-counts end
to-report town-ethnic-simpson report simpson sublist (normalize-list town-ethnicity-counts) 0 (ifelse-value (others-ignore-ethn) [3] [4]) end
to-report town-ethnic-entropy report entropy normalize-list count-ethnicities town-popdata end
to-report average-rel report (sum (map [[ x y ] -> x * y] rel-counts range length religions)) / totalpop / (length religions - 1) end
to-report ethnicity-average-rel [ethn-ind] report ifelse-value (item ethn-ind ethnicity-counts > 0)
  [(sum (map [[ x y ] -> x * y] item ethn-ind popdata range length religions)) / item ethn-ind ethnicity-counts /  (length religions - 1)] ["NA"] end
to-report average-rel-from-list [count-list] report (sum (map [[x y] -> x * y] count-list range length count-list)) / sum count-list / (length count-list - 1) end

  ; Pi = item ethn-ind ethnicity-counts / totalpop
  ; P = item ethn-ind town-ethnicity-counts / town-totalpop
  ; ti = totalpop
  ; T = town-totalpop

to-report dissimilarity [ethn-ind rel-str]
  ifelse (rel-str = "all") [
    report totalpop * abs (item ethn-ind ethnicity-counts / totalpop - item ethn-ind town-ethnicity-counts / town-totalpop)
  ][
    let rel-ind position rel-str religions
    report abs (item rel-ind item ethn-ind popdata / totalpop - item rel-ind item ethn-ind town-popdata / town-totalpop) /
           (2 * item rel-ind item ethn-ind town-popdata / town-totalpop * (1 - item rel-ind item ethn-ind town-popdata / town-totalpop))
  ]
end

to-report dissimilarity-string [ethn-ind rel-str] report (word (precision (sum [dissimilarity ethn-ind rel-str] of districts / (2 * sum [totalpop] of districts * item ethn-ind town-ethnicity-counts / town-totalpop * (1 - item ethn-ind town-ethnicity-counts / town-totalpop))) 3)
  " (emp " (precision (sum [dissimilarity ethn-ind rel-str] of staticempiricals / (2 * town-totalpop * item ethn-ind town-ethnicity-counts / town-totalpop * (1 - item ethn-ind town-ethnicity-counts / town-totalpop))) 3) ")") end

to-report location-quotient [ethn-ind] report (item ethn-ind ethnicity-counts / item ethn-ind town-ethnicity-counts) / (totalpop / town-totalpop) end
to-report location-quotient-rel [ethn-ind rel-ind] report (item rel-ind item ethn-ind popdata / item rel-ind item ethn-ind town-popdata) / (totalpop / town-totalpop) end
;to-report interaction [ethn-ind1 ethn-ind2]
;  let dists ifelse-value data-source = "empirical (static)" [staticempiricals] [districts]
;  let rel-ind1 position interacter-rel religions
;  let rel-ind2 position interact-with-rel religions
;  report sum [ ((ifelse-value rel-ind1 = false [item ethn-ind1 ethnicity-counts] [item rel-ind1 item ethn-ind1 popdata]) /
;                (ifelse-value rel-ind1 = false [item ethn-ind1 town-ethnicity-counts] [item rel-ind1 item ethn-ind1 town-popdata])) *
;               ((ifelse-value rel-ind2 = false [item ethn-ind2 ethnicity-counts] [item rel-ind2 item ethn-ind2 popdata]) /
;                totalpop) ] of dists
;end

to-report moran-I [dists]
  let m mean [value-for-monitoring self] of dists
  report (count dists / (sum [count link-neighbors] of dists)) *
         ((sum [sum [(value-for-monitoring self - m) * (value-for-monitoring myself - m)] of link-neighbors] of dists) / sum [(value-for-monitoring self - m) ^ 2] of dists)
end
;to-report moran-I [dists]
;  let m mean [value-for-monitoring self] of dists
;  report ((sum [sum [(1 / count link-neighbors) * (value-for-monitoring self - m) * (value-for-monitoring myself - m)] of link-neighbors] of dists) / sum [(value-for-monitoring self - m) ^ 2] of dists)
;end

;; GENERAL REPORTERS
; For computations on popdata-type lists of lists
to-report count-rels [popd] report map [y -> sum map [x -> item y x] popd] range length religions end
to-report count-ethnicities [popd] report map sum popd end
to-report count-totalpop [popd] report sum map sum popd end
; General
to-report mean-threshold [indiv-list] report ifelse-value (length indiv-list > 0) [mean map [x -> item 2 x] indiv-list] [threshold-mean] end
to-report normalize-list [x] report map [y -> y / sum x] x end
to-report rounded-percentages [x] report map [y -> precision (100 * y) 1] normalize-list x end
to-report random-beta-musigma [m s]
  ifelse (s > 0) [
    let x random-gamma (alpha-musigma m s) 1
    report ( x / ( x + random-gamma (beta-musigma m s) 1) )
  ][ report m ]
end
to-report alpha-musigma [m s] report max list 0.001 (m * ((m * (1 - m)) / s ^ 2 - 1)) end
to-report beta-musigma [m s] report max list 0.001 ((1 - m) * ((m * (1 - m)) / s ^ 2 - 1)) end
to-report random-gumbel report (- ln (- ln random-float 1)) end


to-report simpson-index report precision (sum [(ethnic-simpson - town-ethnic-simpson) * totalpop] of districts / sum [totalpop] of districts) 3 end
to-report EGJ report precision (sum [dissimilarity 0 "all"] of districts / (2 * sum [totalpop] of districts * item 0 town-ethnicity-counts / town-totalpop * (1 - item 0 town-ethnicity-counts / town-totalpop))) 3 end
to-report CHN report precision (sum [dissimilarity 1 "all"] of districts / (2 * sum [totalpop] of districts * item 1 town-ethnicity-counts / town-totalpop * (1 - item 1 town-ethnicity-counts / town-totalpop))) 3 end
to-report EGS report precision (sum [dissimilarity 2 "all"] of districts / (2 * sum [totalpop] of districts * item 2 town-ethnicity-counts / town-totalpop * (1 - item 2 town-ethnicity-counts / town-totalpop))) 3 end
to-report OTH report precision (sum [dissimilarity 3 "all"] of districts / (2 * sum [totalpop] of districts * item 3 town-ethnicity-counts / town-totalpop * (1 - item 3 town-ethnicity-counts / town-totalpop))) 3 end
to-report moranI report precision moran-I districts 3 end

;; BASELINE SETTINGS

to baseline-further-parameters
  set free-space 0.05
  set turnover 0
  set always-search false
  set always-move false
  set neighbor-weight 0.17
  set others-ignore-ethn true
end

to baseline-core-parameters
  set threshold-mean 0.3
  set threshold-sd 0.1
  set tie-houses-to-religion true
  set beta-eth 8
  set beta-rel 12
end

;; EXPORT DATA

;to export-town_th-m_th-sd_tiehouses_b-eth_b-rel_ticks [TOW TH-M TH-SD TIEHOUSE B-ETH B-REL TI]
;  set town TOW
;  set scale-down-pop 10
;  baseline-further-parameters
;  set threshold-mean TH-M
;  set threshold-sd TH-SD
;  set tie-houses-to-religion TIEHOUSE
;  set beta-eth B-ETH
;  set beta-rel B-REL
;  set stop-tick TI
;  setup
;  shuffle-population
;  repeat stop-tick [ go ]
;  export-world (word "worlds/" TOWN "_" TH-M "_" TH-SD "_" TIEHOUSE "_" B-ETH "_" B-REL "_t" TI ".csv")
;end
;
;to load-town_th-m_th-sd_tiehouses_b-eth_b-rel_ticks [TOW TH-M TH-SD TIEHOUSE B-ETH B-REL TI]
;  import-world (word "worlds/" TOWN "_" TH-M "_" TH-SD "_" TIEHOUSE "_" B-ETH "_" B-REL "_t" TI ".csv")
;  load-gisdataset
;end
@#$#@#$#@
GRAPHICS-WINDOW
518
122
1088
693
-1
-1
17.03030303030303
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
182
42
313
77
Load Town to Sim
setup\n
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
516
42
714
122
Update Map and Plots
carefully [visualize] [print error-message]\nupdate-plots
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
463
622
519
692
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

BUTTON
315
336
516
371
Shuffle Population
shuffle-population
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
182
78
313
111
scale-down-pop
scale-down-pop
20
500
100.0
10
1
NIL
HORIZONTAL

PLOT
1089
42
1310
199
distribution districts
sorted districts
measure
0.0
10.0
0.0
1.0
true
true
"" "clear-plot\nset-plot-x-range 0 count districts"
PENS
"sim" 1.0 0 -2674135 true "" "let y sort filter is-number? map value-for-monitoring [self] of districts\nforeach range length y [x -> plotxy x item x y ]"
"emp" 1.0 0 -7500403 true "" "let y sort filter is-number? map value-for-monitoring [self] of staticempiricals\nforeach range length y [x -> plotxy x item x y ]\n"

PLOT
1310
42
1492
199
histogram districts
measure
#districts
0.0
1.0
0.0
50.0
true
false
"" "clear-plot\n;max [value-for-monitoring self] of staticempiricals\nset-plot-x-range precision (min (fput 0 [value-for-monitoring self] of staticempiricals)) 1  precision (max (fput 1 [value-for-monitoring self] of staticempiricals)) 1"
PENS
"pen-1" 0.025 1 -4539718 true "" "histogram map value-for-monitoring [self] of staticempiricals"
"pen-2" 0.025 1 -2674135 true "" "histogram filter is-number? map value-for-monitoring [self] of districts"

INPUTBOX
3
42
180
111
town
Jakarta
1
0
String

OUTPUT
3
112
313
692
12

SLIDER
1493
58
1714
91
free-space
free-space
0
0.4
0.05
0.01
1
NIL
HORIZONTAL

SWITCH
1568
146
1714
179
always-search
always-search
1
1
-1000

SWITCH
315
302
517
335
tie-houses-to-religion
tie-houses-to-religion
1
1
-1000

CHOOSER
315
42
516
87
data-source
data-source
"empirical (static)" "simulation (dynamic)"
0

PLOT
1088
225
1493
370
segregation index
time
Simpson
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"avg local Simpson index (sim)" 1.0 0 -13345367 true "" "plot sum [totalpop * ethnic-simpson] of districts / sum [totalpop] of districts"
"town Simpson index" 1.0 0 -16777216 true "" "plot town-ethnic-simpson"
"avg local Simpson index (emp)" 1.0 0 -7500403 true "" "plot sum [totalpop * ethnic-simpson] of staticempiricals / sum [totalpop] of districts"

SLIDER
315
268
517
301
threshold-sd
threshold-sd
0
0.3
0.1
0.01
1
NIL
HORIZONTAL

PLOT
315
372
516
504
thresholds
threshold
#agents
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 0.025 1 -16777216 true "" "histogram all-thresholds"

MONITOR
1323
325
1492
370
excess avg Simpson index
(word (precision (sum [(ethnic-simpson - town-ethnic-simpson) * totalpop] of districts / sum [totalpop] of districts) 3)\n   \" (emp. \" (precision (sum [(ethnic-simpson - town-ethnic-simpson) * totalpop] of staticempiricals / sum [totalpop] of staticempiricals) 3) \")\")
3
1
11

CHOOSER
315
130
423
175
ethnicity
ethnicity
"EGJ" "CHINESE" "EGS" "OTHER"
3

CHOOSER
422
130
516
175
religion
religion
"MUSLIM" "CHRISTIAN" "OTHER"
2

CHOOSER
315
86
516
131
measure
measure
"--- for specific ethnicty ---" "ethnicity fraction" "ethnicity dissimilarity" "ethnicity location quotient" "ethnicity avg threshold" "ethnicity avg religion" "--- for specific religion ---" "religion fraction" "religion avg threshold" "--- for specific ethnicity and religion ---" "ethnicity-religion fraction" "ethnicity-religion loc. quo." "ethnicity-religion avg thres" "ethnicity-religion obs utility" "--- local indices ---" "Simpson index" "entropy index" "excess Simpson index" "loss ethnic entropy" "--- other measures ---" "pop / mean pop" "pop / max pop" "avg threshold" "avg religion"
17

SLIDER
315
235
517
268
threshold-mean
threshold-mean
0
1
0.3
0.01
1
NIL
HORIZONTAL

TEXTBOX
9
10
270
30
1. Load Town from GIS Data 
18
114.0
1

TEXTBOX
317
10
591
32
2. Explore Local Data
18
114.0
1

TEXTBOX
316
185
495
205
3. Setup Simulation
18
114.0
1

TEXTBOX
319
206
457
230
individual thresholds from Beta-distribution
9
0.0
1

TEXTBOX
323
562
483
584
4. Run Simulation
18
114.0
1

MONITOR
1356
415
1492
460
Dissimilarity CHINESE
dissimilarity-string 1 dissimilarity-religion
3
1
11

MONITOR
1356
370
1492
415
Dissimilarity EGJ
dissimilarity-string 0 dissimilarity-religion
3
1
11

MONITOR
1356
462
1492
507
Dissimilarity EGS
dissimilarity-string 2 dissimilarity-religion
3
1
11

MONITOR
1356
505
1492
550
Dissimilarity OTHER
dissimilarity-string 3 dissimilarity-religion
3
1
11

PLOT
1088
370
1356
544
dissimilarity
time
dissimilarity
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"EGJ" 1.0 0 -10899396 true "" "plot sum [dissimilarity 0 \"all\"] of districts / (2 * town-totalpop * item 0 town-ethnicity-counts / town-totalpop * (1 - item 0 town-ethnicity-counts / town-totalpop ))"
"CHINESE" 1.0 0 -2674135 true "" "plot sum [dissimilarity 1 \"all\"] of districts / (2 * town-totalpop * item 1 town-ethnicity-counts / town-totalpop * (1 - item 1 town-ethnicity-counts / town-totalpop ))"
"EGS" 1.0 0 -13345367 true "" "plot sum [dissimilarity 2 \"all\"] of districts / (2 * town-totalpop * item 2 town-ethnicity-counts / town-totalpop * (1 - item 2 town-ethnicity-counts / town-totalpop ))"
"OTHER" 1.0 0 -7500403 true "" "plot sum [dissimilarity 3 \"all\"] of districts / (2 * town-totalpop * item 3 town-ethnicity-counts / town-totalpop * (1 - item 3 town-ethnicity-counts / town-totalpop ))"

SLIDER
715
42
953
75
color-axis-max
color-axis-max
0.1
5
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
1493
116
1714
149
turnover
turnover
0
0.05
0.0
0.001
1
NIL
HORIZONTAL

SLIDER
315
622
462
655
beta-eth
beta-eth
0
30
8.0
1
1
NIL
HORIZONTAL

SLIDER
315
658
462
691
beta-rel
beta-rel
0
30
8.0
1
1
NIL
HORIZONTAL

SLIDER
1493
245
1714
278
neighbor-weight
neighbor-weight
0
1
0.17
0.01
1
NIL
HORIZONTAL

SWITCH
1493
276
1714
309
others-ignore-ethn
others-ignore-ethn
0
1
-1000

INPUTBOX
1493
376
1559
436
stop-tick
1000.0
1
0
Number

MONITOR
713
76
953
121
data in map
color-explain-string
17
1
11

MONITOR
315
505
384
550
#agents
town-totalpop
17
1
11

MONITOR
955
76
1087
121
Moran-I (spatial cor.)
(word precision moran-I districts 3 \" (emp \" precision moran-I staticempiricals 3 \")\")
3
1
11

TEXTBOX
1492
10
1674
29
Further Parameters
18
114.0
1

TEXTBOX
1494
42
1652
60
Used while loading town
12
0.0
1

TEXTBOX
1493
98
1663
116
Used at simulation runtime
12
0.0
1

BUTTON
953
42
1086
75
Toggle 1| max
toggle-color-axis-max\nvisualize\nupdate-plots
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
1092
10
1453
33
6. Simulation vs. Emipirical 
18
114.0
1

TEXTBOX
326
588
502
613
weights for the fraction of similars in the function of observable utility
9
0.0
1

BUTTON
1493
310
1714
343
Set baseline further params
baseline-further-parameters\n
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
1493
343
1714
376
Set baseline core params
baseline-core-parameters
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
1095
206
1245
224
Time trends\n
12
0.0
1

PLOT
1088
545
1356
692
Individual activities
time
fraction
0.0
1.0
0.0
0.25
true
true
"" ""
PENS
"searching" 1.0 0 -13791810 true "" "if ticks > 0 [plot searches-count / decisions-count]"
"moving" 1.0 0 -955883 true "" "if ticks > 0 [plot moves-count / decisions-count]"

MONITOR
1283
600
1355
645
searching
searches-count / decisions-count
3
1
11

MONITOR
1283
645
1355
690
moving
moves-count / decisions-count
3
1
11

SWITCH
1568
180
1714
213
always-move
always-move
1
1
-1000

TEXTBOX
1498
155
1566
181
skip decision step 1
9
0.0
1

TEXTBOX
1498
185
1566
208
skip decision step 2
9
0.0
1

SWITCH
1493
212
1714
245
ethn-rel-recommendations
ethn-rel-recommendations
0
1
-1000

CHOOSER
1356
550
1493
595
dissimilarity-religion
dissimilarity-religion
"all" "MUSLIM" "CHRISTIAN" "OTHER"
0

TEXTBOX
527
10
677
32
5. Monitor
18
115.0
1

@#$#@#$#@
## WHAT IS IT?

A model of **residential segregation** inspired by **Schelling's model** implemented on real-world maps in GIS format. The model can be fed with real world demographic data. 
Besides the real-world geography, Schelling's model is extended to also include the **socio-economic status** additional to **ethnicity** of residents. 

## HOW IT WORKS

This is document in the paper:

**"Exploring the dynamics of neighborhood ethnic segregation with agent-based modelling: an empirical application to Bradford"**
by Carolina V. ZUCCOTTI, Jan LORENZ, Rocco PAOLILLO, Alejandra RODRÍGUEZ SÁNCHEZ, and Selamawit SERKA (2021).

This model and, in particular, the **necessary shapefiles and demographic data** is provided here
https://github.com/janlorenz/Schelling_on_GIS

All notes below are just teasers to this full documentation. 
Some modeling details are commented in the code. 

## HOW TO USE IT

Click "Load Town to Sim" to load regions as shape-files and demographic data of a town which must be provided in ESRI format in a subfolder using NetLogo's GIS extension.

Explore the local demographic data by using the parameters provided and clicking "Update Maps and Plots".

Click "Shuffle Population" to create a counter-factual situation where every region's ethnic composition matches those of the whole town. 

Click "Go" to run the simulation of relocation decisions of residents. 

## THINGS TO NOTICE

See if and how ethnic segregation re-emerges from the artifically de-segregated situation. 

## THINGS TO TRY

On outline of successive simulation experiments are documented in the paper cited above.

## NETLOGO FEATURES

This is a short more technical outline highlighting how NetLogo's features are used.

For each LSOA an agent of the breed `district` is created located at the center of the LSOA. Further on, links are created between districts representing shapes which share a border.

Each district object stores the counts for all twelve types of individuals (four ethnicities times three socio-economic status) in a variable. 

Bradford

|Ethnicity |  high|   low|   mid|
|:---------|-----:|-----:|-----:|
|asian     |  9922| 21004| 14407|
|black     |  1292|  2183|   789|
|othereth  |  2993|  8922|  2740|
|whiteb    | 35004| 62098| 33066|

Bradford LSOA E01010597

|Ethnicity | high| mid| low|
|:---------|----:|---:|---:|
|whiteb    |  191| 217| 352|
|asian     |   28|  38|  55|
|black     |    7|  10|   7|
|othereth  |   19|   8|  44|

Further on, in each district a list of lists is created where each sublist represents one of the individuals in the district. Each sublist has three elements: The indication of the individual's ethnicity, the indication of the individual's SES, and a random number from a Beta-distribution drawn upon initialization representing the individual's theshold $\theta$. The Beta-distribution is parameterized by the two parameters `threshold-mean` $\mu_\theta$ and `threshold-sd` $\sigma_\theta$ (in the section "3. Setup Simulation") representing the mean and the standard deviation of the thresholds in the population. 

Finally, for each district we set maximal numbers of individuals for each SES group. These numbers are set such that a is a fraction of `free-space` of these maximal number are not occupied initially. Thus, we assume that in each district there are certain fractions of houses, flats, or rooms suitable and affordable for each of the three SES groups. With `free-space` = 0.05, 5% of these places are initially free. 

Compared to a typical model in NetLogo our implementation does not represent each individual as an agent but as an item in a list stored by a district (which is a turtle in this implementation). 

The selection of an individual is modeled by first selecting a district at random and second one of the individuals from the list of lists stored in that district. From the selected sublist we know the ethnicity, the SES and the threshold of the individual. LSOAs differ in size. Therefore, we adjusted for that by selecting and individual from districts with below average population only with a certain probability. In the case of an above average district, we do more than one individual decision with probabilities such that the expected number of decisions fits to the the relative size of the district.

Once an individual is selected the process is as follows: First, the individual may be forced to move with a (small) probability of `turnover` (e.g. 0.003 or switched off when 0). In this case, the individual selects another place to move. This place is randomly selected from all available places suitable for the SES of the individual in the whole town. To that end, we compute for each district the difference between the population count and the maximal population for the respective SES group. Then we select one district with probabilities proportional to that number, a procedure sometimes called roulette wheel selection. When the individual is not forced to move, the individual assesses their utility with the current residence based on the parameters `beta-eth` and `beta-ses`. This includes the computation of the fractions of population with the same ethnicity and the same SES in the neighborhood and the draw of a random number from a standard Gumbel distribution. The parameter `neighbor-weight` weights the population of all neighboring districts for the counts used to compute the fractions in the neighborhood (e.g. 0.17). When the utility obtained from the current residence is negative, the individual starts to search. In the model that means, the individual selects a potential new place with the same procedure but weights for the roulette wheel selection being adjusted by a `recommendation-probability` when `ethn-ses-recommendations` is switched on. Naturally, this place lies most likely in another district. The individual then assesses the utility for the new place in the same way as it assessesed the utility for the current residence (but with a new independent random draw from a Gumbel distribution). Finally, the individual moves to the new place when the utility from the new place is higher than the utility form the current place. Technically, then the counts in the old and new districts are updated and the sublist for the list of individuals is removed in the old district and appended to the list in the new district. In that way, the individual carries also their the individual threshold to the new district. 


## CREDITS AND REFERENCES

Programmed by Jan Lorenz with the help of Rocco Paolillo and advice from all authors. 
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
  <experiment name="experiment_ethnic-religion" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>simpson-index</metric>
    <metric>EGJ</metric>
    <metric>CHN</metric>
    <metric>EGS</metric>
    <metric>OTH</metric>
    <metric>moranI</metric>
    <enumeratedValueSet variable="town">
      <value value="&quot;Jakarta&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="free-space">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-down-pop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-source">
      <value value="&quot;simulation (dynamic)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="measure">
      <value value="&quot;ethnicity fraction&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ethnicity">
      <value value="&quot;CHINESE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="religion">
      <value value="&quot;OTHER&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-mean">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-sd">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie-houses-to-religion">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-eth">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="beta-rel" first="0" step="4" last="30"/>
    <enumeratedValueSet variable="color-axis-max">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dissimilarity-religion">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turnover">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="always-search">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="always-move">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ethn-rel-recommendations">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor-weight">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="others-ignore-ethn">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-tick">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_ethnic-religion_2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>simpson-index</metric>
    <metric>EGJ</metric>
    <metric>CHN</metric>
    <metric>EGS</metric>
    <metric>OTH</metric>
    <metric>moranI</metric>
    <enumeratedValueSet variable="town">
      <value value="&quot;Jakarta&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="free-space">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scale-down-pop">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-source">
      <value value="&quot;simulation (dynamic)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="measure">
      <value value="&quot;ethnicity fraction&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ethnicity">
      <value value="&quot;CHINESE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="religion">
      <value value="&quot;OTHER&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-mean">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-sd">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie-houses-to-religion">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-eth">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta-rel">
      <value value="4"/>
      <value value="8"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="color-axis-max">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dissimilarity-religion">
      <value value="&quot;all&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turnover">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="always-search">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="always-move">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ethn-rel-recommendations">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor-weight">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="others-ignore-ethn">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-tick">
      <value value="100"/>
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
