extensions [matrix]
globals [m]
breed [robots robot]
breed [points point]
robots-own [assigned leader pointx pointy ciblex cibley id_agent is_placed]
points-own [assigned placed is-vertex id_agent]

; m matrice utilisée pour calculé l'algorithme hongrois
; leader        : booleen permettant de savoir qui est le leader
; pointx pointy : l'un des points de la shape qui lui a été affecté
; ciblex cibley : la position où il devra aller
; assigned      : indique s'il a déja une cible ou pas

; assigne le point d'id point_id au robot appelant
to assign-point [point_id]
  let p (point point_id);one-of (points with[id_agent = point_id]))
         set pointx ([xcor] of p)
         set pointY ([ycor] of p)
         set ciblex ([xcor] of p)
         set cibley ([ycor] of p)
         set assigned true          ; ce robot est affecte
         ask p [set assigned true]  ; ce point est affecte
end

; Crée une forme selon le nb de sommet
to-report shape-to-nb-vertex
  if forms-choice = "line" [report 2]
  if forms-choice = "triangle" [report 3]
  if forms-choice = "square" [report 4]
  if forms-choice = "5-vertex" [report 5]
end

to set-point
  let nb-vertex shape-to-nb-vertex
  let nb-points nb-agents
; creation de la forme (shape)
  let radius form-size
  let nb-points-per-edge ((nb-points - nb-vertex) / nb-vertex)
  ;; Création des points
  create-points nb-vertex [
    set shape "circle"
    set assigned false
    set color white
    set id_agent who
    set placed false
    set is-vertex true
  ]

  ;; Positionements des sommets
  let vertex-list []

  let angles (map [[x] -> x * 360 / nb-vertex] (range nb-vertex))
  foreach angles [[angle] -> ask one-of points with [placed = false and is-vertex] [
    set placed true

    ;;création des couples de sommets
    ;let liste []
    ;ask points with [is-vertex and placed = false] [set liste (lput self liste)]

    ;foreach liste [[vertex] -> set vertex-list (lput (list self vertex) vertex-list)]
    setxy (radius * (cos angle)) (radius * (sin angle))
  ]]

  ;; Création de la liste des couples de sommets
  let liste []
  ask points with [is-vertex] [set liste (lput self liste) set placed false]

  ;show liste
  foreach liste [[vertex] ->
    let id ([who] of vertex)
    ask vertex [set placed true]
    ask points [show distance vertex]
    ask (min-n-of 2 (points with [id != who]) [distance vertex]) with [placed = false] [set vertex-list (lput (list self vertex) vertex-list)]
  ]
  ;show vertex-list

  ;; Positionement des points sur les cotés

    create-points (nb-points - nb-vertex) [
    set shape "circle"
    set assigned false
    set color white
    set id_agent who
    set placed false
    set is-vertex true
  ]

  foreach (but-first vertex-list) [[vertex-tuple] ->
    let a (first vertex-tuple)
    let b (last vertex-tuple)

    let x-vector ([xcor] of b - [xcor] of a)
    let y-vector ([ycor] of b - [ycor] of a)

    ;; création de la liste des coordonées des points à placer entre ces deux sommets
    let coords (map [[i] -> list ([xcor] of a + (i * x-vector / (nb-points-per-edge))) ([ycor] of a + (i * y-vector / (nb-points-per-edge )))] (range 1 (nb-points-per-edge)))

    foreach coords [[coord-tuple] ->
      if any? (points with [placed = false]) [

        ask one-of points with [placed = false] [
          set placed true
          let x (first coord-tuple)
          let y (last coord-tuple)
          setxy x y
    ]]]
  ]

  ;; On place les points sur le dernier coté
  ;print ("toto", nb-points-per-edge)
  if (((nb-points - nb-vertex) mod nb-vertex) != 0) [set nb-points-per-edge (nb-points-per-edge + (((nb-points - nb-vertex) mod nb-vertex)))]
  ;print "toto" nb-points-per-edge

  let vertex-tuple (first vertex-list)
  let a (first vertex-tuple)
  let b (last vertex-tuple)

  let x-vector ([xcor] of b - [xcor] of a)
  let y-vector ([ycor] of b - [ycor] of a)

  ;; création de la liste des coordonées des points à placer entre ces deux sommets
  let coords (map [[i] -> list ([xcor] of a + (i * x-vector / (nb-points-per-edge))) ([ycor] of a + (i * y-vector / (nb-points-per-edge)))] (range 1 (nb-points-per-edge)))

  foreach coords [[coord-tuple] ->
    if any? (points with [placed = false]) [

      ask one-of points with [placed = false] [
        set placed true
        let x (first coord-tuple)
        let y (last coord-tuple)
        setxy x y
  ]]]



end

to m-set [i j x]
  matrix:set m i j x
end
to-report m-get [i j]
  report matrix:get m i j
end
to-report m-get-row [i]
  report matrix:get-row m i
end
to-report m-get-col [i]
  report matrix:get-column m i
end

; initialise la matrice avec les distance entre chaque robots et chaque positions
to init-m
  ;set m (matrix:make-constant (nb-agents - 1) (nb-agents - 1) 0)
  set m (matrix:make-constant (nb-agents) (nb-agents) 0)
  ;let lead (one-of robots with [leader = true])
  ask points with [assigned = false][
    ;let realx ([xcor] of lead  - [pointx] of lead + [xcor] of self)
    ;let realy ([ycor] of lead  - [pointy] of lead + [ycor] of self)
    let realx ([xcor] of self)
    let realy ([ycor] of self)
    ask robots with [leader = false] [
      m-set ([id_agent] of myself) id_agent (distancexy realx realy)
    ]
  ]
end

; renvoi le nombre d'élément valant x dans la liste l
to-report count-list [x l]
  let nb-x 0
  foreach l [ v -> if v = x [set nb-x (nb-x + 1)]]
  report nb-x
end

to-report hungarian_method

  ; zero baré = -2,  zero encadré = -1

  ;;;;;;;;;;; PHASE 0 ;;;;;;;;;;;;;
  ; On soustrait à chaque ligne du tableau initial, le plus petit élément de la ligne
  let minus-matrix (matrix:make-constant (nb-agents) (nb-agents) 0)
  foreach (range (nb-agents - 1)) [ i ->
    let min-of-row (min (m-get-row i))
    foreach (range (nb-agents - 1)) [ j ->
      matrix:set minus-matrix i j (min-of-row)
    ]
  ]
  set m (m matrix:- minus-matrix)
  ; On soustrait à chaque colone du tableau initial, le plus petit élément de la colone
  foreach (range (nb-agents)) [ i ->
    let min-of-col (min (m-get-col i))
    foreach (range (nb-agents)) [ j ->
      matrix:set minus-matrix j i (min-of-col)
    ]
  ]
  set m (m matrix:- minus-matrix)
  ;print matrix:pretty-print-text m

  while [true] [
    ;;;;;;;;;; PHASE 1 ;;;;;;;;;;;;
    ;print "!!!!!!!!!!!!!!!!!!!!!!!!! PHASE 1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    let continue true
    while [continue] [
      ;On  cherche  la  ligne  comportant  le  moins  de  zéros  non  barrés
      let nb-zero-per-row (map [l ->  ifelse-value ((count-list 0 l) = 0) [100000] [count-list 0 l] ] (matrix:to-row-list m))
      ;print nb-zero-per-row
      let line-with-less-zero (position (min nb-zero-per-row) nb-zero-per-row)
      ifelse min nb-zero-per-row = 100000
      [set continue false]
      [
        ;On encadre un des zéros de cette ligne
        let zero-y (position 0 ((m-get-row line-with-less-zero)))
        m-set line-with-less-zero zero-y -1
        ;On  barre  tous  les  zéros  se  trouvant  sur  la  même  ligne  ou  sur  la  même  colonne  que  le  zéro encadré
        foreach (range (nb-agents)) [ i ->
          if m-get line-with-less-zero i = 0 [m-set line-with-less-zero i -2]
          if m-get i zero-y = 0 [m-set i zero-y -2]
        ]
      ]
      ;print matrix:pretty-print-text m
    ]

    ; Si l'on a encadré un zéro par ligne et par colonne -> terminé
    let nb-zero 0
    foreach (matrix:to-row-list m) [ l -> set nb-zero (nb-zero + (count-list -1 l))]
    ifelse (nb-zero = (nb-agents))
    [
      ; on renvoi une liste des positions tel que le robot i est affecté à la position l[i]
      report (map [robot-id -> position -1 (m-get-col robot-id)] (range (nb-agents)))
    ]
    [
      ;;;;;;;;;;;; PHASE 2 ;;;;;;;;;;;;;
      ;print "!!!!!!!!!!!!!!!!!!!!!!!!! PHASE 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      ;On marque toutes les lignes ne contenant aucun zéro encadré
      let marked-row []
      foreach (range (nb-agents)) [ row-id -> if (not (member? -1 (m-get-row row-id))) [set marked-row (fput row-id marked-row)]]

      let marked marked-row != []
      let marked-col []

      while [marked] [
        set marked false
       ; On marque toute colonne ayant un zéro barré sur une ligne marquée
        foreach (range (nb-agents)) [ col-id ->
          foreach marked-row [ row-id ->
            if m-get row-id col-id = -2 and (not (member? col-id marked-col)) [
              set marked-col (fput col-id marked-col)
              set marked true
            ]
          ]
        ]
        ;print marked-col
        ;On marque toute ligne ayant un zéro encadré dans une colonne marquée
        foreach (range (nb-agents)) [ row-id ->
          foreach marked-col [ col-id ->
            if m-get row-id col-id = -1 and (not (member? row-id marked-row))[
              set marked-row (fput row-id marked-row)
              set marked true
            ]
          ]
        ]
      ]
      ;On trace alors un trait sur toute ligne non marquée et sur toute colonne marquée
      let dashed-col marked-col
      let undashed-col []
      foreach (range (nb-agents)) [ i -> if not member? i marked-col [set undashed-col (fput i undashed-col)]]
      let dashed-row []
      foreach (range (nb-agents)) [ i -> if not member? i marked-row [set dashed-row (fput i dashed-row)]]
      let undashed-row marked-row
      ;print matrix:pretty-print-text m
      ;print dashed-col
      ;print dashed-row
      ;print "!!!!!!!!!!!!!!!!!!!!!!!!! PHASE 3 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      ;;;;;;;; PHASE 3 ;;;;;;
      ; Les cases non traversées par un trait constituent un tableau partiel
      ; On retranche à toutes les cases de ce tableau partiel le plus petit élément de celui-ci

      ; on commence par démarqué toute les zero
       foreach (range (nb-agents)) [ row ->
        foreach (range (nb-agents)) [ col ->
          if (m-get row col) < 0 [m-set row col 0]
        ]
      ]

      let min-elem 1000000
      foreach undashed-col [ col ->
        foreach undashed-row [row ->
          let elem (m-get row col)
          if elem >= 0 and elem < min-elem [set min-elem elem]
        ]
      ]
      ;print min-elem
      foreach undashed-col [col ->
        foreach undashed-row [row ->
          let elem (m-get row col)
          m-set row col (elem - min-elem)
        ]
      ]
      ;On ajoute ce même élément à toutes les cases du tableau initial barrées deux fois
      foreach dashed-col [col ->
        foreach dashed-row [row ->
          let elem (m-get row col)
          m-set row col (elem + min-elem)
        ]
      ]
      ;print matrix:pretty-print-text m
    ]
  ]
end


to setup
  ca
  reset-ticks
  ; creation de la forme (shape)
  set-point
  ; creation des robots
  create-robots nb-agents [set shape "person" set size 2 set color red setxy random-pxcor random-pycor set leader false set assigned false set id_agent (who - nb-agents) set is_placed false]
  ifelse method = "hungarian" [setup-hungarian-method]
  [if method = "blackboard" [setup-blackboard-basic]]

end


; Fonction de setup de la V2
to setup-blackboard-basic
  ; Création des points
  ;create-points nb-agents [set shape "circle"  set assigned false set color green setxy (who * 5) (who * 5)]
end


to setup-hungarian-method
  init-m
  print matrix:pretty-print-text m
  ; affectation (assignment) des points aux robots
  let point-ids hungarian_method

  ask robots with [assigned = false] [
    assign-point (item id_agent point-ids)
  ]
end

; Fonction de décision des agents
; qui s'assigne un point aléatoirement
to brain-blackboard-basic-dump
  ; S'il n'a pas d'assignation, il regarde dans le blackboard un point non assigné
  ; et se l'assigne
  if (([assigned] of self) = false) [assign-point ([who] of one-of points with [assigned = false])]

  ; Si il n'est pas arrivé à destination, il avance
  facexy ciblex cibley
  ifelse ((distancexy ciblex cibley) > 0.5) [fd 1][set is_placed true]
end

; Fonction de décision des agents
; qui s'assigne le point le plus proche
; Si il est déjà pris, il cherche le second le plus proche, etc..
to brain-blackboard-basic-near
  ; S'il n'a pas d'assignation, il regarde dans le blackboard le point non assigné le plus proche
  if (([assigned] of self) = false) [assign-point ([who] of min-one-of points with [assigned = false] [distance self])]

  ; Si il n'est pas arrivé à destination, il avance
  facexy ciblex cibley
  ifelse ((distancexy ciblex cibley) > 0.5) [fd 1][set is_placed true]
end

; Fonction décision des agents
; qui s'assigne le point le plus proche, et qui une fois arrivé au point :
; si quelqu'un est présent sur la position, il se bataille avec leur id pour savoir qui reste
; sinon se met sur le point
to brain-blackboard-basic-stronger
  ; S'il n'a pas d'assignation, il regarde dans le blackboard le point le plus proche
  if (([assigned] of self) = false) [assign-point ([who] of min-one-of points [distance self])]

  ; S'il n'est pas arrivé à destination, il avance
  facexy ciblex cibley
  ifelse ((distancexy ciblex cibley) > 0.5)
  [fd 1]
  [
    ; S'il est arrivé et qu'il y a un autre agent
    ifelse (count(robots with [distance myself < 1]) > 1)
    [
      let n (one-of robots with [distance myself < 1])
      if (([who] of self < [who] of n) and ([ciblex] of n = [ciblex] of self))
      [
        assign-point ([who] of min-one-of points with [assigned = false] [distance self])
      ]
    ]
    [set is_placed true]
  ]
end

to go
  ifelse method = "hungarian" [go-hungarian-method]
  [if method = "blackboard" [go-blackboard-basic]]
  if (all? robots [is_placed]) [stop]
  tick
end

to go-hungarian-method
  ask robots [facexy ciblex cibley]
  ask robots [ ifelse ((distancexy ciblex cibley) > 0.5) [fd 1][set is_placed true] ]
end

; Fonction boucle pour les actions de chaque agents
to go-blackboard-basic
  if (agent-behaviour = "dump") [ask robots [brain-blackboard-basic-dump]]
  if (agent-behaviour = "near") [ask robots [brain-blackboard-basic-near]]
  if (agent-behaviour = "stronger") [ask robots [brain-blackboard-basic-stronger]]

end
@#$#@#$#@
GRAPHICS-WINDOW
546
18
1157
630
-1
-1
3.0
1
10
1
1
1
0
0
0
1
-100
100
-100
100
0
0
1
ticks
30.0

PLOT
329
320
529
470
plot 1
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
"default" 1.0 0 -16777216 true "" "plot sum [distancexy ciblex cibley] of robots"

MONITOR
32
411
147
456
sum distances
sum [distancexy ciblex cibley] of robots
17
1
11

CHOOSER
345
124
483
169
forms-choice
forms-choice
"line" "triangle" "square" "5-vertex"
2

SLIDER
345
22
517
55
nb-agents
nb-agents
0
100
42.0
1
1
NIL
HORIZONTAL

CHOOSER
345
238
488
283
agent-behaviour
agent-behaviour
"dump" "near" "stronger"
2

CHOOSER
346
179
484
224
method
method
"hungarian" "blackboard"
1

BUTTON
21
29
94
62
setup
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
21
66
84
99
go
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
344
64
516
97
form-size
form-size
0
100
32.0
1
1
NIL
HORIZONTAL

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
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
