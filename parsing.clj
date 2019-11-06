(ns parsing
  (:require [instaparse.core :as insta]))


;; move = "left" | "right" | "up" | "down"
;; expression =
;; move
;; | "repeat" natural "["  expression* "]"
;; grammar = expression*
;;
;; also eine liste aus: einfachen move Anweisungen
;; oder repeats mit einem count und einer liste von expressions, also z.b.

(def p
  (insta/parser
   "S = token (<space> token)*
    <token> = move
    space = #'\\s'
    move = 'left' | 'right' | 'up' | 'down'"))

(p "up")
(p "down")
(p "updownrightup")
(p "up down right up")

(def whitespace
  (insta/parser "whitespace = #'\\s+'"))

(def p
  (insta/parser
   "S = token+
    <token> = move
    move = 'left' | 'right' | 'up' | 'down'"
   :auto-whitespace whitespace))

(p "up")
(p "down")
(p "updownrightup")
(p "up down right up")

(def p
  (insta/parser
   "S = expr (<space> expr)*
    <expr> = move | repeat
    repeat = 'repeat' <space> number <space> '[' <space> expr (<space> expr)* <space>']'
    space = #'\\s'
    number = #'[0-9]+'
    move = 'left' | 'right' | 'up' | 'down'"))

(p "down right repeat 2 [ up up right ] right")
(p "down right repeat 2 [ up repeat 5 [ up right ] right ] right")


(insta/transform {:number clojure.edn/read-string
                  #_#_:S identity} (p "down repeat 2 [ down ]"))

(def p
  (insta/parser
   "S = expr+
    <expr> = move | repeat
    repeat = <'repeat'> number <'['>  expr+ <']'>
    number = #'[0-9]+'
    move = 'left' | 'right' | 'up' | 'down'"
   :auto-whitespace whitespace))


(p "repeat 2 [ up ]")
(p "repeat 1 [ repeat 2 [ repeat 3 [ repeat 4 [ down ]]]]")
(p "down right repeat 2 [ up up right ] right")
(p "down right repeat 2 [ up repeat 5 [ up right ] right ] right")


(def move {"right" [1 0]
           "left"  [-1 0]
           "up"    [0 -1]
           "down"  [0 1]})




(move "up")
(move "down")


(insta/transform {:number clojure.edn/read-string
                  :move move
                  :S (partial map +)} (p "down down down"))


(map + [0 1] [1 2] [2 3])



(defn ->pos [e]
  (->> e p (insta/transform {:number clojure.edn/read-string
                             :move move
                             :repeat (fn [times & expr]
                                       (map * [times times] (apply map + expr)))
                             :expr identity
                             :S (partial map +)})))

(map * [2 2] [3 5])

(apply concat (repeat 2 [:up :up]))

(->pos "down up")
(->pos "left right")
(->pos "right")
(->pos "down")
(->pos "repeat 1 [ repeat 1 [ repeat 1 [ up ]]]")
(->pos "repeat 2 [ up left ]")
(->pos "repeat 2 [ up repeat 2 [ up up ]]")
(->pos "repeat 3 [ left repeat 2 [ up ] ]")
(->pos "repeat 1 [ repeat 2 [ repeat 3 [ repeat 4 [ down ]]]]")
(->pos "repeat 3 [ left repeat 2 [ left right repeat 3 [ up ] ] ]")
(->pos "repeat 3 [ left repeat 2 [ up ] ]")

(insta/parses
 (insta/parser
  "S = expr+
    <expr> = repeat | move
    number = #'[0-9]+'
    move = 'left' | 'right' | 'up' | 'down'
    repeat = <'repeat'> number <'['>  expr+ <']'>
"
  :auto-whitespace whitespace)
 "repeat 2 [ up repeat 3 [ left up up]]")


(def arithmetic
  (insta/parser
   "expr = add-sub
     <add-sub> = mul-div | add | sub
     add = add-sub <'+'> mul-div
     sub = add-sub <'-'> mul-div
     <mul-div> = term | mul | div
     mul = mul-div <'*'> term
     div = mul-div <'/'> term
     <term> = number | <'('> add-sub <')'>
     number = #'[0-9]+'"))

(arithmetic "1+2+3")
(arithmetic "1-2/(3-4)+5*6")


(->> (arithmetic "1-2/(3-4)+5*6")
     (insta/transform
      {:add +, :sub -, :mul *, :div /,
       :number clojure.edn/read-string :expr identity}))
