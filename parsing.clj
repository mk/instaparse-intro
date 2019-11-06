(ns parsing
  (:require [instaparse.core :as insta]))

(+ 1 2 3)
;; 1 + 2

(re-matches #"hello [a-z]+" "hello regex")

(def word-parser
  (insta/parser "sentence = word (<space> word)* <dot>
                 space = #'\\s'
                 dot = '.'
                 word = #'[A-Za-z]+'"))


(word-parser "hello instaparse.")
(word-parser "hello instaparse")
(word-parser "another.")
(word-parser "my final sentence.")
(word-parser "One more try. A final one.")






(def p
  (insta/parser
   "S = move (<space> move)*
    space = #'\\s'
    move = 'left' | 'right' | 'up' | 'down'"))

(p "up")
(p "down")
(p "up down right up")
(p "updownrightup")

(def whitespace
  (insta/parser "whitespace = #'\\s+'"))

(def p
  (insta/parser
   "<sentence> = token+
    <token> = move
    move = 'left' | 'right' | 'up' | 'down'"
   :auto-whitespace whitespace))

(p " ")
(p "up")
(p "down")
(p "updownrightup")
(p "up down right up")


(def p
  (insta/parser

   "S = expr+
    <expr> = move | repeat
    repeat = <'repeat'> number <'['>  expr+ <']'>
    number = #'[0-9]+'
    move = 'left' | 'right' | 'up' | 'down'"
   :auto-whitespace whitespace))


(p "up up")
(p "repeat 2 [ up up ]")
(p "repeat 1 [ repeat 2 [ repeat 3 [ down ]]]")
(p "down left repeat 2 [ left ]")


(def p+space
  (insta/parser

   "S = expr (<space> expr)*
    <expr> = move | repeat
    repeat = 'repeat' <space> number <space> <'['> <space> expr (<space> expr)* <space> <']'>
    space = #'\\s'
    number = #'[0-9]+'
    move = 'left' | 'right' | 'up' | 'down'"))

(p+space "down right repeat 2 [ up up right ] right")
(p+space "down right repeat 2 [ up repeat 5 [ up right ] right ] right")


(def move {"right" {:x 1}
           "left"  {:x -1}
           "up"    {:y -1}
           "down"  {:y 1}})


(move "up")
(move "down")


(insta/transform {:move move} (p "down down down"))


(merge-with + {:x 1 :y 0} {:x 100 :y -99})
(merge-with + (move "up") (move "up") (move "left"))

(insta/transform {:move move
                  :S (partial merge-with +)}
                 (p "up up left up"))


(insta/transform {:number clojure.edn/read-string}
                 (p "repeat 2 [ down ]"))

(clojure.edn/read-string "2")


(defn ->pos [e]
  (->> e p (insta/transform {:number clojure.edn/read-string
                             :move move
                             :repeat (fn [times & expr]
                                       (apply concat (repeat times expr)))
                             :S (comp (partial apply merge-with +)
                                      (fn [& args] (flatten args)))})))
(->pos "down down")
(->pos "left right")
(->pos "right down right")

(->pos "down repeat 2 [ down ] down")

(->pos "repeat 1 [ down up ]")
(->pos "repeat 2 [ up left ]")
(->pos "repeat 2 [ up repeat 2 [ left ]]")
(->pos "repeat 3 [ left repeat 2 [ up ] ]")
(->pos "repeat 1 [ repeat 2 [ repeat 3 [ repeat 4 [ down ]]]]")
(->pos "repeat 3 [ left repeat 2 [ left right repeat 3 [ up ] ] ]")
(->pos "repeat 3 [ left repeat 2 [ up ] ]")


(clojure.java.shell/sh "open" "http://instaparse-live.matt.is")
(clojure.java.shell/sh "open" "http://nextjournal.com/dubroy/ohm-parsing-made-easy")























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
