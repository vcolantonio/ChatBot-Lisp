(load "dataset.lisp")

; -------------------------------------------------------------------------------------------------
;           FUNZIONI DI UTILITA'
; -------------------------------------------------------------------------------------------------


(defun random-elt (list)
  (nth (random (length list)) list))

(defun getSetByAttribute (attribute)
    (cond ((equal attribute '{Title}) Title_set)
          ((equal attribute '{Year})  Year_set)
          ((equal attribute '{Genre}) Genre_set)
          ((equal attribute '{Place}) Place_set)
          ((equal attribute '{Date}) Date_set)
          ((equal attribute '{Information}) Information_set)
    )
)

(defun attrToCol (attribute)
    (cond ((equal attribute '{Title}) 'Title)
          ((equal attribute '{Year})  'Year)
          ((equal attribute '{Genre}) 'Genre)
          ((equal attribute '{Place}) 'Place)
          ((equal attribute '{Date}) 'Date)
          ((equal attribute '{Key}) 'Key)
          ((equal attribute '{Movements}) 'Movements)
          ((equal attribute '{Famous}) 'Famous)
          ((equal attribute '{Duration}) 'Duration)
    )
)

(defun getValue (tuple attribute)
    (cond ((equal attribute 'famous)
                (cond ((equal (cadr (assoc attribute tuple :test 'equal)) '(yes) ) '(Yes it is))
                        ((equal (cadr (assoc attribute tuple :test 'equal)) '(no) ) '(No it isn't)))                        )
           (T (cadr (assoc attribute tuple :test 'equal)))
    )
)

(defun findAllByValue (dataset attribute value)
    (cond ((equal dataset ()) nil)
        ((equal (getValue (car dataset) attribute) value) (cons (car dataset) (findAllByValue (cdr dataset) attribute value)))
          (T (findAllByValue (cdr dataset) attribute value)))
)

(defun isAttribute (a)
    (or (equal a '{Title})
        (equal a '{Genre})
        (equal a '{Year})
        (equal a '{Place})
        (equal a '{Date})
        (equal a '{Key})
        (equal a '{Movements})
        (equal a '{Duration})
        (equal a '{Famous})
    )
)

(defun missingValues (a)
    (cond ((listp a)
            (cond ((equal a ()) nil) (T (or (missingValues (car a)) (missingValues (cdr a))))))
          ((atom a)(cond ((equal a 'MISSING_VALUE) T)(T nil)))
          (T T)
    )
)

; -------------------------------------------------------------------------------------------------
;                                       GESTIONE REGOLE
; -------------------------------------------------------------------------------------------------


; -------------------------------------------------------------------------------------------------
;       Definizione delle regole
; -------------------------------------------------------------------------------------------------


(defvar _rules_ ())
(defvar _general_rules_ ())
(defvar _rule2tuple_ ())
(defvar _rule2head_ ())
(defvar _rule2tuples_ ())
(defparameter _rule_memory_ ()) ;regole per la gestione della memoria


(defvar _rules_template_ '(
    ((* x key * y {TITLE} * z) (x key ? it's {KEY}))
    ((* x like {Title} * y) 
        ( x like {Title} y ? I do since it's {Genre} ) ( x like {Title} y ? I don't since it's {Genre} ) (Oh we are really similar! I'm a big fan of all {Genre} by Tchaikovsky) (I do not agree with you because I think {Genre} works of my master are the most boring to listen) (Mmm I don't know how to answer you because I only know the title of the work but I didn't have the chance to listen to it))
    ((* x listened to {Title} * y) 
        (x listened to {Title} ? Were you alive in {Year} ?) (Oh wow\, so you are really older than I thought! In {Year} I wasn't even in the mind of my creators))
    (( * x {Title} * y {Genre})( Yes it is) 
        (yes\, if memory serves me correctly\, it should be part of the set of works that fall within the genre {Genre}))
    (( * x {Title} * y Tchaikovsky * y) 
        (Yes it is)(After having scanned all the twists and turns in my memory\, I came to the conclusion that\: yes\, it is))
    (( * x {Title} * y Tchaikovsky's * y) 
        (Yes it is)(After having scanned all the twists and turns in my memory\, I came to the conclusion that\: yes\, it is))
    ((* x listened to {Title} * y) 
        ( x listened to {Title} ? Were you alive in {Year} ?)); => no => me neither
    ((* x about {TITLE} * y) 
        ( x about {TITLE} ? It's a {GENRE} \, which was written in {YEAR} \. The key is {KEY} and The whole thing lasts {DURATION} minutes!)
        ( x about {TITLE} ? It's a {GENRE} \, which was written in {YEAR} \. The key is {KEY} and if you want to listen to it you'll need {DURATION} minutes of your time))
    ((* x about {TITLE} * y) 
        ( x about {TITLE} ? It's a {GENRE} \, which was written in {YEAR} \. It's composed by {Movements} movements. If you want to listen to it you'll need {DURATION} minutes of your time))
    ((* x about {TITLE} * y) 
        ( x about {TITLE} ? It's a {GENRE} \, which was written in {YEAR} \. It's composed by {Movements} movements. ))
    ((* x about {TITLE} * y) 
        ( x about {TITLE} ? It's a {GENRE} \, which was written in {YEAR} \. The key is {KEY} and the whole thing lasts {DURATION} minutes!)
        ( x about {TITLE} ? It's a {GENRE} \, which was written in {YEAR} \. The key is {KEY} and if you want to listen to it  you'll need {DURATION} minutes of your time))
    ((* x about {TITLE} * y) 
        ( x about {TITLE} ? It's a {GENRE} \, which was written in {YEAR} \. The whole thing lasts {DURATION} minutes!)
        ( x about {TITLE} ? It's a {GENRE} \, which was written in {YEAR} \. If you want to listen to it you'll need {DURATION} minutes of your time))
    ((* x about {TITLE} * y) 
        ( x about {TITLE} ? It was written in {YEAR} \. The whole thing lasts {DURATION} minutes!)
        ( x about {TITLE} ? It was written in {YEAR} \. If you want to listen to it you'll need {DURATION} minutes of your time))
    ((* x about {TITLE} * y) 
        ( x about {TITLE} ? It was written in {YEAR} \. It's a {GENRE}))
    ((* x about {TITLE} * y) 
        ( x about {TITLE} ? It's a {GENRE}))
    ((* x how long does * y  {TITLE} last * z) 
        (It lasts for just about {DURATION} minutes. ))
    ((* x how long does * y  {TITLE} last * z) 
        (I don't really know\, I never managed to listen to all of it. ))
    ((How many movements * X {Title}) 
        ({Title} consists of {Movements} movements))
    
    ((* x genre * y {TITLE} * z)(if i recall correctly\, it's a {GENRE}))
    ((* x when * y {TITLE} * z)(if i recall correctly\, it was z in {YEAR}))

    ((How many movements * X {Title}) (I have no information about it))
    ((* x {Title} * y {Year} * z) (yes\, it was written in {Year}))
    (( * x {Title} a * x)( No it's not))
    ((* z where * x {Title} * y) ({Title} was y in {Place} \, on the {Date}))
    ((* x {Title} * y by Tchaikovsky * z )(Yes it was.))
    (( * x {Title} * y a Tchaikovsky's work) (Yes it is))
    ((is {TITLE} * x well known ) ({Famous}) )
    ((is {TITLE} * x well known ) (It is well known\, like almost all of them\, of course))
    ((is {TITLE} * x famous ) ({Famous}))
    ((is {TITLE} * x famous ) (I think it is considered famous\, it's written by Tchaikovsky after all))
    
))


(defvar _general_rules_template_ '(
    ((* x favourite {GENRE} * y) (x favourite {GENRE} ? it's {PICK_RANDOM} \, obviously!))
    ((* x how many * y {Genre} * z) (There are {COUNT} works of that type that I know of. My favourite is {MOST_FAMOUS}) (I think there are {COUNT} works of that genre. ))
    ((* x how many * y {Year} * z) (There are {COUNT} works of that type that I know of. My favourite is {MOST_FAMOUS}))
    ((* x how many * y) (There are {COUNT} works that I know of. My favourite is {MOST_FAMOUS}))

))


(defvar _trivia_rules_template_ '(
    ((* x famous * y))
))


(defvar _rules_template_memory_ '(
    ((Title)  (The title is {Title} ))
    ((Genre) (It is a {Genre} ))
    ((Date) (It was composed in {Date}))
    ((Place) (It was presented in {Place})) 
    ((key) (It's written in {key} ))
    ((famous) ({Famous}))
    ((famous) (I think it is considered famous\, it's written by Tchaikovsky after all))
    ((Year) (It was written in {Year})    )
    ((Movements) (it's composed by {Movements} movements  )) 
    ((Duration) (it lasts about {DURATION} minutes))

))

; -------------------------------------------------------------------------------------------------
;           Metodo per la generazione delle regole (su una sola tupla)
; -------------------------------------------------------------------------------------------------

;   generate_rules:
;           input :  ds             =   a dataset 
;                    rules_template =   a set of template rules
;           output:  (not important - modifica _rules_)
;       
;           es: 
;           input   ds             = (((TITLE (SWAN LAKE))))
;                   rules_template = (((* X LIKE {TITLE} * Y) (X LIKE {TITLE} Y ? I DO TOO) (X LIKE {TITLE} Y ? I DON 'T)))
;                   
;           in _rules_               (((* X LIKE SWAN LAKE * Y) (X LIKE SWAN LAKE Y ? I DO TOO) (X LIKE SWAN LAKE Y ? I DON 'T)))

(defun generate_rules (ds rules_template)
    (dolist (curr_tuple ds)
        (dolist (rule_template rules_template)
            (progn  (setq new_rule (generate_rule rule_template curr_tuple))
                    (cond ((missingValues new_rule) nil)
                          (T (progn (defparameter _rules_ (append _rules_ (list new_rule)))
                                    (setq _rule2tuple_ (append _rule2tuple_ (list (list (car new_rule) curr_tuple))))
                             )
                          )
                    )
            )
        )
    )
)

(defun concatenate-l (l)
    (setf res ())
    (dolist (el l)
        (setf res (append res  el))
        (setf res (append res  (list '\,)))
    )
    (reverse (cdr (reverse res)))
)

(defun generate_rule (rule_template curr_tuple)
    (cond ((equal rule_template ()) ())
          (T (cons (instantiate_rule (car rule_template) curr_tuple) (generate_rule (cdr rule_template) curr_tuple))))
)

(defun instantiate_rule (rule curr_tuple)
    (cond   ((equal rule ()) ())
            ((isAttribute (car rule))
                (cond ((equal (getValue curr_tuple (attrToCol (car rule))) nil) '(MISSING_VALUE))
                      (T (append (getValue curr_tuple (attrToCol (car rule))) (instantiate_rule (cdr rule) curr_tuple)))))
            (T (cons (car rule) (instantiate_rule (cdr rule) curr_tuple)))
    )
)

; -------------------------------------------------------------------------------------------------
;           Metodi per la gestione delle regole generali (su più tuple)
; -------------------------------------------------------------------------------------------------


(defun isAggregateFunction (a)
    (or (equal a '{COUNT}) (equal a '{MOST_FAMOUS}) (equal a '{PICK_RANDOM}))    
)


(defun computeAggregateFunction (a tuples)
    (cond ((equal a '{COUNT}) (list (length tuples)))
          ((equal a '{MOST_FAMOUS})  (getvalue (car tuples) 'Title)) ; per ora car, ma da sistemare
          ((equal a '{PICK_RANDOM}) (getvalue (random-elt tuples) 'title))
          (T nil))    
)

(defun generate_general_rules (ds general_rules_template)
    (setf _general_rules_ ())
    (dolist ( rule_template general_rules_template); solo le teste in questo do list
        (dolist (curr_tuple ds)
            (progn  (setq new_rule (generate_rule (list (car rule_template)) curr_tuple))
                    (cond ((missingValues new_rule) nil)
                          (T (progn 
                                    (cond 
                                        ((not (member  new_rule _general_rules_ :test 'equal))
                                            (progn 
                                                (setf _rule2head_ (append _rule2head_ (list (list (car new_rule) (cdr rule_template)))))
                                                (setf _rule2tuples_ (append _rule2tuples_ (list (list (car new_rule) curr_tuple))))
                                                (defparameter _general_rules_ (append _general_rules_ (list new_rule)))
                                        ))
                                        (T  (progn
                                                (setf prev_tuples (cdr (assoc  (car new_rule) _rule2tuples_ :test 'equal)))                                        
                                                (setf  (cdr (assoc  (car new_rule) _rule2tuples_ :test 'equal)) (append prev_tuples (list  curr_tuple)))
                                            )
                                        )
                                    )
                             )
                          )
                    )
            )
        )
    )
    (dolist ( rule2head _rule2head_)
        (dolist (rule_tail (cadr rule2head))
            (progn
                (setq built_rule_tail ())
                (dolist (token rule_tail)
                    (cond ((isAggregateFunction token)
                            
                         (setf built_rule_tail (append built_rule_tail (computeAggregateFunction token (cdr (assoc (car rule2head) _rule2tuples_ :test 'equal))))))
                         (T (cond ((isAttribute token) (setf built_rule_tail (append built_rule_tail (getvalue (cadr (assoc (car rule2head) _rule2tuples_ :test 'equal)) (attrToCol token)))))
                          (T (setf built_rule_tail (append built_rule_tail (list token))))))
                    )
                )
                (setf prev_tails (cdr (assoc (car rule2head) _general_rules_ :test 'equal)))
                (setf (cdr (assoc (car rule2head) _general_rules_ :test 'equal)) (append prev_tails (list built_rule_tail)))
            )
        )
    )
    
)


(defun generate_info_rules (ds)
    (progn
        (setf tuples_to_check ())
        (dolist (curr_tuple ds)
            (cond ((equal (cadr (assoc 'famous curr_tuple )) '(Yes))
                        (setf tuples_to_check (append tuples_to_check (list curr_tuple))))
                   (T nil)
            )
        )
        (setf rule_tail ())
        (cond ((> (length tuples_to_check) 1) (setf rule_tail '(the most famous works are )))
              (T (setf rule_tail '(the most famous work is )))
        )
        (dolist (curr_tuple tuples_to_check)
            ( progn
                (setf rule_tail (append rule_tail (getValue curr_tuple 'Title)))
                (if (not (equal (getValue (car (findAllByValue dataset 'Title (getValue curr_tuple 'Title))) 'Genre) nil))
                    (progn
                        (setf rule_tail (append rule_tail '( \, which is)))
                        (setf rule_tail (append rule_tail (getValue (car (findAllByValue dataset 'Title (getValue curr_tuple 'Title))) 'Genre)))
                        (setf rule_tail (append rule_tail '( \, )))
                    )
                )
            )
        )
        rule_tail
    )
)

; -------------------------------------------------------------------------------------------------
;       Metodi per la gestione delle regole con memoria
; -------------------------------------------------------------------------------------------------


(defun generate_rules_memory (curr_tuple rules_template)
        (setf _rule_memory_ ())
        (dolist (rule_template rules_template)
            (progn  (setq new_rule (generate_rule rule_template curr_tuple))
                    (cond ((missingValues new_rule) nil)
                          (T (defparameter _rule_memory_ (append _rule_memory_ (list new_rule))) 
                          )
                    )
            )
        )
)


(defparameter *prevTuple* nil)

;lista parole chiave nella domanda con memoria
(defparameter keyList (list '(year nil) '(information nil) '(like nil)
 '(instrument nil) '(genre nil) '(movements nil) '(famous nil) '(Duration nil)))


(defun azzeraKey ()
  ;Azzera il vettore delle paroli chiavi
  (dolist (k keyList) (setf (cadr k) nil))
)


;pre: input è una lista di atomi
(defun extractKey (in keylist)
(progn

  ;data la stringa in input
  (dolist (k keylist)
    (if (containsKey in (car k))
      (setf (cadr k) T))
    ))
)


(defun containsKey (in key)
;return true if in cointains the word "key"
    (member key in)
)

;funzione chiamata per ogni parola chiave trovata nella stringa di input
(defun getInfo (tupla key)

;data una chiave 'key' restituisce una stringa con l'informazione delle 'chiave'
;presa dalla tupla salvata
;nota: la tupla si riferisce alla domanda precedente
    (block keyValue
        (dolist (rule _rule_memory_)
            (when (equal (caar rule) key) 
                (return-from keyValue (cadr rule) )))
    )
    
)



; -------------------------------------------------------------------------------------------------
;        MATCHING
; -------------------------------------------------------------------------------------------------


(defparameter _bindings_ ())

(defun match (pattern input)
    (progn  
        (cond   
            ((equal pattern ()) (equal input ()))
            ((equal (car pattern) '*) 
                    (let*  (
                        (new_input (wildcard (cddr pattern)  input (list (cadr pattern))))
                    )    
                        (match (cddr pattern) new_input)))
                
            ((equal (car pattern) (car input)) (match (cdr pattern) (cdr input)))
            (T nil)
    ))
)

(defparameter _viewpoint_ '((I you) (you I) (me you) (am are) (my your)))


(defun wildcard (pattern input curr_binding)
    (cond   ((equal input ())
                (cond ((OR (equal pattern ()) (equal (car pattern) '*)) (progn (setf _bindings_ (append _bindings_ (list curr_binding))) ()))
                      (T (progn (setf _bindings_ ()) ()))))
                
            ((equal (car input) (car pattern)) 
                (cond ((equal (cadr input) (car pattern)) (wildcard pattern (cdr input) (append curr_binding (list (car input)))))
                      (T (progn (setf _bindings_ (append _bindings_ (list curr_binding)))) input)
                )
            )
            ((setq viewpoint (assoc (car input) _viewpoint_)) (wildcard pattern (cdr input) (append curr_binding (list (cadr viewpoint)))))

            (T (wildcard pattern (cdr input) (append curr_binding (list (car input)))))
    )
)

(defun subs (rule_tail)
    (cond   ((equal rule_tail ()) (progn (setf _bindings_ ()) ()))
            ((equal (car (assoc (car rule_tail) _bindings_)) (car rule_tail)) 
                (append (cdr (assoc (car rule_tail) _bindings_)) (subs (cdr rule_tail))))
            (T (cons (car rule_tail) (subs (cdr rule_tail))))
    )
)



; -------------------------------------------------------------------------------------------------
;        GENERATORE REGOLE
; -------------------------------------------------------------------------------------------------


(generate_rules dataset _rules_template_)

(generate_general_rules dataset _general_rules_template_)

(defparameter other_rules ())
(setf other_rules (append other_rules (list (list '(* x most famous * y) (generate_info_rules dataset)))))
(setf other_rules (append other_rules (list (list '(* x major * y) (generate_info_rules dataset)))))
(setf other_rules (append other_rules (list (list '(* x categories * y)  (append '(the genres i know are) (concatenate-l Genre_set))))))
(setf other_rules (append other_rules (list (list '(* x types * y)  (append '(the genres i know are) (concatenate-l Genre_set))))))
(setf other_rules (append other_rules (list (list '(* x genres * y) (append '(the genres i know are) (concatenate-l Genre_set))))))
(setf other_rules (append other_rules (list (list '(* x works * y) (append '(the works i know are) (concatenate-l Title_set))))))
(setf other_rules (append other_rules (list (list '(* x compositions * y) (append '(the works i know are) (concatenate-l Title_set))))))


(setf other_rules (append other_rules '(
    ((* x  period * z) (He was a Russian composer in the Romantic period))
    ((* y last work * y) (it's Pathetique\, also known as Symphony no\. 6\.))
    (( * x * y a Tchaikovsky's work) (No it is not))
    ((* x by Tchaikovsky * z )(No it wasn't.))
    ((yes)(are you sure?)(ok\, if you say so))
    ((no)(me neither))
    ((nice) (am i nice ?))
    ((why)(42)(everything has an explanation\, i think))
    ((* x hello * y) (hello.  what do you wish to know about Tchaikovsky?))
    ((* x i hate * z Tchaikovsky * y) (Why? have you listened to Pathetique ?))
    ((who are you)(i am pyotr\.\.\. i cannot tell you anymore than that))
)))

; -------------------------------------------------------------------------------------------------
;       ATOMIZZATORE
; -------------------------------------------------------------------------------------------------

;   atomizzatore:
;           input :  stringa             =   input string 
;                    lb                  =   stringa left pointer  (initially always 0)
;                    sp                  =   stringa right pointer (initially always 0)
;           output:  (SPLITTED STRINGA)
;       
;           es: 
;           input   stringa              =   (ciao, come stai?)
;                   lb                   =   0
;                   sp                   =   0
;                   
;           output  (CIAO COME STAI)

(defun atomizzazione(stringa)
    (atomizzatore (concatenate 'string stringa "$") 0 0)
)

(defun atomizzatore(stringa lb sp)
    (cond
        ((equal (char stringa lb) #\$)
         '()
        )
        ((equal (char stringa lb) #\Space ) (atomizzatore stringa (+ lb 1 ) (+ sp 1)))
        ((equal (char stringa lb) #\. ) (atomizzatore stringa (+ lb 1 ) (+ sp 1)))
        ((equal (char stringa lb) #\, ) (atomizzatore stringa (+ lb 1 ) (+ sp 1)))
        ((equal (char stringa lb) #\- ) (atomizzatore stringa (+ lb 1 ) (+ sp 1)))
        ((equal (char stringa lb) #\? ) (atomizzatore stringa (+ lb 1 ) (+ sp 1)))
        ((equal (char stringa sp) #\$)
            (append  (list (read-from-string(subseq stringa lb sp))) (atomizzatore stringa (+ lb (- sp lb)) (+ sp 1)))
        )
        ((equal (char stringa sp) #\Space)
            (append  (list (read-from-string(subseq stringa lb sp))) (atomizzatore stringa (+ lb (+ (- sp lb) 1)) (+ sp 1)))
        )
        ((equal (char stringa sp) #\,)
            (append  (list (read-from-string(subseq stringa lb sp))) (atomizzatore stringa (+ lb (+ (- sp lb) 1)) (+ sp 1)))
        )
        ((equal (char stringa sp) #\.)
            (append  (list (read-from-string(subseq stringa lb sp))) (atomizzatore stringa (+ lb (+ (- sp lb) 1)) (+ sp 1)))
        )
        ((equal (char stringa sp) #\-)
            (append  (list (read-from-string(subseq stringa lb sp))) (atomizzatore stringa (+ lb (+ (- sp lb) 1)) (+ sp 1)))
        )
        (T (atomizzatore stringa lb (+ sp 1))
        )
    )
    
)

; -------------------------------------------------------------------------------------------------
;           MAIN DEL BOT
; -------------------------------------------------------------------------------------------------
(defun bot ()
   (princ "(pyotr says) ")
   (princ " hi, i am an expert on the works of cajkovskij / Tchaikovsky / and many other names. In short, I am a fan of the author of Swan Lake. Ask me something and let's see if I can be useful to you!")
   (terpri)
   (defvar *matching* nil)
  (loop
   (princ ">> ")
   (let* (  
            (line (read-line))
            (input (atomizzazione line)))
     (when (string= line "bye") (return))
     (setq _bindings_ nil)
     (princ "(pyotr says) ")
     (format t "~{~(~a ~)~}~%"
            (block rule_management
                (progn 
                        ; -------------------------------------------------------------------------------------------------
                        ;       LIVELLO REGOLE SU SINGOLA TUPLA
                        ; -------------------------------------------------------------------------------------------------

                        (dolist (r _rules_) 
                            (when (match (car r) input)
                                    (progn 
                                    ;dico che ho trovato una regola
                                        (setf *matching* t); ho trovato una matching
                                        (setf *prevTuple* (getValue _rule2tuple_ (car r))); ho trovato una risposta, ho dunque memorizzato tupla
                                        (return-from rule_management (subs (random-elt (rest r))))
                                    )
                            )
                        )

                        ; -------------------------------------------------------------------------------------------------
                        ;        LIVELLO REGOLE SU PIU TUPLE
                        ; -------------------------------------------------------------------------------------------------

                        (dolist (r _general_rules_)
                            (when (match (car r) input)
                                    (progn 
                                        (setf *matching* nil)
                                        (return-from rule_management (subs (random-elt (rest r))))
                                    )
                            )
                        )

                        ; -------------------------------------------------------------------------------------------------
                        ;        LIVELLO REGOLE ULTERIORI
                        ; -------------------------------------------------------------------------------------------------

                        (dolist (r other_rules)
                            (when (match (car r) input)
                                    (progn 
                                        (setf *matching* nil)
                                        (return-from rule_management (subs (random-elt (rest r))))
                                    )
                            )
                        )


                        ; -------------------------------------------------------------------------------------------------
                        ;        LIVELLO REGOLE CON MEMORIA
                        ; -------------------------------------------------------------------------------------------------

                        (if (and *matching* (not (equal *prevTuple* nil))) 
                                    (progn 
                                        (extractKey input keylist)  ;estraggo parole chiave e le salvo
                                        (defparameter rix ())
                                        (generate_rules_memory *prevTuple* _rules_template_memory_)
                                        (dolist (k keyList)
                                            (if (cadr k) 
                                                (progn 
                                                (setf rix (append rix (getInfo *prevTuple* (car k))))
                                                (setf rix (append rix (list '-)))
                                                )
                                            )
                                        )
                                        (setf rix (remove '- rix :count 1 :from-end t ))

                                        (if (equal rix ())  (setf rix (append rix '(I'm sorry I can't answer you - but tell me more if you want)  )) )
                                        (azzeraKey)
                                        (if (not (equal rix ()) ) (return-from rule_management rix))   
                                    )
                        )
                        (setf *matching* nil)
                        (progn (match '(* x) input) (subs (random-elt '((What do you mean by x ?)(I'm sorry I can't answer you - but tell me more if you want)  ) )))
                )
            )
))))


(bot)
