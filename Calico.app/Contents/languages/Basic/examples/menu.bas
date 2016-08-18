100 print "*************************"
101 print "*                       *"
105 print "*  1) Hanoi towers      *"
110 print "*  2) Primes counting   *"
120 print "*  3) Tic-Tac-Toe       *"
130 print "*                       *"
140 print "*  0) Quit              *"
145 print "*                       *"
150 print "*************************"
160 print: input "-> ";a$
170 if a$="1" then load "hanoi.bas"
180 if a$="2" then load "primes.bas"
190 if a$="3" then load "ttt.bas"
200 if a$<>"0" then print "Please choose a valid option...": goto 100
210 print "Thank you"
220 end
