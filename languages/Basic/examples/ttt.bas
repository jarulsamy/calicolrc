10 print "*****************"
11 print "*               *"
12 print "*  Tic-Tac-Toe  *"
13 print "*               *"
14 print "*****************"
20 gosub 1000: computer = 0
40 gosub 1500
50 if computer then gosub 2400: goto 70
55 if count>0 then 60
56 input "Do you want to start (Y/N):";s$
57 if s$="N" or s$="n" then computer=1:x=int(rnd(1)*9)+1:goto 70
58 if s$<>"Y" and s$<>"y" then print "Please reply 'Y' or 'N'": goto 56
60 gosub 1700
70 gosub 1100: gosub 1300
80 if result<>1 then 90
81 print "**************************"
82 if computer=1 then print "COMPUTER WINS!!!"
83 if computer=0 then print "YOU WIN!!!"
84 print "**************************"
85 gosub 2600: gosub 1500: load "menu.bas"
90 col = 1 + 10 - col
110 if count<9 then 120
111 print "****************"
112 print "      DRAW      "
113 print "****************"
114 gosub 1500: load "menu.bas"
120 computer = 1-computer
130 goto 40

1000 rem Board initialization
1010 a1=0: a2=0: a3=0
1020 a4=0: a5=0: a6=0
1030 a7=0: a8=0: a9=0
1040 col = 10
1050 count = 0
1060 return

1100 REM Play move 'x' using color 'col'
1110 count = count+1
1120 if x=1 then a1=col: return
1130 if x=2 then a2=col: return
1140 if x=3 then a3=col: return
1150 if x=4 then a4=col: return
1160 if x=5 then a5=col: return
1170 if x=6 then a6=col: return
1180 if x=7 then a7=col: return
1190 if x=8 then a8=col: return
1200 if x=9 then a9=col: return
1210 print "INTERNAL ERROR: invalid move x=";x
1220 end

1300 rem Checks for a victory of 'col'
1310 if a1=col then if a2=col then if a3=col then result=1: return
1320 if a4=col then if a5=col then if a6=col then result=1: return
1330 if a7=col then if a8=col then if a9=col then result=1: return
1340 if a1=col then if a4=col then if a7=col then result=1: return
1350 if a2=col then if a5=col then if a8=col then result=1: return
1360 if a3=col then if a6=col then if a9=col then result=1: return
1370 if a1=col then if a5=col then if a9=col then result=1: return
1380 if a3=col then if a5=col then if a7=col then result=1: return
1390 result=0: return

1400 rem Prints symbol for 's' or and empty cell
1410 if s=10  then print " x ";:return
1420 if s=1   then print " o ";:return
1430 if s=110 then print "(X)";:return
1440 if s=101 then print "(O)";:return
1450 print " . ";:return

1500 rem Prints current board
1510 print
1520 x=1: s=a1: gosub 1400
1530 x=2: s=a2: gosub 1400
1540 x=3: s=a3: gosub 1400
1550 print
1560 x=4: s=a4: gosub 1400
1570 x=5: s=a5: gosub 1400
1580 x=6: s=a6: gosub 1400
1590 print
1600 x=7: s=a7: gosub 1400
1610 x=8: s=a8: gosub 1400
1620 x=9: s=a9: gosub 1400
1630 print: print
1640 return

1700 rem Asks for a move
1710 print: s=col: gosub 1400
1711 ml$ = ""
1712 if a1=0 then ml$ = ml$ + "1"
1713 if a2=0 then ml$ = ml$ + "2"
1714 if a3=0 then ml$ = ml$ + "3"
1715 ml$ = ml$ + "/"
1716 if a4=0 then ml$ = ml$ + "4"
1717 if a5=0 then ml$ = ml$ + "5"
1718 if a6=0 then ml$ = ml$ + "6"
1719 ml$ = ml$ + "/"
1720 if a7=0 then ml$ = ml$ + "7"
1721 if a8=0 then ml$ = ml$ + "8"
1722 if a9=0 then ml$ = ml$ + "9"
1723 print "Move ( "+ml$;:input ") : ";m$
1725 if len(m$)<>1 or m$<"1" or m$>"9" then 1840
1730 if m$="1" and a1<>0 then 1840
1740 if m$="2" and a2<>0 then 1840
1750 if m$="3" and a3<>0 then 1840
1760 if m$="4" and a4<>0 then 1840
1770 if m$="5" and a5<>0 then 1840
1780 if m$="6" and a6<>0 then 1840
1790 if m$="7" and a7<>0 then 1840
1800 if m$="8" and a8<>0 then 1840
1810 if m$="9" and a9<>0 then 1840
1820 x = int(val(m$))
1830 return
1840 print "Invalid move"
1850 goto 1710

2000 rem Undo move 'x'
2005 count = count - 1
2010 if x=1 then a1=0: return
2020 if x=2 then a2=0: return
2030 if x=3 then a3=0: return
2040 if x=4 then a4=0: return
2050 if x=5 then a5=0: return
2060 if x=6 then a6=0: return
2070 if x=7 then a7=0: return
2080 if x=8 then a8=0: return
2090 if x=9 then a9=0: return

2095 rem Evaluate move 'x' (level 1 only)
2096 gosub 1100: gosub 1300: gosub 2000: return

2100 rem Evaluate move 'x' (full search, result is -1/0/+1)
2105 total = total+1
2110 gosub 1100: gosub 1300
2120 if result=1 then 2300
2130 x9=x8:x8=x7:x7=x6:x6=x5:x5=x4:x4=x3:x3=x2:x2=x1:x1=x
2140 b9=b8:b8=b7:b7=b6:b6=b5:b5=b4:b4=b3:b3=b2:b2=b1:b1=b
2150 col=1+10-col: b=-2: if count>7 then 2160

2151 if a1=0 then x=1:gosub 2095:if result=1 then b=1:goto 2250
2152 if a2=0 then x=2:gosub 2095:if result=1 then b=1:goto 2250
2153 if a3=0 then x=3:gosub 2095:if result=1 then b=1:goto 2250
2154 if a4=0 then x=4:gosub 2095:if result=1 then b=1:goto 2250
2155 if a5=0 then x=5:gosub 2095:if result=1 then b=1:goto 2250
2156 if a6=0 then x=6:gosub 2095:if result=1 then b=1:goto 2250
2157 if a7=0 then x=7:gosub 2095:if result=1 then b=1:goto 2250
2158 if a8=0 then x=8:gosub 2095:if result=1 then b=1:goto 2250
2159 if a9=0 then x=9:gosub 2095:if result=1 then b=1:goto 2250

2160 if a1=0 and b<1 then x=1:gosub 2100:if result>b then b=result
2170 if a2=0 and b<1 then x=2:gosub 2100:if result>b then b=result
2180 if a3=0 and b<1 then x=3:gosub 2100:if result>b then b=result
2190 if a4=0 and b<1 then x=4:gosub 2100:if result>b then b=result
2200 if a5=0 and b<1 then x=5:gosub 2100:if result>b then b=result
2210 if a6=0 and b<1 then x=6:gosub 2100:if result>b then b=result
2220 if a7=0 and b<1 then x=7:gosub 2100:if result>b then b=result
2230 if a8=0 and b<1 then x=8:gosub 2100:if result>b then b=result
2240 if a9=0 and b<1 then x=9:gosub 2100:if result>b then b=result
2250 col=1+10-col
2260 if b=-2 then b=0
2270 result=-b
2280 x=x1:x1=x2:x2=x3:x3=x4:x4=x5:x5=x6:x6=x7:x7=x8:x8=x9
2290 b=b1:b1=b2:b2=b3:b3=b4:b4=b5:b5=b6:b6=b7:b7=b8:b8=b9
2300 gosub 2000
2310 return

2400 rem Computer chooses a move
2410 bx=-1:b=-2:total=0:start=time(0):print "(Searching ... ";
2420 if a1=0 and b<1 then x=1:gosub 2100:if result>b then b=result:bx=1
2430 if a2=0 and b<1 then x=2:gosub 2100:if result>b then b=result:bx=2
2440 if a3=0 and b<1 then x=3:gosub 2100:if result>b then b=result:bx=3
2450 if a4=0 and b<1 then x=4:gosub 2100:if result>b then b=result:bx=4
2460 if a5=0 and b<1 then x=5:gosub 2100:if result>b then b=result:bx=5
2470 if a6=0 and b<1 then x=6:gosub 2100:if result>b then b=result:bx=6
2480 if a7=0 and b<1 then x=7:gosub 2100:if result>b then b=result:bx=7
2490 if a8=0 and b<1 then x=8:gosub 2100:if result>b then b=result:bx=8
2500 if a9=0 and b<1 then x=9:gosub 2100:if result>b then b=result:bx=9
2509 t = int((time(0)-start)*100)/100
2510 print total;" positions ";t;" seconds)"
2520 x=bx: return

2600 rem Marks the victory line
2605 cc = col+100
2610 if a1+a2+a3 = col*3 then a1=cc: a2=cc: a3=cc
2620 if a4+a5+a6 = col*3 then a4=cc: a5=cc: a6=cc
2630 if a7+a8+a9 = col*3 then a7=cc: a8=cc: a9=cc
2640 if a1+a4+a7 = col*3 then a1=cc: a4=cc: a7=cc
2650 if a2+a5+a8 = col*3 then a2=cc: a5=cc: a8=cc
2660 if a3+a6+a9 = col*3 then a3=cc: a6=cc: a9=cc
2670 if a1+a5+a9 = col*3 then a1=cc: a5=cc: a9=cc
2680 if a3+a5+a7 = col*3 then a3=cc: a5=cc: a7=cc
2690 return

