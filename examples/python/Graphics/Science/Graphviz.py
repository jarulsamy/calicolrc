import Graphics

hello_world = """
digraph {
        node [label="\\N"];
        graph [bb="0,0,74,112"];
        Hello [pos="37,93", width="0.91667", height="0.52778"];
        World [pos="37,19", width="1.0278", height="0.52778"];
        Hello -> World [pos="e,37,38.249 37,73.943 37,66.149 37,56.954 37,48.338"];
}
"""

fsm = """
digraph finite_state_machine {
 graph [rankdir=LR, size="8,5"];
 node [label="\\N", shape=doublecircle];
 graph [bb="0,0,816,358"];
 LR_0 [pos="40,116", width="1.1111", height="1.1111"];
 LR_3 [pos="360,40", width="1.1111", height="1.1111"];
 LR_4 [pos="360,318", width="1.1111", height="1.1111"];
 LR_8 [pos="776,165", width="1.1111", height="1.1111"];
 LR_2 [shape=circle, pos="192,159", width=1, height=1];
 LR_1 [shape=circle, pos="192,59", width=1, height=1];
 LR_6 [shape=circle, pos="360,188", width=1, height=1];
 LR_5 [shape=circle, pos="500,116", width=1, height=1];
 LR_7 [shape=circle, pos="636,93", width=1, height=1];
 LR_0 -> LR_2 [label="SS(B)", pos="e,157.22,149.16 78.749,126.96 99.725,132.9 125.73,140.25 147.53,146.42", lp="118,151"];
 LR_0 -> LR_1 [label="SS(S)", pos="e,158.23,71.665 77.573,101.91 99.092,93.84 126.19,83.679 148.61,75.27", lp="118,102"];
 LR_1 -> LR_3 [label="S($end)", pos="e,320.2,44.501 228.04,54.924 251.96,52.219 283.68,48.632 310.02,45.652", lp="274,62"];
 LR_2 -> LR_6 [label="SS(b)", pos="e,324.42,181.86 227.63,165.15 252.91,169.51 287.06,175.41 314.34,180.12", lp="274,186"];
 LR_2 -> LR_5 [label="SS(a)", pos="e,463.91,115.59 226.35,147.77 251.89,139.94 287.74,130.03 320,125 365.02,117.97 417.07,116.03 453.58,115.66", lp="360,134"];
 LR_2 -> LR_4 [label="S(A)", pos="e,330.83,290.4 218.25,183.84 246.53,210.61 291.73,253.39 323.47,283.43", lp="274,271"];
 LR_5 -> LR_7 [label="S(b)", pos="e,600.03,99.082 535.73,109.96 552.31,107.15 572.22,103.79 589.91,100.79", lp="568,116"];
 LR_5 -> LR_5 [label="S(a)", pos="e,512.75,149.75 487.25,149.75 487.25,161 491.5,170 500,170 505.71,170 509.5,165.94 511.38,159.86", lp="500,179"];
 LR_6 -> LR_6 [label="S(b)", pos="e,373.5,221.49 346.5,221.49 346.43,232.86 350.93,242 360,242 366.09,242 370.12,237.87 372.09,231.72", lp="360,251"];
 LR_6 -> LR_5 [label="S(a)", pos="e,467.55,132.69 392.12,171.48 411.81,161.36 437.22,148.29 458.44,137.38", lp="432,165"];
 LR_7 -> LR_8 [label="S(b)", pos="e,740.88,145.25 668.91,108.59 683.96,115.84 701.98,124.68 718,133 722.54,135.36 727.25,137.86 731.94,140.39", lp="704,142"];
 LR_7 -> LR_5 [label="S(a)", pos="e,528.19,93.436 601.62,81.453 586.77,78.125 569.31,76.471 554,81 548.23,82.705 542.51,85.24 537.06,88.18", lp="568,90"];
 LR_8 -> LR_6 [label="S(b)", pos="e,396.05,191.89 736.24,171.95 688.77,179.86 606.88,192.3 536,197 504.07,199.12 495.97,198.38 464,197 444.99,196.18 424.08,194.52\
 406.03,192.84", lp="568,204"];
 LR_8 -> LR_5 [label="S(a)", pos="e,535.1,124.68 736.05,159.85 691.42,153.78 617.14,142.77 554,129 551.03,128.35 547.98,127.67 544.91,126.97", lp="636,159"];
}
"""

kennedys = """
graph G {
 node [label="\\N"];
 graph [bb="0,0,1434,532"];
 I5 [label="Caroline Bouvier Kennedy\nb. 27.11.1957 New York", shape=ellipse, color=red, style=bold, image="images/165px-Caroline_Kennedy.jpg", labelloc=b, pos="288,31", width="3.8333", height="0.86111"];
 I1 [label="John Fitzgerald Kennedy\nb. 29.5.1917 Brookline\nd. 22.11.1963 Dallas", shape=box, color=blue, style=bold, image="images/kennedyface.jpg", labelloc=b, pos="906,266", width="2.5556", height="0.86111"];
 I6 [label="John Fitzgerald Kennedy\nb. 25.11.1960 Washington\nd. 16.7.1999 over the Atlantic Ocean, near Aquinnah, MA, USA", shape=box, color=blue, style=bold, image="images/180px-JFKJr2.jpg", labelloc=b, pos="671,31", width="6.3056", height="0.86111"];
 I7 [label="Patrick Bouvier Kennedy\nb. 7.8.1963\nd. 9.8.1963", shape=box, color=blue, style=bold, pos="1008,31", width="2.5556", height="0.86111"];
 I2 [label="Jaqueline Lee Bouvier\nb. 28.7.1929 Southampton\nd. 19.5.1994 New York City", shape=ellipse, color=red, style=bold, image="images/jacqueline-kennedy-onassis.jpg", labelloc=b, pos="288,142", width="4.1111", height="1.2222"];
 I8 [label="Joseph Patrick Kennedy\nb. 6.9.1888 East Boston\nd. 16.11.1969 Hyannis Port", shape=box, color=blue, style=bold, image="images/1025901671.jpg", labelloc=b, pos="973,501", width="2.8889", height="0.86111"];
 I10 [label="Joseph Patrick Kennedy Jr\nb. 1915\nd. 1944", shape=box, color=blue, style=bold, pos="1112,266", width="2.6667", height="0.86111"];
 I11 [label="Rosemary Kennedy\nb. 13.9.1918\nd. 7.1.2005", shape=ellipse, color=red, style=bold, image="images/rosemary.jpg", labelloc=b, pos="1330,266", width="2.8889", height="1.2222"];
 I12 [label="Kathleen Kennedy\nb. 1920\nd. 1948", shape=ellipse, color=red, style=bold, pos="430,266", width="2.7778", height="1.2222"];
 I13 [label="Eunice Mary Kennedy\nb. 10.7.1921 Brookline", shape=ellipse, color=red, style=bold, pos="672,266", width="3.4444", height="0.86111"];
 I9 [label="Rose Elizabeth Fitzgerald\nb. 22.7.1890 Boston\nd. 22.1.1995 Hyannis Port", shape=ellipse, color=red, style=bold, image="images/Rose_kennedy.JPG", labelloc=b, pos="889,390", width="3.8889", height="1.2222"];
 I15 [label="Aristotle Onassis", shape=box, color=blue, style=bold, pos="66,31", width="1.8333", height="0.5"];
 I3 [label="John Vernou Bouvier III\nb. 1891\nd. 1957", shape=box, color=blue, style=bold, image="images/BE037819.jpg", labelloc=b, pos="155,390", width="2.4444", height="0.86111"];
 I4 [label="Janet Norton Lee\nb. 2.10.1877\nd. 3.1.1968", shape=ellipse, color=red, style=bold, image="images/n48862003257_1275276_1366.jpg", labelloc=b, pos="217,266", width="2.6389", height="1.2222"];
 I1 -- I5 [style=bold, color=blue, pos="836.13,234.95 825.73,230.51 815.11,226.07 805,222 649.2,159.34 464.04,92.829 363,57.194"];
 I1 -- I6 [style=bold, color=orange, pos="874.79,234.79 829.85,189.85 747.35,107.35 702.34,62.335"];
 I2 -- I6 [style=bold, color=orange, pos="394.18,111.23 447.09,95.893 510.92,77.394 563.95,62.025"];
 I1 -- I7 [style=bold, color=orange, pos="919.54,234.79 939.05,189.85 974.86,107.35 994.4,62.335"];
 I2 -- I7 [style=bold, color=orange, pos="430.84,130.46 557.06,118.62 745.63,96.93 907,62 909.96,61.36 912.95,60.673 915.97,59.949"];
 I1 -- I2 [style=bold, color=violet, pos="841.44,234.96 829.52,230.07 817.03,225.49 805,222 768.85,211.52 562.78,181.07 421.98,160.91"];
 I8 -- I1 [style=bold, color=blue, pos="1013,469.73 1023.1,459.54 1032.6,447.37 1038,434 1052.7,397.75 1057.2,380.08 1038,346 1026.3,325.22 1006.8,309.22 986.29,297.19"];
 I8 -- I10 [style=bold, color=orange, pos="1022.6,469.85 1035.6,459.74 1048.6,447.58 1058,434 1087.3,391.85 1101.4,332.94 1107.6,297.24"];
 I9 -- I10 [style=bold, color=orange, pos="958.03,351.61 989.5,334.12 1026.3,313.67 1056,297.15"];
 I8 -- I11 [style=bold, color=orange, pos="1030.4,469.89 1049.9,458.94 1071.6,446.29 1091,434 1157.2,392.07 1230.5,339.56 1278.3,304.51"];
 I9 -- I11 [style=bold, color=orange, pos="1005.4,365.55 1068.6,351.3 1147.7,331.84 1217,310 1229.4,306.08 1242.5,301.47 1255.1,296.75"];
 I8 -- I12 [style=bold, color=orange, pos="868.87,486.11 812.59,475.8 742.79,459.28 684,434 606.29,400.58 526.5,343.31 477.58,305.06"];
 I9 -- I12 [style=bold, color=orange, pos="768.68,367.43 700.57,353.43 614.23,333.62 539,310 526.73,306.15 513.9,301.53 501.53,296.75"];
 I8 -- I13 [style=bold, color=orange, pos="868.7,480.37 816.23,468.19 759.67,451.71 740,434 699.9,397.89 683.01,334.76 676.23,297.03"];
 I9 -- I13 [style=bold, color=orange, pos="821.53,351.45 789.31,333.03 751.42,311.38 721.85,294.48"];
 I8 -- I9 [style=bold, color=violet, pos="949.38,469.78 940.74,458.37 930.81,445.25 921.53,432.99"];
 I9 -- I1 [style=bold, color=red, pos="895.04,345.93 897.26,329.78 899.71,311.86 901.74,297.08"];
 I2 -- I5 [style=bold, color=red, pos="288,97.811 288,85.84 288,73.137 288,62.057"];
 I2 -- I15 [style=bold, color=violet, pos="212.22,104.11 175.04,85.52 131.98,63.99 102.26,49.131"];
 I3 -- I2 [style=bold, color=blue, pos="133.65,358.84 112.16,323.22 85.783,264.37 113,222 127.72,199.09 150.8,182.66 175.43,170.91"];
 I3 -- I4 [style=bold, color=violet, pos="170.65,358.71 178.18,343.64 187.33,325.34 195.51,308.99"];
 I4 -- I2 [style=bold, color=red, pos="241.43,223.33 248.41,211.14 256.03,197.84 263.04,185.6"];
}
"""

phil = """
digraph PhiloDilemma {
 node [label="\\N", shape=box];
 graph [overlap=false,
     label="PetriNet Model PhiloDilemma\nExtracted from ConceptBase and layed out by Graphviz ",
     fontsize=12,
     lp="245.93,19",
     bb="0,0,491.87,530.82"];
 bec3 [width="0.75", pos="296.28,57", height="0.5"];
 rel3 [width="0.75", pos="382.55,170.16", height="0.5"];
 bec2 [width="0.75", pos="463.87,440.61", height="0.5"];
 rel2 [width="0.75", pos="323.5,456.48", height="0.5"];
 acq2 [width="0.75", pos="354.66,327.06", height="0.5"];
 acq3 [width="0.75", pos="249.17,214.35", height="0.5"];
 bec1 [width="0.75", pos="48.589,390.96", height="0.5"];
 rel1 [width="0.75", pos="106.07,262.53", height="0.5"];
 acq1 [width="0.75", pos="202.02,354.64", height="0.5"];
 hu3 [shape=circle, fixedsize=true, width="0.88889", pos="238.5,121.41", height="0.90278"];
 th3 [shape=circle, fixedsize=true, width="0.88889", pos="379.11,85.796", height="0.90278"];
 ri3 [shape=circle, fixedsize=true, width="0.88889", pos="340.22,244.15", height="0.90278"];
 ea3 [shape=circle, fixedsize=true, width="0.88889", pos="309.6,154.58", height="0.90278"];
 hu2 [shape=circle, fixedsize=true, width="0.88889", pos="438.46,357.57", height="0.90278"];
 th2 [shape=circle, fixedsize=true, width="0.88889", pos="396.96,497.32", height="0.90278"];
 ri2 [shape=circle, fixedsize=true, width="0.88889", pos="281.22,382.8", height="0.90278"];
 ea2 [shape=circle, fixedsize=true, width="0.88889", pos="372.04,397.09", height="0.90278"];
 hu1 [shape=circle, fixedsize=true, width="0.88889", pos="133.15,411.38", height="0.90278"];
 th1 [shape=circle, fixedsize=true, width="0.88889", pos="33.5,304.61", height="0.90278"];
 ri1 [shape=circle, fixedsize=true, width="0.88889", pos="190.97,263.25", height="0.90278"];
 ea1 [shape=circle, fixedsize=true, width="0.88889", pos="132.77,334.54", height="0.90278"];
 ri3 -> acq2 [pos="e,351.45,308.62 345.84,276.44 347.12,283.77 348.47,291.51 349.71,298.66"];
 ri3 -> acq3 [pos="e,276.45,223.28 309.65,234.14 302.1,231.67 293.92,228.99 286.1,226.43"];
 hu3 -> acq3 [pos="e,247.11,196.35 242.24,153.91 243.45,164.48 244.78,176.11 245.96,186.32"];
 bec3 -> hu3 [pos="e,260.13,97.298 279.88,75.284 275.85,79.775 271.42,84.713 267,89.637"];
 th3 -> bec3 [pos="e,323.36,66.415 348.73,75.237 343.67,73.476 338.37,71.633 333.16,69.823"];
 rel3 -> th3 [pos="e,380.44,118.36 381.81,151.86 381.52,144.9 381.18,136.64 380.85,128.37"];
 rel3 -> ri3 [pos="e,356.34,215.99 372.09,188.45 368.82,194.16 365.1,200.67 361.37,207.19"];
 ea3 -> rel3 [pos="e,355.35,164.35 340.96,161.28 342.46,161.6 343.97,161.92 345.48,162.24"];
 acq3 -> ea3 [pos="e,286.56,177.37 267.63,196.09 271.36,192.4 275.38,188.43 279.39,184.46"];
 ri2 -> acq1 [pos="e,229.1,364.27 250.83,371.99 246.8,370.56 242.64,369.08 238.53,367.62"];
 ri2 -> acq2 [pos="e,330.83,345.14 306.91,363.3 312.01,359.43 317.43,355.32 322.71,351.31"];
 hu2 -> acq2 [pos="e,381.81,336.94 408.2,346.55 402.81,344.59 397.14,342.53 391.59,340.5"];
 bec2 -> hu2 [pos="e,447.96,388.62 458.36,422.59 456.15,415.38 453.51,406.76 450.89,398.2"];
 th2 -> bec2 [pos="e,442.23,458.95 421.87,476.2 425.95,472.75 430.21,469.14 434.38,465.6"];
 rel2 -> th2 [pos="e,368.84,481.69 350.85,471.68 353.79,473.32 356.81,475 359.84,476.68"];
 rel2 -> ri2 [pos="e,297.48,411.13 313.05,438.26 309.82,432.64 306.14,426.23 302.46,419.81"];
 ea2 -> rel2 [pos="e,338.23,438.45 351.45,422.28 349.23,424.99 346.96,427.76 344.73,430.49"];
 acq2 -> ea2 [pos="e,364.2,365.53 359.13,345.09 359.95,348.41 360.84,351.99 361.76,355.67"];
 ri1 -> acq3 [pos="e,227.59,232.49 215.66,242.51 217.02,241.36 218.4,240.21 219.77,239.05"];
 ri1 -> acq1 [pos="e,199.84,336.62 194.9,295.73 196.12,305.82 197.45,316.86 198.63,326.63"];
 hu1 -> acq1 [pos="e,180.16,372.65 158.02,390.9 162.66,387.08 167.55,383.04 172.32,379.12"];
 bec1 -> hu1 [pos="e,101.86,403.83 75.811,397.54 81.002,398.79 86.54,400.13 92.052,401.46"];
 th1 -> bec1 [pos="e,45.376,372.58 39.118,336.76 40.603,345.26 42.194,354.37 43.641,362.64"];
 rel1 -> th1 [pos="e,61.76,288.22 79.049,278.2 76.228,279.84 73.327,281.52 70.421,283.2"];
 rel1 -> ri1 [pos="e,158.96,262.98 133.4,262.76 138.37,262.8 143.65,262.85 148.92,262.89"];
 ea1 -> rel1 [pos="e,112.77,280.6 121.45,304 119.74,299.39 117.99,294.66 116.3,290.12"];
 acq1 -> ea1 [pos="e,163.6,343.49 174.65,346.7 174.2,346.57 173.74,346.43 173.28,346.3"];
}
"""

tree = """digraph {
        node [label="\\N", shape=record];
        graph [bb="0,0,264,260"];
        14 [label="<left> | 14 | <right>", pos="101,241", rects="62.5,223,83.5,259 83.5,223,118.5,259 118.5,223,139.5,259", width="1.0556", height="0.51389"];
        7 [label="<left> |  7 | <right>", pos="42,167", rects="8,149,29,185 29,149,55,185 55,149,76,185", width="0.94444", height="0.51389"];
        26 [label="<left> | 26 | <right>", pos="160,167", rects="121.5,149,142.5,185 142.5,149,177.5,185 177.5,149,198.5,185", width="1.0556", height="0.51389"];
        12 [label="<left> | 12 | <right>", pos="38,93", rects="-0.5,75,20.5,111 20.5,75,55.5,111 55.5,75,76.5,111", width="1.0556", height="0.51389"];
        20 [label="<left> | 20 | <right>", pos="132,93", rects="93.5,75,114.5,111 114.5,75,149.5,111 149.5,75,170.5,111", width="1.0556", height="0.51389"];
        31 [label="<left> | 31 | <right>", pos="226,93", rects="187.5,75,208.5,111 208.5,75,243.5,111 243.5,75,264.5,111", width="1.0556", height="0.51389"];
        17 [label="<left> | 17 | <right>", pos="104,19", rects="65.5,1,86.5,37 86.5,1,121.5,37 121.5,1,142.5,37", width="1.0556", height="0.51389"];
        14:left -> 7 [pos="e,58.226,185.32 73,223 73,212.77 68.994,202.66 63.861,193.9"];
        14:right -> 26 [pos="e,143.77,185.32 129,223 129,212.77 133.01,202.66 138.14,193.9"];
        7:right -> 12 [pos="e,52.328,111.06 66,149 66,138.84 62.264,128.62 57.521,119.76"];
        26:left -> 20 [pos="e,132,111.16 132,149 132,140.01 132,130.22 132,121.37"];
        26:right -> 31 [pos="e,205.67,111.19 188,149 188,138.14 192.96,127.82 199.29,119.06"];
        20:left -> 17 [pos="e,104,37.159 104,75 104,66.015 104,56.218 104,47.367"];
}
"""

count = 1
for content, options in [(hello_world, {"label": "Hello World",
                                        "default_shape" : "circle",
                                        }),
                         (fsm, {"label": "Finite State Machine", "default_shape":"doublecircle"}),
                         (kennedys, {"label": "Kennedys", "default_shape": "circle", "width": 800}),
                         (phil, {"default_shape":"box"}),
                         (tree, {"label": "Binary Tree", "default_shape" : "box", "line_type": "line"})
                        ]:
    g = Graphics.Graph()
    g.layout(content, False) ## don't process Dot file
    #win = Graphics.Window("Graph #%d" % count, 500, 500)
    count += 1
    g.draw(options)
