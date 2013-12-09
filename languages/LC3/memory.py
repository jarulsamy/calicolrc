fp = open("Memory.csv", "w")

fp.write("PC:,,NZP:,\n")
fp.write("R0:,,R1:,,R2:,,R3:,,R4:,,R5:,,R6:,,R7:,\n")

fp.write("Memory," + ",".join(["x%02X" % col for col in range(256)]) + "\n")
for i in range(256):
    fp.write("x%04X:" % (i * 256) + ("," * 256) + "\n")
fp.close()

def initialize():
    doc = calico.GetDocument("Memory.csv")
    for col in range(256):
        for row in range(256):
            doc.SetData(col + 1, row + 1, "")
