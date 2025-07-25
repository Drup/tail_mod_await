import matplotlib.pyplot as plt
import numpy
import json
import sys
from math import sqrt

# Load JSON data from a file
print("file", sys.argv[1])

with open(sys.argv[1], 'r') as file:
    json_data = json.load(file)

fig, axL = plt.subplots()

axR = axL.twinx()

names = [
    # ("map_tmc", "black", "-"),
    # ("map_async", "blue", "-"),
    # ("map_async_tmca", "blue", "--"),
    ("map_asyncpar_width", "red", "-"),
    ("map_asyncpar_width_tmca", "red", "--"),
    ("map_asyncpar_full", "green", "-"),
    ("map_asyncpar_full_tmca", "green", "--"),
    ]


basetime = {elt["nb"]:elt["time"]/10000 for elt in json_data if elt["name"] == "tree bench map_asyncpar_width_tmca"}

baseline = basetime[1]

top = 12

ideal_x = numpy.linspace(1, top, 100)
ideal_y = baseline/ideal_x
axL.plot(ideal_x, ideal_y, linestyle='dotted', label='Ideal time (1/n)', color='black')

for name, color, linestyle in names:
    xname = "tree bench "+ name
    data = [( elt["nb"],
              elt["time"]/10000,
              elt["rsquare"] or 1)
            for elt in json_data if elt["name"] == xname]
    sort_data = sorted(data, key=lambda d: d[0])
    xx, _, _ = zip(*sort_data)
    time = [ y for _,y,_ in sort_data ]
    overhead = [ y*x/baseline*100-100 for x,y,_ in sort_data ]
    # error = [ sqrt(y * (1 - c)) for _, y, c in sort_data ]
    # ymin = [ y-sqrt(y * (1 - c)) for _, y, c in sort_data ]
    # ymax = [ y+sqrt(y * (1 - c)) for _, y, c in sort_data ]
    # plt.fill_between(x, ymin, ymax, color=color, alpha=0.3)
    axL.plot(xx, time, linestyle=linestyle, label=name, color=color)
    axR.plot(xx, overhead, linestyle=linestyle, label=(name+" Overhead"), color=color, marker=".")
    
plt.title('Map functions on a tree of 10000 elements with a busy loop of 20000')

plt.xlabel('Parallelism factor')
plt.xlim(1,top)
plt.xticks(numpy.arange(1, top+1, 1))

axL.set_yscale('log')
axL.set_ylabel('Time per element (ns)')
axL.set_ylim(1000, 2*10**4)
lines, labels = axL.get_legend_handles_labels()
axL.legend(lines, labels, loc='upper left')

axR.set_ylabel('Overhead relative to ideal speedup (%)')
axR.set_ylim(0, 90)
lines2, labels2 = axR.get_legend_handles_labels()
axR.legend(lines2, labels2, loc='lower right')

plt.grid()

plt.tight_layout()
# plt.show()
plt.savefig(sys.argv[1].replace("json","png"))
