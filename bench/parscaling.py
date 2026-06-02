import matplotlib.pyplot as plt
import numpy
import json
import sys
from math import sqrt

# Load JSON data from a file
print("file", sys.argv[1])

with open(sys.argv[1], 'r') as file:
    json_data = json.load(file)

plt.figure()

names = [
    ("map_tmc", "black", "-"),
    # ("map_async", "red", "-"),
    # ("map_async_tmca", "red", "--"),
    ("map_asyncpar_width", "blue", "-"),
    ("map_asyncpar_width_tmca", "blue", "--"),
    ("map_asyncpar_full", "green", "-"),
    ("map_asyncpar_full_tmca", "green", "--"),
    ]


basetime = {elt["nb"]:elt["time"] for elt in json_data if elt["name"] == "tree bench map_tmc"}

T_1 = basetime[1]

top = 12

ideal_x = numpy.linspace(1, top, 100)
plt.plot(ideal_x, ideal_x, linestyle='dotted', label='Ideal scaling', color='black')

for name, color, linestyle in names:
    xname = "tree bench "+ name
    data = [( elt["nb"],
              elt["time"],
              elt["rsquare"] or 1)
            for elt in json_data if elt["name"] == xname]
    sort_data = sorted(data, key=lambda d: d[0])
    xx, _, _ = zip(*sort_data)
    yy = [ T_p for _,T_p,_ in sort_data ]
    Tmin = [ T_p-sqrt(T_p * (1 - c)) for _, T_p, c in sort_data ]
    Tmax = [ T_p+sqrt(T_p * (1 - c)) for _, T_p, c in sort_data ]
    scaling = [ T_1/T_p for T_p in yy ]
    scalingmin = [ T_1/T_p for T_p in Tmin ]
    scalingmax = [ T_1/T_p for T_p in Tmax ]
    plt.fill_between(xx, scalingmin, scalingmax, color=color, alpha=0.3)
    plt.plot(xx, scaling, linestyle=linestyle, label=name, color=color)

plt.title('Map on a tree of $N=5000$ elements with a loop of $l=10000$ iterations')

plt.xlabel('Parallelism factor ($p$)')
plt.xlim(1,top)
plt.xticks(numpy.arange(1, top+1, 1))

plt.ylabel('Speedup ($T_{seq}/T_p$) -- Higher is better')
plt.ylim(0.5,top)
plt.yticks(numpy.arange(1, top+1, 1))
plt.legend(loc='upper left')

plt.grid()

plt.tight_layout()
# plt.show()
plt.savefig(sys.argv[1].replace("json","png"))
