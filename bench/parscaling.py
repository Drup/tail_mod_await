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
    ("map_seq", "blue", "-"),
    ("map_seq_tmca", "blue", "--"),
    ("map_par_width", "red", "-"),
    ("map_par_width_tmca", "red", "--"),
    ("map_par_full", "green", "-"),
    ("map_par_full_tmca", "green", "--"),
    ]    

# maxtime = max(elt["time"] for elt in json_data)/1000
# maxnb = max(elt["nb"] for elt in json_data)
# minnb = min(elt["nb"] for elt in json_data)



for name, color, linestyle in names:
    xname = "tree bench "+ name
    data = [(elt["nb"],elt["time"]/1000/1000, elt["rsquare"] or 1) for elt in json_data if elt["name"] == xname]
    sort_data = sorted(data, key=lambda d: d[0])
    x, y, _ = zip(*sort_data)
    # ymin = [ y-sqrt(y * (1 - c)) for _, y, c in sort_data ]
    # ymax = [ y+sqrt(y * (1 - c)) for _, y, c in sort_data ]
    # plt.fill_between(x, ymin, ymax, color=color, alpha=0.3)
    plt.plot(x, y, linestyle=linestyle, label=name, color=color)

plt.xlabel('Parallelism factor')
plt.ylabel('Time per elements (ms)')
plt.legend(loc='upper right')

# plt.xscale('log')
plt.xlim(1, None)
plt.ylim(0, None)
# plt.yticks(numpy.arange(0, maxtime, 1))

plt.grid()

plt.tight_layout()
plt.show()
# plt.savefig(sys.argv[1].replace("json","png"))
