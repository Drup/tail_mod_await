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
    ("map_seq", "red", "-"),
    ("map_seq_tmca", "red", "--"),
    ("map_par_width", "blue", "-"),
    ("map_par_width_tmca", "blue", "--"),
    ("map_par_full", "green", "-"),
    ("map_par_full_tmca", "green", "--"),
    ]    


basetime = {elt["nb"]:elt["time"] for elt in json_data if elt["name"] == "tree bench map_tmc"}

for name, color, linestyle in names:
    xname = "tree bench "+ name
    data = [( elt["nb"],
              elt["time"]/10000,
              elt["rsquare"] or 1)
            for elt in json_data if elt["name"] == xname]
    sort_data = sorted(data, key=lambda d: d[0])
    x, y, _ = zip(*sort_data)
    error = [ sqrt(y * (1 - c)) for _, y, c in sort_data ]
    ymin = [ y-sqrt(y * (1 - c)) for _, y, c in sort_data ]
    ymax = [ y+sqrt(y * (1 - c)) for _, y, c in sort_data ]
    plt.fill_between(x, ymin, ymax, color=color, alpha=0.3)
    plt.plot(x, y, linestyle=linestyle, label=name, color=color)
    # plt.errorbar(x, y, yerr=error,
    #              linestyle=linestyle, label=name, color=color,
    #              elinewidth=1,
    #              capsize=3,
    #              )

plt.title('Map functions on a tree of 10000 elements')
    
plt.xlabel('Number of iterations by busy loop')
plt.ylabel('Time per element (ns)')
plt.legend(loc='best')

plt.xscale('log')
plt.yscale('log')
plt.xlim(100, 10**7)
plt.ylim(100, 10**7)

plt.grid()

plt.tight_layout()
plt.show()
# plt.savefig(sys.argv[1].replace("json","png"))
