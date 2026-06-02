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
    ("map_async", "red", "-"),
    ("map_async_tmca", "red", "--"),
    ("map_asyncpar_width", "blue", "-"),
    ("map_asyncpar_width_tmca", "blue", "--"),
    ("map_asyncpar_full", "green", "-"),
    ("map_asyncpar_full_tmca", "green", "--"),
    ]

for name, color, linestyle in names:
    xname = "tree bench "+ name
    data = [( elt["nb"],
              elt["time"]/5000,
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

plt.title('Map on a tree of $N=5000$ elements on $p=4$ CPUs')

plt.xlabel('Number of iterations by busy loop ($l$)')
plt.ylabel('Time per element ($T_p/N$) in $ns$ -- Lower is better')
plt.legend(loc='best')

plt.xscale('log')
plt.yscale('log')
plt.xlim(100, 10**7)
plt.ylim(100, 10**7)

plt.grid()

plt.tight_layout()
# plt.show()
plt.savefig(sys.argv[1].replace("json","png"))
