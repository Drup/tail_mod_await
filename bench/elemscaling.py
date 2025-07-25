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
              elt["time"]/elt["nb"],
              elt["rsquare"] or 1)
            for elt in json_data if elt["name"] == xname]
    sort_data = sorted(data, key=lambda d: d[0])
    x, y, _ = zip(*sort_data)
    error = [ sqrt(y * (1 - c)) for _, y, c in sort_data ]
    ymin = [ y-sqrt(y * (1 - c)) for _, y, c in sort_data ]
    ymax = [ y+sqrt(y * (1 - c)) for _, y, c in sort_data ]
    plt.fill_between(x, ymin, ymax, color=color, alpha=0.3)
    plt.plot(x, y, linestyle=linestyle, label=name, color=color)

plt.title('Map with a busy loop of 20000 iterations on 4 CPUs')
    
plt.xlabel('Number of elements')
plt.ylabel('Time per element (ns)')
plt.legend(loc='upper right')

plt.xscale('log')
plt.yscale('log')
labels = [50, 100, 500, 1000, 5000, 10**4, 50000, 10**5]
plt.xticks(labels,labels)
plt.xlim(50, 10**5)
plt.ylim(10**3, 10**5)

plt.grid()

plt.tight_layout()
# plt.show()
plt.savefig(sys.argv[1].replace("json","png"))
