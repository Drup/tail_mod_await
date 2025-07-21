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
    # ("map_tmc", "black", "-"),
    # ("map_seq", "blue", "-"),
    # ("map_seq_tmca", "blue", "--"),
    # ("map_par_width", "red", "-"),
    # ("map_par_width_tmca", "red", "--"),
    # ("map_par_full", "green", "-"),
    ("picos random map_par_tmca", "green", "--"),
    ("picos multififo map_par_tmca", "red", "--"),
    ("moonpool ws map_par_tmca", "blue", "--"),
    ("moonpool fifo map_par_tmca", "orange", "--"),
    ]

basetime = {elt["nb"]:elt["time"] for elt in json_data if elt["name"] == "tree bench map_tmc"}

for name, color, linestyle in names:
    xname = "tree bench "+ name
    data = [( elt["nb"],
              elt["time"]/basetime[elt["nb"]]*100
              ,
              elt["rsquare"] or 1)
            for elt in json_data if elt["name"] == xname]
    sort_data = sorted(data, key=lambda d: d[0])
    x, y, _ = zip(*sort_data)
    error = [ sqrt(y * (1 - c)) for _, y, c in sort_data ]
    ymin = [ y-sqrt(y * (1 - c)) for _, y, c in sort_data ]
    ymax = [ y+sqrt(y * (1 - c)) for _, y, c in sort_data ]
    plt.fill_between(x, ymin, ymax, color=color, alpha=0.3)
    plt.plot(x, y, linestyle=linestyle, label=name, color=color)
    
top = max(x)
    
x = numpy.linspace(1, top, 100)
y = data[0][1]/x
plt.plot(x, y, linestyle='--', label='Ideal time (100/n)', color='black')
    
plt.xlabel('Parallelism factor')
plt.ylabel('Time relative to TMC (%)')
plt.legend(loc='upper right')

# plt.yscale('log')
plt.xlim(1, top)
# plt.ylim(0, None)
plt.xticks(numpy.arange(1, top, 1))
# plt.yticks(numpy.arange(10, 120, 10))

plt.grid()

plt.tight_layout()
# plt.show()
plt.savefig(sys.argv[1].replace("json","png"))
