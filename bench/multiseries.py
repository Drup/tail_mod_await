import matplotlib.pyplot as plt
import numpy
import json
import sys

# Load JSON data from a file
print("file", sys.argv[1])

with open(sys.argv[1], 'r') as file:
    json_data = json.load(file)

plt.figure()

names = [
    ("map_tmc", "black", "d-"),
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
    nbs = [elt["nb"] for elt in json_data if elt["name"] == name]
    times = [elt["time"]/1000000 for elt in json_data if elt["name"] == name]
    # confidence = [elt["rsquare"] for elt in json_data if elt["name"] == name]
    plt.plot(nbs, times, linestyle, label=name, color=color)
plt.xlabel('Number of elements')
plt.ylabel('Time per elements (ms)')
plt.legend(loc='upper right')

plt.xscale('log')
plt.xlim(100, 10000)
plt.ylim(0, None)
# plt.yticks(numpy.arange(0, maxtime, 1))

plt.grid()

plt.tight_layout()
# plt.show()
plt.savefig(sys.argv[1].replace("json","png"))
