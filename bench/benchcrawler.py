import matplotlib.pyplot as plt
import numpy
import sys

file = sys.argv[1]

# Load JSON data from a file
print("file", file)
data = numpy.genfromtxt(file, delimiter="\t", names=True, dtype=None)

fig, axL = plt.subplots()

# axR = axL.twinx()

names = [
    ("Async Par", "red", "+"),
    ]


# basetime = {elt["nb"]:elt["time"]/10000 for elt in json_data if elt["name"] == "tree bench map_asyncpar_width_tmca"}

# baseline = basetime[1]

top = 24

# ideal_x = numpy.linspace(1, top, 100)
# ideal_y = baseline/ideal_x
# axL.plot(ideal_x, ideal_y, linestyle='dotted', label='Ideal time (1/n)', color='black')

for name, color, marker in names:
    xname = "Webcrawler "+ name
    axL.plot(data["CPUs"], data["Timens"]/data["SizeMb"]/10e6, linestyle=" ", marker=marker, label=name, color=color)
    # axR.plot(xx, overhead, linestyle=linestyle, label=(name+" Overhead"), color=color, marker=".")
    
plt.title('Webcrawler fetching 10000 pages')

plt.xlabel('Parallelism factor')
plt.xlim(1,top)
plt.xticks(numpy.arange(1, top+1, 1))

# axL.set_yscale('log')
axL.set_ylabel('Time per data (ms/Mb)')
axL.set_ylim(0, None)
lines, labels = axL.get_legend_handles_labels()
axL.legend(lines, labels, loc='upper left')

# axR.set_ylabel('Overhead relative to ideal speedup (%)')
# axR.set_ylim(0, 90)
# lines2, labels2 = axR.get_legend_handles_labels()
# axR.legend(lines2, labels2, loc='lower right')

plt.grid()

plt.tight_layout()
plt.show()
# plt.savefig(sys.argv[1].replace("json","png"))
