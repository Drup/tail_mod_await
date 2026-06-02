import matplotlib.pyplot as plt
import numpy
import sys

file = sys.argv[1]

# Load JSON data from a file
print("file", file)
data = numpy.genfromtxt(file, delimiter=",", names=True, dtype=None)

fig, axL = plt.subplots()

# axR = axL.twinx()

maxcpu = 4
cpus = numpy.linspace(1, maxcpu, maxcpu)

basemode = "breath"
baseline = numpy.average(sorted(data[data["Mode"] == basemode]["Timens"])[1:-1])

def select_data(d):
    return d["Timens"]
def get_y(d):
    ys = sorted(select_data(d))[1:-1]
    y = numpy.average(ys)
    ymin = min(ys)
    ymax = max(ys)
    return (y, ymin, ymax)


ideal_x = numpy.linspace(1, maxcpu, 100)
axL.plot(ideal_x, ideal_x, linestyle='dotted', label='Ideal speedup', color='black')

seq_series=[
    ("breath", "seq_manual", "black", ","),
    # ("depth", "seq_depth", "green", ","),
    ]
for mode, label, color, marker in seq_series:
    y, errmin, errmax = get_y(data[data["Mode"] == mode])
    plt.plot(cpus,
             [baseline/y]*maxcpu,
             label=label,
             color=color,
             marker=marker
             )


par_series = [
    # ("tree", "asyncpar_tree_tmca", "blue", "--"),
    ("graph", "asyncpar_graph_tmca", "red", "--"),
    ("manual", "asyncpar_manual", "blue", "-"),
    ]
for mode, label, color, linestyle in par_series:
    d = data[data["Mode"] == mode]
    time = list()
    yerrU = list()
    yerrL = list()
    for cpu in cpus:
        t, tmin, tmax = get_y(d[d["CPUs"] == cpu])
        time.append(baseline/t)
        yerrU.append(baseline/tmin)
        yerrL.append(baseline/tmax)
    plt.fill_between(cpus, yerrU, yerrL, color=color, alpha=0.3)
    plt.plot(cpus,
             time,
             linestyle=linestyle,
             label=label, color=color)

plt.title('Webcrawler fetching 10k pages')

plt.xlabel('Parallelism factor ($p$)')
plt.xlim(1,maxcpu)
plt.xticks(numpy.arange(1, maxcpu+1, 1))

# axL.set_yscale('log')
plt.ylabel('Speedup ($T_{seq}/T_p$) -- Higher is better')
plt.ylim(0.5, maxcpu)
plt.yticks(numpy.arange(1, maxcpu+1, 1))
plt.legend(loc='upper left')

# axR.set_ylabel('Overhead relative to ideal speedup (%)')
# axR.set_ylim(0, 90)
# lines2, labels2 = axR.get_legend_handles_labels()
# axR.legend(lines2, labels2, loc='lower right', bbox_to_anchor=(0.95, 0))

plt.grid()

plt.tight_layout()
plt.savefig(file.replace("csv","png"))
# plt.show()
