import matplotlib.pyplot as plt
import numpy
import sys

file = sys.argv[1]

# Load JSON data from a file
print("file", file)
data = numpy.genfromtxt(file, delimiter=",", names=True, dtype=None)

fig, axL = plt.subplots()

axR = axL.twinx()

# Factor for time (from ns)
unit="s"
factor=1e9

maxcpu = 12
cpus = numpy.linspace(1, maxcpu, maxcpu)

def select_data(d):
    return d["Timens"]/factor
def get_y(d):
    ys = sorted(select_data(d))[1:-1]
    y = numpy.average(ys)
    errmin = y-min(ys)
    errmax = max(ys)-y
    return (y, errmin, errmax)

seq_series=[
    ("breath", "seq_manual", "black", ","),
    # ("depth", "seq_depth", "green", ","),
    ]
for mode, label, color, marker in seq_series:
    y, errmin, errmax = get_y(data[data["Mode"] == mode])
    axL.errorbar(cpus,
             [y]*maxcpu,
             yerr=[[errmin]*maxcpu,[errmax]*maxcpu],
             label=label,
             color=color,
             marker=marker
             )

basemode = "breath"
baseline, _, _ = get_y(data[data["Mode"] == basemode])

ideal_x = numpy.linspace(1, maxcpu, 100)
ideal_y = baseline/ideal_x
axL.plot(ideal_x, ideal_y, linestyle='dotted', label='Ideal time (seq/n)', color='black')

par_series = [
    # ("tree", "asyncpar_tree_tmca", "blue", "--"),
    ("graph", "asyncpar_graph_tmca", "red", "--"),
    ("manual", "asyncpar_manual", "blue", "-"),
    ]
for mode, label, color, linestyle in par_series:
    d = data[data["Mode"] == mode]
    time = list()
    # for cpu in cpus:
    #     time.append(select_data(d[d["CPUs"] == cpu]))
    # axL.violinplot(positions=cpus,
    #                dataset=time,
    #                widths=0.1,
    #                showmeans=True,
    #                )    
    time = list()
    yerrU = list()
    yerrL = list()
    overhead = list()
    for cpu in cpus:
        t, errmin, errmax = get_y(d[d["CPUs"] == cpu])
        time.append(t)
        yerrU.append(errmin)
        yerrL.append(errmax)
        overhead.append(t*cpu/baseline*100-100)
    axL.errorbar(cpus,
                 time,
                 yerr=(yerrL,yerrU),
                 linestyle=linestyle,
                 label=label, color=color)
    axR.plot(cpus, overhead, linestyle=linestyle, label=(label+" Overhead"), color=color, marker=".")
    
plt.title('Webcrawler fetching 10k pages')

plt.xlabel('Parallelism factor')
plt.xlim(1,maxcpu)
plt.xticks(numpy.arange(1, maxcpu+1, 1))

# axL.set_yscale('log')
axL.set_ylabel('Time ('+unit+')')
axL.set_ylim(0, None)
lines, labels = axL.get_legend_handles_labels()
axL.legend(lines, labels, loc='upper left', bbox_to_anchor=(0.05, 1))

axR.set_ylabel('Overhead relative to ideal speedup (%)')
axR.set_ylim(0, 90)
lines2, labels2 = axR.get_legend_handles_labels()
axR.legend(lines2, labels2, loc='lower right', bbox_to_anchor=(0.95, 0))

plt.grid()

plt.tight_layout()
plt.savefig(file.replace("csv","png"))
plt.show()
