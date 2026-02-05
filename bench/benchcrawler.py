import matplotlib.pyplot as plt
import numpy
import sys

file = sys.argv[1]

# Load JSON data from a file
print("file", file)
data = numpy.genfromtxt(file, delimiter=",", names=True, dtype=None)

fig, axL = plt.subplots()

# axR = axL.twinx()

# Factor for time (from ns)
unit="ms"
factor=1e6

maxcpu = 4
cpus = numpy.linspace(1, maxcpu, maxcpu)

def get_y(d):
    ys = d["Timens"]/d["SizeMb"]/factor
    y = numpy.average(ys)
    errmin = y-min(ys)
    errmax = max(ys)-y
    return (y, errmin, errmax)

seq_series=[
    ("breath", "blue", ","),
    ("depth", "black", ","),
    ]
for mode, color, marker in seq_series:
    label= "Seq "+ mode
    y, errmin, errmax = get_y(data[data["Mode"] == mode])
    axL.errorbar(cpus,
             [y]*maxcpu,
             yerr=[[errmin]*maxcpu,[errmax]*maxcpu],
             label=label,
             color=color,
             marker=marker
             )

# ideal_x = numpy.linspace(1, maxcpu, 100)
# ideal_y = baseline/ideal_xq
# axL.plot(ideal_x, ideal_y, linestyle='dotted', label='Ideal time (1/n)', color='black')

par_series = [
    ("tree", "green", ","),
    ("graph", "red", ","),
    ]
for mode, color, marker in par_series:
    label = "Async "+ mode
    d = data[data["Mode"] == mode]
    time = list()
    yerrU = list()
    yerrL = list()
    for cpu in cpus:
        t, errmin, errmax = get_y(d[d["CPUs"] == cpu])
        time.append(t)
        yerrU.append(errmin)
        yerrL.append(errmax)
    axL.errorbar(cpus,
                 time,
                 yerr=(yerrL,yerrU),
                 marker=marker,
                 label=label, color=color)
    # axR.plot(xx, overhead, linestyle=linestyle, label=(name+" Overhead"), color=color, marker=".")
    
plt.title('Webcrawler fetching 10000 pages')

plt.xlabel('Parallelism factor')
plt.xlim(1,maxcpu)
plt.xticks(numpy.arange(1, maxcpu+1, 1))

# axL.set_yscale('log')
axL.set_ylabel('Time per data ('+unit+'/Mb)')
axL.set_ylim(0, None)
lines, labels = axL.get_legend_handles_labels()
axL.legend(lines, labels, loc='upper right')

# axR.set_ylabel('Overhead relative to ideal speedup (%)')
# axR.set_ylim(0, 90)
# lines2, labels2 = axR.get_legend_handles_labels()
# axR.legend(lines2, labels2, loc='lower right')

plt.grid()

plt.tight_layout()
plt.show()
# plt.savefig(sys.argv[1].replace("json","png"))
