import os

import matplotlib.pyplot as plt
import csv

def add_data(group, values):
    min_val = values[0]
    mean = values[1]
    max_val = values[2]
    std = values[3]

    std_low = mean - std
    if std_low < 0:
        std_low = min_val
    err_low = mean - std_low

    std_high = mean + std
    if std_high > 1:
        std_high = max_val
    err_high = std_high - mean

    group["vals"].append(mean)
    group["errsLow"].append(err_low)
    group["errsHigh"].append(err_high)


def add_error_bar(ax, x_vals, data, type):
    s = '-' if type == "coop" else '-.'
    c = 'blue' if type == "coop" else 'red'
    eb = ax.errorbar(x_vals, data["vals"], [data["errsLow"], data["errsHigh"]], fmt='.', linewidth=2, capsize=6, ls=s, color=c, label=type)
    eb[-1][0].set_linestyle(s)


def create_graph(file):
    data={
        "dimension": "",
        "xVals": [],
        "coop": {
            "vals": [],
            "errsLow": [],
            "errsHigh": []
        },
        "mal": {
            "vals": [],
            "errsLow": [],
            "errsHigh": []
        }
    }

    with open(file, 'r') as f:
        reader = csv.reader(f)
        for i, row in enumerate(reader):
            if i == 0:
                data['dimension'] = row[0]
            else:
                row_vals = [float(x) for x in row]
                data['xVals'].append(row_vals[0])
                add_data(data["coop"], row_vals[1:7])
                add_data(data["mal"], row_vals[8:])

    plt.style.use('_mpl-gallery')
    fig, ax = plt.subplots(figsize=(8, 6))
    fig.tight_layout()

    add_error_bar(ax, data["xVals"], data["coop"], "coop")
    add_error_bar(ax, data["xVals"], data["mal"], "mal")

    ax.set(xlim=(0, None), ylim=(0, None))
    ax.grid(True, which='both')
    ax.axhline(y=0, color='k')
    ax.axvline(x=0, color='k')
    plt.xticks(data["xVals"], data["xVals"])
    plt.legend(loc='lower left')

    plt.savefig(f'../data/graphs/{data["dimension"]}_rep.png')


def main():
    csvs = os.listdir('../data/processed')
    for f in csvs:
        if f.endswith('_litest.csv'):
            create_graph(f'../data/processed/{f}')

if __name__ == "__main__":
    main()