import os
import csv
import matplotlib.pyplot as plt

def add_category(cat, values):
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

    cat['vals'].append(mean)
    cat['errsLow'].append(err_low)
    cat['errsHigh'].append(err_high)


def add_error_bar(ax, x_vals, data, node_type, cat):
    data = data[node_type][cat]
    if node_type == 'coop':
        s = '-'
        c = 'blue'
    else:
        s = '-.'
        c = 'red'

    eb = ax.errorbar(
        x_vals,
        data["vals"],
        [data["errsLow"], data["errsHigh"]],
        fmt='.',
        linewidth=2,
        capsize=6,
        ls=s,
        color=c,
        label=f'{node_type} {cat}'
    )
    eb[-1][0].set_linestyle(s)


def create_trust_graph(data):
    plt.style.use('_mpl-gallery')
    fig, ax = plt.subplots(figsize=(8, 6))
    fig.tight_layout()

    add_error_bar(ax, data["xVals"], data, "coop", 'trusted')
    add_error_bar(ax, data["xVals"], data, "mal", 'trusted')

    ax.set(xlim=(0, None), ylim=(0, None))
    ax.grid(True, which='both')
    ax.axhline(y=0, color='k')
    ax.axvline(x=0, color='k')
    plt.xticks(data["xVals"], data["xVals"])
    ax.legend(loc='best',fontsize='small')

    plt.savefig(f'../data/graphs/{data["dimension"]}_trusted.png')


def create_untrust_graph(data):
    plt.style.use('_mpl-gallery')
    fig, ax = plt.subplots(figsize=(8, 6))
    fig.tight_layout()

    add_error_bar(ax, data["xVals"], data, "coop", 'untrusted')
    add_error_bar(ax, data["xVals"], data, "mal", 'untrusted')

    ax.set(xlim=(0, None), ylim=(0, 1.1))
    ax.grid(True, which='both')
    ax.axhline(y=0, color='k')
    ax.axvline(x=0, color='k')
    plt.xticks(data["xVals"], data["xVals"])
    ax.legend(loc='best',fontsize='small')

    plt.savefig(f'../data/graphs/{data["dimension"]}_untrusted.png')


def create_assess_graph(data):
    plt.style.use('_mpl-gallery')
    fig, ax = plt.subplots(figsize=(8, 6))
    fig.tight_layout()

    add_error_bar(ax, data["xVals"], data, "coop", 'assessed')
    add_error_bar(ax, data["xVals"], data, "mal", 'assessed')

    ax.set(xlim=(0, None), ylim=(0, None))
    ax.grid(True, which='both')
    ax.axhline(y=0, color='k')
    ax.axvline(x=0, color='k')
    plt.xticks(data["xVals"], data["xVals"])
    ax.legend(loc='best',fontsize='small')

    plt.savefig(f'../data/graphs/{data["dimension"]}_assessed.png')


def add_group(data, values):
    add_category(data['trusted'], values[:5])
    add_category(data['untrusted'], values[5:10])
    add_category(data['assessed'], values[10:])


def create_graph(file):
    data = {
        'xVals': [],
        'coop': {
            'trusted': {
                'vals': [],
                'errsLow': [],
                'errsHigh': []
            },
            'untrusted': {
                'vals': [],
                'errsLow': [],
                'errsHigh': []
            },
            'assessed': {
                'vals': [],
                'errsLow': [],
                'errsHigh': []
            }
        },
        "mal": {
            'trusted': {
                'vals': [],
                'errsLow': [],
                'errsHigh': []
            },
            'untrusted': {
                'vals': [],
                'errsLow': [],
                'errsHigh': []
            },
            'assessed': {
                'vals': [],
                'errsLow': [],
                'errsHigh': []
            }
        }
    }

    with open(file, 'r') as f:
        reader = csv.reader(f)
        for i, row in enumerate(reader):
            if i == 0:
                data['dimension'] = row[0]
            else:
                data['xVals'].append(row[0])
                row_vals = [float(x) for x in row]
                add_group(data['coop'], row_vals[21:36])
                add_group(data['mal'], row_vals[56:])

    create_trust_graph(data)
    create_untrust_graph(data)
    create_assess_graph(data)


def main():
    csvs = os.listdir('../data/processed')
    for f in csvs:
        if f.endswith('_lite.csv'):
            create_graph(f'../data/processed/{f}')

if __name__ == "__main__":
    main()