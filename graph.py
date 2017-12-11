import matplotlib.pyplot as plt
import numpy as np
import os

def courbe(Y, name, istick=True):
    X = [1,2,3,4,5,6]
    labels = ["hungarian", "bd-dump", "bd-near", "bd-stronger", "bd-stronger-target", "bd-individual"]

    _, ax = plt.subplots()
    width = 0.6
    ax.barh(X, Y, width, color="seagreen")
    ax.set_yticks(np.array(X)+width/2)
    ax.set_yticklabels(labels, minor=False)
    ax.set_xlim([0, np.max(Y)+(1/10)*np.max(Y)])
    if istick:
        ax.set_xlabel('Moyenne du nombre de ticks')
    else:
        ax.set_xlabel('Moyenne des distances parcourues')
    for i, v in enumerate(Y):
        ax.text(v+(1/50)*v, i + .25 + 1, str(v), color='black', fontweight='bold')
    plt.margins(y=0.1)
    plt.tight_layout()
    plt.savefig(name)
    plt.close()

#### moyenne des distances 20*20
Y = [1239, 1579, 1273, 1330, 1341, 1489]
courbe(Y, 'distance_20_20.png', False)

#### moyenne des ticks 20*20
Y = [108, 132, 112, 131, 144, 210]
courbe(Y, 'ticks-20-20.png')

####################################

#### moyenne des distances 20*80

Y = [690, 1957, 821, 946, 1138, 1518]
courbe(Y,'distance_20_80.png', False)

#### moyenne des ticks 20*80

Y = [90, 177, 125, 167, 266, 473]
courbe(Y,'ticks-20-80.png')

####################################

#### moyenne des distances 50*20

Y = [2894, 3890, 3112, 3190, 3281, 3467]
courbe(Y,'distance_50_20.png', False)

#### moyenne des ticks 50*20

Y = [111, 138, 119, 139, 155, 250]
courbe(Y,'ticks-50-20.png')

####################################

#### moyenne des distances 50*80

Y = [1403, 4875, 1778, 2011, 2304, 3020]
courbe(Y,'distance_50_80.png', False)

#### moyenne des ticks 50*80

Y = [73, 187, 139, 178, 289, 603]
courbe(Y, 'ticks-50_80.png')

