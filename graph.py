import matplotlib.pyplot as plt

def courbe(Y, name):
    X = [1,2,3,4]
    labels = ["hungarian", "dump", "near", "stronger"]

    plt.bar(X, Y)
    plt.xticks(X, labels)
    for x, y in zip(X,Y):
        plt.text(x, y, str(y))
    plt.savefig(name)
    plt.close()    

#### moyenne des distances 20*20
Y = [1239, 1579, 1273, 1330]
courbe(Y, 'distance_20_20.png')


#### moyenne des ticks 20*20
Y = [108, 132, 112, 131]
courbe(Y, 'ticks-20-20.png')
#### moyenne des distances 20*80

Y = [690, 1957, 821, 946]
courbe(Y,'distance_20_80.png')

#### moyenne des ticks 20*80

Y = [90, 177, 125, 167]
courbe(Y,'ticks-20-80.png')
#### moyenne des distances 50*20

Y = [100000, 3890, 3112, 3190]
courbe(Y,'distance_50_20.png')


#### moyenne des ticks 50*20

Y = [100000, 138, 119, 139]
courbe(Y,'ticks-50-20.png')

#### moyenne des distances 50*80

Y = [100000, 4875, 1778, 2011]
courbe(Y,'distance_50_80.png')

#### moyenne des ticks 50*80

Y = [100000, 187, 139, 178]
courbe(Y, 'ticks-50_80.png')

