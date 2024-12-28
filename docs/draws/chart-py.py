import matplotlib.pyplot as plt
import numpy as np
x = np.array([0, 1, 2, 3, 4])
y = np.array([5, 4, 3, 2, 1])
plt.bar(x, y, color='g')
plt.savefig('draws_out/chart-py.svg')
