#!/bin/bash

TMP=$(mktemp)
haltons=$(ghc 2025-04-16-unsure-calculator-fast.hs -e 'take 2000 $ zip (halton 2) (halton 3)')
mullers=$(ghc 2025-04-16-unsure-calculator-fast.hs -e 'take 2000 $ map (uncurry boxMuller) $ zip (halton 2) (halton 3)')
flat_mullers=$(ghc 2025-04-16-unsure-calculator-fast.hs -e 'take 100 $ boxMullers $ zip (halton 2) (halton 3)')
flat_mullers_sin=$(ghc 2025-04-16-unsure-calculator-fast.hs -e 'take 100 $ map (snd . uncurry boxMuller) $ zip (halton 2) (halton 3)')
echo $points

cat <<EOF > $TMP
import numpy as np
import matplotlib.pyplot as plt

# Given list of 2D points
haltons = $haltons
mullers = $mullers
flat_mullers_ys = $flat_mullers
flat_mullers_xs = list(range(len(flat_mullers_ys)))

flat_mullers_sin_ys = $flat_mullers_sin
flat_mullers_sin_xs = list(range(len(flat_mullers_ys)))

# Separate x and y coordinates
x_vals, y_vals = zip(*haltons)
xm_vals, ym_vals = zip(*mullers)

# control
samples = np.random.randn(100)

plt.figure(figsize=(6, 6))

# plt.scatter(xm_vals, ym_vals, color='red', s=25)
# plt.scatter(x_vals, y_vals, color='blue', s=25)
plt.plot(flat_mullers_xs, flat_mullers_ys, marker='o', linestyle='-', color='green')
# plt.plot(flat_mullers_sin_xs, flat_mullers_sin_ys, marker='o', linestyle='-', color='yellow')
plt.plot(samples, color='purple')

plt.title('2D Point Plot')
plt.xlabel('X-axis')
plt.ylabel('Y-axis')
plt.grid(True)
# plt.axis('equal')  # Keep the aspect ratio square
plt.show()
EOF

python3 $TMP
