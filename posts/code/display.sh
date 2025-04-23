#!/bin/bash

TMP=$(mktemp)
haltons=$(ghc 2025-04-16-unsure-calculator-fast.hs -e 'take 2000 $ zip (halton 2) (halton 3)')
mullers=$(ghc 2025-04-16-unsure-calculator-fast.hs -e 'take 2000 $ map (uncurry boxMuller) $ zip (halton 2) (halton 3)')
echo $points

cat <<EOF > $TMP
import matplotlib.pyplot as plt

# Given list of 2D points
haltons = $haltons
mullers = $mullers

# Separate x and y coordinates
x_vals, y_vals = zip(*haltons)
xm_vals, ym_vals = zip(*mullers)

# Plot
plt.figure(figsize=(6, 6))
plt.scatter(xm_vals, ym_vals, color='red', s=25)
# plt.scatter(x_vals, y_vals, color='blue', s=25)
plt.title('2D Point Plot')
plt.xlabel('X-axis')
plt.ylabel('Y-axis')
plt.grid(True)
plt.axis('equal')  # Keep the aspect ratio square
plt.show()
EOF

python3 $TMP
