import yt
import glob
import numpy as np
'''We track the flame by defining the 'front' as the highest value of nuclear energy generation
(H_nuc). (Schwab et. at al 2020 (arXiv:2001.0773) https://arxiv.org/abs/2001.07733 )'''

flames = np.sort(glob.glob("flame_0*"))

for flame in flames:
    ds = yt.load(flame)
    ad = ds.all_data()
    Nuclear_energy = ad['Hnuc']
    ds.print_stats()

    #print(Nuclear_energy[1])
    #print(np.max(Nuclear_energy))
