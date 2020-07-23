import yt
import glob
save_dir = "png/"
flames = glob.glob("plt*")

for flame in flames:
    ds = yt.load(flame)
    sl = yt.LinePlot(ds, 'density', [0], [8.192e4], 10000)
    #sl = yt.SlicePlot(ds,3,'density')
    sl.save(save_dir + flame + ".png")
