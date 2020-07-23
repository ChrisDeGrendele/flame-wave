import yt
import glob

save_dir = "png/"

flames = glob.glob("flame_0*")
off_flames = ["flame_after_DivuIter", "flame_after_InitProj", "flame_InitData"]
smallflames = glob.glob("smallflame_*")

flames.extend(off_flames)
flames.extend(smallflames)

for flame in flames:
    ds = yt.load(flame)
    sl = yt.SlicePlot(ds,2,'rho')
    sl.save(save_dir + flame + ".png")
