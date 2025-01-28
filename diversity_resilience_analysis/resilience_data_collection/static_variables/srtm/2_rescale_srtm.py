import rasterio as rio
from rasterio.windows import Window
import rasterio.windows
import numpy as np

# import initialise_py.py
current_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(os.path.join(current_dir, "main"))
import initialise_py

# use variables from initialise_py
root_project = initialise_py.root_project
root_data_input = initialise_py.root_data_input

width = 221800
height = 146400
window_size = 200

o = np.empty((height // 200, width // 200), dtype=np.float64)
r = rio.open(os.path.join(root_data_input, 'static_variables/srtm/SRTM_000025/srtmElevation_000025_lzw.tif'))

for i in range(o.shape[0]):
    for j in range(o.shape[1]):
        print(f"{(i * o.shape[1] + j + 1) / np.prod(o.shape) * 100:.1f}%")
        window = Window(window_size * j, window_size * i, window_size, window_size)
        #o[i, j] = np.nanstd(r.read(1, window=window))
        o[i, j] = np.nanmean(r.read(1, window=window))

bounds = r.window_bounds(Window(0, 0, width, height))
transform = rasterio.transform.from_bounds(*bounds, o.shape[1], o.shape[0])

with rio.open(
	      os.path.join(root_data_input, 'static_variables/srtm/SRTM_005/elevation_mean.tif'),
              mode='w',
              compress='LZW',
              transform=transform,
              width=o.shape[1],
              height=o.shape[0],
              count=1,
              dtype=np.float64,
              crs=r.crs) as dst:
    dst.write(o, 1)

r.close()