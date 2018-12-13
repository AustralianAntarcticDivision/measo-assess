## Data composites

Develop monthly climatologies for a subset of variables for the first (and then last) decade of the RCP8.5 scenario.

 

The ensemble member we want is RCP-85-10F. The files each contain six months of output (the file name date = end date of ea 6m segment); so there are 20 files per decade. The start and end dates look like say

* ocean_bgc.nc-20060630 (file 1) to ocean_bgc.nc-20151231 (file 20) for the first decade,  and then equivalently
* ocean_bgc.nc-20910630 (file 1) to ocean_bgc.nc-21001231 (file 20) for the final decade of the run

 We would be trawling across three lodged outputs, two of which – ocean and BGC - are located in:

/g/data1a/p66/txz599/ACCESS/archive/RCP-85-10F/history/ocn/

(NB The BGC files are 1.5GB each whereas the ocean_month files are 9GB each).


In ocean_bgc.nc-YYYMMDD we would get say ‘phyc’ and ‘zooc’. These are typically only valid in upper ~200m of the vertical layers, but a sum(all layers) would suffice.

In ocean_month.nc-YYYMMDD we would get say ‘tos’ (single sea surface temperature layer)

Be good to crop out whole Southern Ocean spatially, and 6 time slices of one or two single-layer variables would not be a big size for export
