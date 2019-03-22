# measo-access


Andrew C's [MEASO Biota-Habitatssoki page](http://soki.aq/display/MEASO/MEASO+Biota-Habitats) outlines these aims, and a long list of possibly relevant variables. 

In DecadalProto we used the last 10yrs of SST, EKE, SSH, ICE-Season, and Spring/Summer CHLA

>The aim here is to summarise the nature of habitats in the Southern Ocean and how they may vary in space and time.  Habitat metrics reported here are those that are considered to represent spatio-temporal scales of primary importance to biota in the Southern Ocean.  Some metrics are comparatively unchanging, e.g. bathymetry, while others will need to be reported seasonally.  In space, finer grain reporting occurs when taxa are restricted in their distributions, while coarser grain reporting, e.g. whole sectors, is suitable for widely distributed taxa. 

>Where possible, consideration will be given to changes that have occurred since the industrial revolution.  As per the IPCC, we use 1750 as marking the beginning of the industrial period.

>Methods

>A habitat assessment has the following steps (here, the focus is on physical factors):

>*Identify important physical parameters that can be summarised as an attribute of habitat of one or more species
>*Identify the attributes to be derived from one or more parameters that are important to the species e.g. temperature thresholds important to the success of species
>*Identify the quantity/quantities for expressing the status and trends of the attributes
>*Establish links to the appropriate datasets and extract the quantities for presentation



## Data on RAATD

Early extraction products are in 

"/mnt/home/<someuser>/RAATD_01/RAATDfuture/Working/Data/Climate"

Use these to convert into the DecadalProto form. 



## Meeting 23 January 2019

Andrew Lenton CSIRO, AConstable, SKawaguchi, JMelb-Thomas, Rowan Trebilico, MSumner

Goals

* habitat projections, assessment summary stats, interest especially in extreme differentiation, e.g. west ant ice shelf, also east ant is a big unknown
* krill projections, ocean acidification and application of a krill model
* time slice stuff with Atlantis

Summary

- lcol is likely to be a good start for Atlantis
- 



Q

- navigate outputs, on raijin? sqlite db?  catalogue?


what scenarios? 

ACCESS-ESM

1.5 looking very unlikely

8.5 is a 1/10 deg ocean model

probably CMIP5 fit for purpose in the next year

mark3-lcol - steven phipps model, coupled to carbon cycle (two degree resolution)
- runs in about a week and a half
- no very high resolution stuff 
- doesn't represent sea ice better, Will Hobbs a better connection for that
- most carbon cycle runs have been 2.6 or 3.0 
- for So, go to 1.5 degrees, assume that continues

Tool - ecm val
- take the cmip6 output and produce a whole lot of output
- could incorporate our diagnostic approach ?
- Andrew L also keen to align to standards for output, e.g. COARDs etc. 


There is a summer bias in SO in Access, uses UM, lots of techniques been tried to overcome - the higher res stuff is better
- Andrew L happy to set up a pipeline to output stuff and dump it somewhere
- 5th or 10th percentile approach is prob best, no model is great over the shelf

Andrew Meijers has a published list of best products, and S Corney

Lot of value in overshoot space, compelling arguments for mitigating now

