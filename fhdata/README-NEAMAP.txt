Hi Sarah,

Attached is a pretty complete version of that translator that we have been talking about (NEFSC-VIMS_SPCodes.rds)

 

I took the NEFSC species code list from the NOAA InPort site and used that as the base to compare with the VIMS species codes. If you all have a code for something and we do not, I listed the VIMSCODE as NA. If we have a code for something that you all don’t, I just left those off the list. I though that this approach would be more useful than having all of the VIMS codes, since its likely use will be to “add NEAMAP” to an NEFSC analysis.



Attached are the three lists that you had sent to me previously – benthivore, macrobenthosprey, megabenthosprey.

benthivore.csv, macrobenthosprey.csv, megabenthosprey.csv
 

For the benthivore file, I added the VIMS species codes for each of the predators (subset of the ‘translator’ file that I just sent) as well as a “NM_Diet” column. We don’t collect diet data on all of the predators that you had listed, so I used that column to indicate where we do (Y) or do not (N) collect diet data.

 

For both the megabenthosprey and macrobenthosprey files, I added a column that gives the VIMSCODE for each prey item. If we never encountered a particular prey type in a stomach before, that prey wouldn’t have a code so I gave that entry an “NA”. In short, these files show the prey types that are included in the NEAMAP “mean stomach weights” datasets (i.e., those with VIMSCODEs) that I’m going to send here in a minute.

 

I thought that these files could be useful when comparing the NEFSC and NEAMAP mean stomach weight datasets.

And finally…the actual data that you had requested! :)

NEAMAP_Mean Stomach Weights_Macrobenthos prey_wWQ.csv

NEAMAP_Mean Stomach Weights_Megabenthos prey_wWQ.csv

The formats of these datasets are nearly identical to those of the piscivore prey datasets that I sent back when we were working on that.

 

The meanmgbpreywt and meanmacpreywt columns give the mean megabenthos prey weight/stomach and mean macrobenthos prey weight/stomach, respectively. Also, since this is kind of a ‘fresh start’, I included SST directly in each of these datasets rather than providing it as a separate file. I thought that might make life a little easier.

 

If any of the column names don’t make sense or if any of the data look weird, let me know and I can investigate.

 

Thanks!

Jim