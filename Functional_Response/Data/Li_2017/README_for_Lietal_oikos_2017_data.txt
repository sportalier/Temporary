Dataset 'Lietal_oikos_2017.csv' contains the data used in the publication 'Experimental duration and predator satiation levels systematically affect functional response parameters' first published in Oikos http://dx.doi.org/10.1111/oik.04479

This dataset contains 23 columns and 451 rows. 

Please contact Yuanheng Li (li_yuanheng_eco@126.com) or Bjoern Rall (bjoern.rall@idiv.de) for further quesions about the dataset.

definitions of column headings: 
'original.publication.short': the short name of the paper where the specific row of data comes from - referred to as "publication.short" in the R-code of Li et al., Appendix II;
'setting': the setting of functional response study, if it's a laboratory ,"lab", or green house, "greenhouse" or field experiment, "field";
'ecosystem.type': from which ecosystem type the studied species come from, can be "freshwater", "marine" or "terrestrial" - needed to build the factorial "eco.met" in the R-code and referred to as "habitat" in the according text of Li et al., Appendix II;
'dimensionality': if the feeding interaction belongs to two-dimensional, "2D", or three-dimensional, "3D", space- referred to as "search.space" in the R-code of Li et al., Appendix II;
'arena.size': the size of the experimental unit where the functional response study is conducted, units for 2D in square meters, for 3D in cubic meters;
'experimental.duration.seconds': how many seconds does a single trial of a given the functional response study last, in seconds [s];
'temperature.degree.celcius': the environmental (arena) temperature when the study is conducted, all in degree Celcius [°C];
'predator.species': the latin scientific name of the predator;
'predator.met.group': three groups describing the predator: "invert" (invertebrate predator), "ectovert" (ectotherm vertebrate predator), "unicell" (unicell predator) - needed to build the factorial "eco.met" in the R-code and referred to as "metabolic type" in the according text of Li et al., Appendix II;
'predator.ana.group': the vertebral column status of the predator, can be "invertebrate" or "vertebrate";
'consumption.type': the resource type of the predator, can be "bacterivore", "carnivore", "fungivore" or "herbivore";
'predator.mass.mg': the average predator weight in milligrams [mg];
'prey.species': the latin scientific name of the prey;
'prey.mass.mg': the average prey weight in milligrams [mg];
'attack.rate': the attack rate measured, either in [m^2/s] for 2D or [m^3/s] for 3D;
'handling.time': the handling time measured, [s/mg], how long does a predator individual need to handle 1 mg prey;
'Starvation.Y.N': if the predator is starved before the functional resonponse study, the entry is given "Y". if not, given "N" - referred to as "Starve" in the R-code of Li et al., Appendix II;
'Starvation.time.seconds': for how many seconds does the used predator/predators is/are starved before the study, in [s];
'original.publication.full': the full reference of the original paper;
'meta.data.from': from where the data are from other than the part about starvation, 'Rall.2012.Phil.Trans.R.Soc.B' refers to 'Rall, B. C., Brose, U., Hartvig, M., Kalinkat, G., Schwarzmuller, F., Vucic-Pestic, O. and Petchey, O. L. 2012. Universal temperature and body-mass scaling of feeding rates. – Philosophical Transactions of the Royal Society B: Biological Sciences 367(1605): 2923–2934.' and 'Hansen.1997.LimnolOceanogr' refers to 'Hansen, P. J., Bjørnsen, P. K. and Hansen, B. W. 1997. Zooplankton grazing and growth: Scaling within the 2-2,000-μm body size range. – Limnology and Oceanography 42(4): 687–704.';
