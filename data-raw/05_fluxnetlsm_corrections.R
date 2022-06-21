# Run this code after processing data through FluxnetLSM and adding
# LAI time series from MODIS and Copernicus

source("R/met_corrections.R")
source("R/flux_corrections.R")
source("R/energy_balance_correction.R")
source("R/gapfill_postprocess.R")

#Output path
outpath <- "data/fluxnetlsm_corrected/"

###########################
### Read QC information ###
###########################

# Read QC information from google sheet,
# e.g. start and end year and CO2 processing

url <- "https://docs.google.com/spreadsheets/d/1bi9bbUpwzRycDJ16VTYGx8ApkDLV_IeSwEsdDJsX1SI/edit?usp=sharing"
qc_info <- gsheet2tbl(url)
qc_sites <- qc_info$Site_code


#####################
### Read CO2 data ###
#####################

# Use Mauna Loa annual CO2 record to gapfill CO2 records that are
# poor quality or missing
global_co2 <- read.table(paste0(path, "/Global_CO2_data/co2_annmean_mlo.txt"))


# Set new QC values for post-processing
# Use 101 as OzFlux using a lot of smaller values
new_qc <- 101


######################
### Load site data ###
######################

#Met data

#Get sites
met_files <- list.files(paste0(path, "/all_sites_no_duplicates/Nc_files/Met_with_LAI/"), full.names=TRUE)

#Open file handles
met_nc <- lapply(met_files, nc_open, write=TRUE)


#Flux data

#Get sites
flux_files <- list.files(paste0(path, "/all_sites_no_duplicates/Nc_files/Flux/"), full.names=TRUE)

#Open file handles
#flux_nc <- lapply(flux_files, nc_open, write=TRUE)

#Get site codes
site_codes <- sapply(met_nc, function(x) ncatt_get(x, varid=0, "site_code")$value)

#Close met file handles
lapply(met_nc, nc_close)


#Create output directories

#Met
outdir_met <- paste0(outpath, "/Nc_files/Met/")

#unlink(outdir_met, recursive = TRUE) #remove if already exists
dir.create(outdir_met, recursive=TRUE)

#Flux
outdir_flux <- paste0(outpath, "/Nc_files/Flux/")

#unlink(outdir_flux, recursive = TRUE) #remove if already exists
dir.create(outdir_flux, recursive=TRUE)

#Plot
outdir_plot <- paste0(outpath, "/Diagnostic_plots/")

#unlink(outdir_plot, recursive = TRUE) #remove if already exists
dir.create(outdir_plot, recursive=TRUE)


#Create output file names
#Met
outfiles_met <- sapply(met_files, function(x)paste0(outdir_met, "/", basename(x)))

#Flux
outfiles_flux <- sapply(flux_files, function(x)paste0(outdir_flux, "/", basename(x)))



### Check which sites to process ###

if (any(!(site_codes %in% qc_info$Site_code))) {
  stop("Site missing from google sheet")
}


status <- unlist(sapply(site_codes, function(x) qc_info$decision[which(qc_info$Site_code == x)]))





#Then find sites to process (i.e. decision is not kill)
good_sites <- which(status != "kill")

#Get info for good sites
qc_info_list <- lapply(site_codes, function(x) qc_info[which(qc_info$Site_code ==x),])



#Initialise parallel cores
cl <- makeCluster(getOption('cl.cores', 12))

clusterExport(cl, list('qc_info_list', 'new_qc','global_co2',
                       'outdir_met','outdir_flux', 'outdir_plot'))

clusterExport(cl, list('met_corrections', 'flux_corrections','energy_balance_correction',
                       'hrs', 'add_months', 'SynthesizeLWdown','site_exceptions','VPD2RelHum',
                       'SpecHumidity2Rel','calc_esat','linear_pred_co2','diagnostic_plot',
                       "na.approx"))

clusterExport(cl, list('Timeseries', 'GetTimingNcfile', 'FindTimeVarName', 'GetTimeUnits',
                       'GetNumberTimesteps','GetTimestepSize','Yeardays','is.leap'))


clusterEvalQ(cl, library(ncdf4))
clusterEvalQ(cl, library(chron))



#######################
### Met corrections ###
#######################


#Met corrections

print("######## Starting met corrections ########")


clusterMap(cl, function(met, out, qc) met_corrections(infile_met=met, outfile_met=out, outdir=outdir_met, #passing outdir to be able to alter filename for years
                                                      qc_info=qc, new_qc=new_qc, global_co2=global_co2),
           met=met_files[good_sites], out=outfiles_met[good_sites],
           qc=qc_info_list[good_sites])

# #
# # #
# # # # For testing individual site:
# s=152 #s=28
# met_corrections(infile_met=met_files[s], outfile_met=outfiles_met[s], outdir=outdir_met,
#                  qc=qc_info_list[[which(qc_sites %in% site_codes[s])]],
#                 new_qc=new_qc, global_co2=global_co2)

#
# mapply(function(met, out, qc) met_corrections(infile_met=met, outfile_met=out,
#                                                       qc_info=qc, new_qc=new_qc, global_co2=global_co2),
#            met=met_files[good_sites], out=outfiles_met[good_sites],
#            qc=qc_info_list[good_sites])


########################
### Flux corrections ###
########################


print("######## Starting flux corrections ########")


#Flux corrections
clusterMap(cl, function(flx, out, qc) flux_corrections(infile_flux=flx, outfile_flux=out,
                                                       outdir=outdir_flux, #passing outdir to be able to alter filename for years
                                                       qc_info=qc, new_qc=new_qc),
           flx=flux_files[good_sites], out=outfiles_flux[good_sites],
           qc=qc_info_list[good_sites])


#
# mapply(function(flx, out, qc) flux_corrections(infile_flux=flx, outfile_flux=out,
#                                                outdir=outdir_flux,
#                                                        qc_info=qc, new_qc=new_qc),
#            flx=flux_files[good_sites], out=outfiles_flux[good_sites],
#            qc=qc_info_list[which(qc_sites %in% site_codes[good_sites])])
#


#############################################
#########------ Plot new data ------#########
#############################################

print("######## Plotting data ########")

#Find output files
met_out_files  <- list.files(outdir_met, full.names=TRUE)
flux_out_files <- list.files(outdir_flux, full.names=TRUE)


#Check that site codes match
if (length(met_out_files) != length(flux_out_files)) {
  stop("different number of met and flux files")
}


#Open file handles
met_nc <- lapply(met_out_files, nc_open, write=TRUE)
flux_nc <- lapply(flux_out_files, nc_open, write=TRUE)

#Get site codes
met_site_codes  <- sapply(met_nc, function(x) ncatt_get(x, varid=0, "site_code")$value)
flux_site_codes <- sapply(met_nc, function(x) ncatt_get(x, varid=0, "site_code")$value)


#Check that site codes match
if (!all(flux_site_codes == met_site_codes)) {
  stop("met and flux sites don't match")
}


#Close met file handles
lapply(met_nc, nc_close)
lapply(flux_nc, nc_close)



#Create plots
clusterMap(cl, function(site, met_file, flux_file) diagnostic_plot(site_code=site, outdir=outdir_plot,
                                                                   met_file=met_file, flux_file=flux_file),
           site=met_site_codes, met_file=met_out_files, flux_file=flux_out_files)




# #Create plots
# mapply(function(site, met_file, flux_file) diagnostic_plot(site_code=site, outdir=outdir_plot,
#                                                                    met_file=met_file, flux_file=flux_file),
#            site=met_site_codes, met_file=met_out_files, flux_file=flux_out_files)
#


stopCluster(cl)



