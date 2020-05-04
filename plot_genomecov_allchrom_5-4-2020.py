import sys
import os
import numpy as np
import matplotlib.pyplot as plt
import datetime
import time

#modified from MBA's original script

'''
call as: python plot_genomecov.py file.fastq_pooled_reads
input: bedgraph -d alignment(s) to reference genome
output: aligned plots
'''

pooled_read_files = sys.argv[1:]

def ParseFromFile(pooled_reads):
    '''
    Input: _pooled_reads outfile from rh-seq pipeline
     tsv of: ID,scaffold,strand,location,annotation,n,rel_loc rel_prop,gene_length
    Output: {'chrom':[[positions],[coverages]]
    '''
    coverage_dict={}
    #print("coverage_dict",coverage_dict)
        
    #add position and coverage for each base to a dictionary for that chromosome
    f = open(pooled_reads)
    next(f)
    for line in f:
        row_data = line.strip().split("\t")
        print(row_data, "row_data")
        chromosome = row_data[1]
        print(chromosome, "chromosome")
        if chromosome not in coverage_dict:
            coverage_dict[chromosome] = [],[]
        pos = int(row_data[3])
        print(pos, "pos")
        cov = int(row_data[5]) #coverage "n" 
        print(cov, "cov")
        exit()
        allele = chromosome[:2]
        #print("allele", allele)
        coverage_dict[chromosome][0].append(pos)
        coverage_dict[chromosome][1].append(cov)
        #coverage_dict[chromosome][2].append(allele)
        #print("coverage_dict",coverage_dict)
    return coverage_dict



def plotCov(chromosome_vals, saveName):

    figName=saveName+".pdf"

    fig=plt.figure(figsize=(20,15))
    pl = plt.subplot(111)

    for chromosome in coverage_dict:
        #print("chromosome",chromosome)
        chromosome_vals=coverage_dict[chromosome]
        #print("chromosome_vals",chromosome_vals)
        allele=chromosome[:2]
        chr_x=np.array(chromosome_vals[0])
        chr_y=np.array(chromosome_vals[1])
        if allele == "sp":
            pl.scatter(chr_x,chr_y, s=300, label = allele, color = '#fdae61') #orange
        else: 
            pl.scatter(chr_x,chr_y, s=300,label = allele, color = '#3288bd') #blue
    
    pl.set_xlabel('position',fontsize=50)
    pl.set_ylabel('coverage',fontsize=50)

    # Hide the right and top spines
    pl.spines['right'].set_visible(False)
    pl.spines['top'].set_visible(False)

    plt.ylim(0,700) #set ylim to see bulk of data points
    plt.legend(['Scer','Spar'],markerscale = 1, frameon=False, prop={'size': 20}, bbox_to_anchor=(1.1, 0.5), loc='center right')
    plt.tight_layout()
    plt.title(saveName)
    plt.savefig(figName, bbox_inches='tight', format='pdf', dpi=1000)

    plt.clf()
    return None



##RUN###
now = datetime.datetime.now()

for pooled_read_file in pooled_read_files:
    saveName =pooled_read_file.split('.')[0]+"_allchr_"+now.strftime("%m-%d-%Y_%H-%M")

    
    #Parse
    coverage_dict=ParseFromFile(pooled_read_file)

    #Plot
    plotCov(coverage_dict,saveName)


