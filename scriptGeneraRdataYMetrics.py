#!/usr/bin/env bash

from os import mkdir
import shutil
from os import system

#Pres = ['1stPres', '2ndPres', '3rdPres']
Pres = ['L0','L1', 'L2']

Alg = ['N', 'SR']
       
targ = ['ACOTSP-STNs','MMASQAP-STNs']

for precision in Pres:
    for tar in targ:
        mergepath = tar + "/" + precision + "_Merged"
        mkdir(mergepath)
        for al in Alg:
            workpath = tar + "/" + al + "_" + precision + "_STN"
            comando = 'Rscript create.R {}'
            cmd = comando.format(workpath)
            system(cmd)

            
            metpath = workpath + "-stn"
            filepath = metpath + "/*.RData"
            comando = "cp {} {}"
            cmd = comando.format(filepath, mergepath)
            print(cmd)
            system(cmd)
            
            name = tar + '_' + al + '_' + precision + '.csv'
            comando = 'Rscript metrics-alg.R {} {}'
            cmd = comando.format(metpath,name)
            system(cmd)

        

        comando = 'Rscript merge.R {}'
        comando = comando.format(mergepath)
        print(comando)
        system(comando)
        
        comando = 'Rscript metrics-merged.R {} 1 {}'
        comando = comando.format(mergepath+'-merged.RData','Merged_'+tar+"_"+precision+".csv")
        print(comando)
        system(comando)
        #cont += 1
        
