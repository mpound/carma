wavelength=raw_input("Which wavelength? (1mm or 3mm) : ")
namefile=open('fluxcalname'+wavelength+'.list','r')
donefile=open('fluxcaldone'+wavelength+'.list','r')
output=open('fluxcalleft'+wavelength+'.list','w')

donelist=[]
for done in donefile.readlines():
    donename = done.split()[0]
    donelist.append(donename)
#print donelist

for name in namefile.readlines():
    namename = name.split()[0]
#    print namename
    if namename in donelist: continue
#   else: output.write('%10s' %namename)
    else:
        print namename 
        output.write(namename+'\n')

namefile.close()
donefile.close()
output.close()

