# -*- coding: mbcs -*-
#
# Abaqus/CAE Release 2023.HF4 replay file
# Internal Version: 2023_07_21-20.45.57 RELr425 183702
# Run by nguyenb5 on Thu Feb  6 12:53:00 2025
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=253.52082824707, 
    height=147.207168579102)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from caeModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
openMdb('CP1000_all_geometries_denser_mesh.cae')
#: The model database "C:\LocalUserData\User-data\nguyenb5\Hydrogen-Embrittlement-Subroutine-Aachen\CP1000_all_geometries_denser_mesh.cae" has been opened.
session.viewports['Viewport: 1'].setValues(displayedObject=None)
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=ON)
p = mdb.models['CHD2_combined'].parts['CHD2']
session.viewports['Viewport: 1'].setValues(displayedObject=p)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.460679, 
    farPlane=0.625012, width=0.0313092, height=0.0190787, 
    viewOffsetX=0.00524195, viewOffsetY=-0.0123855)
mdb.Model(name='CHD2_combined_coarse', 
    objectToCopy=mdb.models['CHD2_combined'])
#: The model "CHD2_combined_coarse" has been created.
p = mdb.models['CHD2_combined_coarse'].parts['CHD2']
session.viewports['Viewport: 1'].setValues(displayedObject=p)
