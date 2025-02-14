# -*- coding: mbcs -*-
#
# Abaqus/CAE Release 2023.HF4 replay file
# Internal Version: 2023_07_21-20.45.57 RELr425 183702
# Run by nguyenb5 on Fri Feb 14 14:08:06 2025
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Abaqus Warning: Unknown keyword (current_os) in environment file.
#: Abaqus Warning: Please check spelling of the environment variable names.
#:                 Unknown keyword "keywordname" can be removed using "del keywordname"
#:                 at the end of environment file.
#: Executing "onCaeGraphicsStartup()" in the site directory ...
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=234.03645324707, 
    height=121.741897583008)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from caeModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
#: Abaqus Warning: Unknown keyword (current_os) in environment file.
#: Abaqus Warning: Please check spelling of the environment variable names.
#:                 Unknown keyword "keywordname" can be removed using "del keywordname"
#:                 at the end of environment file.
openMdb('elastic_hole_plate_3D.cae')
#: The model database "C:\LocalUserData\User-data\nguyenb5\Hydrogen-Embrittlement-Subroutine-Aachen\elastic_plate_3D_v2\elastic_hole_plate_3D.cae" has been opened.
session.viewports['Viewport: 1'].setValues(displayedObject=None)
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=ON)
p = mdb.models['elastic_plate_2D'].parts['elastic_hole_plate']
session.viewports['Viewport: 1'].setValues(displayedObject=p)
a = mdb.models['elastic_plate_2D'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    adaptiveMeshConstraints=ON, optimizationTasks=OFF, 
    geometricRestrictions=OFF, stopConditions=OFF)
mdb.models['elastic_plate_2D'].steps['Step-1'].control.setValues(
    allowPropagation=OFF, resetDefaultValues=OFF)
session.viewports['Viewport: 1'].setValues(displayedObject=None)
o1 = session.openOdb(
    name='C:/LocalUserData/User-data/nguyenb5/Hydrogen-Embrittlement-Subroutine-Aachen/elastic_plate_3D_v2/elastic_plate_3D_processed.odb')
session.viewports['Viewport: 1'].setValues(displayedObject=o1)
#: Model: C:/LocalUserData/User-data/nguyenb5/Hydrogen-Embrittlement-Subroutine-Aachen/elastic_plate_3D_v2/elastic_plate_3D_processed.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       7
#: Number of Node Sets:          6
#: Number of Steps:              1
session.viewports['Viewport: 1'].odbDisplay.display.setValues(plotState=(
    CONTOURS_ON_DEF, ))
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.117512, 
    farPlane=0.210749, width=0.0805066, height=0.0348283, 
    viewOffsetX=-0.000345513, viewOffsetY=-0.00279842)
session.odbs['C:/LocalUserData/User-data/nguyenb5/Hydrogen-Embrittlement-Subroutine-Aachen/elastic_plate_3D_v2/elastic_plate_3D_processed.odb'].close(
    )
