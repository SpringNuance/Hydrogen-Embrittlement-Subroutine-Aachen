# -*- coding: mbcs -*-
#
# Abaqus/Viewer Release 2023.HF4 replay file
# Internal Version: 2023_07_21-20.45.57 RELr425 183702
# Run by nguyenb5 on Thu Feb  6 19:57:06 2025
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=234.03645324707, 
    height=147.207168579102)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from viewerModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
o2 = session.openOdb(name='Axisymmetric_60x300_Reduced_HYDROGEN.odb')
#: Model: C:/LocalUserData/User-data/nguyenb5/Professor-Aravas-Materials/3. Professor Aravas's code/Axisymmetric_60x300_Reduced_HYDROGEN.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       3
#: Number of Node Sets:          7
#: Number of Steps:              7
session.viewports['Viewport: 1'].setValues(displayedObject=o2)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0279505, 
    farPlane=0.0420251, width=0.00465669, height=0.0028282, 
    viewOffsetX=0.000306172, viewOffsetY=-0.0043198)
session.viewports['Viewport: 1'].odbDisplay.display.setValues(plotState=(
    CONTOURS_ON_DEF, ))
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0281641, 
    farPlane=0.0418115, width=0.00348812, height=0.00211849, 
    viewOffsetX=-0.000345494, viewOffsetY=-0.0044575)
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=0 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=0 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=0 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=0 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=0 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=0 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=0 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=0 )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0280668, 
    farPlane=0.0419087, width=0.00452815, height=0.00275014, 
    viewOffsetX=-0.000157793, viewOffsetY=-0.00434875)
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=1 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=1 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=1 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=1 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=1 )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0273453, 
    farPlane=0.0426302, width=0.00796427, height=0.00483704, 
    viewOffsetX=-0.000807326, viewOffsetY=-0.00361308)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0246436, 
    farPlane=0.0427124, width=0.00717741, height=0.00435915, cameraPosition=(
    -0.019215, 0.0202015, 0.0224674), cameraUpVector=(0.0363585, 0.87015, 
    -0.491444), cameraTarget=(0.0014689, 0.00567386, -0.00172492), 
    viewOffsetX=-0.000727563, viewOffsetY=-0.00325611)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0229965, 
    farPlane=0.0418778, width=0.00669771, height=0.00406781, cameraPosition=(
    0.00882593, 0.0193524, 0.0286623), cameraUpVector=(-0.0924909, 0.919973, 
    -0.380914), cameraTarget=(-5.50287e-05, 0.00564769, -0.00228048), 
    viewOffsetX=-0.000678936, viewOffsetY=-0.00303849)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0233995, 
    farPlane=0.0415017, width=0.00681509, height=0.0041391, cameraPosition=(
    0.0131413, 0.0171684, 0.0281664), cameraUpVector=(0.0294846, 0.931195, 
    -0.363326), cameraTarget=(0.000133399, 0.0057207, -0.00222919), 
    viewOffsetX=-0.000690835, viewOffsetY=-0.00309174)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0260808, 
    farPlane=0.0423636, width=0.00759601, height=0.00461339, cameraPosition=(
    -0.0075291, 0.0105036, 0.0327425), cameraUpVector=(-0.278713, 0.945533, 
    -0.168188), cameraTarget=(0.000330871, 0.00679198, -0.00114836), 
    viewOffsetX=-0.000769996, viewOffsetY=-0.00344601)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0266185, 
    farPlane=0.0436735, width=0.00775261, height=0.0047085, cameraPosition=(
    -0.00359017, 0.00106927, 0.0344416), cameraUpVector=(0.175024, 0.966635, 
    0.187041), cameraTarget=(0.00237745, 0.00657388, 0.000408795), 
    viewOffsetX=-0.00078587, viewOffsetY=-0.00351705)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0264978, 
    farPlane=0.0419166, width=0.00771745, height=0.00468715, cameraPosition=(
    0.00865407, 0.00708868, 0.0334496), cameraUpVector=(-0.0882567, 0.99594, 
    0.0177268), cameraTarget=(0.00121906, 0.00703824, -0.000738997), 
    viewOffsetX=-0.000782306, viewOffsetY=-0.0035011)
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='RF', outputPosition=NODAL, refinement=(INVARIANT, 
    'Magnitude'), )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0273255, 
    farPlane=0.0410888, width=0.00277633, height=0.00168618, 
    viewOffsetX=-0.00073639, viewOffsetY=-0.00563116)
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0248884, 
    farPlane=0.043526, width=0.0175505, height=0.0106592, 
    viewOffsetX=-0.00140148, viewOffsetY=-0.0045496)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0182278, 
    farPlane=0.0397805, width=0.0128537, height=0.00780662, cameraPosition=(
    0.00708868, 0.0344299, 0.00497936), cameraUpVector=(-0.602127, 0.405101, 
    -0.687995), cameraTarget=(-0.00247111, 0.0024427, -0.00548837), 
    viewOffsetX=-0.00102642, viewOffsetY=-0.00333205)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.019859, 
    farPlane=0.0381493, width=0.00857135, height=0.00520575, 
    viewOffsetX=-0.000898804, viewOffsetY=-0.00295245)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0212754, 
    farPlane=0.0358355, width=0.00918267, height=0.00557703, cameraPosition=(
    0.0237694, 0.0075646, 0.0183274), cameraUpVector=(0.247209, 0.957516, 
    -0.148493), cameraTarget=(-0.00166804, 0.0104331, -0.00552326), 
    viewOffsetX=-0.000962908, viewOffsetY=-0.00316302)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0223333, 
    farPlane=0.0347777, width=0.00229048, height=0.00139111, 
    viewOffsetX=0.000535195, viewOffsetY=-0.00788451)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.022358, 
    farPlane=0.034753, width=0.00229302, height=0.00139265, cameraPosition=(
    0.0222004, 0.00779137, 0.020028), cameraUpVector=(0.0942612, 0.995363, 
    0.0191821), cameraTarget=(-0.003237, 0.0106599, -0.00382265), 
    viewOffsetX=0.000535787, viewOffsetY=-0.00789324)
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0223799, 
    farPlane=0.0347317, width=0.0020281, height=0.00123175, 
    viewOffsetX=0.000500175, viewOffsetY=-0.00793162)
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.021975, 
    farPlane=0.0351373, width=0.00477436, height=0.00289967, 
    viewOffsetX=0.000482995, viewOffsetY=-0.00748128)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0211579, 
    farPlane=0.0366694, width=0.00459684, height=0.00279185, cameraPosition=(
    0.0136797, 0.00671096, 0.0265391), cameraUpVector=(0.186135, 0.981521, 
    0.0443907), cameraTarget=(-0.000389934, 0.0108159, -0.00523099), 
    viewOffsetX=0.000465036, viewOffsetY=-0.0072031)
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='SDV_U', outputPosition=INTEGRATION_POINT, )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0211794, 
    farPlane=0.0366479, width=0.00493536, height=0.00299746, 
    viewOffsetX=0.000407212, viewOffsetY=-0.00724849)
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=0 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=1 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0214563, 
    farPlane=0.036371, width=0.00286491, height=0.00173998, 
    viewOffsetX=0.000939013, viewOffsetY=-0.0072335)
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=2 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=3 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setPrimaryVariable(
    variableLabel='U', outputPosition=NODAL, refinement=(INVARIANT, 
    'Magnitude'), )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0179883, 
    farPlane=0.039839, width=0.0220216, height=0.0133746, 
    viewOffsetX=-0.00159526, viewOffsetY=-0.003559)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0196953, 
    farPlane=0.0408798, width=0.0241114, height=0.0146439, cameraPosition=(
    0.0005093, 0.00729289, 0.030529), cameraUpVector=(0.202274, 0.974015, 
    0.10188), cameraTarget=(0.00273929, 0.0104668, -0.00424308), 
    viewOffsetX=-0.00174665, viewOffsetY=-0.00389675)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0172183, 
    farPlane=0.0402141, width=0.0210791, height=0.0128022, cameraPosition=(
    0.0126298, 0.015949, 0.0249694), cameraUpVector=(0.0779904, 0.968838, 
    -0.235097), cameraTarget=(-0.00015618, 0.00924455, -0.00690088), 
    viewOffsetX=-0.00152698, viewOffsetY=-0.00340668)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0172951, 
    farPlane=0.040166, width=0.0211731, height=0.0128593, cameraPosition=(
    0.0110091, 0.0188186, 0.0243643), cameraUpVector=(-0.007002, 0.952922, 
    -0.303135), cameraTarget=(-0.000333452, 0.00870932, -0.00715233), 
    viewOffsetX=-0.00153379, viewOffsetY=-0.00342187)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0181423, 
    farPlane=0.0393189, width=0.0153222, height=0.00930583, 
    viewOffsetX=-0.000970364, viewOffsetY=-0.00354045)
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=4 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0179782, 
    farPlane=0.0373153, width=0.0151836, height=0.00922163, cameraPosition=(
    0.0136485, 0.0313898, 0.00148307), cameraUpVector=(-0.505684, 0.496505, 
    -0.705526), cameraTarget=(-0.00480596, 0.00252214, -0.00560479), 
    viewOffsetX=-0.000961584, viewOffsetY=-0.00350841)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0188374, 
    farPlane=0.036456, width=0.0110653, height=0.00672043, 
    viewOffsetX=-0.00159398, viewOffsetY=-0.00344604)
session.viewports['Viewport: 1'].view.setValues(nearPlane=0.0166542, 
    farPlane=0.036937, width=0.00978286, height=0.00594154, cameraPosition=(
    0.00951451, 0.0245627, -0.0186141), cameraUpVector=(-0.857182, 0.233209, 
    -0.459186), cameraTarget=(-0.00747, 0.00134384, 0.00129967), 
    viewOffsetX=-0.00140924, viewOffsetY=-0.00304665)
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
session.viewports['Viewport: 1'].odbDisplay.setFrame(step=0, frame=5 )
