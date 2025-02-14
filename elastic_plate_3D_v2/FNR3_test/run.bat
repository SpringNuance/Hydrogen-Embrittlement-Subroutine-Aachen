python3.12 autoscript.py --input "FNR3_full"
abaqus job=FNR3_full_processed user="UMAT_2D_3D_Hydrogen_Damage_Quad_Hex.for" cpus=4 mp_mode=threads -verbose 1 interactive