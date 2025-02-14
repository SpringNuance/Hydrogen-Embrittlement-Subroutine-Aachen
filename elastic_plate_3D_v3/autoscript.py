import os 
import shutil 
import argparse
import numpy as np
import pandas as pd
import copy
import time

# Going to current directory
os.chdir(os.getcwd())


def return_description_properties(properties_path_excel, flow_curve_excel_path):
    description_properties_dict = {
        "mechanical_properties": {},
        "hydrogen_diffusion_properties": {},
        "flow_curve_properties": {},
    }

    # Loading the properties file
    # Cast to string to avoid issues with the mixed types in the excel file
    properties_df = pd.read_excel(properties_path_excel, dtype=str)

    mechanical_descriptions_list = properties_df["mechanical_descriptions"].dropna().tolist()
    mechanical_values_list = properties_df["mechanical_values"].dropna().tolist()

    hydrogen_diffusion_descriptions_list = properties_df["hydrogen_diffusion_descriptions"].dropna().tolist()
    hydrogen_diffusion_values_list = properties_df["hydrogen_diffusion_values"].dropna().tolist()

    # Loading the flow curve file
    # Cast to string to avoid issues with the mixed types in the excel file

    flow_curve_df = pd.read_excel(flow_curve_excel_path, dtype=str)

    equivalent_plastic_stress = flow_curve_df["stress/Pa"].dropna().tolist()
    equivalent_plastic_strain = flow_curve_df["strain/-"].dropna().tolist()

    ### Now we add the values to the dictionary

    description_properties_dict["mechanical_properties"] = dict(zip(mechanical_descriptions_list, mechanical_values_list))
    description_properties_dict["hydrogen_diffusion_properties"] = dict(zip(hydrogen_diffusion_descriptions_list, hydrogen_diffusion_values_list))
    description_properties_dict["flow_curve_properties"]["equivalent_plastic_strain"] = equivalent_plastic_strain
    description_properties_dict["flow_curve_properties"]["equivalent_plastic_stress"] = equivalent_plastic_stress

    return description_properties_dict



def return_UMAT_property(description_properties_dict): 
    mechanical_properties_list = list(description_properties_dict["mechanical_properties"].values())
    mechanical_description_list = list(description_properties_dict["mechanical_properties"].keys())
    flow_curve_true_strain = description_properties_dict["flow_curve_properties"]["equivalent_plastic_strain"]
    flow_curve_true_stress = description_properties_dict["flow_curve_properties"]["equivalent_plastic_stress"]
    
    flow_curve_zipped = []
    for stress, strain in zip(flow_curve_true_stress, flow_curve_true_strain):
        flow_curve_zipped.append(stress)
        flow_curve_zipped.append(strain)
    

    # Abaqus needs to define 8 properties each line
    mech_prop_num_lines = int(np.ceil(len(mechanical_properties_list)/8))
    mech_prop_num_properties = int(mech_prop_num_lines*8)
    flow_curve_num_lines = int(np.ceil(len(flow_curve_zipped)/8))
    flow_curve_num_properties = int(flow_curve_num_lines*8)

    total_UMAT_num_properties = mech_prop_num_properties + flow_curve_num_properties

    UMAT_property = []
    
    # The last line would be padded with 0.0 and their corresponding description would be "none"
    # If the number of properties is not a multiple of 8

    # For mechanical properties
    UMAT_property.append("**")
    UMAT_property.append("** =====================")
    UMAT_property.append("**")
    UMAT_property.append("** MECHANICAL PROPERTIES")
    UMAT_property.append("**")
    
    for line_index in range(mech_prop_num_lines):
        if line_index != mech_prop_num_lines - 1:
            subset_properties = mechanical_properties_list[line_index*8:(line_index+1)*8]
            subset_description = mechanical_description_list[line_index*8:(line_index+1)*8]
            UMAT_property.append(", ".join(subset_properties))
            UMAT_property.append("** " + ", ".join(subset_description[0:4]))
            UMAT_property.append("** " + ", ".join(subset_description[4:8]))
        else:
            subset_properties = mechanical_properties_list[line_index*8:] + ["0.0"]*(8-len(mechanical_properties_list[line_index*8:]))
            subset_description = mechanical_description_list[line_index*8:] + ["none"]*(8-len(mechanical_description_list[line_index*8:]))
            UMAT_property.append(", ".join(subset_properties))
            UMAT_property.append("** " + ", ".join(subset_description[0:4]))
            UMAT_property.append("** " + ", ".join(subset_description[4:8]))
    
    # For flow curve properties
    # Important: DO NOT PAD THIS TIME FOR FLOW CURVE
    UMAT_property.append("**")
    UMAT_property.append("** =====================")
    UMAT_property.append("**")
    UMAT_property.append("** FLOW CURVE PROPERTIES")
    UMAT_property.append("**")
    
    UMAT_property.append("** True stress (Pa) - PEEQ (dimless) value pairs")
    for line_index in range(flow_curve_num_lines):
        if line_index != flow_curve_num_lines - 1:
            str_values = [str(value) for value in flow_curve_zipped[line_index*8:(line_index+1)*8]]
            UMAT_property.append(", ".join(str_values))
        else:
            str_values = [str(value) for value in flow_curve_zipped[line_index*8:]]
            UMAT_property.append(", ".join(str_values))
    
    UMAT_property.append("**")
    UMAT_property.append("*******************************************************")

    return UMAT_property, total_UMAT_num_properties


def return_UMATHT_property(description_properties_dict): 

    hydrogen_diffusion_properties_list = list(description_properties_dict["hydrogen_diffusion_properties"].values())
    hydrogen_diffusion_description_list = list(description_properties_dict["hydrogen_diffusion_properties"].keys())

    # Abaqus needs to define 8 properties each line
    hydrogen_diffusion_prop_num_lines = int(np.ceil(len(hydrogen_diffusion_properties_list)/8))
    hydrogen_diffusion_prop_num_properties = int(hydrogen_diffusion_prop_num_lines*8)

    total_UMATHT_num_properties = hydrogen_diffusion_prop_num_properties 
    
    UMATHT_property = []
    
    # The last line would be padded with 0.0 and their corresponding description would be "none"
    # If the number of properties is not a multiple of 8

    # For hydrogen diffusion properties
    UMATHT_property.append("**")
    UMATHT_property.append("** =============================")
    UMATHT_property.append("**")
    UMATHT_property.append("** HYDROGEN DIFFUSION PROPERTIES")
    UMATHT_property.append("**")

    for line_index in range(hydrogen_diffusion_prop_num_lines):
        if line_index != hydrogen_diffusion_prop_num_lines - 1:
            subset_properties = hydrogen_diffusion_properties_list[line_index*8:(line_index+1)*8]
            subset_description = hydrogen_diffusion_description_list[line_index*8:(line_index+1)*8]
            UMATHT_property.append(", ".join(subset_properties))
            UMATHT_property.append("** " + ", ".join(subset_description[0:4]))
            UMATHT_property.append("** " + ", ".join(subset_description[4:8]))
        else:
            subset_properties = hydrogen_diffusion_properties_list[line_index*8:] + ["0.0"]*(8-len(hydrogen_diffusion_properties_list[line_index*8:]))
            subset_description = hydrogen_diffusion_description_list[line_index*8:] + ["none"]*(8-len(hydrogen_diffusion_description_list[line_index*8:]))
            UMATHT_property.append(", ".join(subset_properties))
            UMATHT_property.append("** " + ", ".join(subset_description[0:4]))
            UMATHT_property.append("** " + ", ".join(subset_description[4:8]))

    UMATHT_property.append("**")
    UMATHT_property.append("*******************************************************")

    return UMATHT_property, total_UMATHT_num_properties


def return_depvar(depvar_excel_path):

    depvar_df = pd.read_excel(depvar_excel_path, dtype=str)
    nstatev = len(depvar_df)
    #print("The number of state variables is: ", nstatev)

    DEPVAR = [
        "*Depvar       ",
        f"  {nstatev},      ",  
    ]

    depvar_index = depvar_df["depvar_index"].tolist()
    depvar_name = depvar_df["depvar_name"].tolist()
    # print("The depvar names are: ", depvar_name)
    # time.sleep(180)
    output_UVARM = [int(flag) for flag in depvar_df["output_UVARM"].tolist()]

    for i in range(1, nstatev + 1):
        index = depvar_index[i-1]
        name = depvar_name[i-1]
        DEPVAR.append(f"{index}, AR{index}_{name}, AR{index}_{name}")

    # Output UVARM is only a list of binary. Number 0 means this SDV will not be output
    # and number 1 means this SDV will be output
    # We must use UVARM because if we output all SDV, the odb file would be very heavy
    # Since fortran counts from 1, we must add 1 to the index
    
    chosen_output_UVARM = [i + 1 for i, flag in enumerate(output_UVARM) if flag == 1]
    nvars = sum(output_UVARM)
    
    return DEPVAR, depvar_name, nstatev, nvars, chosen_output_UVARM

def extracting_nodes_coordinates(flines):

    # Find the start index of the *NODE section

    flines_upper = [line.upper() for line in flines]

    start_node_idx = -1
    for i in range(len(flines_upper)):
        if "*NODE" in flines_upper[i]:
            start_node_idx = i
            break

    # Find the end index (first occurrence of *ELEMENT after *NODE)
    end_node_idx = len(flines)  # Default to end of file if *ELEMENT is not found
    for i in range(start_node_idx + 1, len(flines_upper)):
        if "*ELEMENT" in flines_upper[i]:
            end_node_idx = i
            break

    # Extract node coordinates (excluding the *NODE line)
    node_coordinates = flines[start_node_idx:end_node_idx]
    total_nodes = len(node_coordinates) - 1 # excluding the header *Node
    
    return node_coordinates, total_nodes, start_node_idx, end_node_idx

def constructing_mesh(flines):

    flines = [line.strip() for line in flines]
    flines_upper = [line.upper() for line in flines]
    start_elements = [i for i, line in enumerate(flines_upper) if '*ELEMENT' in line and '*ELEMENT OUTPUT' not in line]
    start_element_index = start_elements[0]
    element_indices = [] # list of element index
    element_connvectivity = [] # list of of list of nodes that make up the element

    #print("The starting element index is: ", start_element_index)
    #print(start_element_index)

    mesh_index = start_element_index + 1

    while flines_upper[mesh_index][0] != "*" and flines_upper[mesh_index][0] != " ":
    
        # remove all empty spaces and split by comma
        # each line look like this: 1,    35,     2,    36,  2503,  5502,  5503,  5504,  5505
        split_line = flines_upper[mesh_index].replace(" ", "").split(",")
        
        element_indices.append(split_line[0])
        element_connvectivity.append(split_line[1:])
        mesh_index += 1
    
    end_element_index = mesh_index

    # print("The element indices are: ", element_indices)
    # print("The element connectivity are: ", element_connvectivity)

    # Now we would count the number of elements 
    total_elems = len(element_indices)

    original_mesh = [flines[start_element_index]] # The first line is the *ELEMENT line
    # original_mesh = []
    for i in range(total_elems):
        reconstructed_line_original = ",".join([str(value) for value in [element_indices[i]] + element_connvectivity[i]])
        original_mesh.append(reconstructed_line_original)
    
    return original_mesh, total_elems, start_element_index, end_element_index
    
def return_control_settings(controls_path_excel):
    control_df = pd.read_excel(controls_path_excel)
    # Remove nan first
    solver_controls_values = control_df["solver_controls_values"].dropna().tolist()
    displacement_field_controls_values = control_df["displacement_field_controls_values"].dropna().tolist()
    diffusion_field_controls_values = control_df["diffusion_field_controls_values"].dropna().tolist()
    constraints_controls_values = control_df["constraints_controls_values"].dropna().tolist()
    time_incrementation_control_values = control_df["time_incrementation_controls_values"].dropna().tolist()
    line_search_controls_values = control_df["line_search_controls_values"].dropna().tolist()
    CONTROLS = []
    CONTROLS.append("*SOLVER CONTROLS")
    solver_controls_values = [" " if value == "default" else str(value) for value in solver_controls_values]
    CONTROLS.append(", ".join(solver_controls_values) + ",")
    CONTROLS.append("*CONTROLS, PARAMETERS=FIELD, FIELD=DISPLACEMENT")
    displacement_field_controls_values = [" " if value == "default" else str(value) for value in displacement_field_controls_values]
    CONTROLS.append(", ".join(displacement_field_controls_values[0:8]) + ",")
    CONTROLS.append(", ".join(displacement_field_controls_values[8:11]) + ",")
    CONTROLS.append("*CONTROLS, PARAMETERS=FIELD, FIELD=TEMPERATURE")
    diffusion_field_controls_values = [" " if value == "default" else str(value) for value in diffusion_field_controls_values]
    CONTROLS.append(", ".join(diffusion_field_controls_values[0:8]) + ",")
    CONTROLS.append(", ".join(diffusion_field_controls_values[8:11]) + ",")
    CONTROLS.append("*CONTROLS, PARAMETERS=CONSTRAINTS")
    constraints_controls_values = [" " if value == "default" else str(value) for value in constraints_controls_values]
    CONTROLS.append(", ".join(constraints_controls_values) + ",")
    CONTROLS.append("*CONTROLS, PARAMETERS=TIME INCREMENTATION")
    time_incrementation_control_values = [" " if value == "default" else str(value) for value in time_incrementation_control_values]
    CONTROLS.append(", ".join(time_incrementation_control_values[0:13]) + ",")
    CONTROLS.append(", ".join(time_incrementation_control_values[13:21]) + ",")
    CONTROLS.append(", ".join(time_incrementation_control_values[21:29]) + ",")
    CONTROLS.append(time_incrementation_control_values[29] + ",")
    CONTROLS.append("*CONTROLS, PARAMETERS=LINE SEARCH")
    line_search_controls_values = [" " if value == "default" else str(value) for value in line_search_controls_values]
    CONTROLS.append(", ".join(line_search_controls_values) + ",")

    # print("The CONTROLS are:\n", "\n".join(CONTROLS))
    # time.sleep(180)
    return CONTROLS

def return_userinputs_fortran(DEPVAR, depvar_name, total_elems, total_nodes, nvars, chosen_output_UVARM):

    USERINPUTS = []
    USERINPUTS.append("!***********************************************************************")
    USERINPUTS.append("")
    USERINPUTS.append("\t! State variables  ")
    USERINPUTS.append("\t! 1 to 6: sig11, sig22, sig33, sig12, sig13, sig23")
    USERINPUTS.append("\t! 7 to 12: stran11, stran22, stran33, stran12, stran13, stran23")
    for depvar in DEPVAR[12:]:
        USERINPUTS.append(f"\t! {depvar}")
    USERINPUTS.append("")

    USERINPUTS.append("module userinputs")
    USERINPUTS.append("\tuse precision")
    USERINPUTS.append("\timplicit none")

    USERINPUTS.append("\t! THESE TWO VALUES ARE HARD-CODED")
    USERINPUTS.append("\t! YOU MUST CHANGE IT TO THE ACTUAL NUMBER OF ELEMENTS AND NODES IN .INP FILE")
    USERINPUTS.append("\t! YOU CAN USE PYTHON SCRIPTING TO CHANGE VALUES AS WELL")
    USERINPUTS.append("")
    USERINPUTS.append(f"\tinteger, parameter :: total_elems = {total_elems} ! Storing the actual number of elements")
    USERINPUTS.append(f"\tinteger, parameter :: total_nodes = {total_nodes} ! Storing the actual number of nodes")
    USERINPUTS.append("")
    USERINPUTS.append("\t! Subset of SDVs indices that you want to output ")
    USERINPUTS.append(f"\tinteger, parameter :: uvar_indices({nvars}) = (/{', '.join([str(i) for i in chosen_output_UVARM])}/)")
    USERINPUTS.append("")
    USERINPUTS.append("\t! Index of statev in UMAT and UMATHT")
    USERINPUTS.append("\tinteger, parameter :: sig_start_idx = 1 ! Starting index of the stress component in statev")
    USERINPUTS.append("\tinteger, parameter :: sig_end_idx = 6 ! Ending index of the strain component in statev")
    USERINPUTS.append("\tinteger, parameter :: stran_start_idx = 7 ! Starting index of the total strain component in statev")
    USERINPUTS.append("\tinteger, parameter :: stran_end_idx = 12 ! Ending index of the total strain component in statev")
    
    count_index = 13
    # print("The depvar name is: ", depvar_name)
    # time.sleep(180)
    for name in depvar_name[12:]:
        USERINPUTS.append(f"\tinteger, parameter :: {name}_idx = {count_index} ! Index of the {name} in statev")
        count_index += 1    
    USERINPUTS.append("")
    USERINPUTS.append("\tinteger, parameter :: before_flow_props_idx = 8 ! Index of the first flow curve data in props in UMAT")
    USERINPUTS.append("")
    USERINPUTS.append("end module")

    return USERINPUTS


def main():
    
    # Create the parser
    parser = argparse.ArgumentParser(description="Process geometry flag.")
    # Add the --geometry flag, expecting a string argument
    parser.add_argument('--input', type=str, required=True, help='input file name')
    
    # Parse the command-line arguments
    args = parser.parse_args()
    # Access the geometry argument
    inp_file_name = args.input

    output_simulation_path = os.getcwd()
    combined_original_inp_path = os.path.join(os.getcwd(), f"{inp_file_name}.inp")
    processed_inp_path = os.path.join(os.getcwd(), f"{inp_file_name}_processed.inp")

    ##############################
    # STEP 0: Deleting lck file  #
    ##############################

    # Delete all files ending in .lck in output_simulation_path
    for file in os.listdir(output_simulation_path):
        if file.endswith(".lck"):
            os.remove(os.path.join(output_simulation_path, file))

    #################################################
    # STEP 1: Modifying normal inp to processed inp #
    #################################################

    # Write this to the start of the input file

    unit_convention = [
        "**************** UNITS: SI (m) ****************",
        "**",
        "** Length: m",
        "** Time: s",
        "** Force: N",
        "** Stress: Pa",
        "** Mass: kg = (N*s^2)/m",
        "** Density: kg/m^3",
        "**",
        "************************************************"
    ]

    # Read the original file content
    with open(combined_original_inp_path, 'r') as fid:
        flines = fid.readlines()  # Read file as a list of lines
    flines = [line.strip() for line in flines]  # Remove trailing and newline symbols

    # Insert unit convention at the beginning
    flines = unit_convention + flines  # Add newline for spacing

    # time.sleep(180)

    #########################################
    # STEP 1: Extracting the nodes.inc file #
    #########################################

    node_coordinates, total_nodes, start_node_idx, end_node_idx = extracting_nodes_coordinates(flines)

    with open("processing_input/nodes.inc", 'w') as fid:
        for line_idx in range(len(node_coordinates) - 1):
            fid.write(node_coordinates[line_idx] + "\n")
        fid.write(node_coordinates[-1])

    # Now, we remove everything between start_node_idx and end_node_idx, and insert
    include_nodes = [
        "********************************************",
        "*INCLUDE, INPUT=\"processing_input/nodes.inc\"",
        "********************************************",
    ]
    
    flines = flines[:start_node_idx] + include_nodes + flines[end_node_idx:]

    ############################################
    # STEP 2: Extracting the mesh connectivity #
    ############################################

    original_mesh, total_elems, start_element_idx, end_element_idx =\
        constructing_mesh(flines)

    # Write the original mesh. We must avoid empty line at the end
    with open("processing_input/elements.inc", 'w') as fid:
        for line_idx in range(len(original_mesh) - 1):
            fid.write(original_mesh[line_idx] + "\n")
        fid.write(original_mesh[-1])

    # Now, we remove everything between start_element_idx and end_element_idx, and insert
    include_elements = [
        "***********************************************",
        "*INCLUDE, INPUT=\"processing_input/elements.inc\"",
        "***********************************************",
    ]
    
    flines = flines[:start_element_idx] +  include_elements + flines[end_element_idx:]

    #####################################################
    # STEP 3: Extracting the UMAT and UMATHT properties #
    #####################################################

    properties_path_excel = f"processing_input/properties.xlsx"
    flow_curve_excel_path = f"processing_input/flow_curve.xlsx"
    description_properties_dict = return_description_properties(properties_path_excel, flow_curve_excel_path)
    UMAT_PROPERTY, total_UMAT_num_properties = return_UMAT_property(description_properties_dict)
    UMATHT_PROPERTY, total_UMATHT_num_properties = return_UMATHT_property(description_properties_dict)
    
    # 1. Replace the UMAT properties

    # Replacing UMAT properties
    umat_index = [i for i, line in enumerate(flines) if '*USER MATERIAL' in line.upper() and 'MECHANICAL' in line.upper()][0]
    
    # Find where the current UMAT section ends (by finding the next line that starts with '*')
    next_star_line_umat = next(i for i in range(umat_index + 1, len(flines)) if flines[i].startswith('*'))
    
    # Replace the number of constants in the *User Material line
    flines[umat_index] = f"*User Material, constants={total_UMAT_num_properties}, type=MECHANICAL"
    
    # Replace the content under UMAT
    flines = flines[:umat_index + 1] + UMAT_PROPERTY + flines[next_star_line_umat:]

    # 2. Replace the UMATHT properties

    # Replacing UMATHT properties
    umatht_index = [i for i, line in enumerate(flines) if '*USER MATERIAL' in line.upper() and 'THERMAL' in line.upper()][0]
    
    # Find where the current UMATHT section ends (by finding the next line that starts with '*')
    next_star_line_umatht = next(i for i in range(umatht_index + 1, len(flines)) if flines[i].startswith('*'))
    
    # Replace the number of constants in the *User Material line
    flines[umatht_index] = f"*User Material, constants={total_UMATHT_num_properties}, type=THERMAL"
    
    # Replace the content under UMATHT
    flines = flines[:umatht_index + 1] + UMATHT_PROPERTY + flines[next_star_line_umatht:]

    # 3. We would also modify the *Depvar section to include the key descriptions
    
    #########################################
    # STEP 4: Extracting the DEPVAR section #
    #########################################

    # Replacing Depvar section

    # find the index of the *Depvar section
    depvar_index = [i for i, line in enumerate(flines) if '*DEPVAR' in line.upper()][0]
    
    depvar_excel_path = f"processing_input/depvar.xlsx"
    DEPVAR, depvar_name, nstatev, nvars, chosen_output_UVARM = return_depvar(depvar_excel_path)
    flines = flines[:depvar_index] + DEPVAR + flines[depvar_index+2:]
    

    # INITIAL_CONDITIONS = [
    #     "*Initial Conditions, Type=Solution"
    # ]
    # # We initialize all sdv as zeros
    # #print("The number of state variables is: ", nstatev)
    # if (nstatev < 8):
    #     INITIAL_CONDITIONS.append("whole_part, " + ", ".join(["0.0"]*nstatev))
    # else:
    #     INITIAL_CONDITIONS.append("whole_part, " + ", ".join(["0.0"]*7))
    #     for i in range(nstatev//8 - 1):
    #         INITIAL_CONDITIONS.append(", ".join(["0.0"]*8))
    #     INITIAL_CONDITIONS.append(", ".join(["0.0"]*(nstatev%8 + 1)))

    #print("The initial conditions are: ", INITIAL_CONDITIONS)

    # Now insert the initial conditions right under nvars
    # flines = flines[:output_var_index + 2] + INITIAL_CONDITIONS + flines[output_var_index + 2:]

    # ###############################
    # # STEP 5: Adding the controls #
    # ###############################

    controls_path_excel = f"processing_input/controls.xlsx"
    CONTROLS = return_control_settings(controls_path_excel)

    # https://help.3ds.com/2024/english/dssimulia_established/simacaecaerefmap/simacae-t-simothergencontrols.htm?contextscope=all

    # We have to add it two lines after the line that starts with *FIELD, VARIABLE
    field_variable_index = [i for i, line in enumerate(flines) if line.upper().startswith("*FIELD, VARIABLE")][0]
    control_temperature_index = field_variable_index + 2

    flines = flines[:control_temperature_index] + CONTROLS + flines[control_temperature_index:]

    #########################################
    # STEP 6: Changing the output variables #
    #########################################

    # Replacing User Output Variables
    output_var_index = [i for i, line in enumerate(flines) if line.upper().startswith('*USER OUTPUT VARIABLES')][0]
    
    # The line below it is to be replaced
    flines[output_var_index + 1] = f"{nvars},"
    
    with open(processed_inp_path, 'w') as fid:
        for line in flines:
            fid.write(line + "\n")

    # Finally, we would write the fortran file userinputs.f90 to the source_code folder
    USERINPUTS = return_userinputs_fortran(DEPVAR, depvar_name, total_elems, total_nodes, nvars, chosen_output_UVARM)
    with open("source_code/userinputs.f90", 'w') as fid:
        for line in USERINPUTS:
            fid.write(line + "\n")

if __name__ == "__main__":
    main()
