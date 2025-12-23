import pandas as pd
import numpy as np
pd.options.mode.chained_assignment = None  # default='warn'


def plate_builder(samples, samples_per_plate = 40):
    """Function to create a DataFrame of organized samples on a sample plates. 
    
    Criteria for plate organization are defined as:
        - 4 types of sample, noted as Sample1 - Sample4
        - Sample1 - Sample3 can only exist by themselves on a plate
        - Sample4 fills out excess space on plates  
        - The total number of samples on a plate is an input

    Output is the organization of each plate, and the total number of plates needed for the given sample input.
    Tracking variables for the progress of each plate are added as well.
    

    Args:
        samples (list): list containing the number of each sample. First index is Sample1 and so on.
        samples_per_plate (int, optional): Total number of samples per plate. Defaults to 40.
    """
    # get number of samples
    tot_s1 = samples[0]
    tot_s2 = samples[1]
    tot_s3 = samples[2]
    tot_s4 = samples[3]

    # samples 1, 2, 3 can never share plates - total plates must be at least equal to occurance of these samples
    s1_plates = np.ceil(tot_s1 / samples_per_plate)
    s2_plates = np.ceil(tot_s2 / samples_per_plate)
    s3_plates = np.ceil(tot_s3 / samples_per_plate)

    # get open slots available for s4
    s1_open = s1_plates * samples_per_plate - tot_s1
    s2_open = s2_plates * samples_per_plate - tot_s2
    s3_open = s3_plates * samples_per_plate - tot_s3

    s4_remainder = tot_s4 - s1_open - s2_open - s3_open

    # if not enough spots to share for s4, need to make new plates
    s4_plates = 0
    if s4_remainder > 0:
        s4_plates = np.ceil(s4_remainder / samples_per_plate)

    total_plates = s1_plates + s2_plates + s3_plates + s4_plates

    plate_summary = []
    s1_assigned = 0
    s2_assigned = 0
    s3_assigned = 0
    s4_assigned = 0

    # build out individual plates
    for plate in range(int(total_plates)):
        
        plate_name = plate + 1

        plate_s1 = np.clip(tot_s1 - s1_assigned, 0, samples_per_plate)
        plate_s2 = 0
        plate_s3 = 0
        s1_assigned = plate_s1 + s1_assigned

        if plate_s1 == 0:
            
            plate_s2 = np.clip(tot_s2 - s2_assigned, 0, samples_per_plate)
            plate_s3 = 0
            s2_assigned = plate_s2 + s2_assigned

            if plate_s2 == 0:
                plate_s3 = np.clip(tot_s3 - s3_assigned, 0, samples_per_plate)
                s3_assigned = plate_s3 + s3_assigned

        remaining = samples_per_plate - sum([plate_s1, plate_s2, plate_s3])
        
        plate_s4 = np.clip(tot_s4 - s4_assigned, 0, remaining)
        s4_assigned = plate_s4 + s4_assigned

        res = pd.DataFrame([[plate_name, plate_s1, plate_s2, plate_s3, plate_s4]], columns=['Plate Number', 'Sample1', 'Sample2', 'Sample3', 'Sample4'])
        
        plate_summary.append(res)
    
    plate_summary = pd.concat(plate_summary)

    # add variables for tracking plate progression
    plate_summary['Station1_status'] = 1
    plate_summary['Station2_status'] = 0
    plate_summary['Station3_status'] = 0
    plate_summary['Station4_status'] = 0
    plate_summary['Station5_status'] = 0
    plate_summary['Station6_status'] = 0

    plate_summary['total_time'] = 0
    plate_summary['time_in_step'] = 0
    plate_summary['time_target'] = 0
    plate_summary['start_time'] = 0
    plate_summary['end_time'] = 0
    
    return(plate_summary)


def advance_completed_plates(plates, time_marker):
    """Helper function to advance the status of plates that have completed their station.

    Args:
        plates (DataFrame): Plates outject, output of plate_builder.
        time_marker (int): Time counter to note the time of the advancement.
    """
    def process_row(row):

        # check for samples that have completed their in step time and need to advance
        if row['time_in_step'] >= row['time_target']:
            
            # loop over each station
            for i in range(1, 7):

                # define to column name for the target station
                station_col = f'Station{i}_status'

                if row[station_col] == 2:
                    # update the current station status to 3
                    row[station_col] = 3
                    # if its not the last station, update the next station status to 1
                    if i < 6:
                        next_station_col = f'Station{i+1}_status'
                        row[next_station_col] = 1
                    
                    # if advancement is final station lock in end time
                    if i == 6:
                        row['end_time'] = time_marker

                    break  # stop after updating the first '2' found
            
            # reset time in test if the sample was advanced to the next step
            row['time_in_step'] = 0
        return row

    # apply function to each row
    plates = plates.apply(process_row, axis=1)
    return plates

def plate_status_advance(plates, time_marker, s1t, s2t, s3t, s4t, s5t, s6t,
                         s1p, s2p, s3p, s4p, s5p, s6p, level1_clear = False, level2_clear = False,):
    """Function to update the status of all plates between their stations.

    Args:
        plates (DataFrame): Plates outject, output of plate_builder.
        time_marker (int): Time counter.
        s1t (int): Time required for samples to complete station1.
        s2t (int): Time required for samples to complete station2.
        s3t (int): Time required for samples to complete station3.
        s4t (int): Time required for samples to complete station4.
        s5t (int): Time required for samples to complete station5.
        s6t (int): Time required for samples to complete station6.
        s1p (int): Number of plates station1 can run simultaneously.
        s2p (int): Number of plates station2 can run simultaneously.
        s3p (int): Number of plates station3 can run simultaneously.
        s4p (int): Number of plates station4 can run simultaneously.
        s5p (int): Number of plates station5 can run simultaneously.
        s6p (int): Number of plates station6 can run simultaneously.
        level1_clear (bool, optional): Check to ensure all Sample1 have compelted and Sample2 can begin. Defaults to False.
        level2_clear (bool, optional): Check to ensure all Sample2 have compelted and Sample3 can begin. Defaults to False.
    """
    # first simple check for any advancements to save time
    if (plates['time_in_step'] >= plates['time_target']).any():
        
        # second advance needed plates and define all open slots 
        # station status
        # 0 not reached, 1 in queue, 2 in station, 3 completed station
        plates = advance_completed_plates(plates, time_marker)
        
        # determine initial capacity for each station after advancements have occurred
        s1_slots = s1p - len(plates.loc[plates['Station1_status'] == 2])
        s2_slots = s2p - len(plates.loc[plates['Station2_status'] == 2])
        s3_slots = s3p - len(plates.loc[plates['Station3_status'] == 2])
        s4_slots = s4p - len(plates.loc[plates['Station4_status'] == 2])
        s5_slots = s5p - len(plates.loc[plates['Station5_status'] == 2])
        s6_slots = s6p - len(plates.loc[plates['Station6_status'] == 2])

        updated_plates = []

        # for loop over plates
        for i in plates['Plate Number'].unique():
            
            plate = plates[plates['Plate Number'] == i]
            
            # ignore if plate is finished
            if plate['Station6_status'].iloc[0] < 3:
                
                # determine which plates can enter
                plates_eligible = plates.loc[(plates['Sample1'] > 0) | ((plates['Sample2'] == 0) & (plates['Sample3'] == 0))]

                if level1_clear:
                    plates_eligible = plates.loc[(plates['Sample2'] > 0) | (plates['Sample3'] == 0)]

                if (level1_clear & level2_clear):
                    plates_eligible = plates

                # advance eligible plates to station 1
                s1_queue = plate.loc[plate['Plate Number'].isin(plates_eligible['Plate Number'])]
                s1_queue = s1_queue[s1_queue['Station1_status'] == 1]

                # if station 1 has space and a queue:
                # advance plate from in queue to processing
                # define new time target for the station
                # reset the time in the station to 0
                # remove an open slot for the station
                if len(s1_queue) > 0:
                    if s1_slots > 0:
                        plate['Station1_status'] = 2
                        plate['time_target'] = s1t
                        plate['time_in_step'] = 0
                        plate['start_time'] = time_marker
                        s1_slots = s1_slots - 1

                # update other stations
                # eligibility check is only required for station 1
                # update station 2
                s2_queue = plate[plate['Station2_status'] == 1]
                if len(s2_queue) > 0:
                    if s2_slots > 0:
                        plate['Station2_status'] = 2
                        plate['time_in_step'] = 0
                        plate['time_target'] = s2t
                        s2_slots = s2_slots - 1

                # update station 3
                s3_queue = plate[plate['Station3_status'] == 1]
                if len(s3_queue) > 0:
                    if s3_slots > 0:
                        plate['Station3_status'] = 2
                        plate['time_in_step'] = 0
                        plate['time_target'] = s3t
                        s3_slots = s3_slots - 1
                
                # update station 4
                s4_queue = plate[plate['Station4_status'] == 1]
                if len(s4_queue) > 0:
                    if s4_slots > 0:
                        plate['Station4_status'] = 2
                        plate['time_in_step'] = 0
                        plate['time_target'] = s4t
                        s4_slots = s4_slots - 1
                
                # update station 5
                s5_queue = plate[plate['Station5_status'] == 1]
                if len(s5_queue) > 0:
                    if s5_slots > 0:
                        plate['Station5_status'] = 2
                        plate['time_in_step'] = 0
                        plate['time_target'] = s5t
                        s5_slots = s5_slots - 1
                
                # update station 6
                s6_queue = plate[plate['Station6_status'] == 1]
                if len(s6_queue) > 0:
                    if s6_slots > 0:
                        plate['Station6_status'] = 2
                        plate['time_in_step'] = 0
                        plate['time_target'] = s6t
                        s6_slots = s6_slots - 1
                
            updated_plates.append(plate)
    
        updated_plates = pd.concat(updated_plates)
    
    # return same plate if no advancements
    else:
        updated_plates = plates
    
    return(updated_plates)


def organize_results(plates):
    """Helper function to convert plate information into a more plot friendly format.

    Args:
        plates (DataFrame): Plates outject, output of plate_builder.
    """
   
    res1 = pd.DataFrame((plates[['Station1_status', 'Station2_status', 'Station3_status',
                           'Station4_status', 'Station5_status', 'Station6_status']] == 1).sum(axis = 0)).reset_index()
    
    res2 = pd.DataFrame((plates[['Station1_status', 'Station2_status', 'Station3_status',
                           'Station4_status', 'Station5_status', 'Station6_status']] == 2).sum(axis = 0)).reset_index()
    
    res3 = pd.DataFrame((plates[['Station1_status', 'Station2_status', 'Station3_status',
                           'Station4_status', 'Station5_status', 'Station6_status']] == 3).sum(axis = 0)).reset_index()
    
    res1.columns = ['Station', 'total_in_queue']
    res2.columns = ['Station', 'total_in_test']
    res3.columns = ['Station', 'total_completed']

    res = res1.merge(res2, on = 'Station')
    res = res.merge(res3, on = 'Station')

    res['Station'] = res['Station'].replace('_.*', '', regex = True)

    return(res)


def plate_wrapper_simulation(input, staff_level, capacity_level, dt = 5, break_time = 10000):
    """Wrapper function to run plate advancement through time until all plates are complete. 

    Args:
        input (DataFrame): Input data with sample numbers.
        staff_level (int): Level marker for staffing. 0 = low, 1 = medium, 2 = high.
        capacity_level (_type_): Level marker for lab capacity. 0 = low, 1 = medium, 2 = high.
        dt (int, optional): Time step to iterate over. Conceptually in minutes but only needs to be consistent with the defined station times. Defaults to 5.
        break_time (int, optional): Time accumulation until loop breaks as escape condition for while loop.  
    """
    
    time = 0
    cycle_time = 0
    # initial sample input
    initial = [round(input['class_1'].iloc[0]), 
               round(input['class_2'].iloc[0]), 
               round(input['class_3'].iloc[0]), 
               round(input['class_4'].iloc[0])]
    
    # define constants
    # station X times
    s1t = 30
    s2t = 180
    s3t = 10
    s4t = 20
    s5t = 180
    s6t = 15
    
    #  staff and capacity limits (sXp = station X number of total plates)
    if staff_level == 0:
        ss1p = 2
        ss2p = 8
        ss3p = 1
    elif staff_level == 1:
        ss1p = 3
        ss2p = 12
        ss3p = 1
    else:
        ss1p = 4
        ss2p = 16
        ss3p = 2

    if capacity_level == 0:
        ss4p = 1
        ss5p = 4
    elif capacity_level == 1:
        ss4p = 2
        ss5p = 12
    else:
        ss4p = 4
        ss5p = 22

    plates = plate_builder(initial).reset_index()
    n_plates_total = len(plates)
    n_plates_completed = 0

    plates['start_time'] = 0
    plates['end_time'] = 0

    clear1 = True
    if (plates['Sample1'] > 0).any():
        clear1 = False
    
    clear2 = True
    if (plates['Sample2'] > 0).any():
        clear2 = False

    # while loop for all plates to complete
    while n_plates_completed < n_plates_total:

        plates = plate_status_advance(plates, time_marker=time, level1_clear=clear1, level2_clear=clear2,
                                      s1t = s1t, s2t = s2t, s3t = s3t, s4t = s4t, s5t = s5t, s6t = s6t,
                                      s1p = ss1p, s2p = ss2p, s3p = ss3p, s4p = ss4p, s5p = ss5p, s6p = 1)

        plates['total_time'] += dt
        plates['time_in_step'] += dt

        # capture clearances of subsamples 1 & 2
        if plates.loc[plates['Sample1'] > 0]['Station6_status'].mean() == 3:
            clear1 = True
        
        if plates.loc[plates['Sample2'] > 0]['Station6_status'].mean() == 3:
            clear2 = True

        n_plates_completed = len(plates[plates['Station6_status'] == 3])

        if ((2 == plates[['Station5_status']]).any().any() & (~(2 == plates[['Station1_status', 'Station2_status', 'Station3_status', 'Station4_status', 'Station6_status']]).any().any())):
            cycle_time += dt

        time += dt

        # escape condition if it takes too long
        if time > break_time:
            break

    plates['cycle_time'] = cycle_time

    return(plates)