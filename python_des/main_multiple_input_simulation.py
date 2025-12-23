import pandas as pd
pd.options.mode.chained_assignment = None  # default='warn'

from src.lab_functions import plate_builder, plate_status_advance


inputs = pd.read_csv('./data/daily_sample_input_median.csv')
inputs['date'] = pd.to_datetime(inputs['date'])

# subset to requested weeks, run one at a time
# inputs_week = inputs[inputs['date'] >= pd.to_datetime('2020-08-17')]
# inputs_week = inputs_week[inputs_week['date'] <= pd.to_datetime('2020-08-23')]
# week_name = 'week1'

# inputs_week = inputs[inputs['date'] >= pd.to_datetime('2020-08-24')]
# inputs_week = inputs_week[inputs_week['date'] <= pd.to_datetime('2020-08-28')]
# week_name = 'week2'

# inputs_week = inputs[inputs['date'] >= pd.to_datetime('2020-08-31')]
# inputs_week = inputs_week[inputs_week['date'] <= pd.to_datetime('2020-09-04')]
# week_name = 'week3'

inputs_week = inputs[inputs['date'] >= pd.to_datetime('2020-12-08')]
inputs_week = inputs_week[inputs_week['date'] <= pd.to_datetime('2020-12-13')]
week_name = 'week4'

inputs_week['add_time'] = round((inputs_week['date'] - inputs_week['date'].min()).dt.total_seconds() / 60)

# levels = [[0, 0], [0, 1], [1, 0], [1, 1], [2, 0], [2, 1], [2, 2]]
levels = [[0, 0]]

dt = 5

all_results = []

for level in levels:

    staff_level = level[0]
    capacity_level = level[1]

    for method in inputs_week['method'].unique():
        
        method_subset = inputs_week[inputs_week['method'] == method]

        time = 0
        cycle_time = 0

        plates = []
        # loop over all days to build all start and future plates
        for day in range(len(method_subset)):

            day_sub = method_subset.iloc[[day]]
            # initial sample input
            plate_sub = [round(day_sub['class_1'].iloc[0]), 
                        round(day_sub['class_2'].iloc[0]), 
                        round(day_sub['class_3'].iloc[0]), 
                        round(day_sub['class_4'].iloc[0])]
            plate_sub = plate_builder(plate_sub).reset_index()
            plate_sub['add_time'] = day_sub['add_time'].iloc[0]
            plates.append(plate_sub)

        plates = pd.concat(plates)
        plates['Plate Number'] = list(range(1, len(plates) + 1))

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

        n_plates_total = len(plates)
        n_plates_completed = 0

        plates['start_time'] = 0
        plates['end_time'] = 0

        clear1 = False
        clear2 = False

        active_plates = []

        while n_plates_completed < n_plates_total:
            
            # append new plates by time
            if (time == plates['add_time']).any():
                plates_to_add = plates[plates['add_time'] == time]
                if time == 0:
                    active_plates.append(plates_to_add)
                    active_plates = pd.concat(active_plates)
                else:
                    active_plates = pd.concat([active_plates, plates_to_add])

                # reset sample prior timers
                clear1 = True
                if (active_plates['Sample1'] > 0).any():
                    clear1 = False

                clear2 = True
                if (active_plates['Sample2'] > 0).any():
                    clear2 = False
            
            # capture clearances of subsamples 1 & 2
            if active_plates.loc[active_plates['Sample1'] > 0]['Station6_status'].mean() == 3:
                clear1 = True
            
            if active_plates.loc[active_plates['Sample2'] > 0]['Station6_status'].mean() == 3:
                clear2 = True

            active_plates = plate_status_advance(active_plates, time_marker=time, level1_clear=clear1, level2_clear=clear2,
                                            s1t = s1t, s2t = s2t, s3t = s3t, s4t = s4t, s5t = s5t, s6t = s6t,
                                            s1p = ss1p, s2p = ss2p, s3p = ss3p, s4p = ss4p, s5p = ss5p, s6p = 1)

            active_plates['total_time'] += dt
            active_plates['time_in_step'] += dt

            n_plates_completed = len(active_plates[active_plates['Station6_status'] == 3])

            if ((2 == active_plates[['Station5_status']]).any().any() & (~(2 == active_plates[['Station1_status', 'Station2_status', 'Station3_status', 'Station4_status', 'Station6_status']]).any().any())):
                cycle_time += dt

            time += dt

            # escape condition if it takes too long
            if time > 20000:
                break

        # results = pd.concat(results)
        active_plates['cycle_time'] = cycle_time
        active_plates['method'] = method
        active_plates['staff_level'] = staff_level
        active_plates['capacity_level'] = capacity_level

        all_results.append(active_plates)

        print('Method {} Complete...'.format(method))

all_results = pd.concat(all_results)

# all_results.to_csv('./results/weekly_additive_simulation_{}_results.csv'.format(week_name), index = False)
all_results.to_csv('./results/test.csv'.format(week_name), index = False)