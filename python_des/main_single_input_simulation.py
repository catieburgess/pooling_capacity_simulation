import pandas as pd
pd.options.mode.chained_assignment = None  # default='warn'

from src.lab_functions import plate_wrapper_simulation


inputs = pd.read_csv('./data/daily_sample_input_median.csv')
inputs['date'] = pd.to_datetime(inputs['date'])


# loop for each staff and capacity level
levels = [[0, 0], [0, 1], [1, 0], [1, 1], [2, 0], [2, 1], [2, 2]]

all_results = []

for level in levels:

    staff = level[0]
    capacity = level[1]

    try:
        results = inputs.groupby(['date', 'method']).apply(lambda x: plate_wrapper_simulation(x, staff_level = staff, capacity_level = capacity, dt = 5)).reset_index()
    except:
        print("failure")

    results['staff_level'] = staff
    results['capacity_level'] = capacity

    all_results.append(results)
    print(f'level {level} finished...')

all_results = pd.concat(all_results).reset_index()

all_results.to_csv('./results/daily_isolated_simulation_results.csv', index = False)
