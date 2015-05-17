import csv
import itertools
import random


def get_int(s):
    return int(s.replace(',', ''))

pref_probabilities = {
    'Conservative': {
        'Labour': 0.3,
        'Liberal Democrat': 0.25,
        'Green Party': 0.08,
        'UKIP': 0.37
    },
    'Green Party': {
        'Conservative': 0.17,
        'Labour': 0.63,
        'Liberal Democrat': 0.18,
        'UKIP': 0.02
    },
    'Labour': {
        'Conservative': 0.28,
        'Liberal Democrat': 0.24,
        'Green Party': 0.35,
        'UKIP': 0.13
    },
    'Liberal Democrat': {
        'Conservative': 0.42,
        'Labour': 0.36,
        'Green Party': 0.17,
        'UKIP': 0.01
    },
    'UKIP': {
        'Conservative': 0.64,
        'Labour': 0.19,
        'Liberal Democrat': 0.04,
        'Green Party': 0.13
    }
}


def generate_preferences(main_party):
    def get_next_preference(preferences):
        party = preferences[-1]
        dist = pref_probabilities[party]
        left_parties = list(set(considered_parties) - set(preferences))
        total = sum([dist[p] for p in left_parties])
        new_dist = {}

        r = random.random()
        acc = 0
        for p in left_parties:
            new_dist[p] = dist[p] / total
            if r < acc + new_dist[p]:
                return p
            acc += new_dist[p]

    preferences = [main_party]
    for i in range(len(considered_parties) - 1):
        preferences.append(get_next_preference(preferences))
    return ':'.join(map(lambda p: str(considered_parties[p]), preferences))


considered_parties = {
    'Labour': 0,
    'UKIP': 1,
    'Conservative': 2,
    # 'Plaid Cymru': 3,
    'Liberal Democrat': 3,
    'Green Party': 4,
    # 'Scottish National Party': 6,
}

# scotland = {
#     'Labour': 0,
#     'UKIP': 0,
#     'Conservative': 0,
#     'Plaid Cymru': 0,
#     'Liberal Democrat': 0,
#     'Green Party': 0,
#     'Scottish National Party': 0,
#     'Total votes': 0,
#     'Total voters': 0
# }
# wales = {
#     'Labour': 0,
#     'UKIP': 0,
#     'Conservative': 0,
#     'Plaid Cymru': 0,
#     'Liberal Democrat': 0,
#     'Green Party': 0,
#     'Scottish National Party': 0,
#     'Total votes': 0,
#     'Total voters': 0
# }

rows = []

with open('2015.csv') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        if row['Country'] == 'England':
            rows.append(row)

voters = []
constituencies = []
districts = []
seats = 0
total_population = 0

for constituency, results in itertools.groupby(rows, lambda x: x['Constituency']):
    results = list(results)
    electorate_size = get_int(results[0]['Turnout'])
    constituencies.append({
        'districtID': len(constituencies),
        'nseats': 1
    })
    districts.append({
        'districtID': len(constituencies),
        'nseats': electorate_size
    })
    seats += 1
    total_population += electorate_size
    for result in results:
        if result['Party'] not in considered_parties:
            electorate_size -= get_int(result['Votes'])
        else:
            if result['Place'] == '1' and result['Party'] == 'UKIP':
                print 'hello'
            voters.append({
                'district': len(constituencies) - 1,
                'preferences': generate_preferences(result['Party']),
                'size': get_int(result['Votes'])
            })

for d in districts:
    d['nseats'] = d['nseats'] * seats // total_population

with open('constituencies.csv', 'w') as csvfile:
    fieldnames = ['districtID', 'nseats']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(constituencies)

with open('districts.csv', 'w') as csvfile:
    fieldnames = ['districtID', 'nseats']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(districts)

with open('voters.csv', 'w') as csvfile:
    fieldnames = ['size', 'district', 'preferences']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(voters)
