import pandas as pd


f3135accuracy = pd.read_csv('pilot.csv')
# keep the ID, response, attempt, name, and correct column only
f3135accuracy = f3135accuracy[["Participant Public ID", "Response", "Attempt", "Correct", "Name"]]
# Only want the rows which has 1 in Attempt
f3135accuracy = f3135accuracy[f3135accuracy["Attempt"] == 1]


def decide_accuracy(df):
    response=df[0]
    name=df[1]
    correct=df[2]
    if (name == 'Nonpeer_80/20' and response == 'F_56-60_3.jpg') \
        or (name == 'Nonpeer_70/30' and response == 'F_56-60_4.jpg') \
        or (name == 'Peer_80/20' and response == 'F_31-35_2.jpg') \
        or (name == 'Peer_70/30' and response == 'F_31-35_1.jpg'):
        return 1
    if not isinstance(name, str):
        return correct
    return 0


f3135accuracy['accuracyornot']=f3135accuracy[['Response', 'Name', 'Correct']].apply(decide_accuracy, axis=1)
f3135accuracy = f3135accuracy[['Participant Public ID', 'accuracyornot']]
f3135accuracy['index'] = f3135accuracy.groupby(['Participant Public ID']).cumcount()+1
f3135accuracy = f3135accuracy.pivot(index='Participant Public ID', columns='index', values='accuracyornot')

print(f3135accuracy)

f3135accuracy.to_csv('pilotresult.csv')