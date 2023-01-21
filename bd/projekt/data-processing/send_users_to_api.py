# find distinct contributor_id in recipes_table
import json
import requests
from tqdm import tqdm
import pandas as pd


# read recipes table
recipes = pd.read_csv('result/recipes_full.csv')
users = recipes[['AuthorId', 'AuthorName']].drop_duplicates()
print('users:',len(users))
print('recipes:', len(recipes))

# change the name of the first column "id"
users.columns = ['id', 'username']

users['username'] = users['username'].str.replace(" ", "")

# add email column which is equal to id
users['email'] = users['username'].str.lower() + '@unknown.com'
users['password'] = 'pass' + users['username'].str.lower()

# for each row send post request to /api/users and store the response in users_table
users_in_api = pd.DataFrame()
for index, row in tqdm(users.iterrows(), total=len(users)):
    # change request dat to json string
    request_data = json.dumps({ 'email': row['email'], 'password': row['password'], 'username': row['username'] })
    response = requests.post('http://127.0.0.1:8000/users/', data=request_data, 
    headers={'Content-Type': 'application/json', 'Accept': 'application/json'})
    
    new_row = pd.DataFrame({'local_id': row['id'], 'api_id': response.json().get('id')}, index=[0])

    users_in_api = pd.concat([users_in_api, new_row], ignore_index=True)
    if (index % 100 == 0):
        print(index)


# save users_in_api to csv
users_in_api.to_csv('result/users_in_api.csv', index=False)
