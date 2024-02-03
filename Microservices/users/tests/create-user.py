import requests

email = 'w.r.mirror@gmail.com'
password = 'password123'

data = {
    'email': email,
    'password': password,
}

baseUsersUrl = 'http://localhost:8001/users/'
response = requests.post(baseUsersUrl, json=data)
print(response.status_code)
print(response.json())