import requests
import unittest

class APITestCase(unittest.TestCase):
    base_url = 'http://localhost:8000'

    def get_user_by_id(self, user_id):
        url = f'{self.base_url}/users/{user_id}'
        response = requests.get(url)
        return response
    
    def add_user(self, data):
        url = f'{self.base_url}/users'
        response = requests.post(url, json=data)
        if response.status_code == 201:
            self.user_id = response.json()['id']
        return response
    
    def delete_user(self, user_id):
        url = f'{self.base_url}/users/{user_id}'
        response = requests.delete(url)
        return response

    def tearDown(self):
        if hasattr(self, 'user_id') and self.user_id:
            response = self.delete_user(self.user_id)
            self.assertEqual(response.status_code, 200)
    
    def test1_get_user_not_found(self):
        user_id = 'nonexistent_id'
        response = self.get_user_by_id(user_id)
        
        self.assertEqual(response.status_code, 404)
        self.assertEqual(response.json(), {'error': 'User not found'})
    
    def test2_add_user(self):
        data = {
            'email': 'test@example.com',
            'password': 'password123',
            'verified': True
        }
        response = self.add_user(data)
        
        self.assertEqual(response.status_code, 201)
    
    def test3_get_user(self):
        data = {
            'email': 'test@example.com',
            'password': 'password123',
            'verified': True
        }
        response = self.add_user(data)
        self.assertEqual(response.status_code, 201)
        
        response = self.get_user_by_id(self.user_id)
        self.assertEqual(response.status_code, 200)
        
        user = response.json()
        self.assertEqual(user['id'], self.user_id)
        self.assertEqual(user['email'], 'test@example.com')
        self.assertEqual(user['password'], 'password123')
        self.assertEqual(user['verified'], True)
    
    def test4_add_user_duplicate_email(self):
        data = {
            'email': 'test@example.com',
            'password': 'anotherpassword',
            'verified': False
        }
        response = self.add_user(data)
        self.assertEqual(response.status_code, 201)

        response = self.add_user(data)
        self.assertEqual(response.status_code, 409)
        self.assertEqual(response.json(), {'error': 'User already exists'})

    def test5_get_user_not_found(self):
        data = {
            'email': 'test@example.com',
            'password': 'password123',
            'verified': True
        }
        response = self.add_user(data)
        self.assertEqual(response.status_code, 201)

        response = self.delete_user(self.user_id)
        self.assertEqual(response.status_code, 200)

        self.user_id = None

    def test6_login(self):
        data = {
            'email': 'test@example.com',
            'password': 'anotherpassword',
            'verified': False
        }
        login_data = {
            'email': 'test@example.com',
            'password': 'anotherpassword',
        }

        response = self.add_user(data)
        self.assertEqual(response.status_code, 201)

        url = f'{self.base_url}/auth'
        response = requests.post(url, json=login_data)
        self.assertEqual(response.status_code, 200)
        self.assertIsNotNone(response.json()['token'])
    
    def test7_login_incorrect(self):
        data = {
            'email': 'test@example.com',
            'password': 'anotherpassword',
            'verified': False
        }
        login_data = {
            'email': 'test@example.com',
            'password': 'anotherpasswordwrong',
        }

        response = self.add_user(data)
        self.assertEqual(response.status_code, 201)

        url = f'{self.base_url}/auth'
        response = requests.post(url, json=login_data)
        self.assertEqual(response.status_code, 401)

    def test8_check_token(self):
        data = {
            'email': 'test@example.com',
            'password': 'anotherpassword',
            'verified': False
        }
        login_data = {
            'email': 'test@example.com',
            'password': 'anotherpassword',
        }

        response = self.add_user(data)
        self.assertEqual(response.status_code, 201)

        url = f'{self.base_url}/auth'
        response = requests.post(url, json=login_data)
        self.assertEqual(response.status_code, 200)
        token = response.json()['token']

        url = f'{self.base_url}/auth/check'
        headers = {"Authorization": f"Bearer {token}"}
        response = requests.get(url, headers=headers)
        self.assertEqual(response.status_code, 200)
    
    def test9_check_token_incorrect(self):
        token = 'incorrect_token'
        url = f'{self.base_url}/auth/check'

        headers = {"Authorization": f"Bearer {token}"}
        response = requests.get(url, headers=headers)
        self.assertEqual(response.status_code, 401)


if __name__ == '__main__':
    unittest.main()
