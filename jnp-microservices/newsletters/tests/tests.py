import requests
import unittest


class APITestCase(unittest.TestCase):
    base_url = 'http://localhost:8000/newsletters'

    def get_auth_token(self):
        users_url = 'http://localhost:8001/users'
        auth_url = 'http://localhost:8001/auth'
        user = {
            'email': 'example@example.com',
            'password': '123pass',
        }
        # if not exists, create user
        response = requests.post(f'{users_url}', json=user)
        if response.status_code == 201:
            self.user_id = response.json()['id']
        
        # login user
        response = requests.post(f'{auth_url}', json=user)
        if response.status_code == 200:
            token = response.json()['token']
            return token
        return ""
    
    def get_headers(self):
        if hasattr(self, 'headers'):
            return self.headers

        self.headers = {"Authorization": f"Bearer {self.get_auth_token()}"}
        return self.headers

    def get_newsletter_by_id(self, news_id):
        url = f'{self.base_url}/{news_id}'
        response = requests.get(url)
        return response
    
    def add_newsletter(self, data):
        url = f'{self.base_url}'
        response = requests.post(url, json=data, headers=self.get_headers())
        if response.status_code == 201:
            self.news_id = response.json()['id']
        return response
    
    def delete_newsletter(self, news_id):
        url = f'{self.base_url}/{news_id}'
        response = requests.delete(url, headers=self.get_headers())
        return response
    
    def login_user(self, data):
        url = f'{self.base_url_users}/auth'
        data = {
            'email': self.user_email,
            'password': self.user_password,
        }

    def tearDown(self):
        if hasattr(self, 'news_id') and self.news_id:
            response = self.delete_newsletter(self.news_id)
            self.assertEqual(response.status_code, 200)

    def test1_get_newsletter_by_id(self):
        news_id = 'nonexistent_id'
        response = self.get_newsletter_by_id(news_id)
        
        self.assertEqual(response.status_code, 404)
        self.assertEqual(response.json(), {'error': 'Newsletter not found'})

    def test2_not_authorized(self):
        data = {
            'title': 'First tests',
            'content': 'Super content',
        }
        response = requests.post(f'{self.base_url}', json=data)
        
        self.assertEqual(response.status_code, 401)

    def test3_add_newsletter(self):
        data = {
            'title': 'First tests',
            'content': 'Super content',
        }
        response = self.add_newsletter(data)
        self.assertEqual(response.status_code, 201)
        self.assertEqual(response.json()['title'], data['title'])
        self.assertEqual(response.json()['content'], data['content'])
        # asser that id exists
        self.assertTrue(response.json()['id'])
        
    def test4_add_newsletter_recipient(self):
        data = {
            'title': 'First tests',
            'content': 'Super content',
        }
        response = self.add_newsletter(data)
        self.assertEqual(response.status_code, 201)

        recipient_url = f'{self.base_url}/{self.news_id}/recipients'
        recipient_data = {
            'email': 'samplemail1@gmail.com'
        }
        response = requests.post(recipient_url, json=recipient_data, headers=self.get_headers())
        print(response.json())
        self.assertEqual(response.status_code, 201)
        self.assertEqual(response.json()['recipient_email'], recipient_data['email'])
        # asser that id exists
        self.assertTrue(response.json()['newsletter_id'])
        # assert that recipient id exists
        self.assertTrue(response.json()['recipient_id'])
        print(response.json())

        # remove recipient
        recipient_id = response.json()['recipient_id']
        recipient_url = f'{self.base_url}/{self.news_id}/recipients/{recipient_id}'
        response = requests.delete(recipient_url, headers=self.get_headers())
        self.assertEqual(response.status_code, 200)


if __name__ == '__main__':
    unittest.main()
