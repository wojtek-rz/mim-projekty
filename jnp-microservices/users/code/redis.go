package main

import (
	"github.com/go-redis/redis"
	"strconv"

	"encoding/json"
	"fmt"
	"os"
)

var (
	UserDataSet  = "users"
	UserEmailSet = "users-emails"
	UserIDCount  = "users-id"
)

func getRedisClient() *redis.Client {
	redisHost := os.Getenv("REDIS_HOST")
	redisPort := os.Getenv("REDIS_PORT")
	redisAddr := fmt.Sprintf("%s:%s", redisHost, redisPort)

	// Create a Redis client
	client := redis.NewClient(&redis.Options{
		Addr:     redisAddr, // Replace with your Redis server address
		Password: "",        // Set password if applicable
		DB:       0,         // Select the appropriate Redis database
	})
	return client
}

func genereateUserId(client *redis.Client) (string, error) {
	id, err := client.Incr(UserIDCount).Result()
	if err != nil {
		return "", err
	}

	return strconv.FormatInt(id, 10), nil
}

func createUser(user *User) (*User, error) {
	client := getRedisClient()
	id, err := genereateUserId(client)
	if err != nil {
		return nil, err
	}
	user.ID = id

	json_user, err := json.Marshal(user)
	if err != nil {
		return nil, err
	}

	// czy użytkownik o podanym emailu już istnieje?
	_, err = client.HGet(UserEmailSet, user.Email).Result()
	if err == nil {
		return nil, fmt.Errorf("User already exists")
	}

	tx := client.TxPipeline()
	tx.HSet(UserDataSet, user.ID, json_user)
	tx.HSet(UserEmailSet, user.Email, user.ID)

	_, err = tx.Exec()
	if err != nil {
		return nil, err
	}

	return user, nil
}

func getUser(id string) (*User, error) {
	client := getRedisClient()
	json_user, err := client.HGet(UserDataSet, id).Result()
	if err != nil {
		return nil, err
	}
	// create user struct
	user := &User{}
	err = json.Unmarshal([]byte(json_user), user)
	if err != nil {
		return nil, err
	}
	return user, nil
}

func getUserByEmail(email string) (*User, error) {
	client := getRedisClient()

	id, err := client.HGet(UserEmailSet, email).Result()
	if err != nil {
		return nil, err
	}

	json_user, err := client.HGet(UserDataSet, id).Result()

	if err != nil {
		return nil, err
	}
	// create user struct
	user := &User{}
	err = json.Unmarshal([]byte(json_user), user)
	if err != nil {
		return nil, err
	}
	return user, nil
}

func deleteUser(id string) error {
	client := getRedisClient()

	fmt.Printf("Deleting user %s\n", id)

	user, err := getUser(id)
	if err != nil {
		return err
	}

	tx := client.TxPipeline()
	tx.HDel(UserDataSet, user.ID)
	tx.HDel(UserEmailSet, user.Email)
	_, err = tx.Exec()

	return err
}
