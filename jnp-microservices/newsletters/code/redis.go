package main

import (
	"fmt"
	"github.com/adjust/rmq/v5"
	"os"
)

type RedisConfig struct {
	Host           string
	Port           string
	Password       string
	Db             int
	EmailQueueName string
}

func getRedisConfig() *RedisConfig {
	config := RedisConfig{
		Host:           os.Getenv("REDIS_HOST"),
		Port:           os.Getenv("REDIS_PORT"),
		Password:       "",
		Db:             0,
		EmailQueueName: os.Getenv("EMAILS_QUEUE_NAME"),
	}
	return &config
}

func connectToMailQueue() (*rmq.Queue, error) {
	config := getRedisConfig()
	redisAddr := fmt.Sprintf("%s:%s", config.Host, config.Port)

	conn, err := rmq.OpenConnection("producer-user", "tcp", redisAddr, config.Db, nil)
	if err != nil {
		return nil, err
	}
	queue, err := conn.OpenQueue(config.EmailQueueName)

	return &queue, nil
}
