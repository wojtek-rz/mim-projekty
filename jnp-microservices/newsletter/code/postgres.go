package main

import (
	"fmt"
	"gorm.io/driver/postgres"
	"gorm.io/gorm"
	"os"
)

type PosgresConfig struct {
	Host     string
	Port     string
	DB       string
	User     string
	Password string
}

func getPostgresConfig() *PosgresConfig {
	config := PosgresConfig{
		Host:     os.Getenv("POSGTRES_HOST"),
		Port:     os.Getenv("POSGTRES_PORT"),
		DB:       os.Getenv("POSGTRES_DB"),
		User:     os.Getenv("POSGTRES_USER"),
		Password: os.Getenv("POSGTRES_PASSWORD"),
	}
	return &config
}

func connectDB() (*gorm.DB, error) {
	config := getPostgresConfig()
	dsn := fmt.Sprintf("host=%s user=%s password=%s dbname=%s port=%s sslmode=disable",
		config.Host,
		config.User,
		config.Password,
		config.DB,
		config.Port,
	)

	DB, err := gorm.Open(postgres.Open(dsn), &gorm.Config{})
	if err != nil {
		return nil, err
	}
	fmt.Println("Connected Successfully to the Database")
	return DB, nil
}
