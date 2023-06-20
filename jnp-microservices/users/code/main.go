package main

import (
	"fmt"
	"log"
	"net/http"
	"os"

	"github.com/go-redis/redis"
)

func main() {
	// get environment variables
	redisHost := os.Getenv("REDIS_HOST")
	redisPort := os.Getenv("REDIS_PORT")
	port := os.Getenv("PORT")

	// Create a Redis client
	client := redis.NewClient(&redis.Options{
		Addr:      fmt.Sprintf("%s:%s", redisHost, redisPort), // Replace with your Redis server address
		Password: "",               // Set password if applicable
		DB:       0,                // Select the appropriate Redis database
	})

	// Check the connection to Redis
	pong, err := client.Ping().Result()
	if err != nil {
		log.Fatalf("Failed to connect to Redis: %v", err)
	}
	log.Printf("Connected to Redis: %s", pong)
	client.Set("mydata", "Hello World", 0)

	// Define the HTTP handler function
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		// Get data from Redis
		data, err := client.Get("mydata").Result()
		if err != nil {
			http.Error(w, "Failed to retrieve data from Redis", http.StatusInternalServerError)
			return
		}

		// Return the data in the HTTP response
		fmt.Fprintf(w, "Data from Redis: %s", data)
	})

	// Start the HTTP server
	log.Println("Starting server on http://localhost:", port)
	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%s", port), nil))
}
