package main

import (
	"github.com/gin-gonic/gin"
	"os"
)

var users = make(map[string]User)

func main() {
	router := gin.Default()

	router.POST("/users", createUserRoute)
	router.GET("/users/:id", getUserRoute)
	router.DELETE("/users/:id", deleteUserRoute)

	router.POST("/auth", authUser)
	router.GET("/auth/check", checkAuth)

	port := os.Getenv("PORT")
	router.Run(":" + port)
}
