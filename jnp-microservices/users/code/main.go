package main

import (
	"github.com/gin-gonic/gin"
	"os"
)

func main() {
	rd := RouterData{}
	var err error

	rd.pdb, err = connectDB()
	if err != nil {
		panic("Failed to connect to database!")
	}
	rd.mq, err = connectToMailQueue()
	if err != nil {
		panic("Failed to connect to mail queue!")
	}
	err = rd.pdb.AutoMigrate(&User{})
	if err != nil {
		panic("Failed to migrate database!")
	}

	router := gin.Default()
	router.POST("/users", rd.createUserRoute)
	router.GET("/users/:id", rd.getUserRoute)
	router.DELETE("/users/:id", rd.deleteUserRoute)

	router.POST("/auth", rd.authUserRoute)
	router.GET("/auth", rd.getUserFromAuthRoute)

	port := os.Getenv("APP_PORT")
	err = router.Run(":" + port)
	if err != nil {
		panic("Failed to start server!")
		return
	}
}
