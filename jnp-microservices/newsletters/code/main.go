package main

import (
	"github.com/gin-gonic/gin"
	"os"
)

func main() {
	var err error
	router := gin.Default()

	rd := RouterData{}
	rd.DB, err = connectDB()
	if err != nil {
		panic("Failed to connect to database!")
	}

	err = migrateNewsletter(rd.DB)
	if err != nil {
		return
	}

	router.POST("/newsletters", rd.createNewsletterRoute)

	router.GET("/newsletters/:id", rd.getNewsletterRoute)
	router.DELETE("/newsletters/:id", rd.deleteNewsletterRoute)

	router.POST("/newsletters/:id/recipients", rd.addRecipientRoute)
	router.DELETE("/newsletters/:id/recipients/:rid", rd.removeRecipientRoute)

	port := os.Getenv("APP_PORT")
	err = router.Run(":" + port)
	if err != nil {
		panic("Failed to start server!")
	}
}
