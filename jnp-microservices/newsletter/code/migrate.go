package main

import (
	"fmt"
)

func main() {
	fmt.Println("? Starting migration")
	ConnectDB()
	err := DB.AutoMigrate(&Newsletter{}, &NewsletterRecipient{})
	if err != nil {
		fmt.Println("Error migrating the database")
		return
	}
	fmt.Println("? Migration complete")
}
