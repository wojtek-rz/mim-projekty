package main

import (
	"encoding/json"
	"fmt"
	"github.com/gin-gonic/gin"
	"io"
	"net/http"
	"os"
)

type UserData struct {
	ID    string `json:"id"`
	Email string `json:"email"`
}

func get_user_auth_addr() string {
	return os.Getenv("USER_SERVICE_ADDR") + "/auth"
}

func get_user_data(c *gin.Context) (*UserData, error) {
	var user UserData

	req, err := http.NewRequest("GET", get_user_auth_addr(), nil)
	if err != nil {
		fmt.Println("Błąd podczas tworzenia zapytania:", err)
		return nil, err
	}
	req.Header.Add("Authorization", c.GetHeader("Authorization"))
	fmt.Println("Authorization: ", c.GetHeader("Authorization"))
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Println("Błąd podczas wykonywania zapytania:", err)
		return nil, err
	}

	defer func(Body io.ReadCloser) {
		err := Body.Close()
		if err != nil {
			fmt.Println("Błąd podczas zamykania ciała odpowiedzi:", err)
		}
	}(resp.Body)

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Błąd podczas odczytu odpowiedzi:", err)
		return nil, err
	}

	err = json.Unmarshal(body, &user)
	if err != nil {
		fmt.Println("Błąd podczas parsowania odpowiedzi:", err)
		return nil, err
	}

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("Unauthorized")
	}

	return &user, nil
}
