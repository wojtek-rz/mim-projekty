package main

import (
	"fmt"
	"github.com/dgrijalva/jwt-go"
	"time"
)

var (
	SecretKey = "polski-tajny-klucz"
)

func verifyUser(user *User) error {
	dbUser, err := getUserByEmail(user.Email)
	if err != nil {
		return err
	}
	fmt.Println("passwords", dbUser.Password, user.Password)
	if dbUser.Password != user.Password {
		return fmt.Errorf("Invalid password")
	}
	return nil
}

func generateToken(user User) (string, error) {
	// Insert code to create JSON Web Token
	token := jwt.New(jwt.SigningMethodHS256)

	// Ustawienie danych użytkownika jako claim (payload) tokenu
	claims := token.Claims.(jwt.MapClaims)
	claims["email"] = user.Email
	claims["exp"] = time.Now().Add(time.Hour * 24 * 7).Unix() // Token wygaśnie po 7 dniach

	tokenString, err := token.SignedString([]byte(SecretKey))
	if err != nil {
		return "", err
	}
	return tokenString, nil
}

func verifyToken(tokenString string) (string, error) {
	fmt.Println("verifyToken", tokenString)
	token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("Invalid token signing method")
		}

		return []byte(SecretKey), nil
	})
	if err != nil {
		return "", err
	}

	if !token.Valid {
		return "", fmt.Errorf("Invalid token")
	}

	claims := token.Claims.(jwt.MapClaims)
	email := claims["email"].(string)

	return email, nil
}
