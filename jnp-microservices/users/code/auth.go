package main

import (
	"fmt"
	"github.com/golang-jwt/jwt/v5"
	"gorm.io/gorm"
	"time"
)

var (
	SecretKey = "polski-tajny-klucz"
)

type TokenUserData struct {
	Email string `json:"email"`
	Id    string `json:"id"`
}

func verifyUser(db *gorm.DB, user *User) error {
	dbUser, err := findUserByEmail(db, user.Email)
	if err != nil {
		return err
	}
	fmt.Println("passwords", dbUser.Password, user.Password)
	if dbUser.Password != user.Password {
		return fmt.Errorf("Invalid password")
	}

	user.ID = dbUser.ID
	return nil
}

func generateToken(user *User) (string, error) {
	fmt.Println("Generating token for user", user)
	// Insert code to create JSON Web Token
	t := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
		"id":    user.ID,
		"email": user.Email,
		"exp":   time.Now().Add(time.Hour * 24).Unix(),
	})
	s, err := t.SignedString([]byte(SecretKey))
	if err != nil {
		return "", err
	}
	return s, nil
}

func verifyToken(tokenString string) (*TokenUserData, error) {
	userData := TokenUserData{}

	token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("Invalid token signing method")
		}

		return []byte(SecretKey), nil
	})
	if err != nil {
		return nil, err
	}
	if !token.Valid {
		return nil, fmt.Errorf("Invalid token")
	}

	claims := token.Claims.(jwt.MapClaims)
	userData.Id = claims["id"].(string)
	userData.Email = claims["email"].(string)
	exp := claims["exp"].(float64)

	if time.Now().Unix() > int64(exp) {
		return nil, fmt.Errorf("Token expired")
	}

	return &userData, nil
}
