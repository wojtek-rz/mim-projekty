package main

import (
	"github.com/pborman/uuid"
	"gorm.io/gorm"
)

type User struct {
	ID       string `json:"id" gorm:"primary_key"`
	Email    string `json:"email" gorm:"unique"`
	Password string `json:"password"`
	Verified bool   `json:"verified"`
}

func generateUserId(db *gorm.DB) (string, error) {
	id := uuid.New()
	return id, nil
}

func saveNewUser(db *gorm.DB, user *User) (*User, error) {
	var err error
	user.ID, err = generateUserId(db)
	if err != nil {
		return nil, err
	}

	err = db.Create(&user).Error
	if err != nil {
		return nil, err
	}
	return user, nil
}

func findAllUsers(db *gorm.DB) ([]*User, error) {
	var users []*User
	err := db.Find(&users).Error
	if err != nil {
		return nil, err
	}
	return users, nil
}

func findUserById(db *gorm.DB, id string) (*User, error) {
	var user User
	err := db.First(&user, "id = ?", id).Error
	if err != nil {
		return nil, err
	}
	return &user, nil
}

func findUserByEmail(db *gorm.DB, email string) (*User, error) {
	var user User
	err := db.First(&user, "email = ?", email).Error
	if err != nil {
		return nil, err
	}
	return &user, nil
}

func removeUserById(db *gorm.DB, id string) error {
	return db.Delete(&User{}, "id = ?", id).Error
}

func verifyUserById(db *gorm.DB, id string) error {
	return db.Model(&User{}).Where("id = ?", id).Update("verified", true).Error
}

func saveVerificationCode(id string, code string) error {
	client := getRedisClient()
	return client.Set(id, code, 0).Err()
}

func getVerificationCode(id string) (string, error) {
	client := getRedisClient()
	return client.Get(id).Result()
}

func removeVerifiationCode(id string) error {
	client := getRedisClient()
	return client.Del(id).Err()
}
