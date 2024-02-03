package main

import (
	"encoding/json"
	"fmt"
	"github.com/adjust/rmq/v5"
	"github.com/pborman/uuid"
	"os"
)

type Email struct {
	Subject string `json:"subject"`
	Body    string `json:"body"`
	To      string `json:"to"`
}

func sendToMailQueue(queue *rmq.Queue, email *Email) error {
	email_json, err := json.Marshal(email)
	if err != nil {
		return err
	}
	err = (*queue).PublishBytes(email_json)
	return err
}

func getUserBaseAddress() string {
	return os.Getenv("PUBLIC_ADDR")
}

func getVerificationMailBody(verification_url string) string {
	return fmt.Sprintf("Please verify your email by clicking <a href=\"%s\">here</a>.", verification_url)
}

func sendVerificationEmail(queue *rmq.Queue, user *User) error {
	code := uuid.New()
	err := saveVerificationCode(user.ID, code)
	if err != nil {
		return err
	}
	verification_url := fmt.Sprintf("%s/users/%s/verify/%s", getUserBaseAddress(), user.ID, code)
	email := Email{
		Subject: "Verify your email",
		Body:    getVerificationMailBody(verification_url),
		To:      user.Email,
	}
	err = sendToMailQueue(queue, &email)
	fmt.Println("Sent verification email to", user.Email, "errno = ", err)

	return err
}
