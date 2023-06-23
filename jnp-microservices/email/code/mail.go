package main

import (
	"fmt"
	"net/smtp"
	"os"
)

func sendMail(subject string, body string, to []string) error {
	auth := smtp.PlainAuth(
		"",
		os.Getenv("GMAIL_LOGIN"),
		os.Getenv("GMAIL_APP_PASSWORD"),
		os.Getenv("GMAIL_HOST"),
	)

	msg := "Subject: " + subject + "\n" + body

	err := smtp.SendMail(
		os.Getenv("GMAIL_ADDR"),
		auth,
		os.Getenv("GMAIL_FROM_ADDR"),
		to,
		[]byte(msg),
	)

	if err != nil {
		fmt.Println("error sending email", err)
		return err
	}

	fmt.Println("email ", subject, " sent to ", to)
	return nil
}
