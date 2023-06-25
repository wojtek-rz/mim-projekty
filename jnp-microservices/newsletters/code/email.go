package main

import (
	"encoding/json"
	"fmt"
	"github.com/adjust/rmq/v5"
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

func sendEmails(queue *rmq.Queue, email *Email, recipients []string) error {
	for _, recipient := range recipients {
		email.To = recipient
		err := sendToMailQueue(queue, email)
		if err != nil {
			return err
		}
		fmt.Println("Sent email to", recipient, "errno = ", err)
	}

	return nil
}
