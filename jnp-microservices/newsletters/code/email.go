package main

import (
	"encoding/json"
	"fmt"
	"github.com/adjust/rmq/v5"
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

func getResignationLink(newsletter_id string, recipient_id string) string {
	link := fmt.Sprintf("%s/newsletters/%s/recipients/%s/remove", os.Getenv("PUBLIC_ADDR"),
		newsletter_id, recipient_id)
	return fmt.Sprintf("\n\n<br><br>If you want to leave this newsletter click link <a href=\"%s\">here</a>.", link)
}

func sendEmails(queue *rmq.Queue, email *Email, recipients []NewsletterRecipient) error {
	for _, recipient := range recipients {
		rec_email := Email{
			To:      recipient.RecipientEmail,
			Subject: email.Subject,
			Body:    email.Body + getResignationLink(recipient.NewsletterId, recipient.RecipientId),
		}
		err := sendToMailQueue(queue, &rec_email)
		if err != nil {
			return err
		}
		fmt.Println("Sent email to", recipient, "errno = ", err)
	}

	return nil
}
