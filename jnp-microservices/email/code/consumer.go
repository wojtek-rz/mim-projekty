package main

import (
	"encoding/json"
	"fmt"
	"github.com/adjust/rmq/v5"
	"log"
	"math/rand"
	"time"
)

type Email struct {
	Subject      string `json:"subject"`
	Body         string `json:"body"`
	To           string `json:"to"`
	NewsletterId string `json:"newsletter_id"`
}

func convertToEmail(payload string) (*Email, error) {
	var email Email
	err := json.Unmarshal([]byte(payload), &email)
	if err != nil {
		return nil, err
	}
	return &email, nil
}

const (
	HourlyLimit = 15
)

var (
	counter         = 0
	last_reset_time = time.Now()
)

func limitTimesPerHour() {
	if counter >= HourlyLimit {
		time.Sleep(time.Until(last_reset_time.Add(1 * time.Hour)))
	}
	if time.Since(last_reset_time) > 1*time.Hour {
		counter = 0
		last_reset_time = time.Now()
	}

	time.Sleep(time.Duration(100+rand.Intn(400)) * time.Millisecond)
	counter++
}

func consumerFunc(delivery rmq.Delivery) {
	limitTimesPerHour()

	log.Printf("received %s", delivery.Payload())
	email, err := convertToEmail(delivery.Payload())
	if err != nil {
		fmt.Println("error converting to email", err)
		return
	}

	fmt.Println("email ", email.Subject, " sent to ", email.To)
	err = sendMail(email.Subject, email.Body, []string{email.To})
	if err != nil {
		fmt.Println("error sending email", err)
		return
	}

	err = delivery.Ack()
	if err != nil {
		fmt.Println("ack error:", err)
	} else {
		fmt.Println("ack success")
	}
}
