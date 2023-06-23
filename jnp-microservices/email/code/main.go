package main

import (
	"fmt"
	"github.com/adjust/rmq/v5"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"
)

const (
	redisDb       = 0
	prefetchLimit = 5
	pollDuration  = 1 * time.Second
)

func getRedisAddr() string {
	redisHost := os.Getenv("REDIS_HOST")
	redisPort := os.Getenv("REDIS_PORT")
	return fmt.Sprintf("%s:%s", redisHost, redisPort)
}

func main() {
	errChan := make(chan error, 10)
	go logErrors(errChan)

	connection, err := rmq.OpenConnection("consumer", "tcp", getRedisAddr(), redisDb, errChan)
	if err != nil {
		panic(err)
	}

	queue, err := connection.OpenQueue("emails-queue")
	if err != nil {
		panic(err)
	}

	err = queue.StartConsuming(prefetchLimit, pollDuration)
	if err != nil {
		panic(err)
	}

	_, err = queue.AddConsumerFunc("consumer", consumerFunc)
	if err != nil {
		panic(err)
	}

	fmt.Println("consumer started...")

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, syscall.SIGINT)
	defer signal.Stop(signals)

	<-signals // wait for signal
	go func() {
		<-signals // hard exit on second signal (in case shutdown gets stuck)
		os.Exit(1)
	}()

	<-connection.StopAllConsuming() // wait for all Consume() calls to finish
}

func logErrors(errChan <-chan error) {
	for err := range errChan {
		switch err := err.(type) {
		case *rmq.HeartbeatError:
			if err.Count == rmq.HeartbeatErrorLimit {
				log.Print("heartbeat error (limit): ", err)
			} else {
				log.Print("heartbeat error: ", err)
			}
		case *rmq.ConsumeError:
			log.Print("consume error: ", err)
		case *rmq.DeliveryError:
			log.Print("delivery error: ", err.Delivery, err)
		default:
			log.Print("other error: ", err)
		}
	}
}
