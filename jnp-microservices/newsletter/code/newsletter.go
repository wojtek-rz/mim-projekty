package main

import (
	"github.com/pborman/uuid"
	"gorm.io/gorm"
)

type Newsletter struct {
	ID       string `json:"id" gorm:"primary_key"`
	AuthorId string `json:"author_id" gorm:"not null"`
	Title    string `json:"title"`
	Content  string `json:"content"`
}

type NewsletterRecipient struct {
	NewsletterId   string `json:"newsletter_id" gorm:"primary_key"`
	RecipientId    string `json:"recipient_id" gorm:"primary_key"`
	RecipientEmail string `json:"recipient_email" gorm:"not null"`
}

func generateNewsletterId(db *gorm.DB) string {
	id := uuid.New()
	// while id exists generate new id
	for newsl, _ := findNewsletterById(db, id); newsl != nil; {
		id = uuid.New()
	}

	return id
}

func saveNewNewsletter(db *gorm.DB, newsletter *Newsletter) (*Newsletter, error) {
	newsletter.ID = generateNewsletterId(db)
	err := db.Create(&newsletter).Error
	if err != nil {
		return nil, err
	}
	return newsletter, nil
}

func findNewsletterById(db *gorm.DB, id string) (*Newsletter, error) {
	var newsletter Newsletter
	err := db.First(&newsletter, id).Error
	if err != nil {
		return nil, err
	}
	return &newsletter, nil
}

func genereateRecipientId(db *gorm.DB, newsletterId string) (string, error) {
	id := uuid.New()

	var recipient *NewsletterRecipient
	if err := db.First(recipient, "newsletter_id = ? AND recipient_id = ?", newsletterId, id).Error; err != nil {
		return "", err
	}

	for recipient != nil {
		id = uuid.New()
		if err := db.First(recipient, "newsletter_id = ? AND recipient_id = ?", newsletterId, id).Error; err != nil {
			return "", err
		}
	}

	return id, nil
}

func addRecipient(db *gorm.DB, recipient *NewsletterRecipient) (*NewsletterRecipient, error) {
	var err error
	recipient.RecipientId, err = genereateRecipientId(db, recipient.NewsletterId)
	if err != nil {
		return nil, err
	}
	err = db.Create(&recipient).Error
	if err != nil {
		return nil, err
	}
	return recipient, nil
}

func removeRecipient(db *gorm.DB, newsletterId string, recipientId string) error {
	err := db.Delete(&NewsletterRecipient{}, "newsletter_id = ? AND recipient_id = ?", newsletterId, recipientId).Error
	if err != nil {
		return err
	}
	return nil
}

func findRecipients(db *gorm.DB, newsletterId string) ([]string, error) {
	var recipients []NewsletterRecipient
	err := db.Find(&recipients, "newsletter_id = ?", newsletterId).Error
	if err != nil {
		return nil, err
	}

	var emails []string
	for _, recipient := range recipients {
		emails = append(emails, recipient.RecipientEmail)
	}

	return emails, nil
}

func removeNewsletter(db *gorm.DB, id string) error {
	err := db.Delete(&Newsletter{}, id).Error
	if err != nil {
		return err
	}

	// remove all recipients
	err = db.Delete(&NewsletterRecipient{}, "newsletter_id = ?", id).Error
	if err != nil {
		return err
	}

	return nil
}
