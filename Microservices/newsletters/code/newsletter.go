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
	NewsletterId   string `json:"newsletter_id" gorm:"primaryKey"`
	RecipientId    string `json:"recipient_id" gorm:"primaryKey"`
	RecipientEmail string `json:"recipient_email" gorm:"not null; unique"`
}

func migrateNewsletter(db *gorm.DB) error {
	return db.AutoMigrate(&Newsletter{}, &NewsletterRecipient{})
}

func saveNewNewsletter(db *gorm.DB, newsletter *Newsletter) (*Newsletter, error) {
	newsletter.ID = generateNewsletterId(db)
	err := db.Create(&newsletter).Error
	if err != nil {
		return nil, err
	}
	return newsletter, nil
}

func generateNewsletterId(db *gorm.DB) string {
	id := uuid.New()
	return id
}

func findNewsletterById(db *gorm.DB, id string) (*Newsletter, error) {
	var newsletter Newsletter
	err := db.First(&newsletter, "id = ?", id).Error
	if err != nil {
		return nil, err
	}
	return &newsletter, nil
}

func findNewslettersByUserId(db *gorm.DB, userId string) ([]Newsletter, error) {
	var newsletters []Newsletter
	err := db.Find(&newsletters, "author_id = ?", userId).Error
	if err != nil {
		return nil, err
	}
	return newsletters, nil
}

func genereateRecipientId(db *gorm.DB, newsletterId string) (string, error) {
	id := uuid.New()
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

func findRecipients(db *gorm.DB, newsletterId string) ([]NewsletterRecipient, error) {
	var recipients []NewsletterRecipient
	err := db.Find(&recipients, "newsletter_id = ?", newsletterId).Error
	if err != nil {
		return nil, err
	}

	return recipients, nil
}

func removeNewsletter(db *gorm.DB, id string) error {
	err := db.Delete(&Newsletter{}, "id = ?", id).Error
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
