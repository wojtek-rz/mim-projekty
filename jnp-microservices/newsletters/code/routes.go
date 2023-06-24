package main

import (
	"github.com/gin-gonic/gin"
	"gorm.io/gorm"

	"net/http"
)

type RouterData struct {
	DB *gorm.DB
}

type CreateNewsletterRequest struct {
	Title   string `json:"title"`
	Content string `json:"content"`
}

func (rd *RouterData) createNewsletterRoute(c *gin.Context) {
	var newsletterRequest CreateNewsletterRequest
	if err := c.ShouldBindJSON(&newsletterRequest); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}
	userData, err := get_user_data(c)
	if err != nil {
		c.JSON(http.StatusUnauthorized, gin.H{"error": "Unauthorized"})
		return
	}

	// send get request to user service to get user id
	var newsletter = Newsletter{
		AuthorId: userData.ID,
		Title:    newsletterRequest.Title,
		Content:  newsletterRequest.Content,
	}

	newNewsletter, err := saveNewNewsletter(rd.DB, &newsletter)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.JSON(http.StatusCreated, newNewsletter)
}

type ResponseNewsletter struct {
	Newsletter Newsletter `json:"data"`
	Recipients []string   `json:"recipients"`
}

func (rd *RouterData) getNewsletterRoute(c *gin.Context) {
	id := c.Param("id")

	newsletter, err := findNewsletterById(rd.DB, id)
	if err != nil {
		c.JSON(http.StatusNotFound, gin.H{"error": "Newsletter not found"})
		return
	}

	recipients, err := findRecipients(rd.DB, id)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	response := ResponseNewsletter{
		Newsletter: *newsletter,
		Recipients: recipients,
	}

	c.JSON(http.StatusOK, response)
}

func (rd *RouterData) deleteNewsletterRoute(c *gin.Context) {
	id := c.Param("id")

	err := removeNewsletter(rd.DB, id)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.Status(http.StatusOK)
}

type AddRecipientRequest struct {
	Email string `json:"email"`
}

func (rd *RouterData) addRecipientRoute(c *gin.Context) {

	var addRecipientRequest AddRecipientRequest
	if err := c.ShouldBindJSON(&addRecipientRequest); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}

	var recipient NewsletterRecipient
	recipient.RecipientEmail = addRecipientRequest.Email
	recipient.NewsletterId = c.Param("id")

	newRecipient, err := addRecipient(rd.DB, &recipient)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.JSON(http.StatusCreated, newRecipient)
}

func (rd *RouterData) removeRecipientRoute(c *gin.Context) {
	nid := c.Param("id")
	rid := c.Param("rid")

	err := removeRecipient(rd.DB, nid, rid)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.Status(http.StatusOK)
}
