package main

import (
	"fmt"
	"github.com/adjust/rmq/v5"
	"github.com/gin-gonic/gin"
	"gorm.io/gorm"
	"strings"

	"net/http"
)

type RouterData struct {
	pdb *gorm.DB   // postgres database
	mq  *rmq.Queue // mails queue
}

func (rd *RouterData) createUserRoute(c *gin.Context) {
	var user User
	if err := c.ShouldBindJSON(&user); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}

	savedUser, err := saveNewUser(rd.pdb, &user)
	if err != nil {
		if strings.Contains(err.Error(), "unique") {
			c.JSON(http.StatusConflict, gin.H{"error": "User already exists"})
		} else {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		}
		return
	}

	// send verification email
	err = sendVerificationEmail(rd.mq, savedUser)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.JSON(http.StatusCreated, savedUser)
}

func (rd *RouterData) getUserRoute(c *gin.Context) {
	id := c.Param("id")

	user, err := findUserById(rd.pdb, id)
	if err != nil {
		c.JSON(http.StatusNotFound, gin.H{"error": "User not found"})
		return
	}

	c.JSON(http.StatusOK, user)
}

func (rd *RouterData) getUsersRoute(c *gin.Context) {
	users, err := findAllUsers(rd.pdb)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	c.JSON(http.StatusOK, users)
}

func (rd *RouterData) deleteUserRoute(c *gin.Context) {
	id := c.Param("id")

	if err := removeUserById(rd.pdb, id); err != nil {
		c.JSON(http.StatusNotFound, gin.H{"error": "User not found"})
		return
	}

	c.JSON(http.StatusOK, gin.H{"message": "User deleted"})
}

type AuthToken struct {
	Token string `json:"token"`
}

func (rd *RouterData) authUserRoute(c *gin.Context) {
	var user User
	var authResponse AuthToken
	if err := c.ShouldBindJSON(&user); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		return
	}

	err := verifyUser(rd.pdb, &user)
	if err != nil {
		c.JSON(http.StatusUnauthorized, gin.H{"error": "Unauthorized"})
		return
	}

	tokenString, err := generateToken(user)

	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	authResponse.Token = tokenString
	c.JSON(http.StatusOK, authResponse)
}

func (rd *RouterData) getUserFromAuthRoute(c *gin.Context) {
	tokenString := c.GetHeader("Authorization")

	if len(tokenString) < 8 || tokenString[:7] != "Bearer " {
		c.JSON(http.StatusUnauthorized, gin.H{"error": "Unauthorized"})
		return
	}

	userData, err := verifyToken(tokenString[7:]) // remove "Bearer " from token
	if err != nil {
		c.JSON(http.StatusUnauthorized, gin.H{"error": "Unauthorized"})
		return
	}
	fmt.Println("email, err: ", userData.Email, err)

	c.JSON(http.StatusOK, userData)
}

func (rd *RouterData) verifyUserRoute(c *gin.Context) {
	id := c.Param("id")
	code := c.Param("code")

	valid_code, err := getVerificationCode(id)
	if err != nil {
		c.JSON(http.StatusNotFound, gin.H{"error": "User not found"})
		return
	}

	if code != valid_code {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Wrong verification code"})
		return
	}

	if err := verifyUserById(rd.pdb, id); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}

	_ = removeVerifiationCode(id)

	c.JSON(http.StatusOK, gin.H{"message": "User verified"})
}
