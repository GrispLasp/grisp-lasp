/************************************************************************/
/*				        	                        */
/*	  PmodALS Demo Project 		                                */
/*					    	                        */
/************************************************************************/
/*	Author: Eric Marsh					        */
/*	Copyright 2016, Digilent Inc.					*/
/************************************************************************/
/*  File Description: 			             		        */
/*					                        	*/
/* This file implements a simple demo application that demonstrates     */
/* how to setup and use the PmodALS.				        */
/*									*/
/*	Functionality:							*/
/*									*/
/* In the setup() function, the PmodALS is initialized through          */
/* calling the ALS library.                                             */
/*                                                                      */
/*                                                                      */
/* In the loop() function, the application repeatedly grabs the 
    ambient light level and prints */
/*					       	                        */
/*	Required Hardware:		                                */
/*	  1. PIC32 based Microcontroller    	                        */
/*	  2. PmodALS - plugged into an SPI compatible pins      	*/
/*			                                                */
/************************************************************************/
/*  Revision History:			        			*/
/*					                        	*/
/*	08/28/2014(JamesC): Created	       			        */
/*      06/14/2016(EricM) : Added ALS Library support                   */
/*                                                                      */
/*					      	                        */
/************************************************************************/

/* -------------------------------------------------------------------- */
/*		        Include File Definitions                     	*/
/* -------------------------------------------------------------------- */
#include <ALS.h>
#include <DSPI.h>


/* -------------------------------------------------------------------- */
/*		            Global Variables                     	*/
/* -------------------------------------------------------------------- */
ALS myALS;
int lightLevel;
int lightData[16];

/* -------------------------------------------------------------------- */
/*	               Procedure Definitions	                        */
/* -------------------------------------------------------------------- */
/***	setup
**
**	Parameters:
**		none
**
**	Return Value:
**		none
**
**	Errors:
**		none
**
**	Description:
**		 Performs basic initialization.		
/*** ---------------------------------------------------------- ***/
void setup(){
 
  // Set up serial monitor to see the ambient light levels
  Serial.begin(9600);
 
  // Initialize pins
  pinMode(SS, OUTPUT);
  pinMode(MISO, INPUT);
  pinMode(SCK, OUTPUT);
 
  // Set up PmodALS
  myALS.begin(PAR_ACCESS_SPI0);// corresponds to DSPI0 - connector JB
// myALS.begin(PAR_ACCESS_SPI1); // corresponds to DSPI1 - connector J1
  
  
}


/*** ---------------------------------------------------------- ***/
/***	loop
**
**	Parameters:
**		none
**
**	Return Value:
**		none
**
**	Errors:
**		none
**
**	Description:
**		Main program module. Enters the main program loop.
/*** ---------------------------------------------------------- ***/
void loop(){
  // Grab ambent light level from PmodALS
  lightLevel = myALS.ReturnLightLevel();
  
  // Print ambient light level to the serial monitor
  Serial.print("the ambient light level is: ");Serial.println(lightLevel);
  
  // Wait some time
  delay(1000);
  
}
