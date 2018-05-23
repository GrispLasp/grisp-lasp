/************************************************************************/
/*																		*/
/*	ALS.cpp		--		Definition for ALS library 	   			 		*/
/*																		*/
/************************************************************************/
/*	Author:		Eric Marsh												*/
/*	Copyright 2016, Digilent Inc.										*/
/************************************************************************/
/*  File Description:													*/
/*		This file defines functions for the PmodALS						*/
/*																		*/
/************************************************************************/
/*  Revision History:													*/
/*																		*/
/*	06/17/2016(EricM): created											*/
/*																		*/
/************************************************************************/


/* ------------------------------------------------------------ */
/*				Include File Definitions						*/
/* ------------------------------------------------------------ */
#include "ALS.h"
#include <Dspi.h>

/* ------------------------------------------------------------ */
/*				Procedure Definitions							*/
/* ------------------------------------------------------------ */

/* ------------------------------------------------------------ */
/*        ALS::ALS
**
**        Synopsis:
**				
**        Parameters:
**
**
**
**        Return Values:
**                void 
**
**        Errors:
**
**
**        Description:
**			Class constructor. Performs variables initialization tasks
**
**
*/
ALS::ALS()
{
	pdspi = NULL;
	
}

/* ------------------------------------------------------------ */
/*        ALS::begin
**
**        Synopsis:
**				myALS.begin(PAR_ACCESS_SPI0);
**        Parameters:
**				uint8_t bAccessType	- the SPI interface where the Pmod is connected. It can be one of:
**					0	PAR_ACCESS_SPI0	- indicates SPI port 2, connetor JB
**					1	PAR_ACCESS_SPI1	- indicates SPI port 1, connector J1
**				uint8_t spd - The frequency at which the SPI clock should be set to. 2 Mhz by default
**
**        Return Values:
**                void 
**
**        Errors:
**
**
**        Description:
**				This function initializes the specific SPI interface used, setting the SPI frequency to the parameter spd, which is 1 Mhz by default.
**
*/
void ALS::begin(uint8_t bAccessType, uint32_t spd)
{
	
	if(bAccessType == PAR_ACCESS_SPI0)
	{
		pdspi = new DSPI0();
		m_SSPin = PIN_DSPI0_SS;	// default SS pin for SPI0
	}
	if(bAccessType == PAR_ACCESS_SPI1)
	{
		pdspi = new DSPI1();
		m_SSPin = PIN_DSPI1_SS;	// default SS pin for SPI1
	}
	if(pdspi != NULL)
	{
		pdspi->begin(m_SSPin);
		pdspi->setMode(DSPI_MODE3);
		pdspi->setTransferSize(DSPI_16BIT);
		pdspi->setSpeed(spd);
	}
	
}

/* ------------------------------------------------------------ */
/*        ALS::ReturnLightLevel
**
**        Synopsis:
**				int lightLevel = myALS.ReturnLightLevel();
**        Parameters:
**				None
**
**        Return Values:
**              - int - 12 bits of ambient light data from the PmodALS
**
**        Errors:
**				If module is not initialized (using begin), the function does nothing.
**
**        Description:
**				returns the 12 bits of ambient light data from the PmodALS
**
*/
int ALS::ReturnLightLevel()
{
	lightLevel = 0;
	digitalWrite(SS, LOW);
	lightLevel = pdspi->transfer(0);
	lightLevel = lightLevel>>4;
	digitalWrite(SS, HIGH);
	//Serial.print("the ambient light level is: ");Serial.println(lightLevel);
	return lightLevel;
}