/************************************************************************/
/*																		*/
/*	ALS.h	--	Declaration for ALS library 	    					*/
/*																		*/
/************************************************************************/
/*	Author:		Eric Marsh												*/
/*	Copyright 2016, Digilent Inc.										*/
/************************************************************************/
/*  File Description:													*/
/*	This file declares the ALS library functions and the constants	    */
/*	involved.															*/
/*																		*/
/************************************************************************/
/*  Revision History:													*/
/*																		*/
/*	06/17/2016(EricM): created											*/
/*																		*/
/************************************************************************/
#if !defined(ALS_H)
#define ALS_H

/* ------------------------------------------------------------ */
/*				Include File Definitions						*/
/* ------------------------------------------------------------ */
#include <inttypes.h>
#include <DSPI.h>

/* ------------------------------------------------------------ */
/*					Definitions									*/
/* ------------------------------------------------------------ */
#define ALS_NO_BITS		12

#define ALS_SPI_FREQ	2000000 // 2 MHz - spi freq
#define	PAR_ACCESS_SPI0			0
#define	PAR_ACCESS_SPI1			1
#define	PAR_ACCESS_I2C			2	

#define ALS_CTRL_BYTE	0b00000011	// selecting convertor DAC A to be written
/* ------------------------------------------------------------ */
/*					Errors Definitions							*/
/* ------------------------------------------------------------ */

#define ALS_ERR_SUCCESS				0	// The action completed successfully
#define ALS_ERR_VAL_OUT_OF_RANGE	1	// The value is out of range
#define ALS_ERR_ADR_OUT_OF_RANGE	2	// The address is out of range

/* ------------------------------------------------------------ */
/*					Procedure Declarations						*/
/* ------------------------------------------------------------ */


class ALS {
private: 
	DSPI *pdspi;
	uint8_t m_SSPin;
	int lightLevel;

public:
	ALS ();
	void begin(uint8_t bAccessType, uint32_t spd = ALS_SPI_FREQ);
	int ReturnLightLevel();
};



#endif