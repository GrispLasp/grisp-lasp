/************************************************************************/
/*																		*/
/*		GYRO.h	--	Interface Declarations for GYRO.cpp			        */
/*																		*/
/************************************************************************/
/*	Author:		Oliver Jones											*/
/*	Copyright 2011, Digilent Inc.										*/
/************************************************************************/
/*
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/
/************************************************************************/
/*  File Description:													*/
/*																		*/
/*	This header file contains the object class declarations and other	*/
/*	interface declarations need to use the gyroscope driver for the		*/
/*	Digilent PmodGYRO													*/
/*																		*/
/************************************************************************/
/*  Revision History:													*/
/*																		*/
/*	10/17/2011(OliverJ): created										*/
/*																		*/
/************************************************************************/

#if !defined(GYRO_H)
#define GYRO_H

#include "DSPI.h"

extern "C" {
  #include <stdint.h>
}

/*  Register Addresses
*/
#define WHO_AM_I		0x0F
#define CTRL_REG1		0x20
#define CTRL_REG2		0x21
#define CTRL_REG3		0x22
#define CTRL_REG4		0x23
#define CTRL_REG5		0x24
#define REFERENCE		0x25
#define OUT_TEMP		0x26
#define STATUS_REG		0x27
#define OUT_X_L			0x28
#define OUT_X_H			0x29
#define OUT_Y_L			0x2A
#define OUT_Y_H			0x2B
#define OUT_Z_L			0x2C
#define OUT_Z_H			0x2D
#define FIFO_CTRL_REG	0x2E
#define FIFO_SRC_REG	0x2F
#define INT1_CFG		0x30
#define INT1_SRC		0x31
#define INT1_TSH_XH		0x32
#define INT1_TSH_XL		0x33
#define INT1_TSH_YH		0x34
#define INT1_TSH_YL		0x35
#define INT1_TSH_ZH		0x36
#define INT1_TSH_ZL		0x37
#define INT1_DURATION	0x38

/*	CTRL_REG1
*/
#define REG1_DR1		0x80
#define REG1_DR0		0x40
#define REG1_BW1		0x20
#define REG1_BW0		0x10
#define REG1_PD			0x08
#define REG1_ZEN		0x04
#define REG1_YEN		0x02
#define REG1_XEN		0x01

/*	CTRL_REG3
*/
#define REG3_I1_INT1	0x80
#define REG3_I1_BOOT	0x40
#define REG3_H_LACTIVE	0x20
#define REG3_PP_OD		0x10
#define REG3_I2_DRDY	0x08
#define REG3_I2_WTM		0x04
#define REG3_I2_ORUN	0x02
#define REG3_I2_EMPTY	0x01

/*	INT1_CFG
*/
#define INT1_ANDOR		0x80
#define INT1_LIR		0x40
#define INT1_ZHIE		0x20
#define INT1_ZLIE		0x10
#define INT1_YHIE		0x08
#define INT1_YLIE		0x04
#define INT1_XHIE		0x02
#define INT1_XLIE		0x01

/* ------------------------------------------------------------ */
/*					Global Variable Declarations				*/
/* ------------------------------------------------------------ */

class GYRO
{
	public:
		GYRO();
		bool begin();
		void end();
		int16_t getX();
		int16_t getY();
		int16_t getZ();
		int8_t getTemp();
		uint8_t getInt1Src();
		bool enableInt1(uint8_t mode);
		bool disableInt1();
		bool setThsXL(uint8_t ths);
		bool setThsXH(uint8_t ths);
		bool setThsYL(uint8_t ths);
		bool setThsYH(uint8_t ths);
		bool setThsZL(uint8_t ths);
		bool setThsZH(uint8_t ths);
		bool enableInt2(uint8_t mode);
		bool disableInt2();
		int writeReg(uint8_t reg, uint8_t value);
		void readReg(uint8_t reg, uint8_t *recv, int count);

	private:
		DSPI0 spiCon;
};

#endif