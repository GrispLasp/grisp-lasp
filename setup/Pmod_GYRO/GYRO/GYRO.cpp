/************************************************************************/
/*																		*/
/*	GYRO.cpp	--	Gyro Driver for GYRO						        */
/*																		*/
/************************************************************************/
/*	Author: 	Oliver Jones											*/
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
/*  Module Description: 												*/
/*																		*/
/*	This module contains the implementation of the object class that	*/
/*	forms the chipKIT interface to the gyroscope functions of a Digilent*/
/*	PmodGYRO															*/
/*																		*/
/************************************************************************/
/*  Revision History:													*/
/*																		*/
/*	06/01/2011(OliverJ): created										*/
/*	02/01/2012(RyanK): modified getTemp method to return a more accurate*/
/*				temperature back										*/
/*																		*/
/************************************************************************/


/* ------------------------------------------------------------ */
/*				Include File Definitions						*/
/* ------------------------------------------------------------ */

#include "GYRO.h"

extern "C" {
  #include <stdint.h>
}

#define WRITE 		0x00
#define MULTI_WRITE 0x40
#define READ 		0x80
#define MULTI_READ 	0xC0


GYRO::GYRO()
{
}

bool GYRO::begin()
{
	spiCon.begin();

	spiCon.setSpeed(10000000);

	spiCon.setMode(DSPI_MODE3);

	if(!writeReg(CTRL_REG3, 0))
		return false;

	if(!writeReg(CTRL_REG1,REG1_PD | REG1_ZEN | REG1_YEN | REG1_XEN))
		return false;

	return true;
}

void GYRO::end()
{
	spiCon.end();
}

int16_t GYRO::getX()
{
	uint8_t output;
	uint8_t temp[2] = {0,0};
	int16_t xAxis = 0;

	readReg(OUT_X_L, temp, 2);

	xAxis = temp[0];
	xAxis |= (temp[1] << 8);

	return xAxis;
}

int16_t GYRO::getY()
{
	uint8_t temp[2] = {0,0};
	int16_t yAxis = 0;

	readReg(OUT_Y_L, temp, 2);

	yAxis = temp[0];
	yAxis |= (temp[1] << 8);

	return yAxis;
}

int16_t GYRO::getZ()
{
	uint8_t temp[2] = {0,0};
	int16_t zAxis = 0;

	readReg(OUT_Z_L, temp, 2);

	zAxis = temp[0];
	zAxis |= (temp[1] << 8);

	return zAxis;
}

//Changes made:
//Changed temp to int8_t from uint8_t
//Casted &temp to (uint8_t *) in readReg call
//Added temp = 44 - temp; to get a more accurate temperature
int8_t GYRO::getTemp()
{
	int8_t temp = 0;

	readReg(OUT_TEMP, (uint8_t *) &temp, 1);
	
	temp = 44 - temp;
	return temp;
}

uint8_t GYRO::getInt1Src()
{
	uint8_t temp = 0;
	
	readReg(INT1_SRC, &temp, 1);
	
	return temp;
}

bool GYRO::enableInt1(uint8_t mode)
{
	uint8_t temp = 0;

	readReg(CTRL_REG3, &temp, 1);

	temp |= REG3_I1_INT1;

	if(!writeReg(CTRL_REG3, temp))
		return 0;

	if(!writeReg(INT1_CFG, mode))
		return 0;

	return 1;	
}

bool GYRO::disableInt1()
{
	uint8_t temp = 0;

	readReg(CTRL_REG3, &temp, 1);

	temp &= !REG3_I1_INT1;

	if(!writeReg(CTRL_REG3, temp))
		return 0;

	return 1;
}

bool GYRO::setThsXL(uint8_t ths)
{
	return writeReg(INT1_TSH_XL, ths);
}

bool GYRO::setThsXH(uint8_t ths)
{
	return writeReg(INT1_TSH_XH, ths);
}

bool GYRO::setThsYH(uint8_t ths)
{
	return writeReg(INT1_TSH_YH, ths);
}

bool GYRO::setThsYL(uint8_t ths)
{
	return writeReg(INT1_TSH_YL, ths);
}

bool GYRO::setThsZH(uint8_t ths)
{
	return writeReg(INT1_TSH_ZH, ths);
}

bool GYRO::setThsZL(uint8_t ths)
{
	return writeReg(INT1_TSH_ZL, ths);
}

bool GYRO::enableInt2(uint8_t mode)
{
	uint8_t temp = 0;

	readReg(CTRL_REG3, &temp, 1);

	temp |= mode;

	if(!writeReg(CTRL_REG3, temp))
		return 0;

	return 1;	
}

bool GYRO::disableInt2()
{
	uint8_t temp = 0;

	readReg(CTRL_REG3, &temp, 1);

	temp &= 0xF0;

	if(!writeReg(CTRL_REG3, temp))
		return 0;

	return 1;
}

int GYRO::writeReg(uint8_t reg, uint8_t value)
{
	uint8_t temp;

	spiCon.setSelect(LOW);
	spiCon.transfer(reg);
	spiCon.transfer(value);
	spiCon.setSelect(HIGH);

	spiCon.setSelect(LOW);
	spiCon.transfer(READ | reg);
	temp = spiCon.transfer(0x00);
	spiCon.setSelect(HIGH);

	if(temp != value)
		return 0;

	return 1;
}

void GYRO::readReg(uint8_t reg, uint8_t *recv, int count)
{
	if(count == 1)
	{
		int i = 0;
		// Retrieve current state of register
		spiCon.setSelect(LOW);
		spiCon.transfer(READ | reg);
		*recv = spiCon.transfer(0x00);
		spiCon.setSelect(HIGH);
	}
	else
	{
		int i = 0;
		spiCon.setSelect(LOW);
		spiCon.transfer(MULTI_READ | reg);
		for(i=0;i < count;i++)
		{
			*recv = spiCon.transfer(0x00);
			recv++;
		}
		spiCon.setSelect(HIGH);
	}
}