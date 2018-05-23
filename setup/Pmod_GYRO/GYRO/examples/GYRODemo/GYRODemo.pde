/************************************************************************/
/*									*/
/*  GYRODemo.pde	-- Example Sketch for GYRO			*/
/*									*/
/************************************************************************/
/*  Author:	Oliver Jones						*/
/*  Copyright (c) 2011, Digilent Inc.  	    				*/
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
/*  Module Description:							*/
/*									*/
/*  This sketch is an example on on how to read the status of a Gyro    */
/* with the GYRO library.                                               */
/************************************************************************/
/*  Revision History:							*/
/*									*/
/*  10/21/2011(Oliver J): Created                                       */
/*									*/
/************************************************************************/
/************************************************************************/
/*  Board Support:							*/
/*									*/
/*  chipKit Uno32 with Pmod Shield:     Header JC	                */
/*                                    Int1Pin = pin 20                  */
/*                                    Int2Pin = pin 21                  */
/*   (Note: Select Cerebot MX3ck in Tools | Board menu to use this      */
/*                    configuration)                                    */              
/*                                                                      */
/*  Cerebot Mx3ck:                    Header JE                         */
/*                                    Int1Pin = pin 36                  */
/*                                    Int2Pin = pin 37                  */
/*                                                                      */
/*  Cerebot Mx4ck:                    Header JB                         */
/*                                    Int1Pin = pin 12                  */
/*                                    Int2Pin = pin 13                  */
/*                                                                      */
/*  Cerebot Mx7ck:                    Header JD                         */
/*                                    Int1Pin = pin 28                  */
/*                                    Int2Pin = pin 29                  */
/*                                      				*/
/************************************************************************/

#include <DSPI.h>
#include <GYRO.h>

const int Int1Pin = 36;
const int Int2Pin = 37;

int16_t xAxis = 0;
int16_t yAxis = 0;
int16_t zAxis = 0;
int8_t temp = 0;

GYRO myGYRO;

void setup() {
  Serial.begin(9600);
  Serial.println("Pmod Gyro Demo");
  
  //Set interrupt pins as inputs
  pinMode(Int1Pin, INPUT);
  pinMode(Int2Pin, INPUT);
  
  myGYRO.begin();
  //Set XH Threshold Register
  myGYRO.setThsXH(0x0F);
  //Enable Interrupt 
  myGYRO.enableInt1(INT1_XHIE);
  //myGYRO.disableInt2();
  
  myGYRO.enableInt2(REG3_I2_DRDY);
  //myGYRO.disableInt2();
}

void loop() {
   
  //check int1 pin
  if(digitalRead(Int1Pin))
  {
    Serial.println("Int1 was triggerd");
    //Read InterruptSrc1 Reg to clear interrupt
    myGYRO.getInt1Src();
  }
  
  //check int2 pin
  if(digitalRead(Int2Pin))
  {
    Serial.println("Int2 was triggerd");
    xAxis = myGYRO.getX();
    yAxis = myGYRO.getY();
    zAxis = myGYRO.getZ();
    temp = myGYRO.getTemp();
    
    Serial.print("X Axis: ");
    Serial.println(xAxis, DEC);
    Serial.print("Y Axis: ");
    Serial.println(yAxis, DEC);
    Serial.print("Z Axis: ");
    Serial.println(zAxis, DEC);
    Serial.print("Temp: ");
    Serial.println(temp, DEC);
  }
  
  delay(1000);  
}
