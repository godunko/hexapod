Phoenix Hexapod
===============

This repository contains software for Phoenix Hexapod robot.

Short video is available on [YouTube](https://www.youtube.com/watch?v=2j_bzXEz6qw).

It uses [B2F4A](https://github.com/godunko/b2f4a) library to manage hardware.


Hardware
--------

 * [Phoenix Hexapod Kit](https://aliexpress.ru/item/1457972327.html?spm=a2g2w.orderdetail.0.0.4cbc4aa6C7frwp&sku_id=67027752015)
 * [Arduino Due](https://docs.arduino.cc/hardware/due) board powered by SAM3X8E MCU
 * 18x DS3218 PRO motors
 * 2x PCA9685 PWM/Servo Driver boards to control motors
 * 2x ACS7125A (5A) current sensors, connected to MCU's ADC ports
 * 2x relay modules
 * PlayStation2 Controller
 * MPU9150 MotionTracking board

Implemented Features
--------------------

 * Forward kinematics
 * Inverse kinematic solvers
   * Algebraic
   * Geometric
   * Numerical (Gradient Descent and Levenberg-Marquardt)
 * Gait transition (fixed step)

Current Development
-------------------

 * Body trajectry control by the PlayStation/2 controller
 * Use of Max-Plus linear system to implement free gait

Notes
-----

There are some notes about use of things. They was done to don't forgot key features, even if they not implemented in robot.

They are written on Russian, I hope Google Translator can translate them to your language.

 * [Unit Hopf CPG (Central Pattern Generator)](documentation/ru/Unit_Hopf_CPG.md)
