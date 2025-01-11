Phoenix Hexapod
===============

This repository contains software for Phoenix Hexapod robot. Software is designed to be reconfigurable, and can be used to control another six legged robots with similar kinematics.

Software includes firmware of the robot and kinematic simulation application.

Short video is available on [YouTube](https://youtu.be/20ni6MPR1c4).

It uses [CGK](https://github.com/godunko/cgk) for geometric computations.

Hardware
--------

 * [Phoenix Hexapod Kit](https://aliexpress.ru/item/1457972327.html?spm=a2g2w.orderdetail.0.0.4cbc4aa6C7frwp&sku_id=67027752015)
 * WeAct BlackPill with STM32F401/STM32F411 MCU (Cortex-M4 @84 MHz, HSE @25MHz)
 * 18x DS3218 PRO motors
 * 2x PCA9685 PWM/Servo Driver boards to control motors
 * 2x relay modules
 * PlayStation2 Controller

Software can work on Arduino Due (ATSAM3X8E, Cortex-M3 @84 MHz), however, it doesn't suppot hardware floating point operations and not enough to achieve desired performance of used algoriphms.

Implemented Features
--------------------

 * Forward kinematics
 * Inverse kinematic solvers
   * Algebraic
   * Geometric
   * Numerical (Gradient Descent and Levenberg-Marquardt)
 * Gait transition (fixed step)
 * Body trajectry control by the PlayStation/2 controller
 * Free gait genrator for crab movement and turns
 * Trajectory generator of the legs based on:
   * 3D geometry transformation and inverse kinematics of legs
   * whole body velocity transformation matrix, which transforms 6D velocity of the robot's body into velocities in all joints

Current Development
-------------------

 * Improvement of free gait generator

Notes
-----

There are some notes about use of things. They was done to don't forgot key features, even if they not implemented in robot.

They are written on Russian, I hope Google Translator can translate them to your language.

 * [Unit Hopf CPG (Central Pattern Generator)](documentation/ru/Unit_Hopf_CPG.md)
