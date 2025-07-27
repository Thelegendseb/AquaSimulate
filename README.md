# Particle-Based Fluid Dynamics Ecosystem Demo

This project is a real-time simulation of particle-based fluid dynamics with an interactive aquatic ecosystem, built using the [Tzu3D Engine](http://www.tzu3d.com/). The simulation features thousands of fluid particles, fish, plants, and predators, all interacting in a dynamic environment.

## Features

- **Fluid Simulation:** Implements Smoothed Particle Hydrodynamics (SPH) for realistic water movement.
- **Ecosystem AI:** Fish flock, seek food, and avoid predators; plants grow and provide nutrients; predators hunt fish.
- **Interactive Controls:** Move and zoom the camera, attract or repel fluid particles with the mouse, and toggle simulation parameters in real time.
- **Performance Optimizations:** Uses spatial grids and parallel processing for efficient simulation of thousands of entities.
- **Visual Feedback:** Colorful rendering of particles, fish, plants, and predators, with visual indicators for health, hunger, and other states.

## Controls

- **WASD / Arrow Keys:** Move camera
- **Mouse Wheel:** Zoom in/out
- **Left Mouse Button:** Attract fluid particles
- **Right Mouse Button:** Repel fluid particles
- **M:** Toggle mouse interaction
- **+ / -:** Increase or decrease particle count
- **R:** Reset fluid and ecosystem
- **ESC:** Quit simulation

## Requirements

- **Tzu3D Engine:** [Download and install from tzu3d.com](http://www.tzu3d.com/)
- **.NET Framework:** Compatible with .NET Framework 4.7.2+ (or as required by Tzu3D)
- **Visual Studio:** Recommended for building and running the project

## Getting Started

1. **Install Tzu3D Engine:**  
   Download and install the Tzu3D SDK from [http://www.tzu3d.com/](http://www.tzu3d.com/).

2. **Clone or Download this Repository:**  
   Place the project files in your preferred directory.

3. **Open in Visual Studio:**  
   Open the solution file or the main `.vb` file.

4. **Add References:**  
   Ensure your project references the Tzu3D libraries (`Tzu3D.dll` and any dependencies).

5. **Build and Run:**  
   Press `F5` or use the "Start" button in Visual Studio to run the simulation.

## Project Structure

- **Game.vb:** Main simulation logic, rendering, and input handling.
