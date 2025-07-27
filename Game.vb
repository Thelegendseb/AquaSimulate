Imports Tzu3D
Imports Tzu3D.Graphics
Imports Tzu3D.TZGlobal.Colors
Imports Tzu3D.TZGlobal.Maths

'Particle-Based Fluid Dynamics with Interactive Ecosystem
Public Class Demo_01
    Private Engine As TZEngine
    Private Input As TZInput
    Private mSprite As TZSprite

    Private Const MAX_PARTICLES As Integer = 8000
    Private Const MAX_FISH As Integer = 200
    Private Const MAX_PLANTS As Integer = 150
    Private Const MAX_PREDATORS As Integer = 50
    Private Const GRID_SIZE As Integer = 32

    Private mFluidParticles() As FluidParticle
    Private mFish() As Fish
    Private mPlants() As Plant
    Private mPredators() As Predator
    Private mSpatialGrid(,) As List(Of Integer)

    Private ParticleCount As Integer = MAX_PARTICLES
    Private FishCount As Integer = MAX_FISH
    Private PlantCount As Integer = MAX_PLANTS
    Private PredatorCount As Integer = MAX_PREDATORS

    Private WorldBounds As Rectangle = New Rectangle(-2000, -1500, 4000, 3000)
    Private Virtual_Cam_Position As Vector2
    Private Virtual_Cam_Zoom As Single = 1.0F
    Private Camera_Speed As Single = 400.0F

    Private Deltatime As Single
    Private TotalTime As Single
    Private PerformanceCounter As Integer
    Private LastFPS As Single

    ' Fluid dynamics parameters
    Private Const REST_DENSITY As Single = 65.0F
    Private Const STIFFNESS As Single = 200.0F
    Private Const VISCOSITY As Single = 0.018F
    Private Const SMOOTHING_RADIUS As Single = 16.0F
    Private Const PARTICLE_MASS As Single = 2.5F
    Private Const DAMPING As Single = 0.95F

    ' Ecosystem parameters
    Private Const PLANT_GROWTH_RATE As Single = 0.1F
    Private Const FISH_HUNGER_RATE As Single = 0.05F
    Private Const PREDATOR_HUNGER_RATE As Single = 0.08F

    Private mRandom As New TZGlobal.TZRandom
    Private mInfoPosition As Vector2 = New Vector2(20, 20)
    Private mInfoLineY As Single

    Private MouseInteraction As Boolean = True
    Private MouseForceRadius As Single = 80.0F
    Private MouseForceStrength As Single = 500.0F

    Private Sub Game_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Engine = TZEngine.Instance
        Input = TZInput.Instance

        Engine.Debug_Enable = True
        Engine.Init(Handle)
        Engine.Thread_Max = Environment.ProcessorCount - 1
        Engine.FrameLimiter_FPS = 60
        Engine.FrameLimiter_Enable = True
        Engine.Show_Fps = True

        InitializeDemo()
        RenderLoop()
    End Sub

    Private Sub InitializeDemo()
        mSprite = New TZSprite(MAX_PARTICLES * 3 + MAX_FISH + MAX_PLANTS + MAX_PREDATORS + 1000)

        ReDim mFluidParticles(MAX_PARTICLES - 1)
        ReDim mFish(MAX_FISH - 1)
        ReDim mPlants(MAX_PLANTS - 1)
        ReDim mPredators(MAX_PREDATORS - 1)
        ReDim mSpatialGrid(WorldBounds.Width \ GRID_SIZE, WorldBounds.Height \ GRID_SIZE)

        InitializeSpatialGrid()
        InitializeFluidParticles()
        InitializeEcosystem()

        Set_Camera(Vector2.Zero, 0.8F)
    End Sub

    Private Sub InitializeSpatialGrid()
        For x As Integer = 0 To mSpatialGrid.GetLength(0) - 1
            For y As Integer = 0 To mSpatialGrid.GetLength(1) - 1
                mSpatialGrid(x, y) = New List(Of Integer)
            Next
        Next
    End Sub

    Private Sub InitializeFluidParticles()
        For i As Integer = 0 To ParticleCount - 1
            mFluidParticles(i) = New FluidParticle With {
                .Position = New Vector2(
                    mRandom.Range(WorldBounds.Left + 200, WorldBounds.Right - 200),
                    mRandom.Range(WorldBounds.Top + 200, WorldBounds.Bottom - 200)
                ),
                .Velocity = New Vector2(mRandom.Range(-20, 20), mRandom.Range(-20, 20)),
                .Density = REST_DENSITY,
                .Pressure = 0,
                .Mass = PARTICLE_MASS,
                .Temperature = mRandom.Range(0.3F, 1.0F)
            }
        Next
    End Sub

    Private Sub InitializeEcosystem()
        ' Initialize Plants
        For i As Integer = 0 To PlantCount - 1
            mPlants(i) = New Plant With {
                .Position = New Vector2(
                    mRandom.Range(WorldBounds.Left + 100, WorldBounds.Right - 100),
                    mRandom.Range(WorldBounds.Bottom - 400, WorldBounds.Bottom - 50)
                ),
                .Size = mRandom.Range(8.0F, 25.0F),
                .Health = mRandom.Range(0.5F, 1.0F),
                .GrowthRate = mRandom.Range(0.05F, 0.15F),
                .NutrientValue = mRandom.Range(0.3F, 0.8F)
            }
        Next

        ' Initialize Fish
        For i As Integer = 0 To FishCount - 1
            mFish(i) = New Fish With {
                .Position = New Vector2(
                    mRandom.Range(WorldBounds.Left + 150, WorldBounds.Right - 150),
                    mRandom.Range(WorldBounds.Top + 150, WorldBounds.Bottom - 150)
                ),
                .Velocity = New Vector2(mRandom.Range(-30, 30), mRandom.Range(-30, 30)),
                .Size = mRandom.Range(4.0F, 12.0F),
                .Health = 1.0F,
                .Hunger = mRandom.Range(0.2F, 0.8F),
                .Speed = mRandom.Range(80.0F, 150.0F),
                .FlockingRadius = mRandom.Range(40.0F, 80.0F),
                .Age = mRandom.Range(0, 200)
            }
        Next

        ' Initialize Predators
        For i As Integer = 0 To PredatorCount - 1
            mPredators(i) = New Predator With {
                .Position = New Vector2(
                    mRandom.Range(WorldBounds.Left + 200, WorldBounds.Right - 200),
                    mRandom.Range(WorldBounds.Top + 200, WorldBounds.Bottom - 200)
                ),
                .Velocity = New Vector2(mRandom.Range(-20, 20), mRandom.Range(-20, 20)),
                .Size = mRandom.Range(15.0F, 25.0F),
                .Health = 1.0F,
                .Hunger = mRandom.Range(0.3F, 0.7F),
                .Speed = mRandom.Range(120.0F, 200.0F),
                .HuntingRadius = mRandom.Range(100.0F, 150.0F),
                .AttackCooldown = 0
            }
        Next
    End Sub

    Private Sub RenderLoop()
        Do While Engine.DoRender
            mInfoLineY = 0
            Engine.Render_Begin(Color.FromArgb(15, 25, 35))

            Deltatime = Engine.DeltaTime(eTimeUnit.Seconds)
            TotalTime += Deltatime
            PerformanceCounter += 1

            If PerformanceCounter Mod 30 = 0 Then
                LastFPS = 1.0F / Deltatime
            End If

            HandleInput()
            UpdateSpatialGrid()
            UpdateFluidDynamics()
            UpdateEcosystem()
            ApplyMouseForces()

            DrawBackground()
            DrawFluidParticles()
            DrawEcosystem()
            DrawUI()

            mSprite.Render(True)
            Engine.Render_End()
            Application.DoEvents()
        Loop

        Engine.Dispose()
        Application.Exit()
    End Sub

    Private Sub UpdateSpatialGrid()
        ' Clear grid
        For x As Integer = 0 To mSpatialGrid.GetLength(0) - 1
            For y As Integer = 0 To mSpatialGrid.GetLength(1) - 1
                mSpatialGrid(x, y).Clear()
            Next
        Next

        ' Populate grid with particles
        For i As Integer = 0 To ParticleCount - 1
            Dim gridX As Integer = CInt((mFluidParticles(i).Position.X - WorldBounds.Left) / GRID_SIZE)
            Dim gridY As Integer = CInt((mFluidParticles(i).Position.Y - WorldBounds.Top) / GRID_SIZE)

            If gridX >= 0 AndAlso gridX < mSpatialGrid.GetLength(0) AndAlso
               gridY >= 0 AndAlso gridY < mSpatialGrid.GetLength(1) Then
                mSpatialGrid(gridX, gridY).Add(i)
            End If
        Next
    End Sub

    Private Sub UpdateFluidDynamics()
        ' Calculate density and pressure
        Parallel.For(0, ParticleCount, Sub(i)
                                           CalculateDensityAndPressure(i)
                                       End Sub)

        ' Calculate forces
        Parallel.For(0, ParticleCount, Sub(i)
                                           CalculateForces(i)
                                       End Sub)

        ' Integrate
        Parallel.For(0, ParticleCount, Sub(i)
                                           IntegrateParticle(i)
                                       End Sub)
    End Sub

    Private Sub CalculateDensityAndPressure(index As Integer)
        Dim particle As FluidParticle = mFluidParticles(index)
        particle.Density = 0

        Dim neighbors As List(Of Integer) = GetNeighbors(particle.Position)

        For i As Integer = 0 To neighbors.Count - 1
            Dim neighborIndex As Integer = neighbors(i)
            Dim neighbor As FluidParticle = mFluidParticles(neighborIndex)
            Dim distance As Single = Vector2.Distance(particle.Position, neighbor.Position)

            If distance < SMOOTHING_RADIUS Then
                Dim influence As Single = SmoothingKernel(distance, SMOOTHING_RADIUS)
                particle.Density += neighbor.Mass * influence
            End If
        Next

        particle.Pressure = STIFFNESS * (particle.Density - REST_DENSITY)
        mFluidParticles(index) = particle
    End Sub

    Private Sub CalculateForces(index As Integer)
        Dim particle As FluidParticle = mFluidParticles(index)
        Dim pressureForce As Vector2 = Vector2.Zero
        Dim viscosityForce As Vector2 = Vector2.Zero

        Dim neighbors As List(Of Integer) = GetNeighbors(particle.Position)

        For i As Integer = 0 To neighbors.Count - 1
            Dim neighborIndex As Integer = neighbors(i)
            If neighborIndex = index Then Continue For

            Dim neighbor As FluidParticle = mFluidParticles(neighborIndex)
            Dim distance As Single = Vector2.Distance(particle.Position, neighbor.Position)

            If distance > 0 AndAlso distance < SMOOTHING_RADIUS Then
                Dim direction As Vector2 = (particle.Position - neighbor.Position) / distance

                ' Pressure force
                Dim pressureGradient As Single = SmoothingKernelDerivative(distance, SMOOTHING_RADIUS)
                pressureForce += direction * neighbor.Mass * (particle.Pressure + neighbor.Pressure) / (2 * neighbor.Density) * pressureGradient

                ' Viscosity force
                Dim viscosityLaplacian As Single = ViscosityKernel(distance, SMOOTHING_RADIUS)
                viscosityForce += (neighbor.Velocity - particle.Velocity) * neighbor.Mass / neighbor.Density * viscosityLaplacian
            End If
        Next

        particle.Force = -pressureForce + viscosityForce * VISCOSITY + New Vector2(0, 200) ' Gravity
        mFluidParticles(index) = particle
    End Sub


    Private Sub IntegrateParticle(index As Integer)
        Dim particle As FluidParticle = mFluidParticles(index)

        If particle.Density > 0 Then
            Dim acceleration As Vector2 = particle.Force / particle.Density
            particle.Velocity += acceleration * Deltatime
            particle.Velocity *= DAMPING
            particle.Position += particle.Velocity * Deltatime
        End If

        ' Boundary conditions
        If particle.Position.X < WorldBounds.Left Then
            particle.Position.X = WorldBounds.Left
            particle.Velocity.X *= -0.5F
        ElseIf particle.Position.X > WorldBounds.Right Then
            particle.Position.X = WorldBounds.Right
            particle.Velocity.X *= -0.5F
        End If

        If particle.Position.Y < WorldBounds.Top Then
            particle.Position.Y = WorldBounds.Top
            particle.Velocity.Y *= -0.5F
        ElseIf particle.Position.Y > WorldBounds.Bottom Then
            particle.Position.Y = WorldBounds.Bottom
            particle.Velocity.Y *= -0.5F
        End If

        ' Update temperature based on velocity and neighbors
        particle.Temperature = Lerp(particle.Temperature,
            0.5F + particle.Velocity.Length() / 200.0F, Deltatime * 0.1F)
        particle.Temperature = Math.Max(0.1F, Math.Min(1.0F, particle.Temperature))

        mFluidParticles(index) = particle
    End Sub

    Private Function GetNeighbors(position As Vector2) As List(Of Integer)
        Dim neighbors As New List(Of Integer)
        Dim gridX As Integer = CInt((position.X - WorldBounds.Left) / GRID_SIZE)
        Dim gridY As Integer = CInt((position.Y - WorldBounds.Top) / GRID_SIZE)

        For dx As Integer = -1 To 1
            For dy As Integer = -1 To 1
                Dim checkX As Integer = gridX + dx
                Dim checkY As Integer = gridY + dy

                If checkX >= 0 AndAlso checkX < mSpatialGrid.GetLength(0) AndAlso
                   checkY >= 0 AndAlso checkY < mSpatialGrid.GetLength(1) Then
                    neighbors.AddRange(mSpatialGrid(checkX, checkY))
                End If
            Next
        Next

        Return neighbors
    End Function

    Private Sub UpdateEcosystem()
        UpdatePlants()
        UpdateFish()
        UpdatePredators()
        HandleEcosystemInteractions()
    End Sub

    Private Sub UpdatePlants()
        Parallel.For(0, PlantCount, Sub(i)
                                        Dim plant As Plant = mPlants(i)

                                        ' Growth
                                        plant.Health += plant.GrowthRate * Deltatime
                                        plant.Health = Math.Min(1.0F, plant.Health)
                                        plant.Size = 8.0F + plant.Health * 17.0F

                                        ' Environmental effects
                                        Dim nearbyParticles As Integer = CountNearbyParticles(plant.Position, 50.0F)
                                        If nearbyParticles > 10 Then
                                            plant.Health += 0.02F * Deltatime ' Water helps growth
                                        End If

                                        mPlants(i) = plant
                                    End Sub)
    End Sub

    Private Sub UpdateFish()
        Parallel.For(0, FishCount, Sub(i)
                                       Dim fish As Fish = mFish(i)

                                       ' Aging and hunger
                                       fish.Age += Deltatime
                                       fish.Hunger += FISH_HUNGER_RATE * Deltatime
                                       fish.Health = Math.Max(0, fish.Health - fish.Hunger * 0.01F * Deltatime)

                                       ' Flocking behavior
                                       Dim flockForce As Vector2 = CalculateFlockingForce(i)

                                       ' Food seeking
                                       Dim foodForce As Vector2 = SeekFood(fish.Position)

                                       ' Predator avoidance
                                       Dim avoidanceForce As Vector2 = AvoidPredators(fish.Position)

                                       ' Fluid interaction
                                       Dim fluidForce As Vector2 = GetFluidForceAtPosition(fish.Position)

                                       ' Combine forces
                                       Dim totalForce As Vector2 = flockForce * 0.3F + foodForce * 0.5F + avoidanceForce * 2.0F + fluidForce * 0.1F

                                       fish.Velocity += totalForce * Deltatime
                                       fish.Velocity = New Vector2(
                                            Math.Max(-fish.Speed, Math.Min(fish.Velocity.X, fish.Speed)),
                                            Math.Max(-fish.Speed, Math.Min(fish.Velocity.Y, fish.Speed))
                                        )

                                       fish.Position += fish.Velocity * Deltatime

                                       ' Boundary wrapping
                                       WrapPosition(fish.Position)

                                       mFish(i) = fish
                                   End Sub)
    End Sub

    Private Sub UpdatePredators()
        Parallel.For(0, PredatorCount, Sub(i)
                                           Dim predator As Predator = mPredators(i)

                                           predator.Hunger += PREDATOR_HUNGER_RATE * Deltatime
                                           predator.Health = Math.Max(0, predator.Health - predator.Hunger * 0.008F * Deltatime)
                                           predator.AttackCooldown = Math.Max(0, predator.AttackCooldown - Deltatime)

                                           ' Hunt for fish
                                           Dim huntForce As Vector2 = HuntFish(predator.Position, i)

                                           ' Fluid interaction
                                           Dim fluidForce As Vector2 = GetFluidForceAtPosition(predator.Position) * 0.05F

                                           predator.Velocity += (huntForce + fluidForce) * Deltatime
                                           predator.Velocity = New Vector2(
                                                Math.Max(-predator.Speed, Math.Min(predator.Velocity.X, predator.Speed)),
                                                Math.Max(-predator.Speed, Math.Min(predator.Velocity.Y, predator.Speed))
                                            )
                                           predator.Position += predator.Velocity * Deltatime

                                           WrapPosition(predator.Position)

                                           mPredators(i) = predator
                                       End Sub)
    End Sub

    Private Sub HandleEcosystemInteractions()
        ' Fish eating plants
        For i As Integer = 0 To FishCount - 1
            Dim fish As Fish = mFish(i)
            If fish.Hunger > 0.3F Then
                For j As Integer = 0 To PlantCount - 1
                    Dim plant As Plant = mPlants(j)
                    If Vector2.Distance(fish.Position, plant.Position) < plant.Size + fish.Size Then
                        fish.Hunger = Math.Max(0, fish.Hunger - plant.NutrientValue * 0.5F)
                        fish.Health = Math.Min(1.0F, fish.Health + plant.NutrientValue * 0.3F)
                        plant.Health = Math.Max(0, plant.Health - 0.3F)
                        mFish(i) = fish
                        mPlants(j) = plant
                        Exit For
                    End If
                Next
            End If
        Next

        ' Predators eating fish
        For i As Integer = 0 To PredatorCount - 1
            Dim predator As Predator = mPredators(i)
            If predator.AttackCooldown <= 0 AndAlso predator.Hunger > 0.4F Then
                For j As Integer = 0 To FishCount - 1
                    Dim fish As Fish = mFish(j)
                    If Vector2.Distance(predator.Position, fish.Position) < predator.Size + fish.Size + 5 Then
                        predator.Hunger = Math.Max(0, predator.Hunger - 0.8F)
                        predator.Health = Math.Min(1.0F, predator.Health + 0.5F)
                        predator.AttackCooldown = 3.0F

                        ' Respawn fish
                        fish.Position = New Vector2(
                            mRandom.Range(WorldBounds.Left + 150, WorldBounds.Right - 150),
                            mRandom.Range(WorldBounds.Top + 150, WorldBounds.Bottom - 150)
                        )
                        fish.Health = 1.0F
                        fish.Hunger = 0.2F

                        mPredators(i) = predator
                        mFish(j) = fish
                        Exit For
                    End If
                Next
            End If
        Next
    End Sub

    Private Sub ApplyMouseForces()
        If Not MouseInteraction Then Return

        Dim mouseWorldPos As Vector2 = ScreenToWorld(Input.Mouse_Position)

        If Input.Mouse_Buttons_Pressed(eMouseButton.Button_1) Then
            ' Attract particles
            For i As Integer = 0 To ParticleCount - 1
                Dim particle As FluidParticle = mFluidParticles(i)
                Dim distance As Single = Vector2.Distance(particle.Position, mouseWorldPos)

                If distance < MouseForceRadius AndAlso distance > 0 Then
                    Dim direction As Vector2 = (mouseWorldPos - particle.Position) / distance
                    Dim strength As Single = (1.0F - distance / MouseForceRadius) * MouseForceStrength
                    particle.Velocity += direction * strength * Deltatime
                    mFluidParticles(i) = particle
                End If
            Next
        ElseIf Input.Mouse_Buttons_Pressed(eMouseButton.Button_2) Then
            ' Repel particles
            For i As Integer = 0 To ParticleCount - 1
                Dim particle As FluidParticle = mFluidParticles(i)
                Dim distance As Single = Vector2.Distance(particle.Position, mouseWorldPos)

                If distance < MouseForceRadius AndAlso distance > 0 Then
                    Dim direction As Vector2 = (particle.Position - mouseWorldPos) / distance
                    Dim strength As Single = (1.0F - distance / MouseForceRadius) * MouseForceStrength
                    particle.Velocity += direction * strength * Deltatime
                    mFluidParticles(i) = particle
                End If
            Next
        End If
    End Sub

    ' Helper functions for ecosystem AI
    Private Function CalculateFlockingForce(fishIndex As Integer) As Vector2
        Dim fish As Fish = mFish(fishIndex)
        Dim separation As Vector2 = Vector2.Zero
        Dim alignment As Vector2 = Vector2.Zero
        Dim cohesion As Vector2 = Vector2.Zero
        Dim count As Integer = 0

        For i As Integer = 0 To FishCount - 1
            If i = fishIndex Then Continue For

            Dim other As Fish = mFish(i)
            Dim distance As Single = Vector2.Distance(fish.Position, other.Position)

            If distance < fish.FlockingRadius Then
                ' Separation
                If distance < fish.Size * 3 Then
                    Dim diff As Vector2 = fish.Position - other.Position
                    diff = diff / Math.Max(distance, 0.1F)
                    separation += diff
                End If

                ' Alignment and Cohesion
                alignment += other.Velocity
                cohesion += other.Position
                count += 1
            End If
        Next

        If count > 0 Then
            alignment /= count
            cohesion /= count

            alignment = Vector2.Normalize(alignment) * fish.Speed - fish.Velocity
            cohesion = Vector2.Normalize(cohesion - fish.Position) * fish.Speed - fish.Velocity
        End If

        Return separation * 2.0F + alignment * 0.5F + cohesion * 0.3F
    End Function

    Private Function SeekFood(position As Vector2) As Vector2
        Dim closestPlant As Vector2 = Vector2.Zero
        Dim minDistance As Single = Single.MaxValue

        For i As Integer = 0 To PlantCount - 1
            Dim plant As Plant = mPlants(i)
            If plant.Health > 0.3F Then
                Dim distance As Single = Vector2.Distance(position, plant.Position)
                If distance < minDistance Then
                    minDistance = distance
                    closestPlant = plant.Position
                End If
            End If
        Next

        If minDistance < Single.MaxValue Then
            Return Vector2.Normalize(closestPlant - position) * 100.0F
        End If

        Return Vector2.Zero
    End Function

    Private Function AvoidPredators(position As Vector2) As Vector2
        Dim avoidanceForce As Vector2 = Vector2.Zero

        For i As Integer = 0 To PredatorCount - 1
            Dim predator As Predator = mPredators(i)
            Dim distance As Single = Vector2.Distance(position, predator.Position)

            If distance < predator.HuntingRadius Then
                Dim direction As Vector2 = position - predator.Position
                If direction.Length() > 0 Then
                    direction = Vector2.Normalize(direction)
                    Dim strength As Single = (predator.HuntingRadius - distance) / predator.HuntingRadius
                    avoidanceForce += direction * strength * 300.0F
                End If
            End If
        Next

        Return avoidanceForce
    End Function

    Private Function HuntFish(position As Vector2, predatorIndex As Integer) As Vector2
        Dim predator As Predator = mPredators(predatorIndex)
        Dim closestFish As Vector2 = Vector2.Zero
        Dim minDistance As Single = Single.MaxValue

        For i As Integer = 0 To FishCount - 1
            Dim fish As Fish = mFish(i)
            Dim distance As Single = Vector2.Distance(position, fish.Position)

            If distance < predator.HuntingRadius AndAlso distance < minDistance Then
                minDistance = distance
                closestFish = fish.Position
            End If
        Next

        If minDistance < Single.MaxValue Then
            Return Vector2.Normalize(closestFish - position) * predator.Speed * 1.5F
        End If

        Return Vector2.Zero
    End Function

    Private Function GetFluidForceAtPosition(position As Vector2) As Vector2
        Dim force As Vector2 = Vector2.Zero
        Dim count As Integer = 0

        For i As Integer = 0 To ParticleCount - 1
            Dim particle As FluidParticle = mFluidParticles(i)
            Dim distance As Single = Vector2.Distance(position, particle.Position)

            If distance < 30.0F AndAlso distance > 0 Then
                force += particle.Velocity * (1.0F - distance / 30.0F)
                count += 1
            End If
        Next

        If count > 0 Then
            Return force / count
        End If

        Return Vector2.Zero
    End Function

    Private Sub WrapPosition(ByRef position As Vector2)
        If position.X < WorldBounds.Left Then position.X = WorldBounds.Right
        If position.X > WorldBounds.Right Then position.X = WorldBounds.Left
        If position.Y < WorldBounds.Top Then position.Y = WorldBounds.Bottom
        If position.Y > WorldBounds.Bottom Then position.Y = WorldBounds.Top
    End Sub

    Private Function CountNearbyParticles(position As Vector2, radius As Single) As Integer
        Dim count As Integer = 0
        For i As Integer = 0 To ParticleCount - 1
            If Vector2.Distance(position, mFluidParticles(i).Position) < radius Then
                count += 1
            End If
        Next
        Return count
    End Function

    ' Smoothing kernels for SPH
    Private Function SmoothingKernel(distance As Single, radius As Single) As Single
        If distance >= radius Then Return 0
        Dim volume As Single = CSng((Math.PI * Math.Pow(radius, 4)) / 6)
        Return Math.Max(0, radius * radius - distance * distance) / volume
    End Function

    Private Function SmoothingKernelDerivative(distance As Single, radius As Single) As Single
        If distance >= radius Then Return 0
        Dim volume As Single = CSng((Math.PI * Math.Pow(radius, 4)) / 6)
        Return -distance / volume
    End Function

    Private Function ViscosityKernel(distance As Single, radius As Single) As Single
        If distance >= radius Then Return 0
        Dim volume As Single = CSng((Math.PI * Math.Pow(radius, 8)) / 4)
        Return (radius - distance) / volume
    End Function

    Private Sub DrawBackground()
        ' Draw world bounds
        DrawRectangleOutline(WorldBounds, Color.FromArgb(60, 60, 80), 2.0F)

        ' Draw grid
        Dim gridSpacing As Single = 200.0F
        Dim gridColor As Color = Color.FromArgb(20, 30, 40)

        For x As Single = WorldBounds.Left To WorldBounds.Right Step gridSpacing
            DrawLine(New Vector2(x, WorldBounds.Top), New Vector2(x, WorldBounds.Bottom), gridColor, 1.0F)
        Next

        For y As Single = WorldBounds.Top To WorldBounds.Bottom Step gridSpacing
            DrawLine(New Vector2(WorldBounds.Left, y), New Vector2(WorldBounds.Right, y), gridColor, 1.0F)
        Next
    End Sub

    Private Sub DrawFluidParticles()
        For i As Integer = 0 To ParticleCount - 1
            Dim particle As FluidParticle = mFluidParticles(i)

            ' Color based on temperature and density
            Dim tempColor As Single = particle.Temperature
            Dim densityFactor As Single = Math.Min(1.0F, particle.Density / (REST_DENSITY * 1.5F))

            Dim r As Integer = CInt(50 + tempColor * 150 + densityFactor * 50)
            Dim g As Integer = CInt(100 + tempColor * 100)
            Dim b As Integer = CInt(200 + tempColor * 55 - densityFactor * 100)

            Dim color As Color = Color.FromArgb(255, Math.Max(0, Math.Min(255, r)), Math.Max(0, Math.Min(255, g)), Math.Max(0, Math.Min(255, b)))

            Dim size As Single = 3.0F + densityFactor * 2.0F
            DrawCircle(particle.Position, size, color)

            ' Draw velocity trails for fast-moving particles
            If particle.Velocity.Length() > 50.0F Then
                Dim trailEnd As Vector2 = particle.Position - Vector2.Normalize(particle.Velocity) * 15.0F
                DrawLine(particle.Position, trailEnd, Color.FromArgb(100, color), 1.0F)
            End If
        Next
    End Sub

    Private Sub DrawEcosystem()
        ' Draw plants
        For i As Integer = 0 To PlantCount - 1
            Dim plant As Plant = mPlants(i)
            Dim healthColor As Single = plant.Health
            Dim r As Integer = Math.Max(0, Math.Min(255, CInt(50 + healthColor * 50)))
            Dim g As Integer = Math.Max(0, Math.Min(255, CInt(100 + healthColor * 155)))
            Dim b As Integer = Math.Max(0, Math.Min(255, CInt(50 + healthColor * 50)))
            Dim color As Color = Color.FromArgb(255, r, g, b)

            DrawCircle(plant.Position, plant.Size, color)

            ' Draw stems
            Dim stemEnd As Vector2 = plant.Position + New Vector2(0, plant.Size + 10)
            DrawLine(plant.Position, stemEnd, Color.FromArgb(80, 60, 40), 2.0F)
        Next

        ' Draw fish
        For i As Integer = 0 To FishCount - 1
            Dim fish As Fish = mFish(i)
            Dim healthColor As Single = fish.Health * (1.0F - fish.Hunger * 0.5F)
            Dim color As Color = Color.FromArgb(255, CInt(200 + healthColor * 55), CInt(150 + healthColor * 105), CInt(50 + healthColor * 100))

            DrawCircle(fish.Position, fish.Size, color)

            ' Draw direction indicator
            If fish.Velocity.Length() > 5.0F Then
                Dim direction As Vector2 = Vector2.Normalize(fish.Velocity)
                Dim tip As Vector2 = fish.Position + direction * (fish.Size + 3)
                DrawLine(fish.Position, tip, Color.White, 1.0F)
            End If

            ' Draw flocking radius when focused
            If i = 0 Then ' Just for first fish to avoid clutter
                DrawCircleOutline(fish.Position, fish.FlockingRadius, Color.FromArgb(30, 255, 255, 255))
            End If
        Next

        ' Draw predators
        For i As Integer = 0 To PredatorCount - 1
            Dim predator As Predator = mPredators(i)
            Dim healthColor As Single = predator.Health * (1.0F - predator.Hunger * 0.3F)
            Dim color As Color = Color.FromArgb(255, CInt(150 + healthColor * 105), CInt(50 + healthColor * 50), CInt(50 + healthColor * 50))

            DrawCircle(predator.Position, predator.Size, color)

            ' Draw hunting radius
            If predator.Hunger > 0.5F Then
                DrawCircleOutline(predator.Position, predator.HuntingRadius, Color.FromArgb(20, 255, 100, 100))
            End If

            ' Draw attack indicator
            If predator.AttackCooldown > 0 Then
                Dim attackColor As Color = Color.FromArgb(CInt(predator.AttackCooldown * 85), 255, 0, 0)
                DrawCircle(predator.Position, predator.Size + 5, attackColor)
            End If
        Next
    End Sub

    Private Sub DrawUI()
        mInfoPosition = New Vector2(20, 20)

        WriteScreen($"FPS: {LastFPS:F1}", Color.White)
        WriteScreen($"Particles: {ParticleCount}", Color.Cyan)
        WriteScreen($"Fish: {FishCount}", Color.Yellow)
        WriteScreen($"Plants: {PlantCount}", Color.Green)
        WriteScreen($"Predators: {PredatorCount}", Color.Red)
        WriteScreen($"Camera: {Virtual_Cam_Position} Zoom: {Virtual_Cam_Zoom:F2}", Color.Gray)
        WriteScreen($"Mouse Forces: {If(MouseInteraction, "ON", "OFF")}", If(MouseInteraction, Color.Lime, Color.Gray))

        ' Performance metrics
        Dim avgDensity As Single = 0
        For i As Integer = 0 To ParticleCount - 1
            avgDensity += mFluidParticles(i).Density
        Next
        avgDensity /= ParticleCount

        WriteScreen($"Avg Density: {avgDensity:F1}", Color.Cyan)
        WriteScreen($"Time: {TotalTime:F1}s", Color.White)

        ' Instructions
        Engine.Text_Write("WASD: Move Camera | Mouse Wheel: Zoom | LMB: Attract | RMB: Repel | M: Toggle Mouse | +/-: Particles | R: Reset",
                         New Vector2(10, Engine.Viewport_Height - 25), Color.LightGray, 0.45F)
    End Sub

    Private Sub WriteScreen(text As String, color As Color)
        Engine.Text_Write(text, New Vector2(mInfoPosition.X, mInfoPosition.Y + mInfoLineY), color, 0.5F)
        mInfoLineY += 16
    End Sub

    ' Drawing helper methods
    Private Sub DrawCircle(position As Vector2, radius As Single, color As Color)
        Dim screenPos As Vector2 = WorldToScreen(position)
        Dim screenRadius As Single = radius * Virtual_Cam_Zoom
        mSprite.Draw(Vector4.Zero, New Vector4(screenPos.X - screenRadius, screenPos.Y - screenRadius, screenRadius * 2, screenRadius * 2), color, 0.5F)
    End Sub

    Private Sub DrawCircleOutline(position As Vector2, radius As Single, color As Color)
        Dim screenPos As Vector2 = WorldToScreen(position)
        Dim screenRadius As Single = radius * Virtual_Cam_Zoom
        Dim segments As Integer = Math.Max(8, CInt(screenRadius / 4))

        For i As Integer = 0 To segments - 1
            Dim angle1 As Single = CSng((i / segments) * Math.PI * 2)
            Dim angle2 As Single = CSng(((i + 1) / segments) * Math.PI * 2)

            Dim p1 As Vector2 = screenPos + New Vector2(CSng(Math.Cos(angle1)), CSng(Math.Sin(angle1))) * screenRadius
            Dim p2 As Vector2 = screenPos + New Vector2(CSng(Math.Cos(angle2)), CSng(Math.Sin(angle2))) * screenRadius

            mSprite.DrawLine(Vector4.Zero, p1, p2, 1.0F, color, 0.4F)
        Next
    End Sub

    Private Sub DrawLine(start As Vector2, endPos As Vector2, color As Color, width As Single)
        Dim screenStart As Vector2 = WorldToScreen(start)
        Dim screenEnd As Vector2 = WorldToScreen(endPos)
        mSprite.DrawLine(Vector4.Zero, screenStart, screenEnd, width * Virtual_Cam_Zoom, color, 0.6F)
    End Sub

    Private Sub DrawRectangleOutline(rect As Rectangle, color As Color, width As Single)
        Dim tl As Vector2 = New Vector2(rect.Left, rect.Top)
        Dim tr As Vector2 = New Vector2(rect.Right, rect.Top)
        Dim bl As Vector2 = New Vector2(rect.Left, rect.Bottom)
        Dim br As Vector2 = New Vector2(rect.Right, rect.Bottom)

        DrawLine(tl, tr, color, width)
        DrawLine(tr, br, color, width)
        DrawLine(br, bl, color, width)
        DrawLine(bl, tl, color, width)
    End Sub

    Private Function WorldToScreen(worldPos As Vector2) As Vector2
        Return (worldPos - Virtual_Cam_Position) * Virtual_Cam_Zoom
    End Function

    Private Function ScreenToWorld(screenPos As Vector2) As Vector2
        Return screenPos / Virtual_Cam_Zoom + Virtual_Cam_Position
    End Function

    Private Sub HandleInput()
        ' Camera movement
        Dim moveSpeed As Single = Camera_Speed / Virtual_Cam_Zoom

        If Input.Key(Keys.W) OrElse Input.Key(Keys.Up) Then
            Virtual_Cam_Position.Y -= moveSpeed * Deltatime
        End If
        If Input.Key(Keys.S) OrElse Input.Key(Keys.Down) Then
            Virtual_Cam_Position.Y += moveSpeed * Deltatime
        End If
        If Input.Key(Keys.A) OrElse Input.Key(Keys.Left) Then
            Virtual_Cam_Position.X -= moveSpeed * Deltatime
        End If
        If Input.Key(Keys.D) OrElse Input.Key(Keys.Right) Then
            Virtual_Cam_Position.X += moveSpeed * Deltatime
        End If

        ' Zoom
        Dim mouseScroll As Single = Input.Mouse_Position_Relative.Z
        If mouseScroll > 0 Then
            Virtual_Cam_Zoom = Math.Min(Virtual_Cam_Zoom * 1.1F, 3.0F)
        ElseIf mouseScroll < 0 Then
            Virtual_Cam_Zoom = Math.Max(Virtual_Cam_Zoom * 0.9F, 0.1F)
        End If

        ' Controls
        If Input.Key_Pressed(Keys.M) Then
            MouseInteraction = Not MouseInteraction
        End If

        If Input.Key_Pressed(Keys.Oemplus) Then
            ParticleCount = Math.Min(ParticleCount + 100, MAX_PARTICLES)
        ElseIf Input.Key_Pressed(Keys.OemMinus) Then
            ParticleCount = Math.Max(ParticleCount - 100, 500)
        End If

        If Input.Key_Pressed(Keys.R) Then
            InitializeFluidParticles()
            InitializeEcosystem()
        End If

        If Input.Key_Pressed(Keys.Escape) Then
            Engine.Quit()
        End If
    End Sub

    Private Sub Set_Camera(position As Vector2, zoom As Single)
        Virtual_Cam_Position = position - Engine.Viewport_Size / 2.0F / zoom
        Virtual_Cam_Zoom = zoom
    End Sub

    ' Data structures
    Private Structure FluidParticle
        Public Position As Vector2
        Public Velocity As Vector2
        Public Force As Vector2
        Public Density As Single
        Public Pressure As Single
        Public Mass As Single
        Public Temperature As Single
    End Structure

    Private Structure Fish
        Public Position As Vector2
        Public Velocity As Vector2
        Public Size As Single
        Public Health As Single
        Public Hunger As Single
        Public Speed As Single
        Public FlockingRadius As Single
        Public Age As Single
    End Structure

    Private Structure Plant
        Public Position As Vector2
        Public Size As Single
        Public Health As Single
        Public GrowthRate As Single
        Public NutrientValue As Single
    End Structure

    Private Structure Predator
        Public Position As Vector2
        Public Velocity As Vector2
        Public Size As Single
        Public Health As Single
        Public Hunger As Single
        Public Speed As Single
        Public HuntingRadius As Single
        Public AttackCooldown As Single
    End Structure

    ' Window events
    Private Sub Game_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        If Engine.DoRender Then
            Engine.Quit()
            e.Cancel = True
        End If
    End Sub
End Class