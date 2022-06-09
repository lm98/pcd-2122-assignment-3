package controller

object SimulationMain:
  @main def startSim =
    new Simulator(
      1000,
      5000,
      Runtime.getRuntime.availableProcessors + 1
    ).execute()
