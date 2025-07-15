# Risk

## Project Description
Risk is a multiplayer desktop application inspired by the board game Risiko, developed entirely in Scala. The project is designed with a modular and functional architecture, supporting two game modes: local and online.

### Game Modes
- **Local Mode**: Players face bot-controlled opponents without requiring a network connection.
- **Online Mode**: Multiple clients can connect to a central server to participate in multiplayer matches.

---

## Requirements
- **Java 17** or higher
- **Scala 3**
- **SBT** (Scala Build Tool)
- **Akka** for concurrency and network communication
- **ScalaFX** for the graphical interface

---

## Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/danielmec/PPS-24-Risk.git
   cd PPS-24-Risk
   ```

2. **Ensure Java, Scala, and SBT are installed**
   - Verify with:
     ```bash
     java -version
     scala -version
     sbt --version
     ```

3. **(Optional) Install ScalaFX**
   - ScalaFX is managed as an SBT dependency, no manual installation is required.

---

## Running the Server

Open a terminal in the project folder and start the server with:

```bash
sbt "project server" run
```

---

## Running the Client

Open another terminal in the project folder and start the client with:

```bash
sbt "project client" run
```

You can start multiple clients to simulate multiple players.

---

## Notes

- The project is compatible with Mac, Linux, and Windows (requires JavaFX/ScalaFX to be properly configured on the system).

---

## Known Issues

- If you encounter issues with JavaFX on Mac/Linux, ensure the JavaFX libraries are correctly installed and configured in your environment.
- In case of dependency errors, run `sbt clean` followed by `sbt compile`.

## Windows Requirements for Running Release Executables

### Option 1: JDK with JavaFX included (RECOMMENDED)
- Download **Liberica JDK Full**: https://bell-sw.com/pages/downloads/
- Or **Azul Zulu JDK FX**: https://www.azul.com/downloads/?package=jdk-fx

### Option 2: Separate JavaFX SDK
1. Download JavaFX SDK: https://gluonhq.com/products/javafx/
2. Extract to a folder (e.g., `C:\javafx-sdk-21\`)
3. Run with:
```cmd
java --module-path "C:\javafx-sdk-21\lib" --add-modules javafx.controls,javafx.fxml -jar RiskClient.jar
```

### Running the Application
```cmd
# For Windows with JavaFX included
java -jar RiskClient.jar

# For Windows with separate JavaFX SDK
java --module-path "C:\javafx-sdk-21\lib" --add-modules javafx.controls,javafx.fxml -jar RiskClient.jar
```

---

## Contact

If you need assistance with running the project, open an issue on GitHub or contact the developers
