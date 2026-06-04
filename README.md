# Presifeur 🃏

Presifeur is a modern implementation of the classic French card shedding game **Président** (also known as *Trouduc*, or *Asshole* / *Scum* / *Landlord* in English). 

The project features a **shared game engine**, a ZIO-powered **multiplayer server**, a terminal **CLI client**, and a web client using **Scala.js** with an interactive **3D table rendering using Three.js**.

---

## 🛠️ Project Architecture

The codebase is built with Scala 3 using a shared-source architecture between the JVM backend and the Scala.js frontend:

- **[`src/main/scala/presifeur/model`](file:///home/noa/Documents/DO4/scala/presifeur/src/main/scala/presifeur/model)**: Core game entities (`Card`, `Player`, `GameState`, `Play`, `Deck`).
- **[`src/main/scala/presifeur/engine`](file:///home/noa/Documents/DO4/scala/presifeur/src/main/scala/presifeur/engine)**: Pure functional game logic (`GameEngine`).
- **[`src/main/scala/presifeur/server`](file:///home/noa/Documents/DO4/scala/presifeur/src/main/scala/presifeur/server)**: Multiplayer game room coordinating players, WebSocket messaging, and lobbies via **ZIO HTTP & WebSockets**.
- **[`src/main/scala/presifeur/web`](file:///home/noa/Documents/DO4/scala/presifeur/src/main/scala/presifeur/web)**: Frontend logic compiled to JS using **Scala.js** and **Three.js** interop for 3D card layout and animations.
- **[`src/main/scala/presifeur/io`](file:///home/noa/Documents/DO4/scala/presifeur/src/main/scala/presifeur/io)** & **[`ConsoleMain.scala`](file:///home/noa/Documents/DO4/scala/presifeur/src/main/scala/presifeur/ConsoleMain.scala)**: ZIO-based terminal CLI client.

---

## 🚀 How to Run the Project

Ensure you have **Java JDK 11+** and **sbt** installed on your system.

### 🌐 Web Multiplayer Version

1. **Compile the Scala.js frontend:**
   In your terminal, run the following command to build the frontend assets:
   ```bash
   sbt fastOptJS
   ```
   *(This compiles the frontend code to `target/scala-3.4.2/presifeur-fastopt/main.js`)*

2. **Start the ZIO backend server:**
   Start the game server with the configured alias:
   ```bash
   sbt server
   ```
   *(The server will start on port `8080` and host both the WebSocket API and static files)*

3. **Play the game:**
   - Open your browser and go to: **`http://localhost:8080`**
   - Connect with your player name.
   - Open at least **3 separate browser tabs/windows** (representing 3 players) to meet the minimum player count.
   - Once 3 or more players are connected, click **Commencer la partie** (Start game) to begin!
   - You can click on cards to select/deselect them, and press **Play** (or **Pass**).
   - Enjoy the responsive layout and the **interactive 3D card table** (rendered with Three.js)!

### 💻 CLI Console Version

If you prefer to play a local game directly in the command line:

1. **Run the CLI client:**
   In your terminal, execute:
   ```bash
   sbt cli
   ```
2. **Follow the prompt instructions:**
   - Enter the names of the players (minimum 3, separated by commas).
   - Enter plays by typing card symbols (e.g. `5♥ 5♠` or `5C 5P` for the five of Hearts and five of Spades).
   - Type `passer` to pass your turn.
