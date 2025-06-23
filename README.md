# Risk

## Descrizione del Progetto
Risk è un'applicazione desktop multiplayer ispirata al gioco da tavolo Risiko, sviluppata interamente in Scala. Il progetto è progettato con un'architettura modulare e funzionale, supportando due modalità di gioco: locale e online.

### Modalità di Gioco
- **Modalità Locale**: Giocatori affrontano avversari controllati da bot senza necessità di connessione di rete.
- **Modalità Online**: Più client possono connettersi a un server centrale per partecipare a partite multiplayer.

---

## Requisiti
- **Java 17** o superiore
- **Scala 3**
- **SBT** (Scala Build Tool)
- **Akka** per la gestione della concorrenza e della comunicazione di rete
- **ScalaFX** per l'interfaccia grafica

---

## Installazione

1. **Clona il repository**
   ```bash
   git clone https://github.com/danielmec/PPS-25-Risk.git
   cd PPS-25-Risk
   ```

2. **Assicurati di avere Java, Scala e SBT installati**
   - Puoi verificare con:
     ```bash
     java -version
     scala -version
     sbt --version
     ```

3. **(Opzionale) Installa ScalaFX**
   - ScalaFX viene gestito come dipendenza SBT, non serve installazione manuale.

---

## Esecuzione del Server

Apri un terminale nella cartella del progetto e avvia il server con:

```bash
sbt "project server" run
```

---

## Esecuzione del Client

Apri un altro terminale nella cartella del progetto e avvia il client con:

```bash
sbt "project client" run
```

Puoi avviare più client per simulare più giocatori.

---

## Note

- Assicurati che il server sia avviato **prima** di avviare i client per la modalità online.
- Per la modalità locale, puoi avviare solo il client.
- Il progetto è compatibile con Mac, Linux e Windows (richiede JavaFX/ScalaFX funzionanti sul sistema).

---

## Problemi noti

- Se hai problemi con JavaFX su Mac/Linux, assicurati che le librerie JavaFX siano correttamente installate e configurate nel tuo ambiente.
- In caso di errori di dipendenze, esegui `sbt clean` seguito da `sbt compile`.

---

## Contatti

Per segnalare bug o richiedere supporto, apri una issue su GitHub o contatta gli sviluppatori tramite la repository.