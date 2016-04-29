class SwitchedVertGate extends VertGate {

protected int nextChangeTime;	/* ms */

public void updatePiece() {
    super.updatePiece();
    if (game.updateTime() > nextChangeTime) {
        setEnabled(!isEnabled());
        nextChangeTime = game.updateTime() + TIMETOCHANGESWITCHEDGATE;
    }
}

public void openGate() {
    setEnabled(true);
    nextChangeTime = game.updateTime() + TIMETOCLOSESWITCHEDGATE;
}

public void closeGate() {
    setEnabled(false);
    nextChangeTime = game.updateTime() + TIMETOCHANGESWITCHEDGATE;
}

}
