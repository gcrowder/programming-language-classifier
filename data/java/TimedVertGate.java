class TimedVertGate extends VertGate {

protected int nextChangeTime;	/* ms */

public void updatePiece() {
    super.updatePiece();
    if (game.updateTime() > nextChangeTime) {
        setEnabled(!isEnabled());
        nextChangeTime = game.updateTime() + TIMETOCHANGEGATE - 30 + Game.randInt(60);
    }
}

}
