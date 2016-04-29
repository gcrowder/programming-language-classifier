class TimedHorizGate extends HorizGate {

protected int nextChangeTime;	/* ms */

public void updatePiece() {
    super.updatePiece();

    if (game.updateTime() > nextChangeTime) {
        setEnabled(!isEnabled());
        nextChangeTime = game.updateTime() + TIMETOCHANGEGATE - 50 + Game.randInt(100);
    }
}

}
