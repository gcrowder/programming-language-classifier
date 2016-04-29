class PassableVertGate extends VertGate {

protected int nextChangeTime;	/* ms */

public void updatePiece() {
    super.updatePiece();

    if (game.helicopter() != null && game.helicopter().touchesRect(pos)) {
        setEnabled(true);
        nextChangeTime = game.updateTime() + TIMETOOPENGATE;
    } else if (game.updateTime() > nextChangeTime) {
        setEnabled(!isEnabled());
        nextChangeTime = game.updateTime() + (isEnabled() ? TIMETOCLOSEGATE : TIMETOOPENGATE);
    }
}

}
