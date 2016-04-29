class Gate extends Mine {

public void setEnabled(boolean flag){
    curPose = flag ? 1 : 0;
}

public boolean isEnabled() {
    return (curPose == 1) ? true : false;
}

public boolean isClosed() {
    return !isEnabled();
}

public void explode() {
}

}
