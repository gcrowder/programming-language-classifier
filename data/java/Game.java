/*
        Game.java
        Copyright (c) 1990-2004, Apple Computer, Inc., all rights reserved.
        Author: Ali Ozer

        Subclass of NSView implementing main game loop
 	 (an MVC design would have split the view from the game)
*/
/*
 IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc. ("Apple") in
 consideration of your agreement to the following terms, and your use, installation, 
 modification or redistribution of this Apple software constitutes acceptance of these 
 terms.  If you do not agree with these terms, please do not use, install, modify or 
 redistribute this Apple software.
 
 In consideration of your agreement to abide by the following terms, and subject to these 
 terms, Apple grants you a personal, non-exclusive license, under Apple’s copyrights in 
 this original Apple software (the "Apple Software"), to use, reproduce, modify and 
 redistribute the Apple Software, with or without modifications, in source and/or binary 
 forms; provided that if you redistribute the Apple Software in its entirety and without 
 modifications, you must retain this notice and the following text and disclaimers in all 
 such redistributions of the Apple Software.  Neither the name, trademarks, service marks 
 or logos of Apple Computer, Inc. may be used to endorse or promote products derived from 
 the Apple Software without specific prior written permission from Apple. Except as expressly
 stated in this notice, no other rights or licenses, express or implied, are granted by Apple
 herein, including but not limited to any patent rights that may be infringed by your 
 derivative works or by other works in which the Apple Software may be incorporated.
 
 The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO WARRANTIES, 
 EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF NON-INFRINGEMENT, 
 MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS 
 USE AND OPERATION ALONE OR IN COMBINATION WITH YOUR PRODUCTS.
 
 IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR CONSEQUENTIAL 
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) ARISING IN ANY WAY OUT OF THE USE, 
 REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE APPLE SOFTWARE, HOWEVER CAUSED AND 
 WHETHER UNDER THEORY OF CONTRACT, TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR 
 OTHERWISE, EVEN IF APPLE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
All time values in milliseconds
All distances in points (and 3 points = 1 meter, but that doesn't really matter)
*/

import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;
import java.util.Date;		// Because our Date on the bridge isn't complete...
import java.util.Vector;
import java.util.Random;

public class Game extends NSView {

/* Max time to finish a level in order to get bonus points... */
public static final int MAXBONUSTIME = 120000;	/* ms */

/* Bonus for finishing a level in under MAXBONUSTIME */
public static final int BONUS = 10;

public static final float HELICOPTERSTARTX = 2.0f;
public static final float HELICOPTERSTARTY = 80.0f;

public static final int TIMING = 0;

public static final float ANIMATIONINTERVAL = 0.04f;

/* Maximum time a frame can take. This is the maximum value returned by the elapsedTime method. */
public static final int MAXUPDATETIME = 100;

/* Maximum velocity (in pixel/seconds) for any object in the game */
public static final int MAXVELX = 65;
public static final int MAXVELY = 65;
public static final int MINVELX = 1;
public static final int MINVELY = 1;

/* User commands */
public static final int NoCommand = 0;
public static final int GoUpCommand = 1;
public static final int GoUpRightCommand = 2;
public static final int GoRightCommand = 3;
public static final int GoDownRightCommand = 4;
public static final int GoDownCommand = 5;
public static final int GoDownLeftCommand = 6;
public static final int GoLeftCommand = 7;
public static final int GoUpLeftCommand = 8;
public static final int StopCommand = 9;
public static final int FireCommand = 10;
public static final int NumCommands = 11;

/* Timing related */
private static final boolean timing = false;
private int timingTime;
private int timingFrames;
private int timingMissed;

/* "Cheat" (or "test") mode. If not zero, this stores the number of times hit while in cheat mode. */
private int cheating;	

/* Vectors of GamePieces, grouped by piece type */
private Vector pieces[] = new Vector[GamePiece.LastGamePiece];	

/* References to some special pieces... */
private Background background;
private Helicopter helicopter;
private GamePiece focusObject;

/* Outlets to UI elements */
private NSTextField statusField;
private NSTextField scoreField;
private RemainingHelicopters livesField;
private NSTextField levelField;
private NSTextField highscoreField;
private InputIndicator inputIndicator;
private NSButton pauseButton;

/* The next three are UI gadgets in the Prefs panel */
private NSMatrix levelMatrix;
private NSSlider levelSlider;
private NSTextField levelPrefsIndicator;
private NSButton soundPrefItem;

/* Game variables */
private NSTimer timer;
private NSPoint lastOrigin;
private boolean gameOver;	
private boolean bonusGiven;	
private boolean newHighScore;	// Indicates a new high score was reached
private boolean started;	// If true, indicates the helicopter started moving in this level
public int score, lives, level, highScore, highLevel, numFrames;
/*
    updateTime is the game clock; only stops when paused.
    updateTime is updated only between frames.
    elapsedTime is the time since last frame.
    timeStopped is the time game was stopped (including overrun frames)
    pausedAt is the time at which the game was paused.
    levelStartedAt is the time at which the helicopter started moving
    at this level. Used in determining bonus points. If -1, then the
    helicopter is still at the start of the level.
*/
public int updateTime, elapsedTime, timeStopped, pausedAt, levelStartedAt;	/* ms */
public Date gameStartTime;
public int lastCommand;

public boolean isDemo;

// Some misc game stuff

private static Random randomNumberGenerator;

public static int randInt(int n) {	// Random integer 0..n (inclusive)
    if (randomNumberGenerator == null) randomNumberGenerator = new Random();
    int r = randomNumberGenerator.nextInt() % (n + 1);
    return (r < 0) ? -r : r;
}

public static boolean oneIn(int n) {	// Returns true one in n times
    return randInt(n - 1) == 0;
}

public static double timeInSeconds(int ms) {
    return ms / 1000.0f;
}

public static int secondsToMilliseconds(double seconds) {
    return (int)(seconds * 1000.0f);
}

public static float restrictValue(float val, float max) {
    if (val > max) return max;
    else if (val < -max) return -max;
    else return val;
}

public static float distance(NSPoint p1, NSPoint p2) {
    return (float)Math.sqrt((p1.x() - p2.x()) * (p1.x() - p2.x()) + (p1.y() - p2.y()) * (p1.y() - p2.y()));
}

public static int minInt(int a, int b) {
    if (a < b) return a;
    return b;
}

public static int maxInt(int a, int b) {
    if (a > b) return a;
    return b;
}

public static float minFloat(float a, float b) {
    if (a < b) return a;
    return b;
}

public static float maxFloat(float a, float b) {
    if (a > b) return a;
    return b;
}

// Sound stuff

private static boolean soundRequested;

private static NSSound fireSound;
private static NSSound explosionSound;
private static NSSound loudExplosionSound;

static boolean initSounds() {
    if (fireSound != null) return true;	// Already done...
    fireSound = NSSound.soundNamed("EnemyFire");
    explosionSound = NSSound.soundNamed("LowExplosion");
    loudExplosionSound = NSSound.soundNamed("LoudExplosion");
    return fireSound != null && explosionSound != null && loudExplosionSound != null;
}

public void playEnemyFireSound() {
    if (soundRequested) fireSound.play();
}

public void playFriendlyFireSound() {
    if (soundRequested) fireSound.play();	// For now, the same sound...
}

public void playExplosionSound() {
    if (soundRequested) explosionSound.play();
}

public void playLoudExplosionSound() {
    if (soundRequested) loudExplosionSound.play();
}

// We initialize some pieces ahead of time to prevent jerkiness during the game.
// This should really be done more dynamically...

private static boolean nonLazyInitDone = false;

private static void cacheImage(String imageName) {
}

public static void doNonLazyInitialization() {
    if (nonLazyInitDone) return;
    nonLazyInitDone = true;

    GamePiece.cacheImageNamed("bullet");
    GamePiece.cacheImageNamed("missile");
    GamePiece.cacheImageNamed("smartmissile");
    GamePiece.cacheImageNamed("arrowbaseexplosion");
    GamePiece.cacheImageNamed("backshooterexplosion");
    GamePiece.cacheImageNamed("bigexplosion");
    GamePiece.cacheImageNamed("dropshipexplosion");
    GamePiece.cacheImageNamed("hexplosion");
    GamePiece.cacheImageNamed("hbexplosion");
    GamePiece.cacheImageNamed("mbexplosion");
    GamePiece.cacheImageNamed("smallexplosion");
    GamePiece.cacheImageNamed("smallmineexplosion");
    GamePiece.cacheImageNamed("smartmbexplosion");
    GamePiece.cacheImageNamed("smartmineexplosion");
}


// Methods...

// Mostly a hack; assures that the second game we create is a "demo"
static boolean alreadyLoadedAGame = false;

public Game(NSRect rect) {
    super(rect);

    allocateGState();

    if (!alreadyLoadedAGame) {	// Hack; just allow one game
        alreadyLoadedAGame = true;
    } else {
        setDemo(true);
    }
    
    background = new Background();
    background.initInGame(this);

    int cnt;
    for (cnt = 0; cnt < GamePiece.LastGamePiece; cnt++) {
        pieces[cnt] = new Vector(40);
    }

    readHighScore();

    boolean soundDisabled = NSUserDefaults.standardUserDefaults().booleanForKey("SoundDisabled");
    setSoundRequested(!soundDisabled);

    doNonLazyInitialization();
}

public boolean acceptsFirstResponder() {
    return true;
}

public void viewWillMoveToWindow(NSWindow newWindow) {
    NSWindow oldWindow = window();
    if (oldWindow != null) NSNotificationCenter.defaultCenter().removeObserver(this, "NSWindowDidUpdateNotification", oldWindow);
    if (newWindow != null) {
	Class[] classes = {this.getClass()};
	NSNotificationCenter.defaultCenter().addObserver(this, new NSSelector("windowDidUpdate", classes), "NSWindowDidUpdateNotification", newWindow);
    }
}

public boolean isOpaque() {
    return true;
}

public void setDemo(boolean flag) {
    isDemo = flag;
}

public boolean isDemo() {
    return isDemo;
}

public int timeInMsSinceGameBegan() {
    long ms = gameStartTime.getTime();
    long msNow = (new Date()).getTime();
    int diff = (int)(msNow - ms);
    return diff - timeStopped;
}

protected void setHighscoreField(NSTextField anObject) {
    highscoreField = anObject;
    highscoreField.setIntValue(highScore);
}

public void markGameAsRunning() {
    if (started == false) {
        started = true;
        displayRunningMessage();
        if (levelStartedAt == -1) levelStartedAt = updateTime;
    }
}

/* Convert a keypress to a command. Note that because the game uses the numberic keypad, and because we want the keys to work whether or not "NumLock" is enabled, we check the various function keys corresponding to the numbers on the keypad. The "5" key has no corresponding function key, unfortunately...
*/
protected int commandForKey(char key) {
    switch (key) {
        case '8': case NSEvent.UpArrowFunctionKey:		return GoUpCommand; 
        case '6': case NSEvent.RightArrowFunctionKey:		return GoRightCommand; 
        case '2': case NSEvent.DownArrowFunctionKey:		return GoDownCommand; 
        case '4': case NSEvent.LeftArrowFunctionKey:		return GoLeftCommand; 
        case '9': case NSEvent.PageUpFunctionKey:		return GoUpRightCommand; 
        case '3': case NSEvent.PageDownFunctionKey:		return GoDownRightCommand; 
        case '1': case NSEvent.EndFunctionKey:			return GoDownLeftCommand; 
        case '7': case NSEvent.HomeFunctionKey:			return GoUpLeftCommand; 
        case '5': case '0': case NSEvent.InsertFunctionKey:	return StopCommand; 
        case '.': case ' ': case NSEvent.DeleteFunctionKey: 	return FireCommand; 
        default: 						return NoCommand;
    }
}

/* Recognize a certain sequence of input keys to go into "cheat" mode */
private static char bugCmd = '*';

public void keyDown(NSEvent event) {

    if (isPaused()) {
        super.keyDown(event);
    } else {
        char key = (event.characters().length() < 1) ? '\0' : event.characters().charAt(0);
        int command = commandForKey(key);

        switch (command) {
        case NoCommand:	// Cheat mode?
            if ((key == 'B' && bugCmd == '*') || (key == 'u' && bugCmd == 'B') || (key == 'g' && bugCmd == 'u')) {
                bugCmd = key;
                if (bugCmd == 'g') {
                    bugCmd = '*';
                    if (cheating != 0) {
                        statusField.setIntValue(cheating - 1);
                        cheating = 0;
                    } else {
                        statusField.setStringValue("Hey!");
                        cheating = 1;
                    }
                }
            } else {
                bugCmd = '*';
                if (key == 'h') {	// Quick hide
                    NSApplication.sharedApplication().hide(null);
                } else {
                    super.keyDown(event);
                }
            }
            break;

        case FireCommand:
            markGameAsRunning();
            if (helicopter != null) helicopter.startFiring();
            break;

        default:
            if (lastCommand != command) {
                if (lastCommand != NoCommand) {
                    stopLastCommand();
                }
                lastCommand = command;
                markGameAsRunning();
                if (helicopter != null) helicopter.setCommand(lastCommand);
                inputIndicator.turnOn(command);
            }
            break;
        }
    }
}

public void keyUp(NSEvent event) {
    if (isPaused()) {
        super.keyUp(event);
    } else {
        char key = (event.characters().length() < 1) ? '\0' : event.characters().charAt(0);
        int command = commandForKey(key);

        if (command == FireCommand) {
            if (helicopter != null) helicopter.stopFiring();
        } else if (command == lastCommand) {
            stopLastCommand();
        } else {
            super.keyUp(event);
        }
    }
}    

public void stopLastCommand() {
    if (helicopter != null) helicopter.setCommand(NoCommand);
    inputIndicator.turnOff(lastCommand);
    lastCommand = NoCommand; 
}

public Helicopter helicopter() {
    return helicopter;
}

public Background background() {
    return background;
}

public void updatePieces() {
    int cnt;
    for (cnt = 0; cnt < GamePiece.LastGamePiece; cnt++) {
        int objectCnt = pieces[cnt].size();
        while (objectCnt-- != 0) {
	    ((GamePiece)(pieces[cnt].elementAt(objectCnt))).updatePiece();
        }
    } 
}

public Vector piecesOfType(int type) {
    return pieces[type];
}

public void drawPieces() {
    NSRect drawRect = currentBackgroundPosition();
    int pieceCnt; 

    background.draw(drawRect);

    for (pieceCnt = 0; pieceCnt < GamePiece.LastGamePiece; pieceCnt++) {
	int cnt;
	Vector pieceList = pieces[pieceCnt];
	for (cnt = pieceList.size() - 1; cnt >= 0; cnt--) {
            ((GamePiece)(pieceList.elementAt(cnt))).draw(drawRect);
	}
    }
}

public void drawRect(NSRect rect) {
    drawPieces();
}

public void checkCollisions() {
    int cnt;

    // Check friendly pieces against background and enemy bases

    for (cnt = pieces[GamePiece.FriendlyPiece].size() - 1; cnt >= 0; cnt--) {
	boolean hit = false;
	int objCnt;
        GamePiece piece = (GamePiece)(pieces[GamePiece.FriendlyPiece].elementAt(cnt));

	// If two friendly objects hit the same enemy object, second one won't blow up.

        for (objCnt = pieces[GamePiece.StationaryEnemyPiece].size() - 1; objCnt >= 0; objCnt--) {
            GamePiece enemy = (GamePiece)(pieces[GamePiece.StationaryEnemyPiece].elementAt(objCnt));
	    if (piece.touches(enemy)) {
		enemy.explode();
                hit = true;
	    }
	}
        for (objCnt = pieces[GamePiece.MobileEnemyPiece].size() - 1; objCnt >= 0; objCnt--) {
            GamePiece enemy = (GamePiece)(pieces[GamePiece.MobileEnemyPiece].elementAt(objCnt));
	    if (piece.touches(enemy)) {
		enemy.explode();
		hit = true;
	    }
	}

        if (hit || piece.touches(background)) {
	    if (helicopter == piece) {
                if (!isDemo()) {
                    if (cheating == 0) {
                        helicopter.explode();
                        helicopter = null;	// ??? Hopefully this won't cause helicopter to be garbage collected...
                        lives--;
                        livesField.setIntValue(lives);
                    } else {
                        cheating++;
                    }
                }
	    } else {
                piece.explode();
	    }
	}
    }

    // Check mobile enemy against background

    for (cnt = pieces[GamePiece.MobileEnemyPiece].size() - 1; cnt >= 0; cnt--) {
        GamePiece piece = (GamePiece)(pieces[GamePiece.MobileEnemyPiece].elementAt(cnt));
        if (piece.touches(background)) {
            piece.explode();
	}
    }
}

public void removeGamePiece(GamePiece piece) {
    pieces[piece.pieceType()].removeElement(piece);
    if (piece == focusObject) focusObject = null;
}

public void addGamePiece(GamePiece piece) {
    pieces[piece.pieceType()].addElement(piece);
}

public void setFocusObject(GamePiece piece) {
    focusObject = piece;
}

public int updateTime() {
    return updateTime;
}

public int elapsedTime() {
    return elapsedTime;
}

public NSRect currentBackgroundPosition() {
    NSMutableRect rect = new NSMutableRect();
    rect.setSize(bounds().size());
    if (focusObject == null) {
        rect.setOrigin(lastOrigin);
    } else {
        NSRect focusObjectRect = focusObject.rect();
        rect.setX((float)Math.floor(focusObjectRect.midX() - rect.width() / 2f));
        rect.setY((float)Math.floor(focusObjectRect.midY() - rect.height() / 2f));
        rect.setX(maxFloat(0f, minFloat(rect.x(), background.gameRect().width() - rect.width())));
	rect.setY(maxFloat(0f, minFloat(rect.y(), background.gameRect().height() - rect.height())));
	lastOrigin = rect.origin();
    }
    return rect;
}

public void doAward() {
    NSAlertPanel.runAlert("BlastApp Completed", "Congratulations! You finished BlastApp with " + score + " points.", "Great!", null, null);
    if (background.levelDataName() == null) {
        if (NSAlertPanel.runAlert("BlastApp Completed", "Would you like a certificate documenting your victory?  (Even if you don't have a printer you can save the certificate as a PostScript file and print it later.)", "Yes", "No", null) == NSAlertPanel.DefaultReturn) {
	    AwardView.makeNewAward(background.numLevels(), score);
	}
    }
}

/*
  The main game loop
  This method is called at the desired frame rate (or lower...)
  elapsedTime allows us to compute travelled distances, etc correctly
*/
public void step(NSTimer obj) {
    int scoreBefore = score;

    numFrames++;
    elapsedTime = timeInMsSinceGameBegan() - updateTime;

    if (timing) {
        if (elapsedTime < 2000) {
            timingTime += elapsedTime;
            timingFrames += 1;
        }
    }

    if (elapsedTime > MAXUPDATETIME) {	// Upper bound on elaspedTime (otherwise objects start going through each other)
	if (timing) {
            timingMissed++;
	}
        stopGameFor(elapsedTime - MAXUPDATETIME);
        elapsedTime = MAXUPDATETIME;
    }

    updateTime += elapsedTime;

    updatePieces();

    checkCollisions();

    setNeedsDisplay(true);

    if (!gameOver) {	// Check to see if the game is over
        if (helicopter != null) {
            NSRect helicopterRect = helicopter.rect();
            if (!GamePiece.intersectsRects(background.gameRect(), helicopterRect)) {  // Out of bounds
		if (isDemo()) {
                    statusField.setStringValue("Level completed.");
		} else if (updateTime - levelStartedAt < MAXBONUSTIME) {
                    addScore(BONUS);
                    statusField.setStringValue("Level " + level + " completed with bonus points!");
                } else {
                    statusField.setStringValue("Level " + level + " completed.");
                }
		if (isDemo()) {
		    startLevel((level < background.numLevels()) ? level + 1 : 1);
		} else if (level == background.numLevels()) {
                    gameOver = true;
                    removeGamePiece(helicopter);
                    helicopter = null;
                    updateHighScore();
                    scoreField.setIntValue(score);
                    doAward();
                    statusField.setStringValue("Game completed with " + score + " points.");
                } else {
                    startLevel(level + 1);
                }
            }
        }

        updateHighScore();
        
        if (focusObject == null && !gameOver) {
            if (lives != 0) {
                statusField.setStringValue(lives + " helicopter" + (lives > 1 ? "s" : "") + " left. Ready to start.");
                restartLevel();
            } else if (!bonusGiven && score >= 1000) {
                lives = 1;
                bonusGiven = true;
                statusField.setStringValue("You get a Bonus Helicopter!");
                restartLevel();
            } else {
                statusField.setStringValue(newHighScore ? "Game over with a new high score." : "All helicopters destroyed. Game Over.");
                gameOver = true;
            }
        }
    }

    if (score != scoreBefore) {
        scoreField.setIntValue(score);
    }   

}

public void windowDidUpdate(NSNotification notification) {
    // We used to synchronize here, but it no longer seems necessary
    // So we should remove the notification altogether
}

// The stop method will pause a running game. The go method will start it up
// again. They can be assigned to buttons or other appkit objects through IB.

public void go(NSControl sender) {
    if (timer == null) {
    	if (lives != 0) {
            displayRunningMessage();
	} else {
            statusField.setStringValue("Game Over.");
	}
	Class[] classes = {this.getClass()};
        timer = new NSTimer(ANIMATIONINTERVAL, this, new NSSelector("step", classes), this, true);
        NSRunLoop.currentRunLoop().addTimerForMode(timer, NSRunLoop.DefaultRunLoopMode);

        pauseButton.setState(0);

        if (timing) {
            timingTime = 0;
            timingFrames = 0;
            timingMissed = 0;
        }
    }
    window().makeFirstResponder(this);
}

public void stop(NSControl sender) {
    if (timer != null) {
        timer.invalidate();
        timer = null;

        pausedAt = timeInMsSinceGameBegan();
        statusField.setStringValue("Paused.");
        pauseButton.setState(1);
    }
}

public boolean isPaused() {
    return timer == null;
}

public void togglePause(NSControl sender) {
    if (timer != null) {
        stop(sender);
    } else {
 	int timeAdj = timeInMsSinceGameBegan() - pausedAt;
        stopGameFor(maxInt(timeAdj, 0));
	go(sender);
    }
}

public void stopGameFor(int timeAdj) {
    timeStopped += timeAdj;
}

public void startLevel(int newLevel) {
    int cnt;

    level = minInt(newLevel, background.numLevels());
    levelField.setIntValue(level);
    updateHighScore();

    for (cnt = 0; cnt < GamePiece.LastGamePiece; cnt++) {
        pieces[cnt].removeAllElements();
    }
    helicopter = null;

    background.setLevel(level);

    restartLevel();
}

public void startGame(NSControl sender) {
    int startingLevel = ((levelMatrix == null) || (levelMatrix.selectedTag() == 0)) ? highLevel : levelSlider.intValue();
    startGameAtLevel(sender, startingLevel);
}

public void startGameAtLevel(Object sender, int startingLevel) {
    updateHighScore();

    lives = 3;
    score = 0;
    
    livesField.setIntValue(lives);
    scoreField.setIntValue(score);
        
    gameStartTime = new Date();

    timeStopped = 0;	// Amount of time game was stopped (including overrun frames)

    updateTime = 0;	// The game clock, updated at start of the step method

    startLevel(startingLevel);
    go(null);

    if (highScore == 0 && highLevel == 1) {
        statusField.setStringValue("Welcome! See About Box for help.");
    } else {
        statusField.setStringValue("Ready to start new game.");
    }
    gameOver = false;
    newHighScore = false;
    levelStartedAt = -1; 
}

/* 
  This method is called as a result of user action. If successful in changing the sound status, writes it out to the database. Thus if the user plays on a machine with no sound, their default setting won't be screwed up.
*/
public void toggleSound(NSControl sender) {
    boolean newState = !soundRequested;
    setSoundRequested(newState);
    if (newState != soundRequested) {
        if (newState) NSAlertPanel.runAlert(null, "Can't play sounds.", "Bummer", null, null);
    } else {
	NSUserDefaults.standardUserDefaults().setBooleanForKey(!soundRequested, "SoundDisabled");
    }
    updateSoundPref();
}

public void setSoundRequested(boolean flag) {
    soundRequested = flag && initSounds();
}
    
public void setStartingLevel(NSControl sender) {
    levelMatrix.selectCellAtLocation(1, 0);
    levelPrefsIndicator.setIntValue(sender.intValue()); 
}

public void updateStartingLevelPrefs() {
    if (levelMatrix != null && levelMatrix.window().isVisible()) {
        levelSlider.setMaxValue((double)highLevel);
	if (highLevel < 2) {
            levelSlider.setEnabled(false);
            ((NSCell)(levelMatrix.cellAtLocation(1, 0))).setEnabled(false);
	} else {
            levelSlider.setEnabled(true);
            ((NSCell)(levelMatrix.cellAtLocation(1, 0))).setEnabled(true);
	}
    }
}

public void updateSoundPref() {
    if (soundPrefItem != null && soundPrefItem.window().isVisible()) {
        soundPrefItem.setState(soundRequested ? 1 : 0);
    }
}

public void showPrefs(NSControl sender) {
    if (levelMatrix == null) {
	if (!NSApplication.loadNibNamed("Prefs", this) || levelMatrix == null) {
	    NSSystem.log("Prefs.nib could not be loaded successfully.");
	}
        ((NSPanel)(levelMatrix.window())).setBecomesKeyOnlyIfNeeded(true);
    }
    levelMatrix.window().orderFront(null);
    updateStartingLevelPrefs();
    updateSoundPref();
}

private static int hash(String str, int modBy) {
    int hash = 0;
    if (str != null) {
        int cnt;
        int len = str.length();
        for (cnt = 0; cnt < len; cnt++) hash = (hash + str.charAt(cnt)) % modBy;
    }
    return hash;
}

private static final char VERSION = 'B';	// Version 'A' high scores have user names in them...

public void writeHighScore() {
    String fileName = background.levelDataName();
    String defaultValue = VERSION + "" + (((highScore * 99) + highLevel) * 99 + hash("nousername", 49) + hash(fileName, 50));
    String defaultName = (fileName != null) ? ("HighScoreFor" + fileName) : "HighScore";
    NSUserDefaults.standardUserDefaults().setObjectForKey(defaultValue, defaultName);
}

public void readHighScore() {
    String fileName = background.levelDataName();
    String defaultName = (fileName != null) ? ("HighScoreFor" + fileName) : "HighScore";
    String defaultValue = NSUserDefaults.standardUserDefaults().stringForKey(defaultName);
    highLevel = 1;
    highScore = 0;
    if (defaultValue != null && defaultValue.length() > 1 && defaultValue.charAt(0) == VERSION) {
        try {
            int value = Integer.parseInt(defaultValue.substring(1));
            if (value % 99 == hash("nousername", 49) + hash(fileName, 50)) {
                value = value / 99;
                highLevel = Game.maxInt(1, Game.minInt(value % 99, Background.MAXLEVELS));
                highScore = Game.maxInt(0, value / 99);
            }
        } catch (Exception e) {
        }
    }
}

public void updateHighScore() {
    newHighScore = newHighScore || (score > highScore);
    if (score > highScore || level > highLevel) {
	highScore = maxInt(score, highScore);
	highLevel = maxInt(level, highLevel);
        highscoreField.setIntValue(highScore);
        updateStartingLevelPrefs();
        writeHighScore();
    }
}

public void restartLevel() {
    NSPoint startLocation = new NSPoint(HELICOPTERSTARTX, HELICOPTERSTARTY);
    NSSize zero = new NSSize(0f, 0f);

    if (isDemo()) {
	helicopter = new AutoPilotHelicopter();
    } else {
	helicopter = new Helicopter();
    }
    helicopter.initInGame(this);
    addGamePiece(helicopter);    
    setFocusObject(helicopter);
    helicopter.setLocation(startLocation);    
    helicopter.setVelocity(zero);    
    helicopter.setAcceleration(zero);

    System.gc();	// Start new levels with a clean slate 

    started = false;
    lastCommand = NoCommand;
}

public void addScore(int points) {
    score += points * level;
}

public void displayRunningMessage () {
    statusField.setStringValue(((lives == 3) ? "Welcome to " : "Running in ") + background.levelDescription());
}

// Delegate methods from window...

public void windowWillClose(NSNotification notification) {
    stop(null);
}

public void windowDidMiniaturize(NSNotification notification) {
    stop(null);
}

}

