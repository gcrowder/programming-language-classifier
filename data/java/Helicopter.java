/*
        Helicopter.java
        Copyright (c) 1990-2004, Apple Computer, Inc., all rights reserved.
        Author: Ali Ozer

        One of the interesting subclasses of GamePiece, implementing the helicopter
*/
/*
 IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc. ("Apple") in
 consideration of your agreement to the following terms, and your use, installation, 
 modification or redistribution of this Apple software constitutes acceptance of these 
 terms.  If you do not agree with these terms, please do not use, install, modify or 
 redistribute this Apple software.
 
 In consideration of your agreement to abide by the following terms, and subject to these 
 terms, Apple grants you a personal, non-exclusive license, under AppleÕs copyrights in 
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

import com.apple.cocoa.foundation.*;

public class Helicopter extends GamePiece {

private int command;
private boolean fireRequested;
private int nextFireTime;	/* ms */
    
private static final float ZEROVELTHRESHOLD = 2f;		/* speed below which we zero the velocity; pixels/second */

public int pieceType() {
    return FriendlyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "helicopter", 4);
}

public void startFiring() {
    fireRequested = true;
}

public void stopFiring() {
    if (fireRequested) {
	nextFireTime = Game.minInt(nextFireTime, game.updateTime() + TIMETORECHARGE / 4);
	fireRequested = false;
    }
}

public boolean fireRequested() {
    return fireRequested;
}

public void setVelocity(NSSize newVelocity) {
    NSMutableSize newVel = new NSMutableSize(Game.maxFloat(0f, Game.minFloat(Game.MAXVELX, newVelocity.width())), Game.restrictValue(newVelocity.height(), Game.MAXVELY));
    if ((newVel.height() > 0f && vel.height() > 0f && newVel.height() < vel.height() && newVel.height() < ZEROVELTHRESHOLD) ||
	(newVel.height() < 0f && vel.height() < 0f && newVel.height() > vel.height() && newVel.height() > -ZEROVELTHRESHOLD)) {
	newVel.setHeight(0f);
    }
    if (newVel.width() < vel.width() && newVel.width() < ZEROVELTHRESHOLD) {
    	newVel.setWidth(0f);
    }
    super.setVelocity(newVel);
}

public void setCommand(int cmd) {
    command = cmd;
}

public NSSize acceleration() {
    float dw = 0f, dh = 0f;
    switch (command) {
        case Game.StopCommand:		dw = -7 * vel.width(); dh = -7 * vel.height(); break;
        case Game.GoDownLeftCommand:	dw = dh = -HELICOPTERACCVALUE; break;
        case Game.GoDownCommand:	dh = -HELICOPTERACCVALUE; break;
        case Game.GoDownRightCommand:	dw = HELICOPTERACCVALUE; dh = -HELICOPTERACCVALUE; break;
        case Game.GoLeftCommand:	dw = -HELICOPTERACCVALUE; break;
        case Game.GoRightCommand:	dw = HELICOPTERACCVALUE; break;
        case Game.GoUpLeftCommand:	dw = -HELICOPTERACCVALUE; dh = HELICOPTERACCVALUE; break;
        case Game.GoUpCommand:		dh = HELICOPTERACCVALUE; break;
        case Game.GoUpRightCommand:	dw = HELICOPTERACCVALUE; dh = HELICOPTERACCVALUE; break;
        default:			break;
    }
    return new NSSize(acc.width() + dw, acc.height() + dh);
}

public void updatePiece() {
    if (fireRequested() && game.updateTime() > nextFireTime) {
        GamePiece bullet = new Bullet();
        bullet.initInGame(game);
        game.playFriendlyFireSound();
        bullet.setVelocity(new NSSize(vel.width() + BULLETVEL, vel.height()));
        bullet.setLocation(new NSPoint(pos.maxX() - 8, pos.y()));
	game.addGamePiece(bullet);
	nextFireTime = game.updateTime() + TIMETORECHARGE;
    }
    super.updatePiece();
}

public void playExplosionSound() {
    game.playLoudExplosionSound();
}

public void explode() {
    GamePiece explosion = new HelicopterExplosion();
    explosion.initInGame(game);
    game.setFocusObject(explosion);
    explode(explosion);
}

// Touch rects for the helicopter pointing right.

public static final int NUMHELICOPTERRECTS = 5;
private static NSRect helicopterRectArray[];

public NSRect[] helicopterRects() {
    if (helicopterRectArray == null) {
        helicopterRectArray = new NSRect[NUMHELICOPTERRECTS];
        helicopterRectArray[0] = new NSRect(27, 0, 18, 11);	// Bottom
        helicopterRectArray[1] = new NSRect(0, 8, 4, 11);		// Tail
        helicopterRectArray[2] = new NSRect(0, 16, 50, 1);
        helicopterRectArray[3] = new NSRect(22, 5, 18, 12);	// Body
        helicopterRectArray[4] = new NSRect(0, 10, 42, 3);
    }
    return helicopterRectArray;
};

public boolean touches(GamePiece obj) {
    if (!super.touches(obj)) return false;	// Easy case 
    int cnt;
    for (cnt = 0; cnt < NUMHELICOPTERRECTS; cnt++) {
        NSRect rect = helicopterRects()[cnt];	// ??? Ugh - new Rect
        if (obj.touchesRect(new NSRect(rect.x() + pos.x(), rect.y() + pos.y(), rect.width(), rect.height()))) return true;
    }
    return false;
}

public boolean touchesRect(NSRect r) {
    if (!super.touchesRect(r)) return false;	// Easy case
    int cnt;
    for (cnt = 0; cnt < NUMHELICOPTERRECTS; cnt++) {
        NSRect rect = helicopterRects()[cnt];	// ??? Ugh - new Rect 
        if (intersectsRects(r, new NSRect(rect.x() + pos.x(), rect.y() + pos.y(), rect.width(), rect.height()))) return true;
    }
    return false;
}

}
