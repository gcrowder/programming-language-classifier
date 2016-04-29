/*
        AutoPilotHelicopter.java
        Copyright (c) 1997-2004, Apple Computer, Inc., all rights reserved.
        Author: Ali Ozer

        One of the interesting subclasses of GamePiece, implementing the demo helicopter
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

class AutoPilotHelicopter extends Helicopter {

protected static final int AUTOPILOTSTARTTIME = 2000;		/* ms */
protected static final int AUTOPILOTTAKEOVERTIME = 4000;	/* ms */
protected int autopilotTakeoverTime;	/* ms */
protected boolean autopilot;

protected static final int AUTOPILOTHELICOPTERSPEED = (Game.MAXVELX * 3) / 4;

public void initInGame(Game g) {
    super.initInGame(g);
    autopilotTakeoverTime = game.updateTime() + AUTOPILOTSTARTTIME;
}

public void setCommand(int cmd) {
    autopilot = false;
    autopilotTakeoverTime = game.updateTime() + AUTOPILOTTAKEOVERTIME;
    super.setCommand(cmd);
}

public void startFiring() {
    autopilot = false;
    autopilotTakeoverTime = game.updateTime() + AUTOPILOTTAKEOVERTIME * 10;
    super.startFiring();
}

public void stopFiring() {
    autopilot = false;
    autopilotTakeoverTime = game.updateTime() + AUTOPILOTTAKEOVERTIME;
    super.stopFiring();
}

public boolean fireRequested() {
    if (autopilot) {
	return Game.oneIn(50);
    } else {
	return super.fireRequested();
    }
}

public void updatePiece() {
    if (!autopilot && game.updateTime() >= autopilotTakeoverTime) {
	autopilot = true;
        game.markGameAsRunning();
        setVelocity(new NSSize(AUTOPILOTHELICOPTERSPEED, 0f));
	setAcceleration(new NSSize(0f, 0f));
    }
    super.updatePiece();
    // Prevent a crash...
    NSRect rect = game.background().clearRect(pos.x(), pos.maxX() + 4.0f);
    if (pos.maxY() >= rect.maxY()) {	// Oops, need to adjust
        setLocation(new NSPoint(pos.x(), rect.maxY() - pos.height() - 1f));
    } else if (pos.y() <= rect.y()) {	// Oops, need to adjust
        setLocation(new NSPoint(pos.x(), rect.y() + 1f));
    } else {
        rect = game.background().clearRect(pos.x(), pos.maxX() + 20.0f);
        if (pos.maxY() >= rect.maxY() - 2f) {	// Oops, need to adjust
            setLocation(new NSPoint(pos.x(), pos.y() - 1f));
        } else if (pos.y() < rect.y() + 2f) {	// Oops, need to adjust
            setLocation(new NSPoint(pos.x(), pos.y() + 1f));
        } else if (!autopilot && Game.oneIn(100)) {
            setLocation(new NSPoint(pos.x(), pos.y() + Game.randInt(2) - 1f));	// random drift
        }
    }
}

}
