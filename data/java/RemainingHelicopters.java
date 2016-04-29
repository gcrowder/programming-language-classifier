/*
        RemainingHelicopters.java
        Copyright (c) 1990-2004, Apple Computer, Inc., all rights reserved.
        Author: Ali Ozer

        Subclass of NSView for displaying the number of helicopters left
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

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;

public class RemainingHelicopters extends NSView {

protected int remainingHelicopters;

public RemainingHelicopters(NSRect rect) {
    super(rect);
}
    
public void drawRect(NSRect r) {
    NSRect bounds = bounds();
    int helicoptersToDisplay = remainingHelicopters;
    if (helicoptersToDisplay < 3) helicoptersToDisplay = 3;

    float inc;
    int cnt;
    NSMutablePoint point = new NSMutablePoint();
    NSImage helicopters = (NSImage)NSImage.imageNamed("helicopter");
    NSRect rect = new NSRect(0f, 0f, helicopters.size().width(), helicopters.size().height() / 4);
    point.setX((float)Math.floor((bounds.width() - rect.width()) / 2f));
    if (helicoptersToDisplay * rect.height() >= bounds.height()) {
        inc = (bounds.height() - helicoptersToDisplay * rect.height()) / (helicoptersToDisplay - 1);
    } else {
        inc = (bounds.height() - helicoptersToDisplay * rect.height()) / (helicoptersToDisplay + 1);
    }
    point.setY((float)Math.floor(bounds.maxY() - rect.height() - ((inc < 0) ? 0 : inc)));
    for (cnt = helicoptersToDisplay; cnt > 0; cnt--) {
	helicopters.dissolveToPointFromRect(point, rect, (cnt > remainingHelicopters) ? 0.2f : 1.0f);
        point.setY(point.y() - (rect.height() + inc));
    }
}

public void setIntValue(int val) {
    remainingHelicopters = val - 1;
    setNeedsDisplay(true);
}

public int intValue() {
    return remainingHelicopters + 1;
}

}
