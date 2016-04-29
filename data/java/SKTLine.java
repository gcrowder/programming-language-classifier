// SKTLine.java
// Sketch Example
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;



public class SKTLine extends SKTGraphic {

    private boolean _startsAtLowerLeft;

    public Object clone() {
        SKTLine newObj = (SKTLine)super.clone();

        if (newObj != null) {
            newObj.setStartsAtLowerLeft(this.startsAtLowerLeft());
        }
        return newObj;
    }

    public void setStartsAtLowerLeft(boolean flag) {
        if (_startsAtLowerLeft != flag) {
            if (undoManager() != null) {
                Class arrClass[] = {Boolean.class};
                Object args[] = {new Boolean(_startsAtLowerLeft)};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setStartsAtLowerLeft", arrClass), args);
            }
            _startsAtLowerLeft = flag;
            didChange();
        }
    }

    public boolean startsAtLowerLeft() {
        return _startsAtLowerLeft;
    }

    public void flipHorizontally() {
        setStartsAtLowerLeft(!this.startsAtLowerLeft());
        return;
    }

    public void flipVertically() {
        setStartsAtLowerLeft(!startsAtLowerLeft());
        return;
    }

    public boolean drawsFill() {
        // Lines never draw fill
        return false;
    }

    public boolean canDrawFill() {
        // Lines never draw fill
        return false;
    }

    public boolean hasNaturalSize() {
        // Lines have no "natural" size
        return false;
    }

    public NSBezierPath bezierPath(){
        NSBezierPath path = NSBezierPath.bezierPath();
        NSRect bounds = bounds();

        if (this.startsAtLowerLeft()) {
            path.moveToPoint(new NSPoint(bounds.x(), bounds.maxY()));
            path.lineToPoint(new NSPoint(bounds.maxX(), bounds.y()));
        } else {
            path.moveToPoint(new NSPoint(bounds.x(), bounds.y()));
            path.lineToPoint(new NSPoint(bounds.maxX(), bounds.maxY()));
        }

        path.setLineWidth(strokeLineWidth());

        return path;
    }


    public int knobMask()    {
        if (startsAtLowerLeft()) {
            return (LowerLeftKnobMask | UpperRightKnobMask);
        } else {
            return (UpperLeftKnobMask | LowerRightKnobMask);
        }
    }

    public boolean hitTest(NSPoint point, boolean isSelected) {
        if (isSelected && (this.knobUnderPoint(point) != NoKnob)) {
            return true;
        } else {
            NSRect bounds = this.bounds();
            float halfWidth = this.strokeLineWidth() / 2.0f;
            halfWidth += 2.0f;  // Fudge
            if (bounds.width() == 0.0f) {
                if (Math.abs(point.x() - bounds.x()) <= halfWidth) {
                    return true;
                }
            } else {
                boolean startsAtLowerLeft = this.startsAtLowerLeft();
                float slope = bounds.height() / bounds.width();

                if (startsAtLowerLeft) {
                    slope = -slope;
                }

                if (Math.abs(((point.x() - bounds.x()) * slope) - (point.y() - (startsAtLowerLeft ? bounds.maxY() : bounds.y()))) <= halfWidth) {
                    return true;
                }
            }
            return false;
        }
    }

    private static final String LineStartsAtLowerLeftKey = "LineStartsAtLowerLeft";

    public NSMutableDictionary propertyListRepresentation() {
        NSMutableDictionary dict = super.propertyListRepresentation();
        dict.setObjectForKey( (startsAtLowerLeft() ? "YES": "NO"), LineStartsAtLowerLeftKey);
        return dict;
    }

    public void loadPropertyListRepresentation(NSDictionary dict) {
        Object obj;

        super.loadPropertyListRepresentation(dict);

        obj = dict.objectForKey(LineStartsAtLowerLeftKey);
        if (obj != null) {
            setStartsAtLowerLeft(((String) obj).equals("YES"));
        }
    }

}

/*
 IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc. ("Apple") in
 consideration of your agreement to the following terms, and your use, installation,
 modification or redistribution of this Apple software constitutes acceptance of these
 terms.  If you do not agree with these terms, please do not use, install, modify or
 redistribute this Apple software.

 In consideration of your agreement to abide by the following terms, and subject to these
 terms, Apple grants you a personal, non-exclusive license, under Apple's copyrights in
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
