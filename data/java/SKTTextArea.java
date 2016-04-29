// SKTTextArea.java
// Sketch Example
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;


public class SKTTextArea extends SKTGraphic {

    private static NSLayoutManager sharedLM = null;
    private static NSTextView sharedEditor = null;
    private static boolean sharedEditorInUse = false;
    
    private static final String TextAreaContentsKey = "Text";
    private static final float EditingMargin = 36.0f;

    private NSTextStorage _contents;
    private NSSize _minSize;

    public SKTTextArea () {
        Class arrClass[] = {NSNotification.class};
        _contents = new NSTextStorage();
        NSNotificationCenter.defaultCenter().addObserver(this, new NSSelector("SKT_contentsChanged", arrClass), NSTextStorage.TextStorageDidProcessEditingNotification, _contents);
    }

    protected void finalize() {
        NSNotificationCenter.defaultCenter().removeObserver(this);
    }

    public Object clone() {
        SKTTextArea newObj = (SKTTextArea)super.clone();

        if (newObj != null) {
            newObj.setContents(this.contents());
        }
        return newObj;
    }

    public void setContents(Object contents) {
        if (contents != _contents) {
            if (undoManager() != null) {
                Class arrClass[] = {NSAttributedString.class};
                Object args[] = {new NSAttributedString(_contents)};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setContents", arrClass), args);
            }
            if (contents instanceof NSAttributedString) {
                _contents.replaceCharactersInRange(new NSRange(0,_contents.length()), (NSAttributedString)contents);
            } else {
                _contents.replaceCharactersInRange(new NSRange(0,_contents.length()), (String)contents);
            }
            didChange();
        }
    }

    public Object coerceValueForContents(Object value) {
        // We want to just get Strings unchanged.  We will detect this and do the right thing in setContents().  We do this because, this way, we will do more reasonable things about attributes when we are receiving plain text.
        if (value instanceof String) {
            return value;
        } else {
            return ((NSScriptCoercionHandler)NSScriptCoercionHandler.sharedCoercionHandler()).coerceValueToClass(value, NSTextStorage.class);
        }
    }

    public NSTextStorage contents() {
        return _contents;
    }

    public void SKT_contentsChanged(NSNotification notification) {
        // MF:!!! We won't be able to undo piecemeal changes to the text currently.
        this.didChange();
    }

    public boolean drawsStroke() {
        // Never draw stroke.
        return false;
    }

    public boolean canDrawStroke() {
        // Never draw stroke.
        return false;
    }

    public static NSLayoutManager sharedDrawingLayoutManager() {
        if (sharedLM == null) {
            NSTextContainer tc = new NSTextContainer(new NSSize(1.0e6f, 1.0e6f));

            sharedLM = new NSLayoutManager();

            tc.setWidthTracksTextView(false);
            tc.setHeightTracksTextView(false);
            sharedLM.addTextContainer(tc);
        }
        return sharedLM;
    }

    public void drawInViewIsSelected(SKTGraphicView view, boolean flag) {
        NSRect bounds = bounds();
        if (drawsFill()) {
            fillColor().set();
            NSBezierPath.fillRect((NSRect) bounds);
        }
        if ((view != null) && ((view.editingGraphic() == this) || (view.creatingGraphic() == this))) {
            NSColor.knobColor().set();
            NSGraphics.frameRect(bounds.rectByInsettingRect(-1.0f, -1.0f));
            // If we are creating we have no text.  If we are editing, the editor (ie NSTextView) will draw the text.
        } else {
            NSTextStorage contents = contents();
            if (contents.length() > 0) {
                NSLayoutManager lm = sharedDrawingLayoutManager();
                NSTextContainer tc = (NSTextContainer)lm.textContainers().objectAtIndex(0);
                NSRange glyphRange;

                tc.setContainerSize(bounds.size());
                contents.addLayoutManager(lm);
                // Force layout of the text and find out how much of it fits in the container.
                glyphRange = lm.glyphRangeForTextContainer(tc);

                if (glyphRange.length() > 0) {
                    lm.drawBackgroundForGlyphRange(glyphRange, bounds.origin());
                    lm.drawGlyphsForGlyphRange(glyphRange, bounds.origin());
                }
                contents.removeLayoutManager(lm);
            }
        }
        super.drawInViewIsSelected(view, flag);
    }

    public NSSize minSize() {
        return (new NSSize(10.0f, 15.0f));
    }

    public NSSize maxSize() {
        NSRect bounds = this.bounds();
        NSSize paperSize = this.document().documentSize();
        NSSize maxSize = new NSSize(paperSize.width() - bounds.x() - EditingMargin, paperSize.height() - bounds.y() - EditingMargin);
        return maxSize;
    }

    public NSSize requiredSize(float maxWidth) {
        NSTextStorage contents = contents();
        NSSize minSize = minSize();
        NSSize maxSize = this.maxSize();
        int len = contents.length();

        if (len > 0) {
            NSLayoutManager lm = sharedDrawingLayoutManager();
            NSTextContainer tc = (NSTextContainer) lm.textContainers().objectAtIndex(0);
            NSRange glyphRange;
            NSMutableSize requiredSize;

            tc.setContainerSize(new NSSize(((maxSize.width() < maxWidth) ? maxSize.width() : maxWidth), maxSize.height()));
            contents.addLayoutManager(lm);
            // Force layout of the text and find out how much of it fits in the container.
            glyphRange = lm.glyphRangeForTextContainer(tc);

            NSSize usedSize = lm.usedRectForTextContainer(tc).size();
            requiredSize = new NSMutableSize(usedSize.width() + 1.0f, usedSize.height());
            
            if (requiredSize.width() < minSize.width()) {
                requiredSize.setWidth ( minSize.width());
            }
            if (requiredSize.height() < minSize.height()) {
                requiredSize.setHeight (minSize.height());
            }

            contents.removeLayoutManager(lm);

            return requiredSize;
        } else {
            return minSize;
        }
    }

    public void makeNaturalSize() {
        NSMutableRect bounds = new NSMutableRect(bounds());
        NSSize requiredSize = requiredSize(1.0e6f);

        bounds.setSize(requiredSize);
        setBounds(bounds);
    }

    public void setBounds(NSRect rect) {
        // We need to make sure there's enough room for the text.
        NSSize minSize = this.minSize();
        NSMutableRect tweakedRect = new NSMutableRect(rect);
        if (minSize.width() > rect.size().width()) {
            tweakedRect.setWidth( minSize.width());
        }
        if (minSize.height() > rect.size().height()) {
            tweakedRect.setHeight (minSize.height());
        }
        super.setBounds(tweakedRect);
    }

    public int resizeByMovingKnobToPoint(int knob, NSPoint point) {
        NSSize minSize = minSize();
        NSRect bounds = bounds();

        NSMutablePoint mutPoint = new NSMutablePoint(point);

        // This constrains the size to be big enough for the text.  It is different from the constraining in -setBounds since it takes into account which corner or edge is moving to figure out which way to grow the bounds if necessary.
        if ((knob == UpperLeftKnob) || (knob == MiddleLeftKnob) || (knob == LowerLeftKnob)) {
            // Adjust left edge
            if ((bounds.maxX() - mutPoint.x()) < minSize.width()) {
                mutPoint.setX (mutPoint.x() - minSize.width() - (bounds.maxX() - mutPoint.x()));
            }
        } else if ((knob == UpperRightKnob) || (knob == MiddleRightKnob) || (knob == LowerRightKnob)) {
            // Adjust right edge
            if ((mutPoint.x() - bounds.origin().x()) < minSize.width()) {
                mutPoint.setX ( mutPoint.x() + minSize.width() - (mutPoint.x() - bounds.origin().x()));
            }
        }
        if ((knob == UpperLeftKnob) || (knob == UpperMiddleKnob) || (knob == UpperRightKnob)) {
            // Adjust top edge
            if ((bounds.maxY() - mutPoint.y()) < minSize.height()) {
                mutPoint.setY ( mutPoint.y() - minSize.height() - (bounds.maxY() - mutPoint.y()));
            }
        } else if ((knob == LowerLeftKnob) || (knob == LowerMiddleKnob) || (knob == LowerRightKnob)) {
            // Adjust bottom edge
            if ((mutPoint.y() - bounds.origin().y()) < minSize.height()) {
                mutPoint.setY ( mutPoint.y() + minSize.height() - (mutPoint.y() - bounds.origin().y()));
            }
        }

        return super.resizeByMovingKnobToPoint(knob, mutPoint);
    }

    public boolean isEditable() {
        return true;
    }

    static NSTextView newEditor() {
        // This method returns an NSTextView whose NSLayoutManager has a refcount of 1.  It is the caller's responsibility to release the NSLayoutManager.  This function is only for the use of the following method.

        NSLayoutManager lm = new NSLayoutManager();
        NSTextContainer tc = new NSTextContainer(new NSSize(1.0e6f,1.0e6f));
        NSTextView tv = new NSTextView(new NSRect(0.0f, 0.0f, 100.0f, 100.0f));
        tv.usesFontPanel();
        lm.addTextContainer(tc);

        tv.setTextContainerInset(new NSSize(0.0f,0.0f));
        tv.setDrawsBackground(false);
        tv.setAllowsUndo(true);
        tc.setTextView(tv);

        return tv;
    }

    public void startEditingWithEventInView(NSEvent event, SKTGraphicView view) {
        NSTextView editor;
        NSTextStorage contents = this.contents();
        NSSize maxSize = this.maxSize();
        NSSize minSize = this.minSize();
        NSRect bounds = this.bounds();

        if (!sharedEditorInUse) {
            if (sharedEditor == null) {
                sharedEditor = newEditor();
            }
            sharedEditorInUse = true;
            editor = sharedEditor;
        } else {
            editor = newEditor();
        }
        editor.textContainer().setWidthTracksTextView(false);
        if (bounds.width() > minSize.width() + 1.0) {
            // If we are bigger than the minimum width we assume that someone already edited this SKTTextArea or that they created it by dragging out a rect.  In either case, we figure the width should remain fixed.
            editor.textContainer().setContainerSize(new NSSize(bounds.width(), maxSize.height()));
            editor.setHorizontallyResizable(false);
        } else {
            editor.textContainer().setContainerSize(maxSize);
            editor.setHorizontallyResizable(true);
        }
        editor.setMinSize(minSize);
        editor.setMaxSize(maxSize);
        editor.textContainer().setHeightTracksTextView(false);
        editor.setVerticallyResizable(true);
        editor.setFrame(bounds);

        contents.addLayoutManager(editor.layoutManager());
        view.addSubview(editor);
        view.setEditingGraphicEditorView(this, editor);
        editor.setSelectedRange(new NSRange(0,contents.length()));
        editor.setDelegate(this);

        // Make sure we redisplay
        this.didChange();

        view.window().makeFirstResponder(editor);
        if (event != null) {
            editor.mouseDown(event);
        }
    }

    public void endEditingInView(SKTGraphicView view) {
        if (view.editingGraphic() == this) {
            NSTextView editor = (NSTextView )view.editorView();
            editor.setDelegate(null);
            editor.removeFromSuperview();
            this.contents().removeLayoutManager(editor.layoutManager());
            if (editor == sharedEditor) {
                sharedEditorInUse = false;
            } else {
                editor.layoutManager();
            }
            view.setEditingGraphicEditorView(null,null);
        }
    }

    public void textDidChange(NSNotification notification) {
        NSSize textSize;
        NSRect myBounds = this.bounds();
        boolean fixedWidth = (((NSTextView)(notification.object())).isHorizontallyResizable() ? false : true);

        textSize = this.requiredSize((fixedWidth ? myBounds.width() : 1.0e6f));
        if ((textSize.width() > myBounds.width()) || (textSize.height() > myBounds.height())) {
            this.setBounds(new NSRect(myBounds.x(), myBounds.y(), ((!fixedWidth && (textSize.width() > myBounds.width())) ? textSize.width() : myBounds.width()), ((textSize.height() > myBounds.height()) ? textSize.height() : myBounds.height())));
            // MF: For multiple editors we must fix up the others...  but we don't support multiple views of a document yet, and that's the only way we'd ever have the potential for multiple editors.
        }
    }

    public NSMutableDictionary propertyListRepresentation() {
        NSMutableDictionary dict = super.propertyListRepresentation();
        dict.setObjectForKey(NSArchiver.archivedDataWithRootObject(this.contents()),TextAreaContentsKey);
        return dict;
    }

    public void loadPropertyListRepresentation(NSDictionary dict) {
        Object obj;

        super.loadPropertyListRepresentation(dict);

        obj = dict.objectForKey(TextAreaContentsKey);
        if (obj != null) {
            this.setContents((NSAttributedString) NSUnarchiver.unarchiveObjectWithData((NSData) obj));
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
