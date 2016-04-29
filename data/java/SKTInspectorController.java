// SKTInspectorController.java
// Sketch Example
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;

public class SKTInspectorController extends NSWindowController {

    private static SKTInspectorController _sharedInspectorController = null;

    private NSButton fillCheckbox;
    private NSColorWell fillColorWell;
    private NSButton lineCheckbox;
    private NSColorWell lineColorWell;
    private NSSlider lineWidthSlider;
    private NSTextField lineWidthTextField;
    private NSTextField xTextField;
    private NSTextField yTextField;
    private NSTextField widthTextField;
    private NSTextField heightTextField;

    private SKTGraphicView _inspectingGraphicView;
    private boolean needsUpdate;

    public static SKTInspectorController sharedInspectorController() {

        if (_sharedInspectorController == null) {
            _sharedInspectorController = new SKTInspectorController();
        }
        return _sharedInspectorController;
    }

    public SKTInspectorController() {
        super("Inspector");
        this.setWindowFrameAutosaveName("Inspector");
        needsUpdate = false;
    }

    protected void finalize() {
        NSNotificationCenter.defaultCenter().removeObserver(this);
    }

    public void setMainWindow(NSWindow mainWindow) {
        NSWindowController controller = ((mainWindow != null) ? (NSWindowController)mainWindow.windowController() : null);

        // Technically this should be an isKindOfClass: type test, but it doesn't matter in this case and that test is a bit more tricky in Java
        if ((controller != null)  && (controller.getClass() == SKTDrawWindowController.class)) {
            _inspectingGraphicView = ((SKTDrawWindowController)controller).graphicView();
        } else {
            _inspectingGraphicView = null;
        }
        needsUpdate = true;
    }

    public void windowDidLoad() {
        super.windowDidLoad();
        setMainWindow(NSApplication.sharedApplication().mainWindow());
        Class arrClass [] = {NSNotification.class};
        NSNotificationCenter.defaultCenter().addObserver(this, new NSSelector ("mainWindowChanged", arrClass), NSWindow.WindowDidBecomeMainNotification, null);
        NSNotificationCenter.defaultCenter().addObserver(this, new NSSelector ("mainWindowResigned", arrClass), NSWindow.WindowDidResignMainNotification, null);
        NSNotificationCenter.defaultCenter().addObserver(this, new NSSelector ("graphicChanged", arrClass), SKTGraphic.GraphicDidChangeNotification, null);
        NSNotificationCenter.defaultCenter().addObserver(this, new NSSelector ("selectionChanged", arrClass), SKTGraphicView.GraphicViewSelectionDidChangeNotification, null);
    }

    public void mainWindowChanged(NSNotification notification) {
        setMainWindow((NSWindow)notification.object());
    }

    public void mainWindowResigned(NSNotification notification) {
        setMainWindow(null);
    }

    public void graphicChanged(NSNotification notification) {
        if (_inspectingGraphicView != null) {
            if (_inspectingGraphicView.selectedGraphics().containsObject(notification.object())) {
                needsUpdate = true;
            }
        }
    }

    public void selectionChanged(NSNotification notification) {
        if (notification.object() == _inspectingGraphicView) {
            needsUpdate = true;
        }
    }

    public void windowDidUpdate(NSNotification notification) {
        if (needsUpdate) {
            NSArray selectedGraphics = ((_inspectingGraphicView != null) ? _inspectingGraphicView.selectedGraphics() : null);
            int c = ((selectedGraphics != null)? selectedGraphics.count() : 0);
            SKTGraphic graphic;

            needsUpdate = false;

            if (c == 1) {
                NSRect bounds;
                boolean tempFlag;

                graphic = (SKTGraphic) selectedGraphics.objectAtIndex(0);
                bounds = graphic.bounds();
                tempFlag = graphic.drawsFill();
                fillCheckbox.setState((tempFlag? NSCell.OnState : NSCell.OffState));
                fillCheckbox.setEnabled(graphic.canDrawFill());
                fillColorWell.setColor(((graphic.fillColor() != null) ? graphic.fillColor() : NSColor.clearColor()));
                fillColorWell.setEnabled(tempFlag);
                tempFlag = graphic.drawsStroke();
                lineCheckbox.setState((tempFlag? NSCell.OnState : NSCell.OffState));
                lineCheckbox.setEnabled(graphic.canDrawStroke());
                lineColorWell.setColor(((graphic.strokeColor() != null) ? graphic.strokeColor() : NSColor.clearColor()));
                lineColorWell.setEnabled(tempFlag);
                lineWidthSlider.setFloatValue(graphic.strokeLineWidth());
                lineWidthSlider.setEnabled(tempFlag);
                lineWidthTextField.setFloatValue(graphic.strokeLineWidth());
                lineWidthTextField.setEnabled(tempFlag);
                xTextField.setFloatValue(bounds.x());
                xTextField.setEnabled(true);
                yTextField.setFloatValue(bounds.y());
                yTextField.setEnabled(true);
                widthTextField.setFloatValue(bounds.width());
                widthTextField.setEnabled(true);
                heightTextField.setFloatValue(bounds.height());
                heightTextField.setEnabled(true);
            } else if (c > 1) {
                // MF: Multiple selection should be editable
                fillCheckbox.setState(NSCell.MixedState);
                fillCheckbox.setEnabled(false);
                fillColorWell.setColor(NSColor.whiteColor());
                fillColorWell.setEnabled(false);
                lineCheckbox.setState(NSCell.MixedState);
                lineCheckbox.setEnabled(false);
                lineColorWell.setColor(NSColor.whiteColor());
                lineColorWell.setEnabled(false);
                lineWidthSlider.setFloatValue(0.0f);
                lineWidthSlider.setEnabled(false);
                lineWidthTextField.setStringValue("--");
                lineWidthTextField.setEnabled(false);
                xTextField.setStringValue("--");
                xTextField.setEnabled(false);
                yTextField.setStringValue("--");
                yTextField.setEnabled(false);
                widthTextField.setStringValue("--");
                widthTextField.setEnabled(false);
                heightTextField.setStringValue("--");
                heightTextField.setEnabled(false);
            } else {
                fillCheckbox.setState(NSCell.OffState);
                fillCheckbox.setEnabled(false);
                fillColorWell.setColor(NSColor.whiteColor());
                fillColorWell.setEnabled(false);
                lineCheckbox.setState(NSCell.OffState);
                lineCheckbox.setEnabled(false);
                lineColorWell.setColor(NSColor.whiteColor());
                lineColorWell.setEnabled(false);
                lineWidthSlider.setFloatValue(0.0f);
                lineWidthSlider.setEnabled(false);
                lineWidthTextField.setFloatValue(0.0f);
                lineWidthTextField.setEnabled(false);
                xTextField.setStringValue("");
                xTextField.setEnabled(false);
                yTextField.setStringValue("");
                yTextField.setEnabled(false);
                widthTextField.setStringValue("");
                widthTextField.setEnabled(false);
                heightTextField.setStringValue("");
                heightTextField.setEnabled(false);
            }
        }
    }

    public void fillCheckboxAction(NSButton sender) {
        NSArray selectedGraphics = _inspectingGraphicView.selectedGraphics();
        int i, c = selectedGraphics.count();
        if (c > 0) {
            int state = sender.state();

            for (i=0; i<c; i++) {
                ((SKTGraphic)selectedGraphics.objectAtIndex(i)).setDrawsFill(((state == NSCell.OnState) ? true : false));
                if ( ((SKTGraphic) selectedGraphics.objectAtIndex(i)).fillColor() == null) {
                    ((SKTGraphic)selectedGraphics.objectAtIndex(i)).setFillColor(NSColor.whiteColor());
                }
            }
            if (_inspectingGraphicView.undoManager() != null) {
                _inspectingGraphicView.undoManager().setActionName(NSBundle.localizedString("Set Fill Color","Action name for set fill color."));
            }
        }
    }

    public void fillColorWellAction(NSColorWell sender) {
        NSArray selectedGraphics = _inspectingGraphicView.selectedGraphics();
        int i, c = selectedGraphics.count();
        if (c > 0) {
            NSColor color = sender.color();

            for (i=0; i<c; i++) {
                ((SKTGraphic) selectedGraphics.objectAtIndex(i)).setFillColor(color);
            }
            if (_inspectingGraphicView.undoManager() != null) {
                _inspectingGraphicView.undoManager().setActionName(NSBundle.localizedString("Set Fill Color", "Action name for set fill color."));
            }
        }
    }

    public void lineCheckboxAction(NSButton sender) {
        NSArray selectedGraphics = _inspectingGraphicView.selectedGraphics();
        int i, c = selectedGraphics.count();
        if (c > 0) {
            int state = sender.state();

            for (i=0; i<c; i++) {
                ((SKTGraphic) selectedGraphics.objectAtIndex(i)).setDrawsStroke(((state == NSCell.OnState) ? true : false));
                if ( ((SKTGraphic) selectedGraphics.objectAtIndex(i)).strokeColor() == null) {
                    ((SKTGraphic) selectedGraphics.objectAtIndex(i)).setStrokeColor(NSColor.blackColor());
                }
            }
            if (_inspectingGraphicView.undoManager() != null) {
                _inspectingGraphicView.undoManager().setActionName(NSBundle.localizedString("Set Stroke Color", "Action name for set stroke color."));
            }
        }
    }

    public void lineColorWellAction(NSColorWell sender) {
        NSArray selectedGraphics = _inspectingGraphicView.selectedGraphics();
        int i, c = selectedGraphics.count();
        if (c > 0) {
            NSColor color = sender.color();

            for (i=0; i<c; i++) {
                ((SKTGraphic) selectedGraphics.objectAtIndex(i)).setStrokeColor(color);
            }
            if (_inspectingGraphicView.undoManager() != null) {
                _inspectingGraphicView.undoManager().setActionName(NSBundle.localizedString("Set Stroke Color","Action name for set stroke color."));
            }
        }
    }

    public void lineWidthSliderAction(NSSlider sender) {
        NSArray selectedGraphics = _inspectingGraphicView.selectedGraphics();
        int i, c = selectedGraphics.count();
        if (c > 0) {
            float lineWidth = sender.floatValue();

            for (i=0; i<c; i++) {
                ((SKTGraphic) selectedGraphics.objectAtIndex(i)).setStrokeLineWidth(lineWidth);
            }
            lineWidthTextField.setFloatValue(lineWidth);
            if (_inspectingGraphicView.undoManager() != null) {
                _inspectingGraphicView.undoManager().setActionName(NSBundle.localizedString("Set Line Width","UndoStrings","Action name for set line width."));
            }
        }
    }

    public void lineWidthTextFieldAction(NSTextField sender) {
        NSArray selectedGraphics = _inspectingGraphicView.selectedGraphics();
        int i, c = selectedGraphics.count();
        if (c > 0) {
            float lineWidth = sender.floatValue();

            for (i=0; i<c; i++) {
                ((SKTGraphic) selectedGraphics.objectAtIndex(i)).setStrokeLineWidth(lineWidth);
            }
            if (_inspectingGraphicView.undoManager() != null) {
                _inspectingGraphicView.undoManager().setActionName(NSBundle.localizedString("Set Line Width","Action name for set line width."));
            }
        }
    }

    public void dimensionTextFieldAction(Object sender) {
        NSArray selectedGraphics = _inspectingGraphicView.selectedGraphics();
        int i, c = selectedGraphics.count();
        if (c > 0) {
            NSMutableRect bounds = new NSMutableRect(xTextField.floatValue(), yTextField.floatValue(), widthTextField.floatValue(), heightTextField.floatValue());

            for (i=0; i<c; i++) {
                ((SKTGraphic) selectedGraphics.objectAtIndex(i)).setBounds(bounds);
            }
            if (_inspectingGraphicView.undoManager() != null) {
                _inspectingGraphicView.undoManager().setActionName(NSBundle.localizedString("Set Bounds","Action name for numerically setting bounds."));
            }
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
