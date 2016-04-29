// SKTGraphic.java
// Sketch Example
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;
import java.lang.Math;

public class SKTGraphic extends NSObject implements Cloneable {

    public static final String GraphicDidChangeNotification = "GraphicDidChange";
    
    public static final int NoKnob = 0;
    public static final int UpperLeftKnob = 1;
    public static final int UpperMiddleKnob = 2;
    public static final int UpperRightKnob = 3;
    public static final int MiddleLeftKnob = 4;
    public static final int MiddleRightKnob = 5;
    public static final int LowerLeftKnob = 6;
    public static final int LowerMiddleKnob = 7;
    public static final int LowerRightKnob = 8;

    public static final int NoKnobsMask = 0;
    public static final int UpperLeftKnobMask = 1<<UpperLeftKnob;
    public static final int UpperMiddleKnobMask = 1<<UpperMiddleKnob;
    public static final int UpperRightKnobMask = 1<<UpperRightKnob;
    public static final int MiddleLeftKnobMask = 1<<MiddleLeftKnob;
    public static final int MiddleRightKnobMask = 1<<MiddleRightKnob;
    public static final int LowerLeftKnobMask = 1<<LowerLeftKnob;
    public static final int LowerMiddleKnobMask = 1<<LowerMiddleKnob;
    public static final int LowerRightKnobMask = 1<<LowerRightKnob;
    public static final int AllKnobsMask = 0xffffffff;

    private static boolean initedFlips = false;
    private static int horizFlips[] = new int [9];
    private static int vertFlips[] = new int [9];

    private static final String ClassKey = "Class";
    private static final String BoundsKey = "Bounds";
    private static final String DrawsFillKey = "DrawsFill";
    private static final String FillColorKey = "FillColor";
    private static final String DrawsStrokeKey = "DrawsStroke";
    private static final String StrokeColorKey = "StrokeColor";
    private static final String StrokeLineWidthKey = "StrokeLineWidth";

    private static NSCursor crosshairCursor = null;

    private SKTDrawDocument _document;
    private NSRect _bounds;
    private NSRect _origBounds;
    private float _lineWidth;
    private NSColor _fillColor;
    private NSColor _strokeColor;
    private boolean _drawsFill;
    private boolean _drawsStroke;
    private boolean _manipulatingBounds;

    public SKTGraphic () {
        super ();
        _document = null;
        this.setBounds(new NSRect(0.0f, 0.0f, 1.0f, 1.0f));
        this.setFillColor(NSColor.whiteColor());
        this.setDrawsFill(false);
        this.setStrokeColor(NSColor.blackColor());
        this.setDrawsStroke(true);
        this.setStrokeLineWidth(1.0f);
        _origBounds = null;
        _manipulatingBounds = false;
    }

    public Object clone() {
        SKTGraphic newObj = null;
        try {
            newObj = (SKTGraphic)this.getClass().newInstance();
        } catch(InstantiationException e) {
            return null;
        } catch(IllegalAccessException e) {
            return null;
        }

        // Document is not "copied".  The new graphic will need to be inserted into a document.
        if (newObj != null) {
            newObj.setBounds(this.bounds());
            newObj.setFillColor(this.fillColor());
            newObj.setDrawsFill(this.drawsFill());
            newObj.setStrokeColor(this.strokeColor());
            newObj.setDrawsStroke(this.drawsStroke());
            newObj.setStrokeLineWidth(this.strokeLineWidth());
        }
        return newObj;
    }

// Document accessors and conveniences
    
    public void setDocument(SKTDrawDocument document) {
        _document = document;
    }

    public SKTDrawDocument document() {
        return _document;
    }

    public NSUndoManager undoManager() {
        if (_document != null) {
            return _document.undoManager();
        } else {
            return null;
        }
    }

    public String graphicType() {
        return (this.getClass().getName());
    }

// Primitives

    public void didChange() {
        if (_document != null) {
            _document.invalidateGraphic(this);
        }
        NSNotificationCenter.defaultCenter().postNotification(GraphicDidChangeNotification, this);
    }


    public void setBounds(NSRect bounds) {
        if ((_bounds == null) || !bounds.isEqualToRect(_bounds)) {
            if (!_manipulatingBounds) {
                // Send the notification before and after so that observers who invalidate display in views will wind up invalidating both the original rect and the new one.
                didChange();
                if (undoManager() != null) {
                    Class arrClass[] = {NSRect.class};
                    Object args[] = {_bounds};
                    undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setBounds", arrClass), args);
                }
            }
            _bounds = new NSRect(bounds);
            if (!_manipulatingBounds) {
                didChange();
            }
        }
    }

    public NSRect bounds() {
        return _bounds;
    }

    public void setDrawsFill(boolean flag) {
        if (_drawsFill != flag) {
            if (undoManager() != null) {
                Class arrClass[] = {Boolean.TYPE};
                Object args[] = {new Boolean(_drawsFill)};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setDrawsFill", arrClass), args);
            }
            _drawsFill = (flag ? true : false);
            didChange();
        }
    }

    public boolean drawsFill() {
        return _drawsFill;
    }

    public void setFillColor(NSColor fillColor){
        if (_fillColor != fillColor) {
            if (undoManager() != null) {
                Class arrClass[] = {NSColor.class};
                Object args[] = {_fillColor};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setFillColor", arrClass), args);
            }
            _fillColor = fillColor;
            didChange();
        }
        if (_fillColor != null) {
            this.setDrawsFill(true);
        } else {
            this.setDrawsFill(false);
        }
    }

    public NSColor fillColor() {
        return _fillColor;
    }

    public void setDrawsStroke(boolean flag) {
        if (_drawsStroke != flag) {
            if (undoManager() != null) {
                Class arrClass[] = {Boolean.TYPE};
                Object args[] = {new Boolean(_drawsStroke)};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setDrawsStroke", arrClass), args);
            }
            _drawsStroke = flag;
            didChange();
        }
    }

    public boolean drawsStroke() {
        return _drawsStroke;
    }

    public void setStrokeColor(NSColor strokeColor) {
        if (_strokeColor != strokeColor) {
            if (undoManager() != null) {
                Class arrClass[] = {NSColor.class};
                Object args[] = {_strokeColor};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setStrokeColor", arrClass), args);
            }
            _strokeColor = strokeColor;
            didChange();
        }
        if (_strokeColor != null) {
            this.setDrawsStroke(true);
        } else {
            this.setDrawsStroke(false);
        }
    }

    public NSColor strokeColor() {
        return _strokeColor;
    }

    public void setStrokeLineWidth(float width){
        if (_lineWidth != width) {
            if (undoManager() != null) {
                Class arrClass[] = {Float.TYPE};
                Object args[] = {new Float(_lineWidth)};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setStrokeLineWidth", arrClass), args);
            }
            if (width >= 0.0f) {
                this.setDrawsStroke(true);
                _lineWidth = width;
            } else {
                this.setDrawsStroke(false);
                _lineWidth = 0.0f;
            }
            didChange();
        }
    }

    public float strokeLineWidth() {
        return _lineWidth;
    }

// Extended mutation

    public void startBoundsManipulation() {
        // Save the original bounds.
        _manipulatingBounds = true;
        _origBounds = new NSRect(_bounds);
    }

    public void stopBoundsManipulation() {
        if (_manipulatingBounds) {
            // Restore the original bounds, the set the new bounds.
            if (!(_origBounds.isEqualToRect(_bounds))) {
                NSRect temp;

                _manipulatingBounds = false;
                temp = _bounds;
                _bounds = _origBounds;
                setBounds(temp);
            } else {
                _manipulatingBounds = false;
            }
        }
    }

    public void moveBy(NSPoint vector) {
        setBounds(new NSMutableRect(bounds().rectByOffsettingRect(vector.x(), vector.y())));
    }

    public void flipHorizontally() {
        // Some subclasses need to know.
        return;
    }

    public void flipVertically() {
        // Some subclasses need to know.
        return;
    }

    public static int flipKnobHorizontal(int knob, boolean horizFlag) {

        if (!initedFlips) {
            horizFlips[UpperLeftKnob] = UpperRightKnob;
            horizFlips[UpperMiddleKnob] = UpperMiddleKnob;
            horizFlips[UpperRightKnob] = UpperLeftKnob;
            horizFlips[MiddleLeftKnob] = MiddleRightKnob;
            horizFlips[MiddleRightKnob] = MiddleLeftKnob;
            horizFlips[LowerLeftKnob] = LowerRightKnob;
            horizFlips[LowerMiddleKnob] = LowerMiddleKnob;
            horizFlips[LowerRightKnob] = LowerLeftKnob;

            vertFlips[UpperLeftKnob] = LowerLeftKnob;
            vertFlips[UpperMiddleKnob] = LowerMiddleKnob;
            vertFlips[UpperRightKnob] = LowerRightKnob;
            vertFlips[MiddleLeftKnob] = MiddleLeftKnob;
            vertFlips[MiddleRightKnob] = MiddleRightKnob;
            vertFlips[LowerLeftKnob] = UpperLeftKnob;
            vertFlips[LowerMiddleKnob] = UpperMiddleKnob;
            vertFlips[LowerRightKnob] = UpperRightKnob;
            initedFlips = true;
        }
        if (horizFlag) {
            return horizFlips[knob];
        } else {
            return vertFlips[knob];
        }
    }

    public int resizeByMovingKnobToPoint(int knob, NSPoint point){
        NSMutableRect bounds = new NSMutableRect(bounds());
        boolean	negativeMeasur = false;

        if ((knob == UpperLeftKnob) || (knob == MiddleLeftKnob) || (knob == LowerLeftKnob)) {
            // Adjust left edge
            // check for negative, NSRect does not like negative height or width
            if (bounds.maxX() - point.x() < 0) {
                negativeMeasur = true;
                bounds.setWidth(point.x() - bounds.maxX());
            } else {
                bounds.setWidth(bounds.maxX() - point.x());
            }
            bounds.setX(point.x());
        } else if ((knob == UpperRightKnob) || (knob == MiddleRightKnob) || (knob == LowerRightKnob)) {
            // Adjust right edge
            if (point.x() - bounds.x() < 0) {
                negativeMeasur = true;
                bounds.setWidth(bounds.x() - point.x());
            } else {
                bounds.setWidth(point.x() - bounds.x());
            }
        }
        if (negativeMeasur) {
            knob = SKTGraphic.flipKnobHorizontal(knob,true);
            bounds.setX (bounds.x() - bounds.width());
            flipHorizontally();
        }

        negativeMeasur = false;

        if ((knob == UpperLeftKnob) || (knob == UpperMiddleKnob) || (knob == UpperRightKnob)) {
            // Adjust top edge
            // check for negative, NSRect does not like negative height or width
            if (bounds.maxY() - point.y() < 0) {
                negativeMeasur = true;
                bounds.setHeight (point.y() - bounds.maxY());
            } else {
                bounds.setHeight (bounds.maxY() - point.y());
            }
            bounds.setY( point.y());
        } else if ((knob == LowerLeftKnob) || (knob == LowerMiddleKnob) || (knob == LowerRightKnob)) {
            // Adjust bottom edge
            if (point.y() - bounds.y() < 0) {
                negativeMeasur = true;
                bounds.setHeight (bounds.y() - point.y());
            } else {
                bounds.setHeight (point.y() - bounds.y());
            }
        }
        if (negativeMeasur) {
            knob = SKTGraphic.flipKnobHorizontal(knob,false);
            bounds.setY (bounds.y() - bounds.height());
            flipVertically();
        }
        setBounds(bounds);
        return knob;
    }

    public void makeNaturalSize(){
        // Do nothing by default
    }

// Subclass capabilities

// Some subclasses will not ever have a stroke or fill or a natural size.  Overriding these methods in such subclasses allows the Inspector and Menu items to better reflect allowable actions.

    public boolean canDrawStroke() {
        return true;
    }

    public boolean canDrawFill() {
        return true;
    }

    public boolean hasNaturalSize() {
        return true;
    }

// Sorting graphics by their position in their document
    
    public int orderByIndex(SKTGraphic graphic) {
        int myIndex, otherIndex;

        NSArray graphics = document().graphics();

        myIndex = graphics.indexOfIdenticalObject(this);
        otherIndex = graphics.indexOfIdenticalObject(graphic);
        if (myIndex == otherIndex) {
            // return NSOrderedSame;
            return 0;
        } else if (myIndex < otherIndex) {
            // return NSOrderedAscending;
            return -1;
        } else {
            // return NSOrderedDescending;
            return 1;
        }
    }
    
// Persistence

    public NSMutableDictionary propertyListRepresentation() {
        Object obj, obj2;
        NSMutableDictionary dict = new NSMutableDictionary();
        String className = this.getClass().getName();
    
        // Strip SKT prefix to preserve document capatibility with old versions of Sketch.
        if (className.startsWith("SKT")) {
            className = className.substring(3);
        }
        dict.setObjectForKey(className,ClassKey);
        dict.setObjectForKey(bounds().toString(), BoundsKey);
        dict.setObjectForKey((drawsFill()) ? "YES" : "NO", DrawsFillKey);
        if (fillColor()!= null) {
            dict.setObjectForKey(NSArchiver.archivedDataWithRootObject(this.fillColor()), FillColorKey);
        }
        dict.setObjectForKey((drawsStroke()) ? "YES" : "NO", DrawsStrokeKey);
        if (strokeColor() != null) {
            dict.setObjectForKey(NSArchiver.archivedDataWithRootObject(this.strokeColor()), StrokeColorKey);
        }
        dict.setObjectForKey((Float.toString(this.strokeLineWidth())), StrokeLineWidthKey);

        return dict;
    }

    public static Object graphicWithPropertyListRepresentation (NSDictionary dict) {
        Class theClass;
        SKTGraphic theGraphic = null;

        try {
            theClass = Class.forName((String)dict.objectForKey(ClassKey));
        } catch (ClassNotFoundException e) {
            theClass = null;
        }
        // Prepend SKT to the class name if we did not find it literally.  When we write the classname key we strip the prefix.  We try it first without the prefix because for a short time Sketch did not strip the prefix so there could be documents that do not need it prepended.
        if (theClass == null) {
            try {
                theClass = Class.forName("SKT" + (String)dict.objectForKey(ClassKey));
            } catch (ClassNotFoundException e) {
                theClass = null;
            }
        }
        if (theClass != null) {
            try {
                theGraphic = (SKTGraphic)theClass.newInstance();
            } catch (InstantiationException e) {
                theGraphic = null;
            } catch (IllegalAccessException e) {
                theGraphic = null;
            }
            if (theGraphic != null) {
                theGraphic.loadPropertyListRepresentation(dict);
            }
        }
        return theGraphic;
    }

    public void loadPropertyListRepresentation(NSDictionary dict) {
        Object obj;
        NSRect tempRect;

        obj = dict.objectForKey(BoundsKey);

        if (obj != null) {
            tempRect = NSRect.fromString((String) obj);
            this.setBounds(tempRect);
        }


        obj = dict.objectForKey(FillColorKey);
        if (obj != null) {
            this.setFillColor((NSColor) NSUnarchiver.unarchiveObjectWithData((NSData) obj));
        }
        obj = dict.objectForKey(DrawsFillKey);
        if (obj != null) {
            this.setDrawsFill( ((String) obj).equals("YES") );
        }
        obj = dict.objectForKey(StrokeColorKey);
        if (obj != null) {
            this.setStrokeColor((NSColor) NSUnarchiver.unarchiveObjectWithData((NSData) obj));
        }
        obj = dict.objectForKey(StrokeLineWidthKey);
        if (obj != null) {
            this.setStrokeLineWidth(Float.valueOf((String)obj).floatValue());
        }
        obj = dict.objectForKey(DrawsStrokeKey);
        if (obj != null) {
            this.setDrawsStroke( ((String) obj).equals("YES") );
        }
        return;
    }

// Drawing
    public NSRect drawingBounds(){
        float inset = -SKTGraphicView.HALF_HANDLE_WIDTH;
        if (this.drawsStroke()) {
            float halfLineWidth = (this.strokeLineWidth() / 2.0f) + 1.0f;
            if (-halfLineWidth < inset) {
                inset = -halfLineWidth;
            }
        }
        inset += -1.0f;
        return bounds().rectByInsettingRect( inset, inset);
    }

    public NSBezierPath bezierPath() {
        // Subclasses that just have a simple path override this to return it.  The basic drawInView:isSelected: implementation below will stroke and fill this path.  Subclasses that need more complex drawing will just override drawInView:isSelected:.
        return null;
    }

    public void drawInViewIsSelected(SKTGraphicView view, boolean flag) {
        NSBezierPath path = this.bezierPath();
        if (path != null) {
            boolean b = drawsFill();
            if (this.drawsFill()) {
                this.fillColor().set();
                path.fill();
            }
            b = drawsStroke();
            if (this.drawsStroke()) {
                this.strokeColor().set();
                path.stroke();
            }
        }
        if (flag) {
            this.drawHandlesInView(view);
        }
    }

    public int knobMask()    {
        return AllKnobsMask;
    }

    public int knobUnderPoint(NSPoint point) {
        NSRect bounds = this.bounds();
        int knobMask = this.knobMask();
        NSMutableRect handleRect = new NSMutableRect();

        handleRect.setWidth (SKTGraphicView.HANDLE_WIDTH);
        handleRect.setHeight (SKTGraphicView.HANDLE_WIDTH);

        if ((knobMask & UpperLeftKnobMask) != 0) {
            handleRect.setX(bounds().x() - SKTGraphicView.HALF_HANDLE_WIDTH);
            handleRect.setY(bounds().y() - SKTGraphicView.HALF_HANDLE_WIDTH);
            if (handleRect.containsPoint(point, true)) {
                return UpperLeftKnob;
            }
        }
        if ((knobMask & UpperMiddleKnobMask) !=0) {
            handleRect.setX(bounds().midX() - SKTGraphicView.HALF_HANDLE_WIDTH);
            handleRect.setY(bounds().y() - SKTGraphicView.HALF_HANDLE_WIDTH);
            if (handleRect.containsPoint(point, true)) {
                return UpperMiddleKnob;
            }
        }
        if ((knobMask & UpperRightKnobMask) != 0) {
            handleRect.setX(bounds().maxX() - SKTGraphicView.HALF_HANDLE_WIDTH);
            handleRect.setY(bounds().y() - SKTGraphicView.HALF_HANDLE_WIDTH);
            if (handleRect.containsPoint(point, true)) {
                return UpperRightKnob;
            }
        }
        if ((knobMask & MiddleLeftKnobMask) != 0) {
            handleRect.setX(bounds().x() - SKTGraphicView.HALF_HANDLE_WIDTH);
            handleRect.setY(bounds().midY() - SKTGraphicView.HALF_HANDLE_WIDTH);
            if (handleRect.containsPoint(point, true)) {
                return MiddleLeftKnob;
            }
        }
        if ((knobMask & MiddleRightKnobMask) != 0) {
            handleRect.setX(bounds().maxX() - SKTGraphicView.HALF_HANDLE_WIDTH);
            handleRect.setY(bounds().midY() - SKTGraphicView.HALF_HANDLE_WIDTH);
            if (handleRect.containsPoint(point, true)) {
                return MiddleRightKnob;
            }
        }
        if ((knobMask & LowerLeftKnobMask) != 0) {
            handleRect.setX(bounds().x() - SKTGraphicView.HALF_HANDLE_WIDTH);
            handleRect.setY(bounds().maxY() - SKTGraphicView.HALF_HANDLE_WIDTH);
            if (handleRect.containsPoint(point, true)) {
                return LowerLeftKnob;
            }
        }
        if ((knobMask & LowerMiddleKnobMask) != 0) {
            handleRect.setX(bounds().midX() - SKTGraphicView.HALF_HANDLE_WIDTH);
            handleRect.setY(bounds().maxY() - SKTGraphicView.HALF_HANDLE_WIDTH);
            if (handleRect.containsPoint(point, true)) {
                return LowerMiddleKnob;
            }
        }
        if ((knobMask & LowerRightKnobMask) != 0) {
            handleRect.setX(bounds().maxX() - SKTGraphicView.HALF_HANDLE_WIDTH);
            handleRect.setY(bounds().maxY() - SKTGraphicView.HALF_HANDLE_WIDTH);
            if (handleRect.containsPoint(point, true)) {
                return LowerRightKnob;
            }
        }

        return NoKnob;
    }

    public void drawHandleAtPointInView(NSPoint point, SKTGraphicView view){
        NSMutableRect handleRect = new NSMutableRect();

        handleRect.setX ( point.x() - SKTGraphicView.HALF_HANDLE_WIDTH + 1.0f);
        handleRect.setY ( point.y() - SKTGraphicView.HALF_HANDLE_WIDTH + 1.0f);
        handleRect.setWidth (SKTGraphicView.HANDLE_WIDTH - 1.0f);
        handleRect.setHeight (SKTGraphicView.HANDLE_WIDTH - 1.0f);
        handleRect = new NSMutableRect (view.centerScanRect((NSRect) handleRect));
        NSColor.controlDarkShadowColor().set();
        NSBezierPath.fillRect((NSRect) handleRect);
        handleRect.offsetRect (-1.0f, -1.0f);
        NSColor.knobColor().set();
        NSBezierPath.fillRect((NSRect) handleRect);
    }

    public void drawHandlesInView(SKTGraphicView view) {
        NSRect bounds = this.bounds();
        int knobMask = this.knobMask();

        if ((knobMask & UpperLeftKnobMask) != 0) {
            this.drawHandleAtPointInView(new NSPoint(bounds.x(), bounds.y()),view);
        }
        if ((knobMask & UpperMiddleKnobMask) != 0) {
            this.drawHandleAtPointInView(new NSPoint(bounds.midX(),bounds.y()),view);
        }
        if ((knobMask & UpperRightKnobMask) != 0) {
            this.drawHandleAtPointInView(new NSPoint(bounds.maxX(),bounds.y()),view);
        }

        if ((knobMask & MiddleLeftKnobMask) != 0) {
            this.drawHandleAtPointInView(new NSPoint(bounds.x(),bounds.midY()),view);
        }
        if ((knobMask & MiddleRightKnobMask) != 0) {
            this.drawHandleAtPointInView(new NSPoint(bounds.maxX(),bounds.midY()),view);
        }

        if ((knobMask & LowerLeftKnobMask) != 0) {
            this.drawHandleAtPointInView(new NSPoint(bounds.x(), bounds.maxY()) ,view);
        }
        if ((knobMask & LowerMiddleKnobMask) != 0) {
            this.drawHandleAtPointInView(new NSPoint(bounds.midX(), bounds.maxY()),view);
        }
        if ((knobMask & LowerRightKnobMask) != 0) {
            this.drawHandleAtPointInView(new NSPoint(bounds.maxX(),bounds.maxY()),view);
        }
    }

// Event Handling
    public static NSCursor creationCursor() {
        // The only reason a static method that is meant to be overridden works for us in Java is because the caller uses the reflection API to get invoke the method.  Normally static methods don't get dynamic dispatch.
        // By default we use the crosshair cursor
        if (crosshairCursor == null) {
            NSImage crosshairImage = NSImage.imageNamed("Cross");
            NSSize imageSize = crosshairImage.size();
            crosshairCursor = new NSCursor(crosshairImage, new NSPoint((imageSize.width()/ 2.0f), (imageSize.height() / 2.0f)));
        }
        return crosshairCursor;
    }

    public boolean createWithEventInView(NSEvent theEvent, SKTGraphicView view) {
        // default implementation tracks until mouseUp: just setting the bounds of the new graphic.
        // MF:??? Perhaps this should be in SKTGraphicView...
        NSPoint testp = theEvent.locationInWindow();
        NSMutablePoint point = new NSMutablePoint( view.convertPointFromView(theEvent.locationInWindow(),null));
        int knob = LowerRightKnob;
        NSRect bounds;
        boolean snapsToGrid = view.snapsToGrid();
        float spacing = view.gridSpacing();
        boolean echoToRulers = view.enclosingScrollView().rulersVisible();

        startBoundsManipulation();

        if (snapsToGrid) {
            point.setX( (int) Math.floor(  ((point.x() / spacing) + 0.5f) * spacing));
            point.setY( (int) Math.floor(  ((point.y() / spacing) + 0.5f) * spacing));
        }
        setBounds(new NSMutableRect(point.x(),point.y(), 0.0f, 0.0f));
        if (echoToRulers) {
            view.beginEchoingMoveToRulers(this.bounds());
        }
        while (true) {
            theEvent = view.window().nextEventMatchingMask((NSEvent.LeftMouseDraggedMask| NSEvent.LeftMouseUpMask));
            point = new NSMutablePoint( view.convertPointFromView(theEvent.locationInWindow(),null));
            if (snapsToGrid) {
                point.setX((int) Math.floor (  ((point.x() / spacing) + 0.5f) * spacing));
                point.setY((int) Math.floor (  ((point.y() / spacing) + 0.5f) * spacing));
            }
            view.setNeedsDisplay(drawingBounds());
            knob = this.resizeByMovingKnobToPoint(knob,point);
            view.setNeedsDisplay(drawingBounds());
            if (echoToRulers) {
                view.continueEchoingMoveToRulers(this.bounds());
            }
            if (theEvent.type() == NSEvent.LeftMouseUp) {
                break;
            }
        }
        if (echoToRulers) {
            view.stopEchoingMoveToRulers();
        }

        stopBoundsManipulation();

        bounds = bounds();
        if ((bounds.width() > 0.0f) || (bounds.height() > 0.0f)) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isEditable() {
        return false;
    }

    public void startEditingWithEventInView(NSEvent event, SKTGraphicView view){
        return;
    }

    public void endEditingInView(SKTGraphicView view) {
        return;
    }

    public boolean hitTest(NSPoint point, boolean isSelected) {
        if (isSelected && (this.knobUnderPoint(point) != NoKnob)) {
            return true;
        } else {
            NSBezierPath path = this.bezierPath();

            if (path != null) {
                if (path.containsPoint(point)) {
                    return true;
                }
            } else {
                if (this.bounds().containsPoint(point, true)) {
                    return true;
                }
            }
            return false;
        }
    }


    public String toString()  {
        return propertyListRepresentation().toString();
    }

    public NSScriptObjectSpecifier objectSpecifier() {
        NSArray graphics = this.document().graphics();
        int index = graphics.indexOfIdenticalObject(this);

        if (index != NSArray.NotFound) {
            NSScriptObjectSpecifier containerRef = NSKeyValue.objectSpecifier(this.document());
            if (containerRef != null) {
                return new NSIndexSpecifier(containerRef.keyClassDescription(), containerRef, "graphics", index);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    public float xPosition() {
        NSRect bounds = this.bounds();
        return bounds.x();
    }

    public void setXPosition(float xPos) {
        NSMutableRect bounds = new NSMutableRect(this.bounds());
        bounds.setX(xPos);
        this.setBounds(bounds);
    }

    public float yPosition() {
        NSRect bounds = this.bounds();
        return bounds.y();
    }

    public void setYPosition(float yPos) {
        NSMutableRect bounds = new NSMutableRect(this.bounds());
        bounds.setY(yPos);
        this.setBounds(bounds);
    }

    public float width() {
        NSRect bounds = this.bounds();
        return bounds.width();
    }

    public void setWidth(float w) {
        NSMutableRect bounds = new NSMutableRect(this.bounds());
        bounds.setWidth(w);
        this.setBounds(bounds);
    }

    public float height() {
        NSRect bounds = this.bounds();
        return bounds.height();
    }

    public void setHeight(float h) {
        NSMutableRect bounds = new NSMutableRect(this.bounds());
        bounds.setHeight(h);
        this.setBounds(bounds);
    }

    public int classCode() {
        // If we subclassed NSObject, we wouldn't need this, but since we don't...
	// It would be simpler to just subclass NSObject, but this shows what has to be done in a case where there's some good reason not to.  Basically, your class is required to provide access to keys defined in AbstractObject (which NSObject would be providing automatically).  classCode is the only key defined by AbstractObject.
        System.out.println("classCode");
        NSScriptClassDescription classDesc = (NSScriptClassDescription)NSKeyValue.classDescription(this);
        if (classDesc != null) {
            return classDesc.appleEventCode();
        } else {
            return 0;
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
