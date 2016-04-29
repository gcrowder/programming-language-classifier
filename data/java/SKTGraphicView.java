// SKTGraphicView.java
// Sketch Example 
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;
import java.util.*;
import java.io.*;
import java.lang.Math;


public class SKTGraphicView extends NSView {
    public static final float HALF_HANDLE_WIDTH = 3.0f;
    public static final float HANDLE_WIDTH = (HALF_HANDLE_WIDTH*2.0f);
    public static final String GraphicViewSelectionDidChangeNotification = "GraphicViewSelectionDidChange";
    private static final float DefaultPasteCascadeDelta = 10.0f;

    private SKTDrawWindowController controller;
    private NSMutableArray _selectedGraphics;
    private SKTGraphic _creatingGraphic;
    private NSMutableRect _rubberbandRect;
    private NSSet _rubberbandGraphics;
    private boolean _rubberbandIsDeselecting;
    private boolean _initedRulers;
    private SKTGraphic _editingGraphic;
    private NSView _editorView;
    private int _pasteboardChangeCount;
    private int _pasteCascadeNumber;
    private NSMutablePoint _pasteCascadeDelta;
    private boolean _snapsToGrid;
    private boolean _showsGrid;
    private float _gridSpacing;
    private NSColor _gridColor;
    private boolean _knobsHidden;
    private NSTimer _unhideKnobsTimer;
    private NSRect _verticalRulerLineRect;
    private NSRect _horizontalRulerLineRect;

    public SKTGraphicView(NSRect frame) {
        super(frame);

        NSMutableArray dragTypes = new NSMutableArray ();
        dragTypes.addObject(NSPasteboard.ColorPboardType);
        dragTypes.addObject(NSPasteboard.FilenamesPboardType);
        dragTypes.addObjectsFromArray (NSImage.imagePasteboardTypes());
        registerForDraggedTypes((NSArray) dragTypes);
        _selectedGraphics = new NSMutableArray();
        _creatingGraphic = null;
        _rubberbandRect = new NSMutableRect();
        _rubberbandGraphics = null;
        _rubberbandIsDeselecting = false;
        _initedRulers = false;
        _editingGraphic = null;
        _editorView = null;
        _pasteboardChangeCount = -1;
        _pasteCascadeNumber = 0;
        _pasteCascadeDelta = new NSMutablePoint(DefaultPasteCascadeDelta, DefaultPasteCascadeDelta);
        _snapsToGrid = false;
        _showsGrid = false;
        _gridSpacing = 8.0f;
        _gridColor = NSColor.lightGrayColor();
        _knobsHidden = false;
        _unhideKnobsTimer = null;
    }

    protected void finalize() {
        endEditing();
    }

// SKTDrawWindowController accessors and convenience methods

    public void setDrawWindowController(SKTDrawWindowController theController) {
        controller = theController;
    }

    public SKTDrawWindowController drawWindowController() {
        return controller;
    }

    public SKTDrawDocument drawDocument() {
        return (SKTDrawDocument)(drawWindowController().document());
    }

    public NSMutableArray graphics() {
        return ((NSMutableArray)((SKTDrawDocument)drawWindowController().document()).graphics());
    }

// Display invalidation
    
    public void invalidateGraphic(SKTGraphic graphic) {
        setNeedsDisplay(graphic.drawingBounds());
        if (! graphics().containsObject(graphic)) {
            deselectGraphic(graphic);  // deselectGraphic will call invalidateGraphic, too, but only if the graphic is in the selection and since the graphic is removed from the selection before this method is called again the potential infinite loop should not happen.
        }
    }

    public void invalidateGraphics(NSArray graphics) {
        int i, c = graphics.count();
        for (i=0; i<c; i++) {
            this.invalidateGraphic((SKTGraphic)graphics.objectAtIndex(i));
        }
    }


// Selection primitives
    public NSArray selectedGraphics() {
        return _selectedGraphics;
    }

    public NSArray orderedSelectedGraphics() {
        Class arrClass[] = {SKTGraphic.class};
        return selectedGraphics().sortedArrayUsingSelector(new NSSelector("orderByIndex", arrClass));
    }

    public boolean graphicIsSelected(SKTGraphic graphic) {
        return _selectedGraphics.containsObject(graphic);
    }

    public void selectGraphic(SKTGraphic graphic) {
        if (! _selectedGraphics.containsObject(graphic)) {
            NSUndoManager undoManager = drawDocument().undoManager();
            if (undoManager != null) {
                Class arrClass[] = {SKTGraphic.class};
                Object args[] = {graphic};
                undoManager.registerUndoWithTargetAndArguments(this, new NSSelector("deselectGraphic", arrClass), args);
                undoManager.setActionName(NSBundle.localizedString("Selection Change", "Selection Change", "UndoStrings"));
            }
            _selectedGraphics.addObject(graphic);
            invalidateGraphic(graphic);
            _pasteCascadeDelta = new NSMutablePoint(DefaultPasteCascadeDelta, DefaultPasteCascadeDelta);
            NSNotificationCenter.defaultCenter().postNotification(GraphicViewSelectionDidChangeNotification, this);
            updateRulers();
        }
    }

    public void deselectGraphic(SKTGraphic graphic) {
        if (_selectedGraphics.containsObject(graphic)) {
            NSUndoManager undoManager = drawDocument().undoManager();
            if (undoManager != null) {
                Class arrClass[] = {SKTGraphic.class};
                Object args[] = {graphic};
                undoManager.registerUndoWithTargetAndArguments(this, new NSSelector("selectGraphic", arrClass), args);
                undoManager.setActionName(NSBundle.localizedString("Selection Change", "Selection Change", "UndoStrings"));
            }
            _selectedGraphics.removeObjectAtIndex(_selectedGraphics.indexOfObject(graphic));
            invalidateGraphic(graphic);
            _pasteCascadeDelta = new NSMutablePoint(DefaultPasteCascadeDelta, DefaultPasteCascadeDelta);
            NSNotificationCenter.defaultCenter().postNotification(GraphicViewSelectionDidChangeNotification, this);
            updateRulers();
        }
    }

    public void clearSelection() {
        int i, c = _selectedGraphics.count();
        Object curGraphic;

        if (c > 0) {
            NSUndoManager undoManager = drawDocument().undoManager();
            for (i=0; i<c; i++) {
                curGraphic = _selectedGraphics.objectAtIndex(i);
                if (undoManager != null) {
                    Class arrClass[] = {SKTGraphic.class};
                    Object args[] = {curGraphic};
                    undoManager.registerUndoWithTargetAndArguments(this, new NSSelector("selectGraphic", arrClass), args);
                }
                invalidateGraphic((SKTGraphic)curGraphic);
            }
            if (undoManager != null) {
                undoManager.setActionName(NSBundle.localizedString("Selection Change", "Selection Change", "UndoStrings"));
            }
            _selectedGraphics.removeAllObjects();
            _pasteCascadeDelta = new NSMutablePoint(DefaultPasteCascadeDelta, DefaultPasteCascadeDelta);
            NSNotificationCenter.defaultCenter().postNotification(GraphicViewSelectionDidChangeNotification,this);
            updateRulers();
        }
    }

// Editing
    
    public void setEditingGraphicEditorView(SKTGraphic graphic, NSView editorView) {
        // Called by a SKTGraphic that is told to start editing.  SKTGraphicView doesn't do anything with editorView, just remembers it.
        _editingGraphic = graphic;
        _editorView = editorView;
    }

    public SKTGraphic editingGraphic() {
        return _editingGraphic;
    }

    public NSView editorView() {
        return _editorView;
    }

    public void startEditingGraphicWithEvent(SKTGraphic graphic, NSEvent event) {
        graphic.startEditingWithEventInView(event,this);
    }

    public void endEditing() {
        if (_editingGraphic != null) {
            _editingGraphic.endEditingInView(this);
            _editingGraphic = null;
            _editorView = null;
        }
    }

// Geometry calculations

    public SKTGraphic graphicUnderPoint(NSPoint point) {
        SKTDrawDocument document = drawDocument();
        NSArray graphics = document.graphics();
        int i, c = graphics.count();
        SKTGraphic curGraphic = null;

        for (i=0; i<c; i++) {
            curGraphic = (SKTGraphic) graphics.objectAtIndex(i);
            if (isMouseInRect(point,curGraphic.drawingBounds()) && curGraphic.hitTest(point, this.graphicIsSelected(curGraphic))) {
                break;
            }
        }
        if (i < c) {
            return curGraphic;
        } else {
            return null;
        }
    }

    public NSSet graphicsIntersectingRect(NSRect rect) {
        NSArray graphics = graphics();
        int i, c = graphics.count();
        NSMutableSet result = new NSMutableSet();
        SKTGraphic curGraphic;

        for (i=0; i<c; i++) {
            curGraphic = (SKTGraphic)graphics.objectAtIndex(i);
            if (rect.intersectsRect(curGraphic.drawingBounds())) {
                result.addObject(curGraphic);
            }
        }
        return result;
    }

// Drawing and mouse tracking

    public boolean isFlipped() {
        return true;
    }

    public boolean isOpaque() {
        return true;
    }

    public boolean acceptsFirstResponder() {
        return true;
    }

    public boolean becomeFirstResponder() {
        updateRulers();
        return true;
    }

    public void drawRect(NSRect rect) {
        SKTDrawWindowController drawWindowController = this.drawWindowController();
        NSArray graphics = null;
        int i;
        SKTGraphic curGraphic;
        boolean isSelected;
        NSRect drawingBounds;
        NSGraphicsContext currentContext = NSGraphicsContext.currentContext();
        
        NSColor.whiteColor().set();

        NSBezierPath.fillRect(rect);
        if (showsGrid()) {
            SKTGridPanelController.sharedGridPanelController().drawGridWithSettingsInRect(gridSpacing(), gridColor(), rect, new NSPoint(0,0));
        }
        if (drawWindowController != null) {
            SKTDrawDocument doc = (SKTDrawDocument)drawWindowController.document();
            if (doc != null) {
                graphics = doc.graphics();
            }
        }
        i = ((graphics != null) ? graphics.count() : 0);
        while (i-- > 0) {
            curGraphic = (SKTGraphic)graphics.objectAtIndex(i);
            drawingBounds = curGraphic.drawingBounds();
            if (rect.intersectsRect(drawingBounds)) {
                if (!_knobsHidden && (curGraphic != _editingGraphic)) {
                    // Figure out if we should draw selected.
                    isSelected = graphicIsSelected(curGraphic);
                    // Account for any current rubberband selection state
                    if ((_rubberbandGraphics != null) && (isSelected == _rubberbandIsDeselecting) && _rubberbandGraphics.containsObject(curGraphic)) {
                        isSelected = (isSelected ? false : true);
                    }
                } else {
                    // Do not draw handles on graphics that are editing.
                    isSelected = false;
                }
                currentContext.saveGraphicsState();
                NSBezierPath.clipRect(drawingBounds);
                curGraphic.drawInViewIsSelected(this,isSelected);
                currentContext.restoreGraphicsState();
            }
        }

        if (_creatingGraphic != null) {
            drawingBounds = _creatingGraphic.drawingBounds();
            if (rect.intersectsRect(drawingBounds)) {
                currentContext.saveGraphicsState();
                NSBezierPath.clipRect(drawingBounds);
                _creatingGraphic.drawInViewIsSelected(this, false);
                currentContext.restoreGraphicsState();
            }
        }
        if (!_rubberbandRect.equals (new NSRect(0,0,0,0))) {
            NSColor.knobColor().set();
            NSGraphics.frameRect((NSRect)_rubberbandRect);
        }
    }

    public void beginEchoingMoveToRulers(NSRect echoRect) {
        NSRulerView horizontalRuler = this.enclosingScrollView().horizontalRulerView();
        NSRulerView verticalRuler = this.enclosingScrollView().verticalRulerView();

        _horizontalRulerLineRect = this.convertRectToView(echoRect, horizontalRuler);
        _verticalRulerLineRect = this.convertRectToView(echoRect, verticalRuler);

        horizontalRuler.moveRulerline(-1.0f, _horizontalRulerLineRect.x());
        horizontalRuler.moveRulerline(-1.0f, _horizontalRulerLineRect.midX());
        horizontalRuler.moveRulerline(-1.0f, _horizontalRulerLineRect.maxX());

        verticalRuler.moveRulerline(-1.0f, _verticalRulerLineRect.x());
        verticalRuler.moveRulerline(-1.0f, _verticalRulerLineRect.midX());
        verticalRuler.moveRulerline(-1.0f, _verticalRulerLineRect.maxX());
    }

    public void continueEchoingMoveToRulers(NSRect echoRect) {
        NSRulerView horizontalRuler = this.enclosingScrollView().horizontalRulerView();
        NSRulerView verticalRuler = this.enclosingScrollView().verticalRulerView();
        NSRect newHorizontalRect = this.convertRectToView(echoRect, horizontalRuler);
        NSRect newVerticalRect = this.convertRectToView(echoRect, verticalRuler);

        horizontalRuler.moveRulerline(_horizontalRulerLineRect.x(), newHorizontalRect.x());
        horizontalRuler.moveRulerline(_horizontalRulerLineRect.midX(), newHorizontalRect.midX());
        horizontalRuler.moveRulerline(_horizontalRulerLineRect.maxX(), newHorizontalRect.maxX());

        verticalRuler.moveRulerline(_verticalRulerLineRect.x(), newVerticalRect.x());
        verticalRuler.moveRulerline(_verticalRulerLineRect.midX(), newVerticalRect.midX());
        verticalRuler.moveRulerline(_verticalRulerLineRect.maxX(), newVerticalRect.maxX());

        _horizontalRulerLineRect = newHorizontalRect;
        _verticalRulerLineRect = newVerticalRect;
    }

    public void stopEchoingMoveToRulers() {
        NSRulerView horizontalRuler = this.enclosingScrollView().horizontalRulerView();
        NSRulerView verticalRuler = this.enclosingScrollView().verticalRulerView();

        horizontalRuler.moveRulerline(_horizontalRulerLineRect.x(), -1.0f);
        horizontalRuler.moveRulerline(_horizontalRulerLineRect.midX(), -1.0f);
        horizontalRuler.moveRulerline(_horizontalRulerLineRect.maxX(), -1.0f);

        verticalRuler.moveRulerline(_verticalRulerLineRect.x(), -1.0f);
        verticalRuler.moveRulerline(_verticalRulerLineRect.midX(), -1.0f);
        verticalRuler.moveRulerline(_verticalRulerLineRect.maxX(), -1.0f);

        _horizontalRulerLineRect = null;
        _verticalRulerLineRect = null;
    }

    public void createGraphicOfClassWithEvent(Class theClass, NSEvent theEvent) {
        SKTDrawDocument document = drawDocument();

        try {
            _creatingGraphic = (SKTGraphic) theClass.newInstance();
        } catch (java.lang.InstantiationException e) {
            System.out.print("InstantiationException in createGraphicOfClassWithEvent\n");
        } catch (java.lang.IllegalAccessException e) {
            System.out.print("IllegalAccessException in createGraphicOfClassWithEvent\n");
        }

        if (_creatingGraphic.createWithEventInView(theEvent,this)) {
            document.insertGraphicAtIndex(_creatingGraphic,0);
            selectGraphic(_creatingGraphic);
            if (_creatingGraphic.isEditable()) {
                this.startEditingGraphicWithEvent(_creatingGraphic,null);
            }
            NSBundle bundle = NSBundle.mainBundle();
            NSUndoManager undoManager = document.undoManager();
            if (undoManager != null) {
                undoManager.setActionName(bundle.localizedString("Create ","Create ", "UndoStrings") +  bundle.localizedString(theClass.getName(),"","GraphicClassNames"));
            }
        }
        _creatingGraphic = null;
    }

    public SKTGraphic creatingGraphic() {
        return _creatingGraphic;
    }

    public void trackKnobOfGraphicWithEvent(int knob, SKTGraphic graphic, NSEvent theEvent) {
        NSMutablePoint point;
        boolean snapsToGrid = snapsToGrid();
        float spacing = gridSpacing();
        boolean echoToRulers = this.enclosingScrollView().rulersVisible();

        graphic.startBoundsManipulation();

        if (echoToRulers) {
            this.beginEchoingMoveToRulers(graphic.bounds());
        }
        while (true) {
            theEvent = window().nextEventMatchingMask((NSEvent.LeftMouseDraggedMask| NSEvent.LeftMouseUpMask));
            point = new NSMutablePoint (convertPointFromView(theEvent.locationInWindow(),null));
            invalidateGraphic(graphic);
            if (snapsToGrid) {
                point.setX( (int) Math.floor ( ((point.x() / spacing) + 0.5f) * spacing));
                point.setY( (int) Math.floor ( ((point.y() / spacing) + 0.5f) * spacing));
            }
            knob = graphic.resizeByMovingKnobToPoint(knob,point);
            invalidateGraphic(graphic);
            if (echoToRulers) {
                this.continueEchoingMoveToRulers(graphic.bounds());
            }
            if (theEvent.type() == NSEvent.LeftMouseUp) {
                break;
            }
        }
        if (echoToRulers) {
            this.stopEchoingMoveToRulers();
        }

        graphic.stopBoundsManipulation();

        if (drawDocument().undoManager() != null) {
            drawDocument().undoManager().setActionName(NSBundle.localizedString("Resize", "Resize", "UndoStrings"));
        }
    }

    public void rubberbandSelectWithEvent(NSEvent theEvent) {
        NSPoint origPoint, curPoint;
        Enumeration objEnum;
        SKTGraphic curGraphic, curRubberGraphic;

        _rubberbandIsDeselecting = (((theEvent.modifierFlags() & NSEvent.AlternateKeyMask) != 0) ? true : false);
        curPoint = convertPointFromView(theEvent.locationInWindow(),null);
        origPoint = new NSPoint (curPoint);
        while (true) {
            theEvent = window().nextEventMatchingMask((NSEvent.LeftMouseDraggedMask| NSEvent.LeftMouseUpMask));
            curPoint = convertPointFromView(theEvent.locationInWindow(),null);
            if (origPoint.equals(curPoint)) {
                if (!(_rubberbandRect.equals (new NSRect(0,0,0,0)))) {
                    setNeedsDisplay(_rubberbandRect);
                    // invalidate each graphic in the rubberbanded set
                    if (_rubberbandGraphics != null) {
                        objEnum = _rubberbandGraphics.objectEnumerator();
                        while ((curRubberGraphic = (SKTGraphic) objEnum.nextElement()) != null) {
                            invalidateGraphic(curRubberGraphic);
                        }
                    }
                }
                _rubberbandRect = new NSMutableRect (0,0,0,0);
                _rubberbandGraphics = null;
            } else {
                NSRect newRubberbandRect = new NSRect(origPoint, curPoint);
                if (!(_rubberbandRect.equals (newRubberbandRect))) {
                    setNeedsDisplay(_rubberbandRect);
                    // invalidate each graphic in the rubberbanded set
                    if (_rubberbandGraphics != null) {
                        objEnum = _rubberbandGraphics.objectEnumerator();
                        while ((curRubberGraphic = (SKTGraphic) objEnum.nextElement()) != null) {
                            invalidateGraphic(curRubberGraphic);
                        }
                    }
                    _rubberbandRect = new NSMutableRect(newRubberbandRect);
                    _rubberbandGraphics = graphicsIntersectingRect(_rubberbandRect);
                    setNeedsDisplay(_rubberbandRect);
                    // invalidate each graphic in the rubberbanded set
                    if (_rubberbandGraphics != null) {
                        objEnum = _rubberbandGraphics.objectEnumerator();
                        while ((curRubberGraphic = (SKTGraphic) objEnum.nextElement()) != null) {
                            invalidateGraphic(curRubberGraphic);
                        }
                    }
                }
            }
            if (theEvent.type() == NSEvent.LeftMouseUp) {
                break;
            }
        }

        // Now select or deselect the rubberbanded graphics.
        if (_rubberbandGraphics != null)
          {
            objEnum = _rubberbandGraphics.objectEnumerator();
            while ((curGraphic = (SKTGraphic)objEnum.nextElement()) != null) {
                if (_rubberbandIsDeselecting) {
                    deselectGraphic(curGraphic);
                } else {
                    selectGraphic(curGraphic);
                }
            }
          }
        if (!_rubberbandRect.equals (new NSRect(0,0,0,0))) {
            setNeedsDisplay(_rubberbandRect);
        }

        _rubberbandRect = new NSMutableRect(0,0,0,0);
        _rubberbandGraphics = null;
    }

    public void moveSelectedGraphicsWithEvent(NSEvent theEvent) {
        NSMutablePoint lastPoint, curPoint;
        NSArray selGraphics = this.selectedGraphics();
        int i, c;
        SKTGraphic graphic;
        boolean didMove = false, isMoving = false;
        NSPoint selOriginOffset = new NSPoint(0,0);
        NSMutablePoint boundsOrigin = new NSMutablePoint();
        boolean snapsToGrid = snapsToGrid();
        float spacing = gridSpacing();
        Enumeration objEnum;
        SKTGraphic curGraphic;
        boolean echoToRulers = this.enclosingScrollView().rulersVisible();
        NSRect selBounds = this.drawDocument().boundsForGraphics(selGraphics);

        c = selGraphics.count();

        lastPoint = new NSMutablePoint (convertPointFromView(theEvent.locationInWindow(),null));
        if (snapsToGrid || echoToRulers) {
            selOriginOffset = new NSPoint((lastPoint.x() - selBounds.x()), (lastPoint.y() - selBounds.y()));
        }
        if (echoToRulers) {
            this.beginEchoingMoveToRulers(selBounds);
        }

        while (true) {
            theEvent = window().nextEventMatchingMask((NSEvent.LeftMouseDraggedMask| NSEvent.LeftMouseUpMask));
            curPoint = new NSMutablePoint (convertPointFromView(theEvent.locationInWindow(),null));
            if (!isMoving && ((Math.abs(curPoint.x() - lastPoint.x()) >= 2.0f) || (Math.abs(curPoint.y() - lastPoint.y()) >= 2.0f))) {
                isMoving = true;
                objEnum = selGraphics.objectEnumerator();
                while ((curGraphic = (SKTGraphic) objEnum.nextElement()) != null) {
                    curGraphic.startBoundsManipulation();
                }
                _knobsHidden = true;
            }
            if (isMoving) {
                if (snapsToGrid) {
                    boundsOrigin.setX (curPoint.x() - selOriginOffset.x());
                    boundsOrigin.setY (curPoint.y() - selOriginOffset.y());
                    boundsOrigin.setX ( (int) Math.floor (((boundsOrigin.x() / spacing) + 0.5f) * spacing));
                    boundsOrigin.setY ( (int) Math.floor ((int) ((boundsOrigin.y() / spacing) + 0.5f) * spacing));
                    curPoint.setX (boundsOrigin.x() + selOriginOffset.x());
                    curPoint.setY (boundsOrigin.y() + selOriginOffset.y());
                }
                if (!lastPoint.equals(curPoint)) {
                    for (i=0; i<c; i++) {
                        graphic = (SKTGraphic)selGraphics.objectAtIndex(i);
                        invalidateGraphic(graphic);
                        graphic.moveBy(new NSPoint(curPoint.x() - lastPoint.x(), curPoint.y() - lastPoint.y()));
                        invalidateGraphic(graphic);
                        if (echoToRulers) {
                            this.continueEchoingMoveToRulers(new NSRect(curPoint.x() - selOriginOffset.x(), curPoint.y() - selOriginOffset.y(), selBounds.width(), selBounds.height()));
                        }
                        didMove = true;
                    }
                    // Adjust the delta that is used for cascading pastes.  Pasting and then moving the pasted graphic is the way you determine the cascade delta for subsequent pastes.
                    _pasteCascadeDelta.setX (_pasteCascadeDelta.x() + (curPoint.x() - lastPoint.x()));
                    _pasteCascadeDelta.setY (_pasteCascadeDelta.y()  + (curPoint.y() - lastPoint.y()));
                }
                lastPoint = curPoint;
            }
            if (theEvent.type() == NSEvent.LeftMouseUp) {
                break;
            }
        }

        if (echoToRulers) {
            this.stopEchoingMoveToRulers();
        }
        if (isMoving) {
            objEnum = selGraphics.objectEnumerator();
            while ((curGraphic = (SKTGraphic) objEnum.nextElement()) != null) {
                curGraphic.stopBoundsManipulation();
            }
            _knobsHidden = false;

            if (didMove && drawDocument().undoManager() != null) {
                // Only if we really moved.
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Move", "Move", "UndoStrings"));
            }
        }
    }

    public void selectAndTrackMouseWithEvent(NSEvent theEvent) {
        NSPoint curPoint;
        SKTGraphic graphic = null;
        boolean isSelected;
        boolean extending = (((theEvent.modifierFlags() & NSEvent.ShiftKeyMask) != 0) ? true : false);

        curPoint = convertPointFromView(theEvent.locationInWindow(),null);
        graphic = graphicUnderPoint(curPoint);
        isSelected = ((graphic != null) ? graphicIsSelected(graphic) : false);

        if (!extending && !isSelected) {
            clearSelection();
        }

        if (graphic != null) {
            // Add or remove this graphic from selection.
            if (extending) {
                if (isSelected) {
                    deselectGraphic(graphic);
                    isSelected = false;
                } else {
                    selectGraphic(graphic);
                    isSelected = true;
                }
            } else {
                if (isSelected) {
                    int knobHit = graphic.knobUnderPoint(curPoint);
                    if (knobHit != SKTGraphic.NoKnob) {
                        trackKnobOfGraphicWithEvent(knobHit,graphic,theEvent);
                        return;
                    }
                }
                selectGraphic(graphic);
                isSelected = true;
            }
        } else {
            rubberbandSelectWithEvent(theEvent);
            return;
        }

        if (isSelected) {
            moveSelectedGraphicsWithEvent(theEvent);
            return;
        }

        // If we got here then there must be nothing else to do.  Just track until mouseUp:.
        while (true) {
            theEvent = window().nextEventMatchingMask((NSEvent.LeftMouseDraggedMask| NSEvent.LeftMouseUpMask));
            if (theEvent.type() == NSEvent.LeftMouseUp) {
                break;
            }
        }
    }

    public void mouseDown(NSEvent theEvent){
        Class theClass = SKTToolPaletteController.sharedToolPaletteController().currentGraphicClass();
        if (editingGraphic() != null) {
            endEditing();
        }
        if (theEvent.clickCount() > 1) {
            NSPoint curPoint = convertPointFromView(theEvent.locationInWindow(),null);
            SKTGraphic graphic = graphicUnderPoint(curPoint);
            if ((graphic != null) && graphic.isEditable()) {
                startEditingGraphicWithEvent(graphic,theEvent);
                return;
            }
        }
        if (theClass != null) {
            clearSelection();
            createGraphicOfClassWithEvent(theClass,theEvent);
        } else {
            selectAndTrackMouseWithEvent(theEvent);
        }
    }

    public boolean acceptsFirstMouse(NSEvent theEvent) {
        return true;
    }

// SKTImage graphic creation

    public boolean makeNewImageFromPasteboardAtPoint(NSPasteboard pboard, NSPoint point) {
        String type = pboard.availableTypeFromArray(NSImage.imagePasteboardTypes());
        if (type != null) {
            NSImage contents = new NSImage(pboard);
            if (contents != null) {
                SKTImage newImage = new SKTImage ();
                newImage.setBounds(new NSMutableRect(point.x() ,point.y() - contents.size().height(), contents.size().width(), contents.size().height()));
                newImage.setImage(contents);
                drawDocument().insertGraphicAtIndex(newImage,0);
                clearSelection();
                selectGraphic(newImage);
                return true;
            }
        }
        return false;
    }

    public boolean makeNewImageFromContentsOfFileAtPoint(String filename, NSPoint point) {
        String extension = NSPathUtilities.pathExtension (filename);
        if (NSImage.imageFileTypes().containsObject((Object) extension)) {
            NSData data = new NSData ( new File (filename));
            NSImage contents = new NSImage(data);
            if (contents != null) {
                SKTImage newImage = new SKTImage();
                newImage.setBounds(new NSMutableRect(point.x(),point.y(),
                                                     contents.size().width(), contents.size().height()));
                newImage.setImage(contents);
                drawDocument().insertGraphicAtIndex(newImage,0);
                clearSelection();
                selectGraphic(newImage);
                return true;
            }
        }
        return false;
    }

// Dragging

    public int dragOperationForDraggingInfo(NSDraggingInfo  sender) {
        NSPasteboard pboard = sender.draggingPasteboard();
        NSMutableArray pArray = new NSMutableArray();
        pArray.addObject(NSPasteboard.ColorPboardType);
        pArray.addObject(NSPasteboard.FilenamesPboardType);
        String type = pboard.availableTypeFromArray((NSArray) pArray);

        if (type != null) {
            if (type.equalsIgnoreCase(NSPasteboard.ColorPboardType)) {
                NSPoint point = convertPointFromView(sender.draggingLocation(),null);
                if (graphicUnderPoint(point) != null) {
                    return NSDraggingInfo.DragOperationGeneric;
                }
            }
            if (type.equalsIgnoreCase(NSPasteboard.FilenamesPboardType)) {
                return NSDraggingInfo.DragOperationCopy;
            }
        }

        type = pboard.availableTypeFromArray(NSImage.imagePasteboardTypes());
        if (type != null) {
            return NSDraggingInfo.DragOperationCopy;
        }

        return NSDraggingInfo.DragOperationNone;
    }

    public int draggingEntered(NSDraggingInfo  sender) {
        return dragOperationForDraggingInfo(sender);
    }

    public int draggingUpdated(NSDraggingInfo  sender) {
        return dragOperationForDraggingInfo(sender);
    }

    public void draggingExited(NSDraggingInfo  sender) {
        return;
    }

    public boolean prepareForDragOperation(NSDraggingInfo  sender) {
        return true;
    }

    public boolean performDragOperation(NSDraggingInfo  sender) {
        return true;
    }

    public void concludeDragOperation(NSDraggingInfo sender) {
        NSPasteboard pboard = sender.draggingPasteboard();
        NSMutableArray pArray = new NSMutableArray ();
        pArray.addObject(NSPasteboard.ColorPboardType);
        pArray.addObject(NSPasteboard.FilenamesPboardType);
        String type = pboard.availableTypeFromArray((NSArray) pArray);
        NSPoint point = this.convertPointFromView(sender.draggingLocation(), null);
        NSPoint draggedImageLocation = this.convertPointFromView(sender.draggedImageLocation(), null);

        if (type != null) {
            if (type.equalsIgnoreCase(NSPasteboard.ColorPboardType)) {
                SKTGraphic hitGraphic = graphicUnderPoint(point);

                if (hitGraphic != null) {
                    NSColor color = NSColor.colorFromPasteboard(pboard).colorWithAlphaComponent(1.0f);

                    hitGraphic.setFillColor(color);
                    if (drawDocument().undoManager() != null) {
                        drawDocument().undoManager().setActionName(NSBundle.localizedString("Set Fill Color","Set Fill Color", "UndoStrings"));
                    }
                }
            } else if (type.equalsIgnoreCase(NSPasteboard.FilenamesPboardType)) {
                NSArray filenames = (NSArray) pboard.propertyListForType(NSPasteboard.FilenamesPboardType);
                // Handle multiple files (cascade them?)
                if (filenames.count() == 1) {
                    String filename = (String) filenames.objectAtIndex(0);
                    makeNewImageFromContentsOfFileAtPoint(filename,point);
                }
            }
            return;
        }

        makeNewImageFromPasteboardAtPoint(pboard, draggedImageLocation);
    }

// Ruler support

    public void updateRulers() {
        NSScrollView enclosingScrollView = enclosingScrollView();
        if (enclosingScrollView.rulersVisible()) {
            // MF: Eventually, it'd be nice if we added ruler markers for the selection, but for now we just clear them.  By clearing the markers we make sure that no markers from text editing are left over when the editing stops.
            enclosingScrollView.verticalRulerView().setMarkers(null);
            enclosingScrollView.horizontalRulerView().setMarkers(null);
        }
    }

    public boolean rulerViewShouldMoveMarker(NSRulerView ruler, NSRulerMarker marker) {
        return true;
    }

    public float rulerViewWillMoveMarker(NSRulerView ruler, NSRulerMarker marker, float location) {
        return location;
    }

    public void rulerViewDidMoveMarker(NSRulerView ruler, NSRulerMarker marker) {
    }

    public boolean rulerViewShouldRemoveMarker(NSRulerView ruler, NSRulerMarker marker) {
        return false;
    }

    public static final float RULER_MARKER_THICKNESS = 8.0f;
    public static final float RULER_ACCESSORY_THICKNESS = 10.0f;

    public void toggleRuler(Object sender) {
        NSScrollView enclosingScrollView = enclosingScrollView();
        boolean rulersAreVisible = enclosingScrollView.rulersVisible();
        if (rulersAreVisible) {
            enclosingScrollView.setRulersVisible(false);
        } else {
            if (! _initedRulers) {
                NSRulerView ruler = enclosingScrollView.horizontalRulerView();
                ruler.setReservedThicknessForMarkers(RULER_MARKER_THICKNESS);
                ruler.setReservedThicknessForAccessoryView(RULER_ACCESSORY_THICKNESS);
                ruler = enclosingScrollView.verticalRulerView();
                ruler.setReservedThicknessForMarkers(RULER_MARKER_THICKNESS);
                ruler.setReservedThicknessForAccessoryView(RULER_ACCESSORY_THICKNESS);
                _initedRulers = true;
            }
            enclosingScrollView.setRulersVisible(true);
            updateRulers();
        }
    }

// Action methods and other UI entry points

    public void changeColor(Object sender) {
        NSArray selGraphics = selectedGraphics();
        int i, c = selGraphics.count();
        if (c > 0) {
            SKTGraphic curGraphic;
            NSColor color = ((NSColorPanel) sender).color();

            for (i=0; i<c; i++) {
                curGraphic = (SKTGraphic)(selGraphics.objectAtIndex(i));
                curGraphic.setFillColor(color);
                curGraphic.setDrawsFill(true);
            }
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Set Fill Color","Set Fill Color", "UndoStrings"));
            }
        }
    }

    public void selectAll(Object sender) {
        Enumeration objEnum;
        SKTGraphic curGraphic;

        NSArray graphics = drawDocument().graphics();
        objEnum = graphics.objectEnumerator();
        while ((curGraphic = (SKTGraphic) objEnum.nextElement()) != null) {
            selectGraphic(curGraphic);
        }
    }

    public void deselectAll(Object sender) {
        clearSelection();
    }

    public void delete(Object sender) {
        NSArray selCopy = new NSArray (selectedGraphics());
        SKTGraphic curGraphic;
        Enumeration objEnum;
        
        if (selCopy.count() > 0) {
            objEnum = selCopy.objectEnumerator();
            while ((curGraphic = (SKTGraphic) objEnum.nextElement()) != null) {
                drawDocument().removeGraphic(curGraphic);
            }

            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Delete","Delete", "UndoStrings"));
            }
        }
    }

    public void bringToFront(Object sender) {
        NSArray orderedSelection = orderedSelectedGraphics();
        int c = orderedSelection.count();
        if (c > 0) {
            SKTDrawDocument document = this.drawDocument();
            while (c-- > 0) {
                document.moveGraphicToIndex((SKTGraphic)orderedSelection.objectAtIndex(c),0);
            }
            if (document.undoManager() != null) {
                document.undoManager().setActionName(NSBundle.localizedString("Bring To Front","Bring To Front", "UndoStrings"));
            }
        }
    }

    public void sendToBack(Object sender) {
        NSArray orderedSelection = orderedSelectedGraphics();
        int i, c = orderedSelection.count();
        if (c > 0) {
            SKTDrawDocument document = drawDocument();
            int lastIndex = this.graphics().count();
            for (i=0; i<c; i++) {
                document.moveGraphicToIndex((SKTGraphic) orderedSelection.objectAtIndex(i),lastIndex);
            }
            if (document.undoManager() != null) {
                document.undoManager().setActionName(NSBundle.localizedString("Send To Back","Send To Back", "UndoStrings"));
            }
        }
    }

    public void alignLeftEdges (Object sender) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 1) {
            NSRect firstBounds = ((SKTGraphic) selection.objectAtIndex(0)).bounds();
            SKTGraphic curGraphic;
            NSMutableRect curBounds;
            for (i=1; i<c; i++) {
                curGraphic = (SKTGraphic)selection.objectAtIndex(i);
                curBounds = new NSMutableRect(curGraphic.bounds());
                if (curBounds.x() != firstBounds.x()) {
                    curBounds.setX (firstBounds.x());
                    curGraphic.setBounds(curBounds);
                }
            }
            setNeedsDisplay(true);
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Align Left Edges","Align Left Edges", "UndoStrings"));
            }
        }
    }

    public void alignRightEdges(Object sender) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 1) {
            NSRect firstBounds = ((SKTGraphic) selection.objectAtIndex(0)).bounds();
            SKTGraphic curGraphic;
            NSMutableRect curBounds;
            for (i=1; i<c; i++) {
                curGraphic = (SKTGraphic)selection.objectAtIndex(i);
                curBounds = new NSMutableRect(curGraphic.bounds());
                if (curBounds.maxX() != firstBounds.maxX()) {
                    curBounds.setX (firstBounds.maxX() - curBounds.width());
                    curGraphic.setBounds(curBounds);
                }
            }
            setNeedsDisplay(true);
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Align Right Edges","Align Right Edges", "UndoStrings"));
            }
        }
    }

    public void alignTopEdges(Object sender) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 1) {
            NSRect firstBounds = ((SKTGraphic) selection.objectAtIndex(0)).bounds();
            SKTGraphic curGraphic;
            NSMutableRect curBounds;
            for (i=1; i<c; i++) {
                curGraphic = (SKTGraphic)selection.objectAtIndex(i);
                curBounds = new NSMutableRect(curGraphic.bounds());
                if (curBounds.y() != firstBounds.y()) {
                    curBounds.setY(firstBounds.y());
                    curGraphic.setBounds(curBounds);
                }
            }
            setNeedsDisplay(true);
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Align Top Edges","Align Top Edges", "UndoStrings"));
            }
        }
    }

    public void alignBottomEdges(Object sender) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 1) {
            NSRect firstBounds = ((SKTGraphic) selection.objectAtIndex(0)).bounds();
            SKTGraphic curGraphic;
            NSMutableRect curBounds;
            for (i=1; i<c; i++) {
                curGraphic = (SKTGraphic) selection.objectAtIndex(i);
                curBounds = new NSMutableRect(curGraphic.bounds());
                if (curBounds.maxY() != firstBounds.maxY()) {
                    curBounds.setY( firstBounds.maxY() - curBounds.height());
                    curGraphic.setBounds(curBounds);
                }
            }
            setNeedsDisplay(true);
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Align Bottom Edges","Align Bottom Edges", "UndoStrings"));
            }
        }
    }

    public void alignHorizontalCenters(Object sender) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 1) {
            NSRect firstBounds = ((SKTGraphic) selection.objectAtIndex(0)).bounds();
            SKTGraphic curGraphic;
            NSMutableRect curBounds;
            for (i=1; i<c; i++) {
                curGraphic = (SKTGraphic)selection.objectAtIndex(i);
                curBounds = new NSMutableRect(curGraphic.bounds());
                if (curBounds.midX() != firstBounds.midX()) {
                    curBounds.setX (firstBounds.midX() - (curBounds.width() / 2.0f));
                    curGraphic.setBounds(curBounds);
                }
            }
            setNeedsDisplay(true);
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Align Horizontal Centers","Align Horizontal Centers", "UndoStrings"));
            }
        }
    }

    public void alignVerticalCenters(Object sender) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 1) {
            NSRect firstBounds = ((SKTGraphic) selection.objectAtIndex(0)).bounds();
            SKTGraphic curGraphic;
            NSMutableRect curBounds;
            for (i=1; i<c; i++) {
                curGraphic = (SKTGraphic) selection.objectAtIndex(i);
                curBounds = new NSMutableRect(curGraphic.bounds());
                if (curBounds.midY() != firstBounds.midY()) {
                    curBounds.setY (firstBounds.midY() - (curBounds.height() / 2.0f));
                    curGraphic.setBounds(curBounds);
                }
            }
            setNeedsDisplay(true);
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Align Vertical Centers","Align Vertical Centers", "UndoStrings"));
            }
        }
    }

    public void makeSameWidth(Object sender) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 1) {
            NSRect firstBounds = ((SKTGraphic) selection.objectAtIndex(0)).bounds();
            SKTGraphic curGraphic;
            NSMutableRect curBounds;
            for (i=1; i<c; i++) {
                curGraphic = (SKTGraphic) selection.objectAtIndex(i);
                curBounds = new NSMutableRect(curGraphic.bounds());
                if (curBounds.width() != firstBounds.width()) {
                    curBounds.setWidth (firstBounds.width());
                    curGraphic.setBounds(curBounds);
                }
            }
            setNeedsDisplay(true);
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Make Same Width", "Make Same Width", "UndoStrings"));
            }
        }
    }

    public void makeSameHeight(Object sender) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 1) {
            NSRect firstBounds = ((SKTGraphic)selection.objectAtIndex(0)).bounds();
            SKTGraphic curGraphic;
            NSMutableRect curBounds;
            for (i=1; i<c; i++) {
                curGraphic = (SKTGraphic) selection.objectAtIndex(i);
                curBounds = new NSMutableRect(curGraphic.bounds());
                if (curBounds.height() != firstBounds.height()) {
                    curBounds.setHeight( firstBounds.height());
                    curGraphic.setBounds(curBounds);
                }
            }
            setNeedsDisplay(true);
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Make Same Width","Make Same Width", "UndoStrings"));
            }
        }
    }

    public void makeNaturalSize(Object sender) {
        int i;
        NSArray selection = selectedGraphics();
        if (selection.count() > 0) {
            for (i = 0; i < selection.count(); i++) {
                ((SKTGraphic)selection.objectAtIndex(i)).makeNaturalSize();
            }
        }
        setNeedsDisplay(true);
        if (drawDocument().undoManager() != null) {
            drawDocument().undoManager().setActionName(NSBundle.localizedString("Make Natural Size","Make Natural Size", "UndoStrings"));
        }
    }

    public void snapsToGridMenuAction(NSMenuItem sender) {
        setSnapsToGrid(((sender.state() == 1) ? false : true));
        // Menu item will get state fixed up in validateMenuItem:
        SKTGridPanelController.sharedGridPanelController().updatePanel();
    }

    public void showsGridMenuAction(NSMenuItem sender) {
        setShowsGrid((sender.state() == 1) ? false : true);
        // Menu item will get state fixed up in validateMenuItem:
        SKTGridPanelController.sharedGridPanelController().updatePanel();
    }

    public void gridSelectedGraphicsAction(Object sender) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 0) {
            SKTGraphic curGraphic;
            NSMutableRect curBounds;
            NSMutablePoint curMaxPoint;
            float spacing = gridSpacing();

            for (i=0; i<c; i++) {
                curGraphic = (SKTGraphic) selection.objectAtIndex(i);
                curBounds = new NSMutableRect(curGraphic.bounds());
                curMaxPoint = new NSMutablePoint(curBounds.maxX(), curBounds.maxY());
                curBounds.setX ( (int) Math.floor ((curBounds.x() / spacing) + 0.5f * spacing));
                curBounds.setY ( (int) Math.floor ((int) (curBounds.y() / spacing) + 0.5f * spacing));
                curMaxPoint.setX ( (int) Math.floor ((int) (curMaxPoint.x() / spacing) + 0.5f * spacing));
                curMaxPoint.setY ( (int) Math.floor ((int) (curMaxPoint.y() / spacing) + 0.5f * spacing));
                curBounds.setWidth (curMaxPoint.x() - curBounds.x());
                curBounds.setHeight (curMaxPoint.y() - curBounds.y());
                curGraphic.setBounds(curBounds);
            }
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Grid Selected Graphics","Grid Selected Graphics", "UndoStrings"));
            }
        }
    }

    public boolean validateMenuItem(NSMenuItem item) {
        NSSelector action = item.action();
        Class arrClass [] = {Object.class};

        // We use startsWith here because there seems to be a bit of confusion about colons in NSSelector.
        if (action.name().startsWith("snapsToGridMenuAction")) {
            item.setState((snapsToGrid() ? 1 : 0));	
            return true;
        } else if (action.name().startsWith("showsGridMenuAction")) {
            item.setState((showsGrid() ? 1 : 0));		
            return true;
        } else if (action.name().startsWith("makeNaturalSize")) {
            // Return YES if we have at least one selected graphic that has a natural size.
            NSArray selectedGraphics = this.selectedGraphics();
            int i, c = selectedGraphics.count();
            if (c > 0) {
                for (i=0; i<c; i++) {
                    if (((SKTGraphic) selectedGraphics.objectAtIndex(i)).hasNaturalSize()) {
                    				return true;
                    }
                }
            }
            return false;
        } else if (action.name().startsWith("gridSelectedGraphicsAction") || action.name().startsWith("delete") || action.name().startsWith("bringToFront") || action.name().startsWith("sendToBack") || action.name().startsWith("copy") || action.name().startsWith("cut")) {
            // MF:!!! - NSMenu does not recognise SKTGraphicView (validator) as implementing a copy method and therefore does not enable the copy menu item.
            // These only apply if there is a selection
            return ((selectedGraphics().count() > 0) ? true : false);
        } else if (action.name().startsWith("alignLeftEdges") || action.name().startsWith("alignRightEdges") || action.name().startsWith("alignTopEdges") || action.name().startsWith("alignBottomEdges") || action.name().startsWith("alignHorizontalCenters") || action.name().startsWith("alignVerticalCenters")|| action.name().startsWith("alignTextBaselines")|| action.name().startsWith("makeSameWidth")|| action.name().startsWith("makeSameHeight")) {
            // These only apply to multiple selection
            return ((selectedGraphics().count() > 1) ? true : false);
        } else {
            return true;
        }
    }

    public void copy(Object sender) {
        NSArray orderedSelection = orderedSelectedGraphics();
        if (orderedSelection.count() > 0) {
            SKTDrawDocument document = drawDocument();
            NSPasteboard pboard = NSPasteboard.generalPasteboard();
            Object arrObjects [] = {SKTDrawDocument.DrawDocumentType, NSPasteboard.TIFFPboardType, NSPasteboard.PDFPboardType};
            pboard.declareTypes(new NSArray(arrObjects), null);

            pboard.setDataForType(document.drawDocumentDataForGraphics(orderedSelection), SKTDrawDocument.DrawDocumentType);
            pboard.setDataForType(document.TIFFRepresentationForGraphics(orderedSelection), NSPasteboard.TIFFPboardType);
            pboard.setDataForType(document.PDFRepresentationForGraphics(orderedSelection), NSPasteboard.PDFPboardType);
            _pasteboardChangeCount = pboard.changeCount();
            _pasteCascadeNumber = 1;
            _pasteCascadeDelta = new NSMutablePoint(DefaultPasteCascadeDelta, DefaultPasteCascadeDelta);
        }
    }

    public void cut(Object sender) {
        copy(sender);
        delete(sender);
        if (drawDocument().undoManager() != null) {
            drawDocument().undoManager().setActionName(NSBundle.localizedString("Cut","Cut", "UndoStrings"));
        }
    }

    public void paste(Object sender) {
        NSPasteboard pboard = NSPasteboard.generalPasteboard();
        NSMutableArray pArray = new NSMutableArray();
        pArray.addObject(SKTDrawDocument.DrawDocumentType);
        pArray.addObject(NSPasteboard.FilenamesPboardType);
        String type = pboard.availableTypeFromArray((NSArray) pArray);

        if (type != null) {
            if (type.equals(SKTDrawDocument.DrawDocumentType)) {
                NSArray array = null;
                SKTDrawDocument document = this.drawDocument();
                NSData data = pboard.dataForType(SKTDrawDocument.DrawDocumentType);
                
                NSDictionary docDict = document.drawDocumentDictionaryFromData(data);

                array = document.graphicsFromDrawDocumentDictionary(docDict);

                int i = array.count();
                int currentChangeCount = pboard.changeCount();

                if (_pasteboardChangeCount != currentChangeCount) {
                    _pasteboardChangeCount = currentChangeCount;
                    _pasteCascadeNumber = 0;
                    _pasteCascadeDelta = new NSMutablePoint(DefaultPasteCascadeDelta, DefaultPasteCascadeDelta);
                }

                if (i > 0) {
                    SKTGraphic curGraphic;
                    NSPoint savedPasteCascadeDelta = _pasteCascadeDelta;

                    this.clearSelection();
                    while (i-- > 0) {
                        curGraphic = (SKTGraphic)array.objectAtIndex(i);
                        if (_pasteCascadeNumber > 0) {
                            curGraphic.moveBy( new NSPoint(_pasteCascadeNumber * savedPasteCascadeDelta.x(), _pasteCascadeNumber * savedPasteCascadeDelta.y()));
                        }
                        document.insertGraphicAtIndex(curGraphic,0);
                        this.selectGraphic(curGraphic);
                    }
                    _pasteCascadeNumber++;
                    _pasteCascadeDelta = new NSMutablePoint (savedPasteCascadeDelta);
                    if (drawDocument().undoManager() != null) {
                        drawDocument().undoManager().setActionName(NSBundle.localizedString("Paste","Paste", "UndoStrings"));
                    }
                }
            } else if (type.equalsIgnoreCase(NSPasteboard.FilenamesPboardType)) {
                NSArray filenames = (NSArray) pboard.propertyListForType(NSPasteboard.FilenamesPboardType);
                if (filenames.count() == 1) {
                    String filename = (String)filenames.objectAtIndex(0);
                    if (makeNewImageFromContentsOfFileAtPoint(filename, new NSPoint(50f,50f)) && drawDocument().undoManager() != null) {
                        drawDocument().undoManager().setActionName(NSBundle.localizedString("Paste","Paste", "UndoStrings"));
                    }
                }
            }
            return;
        }

        if (makeNewImageFromPasteboardAtPoint(pboard,new NSPoint(50f,50f)) && drawDocument().undoManager() != null) {
            drawDocument().undoManager().setActionName(NSBundle.localizedString("Paste","Paste", "UndoStrings"));
        }
    }


// Keyboard commands

    public void keyDown(NSEvent event) {
        // Pass on to the key binding manager.  This will end up calling insertText: or some command selector.
        interpretKeyEvents(new NSArray(event));
    }

    public void insertText(String str) {
        NSApplication.beep();
    }

    public void hideKnobsMomentarily() {
        if (_unhideKnobsTimer != null) {
            _unhideKnobsTimer.invalidate();
            _unhideKnobsTimer = null;
        }
        Class args[] = {NSTimer.class};
        _unhideKnobsTimer = new NSTimer(0.5, this, new NSSelector("unhideKnobs", args), null, false);
        NSRunLoop.currentRunLoop().addTimerForMode(_unhideKnobsTimer, NSRunLoop.DefaultRunLoopMode);
        _knobsHidden = true;
        this.invalidateGraphics(this.selectedGraphics());
    }

    public void unhideKnobs(NSTimer timer) {
        _knobsHidden = false;
        this.invalidateGraphics(this.selectedGraphics());
        _unhideKnobsTimer.invalidate();
        _unhideKnobsTimer = null;
    }

    public void moveSelectedGraphicsByPoint(NSPoint delta) {
        NSArray selection = selectedGraphics();
        int i, c = selection.count();
        if (c > 0) {
            this.hideKnobsMomentarily();
            for (i=0; i<c; i++) {
                ((SKTGraphic) selection.objectAtIndex(i)).moveBy(delta);
            }
            if (drawDocument().undoManager() != null) {
                drawDocument().undoManager().setActionName(NSBundle.localizedString("Nudge","Nudge", "UndoStrings"));
            }
        }
    }

    public void moveLeft(Object sender) {
        moveSelectedGraphicsByPoint(new NSPoint(-1.0f,0.0f));
    }

    public void moveRight(Object sender) {
        moveSelectedGraphicsByPoint(new NSPoint(1.0f,0.0f));
    }

    public void moveUp(Object sender) {
        moveSelectedGraphicsByPoint(new NSPoint(0.0f,-1.0f));
    }

    public void moveDown(Object sender) {
        moveSelectedGraphicsByPoint(new NSPoint(0.0f,1.0f));
    }

    public void moveForwardAndModifySelection(Object sender) {
        // We will use this to move by the grid spacing.
        moveSelectedGraphicsByPoint(new NSPoint(this.gridSpacing(), 0.0f));
    }

    public void moveBackwardAndModifySelection(Object sender) {
        // We will use this to move by the grid spacing.
        moveSelectedGraphicsByPoint(new NSPoint(-this.gridSpacing(), 0.0f));
    }

    public void moveUpAndModifySelection(Object sender) {
        // We will use this to move by the grid spacing.
        moveSelectedGraphicsByPoint(new NSPoint(0.0f,-this.gridSpacing()));
    }

    public void moveDownAndModifySelection(Object sender) {
        // We will use this to move by the grid spacing.
        moveSelectedGraphicsByPoint(new NSPoint(0.0f,this.gridSpacing()));
    }

    public void deleteForward(Object sender) {
        delete(sender);
    }

    public void deleteBackward(Object sender) {
        delete(sender);
    }

// Grid settings

    public boolean snapsToGrid() {
        return _snapsToGrid;
    }

    public void setSnapsToGrid(boolean flag) {
        _snapsToGrid = flag;
    }

    public boolean showsGrid() {
        return _showsGrid;
    }

    public void setShowsGrid(boolean flag) {
        if (_showsGrid != flag) {
            _showsGrid = flag;
            setNeedsDisplay(true);
        }
    }

    public float gridSpacing() {
        return _gridSpacing;
    }

    public void setGridSpacing(float spacing) {
        if (_gridSpacing != spacing) {
            _gridSpacing = spacing;
            setNeedsDisplay(true);
        }
    }

    public NSColor gridColor() {
        return ((_gridColor != null) ? _gridColor : NSColor.lightGrayColor());
    }

    public void setGridColor(NSColor color) {
        if (_gridColor != color) {
            _gridColor = color;
            setNeedsDisplay(true);
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
