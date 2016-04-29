/**
 * @(#)SynchronizedSplitPane.java	15.3 03/05/20
 *
 * Copyright (c) 2003 Sun Microsystems, Inc.
 * All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * -Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * -Redistribution in binary form must reproduct the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of Sun Microsystems, Inc. or the names of contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * This software is provided "AS IS," without a warranty of any kind. ALL
 * EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING
 * ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN AND ITS LICENSORS
 * SHALL NOT BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF
 * USING, MODIFYING OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO
 * EVENT WILL SUN OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT
 * OR DATA, OR FOR DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR
 * PUNITIVE DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF
 * LIABILITY, ARISING OUT OF THE USE OF OR INABILITY TO USE SOFTWARE, EVEN
 * IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * You acknowledge that Software is not designed,licensed or intended for
 * use in the design, construction, operation or maintenance of any nuclear
 * facility.
 */


 
import java.awt.Component;
import java.beans.PropertyChangeListener; 
import java.beans.PropertyChangeEvent; 
import javax.swing.JButton;
import javax.swing.JSplitPane;

/** The class to create synchronized split pane.
 *
 */



public class SynchronizedSplitPane extends JSplitPane 
				   implements PropertyChangeListener {

    private int synchronizedDividerSize = 4;

    public SynchronizedSplitPane(int newOrientation) {
	super(newOrientation, true, new JButton("Left button"),
	      new JButton("Right button"));
	setDividerSize(synchronizedDividerSize);
    }

    public SynchronizedSplitPane(int newOrientation,
				 Component newLeftComponent,
				 Component newRightComponent){
	super(newOrientation, true, newLeftComponent, newRightComponent);
	setDividerSize(synchronizedDividerSize);
    }

    public void addSynchronized(SynchronizedSplitPane c) {
	c.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, this);
	this.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, c);
    }

    public void propertyChange(PropertyChangeEvent event) {
	Object source = event.getSource();
	
	if (source instanceof JSplitPane) {
	    JSplitPane other = (JSplitPane)source;
	    if (other.getOrientation() != this.getOrientation())
		throw new IllegalArgumentException("Should have same orientation");
	    
	    if (event.getPropertyName().equals(DIVIDER_LOCATION_PROPERTY)) {
		Object newValue = event.getNewValue();
		if (newValue instanceof Double) {
		    double newLocation = ((Double)newValue).doubleValue();
		    synthronizeDividerLocation(newLocation);
		} else if (newValue instanceof Integer) {
		    int newLocation = ((Integer)newValue).intValue();
		    synthronizeDividerLocation(newLocation);
		}
	    }
	}
    }

    private void synthronizeDividerLocation(double proportionalLocation) {
	if (proportionalLocation < 0.0 ||
	    proportionalLocation > 1.0) {
	    throw new IllegalArgumentException("proportional location must " +
	                                       "be between 0.0 and 1.0.");
	}

	if (getOrientation() == VERTICAL_SPLIT) {
	    synthronizeDividerLocation((int)((double)(getHeight() - getDividerSize()) *
				       proportionalLocation));
	} else {
	    synthronizeDividerLocation((int)((double)(getWidth() - getDividerSize()) *
				       proportionalLocation));
	}
    }

    private void synthronizeDividerLocation(int location) {
	if (getLastDividerLocation() == location)
	    return;
	setDividerLocation(location);
    }
}

