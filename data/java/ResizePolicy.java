/**
 * @(#)ResizePolicy.java	15.2 03/05/20
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


import java.awt.Point;
import javax.swing.JSplitPane;

/** The class to define the resizing policy of an <code>ImagePane</code>.
 *   When the size of an <code>ImagePane</code> is changed, the position 
 *   of the image needs to be adjusted adequately.  A policy is defined 
 *   to indicate how to allign the image region displayed in the 
 *   <code>ImagePane</code> with the pane: for example, keep a fixed 
 *   pixel at the center of the <code>ImagePane</code>.  The supported 
 *   policies are: center-fixed, leftup-corner fixed, rightup-corner 
 *   fixed, leftbottom-fixed and rightbottom-fixed.
 *
 */



public class ResizePolicy {

    /** The defined resizing policies */


    public static int POLICY_CENTER = 0;
    public static int POLICY_LEFTUP = 1;
    public static int POLICY_RIGHTUP = 2;
    public static int POLICY_LEFTBOTTOM = 3;
    public static int POLICY_RIGHTBOTTOM = 4;

    /** Cache the policy serial number */


    private int policy;

    /** The constructor. */


    public ResizePolicy(int policy) {
	if (policy > 4)
	    throw new IllegalArgumentException("Invalid policy");

	this.policy = policy;
    }

    /** Return the policy. */


    public int getPolicy() {
	return policy;
    }

    /**
     * Compute the origin from the reference point and the size of the
     *  image display panel.
     */


    public Point computeOrigin(int referenceX, int referenceY, int w, int h) {
	if (policy == POLICY_CENTER)
	    return new Point(referenceX - w / 2, referenceY - h / 2);
	else if (policy == POLICY_LEFTUP)
	    return new Point(referenceX, referenceY);
	else if (policy == POLICY_RIGHTUP)
	    return new Point(referenceX - w, referenceY);
	else if (policy == POLICY_LEFTBOTTOM)
	    return new Point(referenceX, referenceY - h);
	else if (policy == POLICY_RIGHTBOTTOM)
	    return new Point(referenceX - w, referenceY - h);
	return new Point(referenceX, referenceY);
    }

    /**
     * Compute the reference based on the image size and the image display
     *  panel size.
     */


    public Point computeReference(int imageWidth, int imageHeight, int w, int h) {
	Point reference = new Point(imageWidth/2, imageHeight/2);

	if (policy == POLICY_LEFTUP)
	    reference.translate(-w / 2, -h / 2);
	else if (policy == POLICY_RIGHTUP)
	    reference.translate(w /2, -h / 2);
	else if (policy == POLICY_LEFTBOTTOM)
	    reference.translate(-w / 2, h /2);
	else if (policy == POLICY_RIGHTBOTTOM)
	    reference.translate(w / 2, h / 2);

	return reference;
    }

    /** Return the shift based on the delta values. */


    public Point shiftReference(int dx, int dy) {
	if (policy == POLICY_LEFTUP)
	    return new Point(dx, dy);
	if (policy == POLICY_RIGHTUP)
	    return new Point(-dx, dy);
	if (policy == POLICY_LEFTBOTTOM)
	    return new Point(dx, -dy);
	if (policy == POLICY_RIGHTBOTTOM)
	    return new Point(-dx, -dy);
	return new Point(dx/2, dy/2);
    }

    /** Move the reference. */


    public Point shiftReference(int minx, int miny,
				int maxx, int maxy,
				int width, int height) {

	if (policy == POLICY_LEFTUP)
	    return new Point(minx, miny);

	if (policy == POLICY_RIGHTUP)
	    return new Point(maxx - width, miny);

	if (policy == POLICY_LEFTBOTTOM)
	    return new Point(minx, maxy - height);

	if (policy == POLICY_RIGHTBOTTOM)
	    return new Point(maxx - width, maxy - height);

	return new Point((width - minx - maxx) / 2,
			 (height - miny - maxy) / 2);
    }
}
