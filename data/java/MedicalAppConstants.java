/**
 * @(#)MedicalAppConstants.java	15.2 03/05/20
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


/**
 * An interface contains the public constants used in this application.
 *
 */



interface MedicalAppConstants {
    // Currently the zoom slider is not linear, so the position for
    // zoom factor 1.0 is defined in the property file.
    int nozoom =
	(new Integer(JaiI18N.getString("NoZoomTickPosition")).intValue() -1);

    // The command name and/or message names
    String setLayoutCommand	    = "SetLayout";
    String allViewsCommand	    = "AllViews";
    String currentViewCommand	    = "CurrentView";
    String zoomCommand		    = "zoom";
    String rotationCommand	    = "rotation";
    String speedCommand		    = "cinespeed";
    String startCommand		    = "cinestart";
    String stopCommand		    = "cinestop";
    String windowCommand	    = "window";
    String levelCommand		    = "level";
    String annotationCommand	    = "anotation";
    String measurementCommand	    = "measurement";
    String statisticsCommand	    = "statistics";
    String histogramCommand	    = "histogram";
    String paramSync		    = "paramSync";

    // The default values for window/level.
    int smallestWindow		    = 0;
    int largestWindow		    = 4096;
    int defaultWindow		    = 700;
    int smallestLevel		    = 0;
    int largestLevel		    = 4096;
    int defaultLevel		    = 300;
}
