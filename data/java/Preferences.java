/*
        Preferences.java
        Copyright (c) 1995-2005 by Apple Computer, Inc., all rights reserved.
        Author: Yves Arrouye
        Java translation of original code by Ali Ozer

        Preferences controller for Edit... To add new defaults search for one
        of the existing keys. Some keys have UI, others don't; 
        use one similar to the one you're adding.

        displayedValues is a mirror of the UI. These are committed by copying
        these values to curValues.

        This module allows for UI where there is or there isn't an OK button. 
        If you wish to have an OK button, connect OK to ok:,
        Revert to revert:, and don't call commitDisplayedValues from the 
        various action messages. 
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

import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;

public class Preferences {
    public static final String RichTextFont = "RichTextFont";
    public static final String PlainTextFont = "PlainTextFont";
    public static final String DeleteBackup = "DeleteBackup";
    public static final String SaveFilesWritable = "SaveFilesWritable";
    public static final String OverrideReadOnlyFiles = "OverrideReadOnlyFiles";
    public static final String RichText = "RichText";
    public static final String ShowPageBreaks = "ShowPageBreaks";
    public static final String WindowWidth = "WidthInChars";
    public static final String WindowHeight = "HeightInChars";
    public static final String PlainTextEncodingForRead = "PlainTextEncoding";
    public static final String PlainTextEncodingForWrite = "PlainTextEncodingForWrite";
    public static final String TabWidth = "TabWidth";
    public static final String ForegroundLayoutToIndex = "ForegroundLayoutToIndex";
    public static final String OpenPanelFollowsMainWindow = "OpenPanelFollowsMainWindow";
    public static final String IgnoreRichText = "IgnoreRichText";
    public static final String IgnoreHTML = "IgnoreHTML";

    private NSTextField richTextFontNameField = null;
    private NSTextField plainTextFontNameField;
    private NSButton deleteBackupButton;
    private NSButton saveFilesWritableButton;
    private NSButton overrideReadOnlyFilesButton;
    private NSMatrix richTextMatrix;
    private NSButton showPageBreaksButton;
    private NSTextField windowWidthField;
    private NSTextField windowHeightField;
    private NSPopUpButton plainTextEncodingForReadPopup;
    private NSPopUpButton plainTextEncodingForWritePopup;
    private NSTextField tabWidthField;
    private NSButton ignoreRichTextButton;
    private NSButton ignoreHTMLButton;

    static private NSDictionary defaultValues = null;
    static private Preferences sharedInstance = null;
    
    private NSDictionary curValues;
    private NSMutableDictionary displayedValues;

    private static NSDictionary defaultValues() {
        if (defaultValues == null) {
            NSMutableDictionary dict = new NSMutableDictionary();
            dict.setObjectForKey(new Integer(0), IgnoreRichText);
            dict.setObjectForKey(new Integer(0), IgnoreHTML);
            dict.setObjectForKey(new Integer(1), DeleteBackup);
            dict.setObjectForKey(new Integer(0), SaveFilesWritable);
            dict.setObjectForKey(new Integer(1), OverrideReadOnlyFiles);
            dict.setObjectForKey(new Integer(1), RichText);
            dict.setObjectForKey(new Integer(0), ShowPageBreaks);
            dict.setObjectForKey(new Integer(0), OpenPanelFollowsMainWindow);
            dict.setObjectForKey(new Integer(80), WindowWidth);
            dict.setObjectForKey(new Integer(25), WindowHeight);
            dict.setObjectForKey(new Integer(Document.UnknownStringEncoding), PlainTextEncodingForRead);
            dict.setObjectForKey(new Integer(Document.UnknownStringEncoding), PlainTextEncodingForWrite);
            dict.setObjectForKey(new Integer(8), TabWidth);
            dict.setObjectForKey(new Integer(100000), ForegroundLayoutToIndex);
            dict.setObjectForKey(NSFont.userFixedPitchFontOfSize(0f), PlainTextFont);
            dict.setObjectForKey(NSFont.userFontOfSize(0f), RichTextFont);
            defaultValues = new NSDictionary(dict);
        }
        return defaultValues;
    }

    public static Object objectForKey(Object key) {
        return sharedInstance().preferences().objectForKey(key);
    }

    public static int intValueForKey (Object key) {
        return ((Number) objectForKey(key)).intValue();
    }

    public static boolean booleanValueForKey (Object key) {
        return 0 == ((Number) objectForKey(key)).intValue() ? false : true;
    }

    public static void saveDefaults() {
        if (sharedInstance != null) {
            Preferences.savePreferencesToDefaults(sharedInstance.preferences());
        }
    }

    public static Preferences sharedInstance() {
        if (sharedInstance == null) {
            new Preferences();
        }
        
        return sharedInstance;
    }

    public Preferences() {
        Preferences.sharedInstance = this;
        curValues = (NSDictionary) Preferences.preferencesFromDefaults();
        discardDisplayedValues();
    }

    public NSDictionary preferences() {
        return curValues;
    }

    public void showPanel(Object sender) {
        if (richTextFontNameField == null) {
            NSApplication.loadNibNamed("Preferences", this);

            richTextFontNameField.window().setExcludedFromWindowsMenu(true);
            richTextFontNameField.window().setMenu(null);
            updateUserInterface();
            richTextFontNameField.window().center();
        }

        richTextFontNameField.window().makeKeyAndOrderFront(null);
    }

    private static void showFontInField(NSFont font, NSTextField field) {
        field.setStringValue(font != null ? font.fontName() + " " + font.pointSize() : "");
    }
    
    public void updateUserInterface() {
        showFontInField((NSFont) displayedValues.objectForKey(RichTextFont), richTextFontNameField);
        showFontInField((NSFont) displayedValues.objectForKey(PlainTextFont), plainTextFontNameField);
        deleteBackupButton.setState(((Integer) displayedValues.objectForKey(DeleteBackup)).intValue());
        saveFilesWritableButton.setState(((Integer) displayedValues.objectForKey(SaveFilesWritable)).intValue());
        overrideReadOnlyFilesButton.setState(((Integer) displayedValues.objectForKey(OverrideReadOnlyFiles)).intValue());
        richTextMatrix.selectCellWithTag(((Integer) displayedValues.objectForKey(RichText)).intValue());
        showPageBreaksButton.setState(((Integer) displayedValues.objectForKey(ShowPageBreaks)).intValue());
        ignoreRichTextButton.setState(((Integer) displayedValues.objectForKey(IgnoreRichText)).intValue());
        ignoreHTMLButton.setState(((Integer) displayedValues.objectForKey(IgnoreHTML)).intValue());

        windowWidthField.setIntValue(((Integer) displayedValues.objectForKey(WindowWidth)).intValue());
        windowHeightField.setIntValue(((Integer) displayedValues.objectForKey(WindowHeight)).intValue());

        Document.setupEncodingPopUpButton(plainTextEncodingForReadPopup, ((Integer) displayedValues.objectForKey(PlainTextEncodingForRead)).intValue(), true);
        Document.setupEncodingPopUpButton(plainTextEncodingForWritePopup, ((Integer) displayedValues.objectForKey(PlainTextEncodingForWrite)).intValue(), true);
    }

    public void miscChanged(Object sender) {
        displayedValues.setObjectForKey(new Integer(deleteBackupButton.state()), DeleteBackup);
        displayedValues.setObjectForKey(new Integer(((NSCell) richTextMatrix.selectedCell()).tag()), RichText);
        displayedValues.setObjectForKey(new Integer(saveFilesWritableButton.state()), SaveFilesWritable);
        displayedValues.setObjectForKey(new Integer(overrideReadOnlyFilesButton.state()), OverrideReadOnlyFiles);
        displayedValues.setObjectForKey(new Integer(showPageBreaksButton.state()), ShowPageBreaks);
        displayedValues.setObjectForKey(new Integer(plainTextEncodingForReadPopup.selectedItem().tag()), PlainTextEncodingForRead);
        displayedValues.setObjectForKey(new Integer(plainTextEncodingForWritePopup.selectedItem().tag()), PlainTextEncodingForWrite);
        displayedValues.setObjectForKey(new Integer(ignoreRichTextButton.state()), IgnoreRichText);
        displayedValues.setObjectForKey(new Integer(ignoreHTMLButton.state()), IgnoreHTML);

        int anInt;

        anInt = windowWidthField.intValue();

        if (anInt < 1 || anInt > 10000) {
            Integer anIntDefault = (Integer) displayedValues.objectForKey(WindowWidth);

            if (anIntDefault != null) {
                anInt =  anIntDefault.intValue();
            }

            if (anInt < 1 || anInt > 10000) {
                anInt = ((Integer) defaultValues().objectForKey(WindowWidth)).intValue();
            }

            windowWidthField.setIntValue(anInt);
        } else {
            displayedValues.setObjectForKey(new Integer(anInt), WindowWidth);
        }

        anInt = windowHeightField.intValue();

        if (anInt < 1 || anInt > 10000) {
            Integer anIntDefault = (Integer) displayedValues.objectForKey(WindowHeight);

            if (anIntDefault != null) {
                anInt =  anIntDefault.intValue();
            }

            if (anInt < 1 || anInt > 10000) {
                anInt = ((Integer) defaultValues().objectForKey(WindowHeight)).intValue();
            }

            windowHeightField.setIntValue(anInt);
        } else {
            displayedValues.setObjectForKey(new Integer(anInt), WindowHeight);
        }

        commitDisplayedValues();
    }

    private void commitDisplayedValues() {
        curValues = new NSDictionary(displayedValues);
    }
    
    private void discardDisplayedValues() {
        displayedValues = new NSMutableDictionary(curValues);
        if (richTextFontNameField != null) {	// We're displaying some interface
            updateUserInterface();
        }
    }

    public void ok(Object sender) {
        commitDisplayedValues();
    }

    public void revertToDefault(Object sender) {
        curValues = defaultValues();
        discardDisplayedValues();
    }

    public void revert(Object sender) {
        discardDisplayedValues();
    }

    private static void getBoolDefault(NSUserDefaults defaults, String name, NSMutableDictionary dict) {
        Object obj = defaults.objectForKey(name);
        if (obj != null) {
            dict.setObjectForKey(new Integer(defaults.booleanForKey(name) ? 1 : 0), name);
        } else {
            dict.setObjectForKey(defaultValues().objectForKey(name), name);
        }
    }
    
    private static void getIntDefault(NSUserDefaults defaults, String name, NSMutableDictionary dict) {
        Object obj = defaults.objectForKey(name);
        if (obj != null) {
	    dict.setObjectForKey(new Integer(defaults.integerForKey(name)), name);
        } else {
            dict.setObjectForKey(defaultValues().objectForKey(name), name);
        }
    }

    private static NSDictionary preferencesFromDefaults() {
        NSUserDefaults defaults = NSUserDefaults.standardUserDefaults();
        NSMutableDictionary dict = new NSMutableDictionary();

        getBoolDefault(defaults, RichText, dict);
        getBoolDefault(defaults, DeleteBackup, dict);
        getBoolDefault(defaults, ShowPageBreaks, dict);
        getBoolDefault(defaults, SaveFilesWritable, dict);
        getBoolDefault(defaults, OverrideReadOnlyFiles, dict);
        getBoolDefault(defaults, OpenPanelFollowsMainWindow, dict);
        getIntDefault(defaults, WindowWidth, dict);
        getIntDefault(defaults, WindowHeight, dict);
        getIntDefault(defaults, PlainTextEncodingForRead, dict);
        getIntDefault(defaults, PlainTextEncodingForWrite, dict);
        getIntDefault(defaults, TabWidth, dict);
        getIntDefault(defaults, ForegroundLayoutToIndex, dict);
        getBoolDefault(defaults, IgnoreRichText, dict);
        getBoolDefault(defaults, IgnoreHTML, dict);
        dict.setObjectForKey(NSFont.userFixedPitchFontOfSize(0f), PlainTextFont);
        dict.setObjectForKey(NSFont.userFontOfSize(0f), RichTextFont);

        return dict;
    }

    private static void setBoolDefault(NSUserDefaults defaults, String name, NSDictionary dict) {
        if (defaultValues().objectForKey(name).equals(dict.objectForKey(name))) {
            defaults.removeObjectForKey(name);
        } else {
            defaults.setBooleanForKey(((Integer) dict.objectForKey(name)).intValue() != 0, name);
        }
    }

    private static void setIntDefault(NSUserDefaults defaults, String name, NSDictionary dict) {
        if (defaultValues().objectForKey(name).equals(dict.objectForKey(name))) {
            defaults.removeObjectForKey(name);
        } else {
            defaults.setIntegerForKey(((Integer) dict.objectForKey(name)).intValue(), name);
        }
    }

    private static void savePreferencesToDefaults(NSDictionary dict) {
        NSUserDefaults defaults = NSUserDefaults.standardUserDefaults();

        setBoolDefault(defaults, RichText, dict);
        setBoolDefault(defaults, DeleteBackup, dict);
        setBoolDefault(defaults, ShowPageBreaks, dict);
        setBoolDefault(defaults, SaveFilesWritable, dict);
        setBoolDefault(defaults, OverrideReadOnlyFiles, dict);
        setBoolDefault(defaults, OpenPanelFollowsMainWindow, dict);
        setIntDefault(defaults, WindowWidth, dict);
        setIntDefault(defaults, WindowHeight, dict);
        setIntDefault(defaults, PlainTextEncodingForRead, dict);
        setIntDefault(defaults, PlainTextEncodingForWrite, dict);
        setIntDefault(defaults, TabWidth, dict);
        setIntDefault(defaults, ForegroundLayoutToIndex, dict);
        setBoolDefault(defaults, IgnoreRichText, dict);
        setBoolDefault(defaults, IgnoreHTML, dict);

        if (!dict.objectForKey(RichTextFont).equals(NSFont.userFontOfSize(0f))) {
            NSFont.setUserFont((NSFont) dict.objectForKey(RichTextFont));
        }
        if (!dict.objectForKey(PlainTextFont).equals(NSFont.userFixedPitchFontOfSize(0f))) {
            NSFont.setUserFixedPitchFont((NSFont) dict.objectForKey(PlainTextFont));
        }
    }

    private boolean changingRTFFont = false;

    public void changeRichTextFont(Object sender) {
        changingRTFFont = true;

        richTextFontNameField.window().makeFirstResponder(richTextFontNameField.window());
        NSFontManager.sharedFontManager().setSelectedFont((NSFont) curValues.objectForKey(RichTextFont), false);
        NSFontManager.sharedFontManager().orderFrontFontPanel(this);
    }
    
    public void changePlainTextFont(Object sender) {
        changingRTFFont = false;

        plainTextFontNameField.window().makeFirstResponder(plainTextFontNameField.window());
        NSFontManager.sharedFontManager().setSelectedFont((NSFont) curValues.objectForKey(PlainTextFont), false);
        NSFontManager.sharedFontManager().orderFrontFontPanel(this);
    }

    public void changeFont(Object sender) {
        NSFontManager fontManager = (NSFontManager) sender;

        if (changingRTFFont) {
            displayedValues.setObjectForKey(fontManager.convertFont((NSFont) curValues.objectForKey(RichTextFont)), RichTextFont);
            showFontInField((NSFont) displayedValues.objectForKey(RichTextFont), richTextFontNameField);
        } else {
            displayedValues.setObjectForKey(fontManager.convertFont((NSFont) curValues.objectForKey(PlainTextFont)), PlainTextFont);
            showFontInField((NSFont) displayedValues.objectForKey(PlainTextFont), plainTextFontNameField);
        }
        commitDisplayedValues();
    }
}

