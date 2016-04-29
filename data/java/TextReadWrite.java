/*
        TextReadWrite.java
        Copyright (c) 1998-2005 by Apple Computer, Inc., all rights reserved.
        Author: Ali Ozer

	Code to read/write text documents.

	Most of the complexity here comes from the fact that we want to 
	preserve the original encoding of the file in order to save the file 
	correctly later on. If the user opens a Unicode file, on save, it should 
	be saved as Unicode. If the user opens a file in WinLatin1 encoding, 
	and on save it has to be changed to Unicode, we should be able to tell 
	the user this. Without these constraints, opening plain or rich files as 
	attributed strings would actually be a lot easier (and most apps can just
	go that route)...
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

import java.net.*;
import java.io.File;
import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;

public class TextReadWrite {

    private static float defaultPadding = -1f;

// Old Edit app used to write out 12 pt paddings; we compensate for that but still use the same algorithm.
    // We should actually be using the real desired margins here.
    public static float paddingCompensation() {
        if (defaultPadding < 0f) defaultPadding = new NSTextContainer().lineFragmentPadding();
        return (6f * 2f) - (defaultPadding * 2f);
    }
    
    // Guess at the encoding for the bytes in data
    public static int encodingForData(NSData fileContentsAsData) {
        int len = fileContentsAsData.length();

        if (len == 0) return Document.UnknownStringEncoding;

        byte[] bytes = fileContentsAsData.bytes(0, len < 5 ? len : 5);
        byte[] bigUnicodeHeader = { (byte) 0xff, (byte) 0xfe };
        byte[] littleUnicodeHeader = { (byte) 0xfe, (byte) 0xff };
        byte[] rtfHeader = { (byte)'{', (byte)'\\', (byte)'r', (byte)'t', (byte)'f' };

        // Unicode plain text files start with the Unicode BOM character; check for that first

        if ((len & 1) == 0 && (len >= 0) && ((bytes[0] == bigUnicodeHeader[0] && bytes[1] == bigUnicodeHeader[1]) || (bytes[0] == littleUnicodeHeader[0] && bytes[1] == littleUnicodeHeader[1]))) {
            return NSStringReference.UnicodeStringEncoding;
        } else if (len >= 6 && bytes[0] == rtfHeader[0] && bytes[1] == rtfHeader[1] && bytes[2] == rtfHeader[2] && bytes[3] == rtfHeader[3] && bytes[4] == rtfHeader[4]) {
            return Document.RichTextStringEncoding;
        } else {
            return Document.UnknownStringEncoding;
        }
    }

    // Returns true if the file could be opened; in that case also sets a bunch of stuff in document...
    // Otherwise returns false and changes nothing in document
    
    public static boolean loadFromURL(URL url, Document document, int encoding, boolean ignoreRTF, boolean ignoreHTML) {

        String fileName = NSPathUtilities.pathFromURL(url);
        NSData fileContentsAsData = null;	// Lazily set to the contents of the specified file
	String extension = NSPathUtilities.pathExtension(fileName).toLowerCase();
        boolean success = false;
        boolean isDirectory = false;

        if (url.getProtocol().equals("file")) {
            File file = new File(fileName);

            if ((file == null) || !file.exists() || !file.canRead()) return false;

            isDirectory = file.isDirectory();
        }
        
        // First take a stab at the encoding, first looking at the extensions
        if (isDirectory) {
            if (!extension.equals("rtfd")) return false;
            encoding = Document.RichTextWithGraphicsStringEncoding;
        } else if (extension.equals("rtf")) {
            encoding = Document.RichTextStringEncoding;
        } else if (extension.equals("html") || extension.equals("htm")) {
            encoding = Document.HTMLStringEncoding;
        } else if (encoding == Document.UnknownStringEncoding) {
            fileContentsAsData = new NSData(url);
            if (fileContentsAsData == null) return false;
            encoding = encodingForData(fileContentsAsData);
            if (encoding == Document.UnknownStringEncoding) {
                encoding = Preferences.intValueForKey(Preferences.PlainTextEncodingForRead);
                if (encoding == Document.UnknownStringEncoding) {
                    encoding = NSStringReference.defaultCStringEncoding();
                }
            }
        }

        if (ignoreRTF && encoding == Document.RichTextStringEncoding) {
            encoding = NSStringReference.defaultCStringEncoding();	// ??? Should get from file
        } else if (ignoreRTF && encoding == Document.RichTextWithGraphicsStringEncoding) {
            encoding = NSStringReference.defaultCStringEncoding();	// ??? Should get from file
            fileName = NSPathUtilities.stringByAppendingPathComponent(fileName, "TXT.rtf");
        } else if (ignoreHTML && encoding == Document.HTMLStringEncoding) {
            encoding = NSStringReference.defaultCStringEncoding();	// ??? Should get from file
        }

        NSMutableDictionary docAttrs = new NSMutableDictionary();
        NSAttributedString attrStr = null;

        if (encoding == Document.RichTextWithGraphicsStringEncoding) {
            NSFileWrapper fileWrapper = null;
            if (url.getProtocol().equals("file")) {
                fileWrapper = new NSFileWrapper(fileName, false /* not a symlink */);
                if (fileWrapper != null) {
                    attrStr = new NSAttributedString(fileWrapper, docAttrs);	// RTFD constructor
                    if (attrStr != null) success = true;
                    fileWrapper = null;
                }
            }
        } else {
            if (fileContentsAsData == null) {
                fileContentsAsData = new NSData(url);
            }
            if (fileContentsAsData == null) {
                success = false;
            } else if (encoding == Document.RichTextStringEncoding) {
                attrStr = new NSAttributedString(fileContentsAsData, docAttrs);	// The RTF constructor
                if (attrStr != null) success = true;
            } else if (encoding == Document.HTMLStringEncoding) {
                attrStr = new NSAttributedString(fileContentsAsData, url, docAttrs);	// The HTML constructor
                if (attrStr != null) success = true;
            } else {
                NSStringReference stringRef = new NSStringReference(fileContentsAsData, encoding);
                if (stringRef != null) {
                    document.textStorage().beginEditing();
                    document.textStorage().mutableStringReference().setString(stringRef);
                    document.setRichText(false);
                    document.textStorage().endEditing();
                    document.setEncoding(encoding);
                    stringRef = null;
                    success = true;
                }
            }
        }

        // So at this point, if attrStr is not null, we have read some rich text --- process it further.

        if (attrStr != null) {

            document.textStorage().beginEditing();
            document.setRichText(true);
            document.textStorage().setAttributedString(attrStr);	// ??? Reduce this to one step loading
            document.textStorage().endEditing();
            document.setEncoding(encoding);

            // Now pay attention to the document attributes...

            NSSize paperSize = (NSSize)docAttrs.objectForKey("PaperSize");
            if (paperSize != null) {
                NSMutableSize size = new NSMutableSize(paperSize);
                if (size.width() > paddingCompensation()) size.setWidth(size.width() - paddingCompensation());
                if (size.width() > 0 && size.height() > 0 && !document.hasMultiplePages()) document.setViewSize(size);
            }

            Number hyphFact = (Number) docAttrs.objectForKey("HyphenationFactor");
            if (hyphFact != null) document.setHyphenationFactor(hyphFact.floatValue());

            String baseURLStr = (String)docAttrs.objectForKey("BaseURL");
            URL baseURL = null;
            if (baseURLStr != null) {
                try {
                    baseURL = new URL(baseURLStr);
                } catch (MalformedURLException e) {
                    baseURL = null;
                }
            }
            if (baseURL != null) {
                document.setBaseURL(baseURL);
            }

            attrStr = null;
        }

        // Need to run GC here to make sure filewrapper used to load this document deallocated before writeToFile so that mapped-file references inside the wrapper are properly released.
        fileContentsAsData = null;
        System.gc();
        System.runFinalization();

        return success;
    }

    public static boolean saveToURL(URL url, Document document, int encoding, boolean updateFileNamesFlag) {
        if (!url.getProtocol().equals("file")) {
            return false;
        }
        String fileName = NSPathUtilities.pathFromURL(url);
        boolean success = false;
        File file = new File(fileName);
	NSDictionary curAttributes = NSPathUtilities.fileAttributes(fileName, true);

        String actualFileNameToSave = NSPathUtilities.stringByResolvingSymlinksInPath(fileName);

        if (file.exists()) {		// If not null, the file exists.
            String backupFileName = actualFileNameToSave + "~";
            File backupFile = new File(backupFileName);

            // Delete any existing backup file.
            if (backupFile.exists()) backupFile.delete();

            // If the user wishes to keep backups, simply move the old file aside.
            if (!Preferences.booleanValueForKey(Preferences.DeleteBackup)) file.renameTo(backupFile);
        }

	if (encoding == Document.RichTextWithGraphicsStringEncoding || encoding == Document.RichTextStringEncoding) {
            NSMutableDictionary attrDict = new NSMutableDictionary();
            NSSize viewSize = document.viewSize();
            float hyphenationFactor = document.hyphenationFactor();
            
            attrDict.setObjectForKey(new NSSize(new NSSize(viewSize.width() + paddingCompensation(), viewSize.height())), "PaperSize");
	    attrDict.setObjectForKey(new Float(hyphenationFactor), "HyphenationFactor");

	    if (encoding == Document.RichTextWithGraphicsStringEncoding) {
                NSFileWrapper wrapper = (NSFileWrapper)document.textStorage().RTFDFileWrapperFromRange(new NSRange(0, document.textStorage().length()), attrDict);
                if (wrapper != null) {
                    success = wrapper.writeToFile(fileName, true, updateFileNamesFlag);
                }
                wrapper = null;
	    } else {
                NSData rtf = document.textStorage().RTFFromRange(new NSRange(0, document.textStorage().length()), attrDict);
                if (rtf != null) {
                    success = rtf.writeToURL(url, true);
                }
                rtf = null;
	    }
            attrDict = null;
	} else {
            success = document.textStorage().stringReference().writeToURL(url, true, encoding);
	}

        // Apply the original permissions to the new file, and make it writable if needed.

        if (success && curAttributes != null) {
            Integer permissions = (Integer)curAttributes.objectForKey("NSFilePosixPermissions");

            if (permissions != null) {
                if (Preferences.booleanValueForKey(Preferences.SaveFilesWritable)) {
                    permissions = new Integer(permissions.intValue() | 0200);
                }
                NSMutableDictionary fileAttrs = new NSMutableDictionary();
		fileAttrs.setObjectForKey(permissions, "NSFilePosixPermissions");
	        NSPathUtilities.setFileAttributes(actualFileNameToSave, fileAttrs);
            }
        }

        System.gc();
        System.runFinalization();
        return success;
    }
    
}
