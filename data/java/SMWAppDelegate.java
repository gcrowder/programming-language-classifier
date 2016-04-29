/*
        IMPORTANT: This Apple software is supplied to you by Apple Computer,
        Inc. ("Apple") in consideration of your agreement to the following terms,
        and your use, installation, modification or redistribution of this Apple
        software constitutes acceptance of these terms.  If you do not agree with
        these terms, please do not use, install, modify or redistribute this Apple
        software.

        In consideration of your agreement to abide by the following terms, and
        subject to these terms, Apple grants you a personal, non-exclusive
        license, under Apple’s copyrights in this original Apple software (the
        "Apple Software"), to use, reproduce, modify and redistribute the Apple
        Software, with or without modifications, in source and/or binary forms;
        provided that if you redistribute the Apple Software in its entirety and
        without modifications, you must retain this notice and the following text
        and disclaimers in all such redistributions of the Apple Software.
        Neither the name, trademarks, service marks or logos of Apple Computer,
        Inc. may be used to endorse or promote products derived from the Apple
        Software without specific prior written permission from Apple. Except as
        expressly stated in this notice, no other rights or licenses, express or
        implied, are granted by Apple herein, including but not limited to any
        patent rights that may be infringed by your derivative works or by other
        works in which the Apple Software may be incorporated.

        The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES
        NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
        IMPLIED WARRANTIES OF NON-INFRINGEMENT, MERCHANTABILITY AND FITNESS FOR A
        PARTICULAR PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS USE AND OPERATION
        ALONE OR IN COMBINATION WITH YOUR PRODUCTS.

        IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR
        CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
        SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
        INTERRUPTION) ARISING IN ANY WAY OUT OF THE USE, REPRODUCTION,
        MODIFICATION AND/OR DISTRIBUTION OF THE APPLE SOFTWARE, HOWEVER CAUSED AND
        WHETHER UNDER THEORY OF CONTRACT, TORT (INCLUDING NEGLIGENCE), STRICT
        LIABILITY OR OTHERWISE, EVEN IF APPLE HAS BEEN ADVISED OF THE POSSIBILITY
        OF SUCH DAMAGE.
*/

import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;
import java.io.*;

public class SMWAppDelegate
{
    SMWSecondWindowController controller; //This will point to our window controller subclass
    NSFormCell _answer;
    NSFormCell _question;
    NSArray _answerList;
    NSWindow  _firstWindow;

        /*
         * This action is connected to the Second Window button. It looks to see if the window has already
         * been created. If it has, it's simply told to show. Otherwise we create a new instance of our
         * NSWindowController subclass.
         */
    public void displaySecondWindow(NSObject sender)
        {
            if (controller == null) // Check to see if we've already loaded the second window controller
            {
                    // If not, instantiate a new controller. The controller is told to initWithWindowNibName. The
                    // new instance will use NSBundle to find and load the correct SecondWindow.nib file. When
                    // the nib is loaded, the resultField outlet described in SMWSecondWindow.h will be filled.
                controller = new SMWSecondWindowController("SecondWindow");
            }
                //Now that we have a controller, tell it to show the window.
            controller.showWindow(this);
        }
        // When a question is answered in the other nib, the Application controller gets the message to answer it.  The question is send to this method.
    public void answerQuestion(String question)
        {
            File file;          // This variable holds the plist where the answers are stored.
            NSData data;        // This variable holds the NSData that the plist is converted to.
            double index;
            String errorString[] =  new String[1];
            int    format[] = new int[1];

            controller.window().orderOut(null);	// The second window is ordered out of the screen.
            _firstWindow.makeKeyAndOrderFront(null);	// Make sure the window is open and ordered to the front.
            if (_answerList == null) {
                    // A list of answers is loaded into an array from a plist.
                file = new java.io.File(NSBundle.mainBundle().pathForResource("answerFile", "plist"));
                data = new NSData(file);
                    // The _answerList array is loaded with data from the the XML file.
                _answerList =  (NSArray)NSPropertyListSerialization.propertyListFromData(data,NSPropertyListSerialization.PropertyListImmutable,format,errorString);
            }

                // If no question was asked, we ask a question of our own.
            if (question.length()==0) {
                    // The string is loaded from a localizable strings file.
                String locDefaultQuestion;
                locDefaultQuestion = NSBundle.bundleForClass(this.getClass()).localizedStringForKey("How hard is it to enter a question?", "How hard is it to enter a question?", "SMW");
                _question.setStringValue(locDefaultQuestion);
            }
            else {
                    // The answer field is populated with an answer selected randomly from the array.
                _question.setStringValue(question);
            }
                // The answers are pulled from the array at random.
            index = Math.random() * _answerList.count();
            _answer.setStringValue((String)_answerList.objectAtIndex((int)index));

        }
}
