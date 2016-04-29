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

/*
 *  This class loads and controls the SecondWindow nib. It subclasses NSWindowController
 *  which will handle tasks such as window cascading and nib loading.
 */
public class SMWSecondWindowController extends NSWindowController {

    NSTextField questionField; //This outlet is connected to the textfield in the SecondWindow nib

        /*
         * This is our constructor. It takes a nib name and passes it to super's constructor.
         */
    public SMWSecondWindowController(String nibName)
        {
            super(nibName);
        }

        /*
         *  We've made a connection in the SecondWindow nib from the Ask button to this
         *  action.
         */
    public void askQuestion(Object sender)
        {
                // Using the Application controller, we talk to the window from the other nib.  This is where we send the question being asked.
                // Here, we get a pointer to to Application's delegate.
            SMWAppDelegate appDelegate;
            appDelegate = (SMWAppDelegate)NSApplication.sharedApplication().delegate();
                // Now we send the queston to be asked in a message to the Application's Delegate.
            appDelegate.answerQuestion(questionField.stringValue());
        }
        // The Increase Font menu command sends the increaseFont message through the responder chain which gets accepted here on the second window.
    public void increaseFont(Object sender)
        {
                // The size of the font is increased by one.
            questionField.setFont(NSFont.fontWithNameAndSize(questionField.font().fontName(),
                                                             questionField.font().pointSize()+1));
        }
}
