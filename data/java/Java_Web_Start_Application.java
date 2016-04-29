//
//  Java_Web_Start_Application.java
//  Java_Web_Start_Application
//
//  Created by __MyName__ on 01/01/2001.
//  Copyright (c) 2007, __MyCompanyName__. All rights reserved.
//
//  A simple Web Start application
//

import java.awt.*;
import java.net.*;
import javax.swing.*;
import javax.jnlp.*;

public class Java_Web_Start_Application {
	
    public static void main(String args[]) {
		try {
			System.out.println("Attempting to open www.apple.com in your browser.");
            URL apple = new URL("http://www.apple.com");
            BasicService bs = (BasicService)ServiceManager.lookup("javax.jnlp.BasicService");
            bs.showDocument(apple);
        } catch(UnavailableServiceException unavailableserviceexception) {
			JOptionPane.showMessageDialog(null, "This functionality is only available when this application is launched with Java Web Start.", "", 0);
        } catch(MalformedURLException malformedurlexception) { }
    }
	
}
