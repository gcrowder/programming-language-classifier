/**
 * @(#)FormatDemo.java	15.2 03/05/20
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


import com.sun.media.jai.codec.FileSeekableStream;
import com.sun.media.jai.codec.ImageCodec;
import com.sun.media.jai.codec.SeekableStream;

public class FormatDemo {

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println(
"Format recognizer demo:\n");
            System.out.println(
"  Given a file, determine which file formats it may be encoded in.");
            System.out.println(
"  In addition to the standard formats, a \"samplepnm\" codec is");
            System.out.println(
"  registered.\n");
            System.out.println("usage: java FormatDemo <filenames>");
            System.exit(0);
        }

        // Register the sample PNM codec
        ImageCodec.registerCodec(new SamplePNMCodec());
        
        try {
            for (int i = 0; i < args.length; i++) {
                SeekableStream stream = new FileSeekableStream(args[i]);
                String[] names = ImageCodec.getDecoderNames(stream);

                System.out.println("File " +
                                   args[i] +
                                   " may be in the following format(s):");
                for (int j = 0; j < names.length; j++) {
                    System.out.println("\t" + names[j]);
                }

                System.out.println();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
