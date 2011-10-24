/*
 * Copyright (c) Ian F. Darwin, http://www.darwinsys.com/, 1996-2002.
 * All rights reserved. Software written by Ian F. Darwin and others.
 * $Id: LICENSE,v 1.8 2004/02/09 03:33:38 ian Exp $
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * Java, the Duke mascot, and all variants of Sun's Java "steaming coffee
 * cup" logo are trademarks of Sun Microsystems. Sun's, and James Gosling's,
 * pioneering role in inventing and promulgating (and standardizing) the Java 
 * language and environment is gratefully acknowledged.
 * 
 * The pioneering role of Dennis Ritchie and Bjarne Stroustrup, of AT&T, for
 * inventing predecessor languages C and C++ is also gratefully acknowledged.
 */

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * A simple FileFilter class that works by filename extension, like the one in
 * the JDK demo called ExampleFileFilter, which has been announced to be
 * supported in a future Swing release.
 */
public class JFileFilter extends javax.swing.filechooser.FileFilter {
  protected String description;

  protected ArrayList<String> exts = new ArrayList<String>();

  public void addType(String s) {
    exts.add(s);
  }

  /** Return true if the given file is accepted by this filter. */
  public boolean accept(File f) {
    // Little trick: if you don't do this, only directory names
    // ending in one of the extentions appear in the window.
    if (f.isDirectory()) {
      return true;

    } else if (f.isFile()) {
      Iterator it = exts.iterator();
      while (it.hasNext()) {
        if (f.getName().endsWith((String) it.next()))
          return true;
      }
    }

    // A file that didn't match, or a weirdo (e.g. UNIX device file?).
    return false;
  }

  /** Set the printable description of this filter. */
  public void setDescription(String s) {
    description = s;
  }

  /** Return the printable description of this filter. */
  public String getDescription() {
    return description;
  }
}

