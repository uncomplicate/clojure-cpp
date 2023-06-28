//   Copyright (c) Dragan Djuric. All rights reserved.
//   The use and distribution terms for this software are covered by the
//   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
//   which can be found in the file LICENSE at the root of this distribution.
//   By using this software in any fashion, you are agreeing to be bound by
//   the terms of this license.
//   You must not remove this notice, or any other, from this software.

package uncomplicate.clojure_cpp.pointer;

import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import org.bytedeco.javacpp.Pointer;
import org.bytedeco.javacpp.BytePointer;
import clojure.lang.Keyword;

public class KeywordPointer extends BytePointer {

    public KeywordPointer (String name) {
        super(name);
    }


    public KeywordPointer (String name, Charset charset){
        super(name, charset);
    }

    public KeywordPointer (String name, String charsetName) throws UnsupportedEncodingException {
        super(name, charsetName);
    }

}
