//   Copyright (c) Dragan Djuric. All rights reserved.
//   The use and distribution terms for this software are covered by the
//   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
//   which can be found in the file LICENSE at the root of this distribution.
//   By using this software in any fashion, you are agreeing to be bound by
//   the terms of this license.
//   You must not remove this notice, or any other, from this software.

package uncomplicate.clojure_cpp;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import org.bytedeco.javacpp.Pointer;
import org.bytedeco.javacpp.BytePointer;

public class StringPointer extends BytePointer {

    public StringPointer () {
        super();
    }

    public StringPointer (byte... array) {
        super(array);
    }

    public StringPointer (ByteBuffer buffer) {
        super(buffer);
    }

    public StringPointer (long size) {
        super(size);
    }

    public StringPointer (Pointer p) {
        super(p);
    }

    public StringPointer (String s) {
        super(s);
    }

    public StringPointer (String s, Charset charset) {
        super(s, charset);
    }

    public StringPointer (String s, String charsetName) throws UnsupportedEncodingException {
        super(s, charsetName);
    }

}
