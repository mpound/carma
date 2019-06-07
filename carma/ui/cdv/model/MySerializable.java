// $Id: MySerializable.java,v 1.8 2012/01/20 17:13:10 iws Exp $

package carma.ui.cdv.model;

import java.nio.*;
import carma.util.Debug;

/**
 *  Abstract class for Serializing
 *  @author Rick Hobbs
 *  @version $Revision: 1.8 $
 */

public abstract class MySerializable {
    private ByteBuffer byteBuf_;
    private static int version_;
    
    public MySerializable() {
    }

    public abstract void mySerialize(ByteBuffer byteBuf);
    public abstract void myDeserializeVer0(ByteBuffer byteBuf);
    public abstract void myDeserializeVer1(ByteBuffer byteBuf);
    /**
     *  Return size in bytes of object
     */
    public abstract int getSizeInBytes();


    public byte isLittleEndian() {
        // Java stores all data types as Big Endian(ie. network byte order)
        return 0;
    }

    public void serializeIt(ByteBuffer byteBuf) {
        mySerialize(byteBuf);
    }

    public ByteBuffer serial() {
        int size = getSizeInBytes() + Byte.SIZE / 8 + Integer.SIZE / 8;
        byteBuf_ = ByteBuffer.allocate(size);
        // Java stores in big endian format(ie. network byte order)
        byteBuf_.order(ByteOrder.BIG_ENDIAN);
        // first byte is 0 for big endian format
        byte b = 0;
        byteBuf_.put(b);
        byteBuf_.putInt(version_);
        mySerialize(byteBuf_);
        byteBuf_.flip();
        return byteBuf_;
    }

    /**
     *  Call to initiate the reconstruction of the object from
     *  the byte Array. Subclasses should use the unpack() methods
     *  to reconstruct data.
     */
    public void deserial(ByteBuffer byteBuf) {
        // make sure we start at beginning of buffer
        byte magic = byteBuf.get();
        if ( Debug.debug )
            Debug.print(this, Debug.STATUS, "magic= " + magic);
        if (magic == 0) {
            // Little Endian
            if ( Debug.debug ) {
                Debug.print( this, Debug.STATUS,
                             "MySerializable: buffer in big endian");
            }
            byteBuf.order(ByteOrder.BIG_ENDIAN);
        } else {
            // different endianess
            if ( Debug.debug ) {
                Debug.print( this, Debug.STATUS,
                             "MySerializable: buffer in little endian");
            }
            byteBuf.order(ByteOrder.LITTLE_ENDIAN);
        }
        version_ = byteBuf.getInt();
        switch (version_) {
        case 0:
            if ( Debug.debug ) {
                Debug.print( this, Debug.STATUS,
                             "MySerializable: version= " + version_);
            }
            myDeserializeVer0(byteBuf);
            break;
        case 1:
            if ( Debug.debug ) {
                Debug.print( this, Debug.STATUS,
                             "MySerializable: version= " + version_);
            }
            myDeserializeVer1(byteBuf);
            break;
        case 2:
            myDeserializeVer1(byteBuf);
            break;

        }
    }

    public static int getVersion() {
	return version_;
    }

    /**
     *  Convenience method for serializing an Array of Characters into
     *  the byte buffer
     */
    public void pack(char[] tmpv, ByteBuffer byteBuf) {
        int size = tmpv.length;
        for (int idx = 0; idx < size; ++idx)
            byteBuf_.putChar(tmpv[idx]);
    }
    /**
     *  Convenience method for serializing an Array of integers into
     *  the byte buffer
     */
    public void pack(int[] tmpv, ByteBuffer byteBuf) {
        int size = tmpv.length;
        for (int idx = 0; idx < size; ++idx)
            byteBuf_.putInt(tmpv[idx]);
    }
    /**
     *  Convenience method for serializing an Array of floats into
     *  the byte buffer
     */
    public void pack(float[] tmpv, ByteBuffer byteBuf) {
        int size = tmpv.length;
        for (int idx = 0; idx < size; ++idx)
            byteBuf_.putFloat(tmpv[idx]);
    }
    /**
     *  Convenience method for serializing an Array of doubles into
     *  the byte buffer
     */
    public void pack(double[] tmpv, ByteBuffer byteBuf) {
        int size = tmpv.length;
        for (int idx = 0; idx < size; ++idx)
            byteBuf_.putDouble(tmpv[idx]);
    }
    /**
     *  Convenience method for serializing an Array of longs into
     *  the byte buffer
     */
    public void pack(long[] tmpv, ByteBuffer byteBuf) {
        int size = tmpv.length;
        for (int idx = 0; idx < size; ++idx)
            byteBuf_.putLong(tmpv[idx]);
    }
    /**
     *  Convenience method for deserializing an Array of longs from
     *  a byte buffer
     */
    public void unpack(long[] tmpv, ByteBuffer byteBuf) {
        int size = tmpv.length;
        for (int idx = 0; idx < size; ++idx)
            tmpv[idx] = byteBuf_.getLong();
    }

    /**
     *  Get reference to internal ByteBuffer in order to read/write
     *  primitive types.
     */
    public ByteBuffer getByteBuffer() {
        return byteBuf_;
    }

    /*
      public void pack(char tmp, ByteBuffer byteBuf) {
      byteBuf_.putChar(tmp);
      }
      public void pack(int tmp, ByteBuffer byteBuf) {
      byteBuf_.putInt(tmp);
      }
      public void pack(float tmp, ByteBuffer byteBuf) {
      byteBuf_.putFloat(tmp);
      }
      public void pack(double tmp, ByteBuffer byteBuf) {
      byteBuf_.putDouble(tmp);
      }
      public char unpack(ByteBuffer byteBuf) {
      return byteBuf_.getChar();
      }
      public int unpack(ByteBuffer byteBuf) {
      return byteBuf_.getInt();
      }
      public float unpack(float tmp, ByteBuffer byteBuf) {
      return byteBuf_.getFloat();
      }
      public double unpack(double tmp, ByteBuffer byteBuf) {
      return byteBuf_.getDouble();
      }
    */
}
