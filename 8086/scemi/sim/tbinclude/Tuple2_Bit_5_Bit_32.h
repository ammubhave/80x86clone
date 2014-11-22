// Automatically generated by: ::SceMiMsg
// DO NOT EDIT
// C++ Class with SceMi Message passing for Bluespec type:  Tuple2#(Bit#(5),Bit#(32))
// Generated on: Sat Nov 22 09:18:13 EST 2014
// Bluespec version: 2014.06.A 2014-06-24 33987

#pragma once

#include "bsv_scemi.h"

/// C++ class representing the hardware structure Tuple2#(Bit#(5),Bit#(32))
/// This class has been automatically generated.
class Tuple2_Bit_5_Bit_32 : public BSVType {
 public:
  BitT<32> m_tpl_2 ;
  BitT<5> m_tpl_1 ;

  /// A default constructor
  Tuple2_Bit_5_Bit_32 ()
    :  m_tpl_2()
    , m_tpl_1()
  {}

  /// Constructor for object from a SceMiMessageData object
  /// @param msg -- the scemi message object
  /// @param off -- the starting bit offset, updated to next bit position
  Tuple2_Bit_5_Bit_32 ( const SceMiMessageDataInterface *msg, unsigned int &off )
    : m_tpl_2(msg, off)
    , m_tpl_1(msg, off)
  {}

  /// Converts this object into its bit representation for sending as a SceMi message
  /// @param msg -- the message object written into
  /// @param off -- bit position off set in message
  /// @return next free bit position for writing
  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_tpl_2.setMessageData( msg, running );
    running = m_tpl_1.setMessageData( msg, running );
    if (running != off + 37 ) {
      std::cerr << "Mismatch in sizes: " << std::dec <<  running << " vs " << (off + 37) << std::endl;
    }
    return running;
  }

  /// overload the put-to operator for Tuple2_Bit_5_Bit_32
  friend std::ostream & operator<< (std::ostream &os, const Tuple2_Bit_5_Bit_32 &obj) {
    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    os << "{" ;
    os << "tpl_1 " << obj.m_tpl_1 ;os << " " ;
    os << "tpl_2 " << obj.m_tpl_2 ;os << "}" ;
    return os;
  }

  /// Adds to the stream the bit representation of this structure object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBitString (std::ostream & os) const {
    m_tpl_1.getBitString (os);
    m_tpl_2.getBitString (os);
  return os;
  }
  

  /// Accessor for the BSVType name for this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "Tuple2#(Bit#(5),Bit#(32))" ;
    return os;
  }

  /// Accessor on the size of the object in bits
  /// @return the bit size
  virtual unsigned int getBitSize () const {
    return 37;
  }

  /// returns the class name for this object
  virtual const char * getClassName() const {
    return "Tuple2_Bit_5_Bit_32" ;
  }

  /// returns the BSVKind for this object
  virtual BSVKind getKind() const {
    return BSV_Struct ;
  }

  /// Accessor for the count of members in object
  virtual unsigned int getMemberCount() const {
    return 2;
  };
  
  /// Accessor to member objects
  /// @param idx -- member index
  /// @return BSVType * to this object or null
  virtual BSVType * getMember (unsigned int idx) {
    switch (idx) {
      case 0: return & m_tpl_1;
      case 1: return & m_tpl_2;
      default: std::cerr << "Index error in getMember for class Tuple2_Bit_5_Bit_32" << std::endl ;
    };
    return 0;
  };
  
  /// Accessor for symbolic member names
  /// @param idx -- member index
  /// @return char* to this name or null
  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
      case 0: return "tpl_1";
      case 1: return "tpl_2";
      default: std::cerr << "Index error in getMemberName for class Tuple2_Bit_5_Bit_32" << std::endl ;
    };
    return 0;
  };
  
  
};
