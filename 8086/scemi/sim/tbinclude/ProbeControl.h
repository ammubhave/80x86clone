// Automatically generated by: ::SceMiMsg
// DO NOT EDIT
// C++ Class with SceMi Message passing for Bluespec type:  SceMiSerialProbe::ProbeControl
// Generated on: Wed Nov 26 13:26:56 EST 2014
// Bluespec version: 2014.06.A 2014-06-24 33987

#pragma once

#include "bsv_scemi.h"
#include "PrbNum.h"

/// C++ class representing the hardware structure SceMiSerialProbe::ProbeControl
/// This class has been automatically generated.
class ProbeControl : public BSVType {
 public:
  PrbNum m_num ;
  BitT<1> m_enable ;

  /// A default constructor
  ProbeControl ()
    :  m_num()
    , m_enable()
  {}

  /// Constructor for object from a SceMiMessageData object
  /// @param msg -- the scemi message object
  /// @param off -- the starting bit offset, updated to next bit position
  ProbeControl ( const SceMiMessageDataInterface *msg, unsigned int &off )
    : m_num(msg, off)
    , m_enable(msg, off)
  {}

  /// Converts this object into its bit representation for sending as a SceMi message
  /// @param msg -- the message object written into
  /// @param off -- bit position off set in message
  /// @return next free bit position for writing
  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_num.setMessageData( msg, running );
    running = m_enable.setMessageData( msg, running );
    if (running != off + 17 ) {
      std::cerr << "Mismatch in sizes: " << std::dec <<  running << " vs " << (off + 17) << std::endl;
    }
    return running;
  }

  /// overload the put-to operator for ProbeControl
  friend std::ostream & operator<< (std::ostream &os, const ProbeControl &obj) {
    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    os << "{" ;
    os << "enable " << obj.m_enable ;os << " " ;
    os << "num " << obj.m_num ;os << "}" ;
    return os;
  }

  /// Adds to the stream the bit representation of this structure object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBitString (std::ostream & os) const {
    m_enable.getBitString (os);
    m_num.getBitString (os);
  return os;
  }
  

  /// Accessor for the BSVType name for this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "SceMiSerialProbe::ProbeControl" ;
    return os;
  }

  /// Accessor on the size of the object in bits
  /// @return the bit size
  virtual unsigned int getBitSize () const {
    return 17;
  }

  /// returns the class name for this object
  virtual const char * getClassName() const {
    return "ProbeControl" ;
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
      case 0: return & m_enable;
      case 1: return & m_num;
      default: std::cerr << "Index error in getMember for class ProbeControl" << std::endl ;
    };
    return 0;
  };
  
  /// Accessor for symbolic member names
  /// @param idx -- member index
  /// @return char* to this name or null
  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
      case 0: return "enable";
      case 1: return "num";
      default: std::cerr << "Index error in getMemberName for class ProbeControl" << std::endl ;
    };
    return 0;
  };
  
  
};

