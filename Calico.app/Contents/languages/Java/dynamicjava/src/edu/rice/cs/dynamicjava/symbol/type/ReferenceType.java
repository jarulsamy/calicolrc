package edu.rice.cs.dynamicjava.symbol.type;

import edu.rice.cs.dynamicjava.symbol.*;

/**
 * Class ReferenceType, a component of the ASTGen-generated composite hierarchy.
 * Note: null is not allowed as a value for any field.
 * @version  Generated automatically by ASTGen at Sat Feb 15 13:00:23 EST 2014
 */
@SuppressWarnings("unused")
public abstract class ReferenceType extends ValidType {

  /**
   * Constructs a ReferenceType.
   * @throws java.lang.IllegalArgumentException  If any parameter to the constructor is null.
   */
  public ReferenceType() {
    super();
  }


  public abstract int generateHashCode();
}
