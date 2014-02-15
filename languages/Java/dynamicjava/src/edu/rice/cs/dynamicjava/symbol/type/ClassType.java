package edu.rice.cs.dynamicjava.symbol.type;

import edu.rice.cs.dynamicjava.symbol.*;

/**
 * Class ClassType, a component of the ASTGen-generated composite hierarchy.
 * Note: null is not allowed as a value for any field.
 * @version  Generated automatically by ASTGen at Sat Feb 15 13:00:23 EST 2014
 */
@SuppressWarnings("unused")
public abstract class ClassType extends ReferenceType {
  private final DJClass _ofClass;

  /**
   * Constructs a ClassType.
   * @throws java.lang.IllegalArgumentException  If any parameter to the constructor is null.
   */
  public ClassType(DJClass in_ofClass) {
    super();
    if (in_ofClass == null) {
      throw new java.lang.IllegalArgumentException("Parameter 'ofClass' to the ClassType constructor was null");
    }
    _ofClass = in_ofClass;
  }

  public DJClass ofClass() { return _ofClass; }

  public abstract int generateHashCode();
}
