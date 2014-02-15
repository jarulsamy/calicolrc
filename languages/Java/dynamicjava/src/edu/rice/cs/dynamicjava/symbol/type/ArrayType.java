package edu.rice.cs.dynamicjava.symbol.type;

import edu.rice.cs.dynamicjava.symbol.*;

/**
 * Class ArrayType, a component of the ASTGen-generated composite hierarchy.
 * Note: null is not allowed as a value for any field.
 * @version  Generated automatically by ASTGen at Sat Feb 15 13:00:23 EST 2014
 */
@SuppressWarnings("unused")
public abstract class ArrayType extends ReferenceType {
  private final Type _ofType;

  /**
   * Constructs a ArrayType.
   * @throws java.lang.IllegalArgumentException  If any parameter to the constructor is null.
   */
  public ArrayType(Type in_ofType) {
    super();
    if (in_ofType == null) {
      throw new java.lang.IllegalArgumentException("Parameter 'ofType' to the ArrayType constructor was null");
    }
    _ofType = in_ofType;
  }

  public Type ofType() { return _ofType; }

  public abstract int generateHashCode();
}
