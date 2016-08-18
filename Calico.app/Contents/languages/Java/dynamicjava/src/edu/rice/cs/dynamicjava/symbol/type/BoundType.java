package edu.rice.cs.dynamicjava.symbol.type;

import edu.rice.cs.dynamicjava.symbol.*;

/**
 * Class BoundType, a component of the ASTGen-generated composite hierarchy.
 * Note: null is not allowed as a value for any field.
 * @version  Generated automatically by ASTGen at Sat Feb 15 13:00:23 EST 2014
 */
@SuppressWarnings("unused")
public abstract class BoundType extends ValidType {
  private final Iterable<? extends Type> _ofTypes;

  /**
   * Constructs a BoundType.
   * @throws java.lang.IllegalArgumentException  If any parameter to the constructor is null.
   */
  public BoundType(Iterable<? extends Type> in_ofTypes) {
    super();
    if (in_ofTypes == null) {
      throw new java.lang.IllegalArgumentException("Parameter 'ofTypes' to the BoundType constructor was null");
    }
    _ofTypes = in_ofTypes;
  }

  public Iterable<? extends Type> ofTypes() { return _ofTypes; }

  public abstract int generateHashCode();
}
