package edu.rice.cs.dynamicjava.symbol.type;

import edu.rice.cs.dynamicjava.symbol.*;

/**
 * Class Type, a component of the ASTGen-generated composite hierarchy.
 * Note: null is not allowed as a value for any field.
 * @version  Generated automatically by ASTGen at Sat Feb 15 13:00:23 EST 2014
 */
@SuppressWarnings("unused")
public abstract class Type extends java.lang.Object {

  /**
   * Constructs a Type.
   * @throws java.lang.IllegalArgumentException  If any parameter to the constructor is null.
   */
  public Type() {
    super();
  }


  public abstract <RetType> RetType apply(TypeVisitor<RetType> visitor);

  public abstract void apply(TypeVisitor_void visitor);

  public abstract void output(java.io.Writer writer);
  private int _hashCode;
  private boolean _hasHashCode = false;

  public final int hashCode() {
    if (! _hasHashCode) { _hashCode = generateHashCode(); _hasHashCode = true; }
    return _hashCode;
  }

  public abstract int generateHashCode();
  public abstract void walk(TreeWalker w);
}
