using Microsoft.Scripting.Math;
using System;

public class Rational {
  public int numerator;
  public int denominator;
  
  public Rational(int num) {
	this.numerator = num;
	this.denominator = 1;
  }
  
  public static byte[] GetBytes(string str) {
      byte[] bytes = new byte[str.Length];
      int i=0;
      foreach(char c in str.ToCharArray()) {
	  bytes[i] = (byte)c;
	  i++;
      }
      return bytes;
  }

  public Rational(string snumerator, string sdenominator) {
      //BigInteger numerator = new BigInteger(GetBytes(snumerator));
      //BigInteger denominator = new BigInteger(GetBytes(sdenominator));
      int numerator = System.Int32.Parse(snumerator);
      int denominator = System.Int32.Parse(sdenominator);
      if (denominator == 0)
	  throw new Exception("cannot represent rationals with a zero denominator");
      int gcd = GCD(numerator, denominator);
      this.numerator = new Rational(numerator, gcd);
      this.denominator = new Rational(denominator, gcd);
  }
  
  public Rational(int numerator, int denominator) {
	if (denominator == 0)
	  throw new Exception("cannot represent rationals with a zero denominator");
	int gcd = GCD(numerator, denominator);
	this.numerator = numerator/gcd;
	this.denominator = denominator/gcd;
  }
  
  public static int GCD(int n1, int n2) {
	// Greatest Common Denominator
	n1 = Math.Abs(n1);
	n2 = Math.Abs(n2);
	if (n1 == 0) return n2;
	if (n2 == 0) return n1;
	if (n1 > n2) return GCD(n2, n1 % n2);
	else         return GCD(n1, n2 % n1);
  }
  
  public static int LCM(int n1, int n2) {
	// Least Common Multiple
	n1 = Math.Abs(n1);
	n2 = Math.Abs(n2);
	if (n1 > n2) return checked((n2 / GCD(n1, n2)) * n1);
	else         return checked((n1 / GCD(n1, n2)) * n2);
  }
  
  public override bool Equals(object other) {
	return (other is Rational &&
		(this.numerator == ((Rational)other).numerator &&
			this.denominator == ((Rational)other).denominator));
  }
  
  public override int GetHashCode() {
	double d = ((double) numerator) / denominator;
	return d.GetHashCode();
  }
  
  public String __repr__() {
      return ToString();
  }

  public override string ToString() {
	//if (denominator != 1)
	return string.Format("{0}/{1}", numerator, denominator);
	//else
	//return numerator.ToString();
  }
  
  public static implicit operator double(Rational f) {
	return (((double) f.numerator) / ((double) f.denominator));
  }
  
  public static implicit operator int(Rational f) {
	return f.numerator / f.denominator;
  }
  
  public static implicit operator float(Rational f) {
	return (((float) f.numerator) / ((float) f.denominator));
  }
  
  public static Rational operator +(Rational f1, Rational f2) {
	int lcm = LCM(f1.denominator, f2.denominator);
	return new Rational((f1.numerator * lcm/f1.denominator +
			f2.numerator * lcm/f2.denominator),
		lcm);
  }
  
  public static Rational operator +(Rational f1, int i) {
	int lcm = LCM(f1.denominator, 1);
	return new Rational((f1.numerator * lcm/f1.denominator +
			i * lcm/1),
		lcm);
  }
  
  public static Rational operator +(int i, Rational f1) {
	int lcm = LCM(f1.denominator, 1);
	return new Rational((f1.numerator * lcm/f1.denominator +
			i * lcm/1),
		lcm);
  }

  public static Rational operator -(Rational f1, Rational f2) {
	int lcm = LCM(f1.denominator, f2.denominator);
	return new Rational((f1.numerator * lcm/f1.denominator -
			f2.numerator * lcm/f2.denominator),
		lcm);
  }
  
  public static Rational operator -(Rational f1, int i) {
	int lcm = LCM(f1.denominator, 1);
	return new Rational((f1.numerator * lcm/f1.denominator -
			i * lcm/1),
		lcm);
  }
  
  public static Rational operator -(int i, Rational f1) {
	int lcm = LCM(f1.denominator, 1);
	return new Rational((i * lcm/1) - (f1.numerator * lcm/f1.denominator),
		lcm);
  }

  public static Rational operator *(Rational f1, Rational f2) {
	return new Rational((f1.numerator * f2.numerator),
		(f1.denominator * f2.denominator));
  }
  
  public static Rational operator *(Rational f1, int i) {
	return new Rational((f1.numerator * i), f1.denominator);
  }
  
  public static Rational operator *(int i, Rational f1) {
	return new Rational((f1.numerator * i), f1.denominator);
  }

  public static Rational operator /(Rational f1, Rational f2) {
	return new Rational((f1.numerator * f2.denominator),
		                (f1.denominator * f2.numerator));
  }
  
  public static Rational operator /(Rational f1, int i) {
	return new Rational(f1.numerator, f1.denominator * i);
  }
  
  public static Rational operator /(int i, Rational f1) {
	return new Rational(f1.denominator * i, f1.numerator);
  }

}

