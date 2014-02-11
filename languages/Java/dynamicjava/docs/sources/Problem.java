public class Problem {
    public static void main(String[] args) {
	final int i = 123;
	new C() {
	    int m() {
		return i;
	    }
	};
    }
}

abstract class C {
    C() {
	System.out.println("m() = " + m());
    }
    abstract int m();
}
