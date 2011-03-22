using System.Collections.Generic; // IList, IDictionary

public class TestParams {
    public static IDictionary<object,object> make_dict(object val) {
        if (val as IDictionary<object,object> != null) {
            return ((IDictionary<object,object>)val);
        } else
            throw new System.ArgumentException("object is not a dictionary");
    }

    public static IList<object> make_list(object val) {
        if (val as IList<object> != null) {
            return ((IList<object>)val);
        } else
            throw new System.ArgumentException("object is not a list");
    }

    public static string make_string(object val) {
        return val.ToString();
    }

    public static int make_int(object val) {
        if (val is int) {
            return ((int)val);
        } else
            throw new System.ArgumentException("object is not an int");
    }

    public static double make_double(object val) {
        if (val is double) {
            return ((double)val);
        } else
            throw new System.ArgumentException("object is not a double");
    }

    public static bool make_bool(object val) {
        if (val is bool) {
            return ((bool)val);
        } else
            throw new System.ArgumentException("object is not a bool");
    }
}
