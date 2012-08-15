using System;

// -----------------------------------------------
[AttributeUsage(AttributeTargets.All)]
public class JigsawTabAttribute : System.Attribute 
{	// Attribute that defines the tab that a block will be shown on

	private string _name = null;

	// - - - - - - - - - - - - - - - -
	public JigsawTabAttribute(string tabName)
	{
		this._name = tabName;
	}
	public JigsawTabAttribute()
	{
		this._name = null;
	}

	// - - - - - - - - - - - - - - - -
	public string Name { 
		get {
			return _name;
		}
		set {
			_name = value;
		}
	}
}

// -----------------------------------------------
