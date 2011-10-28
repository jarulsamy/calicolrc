namespace Calico {

	public class Language {
	  public string name;
	  public string [] extensions;
	
	  public Language(string language, string [] extensions) {
	    this.name = language;
	    this.extensions = extensions;
	  }
	
	  public virtual Engine make_engine() {
	     return new Engine();
	  }
	
   	  public static Language RegisterLanguage() {
	     return null;
	  }
	}
	
}