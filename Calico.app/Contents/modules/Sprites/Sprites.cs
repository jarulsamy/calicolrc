/******************************************************************************
Sprites Module - Bryn Mawr College, Summer Research 2013
Documentation: http://calicoproject.org/Calico_Sprites
*******************************************************************************/

/*
Calico - Scripting Environment

Copyright (c) 2013, Hannah Organick <horganick@brynmawr.edu>
Copyright (c) 2013, Doug Blank <dblank@cs.brynmawr.edu>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

$Id: $
*/

using System.Threading;
using System.Collections.Generic;
using System;
using System.IO;

// Events
//using EventsManager = Events;

public static class Sprites
{
    public static Graphics.WindowClass window;
    public delegate void InvokeDelegate ();

    public static Dictionary<string,Sprite> sprites = new Dictionary<string,Sprite> ();
    public static string default_sprite = null; 
    public static Dictionary<string,Background> backgroundObjs = new Dictionary<string,Background> ();
    public static string default_background = null;

    public static bool visibleOnStart = true;
    // public static SdlDotNet.Audio.Sound defaultSound = (SdlDotNet.Audio.Sound) Myro.makeSound(getSoundPath("Asterisk.wav"));
    public static SdlDotNet.Audio.Channel backgroundChannel = null; //defaultSound.Play(0); 




    //-------------------------------------------BACKDROP---------------------------------------------------	
    public class Background{
	private Graphics.Picture currentBackground;
	private Dictionary<string, Graphics.Picture> backdropDictionary;	

	public Background(string filename){
		//TODO automate later by adding everything in a backdrops folder, add more possible backdrops
		backdropDictionary = new Dictionary<string, Graphics.Picture>();
		backdropDictionary.Add(Path.GetFileNameWithoutExtension(filename), new Graphics.Picture(getImagePath(filename)));
		backdropDictionary.Add(Path.GetFileNameWithoutExtension("sun.png"), new Graphics.Picture(getImagePath("sun.png")));

		foreach(KeyValuePair<string, Graphics.Picture> entry in backdropDictionary){
			Graphics.Picture pic = backdropDictionary[entry.Key];

			pic.border = 0;
			pic.undraw();
		}	

		currentBackground = backdropDictionary[Path.GetFileNameWithoutExtension(filename)];
		show();
	}	

	public void changeBackground(string backdropName){          
		if(backdropDictionary.ContainsKey(backdropName)){
			hide();
			currentBackground = backdropDictionary[backdropName];
			show();
		}
		else{
			throw new Exception("\'" + backdropName + "\' has not been added yet.  Please choose a valid backdrop: " + getAllKeysForPrint(backdropDictionary));
		}
	}

	public void addBackground(string filename){
		if(backdropDictionary.ContainsKey(Path.GetFileNameWithoutExtension(filename))){
			throw new Exception("\'" + filename + "\' has already been added.");
		}

		Graphics.Picture newBackground = new Graphics.Picture(getImagePath(filename));
		newBackground.border = 0;
		newBackground.undraw();
		backdropDictionary.Add(Path.GetFileNameWithoutExtension(filename), newBackground);
	}
	public void hide(){
		currentBackground.undraw();
	}
	public void show(){
		currentBackground.draw(window);
		currentBackground.stackOnBottom();
	}
	public void reset(){
		foreach(KeyValuePair<string, Graphics.Picture> entry in backdropDictionary){
			Graphics.Picture pic = backdropDictionary[entry.Key];
			pic.undraw();
		}
		show();
	}

   }


    //-------------------------------------------SPRITE---------------------------------------------------	
    public class Sprite {
	private Graphics.Picture currentCostume; 
	private Dictionary<string, Graphics.Picture> costumeDictionary;	
	
	private bool isVisible;
	private string voiceName;
	//private int fontSize;
	private bool hasFlippedHorizontal;
	private bool hasFlippedVertical;
	private string copying;
	private string copiedBy;
	private string myName;


	public Sprite(string filename){
		isVisible = true;
		voiceName = "default";
		//fontSize = 18;
		hasFlippedHorizontal = false;
		hasFlippedVertical = false;

		copying = "none";
		copiedBy = "none";
		myName = "none";


		//costumeDictionary = new Dictionary<string, string>();

		costumeDictionary = new Dictionary<string, Graphics.Picture>();
		costumeDictionary.Add(Path.GetFileNameWithoutExtension(filename), new Graphics.Picture(getImagePath(filename)));
		costumeDictionary.Add(Path.GetFileNameWithoutExtension("pinkHair.png"), new Graphics.Picture(getImagePath("pinkHair.png")));
		costumeDictionary.Add(Path.GetFileNameWithoutExtension("greenHair.png"), new Graphics.Picture(getImagePath("greenHair.png")));
			
		currentCostume = costumeDictionary[Path.GetFileNameWithoutExtension(filename)];
	    	//costume = new Picture(costumeDict[nameNewCostume]) 
	
	
		currentCostume.border = 0;
		currentCostume.stackOnTop();
		
		if(visibleOnStart){
			show();	
		}
		else{
			hide();
		}

	}
	
	//TODO really needs to be private, it could mess up the associations in sprites (dictionary) if the names were reset
	public void setName(string name){
		myName = name;
	}
	public string getName(){
		return myName;
	}	
	public void setCopying(string newCopying){
		copying = newCopying;
	}
	public void setCopiedBy(string newCopiedBy){
		copiedBy = newCopiedBy;
	}

	private bool isClicked(){
		if(window != null){
			if(window.getMouseState() == "down"){
				IronPython.Runtime.PythonTuple mousePos = window.getMouseNow();
				int mouseX = (int) mousePos[0];
				int mouseY = (int) mousePos[1];
			
				double halfWidth = currentCostume.getWidth()/2;
				double halfHeight = currentCostume.getHeight()/2;

				//System.Console.WriteLine("cc is at: (" + currentCostume.x + ", " + currentCostume.y + ")");
				//System.Console.WriteLine("position: (" + mouseX + ", " + mouseY + ")");

				if(mouseX >= currentCostume.x - halfWidth && mouseX <= currentCostume.x + halfWidth){
					if(mouseY >= currentCostume.y - halfHeight && mouseY <= currentCostume.y + halfHeight){
						return true;
					}
				}
		
			}
		}
		return false;	
	}

	public bool stopImitating(){
		if(copying != "none"){
			getSprite(copying).setCopiedBy("none");
			copying = "none";
			return true;
		}
		
		throw new Exception(myName + " is not imitating any Sprite.");
	}

	public bool imitateSprite(string otherSpriteName){
		if(otherSpriteName == myName){
			throw new Exception(myName + " may not imitate itself.");
		}

		if(containsSprite(otherSpriteName)){
			getSprite(otherSpriteName).setCopiedBy(myName);
			copying = otherSpriteName;
			return true;
		}

		throw new Exception("\'" + otherSpriteName + "\' is not a valid Sprite name.");
	}

	public bool getIsVisible(){
		return isVisible;
	}

	//TODO - make a new user getImagePath so they can put their own pictures on a higher level?
	public void addCostume(string filename){
		if(costumeDictionary.ContainsKey(Path.GetFileNameWithoutExtension(filename))){
			throw new Exception("\'" + filename + "\' has already been added.");
		}
		Graphics.Picture newCostume = new Graphics.Picture(getImagePath(filename));
		costumeDictionary.Add(Path.GetFileNameWithoutExtension(filename), newCostume);
	}

	public void show(){
		isVisible = true;
		currentCostume.draw(window);	
		window.update();

		if(copiedBy != "none"){
			getSprite(copiedBy).show();
		}	
	}

	public void hide(){
		isVisible = false;
		currentCostume.undraw();
		window.update();

		if(copiedBy != "none"){
			getSprite(copiedBy).hide();
		}
	}

	public void flipHorizontal(){
		hasFlippedHorizontal = !hasFlippedHorizontal;
		currentCostume.flipHorizontal();

		if(copiedBy != "none"){
			getSprite(copiedBy).flipHorizontal();
		}
	}

	public void flipVertical(){
		hasFlippedVertical = !hasFlippedVertical;
		currentCostume.flipVertical();

		if(copiedBy != "none"){
			getSprite(copiedBy).flipVertical();
		}
	}

	private void transparency(int value){
		currentCostume.setAlpha((byte)(int)(255*value));
	}
	

	public void changeCostume(string costumeName){
		if(costumeDictionary.ContainsKey(costumeName)){
			//currentCostume  = dictionaryPossible[costumeName];
	
			bool origIsVisible = getIsVisible();
			double origRotation = currentCostume.rotation;
			double origScaleFactor = currentCostume.scaleFactor;
			//int origAlpha = alpha;
			int origBorder = currentCostume.border;
			double origX = currentCostume.x;
			double origY = currentCostume.y;

			currentCostume.undraw();
			currentCostume.rotation = 0;
			if(hasFlippedHorizontal){
				currentCostume.flipHorizontal();
			}
			if(hasFlippedVertical){
				currentCostume.flipVertical();
			}


			currentCostume = costumeDictionary[costumeName];
			currentCostume.stackOnTop();


			currentCostume.border = origBorder;
			currentCostume.rotation = origRotation;
			scaleTo(origScaleFactor);
			moveTo(origX, origY);
			//transparency(origAlpha);
			
			if(hasFlippedHorizontal){
				currentCostume.flipHorizontal();
			}
			if(hasFlippedVertical){
				currentCostume.flipVertical();
			}
			
			if(origIsVisible){			
				show();
			}
			else{
				hide();
			}
			

						
		}
		else{
			throw new Exception("\'" + costumeName + "\' has not been added.  Please choose a valid costume: " + getAllKeysForPrint(costumeDictionary));
		}
	}

	/*public void setFontSize(int newFontSize){
		fontSize = newFontSize;
	}*/
	
	public void speak(string message, double seconds){
		double halfWidth = currentCostume.getWidth()/2;
		double halfHeight = currentCostume.getHeight()/2;

		Graphics.Point costUL = new Graphics.Point(currentCostume.x - halfWidth, currentCostume.y - halfHeight);
		Graphics.Point costO = new Graphics.Point(currentCostume.x, currentCostume.y);


		Graphics.Point upperLeft = new Graphics.Point(costUL.x - halfWidth, (costUL.y - currentCostume.getHeight()*1.5));
		Graphics.Point lowerRight = new Graphics.Point(costUL.x + 140, costUL.y - halfHeight);
		Graphics.Point origin = new Graphics.Point(costO.x - (halfWidth/1.5), costO.y - (halfHeight));
		Graphics.SpeechBubble sb = new Graphics.SpeechBubble(upperLeft, lowerRight, message, origin);
		sb.draw(window);
		speakOnly(message);
		wait(seconds);
		sb.undraw();
	}

	private void speakOnly(string message){
		if(voiceName == "default"){
			Myro.setVoice(voiceName);
		}
		else{
			Myro.setVoiceName(voiceName);					
		}

		Myro.speak(message);
	}	

	public void setVoice(string voiceID){
		voiceName = voiceID;
	}
	
	public void move(double x, double y){
		currentCostume.move(x, y);

		if(copiedBy != "none"){
			getSprite(copiedBy).move(x, y);
		}
	}
	
	public void moveTo(double x, double y){
		currentCostume.moveTo(x, y);

		if(copiedBy != "none"){
			getSprite(copiedBy).moveTo(x, y);
		}
	}

	public void rotate(double degrees){
		currentCostume.rotate(degrees);

		if(copiedBy != "none"){
			getSprite(copiedBy).rotate(degrees);
		}
	}

	public void rotateTo(double degrees){
		currentCostume.rotateTo(degrees);

		if(copiedBy != "none"){
			getSprite(copiedBy).rotateTo(degrees);
		}
	}

	public void forward(double pixels){
		currentCostume.forward(pixels);

		if(copiedBy != "none"){
			getSprite(copiedBy).forward(pixels);
		}
	}

	public void backward(double pixels){
		currentCostume.backward(pixels);

		if(copiedBy != "none"){
			getSprite(copiedBy).backward(pixels);
		}
	}

	public void changeXPositionBy(double pixels){
		currentCostume.move(pixels, 0);

		if(copiedBy != "none"){
			getSprite(copiedBy).move(pixels, 0);
		}
	}

	public void changeYPositionBy(double pixels){
		currentCostume.move(0, pixels);

		if(copiedBy != "none"){
			getSprite(copiedBy).move(0, pixels);
		}
	}

	public void scale(double factor){
		currentCostume.scale(factor);

		if(copiedBy != "none"){
			getSprite(copiedBy).scale(factor);
		}
	}

	public void scaleTo(double factor){
		currentCostume.scaleTo(factor);

		if(copiedBy != "none"){
			getSprite(copiedBy).scaleTo(factor);
		}
	}

	//TODO (see note on glideBackward)
	public void glideTo(double x, double y, double seconds){
		double oX = currentCostume.x;
		double oY = currentCostume.y;
		
		//We are actually in the south-east quadrant so the y values should be negative
		double m = ((-y) - (-oY))/(x - oX);
		double b = -y - m*x;
		double verticalDist = Math.Abs(y - oY);
		double horizontalDist = Math.Abs(x - oX);

		bool goRight; 
		bool goUp;
		double timePerStep; 
		

		if(x >= oX){
			goRight = true;
		}
		else{
			goRight = false;		
		}
		if(y > oY){
			goUp = false;
		}
		else{
			goUp = true;		
		}

		double newX = currentCostume.x;
		double newY = currentCostume.y;
		
		//If moving only horizontally
		if(y == oY){
			timePerStep  = seconds/horizontalDist;
			for(int i = 0; i < horizontalDist; i++){
				if(goRight){
					newX += 1;	
				}
				else{
					newX -= 1;	
				}
		
				currentCostume.moveTo(newX, y);
				wait(timePerStep);
			}
		}
		//If moving only vertically
		else if(x == oX){
			timePerStep = seconds/verticalDist;
			for(int i =0; i < verticalDist; i++){
				if(goUp){
					newY -= 1;		
				}
				else{
					newY += 1;
				}
	
				currentCostume.moveTo(x, newY);
				wait(timePerStep);
			}
		}
		else if(horizontalDist >= verticalDist){
			timePerStep  = seconds/horizontalDist;
			for(int i = 0; i < horizontalDist; i++){
				if(goRight){
					newX += 1;	
				}
				else{
					newX -= 1;	
				}
		
				newY = m*newX + b;
				currentCostume.moveTo(newX, (-1*newY));
				wait(timePerStep);
			}

		}
		else{
			timePerStep = seconds/verticalDist;
			for(int i = 0; i < verticalDist; i++){
				if(goUp){
					newY -= 1;	
				}
				else{
					newY += 1;	
				}
		
				newX = ((-1*newY) - b) / m;				
				currentCostume.moveTo(newX, (newY));
				wait(timePerStep);
			}
	
		}
	

		if(copiedBy != "none"){
			getSprite(copiedBy).glideTo(x, y, seconds);
		}
	}

	//TODO (see note on glideBackward)
	public void glideForward(double pixels, double seconds){
		double timePerStep = seconds/pixels;
		
		for(int i = 0; i < pixels; i++){
			currentCostume.forward(1);
			Graphics.wait(timePerStep);
		} 

		if(copiedBy != "none"){
			getSprite(copiedBy).glideForward(pixels, seconds);
		}
	}

	//TODO - The timing is inaccurate in jigsaw because there seems to be a built in wait 
	//already between window updates  
	public void glideBackward(double pixels, double seconds){
		double timePerStep = seconds/pixels;
		
		for(int i = 0; i < pixels; i++){
			currentCostume.backward(1);
			Graphics.wait(timePerStep);
		} 

		if(copiedBy != "none"){
			getSprite(copiedBy).glideBackward(pixels, seconds);
		}

	}
	

	public void printStatus(){
		System.Console.WriteLine("\t" + myName + " is at (" + currentCostume.x + ", " + currentCostume.y + ")");
		System.Console.WriteLine("\t" + myName + "'s rotation is " + currentCostume.rotation);
		System.Console.Write("\t" + myName + " is ");
		if(!isVisible){
			System.Console.Write("NOT ");
		}
		System.Console.WriteLine("visible. ");
	}

	public double getX(){
		return currentCostume.x;
	}


	public double getY(){
		return currentCostume.y;
	}

	public double getRotation(){
		return currentCostume.rotation;
	}

    }

//-----------------------------------------------------------------------------------------------------------------
//--------------------------------------END OF SPRITE CLASS--------------------------------------------------------
//-----------------------------------STATIC METHODS START HERE-----------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------

	//for error messages
	private static string getAllKeysForPrint(Dictionary<string, Graphics.Picture> dict){
		string str = "";
		foreach(KeyValuePair<string, Graphics.Picture> entry in dict){
				str += "\'"; 
				str += entry.Key;
				str += "\', ";
			}
		return str;
	}

	[JigsawTab("Sprites Look")]
	public static void show(){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].show();
		}
	}

	[JigsawTab("Sprites Look")]
	public static void hide(){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].hide();
		}
	}
		
	[JigsawTab("Sprites Look")]
	public static bool isVisible(){
		if(sprites.ContainsKey(default_sprite)){
			return sprites[default_sprite].getIsVisible();
		}
		
		return false;
	}

	[JigsawTab("Sprites Look")]
	public static void flipHorizontal(){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].flipHorizontal();
		}
	}

	[JigsawTab("Sprites Look")]
	public static void flipVertical(){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].flipVertical();
		}
	}

	//see issue #9 in tracker
	/*[JigsawTab("Sprites Look")]
	private static void transparency(int value){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].transparency(value);
		}
	}*/

	[JigsawTab("Sprites Look")]
	public static void changeCostume(string costumeName){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].changeCostume(costumeName);
		}
	}

	[JigsawTab("Sprites Look")]
	public static void addCostume(string filename){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].addCostume(filename);		
		}
	}

	[JigsawTab("Sprites Look")]
	public static void changeBackground(string backdropName){
		if(backgroundObjs.ContainsKey(default_background)){
			backgroundObjs[default_background].changeBackground(backdropName);
		}
	}

	[method: JigsawTab("Sprites Look")]
	public static void addBackground(string filename){
		if(backgroundObjs.ContainsKey(default_background)){
			backgroundObjs[default_background].addBackground(filename);
		}
	}
	
	//TODO needs to hide all sprites so that their isVisibles are updated
	/*	
	[method: JigsawTab("Sprites Look")]
	public static void clearWindow(){
		if(window != null){
			window.clear();
		}
	}*/

	[JigsawTab("Sprites Sound")]
	public static void speak(string message, double seconds){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].speak(message, seconds);
		}
	}

	[JigsawTab("Sprites Sound")]
	public static void play(string filename){
		Myro.play(getSoundPath(filename));
	}

	[JigsawTab("Sprites Sound")]
	public static void playFor(string filename, int seconds){
		Myro.play(getSoundPath(filename), seconds);
	}

	[JigsawTab("Sprites Sound")]
	private static void playInBackground(string filename){
		//Prevents multiple tracks playing on the background channel at once
		if(backgroundChannel != null){
			backgroundChannel.Pause();
		}

		SdlDotNet.Audio.Sound backgroundSound = (SdlDotNet.Audio.Sound) Myro.makeSound(getSoundPath(filename));
		backgroundChannel = backgroundSound.Play(-1);
	}

	[JigsawTab("Sprites Sound")]
	private static void setBackgroundVolume(int volume){
		if(backgroundChannel != null){
			backgroundChannel.Volume = volume;
		}
	}

	[JigsawTab("Sprites Sound")]
	private static void stopBackgroundSound(){
		if(backgroundChannel != null){
			backgroundChannel.Pause();
		}
	}
	
	[JigsawTab("Sprites Sound")]
	public static void printVoiceOptions(){
		IronPython.Runtime.List l = Myro.getVoiceNames();

		System.Console.WriteLine();
		for(int i = 0; i < l.Count; i++){
			System.Console.Write(l[i] + ", ");
		}
		System.Console.WriteLine();

	}

	[JigsawTab("Sprites Sound")]
	public static void setVoice(string voiceID){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].setVoice(voiceID);
		}
	}


	[JigsawTab("Sprites Move")]
	public static void move(double x, double y){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].move(x, y);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void moveTo(double x, double y){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].moveTo(x, y);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void rotate(double degrees){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].rotate(degrees);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void rotateTo(double degrees){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].rotateTo(degrees);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void forward(double pixels){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].forward(pixels);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void backward(double pixels){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].backward(pixels);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void scale(double factor){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].scale(factor);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void scaleTo(double factor){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].scaleTo(factor);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void glideBackward(double pixels, double seconds){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].glideBackward(pixels, seconds);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void glideForward(double pixels, double seconds){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].glideForward(pixels, seconds);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void glideTo(double x, double y, double seconds){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].glideTo(x, y, seconds);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void changeYPositionBy(double pixels){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].changeYPositionBy(pixels);
		}
	}

	[JigsawTab("Sprites Move")]
	public static void changeXPositionBy(double pixels){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].changeXPositionBy(pixels);
		}
	}

	[JigsawTab("Sprites")]
	public static void printSpriteStatus(){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].printStatus();		
		}
		if(sprites.Count == 0){
			System.Console.WriteLine("\tThere are no sprites.");		
		}
	}

 	[method: JigsawTab("Sprites")]
    	public static Sprite selectSprite(string spriteName) {
		if (sprites.ContainsKey(spriteName)) {
	  		default_sprite = spriteName;
	    		return sprites[spriteName];
		} 
		throw new Exception("\'" + spriteName + "\' is not a valid Sprite name.");
	}

	[method: JigsawTab("Sprites")]
	public static bool removeSprite(string spriteName){
		if(sprites.ContainsKey(spriteName)){
			sprites[spriteName].hide();
			return sprites.Remove(spriteName);
		}
		
		return false;
	}

	[method: JigsawTab("Sprites")]
	public static bool containsSprite(string spriteName){
		if (sprites.ContainsKey(spriteName)) {
	    		return true;
		} 				
		return false;
	}


	[method: JigsawTab("Sprites")]
	public static Sprite getSprite(string spriteName){
		if(sprites.ContainsKey(spriteName)){
			return sprites[spriteName];
		}
		throw new Exception("\'" + spriteName + "\' is not a valid Sprite name."); 
	}	

	[method: JigsawTab("Sprites Interact")]
	public static void imitateSprite(string spriteName){
		if(sprites.ContainsKey(default_sprite)){
			sprites[default_sprite].imitateSprite(spriteName);		
		}
	}
	
	[method: JigsawTab("Sprites Interact")]
	public static bool stopImitating(){
		if(sprites.ContainsKey(default_sprite)){
			return sprites[default_sprite].stopImitating();		
		}
		
		return false;
	}

	//true if any sprite is clicked
	/*[method: JigsawTab("Sprites Interact")]
	private static bool isSpriteClicked(){
		if(sprites.Count == 0){
			throw new Exception("There are no Sprites at present.");
		}

		bool temporaryAnswer;
		foreach(KeyValuePair<string, Sprite> entry in sprites){
			temporaryAnswer = sprites[entry.Key].isClicked();

			if(temporaryAnswer == true){
				return true;
			}
		}

		return false;

	}

	//true if the specified sprite is clicked
	[method: JigsawTab("Sprites Interact")]
	private static bool isThisSpriteClicked(string spriteName){
		if(sprites.ContainsKey(spriteName)){
			return sprites[spriteName].isClicked();		
		}
		return false;
	}*/

	

	[method: JigsawTab("Sprites")]
	public static void printAllSpriteNames(){
		if(sprites.Count == 0){
			System.Console.WriteLine("\tThere are no sprites.");
			return;
		}
		foreach(KeyValuePair<string, Sprite> entry in sprites){
			System.Console.Write(entry.Key + ", ");
		}	

		System.Console.WriteLine();
	}

	private static Background makeBackground(){
		int i = 1; 
		while(backgroundObjs.ContainsKey(System.String.Format("Background{0}", i))){
			i++;
		}
		return makeBackground(System.String.Format("Background{0}", i));
	}
	
	private static Background makeBackground(string name){
		if(backgroundObjs.ContainsKey(name)){
			backgroundObjs[name].hide();
			backgroundObjs.Remove(name);
		}

		Background background = makeBackground(name, getImagePath("landscape.png"));
		return background;
	}

	private static Background makeBackground(string name, string filename){
		if(backgroundObjs.ContainsKey(name)){
			backgroundObjs[name].hide();
			backgroundObjs.Remove(name);
		}

		Background background = new Background(filename);
		backgroundObjs.Add(name, background);
		background.show();
		default_background = name;
		return background;
	}

	
	[JigsawTab("Sprites")]
	public static Sprite makeSprite(){
		int i = 1;
		while(sprites.ContainsKey(System.String.Format("Sprite{0}", i))){
			i++;		
		}
		return makeSprite(System.String.Format("Sprite{0}", i));
	}

	[JigsawTab("Sprites")]
	public static Sprite makeSprite(string spriteName){
		if(sprites.ContainsKey(spriteName)){
			sprites[spriteName].hide();
			sprites.Remove(spriteName);
		}
		if(spriteName == "none"){
			throw new Exception("\'none\' is an invalid Sprite name.");
		}

		Sprite sprite = makeSprite(spriteName, getImagePath("default.png"));
		sprite.moveTo(80, 200);
		return sprite;
	}

	[JigsawTab("Sprites")]
	public static Sprite makeSprite(string spriteName, string filename){
		if(sprites.ContainsKey(spriteName)){
			sprites[spriteName].hide();
			sprites.Remove(spriteName);
		}

		if(spriteName == "none"){
			throw new Exception("\'none\' is an invalid Sprite name.");
		}		

		Sprite sprite = new Sprite(filename);
		sprite.setName(spriteName);
		sprites.Add(spriteName, sprite);
		sprite.moveTo(80, 200);
		default_sprite = spriteName;
		return sprite;
	}


    [method: JigsawTab("Sprites")]
    public static void init() {
	visibleOnStart = true;

	//TODO might need to move this?
	Events.init();
	ManualResetEvent ev = new ManualResetEvent(false);
	Invoke( delegate {
		System.Console.WriteLine(AssemblyDirectory);
		if (window == null) {
		    window = new Window();
		}
	
		window.clear();
		sprites.Clear();
		Background b = makeBackground();
		b.reset();
		Sprite sprite = makeSprite();
		sprite.moveTo(80, 200);
		window.ShowAll();

		if(backgroundChannel != null){
			backgroundChannel.Pause();
		}
		ev.Set();
	    });
	ev.WaitOne();
    }


  [method: JigsawTab("Sprites")]
    public static void init(bool spritesVisibleOnCreation) {
	visibleOnStart = spritesVisibleOnCreation;


	//TODO might need to move this?
	Events.init();
	ManualResetEvent ev = new ManualResetEvent(false);
	Invoke( delegate {
		System.Console.WriteLine(AssemblyDirectory);
		if (window == null) {
		    window = new Window();
		}
	
		window.clear();
		sprites.Clear();
		Background b = makeBackground();
		b.reset();
		Sprite sprite = makeSprite();
		sprite.moveTo(80, 200);
		window.ShowAll();

		if(backgroundChannel != null){
			backgroundChannel.Pause();
		}
		ev.Set();
	    });
	ev.WaitOne();
    }



    [method: JigsawTab("Sprites")]
    public static void makeSpritesVisibleOnCreation(bool visible) {
	visibleOnStart = visible;
    }

    [method: JigsawTab("Sprites")]
    public static void wait(double seconds){
		Graphics.wait(seconds);
	}

    [JigsawTab(null)]
    public static void Invoke (InvokeDelegate invoke) {
	if (needInvoke ())
	    Gtk.Application.Invoke (delegate {
		    invoke ();
		});
	else
	    invoke ();
    }

    [JigsawTab(null)]
    public static bool needInvoke () {
	  return (Thread.CurrentThread.ManagedThreadId != 1);
	}

    public class Window : Graphics.WindowClass {
	public Gtk.Notebook notebook1;
	public Gtk.Label label1, label2;
	public Gtk.VPaned vpaned = new Gtk.VPaned();

	public Window() : base("Calico Sprites", 640, 700) {
	    Remove(canvas);
	    Add(vpaned);
	    canvas.SetSizeRequest(-1, 125);
	    Gtk.Frame frame = new Gtk.Frame();
	    frame.Add(canvas);
	    vpaned.Pack1(frame, true, true);
	    notebook1 = new Gtk.Notebook ();
	    notebook1.CanFocus = true;
	    notebook1.Name = "notebook1";
	    notebook1.CurrentPage = 0;
	    
	    // Notebook tan
		//TODO add images of the costumes to the tabs
	    Gtk.Label w5 = new Gtk.Label ();
	    w5.Visible = true;
	    notebook1.Add (w5);
	    label1 = new Gtk.Label ();
	    label1.Name = "label1";
	    label1.LabelProp = "Sprite Costumes";
	    notebook1.SetTabLabel (w5, label1);
	    
 	    Gtk.Label w6 = new Gtk.Label ();
	    w6.Visible = true;
	    notebook1.Add (w6);
	    label2 = new Gtk.Label ();
	    label2.Name = "label2";
	    label2.LabelProp = "Background Images";
	    notebook1.SetTabLabel (w6, label2);

	    vpaned.Pack2(notebook1, true, true);
	    DeleteEvent += new Gtk.DeleteEventHandler (this.OnDeleteEvent);
	}
	protected void OnDeleteEvent (object sender, Gtk.DeleteEventArgs a)
	{
	    Invoke( delegate {
		    this.Hide();
		});
	    a.RetVal = true;
	    visibleOnStart = true;
		if(backgroundChannel != null){
		    backgroundChannel.Pause();
		}
	}
    }
    
    static string getImagePath(string image) {
	return System.IO.Path.Combine(PictureDirectory, image);
    }
	
	static string getSoundPath(string sound){
		return System.IO.Path.Combine(SoundDirectory, sound);
	}

    static string AssemblyDirectory {
	get {
	    string codeBase = System.Reflection.Assembly.GetExecutingAssembly().CodeBase;
	    System.UriBuilder uri = new System.UriBuilder(codeBase);
	    string path = System.Uri.UnescapeDataString(uri.Path);
	    return System.IO.Path.GetDirectoryName(path);
	}
    }

    static string PictureDirectory {
	get {
	    string path = System.IO.Path.Combine(AssemblyDirectory, "..", "examples");
	    path = System.IO.Path.Combine(path, "images");
	    path = System.IO.Path.Combine(path, "SpriteCostumes");
	    return path;
	}
    }

	static string SoundDirectory{
		get{
			string path = System.IO.Path.Combine(AssemblyDirectory, "..", "examples");
			path = System.IO.Path.Combine(path, "sounds");
			return path;
		}
	}
}
