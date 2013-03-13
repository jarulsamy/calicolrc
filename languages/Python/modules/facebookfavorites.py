# Inft1004 lecture 10 demonstration
# Simon, October 2012, based on Guzdial & Ericson
# Updated March 2013 to allow for changes in the way Facebook organises pages

import urllib # Get the url library

def fbFavorites(name):
  # Facebook scraping - adapted from Guzdial & Ericson
  # Can be called, for example, with fbFavorites("MarkGuzdial") - Simon doesn't have a Facebook page
  # Facebook no longer has "pagelet_activities_and_interests";
  # instead it has <div class="fbTimelineSection mtm timelineFavorites fbTimelineCompactSection">
  # This requires substantial change to the function.
  # This version also substitutes apostrophes and ampersands for their html codes.

  favourites = ""   # I know it's not the American spelling; you can change it if it offends you!
  # Get the Facebook page for name
  connection = urllib.urlopen("http://www.facebook.com/" + name + "?ref=pb")
  fb = connection.read() # fb now contains the whole facebook page
  connection.close()
  
  # Find the favourites section
  pageLoc = fb.find("timelineFavorites") # pageLoc is the index of that string
  if pageLoc == -1: # If the string was not there
    return "Favorites not found in this facebook page"
  else:
    pageLoc = fb.find('<div class="mediaPageName">', pageLoc)  # Find the next instance after pageLoc
    while pageLoc <> -1: # So long as we keep finding pages
      faveStart = pageLoc + 27  # Gets past '<div class="mediaPageName">'
      faveEnd = fb.find("<", faveStart + 1)  # The position of the next html tag
      thisFave = fb[faveStart:faveEnd]  # Get the actual favourite, the bit between those two indexes
      favourites = favourites + thisFave + ", "
      pageLoc = fb.find('<div class="mediaPageName">', pageLoc + 1)  # Move on to the next one
    # This loop is now over, because there were no more 'mediaPageName' links
    
    # We've processed the favourites that are in sections; now for the ungrouped ones
    # At the start of this section we find several instances of "uiCollapsedList",
    # and at the end we find instances of "&quot;uiCollapsedList"
    pageLoc = fb.find("uiCollapsedList")  # Find the start of the ungrouped favourites
    if pageLoc <> -1:  # If this section exists
      endLoc = fb.find("&quot;uiCollapsedList", pageLoc)  # Find where this section ends
      pageLoc = fb.find("<a href", pageLoc)  # Find the first hyperlink
    while pageLoc <> -1 and pageLoc < endLoc: # So long as we keep finding pages within this section
      faveStart = fb.find(">", pageLoc) + 1  # One beyond the end of the hyperlink tag
      faveEnd = fb.find("<", faveStart + 1)  # The position of the next html tag
      thisFave = fb[faveStart:faveEnd]  # Get the actual favourite, the bit between those two indexes
      favourites = favourites + thisFave + ", "
      pageLoc = fb.find("<a href", pageLoc + 1)  # Move on to the next one
    # This loop is now over, because we've reached the end of the ungrouped favourites
    
    favourites = favourites.replace("&amp;","&")  # Replace every &amp; with an ampersand
    favourites = favourites.replace("&#039;","'")  # Replace apostrophe codes with actual apostrophes
    
    return favourites[:-2]   # Remove the last comma and space, and return the result
