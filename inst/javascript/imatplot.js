var grouping = @GROUPING@;

var groups = @GROUPS@;

var metadata = @METADATA@; // the first line contains column names

var palette = @PALETTE@;

var metanames = Object.getOwnPropertyNames(metadata);

var width = @LINEWIDTH@;

var opacity = @OPACITY@;

// initialization code
var lines = document.getElementsByClassName("matplot.line");
var i = lines.length;
var selected = new Array(i); 

while (i--)    
    selected[i] = false;

function toggleMetadata(action, id) {  
  var names = metanames;
  
  for (var i = 0, len = names.length; i < len ; i++){
     var metalabel =  document.getElementById("meta.label." + i);
     var metavalue =  document.getElementById("meta.value." + i);
     
     switch(action) {
        case "show":
        	name = names[i]
        	metalabel.textContent = name;
        	metavalue.textContent = metadata[name][id];
          //if(!selected[id]) 
          highlightLine(id, true);
        	break;
        case "hide":
        	metalabel.textContent = metavalue.textContent = "";
          if(!selected[id]) highlightLine(id, false);
        	break;
        default:
  	      throw new Error("Invalid 'action': "+action);
      }
  }
 }

function highlightLines(id){
  var ids = groups[grouping[id]];
  for (var i = 0, len = ids.length; i < len; i++) {
    highlightLine(ids[i], !selected[ids[i]], palette[i]);
    selected[ids[i]] = !selected[ids[i]];
  }
}

function highlightLine(id, on, color) {
  var line = document.getElementById("matplot.line." + id);
  if(!line) 
    return(false);
    
  var parent = line.parentNode;

  // update child offline
  parent.removeChild(line);
  
  if(on) {
    // set opacity
    if(!line.getAttribute('original-stroke-opacity'))
      line.setAttribute('original-stroke-opacity', line.getAttribute('stroke-opacity'));
    line.setAttribute('stroke-opacity', opacity);
    // set stroke width
    if(!line.getAttribute('original-stroke-width'))
      line.setAttribute('original-stroke-width', line.getAttribute('stroke-width'));
    line.setAttribute('stroke-width', width);
    // set color
    if(color){
      if(!line.getAttribute('original-stroke'))
              line.setAttribute('original-stroke', line.getAttribute('stroke'));
      line.setAttribute('stroke', color);
    }
  } else {
    line.setAttribute('stroke-opacity', line.getAttribute('original-stroke-opacity'));
    line.setAttribute('stroke-width', line.getAttribute('original-stroke-width'));
    if(color) line.setAttribute('stroke', line.getAttribute('original-stroke'));
  }
   
  parent.appendChild(line);
  
  return(true);
}
