function ujablak() 
{
 html = window.open("","","resizable=1,scrollbars=1,screenX=0,screenY=0,width=680,height=550");
}


//--------design----->

// a  megjeleno oldal hatterszine
var windowbg = "#01488f";
// a  megjeleno oldal hatterkepe
var windowbgpict = "bg1.gif";
// tablazat hattere
var tablebg = "#cdeefe";
// jo valasz hattere
var rightanswbg = "#a5e7a3";
//rossz valasz  hattere
var wronganswbg = "#f7a8bc";
//jo valasz kepe
var ranswpic = "right.gif";
//rossz valasz kepe
var wanswpic = "wrong.gif";
//jellemzes hattere
var resbg = "#cdfffe";

//--design--vege--->



//a kerdesek szama (at kell irni a feladatnak megfeleloen)
var maxque = 15;


//a felhasznalo pontszama
var pszum = 0;

//a valaszok szama
var Anscount = new Array
              Anscount[1] = "3"
              Anscount[2] = "3"
              Anscount[3] = "4"
              Anscount[4] = "3"
              Anscount[5] = "3"
              Anscount[6] = "3"
              Anscount[7] = "3"
              Anscount[8] = "3"
              Anscount[9] = "3"
              Anscount[10] = "3"
              Anscount[11] = "3"
              Anscount[12] = "4"
              Anscount[13] = "3"
              Anscount[14] = "3"
              Anscount[15] = "3"

//a jo valaszok tombje (at kell irni a feladatnak megfeleloen)
var rightAns = new Array
              rightAns[1] = "a"
              rightAns[2] = "c"
              rightAns[3] = "d"
              rightAns[4] = "b"
              rightAns[5] = "c"
              rightAns[6] = "c"
              rightAns[7] = "c"
              rightAns[8] = "b"
              rightAns[9] = "a"
              rightAns[10] = "c"
              rightAns[11] = "b"
              rightAns[12] = "d"
              rightAns[13] = "c"
              rightAns[14] = "a"
              rightAns[15] = "c"
			              
//a kaphato pontok tombje (at kell irni a feladatnak megfeleloen)
var points = new Array
              points[1] = "1"
              points[2] = "1"
              points[3] = "2"
              points[4] = "1"
              points[5] = "1"
              points[6] = "2"
              points[7] = "2"
              points[8] = "4"
              points[9] = "5"
              points[10] = "2"
              points[11] = "3"
              points[12] = "3"
              points[13] = "3"
              points[14] = "2"
              points[15] = "3"


var answer = new Array;			  
var erdemjegy = "Elegtelen";

// az erdemjegy mellett megjeleno kommentar (at kell irni a feladatnak megfeleloen)
var message = new Array 
    message[1] = "Sajnos m&eacute;g nem siker&uuml;lt elsaj&aacute;t&iacute;tanod a tananyagot! M&eacute;gegyszer n&eacute;zd &aacute;t a t&eacute;mak&ouml;rt!"
	message[2] = "Az alapokat m&aacute;r tudod, de ez m&eacute;g nem el&eacute;g. M&eacute;gegyszer n&eacute;zd &aacute;t a tananyagot, &eacute;s pr&oacute;b&aacute;lkozz &uacute;jra!"
	message[3] = "A tanyanyag k&ouml;zel fel&eacute;t m&aacute;r meg&eacute;rtetted. N&eacute;zd meg a hib&aacute;idat &eacute;s a tananyagot m&eacute;gegyszer olvasd el figyelmesen!"
	message[4] = "J&oacute;! A tananyag nagy r&eacute;sz&eacute;t m&aacute;r elsaj&aacute;t&iacute;tottad, de m&eacute;g nem t&ouml;k&eacute;letesen. Miel&otilde;tt a k&ouml;vetkez&otilde; anyagr&eacute;szre m&eacute;sz, n&eacute;zd &aacute;t m&eacute;gegyszer a hib&aacute;idat!"
	message[5] = "Nagyon J&oacute;! A tananyag t&uacute;lnyom&oacute; r&eacute;sz&eacute;t elsaj&aacute;t&iacute;tottad. Mehetsz a k&ouml;vetkez&otilde; anyagr&eacute;szre!"
	

// ossszpontszam kiszamitasa
var szum = 0;
 for (var cv = 1 ; cv <= maxque; cv +=1)			  
 {
 szum = szum + eval (points[cv]);
 }

//a pontszamokhoz tartozo szintek (egyes, kettes, harmas, negyes, otos)
var levels = new Array
 	levels[1] = Math.round(szum*0.25);
	levels[2] = Math.round(szum*0.45);
	levels[3] = Math.round(szum*0.65);
	levels[4] = Math.round(szum*0.85);
	

//valaszok kiertekelese 
function checkit()
{
 for (var cv = 1 ; cv <= maxque; cv +=1)			  
 {
   j = 0;
   sv  = 'document.teszt.a' + cv + '[' + j + '].checked';
   while ((j<Anscount[cv])&&(eval(sv)==false)) 
   {
     j++;
     sv  = 'document.teszt.a' + cv + '[' + j + '].checked';
     }
   
  if (j==Anscount[cv]) {answer[cv] = 'nincs v&aacute;lasz'}
   else 
   switch(j) {
        case 0 : answer[cv] = 'a'; break;
		case 1 : answer[cv] = 'b'; break;
     	case 2 : answer[cv] = 'c'; break;
        case 3 : answer[cv] = 'd'; break;
		default: answer[cv] = 'nincs v&aacute;lasz';
      }
    }
   
    for (var cv = 1 ; cv <= maxque; cv +=1)			  
     {
     if (answer[cv] == rightAns[cv])
	   {
	    pszum +=  eval(points[cv]);
	   }
      }
//erdemjegy kiszamolasa
if (pszum<levels[1]) {erdemjegy = "El&eacute;gtelen"}
else if ((pszum>=levels[1]) && (pszum<levels[2])) {erdemjegy = "El&eacute;gs&eacute;ges"}
else if ((pszum>=levels[2]) && (pszum<levels[3])) {erdemjegy = "K&ouml;zepes"}
else if ((pszum>=levels[3]) && (pszum<levels[4])) {erdemjegy = "J&oacute;"}
else erdemjegy = "Jeles"; 
}
 
// kiirja az eredmenyt, tablazatba foglalva			  
function result(maxim,sz) 
{
// az eredmeny szazalekban
var szaz = Math.round(100*(pszum/sz));
  html.document.writeln('<body bgcolor="' + windowbg + '" background=' + windowbgpict +'>');
  html.document.writeln('<table border="1" cellpadding="1" width="640" bgcolor=' + tablebg + '>');
  html.document.writeln('<tr><td colspan="3" align="center" ><h2>Teszted eredm&eacute;nye</h2></td></tr>');
  html.document.writeln('<tr><td><b>Kérdés</b></td><td><b>Te válaszod</b></td><td><b>Pontszámok</b></td></tr>');
 for (var cv = 1 ; cv <= maxque ; cv +=1) {
  html.document.writeln('<tr><td>' + cv + '.</td><td');
  if (answer[cv] == rightAns[cv])
	   {html.document.writeln('bgcolor="' + rightanswbg + '">' + '<img src="' + ranswpic + '"' + 'hspace="5" alt="Jó válasz">');} 
	   else {html.document.writeln('bgcolor="' + wronganswbg + '">' + '<img src="' + wanswpic + '"' + 'hspace="5" alt="Rossz válasz">');}
  html.document.writeln(answer[cv] + '</td><td>' + points[cv] + '</td></tr>'); }
  html.document.writeln('<tr><td>' + '</td><td>' + '<b>Összes pontszám:</b>' + '</td><td>' + sz + '</td></tr>');
  html.document.writeln('<tr><td>' + '</td><td>' + '<b>A Te pontszámod:</b>' + '</td><td>' + pszum + ' (' +  szaz  + ' %)' + '</td></tr>');
  html.document.writeln('<tr><td>' + '</td><td>' + '<b>&Eacute;rdemjegyed:</b>' + '</td><td>' + erdemjegy + '</td></tr>');
  html.document.writeln('</table>');
  html.document.writeln('<p>&nbsp;</p>');
  html.document.writeln('<table border="1" cellpadding="8" width="640" bgcolor=' + resbg + '>');  
  html.document.writeln('<tr><td>');
  
   switch(erdemjegy) {
        case "El&eacute;gtelen" : html.document.writeln(message[1]); break;
		case "El&eacute;gs&eacute;ges" : html.document.writeln(message[2]); break;
     	case "K&ouml;zepes" : html.document.writeln(message[3]); break;
        case "J&oacute;" : html.document.writeln(message[4]); break;
		case "Jeles" :  html.document.writeln(message[5]); break;		
      }  
  
  html.document.writeln('</td></tr>');
  html.document.writeln('</table>');
  html.document.writeln('<hr>');
  html.document.writeln('</body>');
pszum = 0;  
}
