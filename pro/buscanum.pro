
FUNCTION buscanum, texto,palabra,no				;Funcion que busca en el texto mandado un valor despues de la palabra indicada


header=strcompress(texto, /REMOVE_ALL)			;Quita los espacios en blaco del header

a=Strpos(header,palabra)		  							;Busco las columnas

b=strmid(header,a+strlen(palabra),no)	      				;Extrae el valor de las columnas pero como caracter de las columnas


RETURN, b

END