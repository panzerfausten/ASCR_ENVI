;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;Obtiene la matriz de dispersion de las bandas infrarrojas
function mdiagrama, MA,MB
	ma_col = SIZE(MA)
	ma_ren = SIZE(MA)
	 print, ma_col
	ma_col = ma_col[1]
	ma_ren = ma_ren[2]

	MC=BYTARR(ma_col,ma_ren)
	MD=BYTARR(ma_col,ma_ren)

	FOR col = 0, ma_col-1 DO BEGIN
    	FOR ren = 0, ma_ren-1 DO BEGIN

			MC[MA[col,ren],MB[col,ren]]=1+MC[MA[col,ren],MB[col,ren]]

    	ENDFOR
	ENDFOR

	FOR col = 0, ma_col-1 DO BEGIN
    	FOR ren = 0, ma_ren-1 DO BEGIN

			MD[col,ren]=MC[MA[col,ren],MB[col,ren]]

    	ENDFOR
	ENDFOR

	return, MD
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;Abre la imagen de 8 bits, de una banda o multiespectral.
;La guarda en Imagen, y si es multiespectral la guarda en el arreglo.

function abrir8, A, ren, col,bandas,nuBanda
print, "nbanda"
print, bandas
OpenR,UNIT, A, /GET_LUN		 			;Abre la imagen y le asigna UNIT(un valor)
;print, A
if(bandas eq 1) then begin
	Imagen=BytArr(col,ren)		 		;Hace una matriz del tamano de la imagen
	ReadU, UNIT, Imagen				;Lee el archivo y lo guarda en imagen
return, Imagen						;Regresa el arreglo bidimensional Imagen.
endif else begin
	I1=BytArr(col,ren)
	I2=BytArr(col,ren)
	I3=BytArr(col,ren)
	I4=BytArr(col,ren)
	ReadU, UNIT, I1
	ReadU, UNIT, I2
	ReadU, UNIT, I3
	ReadU, UNIT, I4
	return,  [I1,I2,I3,I4]
endelse

Free_Lun, UNIT				 ;Cierra el archivo UNIT
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;Abre la imagen leyendo en archovo text de idl

function showImage,path
file = path
;--------------------Seleccionar el archivo de la imagen------------------------------------------------------------------------------

filters = ['*.*']										;Variable que toma todas los archivos
														;abre un cuadro de dialogo para escoger la imagen y la guarda en file
;--------------------Abre el encabezado y lo almacena---------------------------------------------------------------------------------

directorio=file+'.hdr' 					  		;Guarda el path de la imagen a mostrar

OpenR,UNIT, directorio, /GET_LUN		  			;Abre el archivo del directorio (que es .hdr) y le asigna un numero

hdr=FStat(UNIT)							  	;Guarda la informacion de cuantos bytes tienes el header

tamaniohdr=hdr.size				  		  	;Guarda el tamanio del archivo header

header=BytArr(tamaniohdr)				  		;Crea una variable header para guardar el encabezado de la imagen

ReadU, UNIT, header						  	;Lee el encabezado y lo guarda en hearder

textoheader=String(header)				  		;Convierte el header en caracteres

;Print, textoheader						  	;Imprime el header
;-------------------------Busca y guarda la informacion del header---------------------------------------------------------------

a=strpos(textoheader,'ENVI')								;Busca si el archivo contiene la palabra 'ENVI', si es verdadero lo abre,
											;de otra forma despliega un mensaje de error.

IF ( a GE 0 ) THEN BEGIN

	col='samples='							  		;Lo que busca en el header, en este caso cuantas columnas
	col=Long(buscanum(textoheader,col,3))						;Lo convierte en valor numerico, llama a la funcion que hace la busqueda


	ren='lines='							  		;Busca en el header los renglones
	ren=Long(buscanum(textoheader,ren,3))


	bandas='bands='							  		;Busca en el header las bandas
	bandas=Long(buscanum(textoheader,bandas,1))


	dtype='datatype='								;Busca el tipo de dato es 12 si la imagen es de 16 bytes
	dtype=Long(buscanum(textoheader,dtype,2))					;En el if selecciona el tipo de dato
Tipo=8
if (dtype EQ 12)then begin
	Tipo=16
endif

obyte='byteorder='							  		;Busca en el header el orden del byte
obyte=Long(buscanum(textoheader,obyte,1))


intleave='interleave='							  		;Busca en el header el interleave
intleave=buscanum(textoheader,intleave,3)

ENDIF ELSE BEGIN

	PRINT, 'ERROR:El archivo selecionado no contiene imagen'			;Mensaje de error

ENDELSE

;---------------------------Abre la imagen-----------------------------------------------------------

NG=256											;Niveles de gris.
if (Tipo EQ 8) then begin								;Selecciona una funcion para leer la imagen,
	Imagen=abrir8(file, ren, col,bandas)						;dependiendo del tipo de datos.
endif else begin
	Imagen=abrir16(file, ren, col, NG)
endelse

;----------------------------Despliega la imagen----------------------------------------------------------------------------------------------------------------------------------------------

Free_Lun, UNIT				 ;Cierra el archivo UNIT

if(bandas eq 1) then begin
	WINDOW, 1, XSIZE=col,YSIZE=ren, TITLE='Imagen Original'					;Abre la ventana para desplegar la imagen
	tv, Image , /ORDER	;
endif

	return, Imagen

end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;Funcion principal Calcula la normalizacion :P

pro ASCR_event, ev
;file = "C:\Users\AmericaIvone\Documents\Torres\imagenes\L3_MSS_ENE80_SantaClara"
;file2 = "C:\Users\AmericaIvone\Documents\Torres\imagenes\L3_MSS_JUL79_SantaClara"
;envi_select, title='selecciona la primer imagen' ,
file = DIALOG_PICKFILE(/READ)
file2 = DIALOG_PICKFILE(/READ)
col = 660
ren = 611
imagen1 = showImage(file) 							;Imagen sujeto (imagen a normalizar)
imagen2 = showImage (file2)							;Imagen referencia (con la que se va a normalizar)

;--------------Extrae de la matriz cada banda----------------------------------------------------------------------------------------------------------------------------------------------------

	banda_a1 = Extrac(imagen1,0,0,col,ren) 			;Bandas de la imagen sujeto
	banda_a2 = Extrac(imagen1,col*1,0,col,ren)
	banda_a3 = Extrac(imagen1,col*2,0,col,ren)
	banda_a4 = Extrac(imagen1,col*3,0,col,ren)

    banda_b1 = Extrac(imagen2,0,0,col,ren)			;Bandas de la imagen de referencia
	banda_b2 = Extrac(imagen2,col*1,0,col,ren)
 	banda_b3 = Extrac(imagen2,col*2,0,col,ren)
	banda_b4 = Extrac(imagen2,col*3,0,col,ren)

	diagrama3= mdiagrama(banda_a3,banda_b3)
	diagrama4= mdiagrama(banda_a4,banda_b4)


	col= SIZE(diagrama4)
	ren = SIZE(diagrama4)
	col = col[1]
	ren = ren[2]

;--------------------------Despliega el diagramas de dispersion para las bandas 3 y 4 (infrarojas)-------------------------------------------------------------------------------------------------------------------------------

WINDOW, 1, XSIZE=900, YSIZE=800, TITLE='Diagrama de Dispersión de la Banda 3'
diagrama3=bytscl(diagrama3)
Loadct, 23
device, decompose=0
plot, banda_a3, banda_b3, color=0
AXIS, YAXIS = 0, YTITLE ='Imagen de Referencia'
AXIS, XAXIS = 0, XTITLE ='Imagen Sujeto'
plots, banda_a3, banda_b3, PSYM=3, color=diagrama3
clicks = 0
CURSOR, X, Y, /DOWN
   WHILE (!MOUSE.button NE 4) DO BEGIN
      CURSOR, X1, Y1,  /DOWN

      PLOTS,[X,X1], [Y,Y1]

	  if ( clicks eq 0) then begin
		x_x1 = x;
		y_y1 = y;
		 print, clicks
	 endif
	   X = X1 & Y = Y1
	  clicks = clicks + 1
	 if ( clicks eq 1) then begin
		x_x2 = x;
		y_y2 = y;
		break;

	 endif


   ENDWHILE

  ca3=float([round(y_y1),round(x_x1)])
  cl3=float([round(y_y2),round(x_x2)])


WINDOW, 2, XSIZE=900, YSIZE=800, TITLE='Diagrama de Dispersión de la Banda 4'
diagrama4=bytscl(diagrama4)
plot, banda_a4, banda_b4, color=0
AXIS, YAXIS = 0, YTITLE ='Imagen de Referencia'
AXIS, XAXIS = 0, XTITLE ='Imagen Sujeto'
plots, banda_a4, banda_b4, PSYM=3, color=diagrama4
clicks = 0
CURSOR, X, Y, /DOWN
   WHILE (!MOUSE.button NE 4) DO BEGIN
      CURSOR, X1, Y1,  /DOWN

      PLOTS,[X,X1], [Y,Y1]

	  if ( clicks eq 0) then begin
		x_x1 = x;
		y_y1 = y;
		 print, clicks
 endif
	   X = X1 & Y = Y1
	  clicks = clicks + 1
	 if ( clicks eq 1) then begin
		x_x2 = x;
		y_y2 = y;
		break;

	 endif


   ENDWHILE

 ca4=Float([round(y_y1),round(x_x1)]) 										;centro de agua (y,x) como en el papper
 cl4=Float([round(y_y2),round(x_x2)])										;centro de land (y,x) como en el papper

base = widget_auto_base(title='Indique el HPW')
sb = widget_base(base, /row,/frame)
wp = widget_param (sb, prompt='HPW', dt=3, uvalue='number' ,/auto)
HPW = auto_wid_mng(base)

HPW = HPW.number


;--------------------------Coeficientes para encontrar la region de cambio------------------------------------------------------------------------------------------------------------
a3=float(0)
a4=float(0)
b3=float(0)
b4=float(0)
HVW3=float(0)
HVW4=float(0)

a3=((cl3[0]-ca3[0])/(cl3[1]-ca3[1]))
b3=ca3[0]-(a3*ca3[1])
a4=(cl4[0]-ca4[0])/(cl4[1]-ca4[1])
b4=ca4[0]-(a4*ca4[1])
HVW3=SQRT(1+(a3^2))*HPW
HVW4=SQRT(1+(a4^2))*HPW

;---------------------------Calcular la region de no cambio para las bandas infrarojas----------------------------------------------------------------------------------------------------
NC=bytarr(col,ren)

FOR col = 0, col-1 DO BEGIN
    FOR ren = 0, ren-1 DO BEGIN
        IF (abs(banda_b3[col,ren]-b3-(a3*banda_a3[col,ren])) LE HVW3) and (abs(banda_b4[col,ren]-b4-(a4*banda_a4[col,ren])) LE HVW4) THEN BEGIN
		NC(col,ren)=1
		ENDIF
    ENDFOR
ENDFOR

;-----------------Coeficientes de la regresion para las bandas usando la region de no cambio------------------------------------------------------------------------------------------------
;Encontrar la media, la varianza y la covarianza de las 4 bandas

number_nc=0.0 				;Hola que hace?, declarando variables, como si no tuviera otra cosa que hacer!!! a la una de la maniana
me_a1=0.0
me_a2=0.0
me_a3=0.0
me_a4=0.0
me_b1=0.0
me_b2=0.0
me_b3=0.0
me_b4=0.0
var1=0.0
var2=0.0
var3=0.0
var4=0.0
covar1=0.0
covar2=0.0
covar3=0.0
covar4=0.0
b_a1=float(banda_a1)
b_a2=float(banda_a2)
b_a3=float(banda_a3)
b_a4=float(banda_a4)
b_b1=float(banda_b1)
b_b2=float(banda_b2)
b_b3=float(banda_b3)
b_b4=float(banda_b4)
coa1=0.0
coa2=0.0
coa3=0.0
coa4=0.0
cob1=0.0
cob2=0.0
cob3=0.0
cob4=0.0

FOR col = 0, col-1 DO BEGIN						;Contar cuantos pixeles caen en la region de no cambio
    FOR ren = 0, ren-1 DO BEGIN
		if (NC[col,ren] EQ 1) then begin
			number_nc=number_nc+1
		endif
    ENDFOR
ENDFOR

FOR col = 0, col-1 DO BEGIN						;Encontrar la media
    FOR ren = 0, ren-1 DO BEGIN
    	if (NC[col,ren] EQ 1) then begin
			me_a1=me_a1+b_a1[col,ren]
			me_a2=me_a2+b_a2[col,ren]
			me_a3=me_a3+b_a3[col,ren]
			me_a4=me_a4+b_a4[col,ren]
			me_b1=me_b1+b_b1[col,ren]
			me_b2=me_b2+b_b2[col,ren]
			me_b3=me_b3+b_b3[col,ren]
			me_b4=me_b4+b_b4[col,ren]
		endif
    ENDFOR
ENDFOR

me_a1=me_a1/number_nc
me_a2=me_a2/number_nc
me_a3=me_a3/number_nc
me_a4=me_a4/number_nc
me_b1=me_b1/number_nc
me_b2=me_b2/number_nc
me_b3=me_b3/number_nc
me_b4=me_b4/number_nc


FOR col = 0, col-1 DO BEGIN						;Encontrar la varianza y covarianza
    FOR ren = 0, ren-1 DO BEGIN
    	if (NC[col,ren] EQ 1) then begin
    		var1=var1+(b_a1[col,ren]-me_a1)^2
			var2=var2+(b_a2[col,ren]-me_a2)^2
			var3=var3+(b_a3[col,ren]-me_a3)^2
			var4=var4+(b_a4[col,ren]-me_a4)^2
			covar1=covar1+((b_a1[col,ren]-me_a1)*(b_b1[col,ren]-me_b1))
			covar2=covar2+((b_a2[col,ren]-me_a2)*(b_b2[col,ren]-me_b2))
			covar3=covar3+((b_a3[col,ren]-me_a3)*(b_b3[col,ren]-me_b3))
			covar4=covar4+((b_a4[col,ren]-me_a4)*(b_b4[col,ren]-me_b4))
		endif
    ENDFOR
ENDFOR

var1=var1/number_nc
var2=var2/number_nc
var3=var3/number_nc
var4=var4/number_nc
covar1=covar1/number_nc
covar2=covar2/number_nc
covar3=covar3/number_nc
covar4=covar4/number_nc


coa1=covar1/var1
coa2=covar2/var2
coa3=covar3/var3
coa4=covar4/var4
cob1=me_b1-(coa1*me_a1)
cob2=me_b2-(coa2*me_a2)
cob3=me_b3-(coa3*me_a3)
cob4=me_b4-(coa4*me_a4)

;------------------------La fucking ASCR--------------------------------------------------------------------------------------------------------------
xn1=bytarr(col,ren)
xn2=bytarr(col,ren)
xn3=bytarr(col,ren)
xn4=bytarr(col,ren)

FOR col = 0, col-1 DO BEGIN
    FOR ren = 0, ren-1 DO BEGIN
    	xn1[col,ren]=byte((coa1*b_a1[col,ren])+cob1)
    	xn2[col,ren]=byte((coa2*b_a2[col,ren])+cob2)
    	xn3[col,ren]=byte((coa3*b_a3[col,ren])+cob3)
    	xn4[col,ren]=byte((coa4*b_a4[col,ren])+cob4)
    ENDFOR
ENDFOR


;---------------------------Desplegar la imagen en colorcillo---------------------------------------------------------------------------------------------

ASCRi=bytarr(3,col,ren)

ASCRi[0,*,*]=xn3[*,*]
ASCRi[1,*,*]=xn2[*,*]
ASCRi[2,*,*]=xn1[*,*]

sujeto=bytarr(3,col,ren)
referencia=bytarr(3,col,ren)

sujeto[0,*,*]=banda_a3[*,*]			;En el rojo pone el 1er infrarrojo
sujeto[1,*,*]=banda_a2[*,*]			;En el verde la banda verde
sujeto[2,*,*]=banda_a1[*,*]			;En el azul la banda roja

referencia[0,*,*]=banda_b3[*,*]
referencia[1,*,*]=banda_b2[*,*]
referencia[2,*,*]=banda_b1[*,*]

device, decomposed=1

;Tvlct, r,g,b
WINDOW, 3, XSIZE=col, YSIZE=ren, TITLE='Imagen Sujeto'
tv, sujeto, true=1, /order

WINDOW, 4, XSIZE=col, YSIZE=ren, TITLE='Imagen Referencia'
tv, referencia, true=1, /order

;WINDOW, 5, XSIZE=col, YSIZE=ren, TITLE='Imagen Normalizada'
;tv, ASCRi, true=1, /order
;

imagePath= "c:\imagenfinal\imagen"
ENVI_WRITE_ENVI_FILE,[[xn1],[xn2],[xn3],[xn4]] , out_name= imagePath, NB=4, nl=ren ,ns=col

close,1


;---------------------------------------------------------------------------------------------------------------------------------------------


END


