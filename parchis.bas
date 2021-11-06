DECLARE SUB CONTAR (COMER, COLUMNA, NUM, FICHA(), N, B, X, Y, DOSFIC(), SEGUROS(), CASINI(), DADO)
DECLARE SUB PUENTE (FICHA(), N, B, DADO, SEGUROS(), COLUMNA, COMER, NUM, DOSFIC(), X, Y, CASINI())
DECLARE SUB FICHAS (FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
DECLARE SUB SELECCION (N!, B!, FICHA!(), NUM!, DOSFIC!(), SEGUROS!(), COLUMNA!, X!, Y!, T1!, T2!, T3!, T4!)
DECLARE SUB COLOC.FICHAS (FICHA(), DOSFIC(), N, B, SEGUROS(), COLUMNA)
DECLARE SUB CASILLAS (NUM!, N!, B!, DOSFIC!(), X!, Y!)
DECLARE SUB SALIR (N!, FICHA!())
DECLARE SUB DADOS (N, DADO!, COLUMNA!, FICHA!())
DECLARE SUB TABLERO (FICHA(), N, B, DOSFIC(), SEGUROS(), X, Y, NUM)
DECLARE SUB COLORES (N)


'***********************    PARCHIS 1.2    **************************
'*********                                                 **********
'*********         Daniel Pecos  (jpecosm@nexo.es)         **********
'*********                                                 **********
'*********                 Quick Basic 7.0                 **********
'********************************************************************

'
'     hay que mirar cuando comes cuando sacas
'
'     cuando come a veces cuenta dos veces y tb mueve
'     porque se llama a si mismo en algun sitio
'
'
'
SCREEN 12 'resoluci¢n 640x480
WINDOW

CLS

'תתתתתתתתתת   !!!  DEFINICION DE TODAS LAS VARIABLES !!!   תתתתתתתתתת

DIM FICHA(4, 4) ' n§ de casilla de cada ficha
DIM DOSFIC(4, 4) ' dice a que lado debe ir la ficha
DIM JUGADORE$(4) ' nombre de cada jugador
  FOR N = 1 TO 4: READ JUGADORE$(N): NEXT N
  DATA "Amarillo", "Azul", "Rojo", "Verde"
DIM CASINI(4) ' casilla inicial de las fichas
  FOR I = 1 TO 4: READ CASINI(I): NEXT I
  DATA 5, 22, 39, 56
DIM SEGUROS(12): FOR SGUR = 1 TO 12: READ SEGUROS(SGUR): NEXT SGUR   'seguros
  DATA 5,12,17,22,29,34,39,46,51,56,63,68
'X  coordenada x de las fichas
'Y  coordenada y de las fichas
'NUM  n§ de casilla


'תתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתת
 


'תתתתתתתתתתתתתתתתת  PRESENTACION  תתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתת

'dibujar marco del tablero

COLOR 15: LOCATE 27, 59: PRINT "Daniel Pecos "
COLOR 14: LOCATE 27, 73: PRINT "1"
COLOR 1: LOCATE 27, 74: PRINT "9"
COLOR 4: LOCATE 27, 75: PRINT "9"
COLOR 2: LOCATE 27, 76: PRINT "9"
LINE (450, 13)-(625, 441), 2, B
LINE (17, 17)-(437, 437), 7, B
LINE (13, 13)-(441, 441), 15, B

'dibujar titulo

'!! LINEAS TEMPORALES !!! BORRAR
COLOR 15: LINE (450, 13)-(625, 441), 2, B '!!!!!!!!!!!!
CALL TABLERO(FICHA(), N, B, DOSFIC(), SEGUROS(), X, Y, NUM) '!!!!!!!!11
N = 1: LET TURNO = N'!!!!!!!!!!!!
GOTO TURNO.startB '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

'inicializacion de la seleccion de nombres

COLOR 15
LOCATE 10, 10: PRINT "¨Desea dar nombres a cada jugador? (S/N)"
LOCATE 11, 10: INPUT SN$
IF SN$ = "s" OR SN$ = "S" THEN
        LET R = 12
        FOR N = 1 TO 4
        CALL COLORES(N)
        LOCATE R, 10: PRINT JUGADORE$(N); " ===> "
        LOCATE R, 25: INPUT JUGADORE$(N)
        IF JUGADORE$(N) = "" THEN
              IF N = 1 THEN JUGADORE$(N) = "Amarillo"
              IF N = 2 THEN JUGADORE$(N) = "Azul"
              IF N = 3 THEN JUGADORE$(N) = "Rojo"
              IF N = 4 THEN JUGADORE$(N) = "Verde"
        END IF
        DO UNTIL LEN(JUGADORE$(N)) <= 10
        LOCATE R, 10: PRINT "Es demasiado largo. Vuelve a escribirlo":
        FOR RETARDO = 1 TO 50000: NEXT
        LOCATE R + 1, 10: PRINT "                                        "
        LOCATE R + 1, 10: INPUT JUGADORE$(N)
        LOOP
        LET R = R + 2:
    NEXT N
END IF
FOR RETARDO = 1 TO 50000: NEXT

'תתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתת



'-----------------------  BUCLE PRINCIPAL  --------------------------

CLS
COLOR 15: LINE (450, 13)-(625, 441), 2, B
CALL TABLERO(FICHA(), N, B, DOSFIC(), SEGUROS(), X, Y, NUM)

SELEC.start:
  VIEW SCREEN (451, 14)-(624, 440)
  COLOR 15: LOCATE 3, 64: PRINT "PARCHIS"
            LOCATE 5, 59: PRINT "Vamos a ver quien "
            LOCATE 6, 59: PRINT "empieza."

  DIM A(4)
  LET N = 0: FOR N = 1 TO 4
    LET COLUMNA = 6
    CALL COLORES(N)
    LOCATE COLUMNA + N * 2, 59: PRINT "Aprieta espacio "
    LOCATE (COLUMNA + N * 2 + 1), 59: PRINT "jugador "; JUGADORE$(N)
    CALL DADOS(N, DADO, COLUMNA, FICHA!()): LET A(N) = DADO
    FOR RETARDO = 1 TO 500000: NEXT RETARDO
  NEXT N

  IF A(1) > A(2) AND A(1) > A(3) AND A(1) > A(4) THEN
    COLOR 14
    LOCATE 17, 59: PRINT "T£ comienzas,"
    LOCATE 18, 59: PRINT "jugador Amarillo.": LET N = 1: GOTO SELEC.end
    END IF

  IF A(2) > A(1) AND A(2) > A(3) AND A(2) > A(4) THEN
    COLOR 1
    LOCATE 17, 59: PRINT "T£ comienzas,"
    LOCATE 18, 59: PRINT "jugador Azul.": LET N = 2: GOTO SELEC.end
    END IF

  IF A(3) > A(1) AND A(3) > A(2) AND A(3) > A(4) THEN
    COLOR 4
    LOCATE 17, 59: PRINT "T£ comienzas,"
    LOCATE 18, 59: PRINT "jugador Rojo.": LET N = 3: GOTO SELEC.end
    END IF

  IF A(4) > A(2) AND A(4) > A(3) AND A(4) > A(1) THEN
    COLOR 2
    LOCATE 17, 59: PRINT "T£ comienzas,"
    LOCATE 18, 59: PRINT "jugador Verde.": LET N = 4: GOTO SELEC.end
    END IF


  IF A(1) = A(2) OR A(1) = A(3) OR A(1) = A(4) THEN
    COLOR 15
    LOCATE 17, 59: PRINT "Hab‚is empatado.": LOCATE 18, 59: PRINT "Volver a tirar."
    LET RETARDO = 0: FOR RETARDO = 1 TO 500000: NEXT RETARDO
    GOSUB BORRAR: GOTO SELEC.start
    END IF

  IF A(2) = A(1) OR A(2) = A(3) OR A(2) = A(4) THEN
    COLOR 15
    LOCATE 17, 59: PRINT "Hab‚is empatado.": LOCATE 18, 59: PRINT "Volver a tirar."
    LET RETARDO = 0: FOR RETARDO = 1 TO 500000: NEXT RETARDO
    GOSUB BORRAR: GOTO SELEC.start
    END IF

  IF A(3) = A(1) OR A(3) = A(2) OR A(3) = A(1) THEN
    COLOR 15
    LOCATE 17, 59: PRINT "Hab‚is empatado.": LOCATE 18, 59: PRINT "Volver a tirar."
    LET RETARDO = 0: FOR RETARDO = 1 TO 500000: NEXT RETARDO
    GOSUB BORRAR: GOTO SELEC.start
    END IF

  IF A(4) = A(2) OR A(4) = A(3) OR A(4) = A(4) THEN
    COLOR 15
    LOCATE 17, 59: PRINT "Hab‚is empatado.": LOCATE 18, 59: PRINT "Volver a tirar."
    LET RETARDO = 0: FOR RETARDO = 1 TO 500000: NEXT RETARDO
    GOSUB BORRAR: GOTO SELEC.start
    END IF

SELEC.end:

FOR RETARDO = 1 TO 80000: NEXT RETARDO
GOSUB BORRAR: LET TURNO = N: GOTO TURNO.startB


'תתתתתתתתתתתתתתתתתתתתתתתתתת  BUCLE PRINCIPAL  תתתתתתתתתתתתתתתתתתתתתתתתתת

TURNO.start:
LET N = N + 1: IF N = 5 THEN LET N = 1
GOSUB BORRAR
IF N = TURNO THEN LET CONTAD = CONTAD + 1

TURNO.startB:
LET OTRA.fic = 0: LET PUENTE.B = 0: LET COMER = 0
LET SEIS = 0: LET B = 0: LET COLUMNA = 7: LET CASILLA.casa = 0

'impresi¢n de la cabecera
LINE (490, 27)-(571, 51), 2, B
LOCATE 3, 64: COLOR 15: PRINT "PARCHIS"
LOCATE 5, 63:  PRINT "TURNO N§"; CONTAD + 1
LOCATE COLUMNA, 59: LET COLUMNA = COLUMNA + 1: CALL COLORES(N): PRINT "Puedes tirar,"
LOCATE COLUMNA, 59: LET COLUMNA = COLUMNA + 1: PRINT "jugador "; JUGADORE$(N)

'comienzo de tirada
TURNO.startC:
CALL DADOS(N, DADO!, COLUMNA!, FICHA!())
IF DADO = 6 THEN LET SEIS = SEIS + 1
IF SEIS = 3 AND FICHA(N, B) <> 0 THEN
    LOCATE COLUMNA, 59: PRINT "Te has ido a casa,": LET COLUMNA = COLUMNA + 1
    LOCATE COLUMNA, 59: PRINT "por sacar tres 6": LET COLUMNA = COLUMNA + 1
    LET NUM = FICHA(N, B): LET CASILLA.casa = 2
    CALL FICHAS(FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
    GOTO TURNO.end
  
ELSEIF SEIS = 3 AND FICHA(N, B) = 0 THEN GOTO TURNO.end
END IF


'comprobacion para sacar

COMPRB.sacar:

IF DADO = 5 THEN
  FOR B = 1 TO 4:
    IF FICHA(N, B) = 0 THEN
      'estado inicial de la ficha
      LET FICHA(N, B) = CASINI(N): LET NUM = FICHA(N, B)
      FOR I = 1 TO 4: FOR O = 1 TO 4: IF I = N AND O = B THEN LET O = O + 1: IF O = 5 THEN GOTO SIG.ficha
        IF FICHA(N, B) = FICHA(I, O) THEN
          IF N = I THEN LET PUENTE.B = PUENTE.B + 1: GOTO SIG.ficha
          IF N <> I THEN
            PRINT COMER.B; " "; PUENTE.B: SLEEP
            LET COMER.B = COMER.B + 1
            IF (COMER.B = 2) OR (COMER.B = 1 AND PUENTE.B = 1) THEN
              
              'envia la comida a casa
              LET COMER = 1
              LET TEMP.B = B: LET TEMP.N = N
              LET B = O: LET N = I
              LET CASILLA.casa = 2
              CALL FICHAS(FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
              LET B = TEMP.B: LET N = TEMP.N
              
              'saca la ficha
              LET CASILLA.casa = 1
              CALL FICHAS(FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
              LOCATE COLUMNA, 59: PRINT "Te has comido una": LET COLUMNA = COLUMNA + 1
              LOCATE COLUMNA, 59: PRINT "ficha.": LET COLUMNA = COLUMNA + 1

              GOTO SAC.COM
             
            END IF
          END IF
        END IF

SIG.ficha:

      NEXT O: NEXT I

      IF PUENTE.B = 2 THEN
        LOCATE COLUMNA, 59: PRINT "Hay puente.": LET COLUMNA = COLUMNA + 1:
        LET FICHA(N, B) = 0
        'si hay puente comprueba si puedes mover con otra
        FOR U = 1 TO 4: IF FICHA(N, U) <> 0 THEN GOTO MOVER.ficha
        NEXT U: GOTO TURNO.end
      END IF

      LET CASILLA.casa = 1
      CALL FICHAS(FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
      
      IF CONTAD > 0 THEN LOCATE COLUMNA, 59: PRINT "­Has sacado una": LET COLUMNA = COLUMNA + 1
      IF CONTAD > 0 THEN LOCATE COLUMNA, 59: PRINT "ficha!": LET COLUMNA = COLUMNA + 1
      IF CONTAD = 0 AND OTRA.fic <> 1 THEN LOCATE COLUMNA, 59: PRINT "­Has sacado DOS": LET COLUMNA = COLUMNA + 1
      IF CONTAD = 0 AND OTRA.fic <> 1 THEN LOCATE COLUMNA, 59: PRINT "fichas!": LET COLUMNA = COLUMNA + 1
      IF CONTAD = 0 AND OTRA.fic <> 1 THEN LET OTRA.fic = 1 ELSE GOTO TURNO.end

      'si ha comido, cuenta

SAC.COM: IF COMER = 1 THEN

        LET NUM = FICHA(N, B)
        CALL PUENTE(FICHA(), N, B, DADO, SEGUROS(), COLUMNA, COMER, NUM, DOSFIC(), X, Y, CASINI())
        CALL FICHAS(FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
        LET DADO = 20
        CALL CONTAR(COMER, COLUMNA, NUM, FICHA(), N, B, X, Y, DOSFIC(), SEGUROS(), CASINI(), DADO)
        LET COMER = 0
        GOTO TURNO.end

      END IF


   END IF
  NEXT B
  IF OTRA.fic = 1 THEN GOTO COMPRB.sacar
END IF

FOR Z = 1 TO 4: IF FICHA(N, Z) <> 0 THEN GOTO MOVER.ficha 'para saber si tienes fichas
NEXT Z: GOTO SEIS

MOVER.ficha:

'comprueba si puedes mover alguna ficha
LET Z = 0: FOR Z = 1 TO 4: IF FICHA(N, Z) = 0 THEN GOTO SELEC.ficha
NEXT Z: IF DADO = 6 THEN LET DADO = 7
'falta comprobar si puede mover por un puente

SELEC.ficha: LOCATE COLUMNA, 59: PRINT "¨Qu‚ ficha deseas": LET COLUMNA = COLUMNA + 1
LOCATE COLUMNA, 59: PRINT "mover? (</>)": LET COLUMNA = COLUMNA + 1
CALL SELECCION(N, B, FICHA(), NUM, DOSFIC(), SEGUROS(), COLUMNA, X, Y, T1, T2, T3, T4)

LET FICHA(N, B) = FICHA(N, B) + DADO
IF FICHA(N, B) > 68 THEN LET FICHA(N, B) = FICHA(N, B) - 68
LET NUM = FICHA(N, B)

CALL PUENTE(FICHA(), N, B, DADO, SEGUROS(), COLUMNA, COMER, NUM, DOSFIC(), X, Y, CASINI())

IF COMER = 1 THEN GOTO SEIS
CALL FICHAS(FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
CALL COLORES(N)

SEIS: IF DADO = 6 THEN
  LOCATE COLUMNA, 59: PRINT "­Vuelves a tirar!": LET COLUMNA = COLUMNA + 1
  FOR RETARDO = 1 TO 800000: NEXT RETARDO
  GOTO TURNO.startC
  END IF

TURNO.end: IF N = 4 THEN LET N = 0  '((((((( 700 )))))))

FOR RETARDO = 1 TO 900000: NEXT RETARDO

'siguiente jugador



GOTO TURNO.start

END    '     ­­­­­­­  FIN DE CODIGO FUENTE  !!!!!!!
'תתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתת

BORRAR:
FOR BORRADO = 5 TO 24
LOCATE BORRADO, 59: PRINT "                    "
NEXT BORRADO
'LOCATE 3, 72: PRINT "    "
RETURN

'********************************************************************

SUB CASILLAS (NUM, N, B, DOSFIC(), X, Y)

IF NUM = 0 THEN    'POSICION INICIAL
   IF N = 1 AND B = 1 THEN LET X = 317: Y = 317
   IF N = 1 AND B = 2 THEN LET X = 417: Y = 317
   IF N = 1 AND B = 3 THEN LET X = 317: Y = 417
   IF N = 1 AND B = 4 THEN LET X = 417: Y = 417
   IF N = 2 AND B = 1 THEN LET X = 317: Y = 137
   IF N = 2 AND B = 2 THEN LET X = 317: Y = 37
   IF N = 2 AND B = 3 THEN LET X = 417: Y = 137
   IF N = 2 AND B = 4 THEN LET X = 417: Y = 37
   IF N = 3 AND B = 1 THEN LET X = 137: Y = 137
   IF N = 3 AND B = 2 THEN LET X = 37: Y = 137
   IF N = 3 AND B = 3 THEN LET X = 137: Y = 37
   IF N = 3 AND B = 4 THEN LET X = 37: Y = 37
   IF N = 4 AND B = 1 THEN LET X = 137: Y = 317
   IF N = 4 AND B = 2 THEN LET X = 137: Y = 417
   IF N = 4 AND B = 3 THEN LET X = 37: Y = 317
   IF N = 4 AND B = 4 THEN LET X = 37: Y = 417
END IF
IF NUM = 1 THEN LET X = 267: LET Y = 427: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 2 THEN LET X = 267: LET Y = 407: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 3 THEN LET X = 267: LET Y = 387: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 4 THEN LET X = 267: LET Y = 367: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 5 THEN LET X = 267: LET Y = 347: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 6 THEN LET X = 267: LET Y = 327: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 7 THEN LET X = 267: LET Y = 307: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 8 THEN LET X = 259: LET Y = 287: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 9 THEN LET X = 287: LET Y = 259: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 10 THEN LET X = 307: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 11 THEN LET X = 327: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 12 THEN LET X = 347: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 13 THEN LET X = 367: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 14 THEN LET X = 387: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 15 THEN LET X = 407: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 16 THEN LET X = 427: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 17 THEN LET X = 427: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 18 THEN LET X = 427: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 19 THEN LET X = 407: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 20 THEN LET X = 387: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 21 THEN LET X = 367: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 22 THEN LET X = 347: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 23 THEN LET X = 327: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 24 THEN LET X = 307: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 25 THEN LET X = 287: LET Y = 195: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 26 THEN LET X = 259: LET Y = 168: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 27 THEN LET X = 265.5: LET Y = 148: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 28 THEN LET X = 265.5: LET Y = 128: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 29 THEN LET X = 265.5: LET Y = 108: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 30 THEN LET X = 265.5: LET Y = 88: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 31 THEN LET X = 265.5: LET Y = 68: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 32 THEN LET X = 265.5: LET Y = 48: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 33 THEN LET X = 265.5: LET Y = 28: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 34 THEN LET X = 219: LET Y = 28: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
IF NUM = 35 THEN LET X = 188: LET Y = 28: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 36 THEN LET X = 188: LET Y = 48: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 37 THEN LET X = 188: LET Y = 68: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 38 THEN LET X = 188: LET Y = 88: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 39 THEN LET X = 188: LET Y = 108: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 40 THEN LET X = 188: LET Y = 128: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 41 THEN LET X = 188: LET Y = 148: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 42 THEN LET X = 195: LET Y = 168: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 43 THEN LET X = 168: LET Y = 195: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 44 THEN LET X = 147: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 45 THEN LET X = 127: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 46 THEN LET X = 107: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 47 THEN LET X = 87: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 48 THEN LET X = 67: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 49 THEN LET X = 47: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 50 THEN LET X = 27: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 51 THEN LET X = 27: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
IF NUM = 52 THEN LET X = 27: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 53 THEN LET X = 47: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 54 THEN LET X = 67: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 55 THEN LET X = 87: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 56 THEN LET X = 107: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 57 THEN LET X = 127: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 58 THEN LET X = 147: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 59 THEN LET X = 168: LET Y = 259: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
IF NUM = 60 THEN LET X = 195: LET Y = 286: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 61 THEN LET X = 188: LET Y = 307: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 62 THEN LET X = 188: LET Y = 327: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 63 THEN LET X = 188: LET Y = 347: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 64 THEN LET X = 188: LET Y = 367: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 65 THEN LET X = 188: LET Y = 387: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 66 THEN LET X = 188: LET Y = 407: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 67 THEN LET X = 188: LET Y = 427: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
IF NUM = 68 THEN LET X = 237: LET Y = 427: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5

IF NUM = 100 THEN
  IF N = 1 THEN LET X = 237: LET Y = 407: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
  IF N = 2 THEN LET X = 407: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
  IF N = 3 THEN LET X = 219: LET Y = 48: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
  IF N = 4 THEN LET X = 47: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
END IF

IF NUM = 101 THEN
  IF N = 1 THEN LET X = 237: LET Y = 387: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
  IF N = 2 THEN LET X = 387: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
  IF N = 3 THEN LET X = 219: LET Y = 68: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
  IF N = 4 THEN LET X = 67: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
END IF

IF NUM = 102 THEN
  IF N = 1 THEN LET X = 237: LET Y = 367: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
  IF N = 2 THEN LET X = 367: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
  IF N = 3 THEN LET X = 219: LET Y = 88: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
  IF N = 4 THEN LET X = 87: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
END IF

IF NUM = 103 THEN
  IF N = 1 THEN LET X = 237: LET Y = 347: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
  IF N = 2 THEN LET X = 347: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
  IF N = 3 THEN LET X = 219: LET Y = 108: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
  IF N = 4 THEN LET X = 107: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
END IF

IF NUM = 104 THEN
  IF N = 1 THEN LET X = 237: LET Y = 327: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
  IF N = 2 THEN LET X = 327: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
  IF N = 3 THEN LET X = 219: LET Y = 128: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
  IF N = 4 THEN LET X = 127: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
END IF

IF NUM = 105 THEN
  IF N = 1 THEN LET X = 237: LET Y = 307: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
  IF N = 2 THEN LET X = 307: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
  IF N = 3 THEN LET X = 219: LET Y = 148: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
  IF N = 4 THEN LET X = 147: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
END IF

IF NUM = 106 THEN
  IF N = 1 THEN LET X = 237: LET Y = 287: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
  IF N = 2 THEN LET X = 287: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
  IF N = 3 THEN LET X = 219: LET Y = 168: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
  IF N = 4 THEN LET X = 167: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
END IF

IF NUM = 107 THEN
  IF N = 1 THEN LET X = 237: LET Y = 267: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
  IF N = 2 THEN LET X = 267: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
  IF N = 3 THEN LET X = 219: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
  IF N = 4 THEN LET X = 187: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
END IF

'FALTA LA ULTIMA CASILLA

END SUB

SUB COLOC.FICHAS (FICHA(), DOSFIC(), N, B, SEGUROS(), COLUMNA)

'!! UNICA Y EXCLUSIVAMENTE CAMBIA LA POSICION DE LA FICHA !!

FOR D = 1 TO 4: FOR G = 1 TO 4
IF FICHA(N, B) <> 0 THEN
   IF FICHA(N, B) = FICHA(D, G) THEN
        IF D = N AND G = B THEN GOTO NEX
          IF DOSFIC(D, G) = 0 THEN LET DOSFIC(N, B) = 1: GOTO NEX
          IF DOSFIC(D, G) = 1 THEN LET DOSFIC(N, B) = 0: GOTO NEX
        END IF
    END IF
NEX: NEXT G: NEXT D
END SUB

SUB COLORES (N)

IF N = 1 THEN COLOR 14
IF N = 2 THEN COLOR 1
IF N = 3 THEN COLOR 4
IF N = 4 THEN COLOR 2

END SUB

SUB CONTAR (COMER, COLUMNA, NUM, FICHA(), N, B, X, Y, DOSFIC(), SEGUROS(), CASINI(), DADO)

IF COMER = 1 THEN

        LOCATE COLUMNA, 59: PRINT "­Te cuentas"; DADO; "!": LET FICHA(D, G) = 0: LET COLUMNA = COLUMNA + 1
        LOCATE COLUMNA, 59: PRINT "¨Con qu‚ ficha": LET COLUMNA = COLUMNA + 1
        LOCATE COLUMNA, 59: PRINT "deseas contar?": LET COLUMNA = COLUMNA + 1
        CALL SELECCION(N, B, FICHA(), NUM, DOSFIC(), SEGUROS(), COLUMNA, X, Y, T1, T2, T3, T4)
        LET FICHA(N, B) = FICHA(N, B) + DADO
        IF FICHA(N, B) > 68 THEN LET FICHA(N, B) = FICHA(N, B) - 68
        LET NUM = FICHA(N, B)
        CALL PUENTE(FICHA(), N, B, DADO, SEGUROS(), COLUMNA, COMER, NUM, DOSFIC(), X, Y, CASINI())
        CALL FICHAS(FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)

END IF

END SUB

SUB DADOS (N, DADO, COLUMNA, FICHA())
LET FIN = 0: LET TRUCO = 0
VIEW (0, 0)-(441, 441)
RAN: RANDOMIZE TIMER
LET VAR = INT(RND * 10)
IF VAR > 6 OR VAR < 1 THEN GOTO RAN
FOR DADO = VAR TO 6
LET NUM$ = INKEY$
IF NUM$ = " " THEN LET FIN = 1: GOSUB DADO
IF NUM$ = CHR$(27) THEN CALL SALIR(N, FICHA!()): EXIT SUB
IF NUM$ = "F" THEN
  LET TEMP = N
  FOR T = 1 TO 4: FOR Y = 1 TO 4
    LET N = T: CALL COLORES(N)
    LOCATE 20 + T, 59 + 3 * Y: PRINT FICHA(T, Y)
  NEXT Y: NEXT T
  DO UNTIL D$ <> ""
    LET D$ = INKEY$
  LOOP
  LET D$ = ""
  FOR D = 1 TO 4
  LOCATE 20 + D, 59: PRINT "                  "
  NEXT D
  LET N = TEMP: CALL COLORES(N)
END IF
IF NUM$ = "D" THEN LET TRUCO = 1
IF TRUCO = 1 AND NUM$ = "P" THEN LET TRUCO = 2
IF TRUCO = 2 AND NUM$ = "M" THEN
        GOSUB DADO
        DO UNTIL NUM$ = CHR$(13): LET NUM$ = ""
                DO UNTIL NUM$ <> "":  LET NUM$ = INKEY$: LOOP
        IF NUM$ = "O" THEN
                LET DADO = DADO - 1
                IF DADO = 0 THEN LET DADO = 6:
                GOSUB DADO
            END IF
        IF NUM$ = "P" THEN
                LET DADO = DADO + 1
                IF DADO = 7 THEN LET DADO = 1:
                GOSUB DADO
             END IF
        IF NUM$ = "?" THEN
                LOCATE 26, 59: INPUT TRUC
                LOCATE 26, 59: PRINT "     "
                IF TRUC > 20 THEN GOTO RAN ELSE LET DADO = TRUC: LET FIN = 1: GOTO DADO
            END IF
        LOOP: LET FIN = 1: GOSUB DADO
     END IF
GOSUB DADO
IF DADO = 6 THEN LET DADO = 0
NEXT DADO                       '**************
DADO:                            'LET DADO = 5
                                '**************
LINE (203.5, 203.5)-(250, 250), 0, B
PAINT (205, 205), 0, 0
LINE (215.125, 215.125)-(238.375, 238.375), 15, B
PAINT (216, 216), 15, 15 'borra
PAINT (226.75, 226), 15, 15
PAINT (220.9375, 220.9375), 15, 15
PAINT (232, 232), 15, 15
PAINT (232, 220.9375), 15, 15
PAINT (220.9375, 226.46875#), 15, 15
PAINT (220.9375, 233.21875#), 15, 15
PAINT (232.5625, 226.75), 15, 15   'borra
IF DADO = 1 THEN CIRCLE (226.625, 226.625), 2, 0: PAINT (226.625, 226.625), 0, 0






IF DADO = 2 THEN CIRCLE (220.9375, 220.9375), 2, 0: PAINT (220.9375, 220.9375), 0, 0
IF DADO = 2 THEN CIRCLE (232.5625, 232.5625), 2, 0: PAINT (232.5625, 232.5625), 0, 0




IF DADO = 3 THEN CIRCLE (226.625, 226.625), 2, 0: PAINT (226.625, 226.625), 0, 0
IF DADO = 3 THEN CIRCLE (220.9375, 220.9375), 2, 0: PAINT (220.9375, 220.9375), 0, 0
IF DADO = 3 THEN CIRCLE (232.5625, 232.5625), 2, 0: PAINT (232.5625, 232.5625), 0, 0



IF DADO = 4 THEN CIRCLE (220.9375, 220.9375), 2, 0: PAINT (220.9375, 220.9375), 0, 0
IF DADO = 4 THEN CIRCLE (232.5625, 232.5625), 2, 0: PAINT (232.5625, 232.5625), 0, 0
IF DADO = 4 THEN CIRCLE (232.5625, 220.9375), 2, 0: PAINT (232.5625, 220.9375), 0, 0
IF DADO = 4 THEN CIRCLE (220.9375, 232.5625), 2, 0: PAINT (220.9375, 232.5625), 0, 0


IF DADO = 5 THEN CIRCLE (226.625, 226.625), 2, 0: PAINT (226.625, 226.625), 0, 0
IF DADO = 5 THEN CIRCLE (220.9375, 220.9375), 2, 0: PAINT (220.9375, 220.9375), 0, 0
IF DADO = 5 THEN CIRCLE (232.5625, 232.5625), 2, 0: PAINT (232.5625, 232.5625), 0, 0
IF DADO = 5 THEN CIRCLE (232.5625, 220.9375), 2, 0: PAINT (232.5625, 220.9375), 0, 0
IF DADO = 5 THEN CIRCLE (220.9375, 232.5625), 2, 0: PAINT (220.9375, 232.5625), 0, 0

IF DADO = 6 THEN CIRCLE (220.9375, 220.9375), 2, 0: PAINT (220.9375, 220.9375), 0, 0
IF DADO = 6 THEN CIRCLE (220.9375, 226.75), 2, 0: PAINT (220.9375, 226.75), 0, 0
IF DADO = 6 THEN CIRCLE (220.9375, 232.5625), 2, 0: PAINT (220.9375, 232.5625), 0, 0
IF DADO = 6 THEN CIRCLE (232.5625, 220.9375), 2, 0: PAINT (232.5625, 220.9375), 0, 0
IF DADO = 6 THEN CIRCLE (232.5625, 226.75), 2, 0: PAINT (232.5625, 226.75), 0, 0
IF DADO = 6 THEN CIRCLE (232.5625, 232.5625), 2, 0: PAINT (232.5625, 232.5625), 0, 0
IF FIN = 0 THEN RETURN

END SUB

SUB FICHAS (FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
IF N = 1 THEN COLO = 14
IF N = 2 THEN COLO = 1
IF N = 3 THEN COLO = 4
IF N = 4 THEN COLO = 2

LET TEMP = NUM

IF CASILLA.casa = 1 THEN  'saca ficha

  LET NUM = 0
  CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
  CIRCLE (X, Y), 5, 0: PAINT (X, Y), 0, 0
  LET NUM = CASINI(N)
  CALL COLOC.FICHAS(FICHA(), DOSFIC(), N, B, SEGUROS(), COLUMNA)
  CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
  CIRCLE (X, Y), 5, 7: PAINT (X, Y), COLO, 7
  EXIT SUB

ELSEIF CASILLA.casa = 2 THEN 'ficha muerta

  CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
  CIRCLE (X, Y), 5, 0: PAINT (X, Y), 0, 0
  FOR S = 1 TO 12
    IF NUM = SEGUROS(S) THEN CIRCLE (X, Y), 5, 8: PAINT (X, Y), 8, 7
    IF NUM = 5 THEN CIRCLE (X, Y), 5, 14: PAINT (X, Y), 14, 7
    IF NUM = 22 THEN CIRCLE (X, Y), 5, 1: PAINT (X, Y), 1, 7
    IF NUM = 39 THEN CIRCLE (X, Y), 5, 4: PAINT (X, Y), 4, 7
    IF NUM = 56 THEN CIRCLE (X, Y), 5, 2: PAINT (X, Y), 2, 7

  NEXT S

  LET NUM = 0
  CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
  CIRCLE (X, Y), 5, 7: PAINT (X, Y), COLO, 7
  LET FICHA(N, B) = 0
  EXIT SUB

ELSE   'movimiento normal

  LET NUM = NUM - DADO
  IF NUM < 1 THEN LET NUM = 68 - ABS(NUM)
  
END IF


DO UNTIL NUM = TEMP
        FOR RETARDO = 1 TO 200000: NEXT

        CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
        CIRCLE (X, Y), 5, 0: PAINT (X, Y), 0, 0
        FOR S = 1 TO 12
          IF NUM = SEGUROS(S) THEN CIRCLE (X, Y), 5, 8: PAINT (X, Y), 8, 7
          IF NUM = 5 THEN CIRCLE (X, Y), 5, 14: PAINT (X, Y), 14, 7
          IF NUM = 22 THEN CIRCLE (X, Y), 5, 1: PAINT (X, Y), 1, 7
          IF NUM = 39 THEN CIRCLE (X, Y), 5, 4: PAINT (X, Y), 4, 7
          IF NUM = 56 THEN CIRCLE (X, Y), 5, 2: PAINT (X, Y), 2, 7

        NEXT S
        LET DADO.2 = DADO.2 + 1
        'LOCATE 3, 73: PRINT DADO.2
        LET NUM = NUM + 1
        IF NUM > 68 THEN LET NUM = NUM - 68
        LET FICHA(N, B) = NUM
        CALL COLOC.FICHAS(FICHA(), DOSFIC(), N, B, SEGUROS(), COLUMNA)
        CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
        CIRCLE (X, Y), 5, 7: PAINT (X, Y), COLO, 7


LOOP
LET NUM = TEMP
LET FICHA(N, B) = NUM

END SUB

SUB PUENTE (FICHA(), N, B, DADO, SEGUROS(), COLUMNA, COMER, NUM, DOSFIC(), X, Y, CASINI())

LET FICHA(N, B) = FICHA(N, B) - DADO
  IF FICHA(N, B) < 1 THEN LET FICHA(N, B) = 68 - ABS(FICHA(N, B))
FOR A = 1 TO 4: FOR C = 1 TO 4

'תתתתתתתתתתתתתתתתתתתתתתתתתת PUENTE תתתתתתתתתתתתתתתתתתתתתתתתתתתתת

  FOR D = 1 TO 4: FOR E = 1 TO 4

    IF FICHA(A, C) = FICHA(D, E) AND FICHA(A, C) <> FICHA(N, B) AND FICHA(D, E) <> FICHA(N, B) AND A = D AND C <> E AND FICHA(A, C) <> 0 THEN

      'si el puente esta antes de la casilla 68
      IF FICHA(A, C) > FICHA(N, B) THEN
        IF FICHA(A, C) - FICHA(N, B) <= DADO THEN
          LOCATE COLUMNA, 59: PRINT "No puedes mover,": LET COLUMNA = COLUMNA + 1
          LOCATE COLUMNA, 59: PRINT "porque hay puente.": LET COLUMNA = COLUMNA + 1
          LOCATE COLUMNA, 59: PRINT "Elige otra ficha.": LET COLUMNA = COLUMNA + 1
          
          CALL SELECCION(N!, B!, FICHA!(), NUM!, DOSFIC!(), SEGUROS!(), COLUMNA!, X!, Y!, T1!, T2!, T3!, T4!)
          LOCATE COLUMNA - 3, 59: PRINT "                 "
          LOCATE COLUMNA - 2, 59: PRINT "                 "
          LOCATE COLUMNA - 1, 59: PRINT "                 "
          LET COLUMNA = COLUMNA - 3


          LET FICHA(N, B) = FICHA(N, B) + DADO
          IF FICHA(N, B) > 68 THEN LET FICHA(N, B) = FICHA(N, B) - 68
          LET NUM = FICHA(N, B)
          CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
          CALL PUENTE(FICHA(), N, B, DADO, SEGUROS(), COLUMNA, COMER, NUM, DOSFIC(), X, Y, CASINI())
          EXIT SUB

          END IF
      END IF

      IF FICHA(A, C) < FICHA(N, B) THEN
        IF 68 - FICHA(N, B) + FICHA(A, C) <= DADO THEN
          'PRINT FICHA(N, B); " "; FICHA(A, C); " "; DADO
          LOCATE COLUMNA, 59: PRINT "No puedes mover,": LET COLUMNA = COLUMNA + 1
          LOCATE COLUMNA, 59: PRINT "porque hay puente.": LET COLUMNA = COLUMNA + 1
          LOCATE COLUMNA, 59: PRINT "Elige otra ficha.": LET COLUMNA = COLUMNA + 1
          
          CALL SELECCION(N!, B!, FICHA!(), NUM!, DOSFIC!(), SEGUROS!(), COLUMNA!, X!, Y!, T1!, T2!, T3!, T4!)
          LOCATE COLUMNA - 3, 59: PRINT "                 "
          LOCATE COLUMNA - 2, 59: PRINT "                 "
          LOCATE COLUMNA - 1, 59: PRINT "                 "
          LET COLUMNA = COLUMNA - 3

          LET FICHA(N, B) = FICHA(N, B) + DADO
          IF FICHA(N, B) > 68 THEN LET FICHA(N, B) = FICHA(N, B) - 68
          LET NUM = FICHA(N, B)
          CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
          CALL PUENTE(FICHA(), N, B, DADO, SEGUROS(), COLUMNA, COMER, NUM, DOSFIC(), X, Y, CASINI())
          EXIT SUB

          END IF
      END IF

    END IF

  NEXT E: NEXT D



'תתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתתת

'§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ COMER §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

'comprobacion para dos fichas en la misma casilla  (COMER)



IF FICHA(A, C) = FICHA(N, B) + DADO AND A <> N THEN
  IF FICHA(A, C) <> SEGUROS(1) AND FICHA(A, C) <> SEGUROS(2) AND FICHA(A, C) <> SEGUROS(3) AND FICHA(A, C) <> SEGUROS(4) AND FICHA(A, C) <> SEGUROS(5) AND FICHA(A, C) <> SEGUROS(6) AND FICHA(A, C) <> SEGUROS(7) AND FICHA(A, C) <> SEGUROS(8) AND  _
FICHA(A, C) <> SEGUROS(9) AND FICHA(A, C) <> SEGUROS(10) AND FICHA(A, C) <> SEGUROS(11) AND FICHA(A, C) <> SEGUROS(12) THEN

    LET COMER = 1
    LET TEMP.B = B: LET TEMP.N = N
    LET B = C: LET N = A
    LET CASILLA.casa = 2
    CALL FICHAS(FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
    LET B = TEMP.B: LET N = TEMP.N
    LET CASILLA.casa = 0

    IF N = 1 THEN COLO = 14
    IF N = 2 THEN COLO = 1
    IF N = 3 THEN COLO = 4
    IF N = 4 THEN COLO = 2
      
    LET FICHA(N, B) = FICHA(N, B) + DADO
    IF FICHA(N, B) > 68 THEN LET FICHA(N, B) = FICHA(N, B) - 68
    LET NUM = FICHA(N, B)
    CALL PUENTE(FICHA(), N, B, DADO, SEGUROS(), COLUMNA, COMER, NUM, DOSFIC(), X, Y, CASINI())
    CALL FICHAS(FICHA!(), DOSFIC!(), SEGUROS!(), N!, B!, CASILLA.casa, DADO!, COLUMNA, CASINI!(), NUM)
    LET DADO = 20: LET COMER.2 = COMER.2 + 1
    CALL CONTAR(COMER, COLUMNA, NUM, FICHA(), N, B, X, Y, DOSFIC(), SEGUROS(), CASINI(), DADO)

  END IF

END IF

'§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

NEXT C: NEXT A

LET FICHA(N, B) = FICHA(N, B) + DADO

END SUB

SUB SALIR (N, FICHA!())

VIEW SCREEN (451, 14)-(624, 440)
LINE (470, 361)-(600, 406), 13, B: PAINT (471, 362), 13
LINE (475, 366)-(595, 401), 0, B: PAINT (491, 382), 0, 0
LOCATE 24, 61: COLOR 15: PRINT "¨Deseas salir?"
LOCATE 25, 61: PRINT "(S/N)"
LOCATE 25, 68: INPUT SALR$: IF SALR$ = "S" OR SALR$ = "s" THEN END
FOR BORRADO = 23 TO 26: LOCATE BORRADO, 61: PRINT "                ": NEXT BORRADO: PAINT (471, 362), 0
CALL DADOS(N, DADO, COLUMNA, FICHA!()): EXIT SUB
END SUB

SUB SELECCION (N, B, FICHA(), NUM, DOSFIC(), SEGUROS(), COLUMNA, X, Y, T1, T2, T3, T4)

LET B = 1: IF FICHA(N, B) = 0 THEN LET B = B + 1: IF B = 5 THEN LET B = 1


LET NUM = FICHA(N, B): CALL COLOC.FICHAS(FICHA(), DOSFIC(), N, B, SEGUROS(), COLUMNA)
CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
CIRCLE (X, Y), 6, 15: CIRCLE (X, Y), 7, 15

START.loop: DO UNTIL C$ = CHR$(13)
        IF B = 0 THEN LET B = 4
        IF B = 5 THEN LET B = 1
        LET C$ = INKEY$
        IF FICHA(N, B) = 0 THEN LET B = B + 1: GOTO START.loop
        IF C$ = "P" THEN

                CIRCLE (X, Y), 5, 7: CIRCLE (X, Y), 6, 0: CIRCLE (X, Y), 7, 0
                FOR A = 1 TO 12
                  IF FICHA(N, B) = SEGUROS(A) THEN CIRCLE (X, Y), 6, 8: CIRCLE (X, Y), 7, 8
                NEXT A
                IF FICHA(N, B) = 5 THEN CIRCLE (X, Y), 6, 14: CIRCLE (X, Y), 7, 14
                IF FICHA(N, B) = 22 THEN CIRCLE (X, Y), 6, 1: CIRCLE (X, Y), 7, 1
                IF FICHA(N, B) = 39 THEN CIRCLE (X, Y), 6, 4: CIRCLE (X, Y), 7, 4
                IF FICHA(N, B) = 56 THEN CIRCLE (X, Y), 6, 2: CIRCLE (X, Y), 7, 2
                                                       
                LET B = B + 1: IF B = 5 THEN LET B = 1
                IF FICHA(N, B) = 0 THEN LET B = B + 1: IF B = 5 THEN LET B = 1
                IF FICHA(N, B) = 0 THEN LET B = B + 1: IF B = 5 THEN LET B = 1
                
                LET NUM = FICHA(N, B)
                CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
                CIRCLE (X, Y), 6, 15: CIRCLE (X, Y), 7, 15
            END IF

        IF C$ = "O" THEN

                CIRCLE (X, Y), 5, 7: CIRCLE (X, Y), 6, 0: CIRCLE (X, Y), 7, 0
                FOR A = 1 TO 12
                  IF FICHA(N, B) = SEGUROS(A) THEN CIRCLE (X, Y), 6, 8: CIRCLE (X, Y), 7, 8
                NEXT A
                IF FICHA(N, B) = 5 THEN CIRCLE (X, Y), 6, 14: CIRCLE (X, Y), 7, 14
                IF FICHA(N, B) = 22 THEN CIRCLE (X, Y), 6, 1: CIRCLE (X, Y), 7, 1
                IF FICHA(N, B) = 39 THEN CIRCLE (X, Y), 6, 4: CIRCLE (X, Y), 7, 4
                IF FICHA(N, B) = 56 THEN CIRCLE (X, Y), 6, 2: CIRCLE (X, Y), 7, 2
                
                LET B = B - 1: IF B = 0 THEN LET B = 4
                IF FICHA(N, B) = 0 THEN LET B = B - 1: IF B = 0 THEN LET B = 4: IF B = 0 THEN LET B = 4
                IF FICHA(N, B) = 0 THEN LET B = B - 1: IF B = 0 THEN LET B = 4: IF B = 0 THEN LET B = 4

                LET NUM = FICHA(N, B)
                CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
                CIRCLE (X, Y), 6, 15: CIRCLE (X, Y), 7, 15
              END IF

        IF C$ = CHR$(13) THEN

                CIRCLE (X, Y), 5, 7: CIRCLE (X, Y), 6, 0: CIRCLE (X, Y), 7, 0 ' borra la seleccion
                FOR A = 1 TO 12
                  IF FICHA(N, B) = SEGUROS(A) THEN CIRCLE (X, Y), 6, 8: CIRCLE (X, Y), 7, 8
                NEXT A
                IF FICHA(N, B) = 5 THEN CIRCLE (X, Y), 6, 14: CIRCLE (X, Y), 7, 14
                IF FICHA(N, B) = 22 THEN CIRCLE (X, Y), 6, 1: CIRCLE (X, Y), 7, 1
                IF FICHA(N, B) = 39 THEN CIRCLE (X, Y), 6, 4: CIRCLE (X, Y), 7, 4
                IF FICHA(N, B) = 56 THEN CIRCLE (X, Y), 6, 2: CIRCLE (X, Y), 7, 2

                LET NUM = FICHA(N, B)
                CALL COLOC.FICHAS(FICHA(), DOSFIC(), N, B, SEGUROS(), COLUMNA)
                CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
        END IF

        IF C$ = "F" THEN
                LET TEMP = N
                FOR T = 1 TO 4: FOR Y = 1 TO 4
                LET N = T: CALL COLORES(N)
                LOCATE 20 + T, 59 + 3 * Y: PRINT FICHA(T, Y)
                NEXT Y: NEXT T
                DO UNTIL D$ <> ""
                LET D$ = INKEY$
                LOOP
                LET D$ = ""
                FOR D = 1 TO 4
                LOCATE 20 + D, 59: PRINT "                  "
                NEXT D
                LET N = TEMP: CALL COLORES(N)
        END IF

LOOP
LET C$ = ""
END SUB

SUB TABLERO (FICHA(), N, B, DOSFIC(), SEGUROS(), X, Y, NUM)

COLOR 15: LOCATE 27, 59: PRINT "Daniel Pecos "
COLOR 14: LOCATE 27, 73: PRINT "1"
COLOR 1: LOCATE 27, 74: PRINT "9"
COLOR 4: LOCATE 27, 75: PRINT "9"
COLOR 2: LOCATE 27, 76: PRINT "8"
LINE (450, 13)-(625, 441), 2, B

'LINEAS
'HORIZONT.
LINE (17, 250.5)-(437, 250.5), 7
LINE (17, 203.5)-(437, 203.5), 7
FOR HOR = 17 TO 437 STEP 20
IF HOR = 157 THEN LET HOR = HOR + 140
LINE (157, HOR)-(297, HOR), 7
NEXT HOR

'VERT.
LINE (203.5, 17)-(203.5, 437), 7
LINE (250, 17)-(250, 437), 7
FOR VER = 17 TO 437 STEP 20
IF VER = 157 THEN LET VER = VER + 140
LINE (VER, 157)-(VER, 297), 7
NEXT VER

LINE (17, 17)-(437, 437), 7, B
LINE (13, 13)-(441, 441), 15, B
LINE (297, 157)-(157, 297), 7, B
LINE (17, 17)-(157, 157), 7, B      'rojo
LINE (437, 437)-(297, 297), 7, B    'amarillo
LINE (17, 437)-(157, 297), 7, B     'verde
LINE (437, 17)-(297, 157), 7, B     'azul
LINE (180, 274)-(274, 180), 8, B    'central
PAINT (227, 227), 0, 8                'para borrar el cuadro central
LINE (180, 274)-(274, 180), 7, B
LINE (157, 157)-(297, 297), 7
LINE (297, 157)-(157, 297), 7

PINTAR:
CIRCLE (87, 87), 45, 7: PAINT (87, 87), 4, 7 'rojo
CIRCLE (87, 367), 45, 7: PAINT (87, 367), 2, 7'verde
CIRCLE (367, 87), 45, 7: PAINT (367, 87), 1, 7 'azul
CIRCLE (367, 367), 45, 7: PAINT (367, 367), 14, 7   'amarillo
FOR PIN = 47 TO 407 STEP 20: LET COL = 2
IF PIN > 207 THEN LET COL = 1
PAINT (PIN, 227), COL, 7
NEXT PIN
FOR PIN = 47 TO 407 STEP 20: LET COL = 4
IF PIN > 207 THEN LET COL = 14
PAINT (227, PIN), COL, 7
NEXT PIN


'SALIDAS
PAINT (180, 107), 4, 7
PAINT (107, 256.5), 2, 7
PAINT (273, 347), 14, 7
PAINT (347, 163.5), 1, 7

'SEGUROS
PAINT (27, 227), 8, 7
PAINT (427, 227), 8, 7
PAINT (227, 27), 8, 7
PAINT (227, 427), 8, 7
PAINT (347, 273.5), 8, 7
PAINT (273.5, 105), 8, 7
PAINT (107, 167), 8, 7
PAINT (167, 350), 8, 7
'
'fichas ini
LET TEM1 = N: LET TEM2 = B
FOR T = 1 TO 4
FOR U = 1 TO 4
LET N = T: LET B = U
LET NUM = FICHA(N, B): CALL COLOC.FICHAS(FICHA(), DOSFIC(), N, B, SEGUROS(), COLUMNA): CALL CASILLAS(NUM, N, B, DOSFIC(), X, Y)
IF T = 1 THEN COLO = 14
IF T = 2 THEN COLO = 1
IF T = 3 THEN COLO = 4
IF T = 4 THEN COLO = 2
CIRCLE (X, Y), 5, 7: PAINT (X, Y), COLO, 7:
NEXT U: NEXT T
LET N = TEM1: LET B = TEM2

END SUB

SUB valores.de.casillas    '!!!!! BORRAR !!!!!
'                                                IF num = 1 THEN LET X = 267: LET Y = 427: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
''                                                IF num = 2 THEN LET X = 267: LET Y = 407: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 3 THEN LET X = 267: LET Y = 387: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 4 THEN LET X = 267: LET Y = 367: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 5 THEN LET X = 267: LET Y = 347: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 6 THEN LET X = 267: LET Y = 327: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 7 THEN LET X = 267: LET Y = 307: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 8 THEN LET X = 259: LET Y = 287: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 9 THEN LET X = 287: LET Y = 259: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                               IF num = 10 THEN LET X = 307: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                              IF num = 11 THEN LET X = 327: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                             IF num = 12 THEN LET X = 347: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                            IF num = 13 THEN LET X = 367: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                           IF num = 14 THEN LET X = 387: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                          IF num = 15 THEN LET X = 407: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                         IF num = 16 THEN LET X = 427: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                        IF num = 17 THEN LET X = 427: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                       IF num = 18 THEN LET X = 427: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                      IF num = 19 THEN LET X = 407: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 20 THEN LET X = 387: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 21 THEN LET X = 367: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 22 THEN LET X = 347: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 23 THEN LET X = 327: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 24 THEN LET X = 307: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 25 THEN LET X = 287: LET Y = 195: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 26 THEN LET X = 259: LET Y = 168: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 27 THEN LET X = 265.5: LET Y = 148: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 28 THEN LET X = 265.5: LET Y = 128: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 29 THEN LET X = 265.5: LET Y = 108: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 30 THEN LET X = 265.5: LET Y = 88: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 31 THEN LET X = 265.5: LET Y = 68: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 32 THEN LET X = 265.5: LET Y = 48: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
''                                                IF num = 33 THEN LET X = 265.5: LET Y = 28: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 34 THEN LET X = 219: LET Y = 28: IF DOSFIC(N, B) = 1 THEN LET X = X + 15.5
'                                                IF num = 35 THEN LET X = 188: LET Y = 28: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 36 THEN LET X = 188: LET Y = 48: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 37 THEN LET X = 188: LET Y = 68: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 38 THEN LET X = 188: LET Y = 88: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
 '                                               IF num = 39 THEN LET X = 188: LET Y = 108: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 40 THEN LET X = 188: LET Y = 128: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 41 THEN LET X = 188: LET Y = 148: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 42 THEN LET X = 195: LET Y = 168: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 43 THEN LET X = 168: LET Y = 195: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 44 THEN LET X = 147: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 45 THEN LET X = 127: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 46 THEN LET X = 107: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 47 THEN LET X = 87: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 48 THEN LET X = 67: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 49 THEN LET X = 47: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 50 THEN LET X = 27: LET Y = 188: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 51 THEN LET X = 27: LET Y = 235: IF DOSFIC(N, B) = 1 THEN LET Y = Y - 15.5
'                                                IF num = 52 THEN LET X = 27: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                                IF num = 53 THEN LET X = 47: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                                IF num = 54 THEN LET X = 67: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                                IF num = 55 THEN LET X = 87: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                                IF num = 56 THEN LET X = 107: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                                IF num = 57 THEN LET X = 127: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                                IF num = 58 THEN LET X = 147: LET Y = 266: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                                IF num = 59 THEN LET X = 168: LET Y = 259: IF DOSFIC(N, B) = 1 THEN LET Y = Y + 15.5
'                                                IF num = 60 THEN LET X = 195: LET Y = 286: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 61 THEN LET X = 188: LET Y = 307: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 62 THEN LET X = 188: LET Y = 327: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
 '                                               IF num = 63 THEN LET X = 188: LET Y = 347: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 64 THEN LET X = 188: LET Y = 367: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 65 THEN LET X = 188: LET Y = 387: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 66 THEN LET X = 188: LET Y = 407: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'                                                IF num = 67 THEN LET X = 188: LET Y = 427: IF DOSFIC(N, B) = 1 THEN LET X = X - 15.5
'
END SUB

