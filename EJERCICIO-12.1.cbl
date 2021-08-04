      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJER-12.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT MATI    ASSIGN TO
           'D:\cobol-1\Archivo\Entrada.TXT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS  IS FS-MATI.

           SELECT LISTADO ASSIGN TO
           'D:\cobol-1\Archivo\Planilla.TXT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS FS-LISTADO.
       DATA DIVISION.
       FILE SECTION.

       FD  MATI.
           01  REG-MATI-FD             PIC X(02) VALUE ' '.

       FD  LISTADO.
           01  REG-LISTADO-FD          PIC X(133) VALUE ' '.

       WORKING-STORAGE SECTION.

       77  FS-MATI                     PIC XX VALUE '  '.
           88 88-MATI-FIN                     VALUE '10'.

       77  WS-MATI-OPEN                PIC X  VALUE 'N'.
           88 88-MATI-SI                      VALUE 'S'.
           88 88-MATI-NO                      VALUE 'N'.

       77  FS-LISTADO                  PIC XX VALUE " ".
       77  WS-LISTADO-OPEN             PIC X  VALUE 'N'.
           88  88-LISTADO-SI                  VALUE 'S'.
           88  88-LISTADO-NO                  VALUE 'N'.

       77  WS-AUX-LISTA                PIC X(132) VALUE ' '.

       77  WS-CONT-REG-LEIDOS          PIC 9(05)  VALUE 0.

       77  CONT-MASC                   PIC 99 VALUE 00.
       77  CONT-FEME                   PIC 99 VALUE 00.

       77  MAS-CAS                     PIC 99 VALUE 00.
       77  MAS-SOL                     PIC 99 VALUE 00.
       77  MAS-VIU                     PIC 99 VALUE 00.
       77  MAS-DIV                     PIC 99 VALUE 00.

       77  FEM-CAS                     PIC 99 VALUE 00.
       77  FEM-SOL                     PIC 99 VALUE 00.
       77  FEM-VIU                     PIC 99 VALUE 00.
       77  FEM-DIV                     PIC 99 VALUE 00.

       01  WS-TITULOS-1.
           05  FILLER                  PIC X(14)  VALUE 'GENERO'.
           05  FILLER                  PIC X(11)  VALUE 'SOLTEROS'.
           05  FILLER                  PIC X(10)  VALUE 'CASADOS'.
           05  FILLER                  PIC X(14)  VALUE 'DIVORCIADOS'.
           05  FILLER                  PIC X(11)  VALUE 'VIUDOS'.
           05  FILLER                  PIC X(05)  VALUE 'TOTAL'.

       01  WS-TITULO-2.
           05 FILLER                   PIC X(65) VALUE ALL '-'.

       01  WS-DETALLE.
           05 WS-GEN                   PIC X(09).
           05 FILLER                   PIC X(05) VALUE ' '.
           05 WS-SOLT                  PIC 9(08).
           05 FILLER                   PIC X(03) VALUE ' '.
           05 WS-CASA                  PIC 9(07).
           05 FILLER                   PIC X(03) VALUE ' '.
           05 WS-DIVO                  PIC 9(11).
           05 FILLER                   PIC X(03) VALUE ' '.
           05 WS-VIU                   PIC 9(06).
           05 FILLER                   PIC X(05) VALUE ' '.
           05 WS-TOTAL                 PIC 9(05).
           05 FILLER                   PIC X VALUE " ".

       01  WS-ESTRUCTURA.
           03  WS-GENERO               PIC X VALUE ' '.
           03  WS-ESTADO-CIVIL         PIC X VALUE ' '.

       01  WS-TOTALES.
           05 FILLER                   PIC X(45) VALUE ALL '*'.
           05 FILLER                   PIC X(15) VALUE 'SUMA DE TOTAL:'.
           05 WS-SUMA-TOTAL            PIC 9(05) VALUE 0.

       01  WS-ULTIMA-LINEA.
           05 FILLER                   PIC X(10) VALUE 'REGISTROS '.
           05 FILLER                   PIC X(08) VALUE 'LEIDOS: '.
           05 WS-REG-LEIDOS            PIC 9(05) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO UNTIL 88-MATI-FIN.

           PERFORM 3000-FINALIZO.
      *----------------------------------------------------------------*
       1000-INICIO.

           PERFORM 1100-OPEN-MATI.

           PERFORM 1200-OPEN-LISTADO.

           MOVE WS-TITULOS-1          TO WS-AUX-LISTA.
           PERFORM 1300-IMPRIMO-TITULOS.

           MOVE WS-TITULO-2          TO WS-AUX-LISTA.
           PERFORM 1300-IMPRIMO-TITULOS.

           PERFORM 1400-PRIMERA-LECTURA.

           IF 88-MATI-FIN
              DISPLAY "ARCHIVO VACIO"
           END-IF.

       1100-OPEN-MATI.

           OPEN INPUT MATI.

           IF FS-MATI EQUALS '00'
              MOVE 'S' TO WS-MATI-OPEN
           ELSE
              DISPLAY 'ERROR OPEN EN LISTADO'
              DISPLAY 'ERROR CODE: ' FS-MATI
              PERFORM 3000-FINALIZO
           END-IF.

       1200-OPEN-LISTADO.

           OPEN OUTPUT LISTADO.

           IF FS-LISTADO EQUALS '00'
              MOVE 'S' TO WS-LISTADO-OPEN
           ELSE
              DISPLAY 'ERROR OPEN EN LISTADO'
              DISPLAY 'ERROR CODE: ' FS-LISTADO
              PERFORM 3000-FINALIZO
           END-IF.

       1300-IMPRIMO-TITULOS.

           WRITE REG-LISTADO-FD          FROM WS-AUX-LISTA.
           EVALUATE FS-LISTADO
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    PERFORM 3000-FINALIZO
           END-EVALUATE.

       1400-PRIMERA-LECTURA.

           READ MATI INTO WS-ESTRUCTURA

           IF FS-MATI EQUALS '00'
              ADD 1 TO WS-CONT-REG-LEIDOS
           ELSE
              SET 88-MATI-FIN TO TRUE
           END-IF.

      *----------------------------------------------------------------*
       2000-PROCESO.

           IF WS-GENERO = 'M'
              EVALUATE WS-ESTADO-CIVIL
                  WHEN 'C'
                       ADD 1 TO MAS-CAS
                       ADD 1 TO CONT-MASC
                  WHEN 'S'
                       ADD 1 TO MAS-SOL
                       ADD 1 TO CONT-MASC
                  WHEN 'D'
                       ADD 1 TO MAS-DIV
                       ADD 1 TO CONT-MASC
                  WHEN 'V'
                       ADD 1 TO MAS-VIU
                       ADD 1 TO CONT-MASC
                  WHEN OTHER
                       DISPLAY " "
                       DISPLAY "DATO INVALIDO"
                       DISPLAY "REGISTRO: " WS-CONT-REG-LEIDOS
               END-EVALUATE
           ELSE
               IF WS-GENERO = 'F'
                  EVALUATE WS-ESTADO-CIVIL
                      WHEN 'C'
                          ADD 1 TO FEM-CAS
                          ADD 1 TO CONT-FEME
                      WHEN 'S'
                          ADD 1 TO FEM-SOL
                          ADD 1 TO CONT-FEME
                      WHEN 'D'
                          ADD 1 TO FEM-DIV
                          ADD 1 TO CONT-FEME
                      WHEN 'V'
                          ADD 1 TO FEM-VIU
                          ADD 1 TO CONT-FEME
                      WHEN OTHER
                          DISPLAY " "
                          DISPLAY "DATO INVALIDO"
                          DISPLAY "REGISTRO: " WS-CONT-REG-LEIDOS
                   END-EVALUATE
                ELSE
                   DISPLAY " "
                   DISPLAY "DATO INVALIDO"
                   DISPLAY "REGISTRO: " WS-CONT-REG-LEIDOS
                END-IF
           END-IF.

           PERFORM 2200-LECTURA.

       2200-LECTURA.

           READ MATI INTO WS-ESTRUCTURA

           EVALUATE FS-MATI
               WHEN '00'
                    ADD 1  TO WS-CONT-REG-LEIDOS
               WHEN '10'
                    SET 88-MATI-FIN TO TRUE
               WHEN OTHER
                    DISPLAY 'ERROR EN LA LECTURA'
                    DISPLAY 'ERROR CODE: ' FS-MATI
                    PERFORM 3000-FINALIZO
           END-EVALUATE.

       3000-FINALIZO.

           PERFORM 3100-ESCRITURA.

           PERFORM 3200-CIERRO-ARCHIVOS.

           STOP RUN.

       3100-ESCRITURA.

           MOVE "MASCULINO"            TO WS-GEN
           MOVE MAS-SOL                TO WS-SOLT
           MOVE MAS-CAS                TO WS-CASA
           MOVE MAS-DIV                TO WS-DIVO
           MOVE MAS-VIU                TO WS-VIU
           MOVE CONT-MASC              TO WS-TOTAL

           PERFORM 3150-ESCRITURA-2.

           MOVE "FEMENINO"             TO WS-GEN
           MOVE FEM-SOL                TO WS-SOLT
           MOVE FEM-CAS                TO WS-CASA
           MOVE FEM-DIV                TO WS-DIVO
           MOVE FEM-VIU                TO WS-VIU
           MOVE CONT-FEME              TO WS-TOTAL

           PERFORM 3150-ESCRITURA-2.

           ADD CONT-FEME   TO CONT-MASC GIVING WS-SUMA-TOTAL
           MOVE WS-TOTALES TO WS-AUX-LISTA
           PERFORM 1300-IMPRIMO-TITULOS.

           MOVE WS-CONT-REG-LEIDOS TO WS-REG-LEIDOS
           MOVE WS-ULTIMA-LINEA    TO WS-AUX-LISTA
           PERFORM 1300-IMPRIMO-TITULOS.

       3150-ESCRITURA-2.

           WRITE REG-LISTADO-FD        FROM WS-DETALLE.

           EVALUATE FS-LISTADO
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN ESCRITURA"
                    PERFORM 3200-CIERRO-ARCHIVOS
           END-EVALUATE.

       3200-CIERRO-ARCHIVOS.

           EVALUATE TRUE
              WHEN 88-LISTADO-SI
                   PERFORM 3300-CIERRO-MATI
                   PERFORM 3400-CIERRO-LISTADO
              WHEN OTHER
                   STOP RUN
           END-EVALUATE.

       3300-CIERRO-MATI.

           CLOSE MATI.

           IF FS-MATI EQUAL '00'
               CONTINUE
           ELSE
               DISPLAY "ERROR EN CIERRE"
               DISPLAY "FS-LISTADO: " FS-MATI
               PERFORM 3000-FINALIZO
           END-IF.

           DISPLAY "CIERRE EXITOSO".

       3400-CIERRO-LISTADO.

           CLOSE LISTADO.

           IF FS-LISTADO EQUAL '00'
               CONTINUE
           ELSE
               DISPLAY "ERROR EN CIERRE"
               DISPLAY "FS-LISTADO: " FS-LISTADO
               PERFORM 3000-FINALIZO
           END-IF.

           DISPLAY "CIERRE EXITOSO".

       END PROGRAM EJER-12.
