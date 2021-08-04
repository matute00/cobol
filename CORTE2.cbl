      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT RECAUDACION-D ASSIGN TO
           'D:\cobol-1\Archivo\RECDIAR.txt'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-RECAUDACION.

       SELECT RESUMEN ASSIGN TO
           'D:\cobol-1\Archivo\RESXDIA2.txt'
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS FS-RESUMEN.

       DATA DIVISION.
       FILE SECTION.

       FD  RECAUDACION-D
           RECORDING MODE IS F
           BLOCK 0.
           01 REG-RECAU-FD                PIC X(28).

       FD  RESUMEN
           RECORDING MODE IS F
           BLOCK 0.
       01  REG-RESUMEN-FD                 PIC X(90).

       WORKING-STORAGE SECTION.

       77  FS-RECAUDACION              PIC XX VALUE '  '.
           88 88-RECAU-OKEY                   VALUE '00'.
           88 88-RECAU-FIN                    VALUE '10'.

       77  FS-RESUMEN                  PIC XX VALUE '  '.
           88 88-RESUM-OKEY                   VALUE '00'.

       77  WS-IMPOR-FECHA              PIC 9(13)V99.
       77  WS-IMPOR-CAJE               PIC 9(13)V99.
       77  WS-IMPOR-TOT                PIC 9(13)V99.

       77  WS-CANT-CASOS-F             PIC 9(09).
       77  WS-CANT-CASOS-C             PIC 9(09).
       77  WS-CANT-CASOS-T             PIC 9(09).

       77  WS-GRABADOS                 PIC 9(09).
       77  WS-GRABADOS-ED              PIC ZZZ.ZZZ.ZZ9.
       77  WS-IMP-GRAB                 PIC 9(13)V99.
       77  WS-IMP-GRAB-ED              PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.

       77  WS-LEIDOS                   PIC 9(09).
       77  WS-LEIDOS-ED                PIC ZZZ.ZZZ.ZZ9.
       77  WS-IMP-LEID                 PIC 9(13)V99.
       77  WS-IMP-LEID-ED              PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.

       77  WS-FECHA-MIN                PIC X(10) VALUE HIGH-VALUE.
       77  WS-FECHA-MAX                PIC X(10) VALUE LOW-VALUE.

       01  WS-RECAU-ANT.
           03 WS-COD-CAJA              PIC 9(03).
           03 WS-FECHA.
              05 WS-FECHA-AAAA         PIC 9(04).
              05 FILLER                PIC X(01).
              05 WS-FECHA-MM           PIC 9(02).
              05 FILLER                PIC X(01).
              05 WS-FECHA-DD           PIC 9(02).

       COPY WRECAUDACION.

       COPY WRES-RECAUDACION.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESO UNTIL 88-RECAU-FIN.

           PERFORM 3000-FINALIZO.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIO.

           PERFORM 1100-OPEN-RECAUDACION.

           PERFORM 1200-OPEN-RESUMEN.

           PERFORM 1300-PRIMERA-LECTURA.

       1100-OPEN-RECAUDACION.

           OPEN INPUT RECAUDACION-D.

           IF FS-RECAUDACION EQUALS '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR OPEN EN NOVEDAD'
              DISPLAY 'ERROR CODE: ' FS-RECAUDACION
              PERFORM 3000-FINALIZO
           END-IF.

       1200-OPEN-RESUMEN.

           OPEN OUTPUT RESUMEN.

           IF FS-RESUMEN EQUALS '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR OPEN EN AUTOS-2'
              DISPLAY 'ERROR CODE: ' FS-RESUMEN
              PERFORM 3000-FINALIZO
           END-IF.

       1300-PRIMERA-LECTURA.

           PERFORM 2100-LEER-RECAUDACION.

           IF NOT 88-RECAU-FIN
              MOVE CLAVE-RECAUDACION TO WS-RECAU-ANT
           END-IF.

      *----------------------------------------------------------------*
       2000-PROCESO.

           IF COD-CAJERO IN RECAUDACION-REG = WS-COD-CAJA
              IF FECHA-RECAUDACION IN RECAUDACION-REG = WS-FECHA
                 PERFORM 2700-MOVIMIENTOS
                 PERFORM 2800-FECHAS
              ELSE
                 PERFORM 2600-CORTE-CONTROL-FECHA
      *           PERFORM 2700-MOVIMIENTOS
      *           MOVE CLAVE-RECAUDACION TO WS-RECAU-ANT
              END-IF
           ELSE
              PERFORM 2500-CORTE-CONTROL-CAJERO
      *        PERFORM 2700-MOVIMIENTOS
      *        MOVE CLAVE-RECAUDACION TO WS-RECAU-ANT
           END-IF

           PERFORM 2100-LEER-RECAUDACION

           IF 88-RECAU-FIN
              PERFORM 2500-CORTE-CONTROL-CAJERO
           END-IF.

       2100-LEER-RECAUDACION.

           READ RECAUDACION-D INTO  RECAUDACION-REG

           EVALUATE FS-RECAUDACION
               WHEN '00'
                   ADD 1 TO WS-LEIDOS
                   ADD IMPORTE IN RECAUDACION-REG TO WS-IMP-LEID
               WHEN '10'
                   SET 88-RECAU-FIN TO TRUE
                   DISPLAY " "
                   DISPLAY 'FIN DE ARCHIVO: ' FS-RECAUDACION
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY 'ERROR EN LECTURA RECAUDACION-D: '
                   DISPLAY FS-RECAUDACION
                   DISPLAY 'REGISTRO: ' RECAUDACION-REG
           END-EVALUATE.

       2200-ESCRITURA.

           WRITE REG-RESUMEN-FD  FROM RESUMEN-RECAUDACION-REG

           IF FS-RESUMEN EQUALS '00'
               ADD WS-CANT-CASOS-F TO WS-GRABADOS
               ADD WS-IMPOR-FECHA  TO WS-IMP-GRAB
           ELSE
               DISPLAY 'ERROR EN ESCRITURA RESUMEN: ' FS-RESUMEN
               DISPLAY 'CODIGO: ' COD-CAJERO IN RESUMEN-RECAUDACION-REG
               DISPLAY 'FECHA: ' FECHA-RECAUDACION
                                             IN RESUMEN-RECAUDACION-REG
           END-IF.

       2500-CORTE-CONTROL-CAJERO.

           PERFORM 2600-CORTE-CONTROL-FECHA.

           MOVE SPACE           TO FECHA-RECAUDACION
                                           IN RESUMEN-RECAUDACION-REG
           MOVE WS-IMPOR-CAJE   TO IMPORTE IN RESUMEN-RECAUDACION-REG
           MOVE WS-CANT-CASOS-C TO CANTIDAD-CASOS
                                           IN RESUMEN-RECAUDACION-REG

           PERFORM 2200-ESCRITURA.

           INITIALIZE WS-IMPOR-CAJE
           INITIALIZE WS-CANT-CASOS-C.

       2600-CORTE-CONTROL-FECHA.

           MOVE WS-COD-CAJA     TO COD-CAJERO IN RESUMEN-RECAUDACION-REG
           MOVE WS-FECHA        TO FECHA-RECAUDACION
                                              IN RESUMEN-RECAUDACION-REG
           MOVE WS-IMPOR-FECHA  TO IMPORTE IN RESUMEN-RECAUDACION-REG
           MOVE WS-CANT-CASOS-F TO CANTIDAD-CASOS
                                           IN RESUMEN-RECAUDACION-REG

           PERFORM 2200-ESCRITURA.

           ADD WS-IMPOR-FECHA  TO WS-IMPOR-CAJE
           ADD WS-CANT-CASOS-F TO WS-CANT-CASOS-C

           INITIALIZE WS-IMPOR-FECHA
           INITIALIZE WS-CANT-CASOS-F.

           MOVE CLAVE-RECAUDACION TO WS-RECAU-ANT

           PERFORM 2700-MOVIMIENTOS.

       2700-MOVIMIENTOS.

           ADD IMPORTE IN RECAUDACION-REG TO WS-IMPOR-FECHA

      *     ADD IMPORTE IN RECAUDACION-REG TO WS-IMPOR-CAJE

           ADD 1 TO WS-CANT-CASOS-F.

      *     ADD 1 TO WS-CANT-CASOS-C.

       2800-FECHAS.

           IF WS-FECHA-MIN > WS-FECHA
               MOVE WS-FECHA TO WS-FECHA-MIN
           END-IF.

           IF WS-FECHA-MAX < WS-FECHA
               MOVE WS-FECHA TO WS-FECHA-MAX
           END-IF.

      *----------------------------------------------------------------*
       3000-FINALIZO.

           PERFORM 3500-IMPRIMIR.

           PERFORM 3100-CIERRO-ARCHIVOS.

           STOP RUN.

       3100-CIERRO-ARCHIVOS.

           EVALUATE TRUE
              WHEN 88-RESUM-OKEY
                   PERFORM 3200-CIERRO-RECAUDACION
                   PERFORM 3300-CIERRO-RESUMEN
              WHEN OTHER
                   STOP RUN
           END-EVALUATE.

       3200-CIERRO-RECAUDACION.

           CLOSE RECAUDACION-D.

           IF FS-RECAUDACION EQUAL '00'
               CONTINUE
           ELSE
               DISPLAY "ERROR EN CIERRE"
               DISPLAY "FS-AUTO: " FS-RECAUDACION
               PERFORM 3000-FINALIZO
           END-IF.

           DISPLAY "CIERRE EXITOSO".

       3300-CIERRO-RESUMEN.

           CLOSE RESUMEN.

           IF FS-RESUMEN EQUAL '00'
               CONTINUE
           ELSE
               DISPLAY "ERROR EN CIERRE"
               DISPLAY "FS-NOVEDAD: " FS-RESUMEN
               PERFORM 3000-FINALIZO
           END-IF.

           DISPLAY "CIERRE EXITOSO".

       3500-IMPRIMIR.

           MOVE WS-LEIDOS       TO WS-LEIDOS-ED
           MOVE WS-IMP-LEID     TO WS-IMP-LEID-ED
           MOVE WS-GRABADOS     TO WS-GRABADOS-ED
           MOVE WS-IMP-GRAB     TO WS-IMP-GRAB-ED

           DISPLAY " "
           DISPLAY "***************************************************"
           DISPLAY "Total de Registros leídos: " WS-LEIDOS-ED
           DISPLAY "Importe: " WS-IMP-LEID-ED
           DISPLAY "***************************************************"

           DISPLAY " "
           DISPLAY "***************************************************"
           DISPLAY "Total de Registros grabados: " WS-GRABADOS-ED
           DISPLAY "Importe: " WS-IMP-GRAB-ED
           DISPLAY "***************************************************"

           DISPLAY " "
           DISPLAY "***************************************************"
           DISPLAY "Periodo procesado desde: " WS-FECHA-MIN
           DISPLAY "                  hasta: " WS-FECHA-MAX.
