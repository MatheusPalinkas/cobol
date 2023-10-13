       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP05_01.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  12/09/2023.
       DATE-COMPILED. 12/09/2023.

      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *--------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-CONTADOR-LINHA    PIC 9  VALUE 1.
           88 FLAG-IMPRIMIR-LINHAS    VALUE 1   THRU 3.
       77 WS-CONTADOR-COLUNA   PIC 9  VALUE 1.
           88 FLAG-IMPRIMIR-COLUNAS   VALUE 1   THRU 5.
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
      *--------------------------------------------------------------*
       MAIN.

           PERFORM IMPRIMIR-LINHA
           WITH TEST AFTER UNTIL NOT FLAG-IMPRIMIR-LINHAS.

           STOP RUN.
      *--------------------------------------------------------------*
       IMPRIMIR-LINHA.
           DISPLAY WS-CONTADOR-LINHA NO ADVANCING.
           
           MOVE 1 TO WS-CONTADOR-COLUNA.

           PERFORM IMPRIMIR-COLUNA
           WITH TEST AFTER UNTIL NOT FLAG-IMPRIMIR-COLUNAS.

           DISPLAY "".
           ADD 1 TO WS-CONTADOR-LINHA.
      *--------------------------------------------------------------*
       IMPRIMIR-COLUNA.
           DISPLAY "  " WS-CONTADOR-COLUNA NO ADVANCING.

           ADD 1 TO WS-CONTADOR-COLUNA.
      *--------------------------------------------------------------*
