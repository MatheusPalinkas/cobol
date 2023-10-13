       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP05_02.
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
       77 WS-NUM-INICIAL            PIC 999               VALUE ZEROS.
       77 WS-NUM-FINAL              PIC 999               VALUE ZEROS.
       77 WS-INCREMENTO             PIC 999               VALUE ZEROS.
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
      *--------------------------------------------------------------*
       MAIN.

           PERFORM ENTRADA-DADOS.

           DISPLAY "RESULTADO DA SEQUENCIA: ".

           IF WS-NUM-INICIAL < WS-NUM-FINAL               
               PERFORM INCREMENTAR-NUMEROS
           ELSE
               PERFORM DECREMENTAR-NUMEROS
           END-IF.

           STOP RUN.
      *--------------------------------------------------------------*
       ENTRADA-DADOS.
           DISPLAY "Digite o primeiro numero: ".
           ACCEPT WS-NUM-INICIAL.

           DISPLAY "Digite o segundo numero: ".
           ACCEPT WS-NUM-FINAL.

           DISPLAY "Digite o incremento: ".
           ACCEPT WS-INCREMENTO.
      *--------------------------------------------------------------*
       INCREMENTAR-NUMEROS.

       DISPLAY WS-NUM-INICIAL " " NO ADVANCING.

       ADD WS-INCREMENTO TO WS-NUM-INICIAL.

       IF WS-NUM-INICIAL <= WS-NUM-FINAL
           PERFORM INCREMENTAR-NUMEROS
       END-IF.
      *--------------------------------------------------------------*
       DECREMENTAR-NUMEROS.

       DISPLAY WS-NUM-INICIAL " " NO ADVANCING.

       SUBTRACT WS-INCREMENTO FROM WS-NUM-INICIAL.

       IF WS-NUM-INICIAL >= WS-NUM-FINAL
           PERFORM DECREMENTAR-NUMEROS
       END-IF.
      *--------------------------------------------------------------*
