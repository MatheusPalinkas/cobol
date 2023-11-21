       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP08-CALC-SAL-LIQUIDO.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  20/11/2023.
       DATE-COMPILED. 20/11/2023.

      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *--------------------------------------------------------------*
       DATA DIVISION.
      *--------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       77 WS-PORC-IRRF              PIC 9V9(3)      VALUE ZERO.
       77 WS-DEDUCAO-IRRF           PIC 9(3)V9(2)   VALUE ZERO.
      *--------------------------------------------------------------*
       LINKAGE SECTION.
       01 LS-SALARIO.
           05 LS-SALARIO-BRUTO      PIC 9(5)V9(2)  VALUE ZEROS.
           05 LS-SALARIO-LIQUIDO    PIC 9(5)V9(2)  VALUE ZEROS.
           05 LS-SALARIO-REFERENCIA PIC 9(5)V9(2)  VALUE ZEROS.
                   88 FAIXA-IRRF-0  VALUE 0       THRU  2112,00.
                   88 FAIXA-IRRF-7  VALUE 2112,01 THRU  2826,65.
                   88 FAIXA-IRRF-15 VALUE 2826,66 THRU  3751,06.
                   88 FAIXA-IRRF-22 VALUE 3751,07 THRU  4664,68.
           05 LS-DESCONTO-INSS      PIC 9(5)V9(2)  VALUE ZEROS.
           05 LS-DESCONTO-IRRF      PIC 9(5)V9(2)  VALUE ZEROS.
      *--------------------------------------------------------------*
       PROCEDURE DIVISION USING LS-SALARIO.
      *--------------------------------------------------------------*
       CALCULAR.
           
           PERFORM CALCULAR-IRRF.
           PERFORM CALCULAR-SALARIO-LIQUIDO.

           EXIT PROGRAM.
      *--------------------------------------------------------------*
       CALCULAR-IRRF.

           IF FAIXA-IRRF-0   THEN
               MOVE 0        TO WS-PORC-IRRF
               MOVE 0        TO WS-DEDUCAO-IRRF
           END-IF.

           IF FAIXA-IRRF-7   THEN
               MOVE 0,075    TO WS-PORC-IRRF
               MOVE 158,40   TO WS-DEDUCAO-IRRF
           END-IF.

           IF FAIXA-IRRF-15  THEN
               MOVE 0,15     TO WS-PORC-IRRF
               MOVE 370,40   TO WS-DEDUCAO-IRRF
           END-IF.

           IF FAIXA-IRRF-22  THEN
               MOVE 0,225    TO WS-PORC-IRRF
               MOVE 651,73   TO WS-DEDUCAO-IRRF
           END-IF.

           IF LS-SALARIO-REFERENCIA >  4664,69 THEN
               MOVE 0,275    TO WS-PORC-IRRF
               MOVE 884,96   TO WS-DEDUCAO-IRRF
           END-IF.

           COMPUTE LS-DESCONTO-IRRF =
               LS-SALARIO-REFERENCIA * WS-PORC-IRRF - WS-DEDUCAO-IRRF.
      *--------------------------------------------------------------*
       CALCULAR-SALARIO-LIQUIDO.

           COMPUTE LS-SALARIO-LIQUIDO
                  = LS-SALARIO-REFERENCIA - LS-DESCONTO-IRRF.
      *--------------------------------------------------------------*