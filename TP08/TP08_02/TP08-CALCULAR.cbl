       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP08-CALCULAR.
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
       WORKING-STORAGE SECTION.
       77 WS-PORC-IRRF              PIC 9V9(3)      VALUE ZERO.
       77 WS-DEDUCAO-IRRF           PIC 9(3)V9(2)   VALUE ZERO.
      *---- CONSTS --------------------------------------------------*
       77 CONST-DEDUCAO-DEPENDENTE     PIC 9(3)V9(2)   VALUE 189,59.
      *--------------------------------------------------------------*
       LINKAGE SECTION.
       01 LS-FICHA.
           05 LS-NOME.
               10 LS-PRIMEIRO-NOME      PIC A(20)      VALUE SPACE.
           05 LS-PRONTUARIO             PIC X(8)       VALUE SPACE.
           05 LS-DEPENTENDES.
               10 LS-PENSAO-DEPENTENDE  PIC 9(4)V9(2)  VALUE ZEROS.
               10 LS-QTD-DEPENTENDES    PIC 9(2)       VALUE ZEROS.
           05 LS-SALARIO.
               10 LS-SALARIO-BRUTO      PIC 9(5)V9(2)  VALUE ZEROS.
                   88 FAIXA-INSS-7  VALUE 0       THRU  1320,00.
                   88 FAIXA-INSS-9  VALUE 1320,01 THRU  2571,29.
                   88 FAIXA-INSS-12 VALUE 2571,30 THRU  3856,94.
                   88 FAIXA-INSS-14 VALUE 3856,95 THRU  7507,49.
               10 LS-SALARIO-LIQUIDO    PIC 9(5)V9(2)  VALUE ZEROS.
               10 LS-SALARIO-REFERENCIA PIC 9(5)V9(2)  VALUE ZEROS.
                   88 FAIXA-IRRF-0  VALUE 0       THRU  2112,00.
                   88 FAIXA-IRRF-7  VALUE 2112,01 THRU  2826,65.
                   88 FAIXA-IRRF-15 VALUE 2826,66 THRU  3751,06.
                   88 FAIXA-IRRF-22 VALUE 3751,07 THRU  4664,68.
               10 LS-DESCONTO-INSS      PIC 9(5)V9(2)  VALUE ZEROS.
               10 LS-DESCONTO-IRRF      PIC 9(5)V9(2)  VALUE ZEROS.
       01 LS-HORAS.
           05 LS-HORA-TRABALHADA        PIC 9(3)V9(2)  VALUE ZEROS.
           05 LS-QTD-HORAS              PIC 9(3)       VALUE ZEROS.
      *--------------------------------------------------------------*
       PROCEDURE DIVISION CHAINING REFERENCE LS-FICHA LS-HORAS.
      *--------------------------------------------------------------*
       CALCULAR.

           PERFORM CALULAR-SALARIO-BRUTO.
           PERFORM CALCULAR-INSS.
           PERFORM CALCULAR-SALARIO-REFERENCIA.
           PERFORM CALCULAR-IRRF.
           PERFORM CALCULAR-SALARIO-LIQUIDO.

           CHAIN "TP08-EXIBIR-DADOS" USING REFERENCE LS-FICHA LS-HORAS.

           STOP RUN.
      *--------------------------------------------------------------*
       CALULAR-SALARIO-BRUTO.
           COMPUTE LS-SALARIO-BRUTO = LS-HORA-TRABALHADA * LS-QTD-HORAS.
      *--------------------------------------------------------------*
       CALCULAR-INSS.

           IF FAIXA-INSS-7 THEN
               COMPUTE LS-DESCONTO-INSS = LS-SALARIO-BRUTO * 0,075
           END-IF.

           IF FAIXA-INSS-9 THEN
               COMPUTE LS-DESCONTO-INSS =
                  (LS-SALARIO-BRUTO - 1320,00) * 0,09 + 99
           END-IF.

           IF FAIXA-INSS-12 THEN
               COMPUTE LS-DESCONTO-INSS =
                  (LS-SALARIO-BRUTO - 2571,29) * 0,12 + (99 + 112,61)

           END-IF.

           IF FAIXA-INSS-14 THEN
               COMPUTE LS-DESCONTO-INSS =
                  (LS-SALARIO-BRUTO - 3856,94) * 0,14 +
                  (99 + 112,61 + 154,27)
           END-IF.

           IF LS-SALARIO-BRUTO > 7507,49 THEN
               COMPUTE LS-DESCONTO-INSS =
                  (7507,49 - 3856,94) * 0,14 +
                  (99 + 112,61 + 154,27)
           END-IF.
      *--------------------------------------------------------------*
       CALCULAR-SALARIO-REFERENCIA.

           COMPUTE LS-SALARIO-REFERENCIA = LS-SALARIO-BRUTO
                   - LS-DESCONTO-INSS
                   - LS-PENSAO-DEPENTENDE
                   - (LS-QTD-DEPENTENDES * CONST-DEDUCAO-DEPENDENTE).

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
