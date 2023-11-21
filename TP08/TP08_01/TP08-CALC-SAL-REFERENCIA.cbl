       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP08-CALC-SAL-REFERENCIA.
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
      *---- CONSTS --------------------------------------------------*
       77 CONST-DEDUCAO-DEPENDENTE     PIC 9(3)V9(2)   VALUE 189,59.
      *--------------------------------------------------------------*
       LINKAGE SECTION.
       
       01 LS-SALARIO.
           05 LS-SALARIO-BRUTO      PIC 9(5)V9(2)  VALUE ZEROS.
                   88 FAIXA-INSS-7  VALUE 0       THRU  1320,00.
                   88 FAIXA-INSS-9  VALUE 1320,01 THRU  2571,29.
                   88 FAIXA-INSS-12 VALUE 2571,30 THRU  3856,94.
                   88 FAIXA-INSS-14 VALUE 3856,95 THRU  7507,49.
           05 LS-SALARIO-LIQUIDO    PIC 9(5)V9(2)  VALUE ZEROS.
           05 LS-SALARIO-REFERENCIA PIC 9(5)V9(2)  VALUE ZEROS.
           05 LS-DESCONTO-INSS      PIC 9(5)V9(2)  VALUE ZEROS.
           05 LS-DESCONTO-IRRF      PIC 9(5)V9(2)  VALUE ZEROS.
       01 LS-DEPENTENDES.
           05 LS-PENSAO-DEPENTENDE  PIC 9(4)V9(2)  VALUE ZEROS.
           05 LS-QTD-DEPENTENDES    PIC 9(2)       VALUE ZEROS.
      *--------------------------------------------------------------*
       PROCEDURE DIVISION USING LS-SALARIO LS-DEPENTENDES.
      *--------------------------------------------------------------*
       CALCULAR.
           
           PERFORM CALCULAR-INSS.
           PERFORM CALCULAR-SALARIO-REFERENCIA.

           CALL "TP08-CALC-SAL-LIQUIDO" USING LS-SALARIO.

           EXIT PROGRAM.
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