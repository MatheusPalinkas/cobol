       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP02.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  05/09/2023.
       DATE-COMPILED. 05/09/2023.

      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *--------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WSFICHA.
           05 WSNOME.
               10 WSPRIMEIRO-NOME      PIC A(20)      VALUE SPACE.
               10 WSSOBRENOME          PIC A(30)      VALUE SPACE.
           05 WSPRONTUARIO             PIC X(8)       VALUE SPACE.
           05 WSSALARIO.
               10 WSSALARIO-BRUTO      PIC 9(5)V9(2)  VALUE ZERO.
               10 WSSALARIO-LIQUIDO    PIC 9(5)V9(2)  VALUE ZERO.
               10 WSSALARIO-REFERENCIA PIC 9(5)V9(2)  VALUE ZERO.
               10 WSDESCONTO-INSS      PIC 9(5)V9(2)  VALUE ZERO.
               10 WSDESCONTO-IRRF      PIC 9(5)V9(2)  VALUE ZERO.

       77 WSHORA-TRABALHADA            PIC 9(3)V9(2)  VALUE ZERO.
       77 WSQTD-HORAS                  PIC 9(3)       VALUE ZERO.
       77 WSQTD-DEPENTENDES            PIC 9(2)       VALUE ZERO.

      *---- VARIAVEIS COM MASCARA -----------------------------------*

       77 MASK-SALARIO-BRUTO         PIC $ZZZZ9,99.
       77 MASK-SALARIO-LIQUIDO       PIC $ZZZZ9,99.
       77 MASK-SALARIO-REFERENCIA    PIC $ZZZZ9,99.
       77 MASK-DESCONTO-INSS         PIC $ZZZZ9,99.
       77 MASK-DESCONTO-IRRF         PIC $ZZZZ9,99.
       77 MASK-HORA-TRABALHADA       PIC $ZZ9,99.
       77 MASK-QTD-HORAS             PIC ZZ9.
       77 MASK-QTD-DEPENTENDES       PIC Z9.

      *---- CONSTS --------------------------------------------------*
       77 CONST-PORC-INSS              PIC 9V9(3)      VALUE 0,14.
       77 CONST-PORC-IRRF              PIC 9V9(3)      VALUE 0,275.
       77 CONST-DEDUCAO-IRRF           PIC 9(3)V9(2)   VALUE 869,36.
       77 CONST-DEDUCAO-DEPENDENTE     PIC 9(3)V9(2)   VALUE 189,59.

      *---- CONSTS LAYOUT -------------------------------------------*
       77 CONST-LINE                   PIC A(59)       VALUE
          "+---------------------------------------------------------+".
       77 CONST-COLUMN                 PIC A           VALUE "|".

      *--------------------------------------------------------------*
       SCREEN SECTION.
       01 LIMPATELA.
           05 BLANK SCREEN.
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       CRIAR-LAYOUT.

           DISPLAY
               CONST-LINE     AT 0425 WITH FOREGROUND-COLOR 11
               CONST-COLUMN   AT 0525 WITH FOREGROUND-COLOR 11
               CONST-COLUMN   AT 0583 WITH FOREGROUND-COLOR 11
               CONST-LINE     AT 0625 WITH FOREGROUND-COLOR 11.

           DISPLAY
               CONST-COLUMN   AT 0725 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 0783 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 0825 WITH FOREGROUND-COLOR 1.

           DISPLAY
               CONST-COLUMN   AT 0925 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 0983 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 1025 WITH FOREGROUND-COLOR 1.

           DISPLAY
               CONST-COLUMN   AT 1125 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 1183 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 1225 WITH FOREGROUND-COLOR 1.

           DISPLAY
               CONST-COLUMN   AT 1325 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 1383 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 1425 WITH FOREGROUND-COLOR 1.

           DISPLAY
               CONST-COLUMN   AT 1525 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 1583 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 1625 WITH FOREGROUND-COLOR 1.

           DISPLAY
               CONST-COLUMN   AT 1725 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 1783 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 1825 WITH FOREGROUND-COLOR 1.

      *--------------------------------------------------------------*
       POSITION-LABELS.

           DISPLAY "TP02 - MATHEUS PALINKAS E JOAO TAVARES"
                AT 0536 WITH FOREGROUND-COLOR 8.

           DISPLAY
                "Nome:"             AT 0728 WITH FOREGROUND-COLOR 8
                "Sobrenome:"        AT 0928 WITH FOREGROUND-COLOR 8
                "Prontuario:"       AT 1128 WITH FOREGROUND-COLOR 8
                "Valor hora:"       AT 1328 WITH FOREGROUND-COLOR 8
                "Qtde horas:"       AT 1528 WITH FOREGROUND-COLOR 8
                "Qtde dependentes:" AT 1728 WITH FOREGROUND-COLOR 8.

      *--------------------------------------------------------------*
       ENTRADA-DADOS.

           ACCEPT
               WSPRIMEIRO-NOME       AT 0735 WITH HIGHLIGHT EMPTY-CHECK.

           ACCEPT
               WSSOBRENOME           AT 0939 WITH HIGHLIGHT EMPTY-CHECK.

           ACCEPT
               WSPRONTUARIO          AT 1140 WITH HIGHLIGHT EMPTY-CHECK.

           ACCEPT
               MASK-HORA-TRABALHADA  AT 1340 WITH HIGHLIGHT EMPTY-CHECK.

           ACCEPT
               MASK-QTD-HORAS        AT 1540 WITH HIGHLIGHT EMPTY-CHECK.

           ACCEPT
               MASK-QTD-DEPENTENDES  AT 1745 WITH HIGHLIGHT EMPTY-CHECK.


           MOVE MASK-HORA-TRABALHADA  TO WSHORA-TRABALHADA.
           MOVE MASK-QTD-HORAS        TO WSQTD-HORAS.
           MOVE MASK-QTD-DEPENTENDES  TO WSQTD-DEPENTENDES.
      *--------------------------------------------------------------*
       CALCULAR-SALARIO.

      *    CALCULO SALARIO BRUTO
           COMPUTE WSSALARIO-BRUTO = WSHORA-TRABALHADA * WSQTD-HORAS.

      *    CALCULO INSS
           COMPUTE WSDESCONTO-INSS = WSSALARIO-BRUTO * CONST-PORC-INSS.

      *    CALCULO SALARIO DE REFERENCIA
           COMPUTE
               WSSALARIO-REFERENCIA = WSSALARIO-BRUTO - WSDESCONTO-INSS.

      *    CALCULO IRRF
           COMPUTE WSDESCONTO-IRRF = WSSALARIO-BRUTO * CONST-PORC-IRRF.

           COMPUTE WSDESCONTO-IRRF =
               WSDESCONTO-IRRF - (
                WSQTD-DEPENTENDES * CONST-DEDUCAO-DEPENDENTE
                   + CONST-DEDUCAO-IRRF
                 ).

      *    CALCULO SALARIO LIQUIDO
           COMPUTE
             WSSALARIO-LIQUIDO = WSSALARIO-REFERENCIA - WSDESCONTO-IRRF.
      *--------------------------------------------------------------*
       ATRIBUIR-MASCARAS.

           MOVE WSSALARIO-BRUTO      TO MASK-SALARIO-BRUTO.
           MOVE WSSALARIO-LIQUIDO    TO MASK-SALARIO-LIQUIDO.
           MOVE WSSALARIO-REFERENCIA TO MASK-SALARIO-REFERENCIA.
           MOVE WSDESCONTO-INSS      TO MASK-DESCONTO-INSS.
           MOVE WSDESCONTO-IRRF      TO MASK-DESCONTO-IRRF.
           MOVE WSHORA-TRABALHADA    TO MASK-HORA-TRABALHADA.
           MOVE WSQTD-HORAS          TO MASK-QTD-HORAS.
           MOVE WSQTD-DEPENTENDES    TO MASK-QTD-DEPENTENDES.
      *--------------------------------------------------------------*
       LIMPAR-TELA.
           DISPLAY LIMPATELA.
      *--------------------------------------------------------------*
       CRIAR-LAYOUT-SAIDA.
           PERFORM CRIAR-LAYOUT.

           DISPLAY
               CONST-COLUMN   AT 1925 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 1983 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 2025 WITH FOREGROUND-COLOR 1.

           DISPLAY
               CONST-COLUMN   AT 2125 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 2183 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 2225 WITH FOREGROUND-COLOR 1.

           DISPLAY
               CONST-COLUMN   AT 2325 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 2383 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 2425 WITH FOREGROUND-COLOR 1.

           DISPLAY
               CONST-COLUMN   AT 2525 WITH FOREGROUND-COLOR 1
               CONST-COLUMN   AT 2583 WITH FOREGROUND-COLOR 1
               CONST-LINE     AT 2625 WITH FOREGROUND-COLOR 1.
      *--------------------------------------------------------------*
       POSITION-LABELS-SAIDA.

           DISPLAY "RESULTADOS"
                AT 0548 WITH FOREGROUND-COLOR 8.

           DISPLAY
               "Nome:"                  AT 0728 WITH FOREGROUND-COLOR 8
               "Sobrenome:"             AT 0928 WITH FOREGROUND-COLOR 8
               "Prontuario:"            AT 1128 WITH FOREGROUND-COLOR 8
               "Valor hora:"            AT 1328 WITH FOREGROUND-COLOR 8
               "Qtde horas:"            AT 1528 WITH FOREGROUND-COLOR 8
               "Qtde dependentes:"      AT 1728 WITH FOREGROUND-COLOR 8
               "Salario Bruto:"         AT 1928 WITH FOREGROUND-COLOR 8
               "INSS:"                  AT 2128 WITH FOREGROUND-COLOR 8
               "IRRF:"                  AT 2328 WITH FOREGROUND-COLOR 8
               "Salario Liquido:"       AT 2528 WITH FOREGROUND-COLOR 8.


           DISPLAY
               WSPRIMEIRO-NOME          AT 0750 WITH FOREGROUND-COLOR 8
               WSSOBRENOME              AT 0950 WITH FOREGROUND-COLOR 8
               WSPRONTUARIO             AT 1150 WITH FOREGROUND-COLOR 8
               MASK-HORA-TRABALHADA     AT 1350 WITH FOREGROUND-COLOR 8
               MASK-QTD-HORAS           AT 1550 WITH FOREGROUND-COLOR 8
               MASK-QTD-DEPENTENDES     AT 1750 WITH FOREGROUND-COLOR 8
               MASK-SALARIO-BRUTO       AT 1950 WITH FOREGROUND-COLOR 8
               MASK-DESCONTO-INSS       AT 2150 WITH FOREGROUND-COLOR 8
               MASK-DESCONTO-IRRF       AT 2350 WITH FOREGROUND-COLOR 8
               MASK-SALARIO-LIQUIDO     AT 2550 WITH FOREGROUND-COLOR 8.

           STOP RUN.
      *--------------------------------------------------------------*
