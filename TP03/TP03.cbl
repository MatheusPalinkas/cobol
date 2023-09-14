       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP03.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  14/09/2023.
       DATE-COMPILED. 14/09/2023.

      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *--------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FICHA.
           05 WS-NOME.
               10 WS-PRIMEIRO-NOME      PIC A(20)      VALUE SPACE.
               10 WS-SOBRENOME          PIC A(30)      VALUE SPACE.
           05 WS-PRONTUARIO             PIC X(8)       VALUE SPACE.
           05 WS-SALARIO.
               10 WS-SALARIO-BRUTO      PIC 9(5)V9(2)  VALUE ZEROS.
               10 WS-SALARIO-LIQUIDO    PIC 9(5)V9(2)  VALUE ZEROS.
               10 WS-SALARIO-REFERENCIA PIC 9(5)V9(2)  VALUE ZEROS.
               10 WS-DESCONTO-INSS      PIC 9(5)V9(2)  VALUE ZEROS.
               10 WS-DESCONTO-IRRF      PIC 9(5)V9(2)  VALUE ZEROS.

       77 WS-HORA-TRABALHADA            PIC 9(3)V9(2)  VALUE ZEROS.
       77 WS-QTD-HORAS                  PIC 9(3)       VALUE ZEROS.
       77 WS-QTD-DEPENTENDES            PIC 9(2)       VALUE ZEROS.

      *---- CONSTS --------------------------------------------------*
       77 CONST-PORC-INSS              PIC 9V9(3)      VALUE 0,14.
       77 CONST-PORC-IRRF              PIC 9V9(3)      VALUE 0,275.
       77 CONST-DEDUCAO-IRRF           PIC 9(3)V9(2)   VALUE 869,36.
       77 CONST-DEDUCAO-DEPENDENTE     PIC 9(3)V9(2)   VALUE 189,59.
      *--------------------------------------------------------------*
       SCREEN SECTION.
      *---- CONSTS --------------------------------------------------*
       01 SC-CONSTS.
           05 SC-LINE                    PIC A(59)       VALUE
          "+---------------------------------------------------------+".
           05 SC-COLUMN                    PIC A           VALUE "|".
       01 SC-TABELAS.
           05 SC-TABELA-ENTRADA-DADOS.
             10 LINE 04  COLUMN 25 USING SC-LINE   FOREGROUND-COLOR 11.
             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR 11.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR 11.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR 11.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.
           05 SC-TABELA-SAIDA-DADOS.
             10 LINE 04  COLUMN 25 USING SC-LINE   FOREGROUND-COLOR 11.
             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR 11.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR 11.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR 11.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.

             10 LINE + 1 COLUMN 25 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 0 COLUMN 83 USING SC-COLUMN FOREGROUND-COLOR  1.
             10 LINE + 1 COLUMN 25 USING SC-LINE   FOREGROUND-COLOR  1.
      *---- SCREENS ------------------------------------------------*
       01 SC-LIMPA-TELA.
           05 BLANK SCREEN.
       01 SC-ENTRADA-DADOS.
           05 SC-DISPLAYS-ENTRADA-DADOS.
                  10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
                  "TP03 - MATHEUS PALINKAS E JOAO TAVARES" .

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Nome:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Sobrenome:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Prontuario:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Valor hora:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Qtde horas:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Qtde dependentes:".
           05 SC-ACCEPTS-ENTRADA-DADOS.
               10 SC-PRIMEIRO-NOME   LINE 07   COLUMN  35
               USING WS-PRIMEIRO-NOME.

               10 SC-SOBRENOME       LINE + 2  COLUMN  39
               USING WS-SOBRENOME.

               10 SC-PRONTUARIO      LINE + 2  COLUMN  40
               USING WS-PRONTUARIO.

               10 SC-HORA-TRABALHADA LINE + 2  COLUMN  40    PIC ZZ9,99
               USING WS-HORA-TRABALHADA.

               10 SC-QTD-HORAS       LINE + 2  COLUMN  40    PIC ZZ9
               USING WS-QTD-HORAS.

               10 SC-QTD-DEPENTENDES LINE + 2  COLUMN  45    PIC Z9
               USING WS-QTD-DEPENTENDES.
       01 SC-SAIDA-DADOS.
           05 SC-DISPLAYS-SAIDA-DADOS.
                  10   LINE 05   COLUMN 48     FOREGROUND-COLOR  8 VALUE
                  "RESULTADOS" .

                  10   LINE 07   COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Nome:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Sobrenome:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Prontuario:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Valor hora:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Qtde horas:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Qtde dependentes:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Salario Bruto:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "INSS:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "IRRF:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Salario Liquido:".
           05 SC-ACCEPTS-SAIDA-DADOS.
               10 LINE 07   COLUMN  50          USING WS-PRIMEIRO-NOME.

               10 LINE + 2  COLUMN  50          USING WS-SOBRENOME.

               10 LINE + 2  COLUMN  50          USING WS-PRONTUARIO.

               10 LINE + 2  COLUMN  50          USING WS-HORA-TRABALHADA
                                                PIC $ZZ9,99.

               10 LINE + 2  COLUMN  50          USING WS-QTD-HORAS
                                                PIC ZZ9.

               10 LINE + 2  COLUMN  50          USING WS-QTD-DEPENTENDES
                                                PIC Z9.

               10 LINE + 2  COLUMN  50          USING WS-SALARIO-BRUTO
                                                PIC $ZZZZ9,99.

               10 LINE + 2  COLUMN  50          USING WS-DESCONTO-INSS
                                                PIC $ZZZZ9,99.

               10 LINE + 2  COLUMN  50          USING WS-DESCONTO-IRRF
                                                PIC $ZZZZ9,99.

               10 LINE + 2  COLUMN  50          USING WS-SALARIO-LIQUIDO
                                                PIC $ZZZZ9,99.
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
      *--------------------------------------------------------------*
       ENTRADA-DADOS.

           DISPLAY SC-LIMPA-TELA.

           DISPLAY SC-TABELA-ENTRADA-DADOS.
           DISPLAY SC-ENTRADA-DADOS.

           ACCEPT SC-PRIMEIRO-NOME.
           ACCEPT SC-SOBRENOME.
           ACCEPT SC-PRONTUARIO.
           ACCEPT SC-HORA-TRABALHADA.
           ACCEPT SC-QTD-HORAS.
           ACCEPT SC-QTD-DEPENTENDES.
      *--------------------------------------------------------------*
       CALCULAR-SALARIO.

      *    CALCULO SALARIO BRUTO
           COMPUTE WS-SALARIO-BRUTO = WS-HORA-TRABALHADA * WS-QTD-HORAS.

      *    CALCULO INSS
           COMPUTE WS-DESCONTO-INSS = WS-SALARIO-BRUTO * CONST-PORC-INSS.

      *    CALCULO SALARIO DE REFERENCIA
           COMPUTE WS-SALARIO-REFERENCIA
                  = WS-SALARIO-BRUTO - WS-DESCONTO-INSS.

      *    CALCULO IRRF
           COMPUTE WS-DESCONTO-IRRF = WS-SALARIO-BRUTO * CONST-PORC-IRRF.

           COMPUTE WS-DESCONTO-IRRF =
               WS-DESCONTO-IRRF - (
                WS-QTD-DEPENTENDES * CONST-DEDUCAO-DEPENDENTE
                   + CONST-DEDUCAO-IRRF
                 ).

      *    CALCULO SALARIO LIQUIDO
           COMPUTE WS-SALARIO-LIQUIDO
                  = WS-SALARIO-REFERENCIA - WS-DESCONTO-IRRF.
      *--------------------------------------------------------------*
       SAIDA-DADOS.
           DISPLAY SC-LIMPA-TELA.

           DISPLAY SC-TABELA-SAIDA-DADOS.
           DISPLAY SC-SAIDA-DADOS.

       STOP RUN.
      *--------------------------------------------------------------*
