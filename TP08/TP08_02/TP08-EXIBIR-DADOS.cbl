       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP08-EXIBIR-DADOS.
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
               10 LS-SALARIO-LIQUIDO    PIC 9(5)V9(2)  VALUE ZEROS.
               10 LS-SALARIO-REFERENCIA PIC 9(5)V9(2)  VALUE ZEROS.
               10 LS-DESCONTO-INSS      PIC 9(5)V9(2)  VALUE ZEROS.
               10 LS-DESCONTO-IRRF      PIC 9(5)V9(2)  VALUE ZEROS.
       01 LS-HORAS.
           05 LS-HORA-TRABALHADA        PIC 9(3)V9(2)  VALUE ZEROS.
           05 LS-QTD-HORAS              PIC 9(3)       VALUE ZEROS.
      *--------------------------------------------------------------*
       SCREEN SECTION.
      *---- CONSTS --------------------------------------------------*
       01 SC-CONSTS.
           05 SC-LINE                    PIC A(59)       VALUE
          "+---------------------------------------------------------+".
           05 SC-COLUMN                    PIC A           VALUE "|".
       01 SC-TABELAS.
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

      *---- SCREENS ------------------------------------------------*
       01 SC-LIMPA-TELA.
           05 BLANK SCREEN.
       01 SC-SAIDA-DADOS.
           05 SC-DISPLAYS-SAIDA-DADOS.
                  10   LINE 05   COLUMN 48     FOREGROUND-COLOR  8 VALUE
                  "RESULTADOS" .

                  10   LINE 07   COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Nome:".

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
                  "Desc. INSS:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Desc. IRRF:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Salario Liquido:".
           05 SC-ACCEPTS-SAIDA-DADOS.
               10 LINE 07   COLUMN  50          USING LS-PRIMEIRO-NOME.

               10 LINE + 2  COLUMN  50          USING LS-PRONTUARIO.

               10 LINE + 2  COLUMN  50          USING LS-HORA-TRABALHADA
                                                PIC $ZZ9,99.

               10 LINE + 2  COLUMN  50          USING LS-QTD-HORAS
                                                PIC ZZ9.

               10 LINE + 2  COLUMN  50          USING LS-QTD-DEPENTENDES
                                                PIC Z9.

               10 LINE + 2  COLUMN  50          USING LS-SALARIO-BRUTO
                                                PIC $ZZZZ9,99.

               10 LINE + 2  COLUMN  50          USING LS-DESCONTO-INSS
                                                PIC $ZZZZ9,99.

               10 LINE + 2  COLUMN  50          USING LS-DESCONTO-IRRF
                                                PIC $ZZZZ9,99.

               10 LINE + 2  COLUMN  50          USING LS-SALARIO-LIQUIDO
                                                PIC $ZZZZ9,99.

      *--------------------------------------------------------------*
       PROCEDURE DIVISION CHAINING REFERENCE LS-FICHA LS-HORAS.
      *--------------------------------------------------------------*
       SAIDA-DADOS.
           DISPLAY SC-LIMPA-TELA.

           DISPLAY SC-TABELA-SAIDA-DADOS.
           DISPLAY SC-SAIDA-DADOS.

       STOP RUN.
      *--------------------------------------------------------------*
