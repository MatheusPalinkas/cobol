       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP08-MAIN.
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
       01 WS-FICHA.
           05 WS-NOME.
               10 WS-PRIMEIRO-NOME      PIC A(20)      VALUE SPACE.
           05 WS-PRONTUARIO             PIC X(8)       VALUE SPACE.
           05 WS-DEPENTENDES.
               10 WS-PENSAO-DEPENTENDE  PIC 9(4)V9(2)  VALUE ZEROS.
               10 WS-QTD-DEPENTENDES    PIC 9(2)       VALUE ZEROS.
           05 WS-SALARIO.
               10 WS-SALARIO-BRUTO      PIC 9(5)V9(2)  VALUE ZEROS.
               10 WS-SALARIO-LIQUIDO    PIC 9(5)V9(2)  VALUE ZEROS.
               10 WS-SALARIO-REFERENCIA PIC 9(5)V9(2)  VALUE ZEROS.
               10 WS-DESCONTO-INSS      PIC 9(5)V9(2)  VALUE ZEROS.
               10 WS-DESCONTO-IRRF      PIC 9(5)V9(2)  VALUE ZEROS.
       01 WS-HORAS.
           05 WS-HORA-TRABALHADA        PIC 9(3)V9(2)  VALUE ZEROS.
           05 WS-QTD-HORAS              PIC 9(3)       VALUE ZEROS.
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
      *---- SCREENS ------------------------------------------------*
       01 SC-LIMPA-TELA.
           05 BLANK SCREEN.
       01 SC-ENTRADA-DADOS.
           05 SC-DISPLAYS-ENTRADA-DADOS.
                  10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
                  "TP08 - MATHEUS PALINKAS E JOAO TAVARES" .

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Nome:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Prontuario:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Valor hora:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Qtde horas:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Valor Pensao:".

                  10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
                  "Qtde dependentes:".
           05 SC-ACCEPTS-ENTRADA-DADOS.
               10 SC-PRIMEIRO-NOME   LINE 07   COLUMN  35
               USING WS-PRIMEIRO-NOME.

               10 SC-PRONTUARIO      LINE + 2  COLUMN  40
               USING WS-PRONTUARIO.

               10 SC-HORA-TRABALHADA LINE + 2  COLUMN  40    PIC ZZ9,99
               USING WS-HORA-TRABALHADA.

               10 SC-QTD-HORAS       LINE + 2  COLUMN  40    PIC ZZ9
               USING WS-QTD-HORAS.

               10 SC-PENSAO-DEPENTENDE LINE + 2 COLUMN  42   PIC ZZZ9,99
               USING WS-PENSAO-DEPENTENDE.

               10 SC-QTD-DEPENTENDES LINE + 2  COLUMN  45    PIC Z9
               USING WS-QTD-DEPENTENDES.
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
      *--------------------------------------------------------------*
       ENTRADA-DADOS.

           MOVE SPACES  TO WS-PRIMEIRO-NOME.
           MOVE SPACES  TO WS-PRONTUARIO.
           MOVE ZEROS   TO WS-HORA-TRABALHADA.
           MOVE ZEROS   TO WS-QTD-HORAS.
           MOVE ZEROS   TO WS-PENSAO-DEPENTENDE.
           MOVE ZEROS   TO WS-QTD-DEPENTENDES.

           DISPLAY SC-LIMPA-TELA.

           DISPLAY SC-TABELA-ENTRADA-DADOS.
           DISPLAY SC-ENTRADA-DADOS.

           ACCEPT SC-PRIMEIRO-NOME.
           ACCEPT SC-PRONTUARIO.
           ACCEPT SC-HORA-TRABALHADA.
           ACCEPT SC-QTD-HORAS.
           ACCEPT SC-PENSAO-DEPENTENDE.
           ACCEPT SC-QTD-DEPENTENDES.
      *--------------------------------------------------------------*
       CALCULAR-SALARIO.

           CHAIN "TP08-CALCULAR"
           USING REFERENCE WS-FICHA WS-HORAS.

       STOP RUN.
      *--------------------------------------------------------------*
