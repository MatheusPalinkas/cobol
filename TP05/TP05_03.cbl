       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP05_03.
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
       77 WS-OPCAO                  PIC 9                 VALUE ZEROS.
           88 FLAG-SAIR             VALUE 0.
           88 FLAG-SOMA             VALUE 1.
           88 FLAG-SUBTRACAO        VALUE 2.
           88 FLAG-MULTIPLICACAO    VALUE 3.
           88 FLAG-DIVISAO          VALUE 4.
           88 FLAG-OPCAO-VALIDA     VALUE 0               THRU 4.
       77 WS-NUM-1                  PIC 9(3)V9(2)         VALUE ZEROS.
       77 WS-NUM-2                  PIC 9(3)V9(2)         VALUE ZEROS.
       77 WS-RESULTADO              PIC 9(5)V9(2)         VALUE ZEROS.
       SCREEN SECTION.
      *---- SCREENS ------------------------------------------------*
       01 SC-LIMPA-TELA.
           05 BLANK SCREEN.
       01 SC-ENTRADA-OPCOES.
           10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
           "TP05 - Calculadora" .
           10   LINE 8   COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "1 - Soma".
           10   LINE + 1  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "2 - Subtracao".
           10   LINE + 1  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "3 - Multiplicacao".
           10   LINE + 1  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "4 - Divisao".
           10   LINE + 1  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "0 - Sair".
           10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "Digite a opcao: ".
           10 SC-OPCAO         LINE + 0  COLUMN  44     
           USING WS-OPCAO.

           10   LINE 29 COLUMN 28     FOREGROUND-COLOR  3 VALUE
           "Desenvolvido por Matheus Palinkas e Joao Tavares".
       01 SC-ENTRADA-DADOS.
           05 SC-DISPLAYS-ENTRADA-DADOS.
               10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
               "TP05 - Calculadora" .
    
               10   LINE 10   COLUMN 28     FOREGROUND-COLOR  8 VALUE
               "Digite o primeiro numero: ".
               10   LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
               "Digite o segundo numero: ".
           05 SC-ACCEPTS-ENTRADA-DADOS.
               10 SC-NUM-1     LINE 10   COLUMN  54    PIC ZZ9,99  
               USING WS-NUM-1.
               10 SC-NUM-2     LINE + 2  COLUMN  54    PIC ZZ9,99 
               USING WS-NUM-2.
           05   LINE 29 COLUMN 28     FOREGROUND-COLOR  3 VALUE
           "Desenvolvido por Matheus Palinkas e Joao Tavares".
       01 SC-SAIDA-DADOS.
           05 SC-DISPLAYS-SAIDA-DADOS.
               10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
               "TP05 - Calculadora" .
    
               10   LINE 10   COLUMN 28     FOREGROUND-COLOR  8 VALUE
               "O resultado:".
           05 SC-ACCEPTS-SAIDA-DADOS.
               10 LINE 10   COLUMN  42    PIC ZZZZ9,99  
               USING WS-RESULTADO.
           05   LINE 29 COLUMN 28     FOREGROUND-COLOR  3 VALUE
           "Desenvolvido por Matheus Palinkas e Joao Tavares".
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
      *--------------------------------------------------------------*
       MAIN.
           
           DISPLAY SC-LIMPA-TELA.

           PERFORM ENTRADA-OPCAO.
           PERFORM ENTRADA-DADOS.
           PERFORM CALCULAR.
           PERFORM SAIDA-DADOS.

           STOP RUN.
      *--------------------------------------------------------------*
       ENTRADA-OPCAO.
           
           DISPLAY SC-ENTRADA-OPCOES.
           ACCEPT  SC-OPCAO.

           IF NOT FLAG-OPCAO-VALIDA
               DISPLAY SC-LIMPA-TELA
               DISPLAY "Opcao Invalida - Escolha novamente" AT 1825
               FOREGROUND-COLOR 6
               PERFORM ENTRADA-OPCAO
           END-IF.

           IF FLAG-SAIR
               STOP RUN
           END-IF.
      *--------------------------------------------------------------*
       ENTRADA-DADOS.
           
           DISPLAY SC-LIMPA-TELA.

           DISPLAY SC-ENTRADA-DADOS.
           ACCEPT  SC-NUM-1.
           ACCEPT  SC-NUM-2.
      *--------------------------------------------------------------*
       CALCULAR.
           
           IF FLAG-SOMA
               ADD WS-NUM-1      TO WS-NUM-2      GIVING WS-RESULTADO
           END-IF.

           IF FLAG-SUBTRACAO
               SUBTRACT WS-NUM-1 FROM WS-NUM-2    GIVING WS-RESULTADO
           END-IF.

           IF FLAG-MULTIPLICACAO
               MULTIPLY WS-NUM-1 BY WS-NUM-2      GIVING WS-RESULTADO
           END-IF.

           IF FLAG-DIVISAO
               DIVIDE WS-NUM-1   INTO WS-NUM-2    GIVING WS-RESULTADO
           END-IF.
      *--------------------------------------------------------------*
       SAIDA-DADOS.
           
           DISPLAY SC-LIMPA-TELA.

           DISPLAY SC-SAIDA-DADOS.
      *--------------------------------------------------------------*
      