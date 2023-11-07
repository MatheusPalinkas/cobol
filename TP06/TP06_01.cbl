       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP06_01.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  04/11/2023.
       DATE-COMPILED. 04/11/2023.
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SETORES ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE.
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD SETORES
           LABEL RECORD IS STANDARD
           DATA RECORD IS REG-SETOR
           VALUE OF FILE-ID IS
           "C:\cobol\setores.txt".

       01 REG-SETOR.
           05 FD-CODIGO             PIC 9                 VALUE ZEROS.
           05 FD-DESCRICAO          PIC x(11)             VALUE SPACES.

       WORKING-STORAGE SECTION.
       77 WS-SETOR                  PIC 9                 VALUE ZEROS.
           88 FLAG-SETOR-VALIDA     VALUE 1               THRU 5.
       77 WS-PERC-AUMENTO           PIC 9(3)V9(2)         VALUE ZEROS.
       77 WS-COUNT-LINE             PIC 99                VALUE 9.
      *---- FILE ---------------------------------------------------*
       77 WS-STATUS-FILE            PIC X(02)             VALUE SPACES.
           88 FLAG-SETORES-SUCESSO  VALUE "00".
           88 FLAG-SETORES-EOF      VALUE "10".
           88 FLAG-SETORES-ERRO     VALUE "30".
      *---- SCREENS ------------------------------------------------*
       SCREEN SECTION.
       01 SC-LIMPA-TELA.
           05 BLANK SCREEN.
       01 SC-SETORES.
           10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
           "TP06.01 - Ajuste de preco por setor".

           10   LINE 7    COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "Setores:".

           10   LINE 16   COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "Digite o codigo do setor: ".
           10 SC-SETOR         LINE + 0  COLUMN  54
           USING WS-SETOR.

           10   LINE 17   COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "Digite o percentual de aumento: %".
           10 SC-PERC-AUMENTO         LINE + 0  COLUMN  61  PIC ZZ9,99
           USING WS-PERC-AUMENTO.

           10   LINE 27 COLUMN 28     FOREGROUND-COLOR  3 VALUE
           "Desenvolvido por Matheus Palinkas e Joao Tavares".
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
      *--------------------------------------------------------------*
       MAIN.

           DISPLAY SC-LIMPA-TELA.
           PERFORM MOSTRAR-SETORES.

           PERFORM ENTRADA-SETOR.
           PERFORM ENTRADA-PERC-AUMENTO.

           STOP RUN.
      *--------------------------------------------------------------*
       MOSTRAR-SETORES.
           OPEN INPUT SETORES.

           IF NOT FLAG-SETORES-SUCESSO
                  DISPLAY "ERROR DE ABERTURA DO ARQUIVO" AT 2029
                  STOP RUN
           END-IF.

           DISPLAY SC-SETORES.
           PERFORM READ-FILE-SETORES.

           CLOSE SETORES.
      *--------------------------------------------------------------*
       READ-FILE-SETORES.
           PERFORM READ-LINE-FILE-SETORES UNTIL FLAG-SETORES-EOF.
      *--------------------------------------------------------------*
       READ-LINE-FILE-SETORES.

           READ SETORES.

           IF NOT FLAG-SETORES-EOF
               DISPLAY FD-CODIGO    AT LINE WS-COUNT-LINE COLUMN 28
               DISPLAY " - "        AT LINE WS-COUNT-LINE COLUMN 29
               DISPLAY FD-DESCRICAO AT LINE WS-COUNT-LINE COLUMN 32
               ADD 1 TO WS-COUNT-LINE
           END-IF.


      *--------------------------------------------------------------*
       ENTRADA-SETOR.

           ACCEPT SC-SETOR.

           IF NOT FLAG-SETOR-VALIDA
               DISPLAY "Setor Invalido - Escolha novamente" AT 2128
               FOREGROUND-COLOR 6
               PERFORM ENTRADA-SETOR
           END-IF.

           DISPLAY "                                  " AT 2128.
      *--------------------------------------------------------------*
       ENTRADA-PERC-AUMENTO.

           ACCEPT SC-PERC-AUMENTO.

           IF WS-PERC-AUMENTO <= 0
               DISPLAY "Percentual Invalido - Escolha novamente" AT 2128
               FOREGROUND-COLOR 6
               PERFORM ENTRADA-PERC-AUMENTO
           END-IF.

           DISPLAY "                                       " AT 2128.
      *--------------------------------------------------------------*
