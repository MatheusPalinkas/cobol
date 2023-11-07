       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP06_02.
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

           SELECT PRODUTOS ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE.

           SELECT PRODUTOS-REAJUSTE ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE-WRITE.
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD SETORES
           LABEL RECORD IS STANDARD
           DATA RECORD IS REG-SETOR
           VALUE OF FILE-ID IS "C:\cobol\setores.txt".

       01 REG-SETOR.
           05 FD-SETOR-CODIGO       PIC 9                 VALUE ZEROS.
           05 FD-SETOR-DESCRICAO    PIC X(11)             VALUE SPACES.

       FD PRODUTOS
           LABEL RECORD IS STANDARD
           DATA RECORD IS REG-PRODUTO
           VALUE OF FILE-ID IS "C:\cobol\produtos.TXT".

       01 REG-PRODUTO.
           05 FD-PROD-CODIGO        PIC X(5)              VALUE SPACES.
           05 FD-PROD-DESCRICAO     PIC X(40)             VALUE SPACES.
           05 FD-PROD-VALOR         PIC 9(2)V9(2)         VALUE ZEROS.
           05 FD-PROD-SETOR         PIC X                 VALUE SPACES.
           05 FD-PROD-ESTOQUE       PIC X                 VALUE SPACES.
               88 FLAG-ESTOQUE-PRESETE                    VALUE "T".
               88 FLAG-ESTOQUE-INDISPONIVEL               VALUE "F".
           05 FD-PROD-ANO-ESTOQUE   PIC X(4)              VALUE SPACES.
           05 FD-PROD-MES-ESTOQUE   PIC X(2)              VALUE SPACES.

       FD PRODUTOS-REAJUSTE
           LABEL RECORD IS STANDARD
           DATA RECORD IS REG-PRODUTO-REAJUSTE
           VALUE OF FILE-ID IS "C:\cobol\produtos_reajuste.TXT".

       01 REG-PRODUTO-REAJUSTE.
           05 FD-PROD-REA-CODIGO      PIC X(5)            VALUE SPACES.
           05 FD-PROD-REA-DESCRICAO   PIC X(40)           VALUE SPACES.
           05 FD-PROD-REA-VALOR       PIC 9(2)V9(2)       VALUE ZEROS.
           05 FD-PROD-REA-SETOR       PIC X               VALUE SPACES.
           05 FD-PROD-REA-ESTOQUE     PIC X               VALUE SPACES.
           05 FD-PROD-REA-ANO-ESTOQUE PIC X(4)            VALUE SPACES.
           05 FD-PROD-REA-MES-ESTOQUE PIC X(2)            VALUE SPACES.

       WORKING-STORAGE SECTION.
       77 WS-SETOR                  PIC 9                 VALUE ZEROS.
           88 FLAG-SETOR-VALIDA     VALUE 1               THRU 5.
       77 WS-PERC-AUMENTO           PIC 9(3)V9(2)         VALUE ZEROS.
       77 WS-COUNT-LINE             PIC 99                VALUE 9.
       77 WS-CALC-NOVO-VALOR        PIC 9(2)V9(2)         VALUE ZEROS.
      *---- FILE ---------------------------------------------------*
       77 WS-STATUS-FILE-WRITE      PIC X(02)             VALUE SPACES.
       77 WS-STATUS-FILE            PIC X(02)             VALUE SPACES.
           88 FLAG-OPEN-FILE-SUCESSO   VALUE "00".
           88 FLAG-FILE-EOF            VALUE "10".
           88 FLAG-SETORES-ERRO        VALUE "30".
      *---- SCREENS ------------------------------------------------*
       SCREEN SECTION.
       01 SC-LIMPA-TELA.
           05 BLANK SCREEN.
       01 SC-SETORES.
           10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
           "TP06.02 - Ajuste de preco por setor".

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

           PERFORM APLICAR-REAJUSTE.

           STOP RUN.
      *--------------------------------------------------------------*
       MOSTRAR-SETORES.
           OPEN INPUT SETORES.

           IF NOT FLAG-OPEN-FILE-SUCESSO
                  DISPLAY "ERROR NA ABERTURA DO ARQUIVO" AT 2029
                  STOP RUN
           END-IF.

           DISPLAY SC-SETORES.
           PERFORM READ-LINE-FILE-SETORES UNTIL FLAG-FILE-EOF.

           CLOSE SETORES.
      *--------------------------------------------------------------*
       READ-LINE-FILE-SETORES.

           READ SETORES.

           IF NOT FLAG-FILE-EOF
                  DISPLAY
                      FD-SETOR-CODIGO    AT LINE WS-COUNT-LINE COLUMN 28
                      " - "              AT LINE WS-COUNT-LINE COLUMN 29
                      FD-SETOR-DESCRICAO AT LINE WS-COUNT-LINE COLUMN 32
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
       APLICAR-REAJUSTE.
           MOVE SPACES TO WS-STATUS-FILE.

           OPEN INPUT   PRODUTOS.
           OPEN OUTPUT  PRODUTOS-REAJUSTE.

           IF NOT FLAG-OPEN-FILE-SUCESSO
                  DISPLAY "ERROR NA ABERTURA DO ARQUIVO" AT 2029
                  STOP RUN
           END-IF.

           PERFORM REWRITE-LINE-FILE-PRODUTOS
           WITH TEST BEFORE UNTIL FLAG-FILE-EOF.

           CLOSE PRODUTOS-REAJUSTE.
           CLOSE PRODUTOS.

           DISPLAY "REAJUSTE APLICADO E DADOS EM PRODUTOS_REAJUSTE.TXT"
           AT 2029.
      *--------------------------------------------------------------*
       REWRITE-LINE-FILE-PRODUTOS.

           READ PRODUTOS.

           IF NOT FLAG-FILE-EOF
               MOVE REG-PRODUTO TO REG-PRODUTO-REAJUSTE

               IF FD-PROD-SETOR = WS-SETOR
                   COMPUTE WS-CALC-NOVO-VALOR = FD-PROD-VALOR
                                   * ( 1 + WS-PERC-AUMENTO / 100)

                   MOVE WS-CALC-NOVO-VALOR TO FD-PROD-REA-VALOR
               END-IF

               WRITE REG-PRODUTO-REAJUSTE
           END-IF.
      *--------------------------------------------------------------*
