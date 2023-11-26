       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP09_02.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN. 26/11/2023.
       DATE-COMPILED. 26/11/2023.
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUTOS ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE.

           SELECT RELAT-PRODUTOS ASSIGN TO "LPT1".
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
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

       FD RELAT-PRODUTOS
           LABEL RECORD OMITTED
           LINAGE 15
           FOOTING 14
           TOP 2
           BOTTOM 2.
       01 REC-RELAT-PRODUTOS PIC X(80).
      *--------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       77 TOTAL-PAG PIC 9(4)V99 VALUE ZEROS.
       77 CONTADOR PIC 9999.
       77 CONTPAG PIC 999.
       77 FLAGFIM PIC 9.
       77 WS-STATUS-FILE            PIC X(02)             VALUE SPACES.
           88 FLAG-OPEN-FILE-SUCESSO   VALUE "00".
           88 FLAG-FILE-EOF            VALUE "10".
           88 FLAG-SETORES-ERRO        VALUE "30".
       01 HEADER-PAG.
           05 FILLER PIC X(25) VALUE SPACES.
           05 FILLER PIC X(20) VALUE "Listagem de produtos".
           05 FILLER PIC X(25) VALUE SPACES.
           05 FILLER PIC X(07) VALUE "página ".
           05 RELPAG PIC ZZ9.
       01 HEADER-ITENS.
           05 FILLER PIC X(07) value "Código".
           05 FILLER PIC X(01) value SPACES.
           05 FILLER PIC X(50) VALUE "Descrição".
           05 FILLER PIC X(01) value SPACES.
           05 FILLER PIC X(05) VALUE "Setor".
           05 FILLER PIC X(01) value SPACES.
           05 FILLER PIC X(08) VALUE "Estoque?".
           05 FILLER PIC X(01) value SPACES.
           05 FILLER PIC X(06) VALUE "Preço".
       01 FOOTER.
           05 FILLER PIC X(25) VALUE "Total de valores listados".
           05 FILLER PIC X(45) VALUE SPACES.
           05 TOTAL-PARCIAL PIC $ZZZ9,99.
       01 LINE-PRODUTO.
           05 PROD-CODIGO PIC X(05) value SPACES.
           05 FILLER PIC X(03) value SPACES.
           05 PROD-DESCRICAO PIC X(40) VALUE SPACES.
           05 FILLER PIC X(10) value SPACES.
           05 PROD-SETOR PIC X(01) VALUE SPACES.
           05 FILLER PIC X(05) value SPACES.
           05 PROD-ESTOQUE PIC X(03) VALUE SPACES.
           05 FILLER PIC X(05) value SPACES.
           05 PROD-VALOR PIC $Z9,99 VALUE ZEROS.
      *******************************************************
       PROCEDURE DIVISION.
           PERFORM ABRIR-ARQUIVOS.

           PERFORM WRITE-CABECALHO.

           PERFORM IMPRIMIR-LINHA UNTIL FLAGFIM = 1.

           PERFORM FECHAR-ARQUIVOS.
      *--------------------------------------------------------------*
       ABRIR-ARQUIVOS.
           OPEN INPUT PRODUTOS.

           IF NOT FLAG-OPEN-FILE-SUCESSO
                  DISPLAY "ERROR NA ABERTURA DO ARQUIVO" AT 2029
                  STOP RUN
           END-IF.

           OPEN OUTPUT RELAT-PRODUTOS.

           MOVE 1 TO CONTPAG.
           MOVE 0 TO FLAGFIM.
           MOVE 0 TO CONTADOR.
      *--------------------------------------------------------------*
       WRITE-CABECALHO.
           MOVE CONTPAG TO RELPAG.
           WRITE REC-RELAT-PRODUTOS FROM HEADER-PAG
           BEFORE ADVANCING 1 LINE.

           WRITE REC-RELAT-PRODUTOS FROM HEADER-ITENS
           BEFORE ADVANCING 2 LINES.
      *--------------------------------------------------------------*
       IMPRIMIR-LINHA.
           READ PRODUTOS AT END
           MOVE 1 TO FLAGFIM.

           ADD FD-PROD-VALOR TO TOTAL-PAG.

           IF NOT FLAG-FILE-EOF
                  PERFORM MOVE-PROD-TO-LINE

                  WRITE REC-RELAT-PRODUTOS
                  FROM LINE-PRODUTO BEFORE ADVANCING 1 LINE
                  END-OF-PAGE PERFORM FIMPAGINA

                  ADD 1 TO CONTADOR
           END-IF.
      *--------------------------------------------------------------*
       MOVE-PROD-TO-LINE.
           MOVE FD-PROD-CODIGO     TO PROD-CODIGO.
           MOVE FD-PROD-DESCRICAO  TO PROD-DESCRICAO.
           MOVE FD-PROD-SETOR      TO PROD-SETOR.
           MOVE FD-PROD-VALOR      TO PROD-VALOR.

           IF FLAG-ESTOQUE-PRESETE
               MOVE "Sim" TO PROD-ESTOQUE
           ELSE
               MOVE "Não" TO PROD-ESTOQUE
           END-IF.
      *--------------------------------------------------------------*
       FIMPAGINA.
           ADD 1 TO CONTPAG.
           MOVE TOTAL-PAG TO TOTAL-PARCIAL.
           MOVE 0 TO TOTAL-PAG.

           WRITE REC-RELAT-PRODUTOS FROM FOOTER BEFORE ADVANCING PAGE.

           PERFORM WRITE-CABECALHO.
      *--------------------------------------------------------------*
       FECHAR-ARQUIVOS.
           CLOSE RELAT-PRODUTOS PRODUTOS.
           STOP RUN.
      *--------------------------------------------------------------*
