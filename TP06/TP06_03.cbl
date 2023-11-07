       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP06_03.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  05/11/2023.
       DATE-COMPILED. 05/11/2023.
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COTAHIST_M092023 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE.

           SELECT EXTRACAO ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE-WRITE.
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD COTAHIST_M092023
           LABEL RECORD IS STANDARD
           DATA RECORD IS REG-COTA
           VALUE OF FILE-ID IS "C:\cobol\COTAHIST_M092023.TXT".

       01 REG-COTA.
           05 FD-CAMPO-COTA             PIC X(245)             VALUE SPACES.

       FD EXTRACAO
           LABEL RECORD IS STANDARD
           DATA RECORD IS REG-EXTRACAO
           VALUE OF FILE-ID IS "C:\cobol\EXTRACAO.TXT".

       01 REG-EXTRACAO.
           05 FD-CAMPO-EXTRACAO         PIC X(245)        VALUE SPACES.

       WORKING-STORAGE SECTION.
       77 WS-COD-NEGOCIACAO             PIC X(12)         VALUE SPACES.
       77 WS-TOTAL-REGISTROS-EXP        PIC 9(8)          VALUE ZEROS.
       01 DATE-NOW.
           05 DATE-YEAR                 PIC 9(4)          VALUE ZEROS.
           05 DATE-MONTH                PIC 99            VALUE ZEROS.
           05 DATE-DAY                  PIC 99            VALUE ZEROS.
           05 DATE-HOUR                 PIC 99            VALUE ZEROS.
           05 DATE-MINUTE               PIC 99            VALUE ZEROS.
           05 DATE-SECOND               PIC 99            VALUE ZEROS.
           05 DATE-HUNDREDTHS           PIC 99            VALUE ZEROS.
       01 DATE-NOW-FORMAT.
           05 NOW-DAY                   PIC 99            VALUE ZEROS.
           05 NOW-MONTH                 PIC 99            VALUE ZEROS.
           05 NOW-YEAR                  PIC 9(4)          VALUE ZEROS.
      *---- FILE COTAS ---------------------------------------------*
       01 WS-COTA-CABECALHO.
           05 WS-TIPO-FILE              PIC 9(2)          VALUE ZEROS.
           05 WS-NOME-FILE              PIC X(13)         VALUE SPACES.
           05 WS-COD-ORIGEM             PIC X(8)          VALUE SPACES.
           05 WS-GERACAO-FILE           PIC 9(8)          VALUE ZEROS.
           05 WS-RESERVA                PIC X(214)        VALUE SPACES.
       01 WS-COTA-LINHA.
           05 WS-TIPREG-LINE            PIC 9(2)          VALUE ZEROS.
           05 WS-DTPREGAO-LINE.
               10 WS-DTPREGAO-LINE-ANO  PIC 9(4)          VALUE ZEROS.
               10 WS-DTPREGAO-LINE-MES  PIC 9(2)          VALUE ZEROS.
               10 WS-DTPREGAO-LINE-DIA  PIC 9(2)          VALUE ZEROS.
           05 WS-CODBDI-LINE            PIC X(2)          VALUE SPACES.
           05 WS-CODNEG-LINE            PIC X(12)         VALUE SPACES.
           05 WS-TPMERC-LINE            PIC 9(3)          VALUE ZEROS.
           05 WS-NOMRES-LINE            PIC X(12)         VALUE SPACES.
           05 WS-ESPECI-LINE            PIC X(10)         VALUE SPACES.
           05 WS-PRAZOT-LINE            PIC X(3)          VALUE SPACES.
           05 WS-MODREF-LINE            PIC X(4)          VALUE SPACES.
           05 WS-PREABE-LINE            PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-PREMAX-LINE            PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-PREMIN-LINE            PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-PREMED-LINE            PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-PREULT-LINE            PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-PREOFC-LINE            PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-PREOFV-LINE            PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-TOTNEG-LINE            PIC 9(5)          VALUE ZEROS.
           05 WS-QUATOT-LINE            PIC 9(18)         VALUE ZEROS.
           05 WS-VOLTOT-LINE            PIC 9(16)V9(2)    VALUE ZEROS.
           05 WS-PREEXE-LINE            PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-INDOPC-LINE            PIC 9(1)          VALUE ZEROS.
           05 WS-DATVEN-LINE            PIC 9(8)          VALUE ZEROS.
           05 WS-FATCOT-LINE            PIC 9(7)          VALUE ZEROS.
           05 WS-PTOEXE-LINE            PIC 9(3)V9(6)     VALUE ZEROS.
           05 WS-CODISI-LINE            PIC X(12)         VALUE SPACES.
           05 WS-DISMES-LINE            PIC 9(3)          VALUE ZEROS.
       01 WS-COTA-RODAPE.
           05 WS-TIPO-REG               PIC 9(2)          VALUE ZEROS.
           05 WS-NOME-FILE-ROD          PIC X(13)         VALUE SPACES.
           05 WS-COD-ORIGEM-ROD         PIC X(8)          VALUE SPACES.
           05 WS-DTGERACAO-FILE         PIC 9(8)          VALUE ZEROS.
           05 WS-TOTAL-REGS             PIC 9(11)         VALUE ZEROS.
           05 WS-RESERVA                PIC X(203)        VALUE SPACES.
      *---- FILE EXPORTACAO ----------------------------------------*
       01 WS-EXP-CABECALHO.
           05 WS-TIPO                   PIC X(2)          VALUE "00".
           05 WS-DT-EXEC                PIC 9(8)          VALUE ZEROS.
           05 WS-COD-NEG                PIC X(12)         VALUE SPACES.
           05 WS-NOME-RESUMIDO          PIC X(12)         VALUE SPACES.
           05 WS-NOME-EXEC-PROGRAM      PIC X(40)         VALUE SPACES.
           05 WS-RESERVA                PIC X(6)          VALUE SPACES.
       01 WS-EXP-LINHA.
           05 WS-TIPO                   PIC X(2)          VALUE "01".
           05 WS-DATA-EXEC.
               10 WS-DATA-EXEC-DIA      PIC 9(2)          VALUE ZEROS.
               10 WS-DATA-EXEC-MES      PIC 9(2)          VALUE ZEROS.
               10 WS-DATA-EXEC-ANO      PIC 9(4)          VALUE ZEROS.
           05 WS-PRECO-ABERTURA         PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-PRECO-MIN              PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-PRECO-MAX              PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-PRECO-ULT-NEG          PIC 9(11)V9(2)    VALUE ZEROS.
           05 WS-QTD-TITULOS            PIC 9(18)         VALUE ZEROS.
       01 WS-EXP-RODAPE.
           05 WS-TIPO                   PIC X(2)          VALUE "99".
           05 WS-TOTAL-REGISTROS        PIC 9(9)          VALUE ZEROS.
           05 WS-RESERVA                PIC X(70)         VALUE SPACES.

      *---- STATUS FILE --------------------------------------------*
       77 WS-STATUS-FILE-WRITE          PIC X(02)         VALUE SPACES.
       77 WS-STATUS-FILE                PIC X(02)         VALUE SPACES.
           88 FLAG-OPEN-FILE-SUCESSO   VALUE "00".
           88 FLAG-FILE-EOF            VALUE "10".
           88 FLAG-SETORES-ERRO        VALUE "30".
      *---- SCREENS ------------------------------------------------*
       SCREEN SECTION.
       01 SC-LIMPA-TELA.
           05 BLANK SCREEN.
       01 SC-INPUT.
           10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
           "TP06.03 - Codigo de negociacao do papeL".

           10   LINE 16   COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "Digite o codigo da negociacao: ".
           10 SC-COD-NEGOCIACAO         LINE + 0  COLUMN  60
           USING WS-COD-NEGOCIACAO.

           10   LINE 27 COLUMN 28     FOREGROUND-COLOR  3 VALUE
           "Desenvolvido por Matheus Palinkas e Joao Tavares".
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
      *--------------------------------------------------------------*
       MAIN.

           DISPLAY SC-LIMPA-TELA.
           PERFORM ENTRADA-COD-NEGOCIACAO.
           PERFORM EXPORTAR-COTAS.

           STOP RUN.
      *--------------------------------------------------------------*
       ENTRADA-COD-NEGOCIACAO.
           DISPLAY SC-INPUT.
           ACCEPT SC-COD-NEGOCIACAO.
      *--------------------------------------------------------------*
       EXPORTAR-COTAS.

           OPEN INPUT COTAHIST_M092023.
           OPEN OUTPUT EXTRACAO.

           IF NOT FLAG-OPEN-FILE-SUCESSO
                  DISPLAY "ERROR NA ABERTURA DO ARQUIVO" AT 2029
                  STOP RUN
           END-IF.

           PERFORM READ-HEADER-COTA.

           PERFORM READ-LINE-COTA UNTIL FLAG-FILE-EOF.

           PERFORM READ-FOOTER-COTA.
           PERFORM WRITE-RODAPE-EXPORTACAO.

           CLOSE EXTRACAO.
           CLOSE COTAHIST_M092023.
      *--------------------------------------------------------------*
       READ-FOOTER-COTA.

           READ COTAHIST_M092023.

           MOVE REG-COTA TO WS-COTA-RODAPE.
      *--------------------------------------------------------------*
       READ-LINE-COTA.

           READ COTAHIST_M092023.

           MOVE REG-COTA TO WS-COTA-LINHA.

           IF WS-CODNEG-LINE = WS-COD-NEGOCIACAO
               IF WS-TOTAL-REGISTROS-EXP = ZEROS
                    PERFORM WRITE-HEADER-EXPORTACAO
               END-IF

               PERFORM WRITE-LINHA-EXPORTACAO
               ADD 1 TO WS-TOTAL-REGISTROS-EXP
           END-IF.
      *--------------------------------------------------------------*
       READ-HEADER-COTA.

           READ COTAHIST_M092023.

           MOVE REG-COTA TO WS-COTA-CABECALHO.
      *--------------------------------------------------------------*
       WRITE-HEADER-EXPORTACAO.
           PERFORM GET-DATE-NOW.

           MOVE DATE-NOW-FORMAT               TO WS-DT-EXEC.
           MOVE WS-COD-NEGOCIACAO             TO WS-COD-NEG.
           MOVE WS-NOMRES-LINE                TO WS-NOME-RESUMIDO.
           MOVE "MATHEUS PALINKAS E JOAO TAVARES"
                                              TO WS-NOME-EXEC-PROGRAM.
           MOVE WS-EXP-CABECALHO              TO REG-EXTRACAO.

           WRITE REG-EXTRACAO.
      *--------------------------------------------------------------*
       WRITE-RODAPE-EXPORTACAO.
           IF WS-TOTAL-REGISTROS-EXP <= 0
               DISPLAY
               "NAO FORAM ENCONTRADAS NEGOCIACOES COM ESSE CODIGO"
               FOREGROUND-COLOR 6 AT 2029
           ELSE
               MOVE WS-TOTAL-REGISTROS-EXP    TO WS-TOTAL-REGISTROS
               MOVE WS-EXP-RODAPE             TO REG-EXTRACAO

               WRITE REG-EXTRACAO

               DISPLAY "ARQUIVO DE EXPORTACAO GERADO COM SUCESSO"
               AT 2029
           END-IF.
      *--------------------------------------------------------------*
       WRITE-LINHA-EXPORTACAO.
           MOVE WS-DTPREGAO-LINE-ANO          TO WS-DATA-EXEC-ANO.
           MOVE WS-DTPREGAO-LINE-MES          TO WS-DATA-EXEC-MES.
           MOVE WS-DTPREGAO-LINE-DIA          TO WS-DATA-EXEC-DIA.

           MOVE WS-PREABE-LINE                TO WS-PRECO-ABERTURA.
           MOVE WS-PREMIN-LINE                TO WS-PRECO-MIN.
           MOVE WS-PREMAX-LINE                TO WS-PRECO-MAX.
           MOVE WS-PREULT-LINE                TO WS-PRECO-ULT-NEG.
           MOVE WS-QUATOT-LINE                TO WS-QTD-TITULOS.

           MOVE WS-EXP-LINHA                  TO REG-EXTRACAO.

           WRITE REG-EXTRACAO.
      *--------------------------------------------------------------*
       GET-DATE-NOW.
           MOVE FUNCTION CURRENT-DATE TO DATE-NOW.

           MOVE DATE-YEAR                 TO NOW-YEAR.
           MOVE DATE-MONTH                TO NOW-MONTH.
           MOVE DATE-DAY                  TO NOW-DAY.
      *--------------------------------------------------------------*
