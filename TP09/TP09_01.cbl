       IDENTIFICATION DIVISION.
       PROGRAM-ID. relat-1.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN. 26/11/2023.
       DATE-COMPILED. 26/11/2023.
       SECURITY. EXEMPLO DE RELATORIOS.
      *******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PLANILHA ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS wsstatus.

           SELECT RELAT1 ASSIGN TO "LPT1".
      *******************************************************
       DATA DIVISION.
       FILE SECTION.
           FD PLANILHA
               LABEL RECORD STANDARD
               DATA RECORD IS REGISTRO
               VALUE OF FILE-ID IS "C:\cobol\ALUNOS.DAT".
           01 REGISTRO.
               05 MATRICULA PIC X(06).
               05 NOME PIC X(35).
               05 NOTA PIC 99V9.
               05 FILLER PIC X(02).
           FD RELAT1
               LABEL RECORD OMITTED
               LINAGE 60
               FOOTING 58
               TOP 2
               BOTTOM 2.
           01 REC-RELAT PIC X(80).
       WORKING-STORAGE SECTION.
           01 CABEC1.
               05 RELDIA PIC XX.
               05 FILLER PIC X VALUE "/".
               05 RELMES PIC XX.
               05 FILLER PIC X VALUE "/".
               05 RELANO PIC XX.
               05 FILLER PIC X(12).
               05 FILLER PIC X(18) VALUE "RELATORIO DE NOTAS".
               05 FILLER PIC X(34).
               05 FILLER PIC X(5) VALUE "PAG. ".
               05 RELPAG PIC ZZ9.
           01 CABEC2.
               05 FILLER PIC x(09) value "Matric.".
               05 FILLER PIC X(40) VALUE "Nome do aluno".
               05 FILLER PIC X(10) VALUE "Media".
           01 DETALHE.
               05 RELMATRICULA PIC 9(6).
               05 FILLER PIC X(3).
               05 RELNOME PIC X(35).
               05 FILLER PIC X(5).
               05 RELNOTA PIC Z9,9.
               05 FILLER PIC X(27).
           01 RODAPE.
               05 FILLER PIC X(50).
               05 FILLER PIC x(27) value "Subtotal de registros: ".
               05 relcont pic zz9.
           01 DATAHOJE.
               05 ANO PIC 99.
               05 MES PIC 99.
               05 DIA PIC 99.
           77 WSVAL PIC 999.
           77 CONTADOR PIC 9999.
           77 FLAGFIM PIC 9.
           77 WSSTATUS PIC XX.
           77 CONTPAG PIC 999.
      *******************************************************
       PROCEDURE DIVISION.
           MOVE 1 TO CONTPAG.
           ACCEPT DATAHOJE FROM DATE.
           MOVE DIA TO RELDIA.
           MOVE MES TO RELMES.
           MOVE ANO TO RELANO.

           OPEN INPUT PLANILHA.
           IF wsstatus = "00"
               NEXT SENTENCE
           ELSE
               DISPLAY "ERRO NO ARQUIVO"
               STOP RUN.

           MOVE 0 TO CONTADOR.
           OPEN OUTPUT RELAT1.
           PERFORM CABECALHO.
           MOVE 0 TO FLAGFIM.
           PERFORM IMPRESSAO UNTIL FLAGFIM=1.
      *******************************************************
       CABECALHO.
           MOVE CONTPAG TO RELPAG.
           WRITE REC-RELAT FROM CABEC1 BEFORE ADVANCING 1 LINE.
           WRITE REC-RELAT FROM CABEC2 BEFORE ADVANCING 2 LINES.
      *******************************************************
       FIM.
           CLOSE RELAT1 PLANILHA.
           STOP RUN.
      *******************************************************
       IMPRESSAO.
           PERFORM LEITURA.

           IF NOT wsstatus = "10"
                  MOVE MATRICULA TO RELMATRICULA
                  MOVE NOME TO RELNOME
                  MOVE NOTA TO RELNOTA
                  WRITE REC-RELAT FROM DETALHE BEFORE ADVANCING 1 LINE
                  END-OF-PAGE PERFORM FIMPAGINA
                  COMPUTE CONTADOR = CONTADOR + 1
           END-IF.
      *******************************************************
       LEITURA.
           READ PLANILHA AT END
           MOVE 1 TO FLAGFIM.
      *******************************************************
       FIMPAGINA.
           MOVE CONTADOR TO RELCONT.
      * move rodape to rec-relat
           WRITE REC-RELAT FROM RODAPE before ADVANCING PAGE.
           COMPUTE CONTPAG = CONTPAG + 1.
           PERFORM CABECALHO.
      *******************************************************
