       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP07_01.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  06/11/2023.
       DATE-COMPILED. 06/11/2023.
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-PRODUTOS ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE.

           SELECT FILE-PRODUTOS2 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE-WRITE.
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD FILE-PRODUTOS
           LABEL RECORD IS STANDARD
           DATA RECORD IS REG-PRODUTO
           VALUE OF FILE-ID IS "C:\cobol\produtos.TXT".

       01 REG-PRODUTO.
           05 FD-PROD-CODIGO        PIC X(5)              VALUE SPACES.
           05 FD-PROD-DESCRICAO     PIC X(40)             VALUE SPACES.
           05 FD-PROD-VALOR         PIC 9(2)V9(2)         VALUE ZEROS.
           05 FD-PROD-SETOR         PIC X                 VALUE SPACES.
           05 FD-PROD-ESTOQUE       PIC X                 VALUE SPACES.
           05 FD-PROD-ANO-ESTOQUE   PIC X(4)              VALUE SPACES.
           05 FD-PROD-MES-ESTOQUE   PIC X(2)              VALUE SPACES.

       FD FILE-PRODUTOS2
       LABEL RECORD IS STANDARD
       DATA RECORD IS REG-PRODUTO2
       VALUE OF FILE-ID IS "C:\cobol\PRODUTOS2.TXT".

       01 REG-PRODUTO2.
           05 FD-PROD2-CODIGO       PIC X(5)              VALUE SPACES.
           05 FD-PROD2-DESCRICAO    PIC X(40)             VALUE SPACES.
           05 FD-PROD2-VALOR        PIC 9(2)V9(2)         VALUE ZEROS.
           05 FD-PROD2-SETOR        PIC X                 VALUE SPACES.
           05 FD-PROD2-ESTOQUE      PIC X                 VALUE SPACES.
           05 FD-PROD2-ANO-ESTOQUE  PIC X(4)              VALUE SPACES.
           05 FD-PROD2-MES-ESTOQUE  PIC X(2)              VALUE SPACES.
      *-------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       77 WS-OPCAO                  PIC 9                 VALUE ZEROS.
       77 WS-LIMPAR-MENSAGEM        PIC X(80)             VALUE SPACES.
       77 WS-CONT                   PIC 9(3)              VALUE ZEROS.
       77 WS-TOTAL-ITENS            PIC 9(3)              VALUE ZEROS.
       77 WS-COD-CLASSE             PIC 9                 VALUE ZEROS.
       77 WS-PERC-REAJUSTE          PIC 9(3)V9(2)         VALUE ZEROS.
       77 WS-CALC-NOVO-VALOR        PIC 9(2)V9(2)         VALUE ZEROS.
       01 TB-PRODUTOS.
           02 PRODUTOS OCCURS 1000 TIMES INDEXED BY INDICE.
               10 PROD-CODIGO       PIC X(5)              VALUE SPACES.
               10 PROD-DESCRICAO    PIC X(40)             VALUE SPACES.
               10 PROD-VALOR        PIC 9(2)V9(2)         VALUE ZEROS.
               10 PROD-SETOR        PIC X                 VALUE SPACES.
               10 PROD-ESTOQUE      PIC X                 VALUE SPACES.
               10 PROD-ANO-ESTOQ    PIC X(4)              VALUE SPACES.
               10 PROD-MES-ESTOQ    PIC X(2)              VALUE SPACES.
           02 SITUACAO-TABELA       PIC 9                 VALUE ZEROS.
               88 TABELA-CARREGADA  VALUE 1.
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
       01 SC-OPCOES.
           10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
           "TP07.01 - Aplicacao com menu e tabela".

           10   LINE 7    COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "Opcoes:".

            10  LINE + 2  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "1 - carregar do arquivo para a tabela".
            10  LINE + 1  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "2 - selecionar classe de produtos para aumento".
            10  LINE + 1  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "3 - selecionar classe de produtos para desconto".
            10  LINE + 1  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "4 - salvar tabela em noco arquivo".
            10  LINE + 1  COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "5 - encerrar".

           10   LINE 16   COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "Digite a opcao: ".
           10 SC-OPCAO         LINE + 0  COLUMN  44
           USING WS-OPCAO.

           10   LINE 27 COLUMN 28     FOREGROUND-COLOR  3 VALUE
           "Desenvolvido por Matheus Palinkas e Joao Tavares".

       01 SC-PROD-AUMENTO.
           10   LINE 05   COLUMN 36     FOREGROUND-COLOR  8 VALUE
           "TP07.01 - Aplicacao com menu e tabela".

           10   LINE 16   COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "Digite o codigo da classe: ".
           10 SC-COD-CLASSE         LINE + 0  COLUMN  55
           USING WS-COD-CLASSE.

           10   LINE 17   COLUMN 28     FOREGROUND-COLOR  8 VALUE
           "Digite o percentual de aumento: %".
           10 SC-PERC-REAJUSTE         LINE + 0  COLUMN  61  PIC ZZ9,99
           USING WS-PERC-REAJUSTE.

           10   LINE 27 COLUMN 28     FOREGROUND-COLOR  3 VALUE
           "Desenvolvido por Matheus Palinkas e Joao Tavares".
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
      *--------------------------------------------------------------*
       MAIN.

           DISPLAY SC-LIMPA-TELA.
           PERFORM MOSTRAR-OPCOES.

           STOP RUN.
      *--------------------------------------------------------------*
       MOSTRAR-OPCOES.
           DISPLAY SC-OPCOES.
           ACCEPT SC-OPCAO.

           EVALUATE WS-OPCAO
               WHEN "1"
                   PERFORM CARREGAR-ARQUIVO-PARA-TABELA
               WHEN "2"
                   PERFORM CLASSE-PRODUTOS-PARA-AUMENTO
               WHEN "3"
                   PERFORM CLASSE-PRODUTOS-PARA-DESCONTO
               WHEN "4"
                   PERFORM SALVAR-TABELA-NOVO-ARQUIVO
               WHEN "5"
                   PERFORM ENCERRAR-PROGRAMA
               WHEN OTHER
                   DISPLAY WS-LIMPAR-MENSAGEM AT 2028
                   DISPLAY "Opcao invalida" FOREGROUND-COLOR 6 AT 2028
           END-EVALUATE.

           PERFORM MOSTRAR-OPCOES.
      *--------------------------------------------------------------*
       CARREGAR-ARQUIVO-PARA-TABELA.
           DISPLAY WS-LIMPAR-MENSAGEM AT 2028.

           OPEN INPUT FILE-PRODUTOS.

           IF NOT FLAG-OPEN-FILE-SUCESSO
                  DISPLAY "ERROR NA ABERTURA DO ARQUIVO"
                  FOREGROUND-COLOR 6 AT 2028
                  STOP RUN
           END-IF.

           PERFORM CARREGAR-LINHA-TABELA UNTIL FLAG-FILE-EOF.

           CLOSE FILE-PRODUTOS.

           DISPLAY "Tabela carregada com sucesso"
           FOREGROUND-COLOR 2 AT 2028.
           MOVE 1 TO SITUACAO-TABELA.
      *--------------------------------------------------------------*
       CLASSE-PRODUTOS-PARA-AUMENTO.
           PERFORM VALIDAR-CARREGAMENTO-TABELA.

           IF TABELA-CARREGADA
               PERFORM ENTRADA-CLASSE-PARA-AUMENTO
           END-IF.

      *--------------------------------------------------------------*
       CLASSE-PRODUTOS-PARA-DESCONTO.
           PERFORM VALIDAR-CARREGAMENTO-TABELA.

           IF TABELA-CARREGADA
               PERFORM ENTRADA-CLASSE-PARA-DESCONTO
           END-IF.
      *--------------------------------------------------------------*
       SALVAR-TABELA-NOVO-ARQUIVO.
           PERFORM VALIDAR-CARREGAMENTO-TABELA.

           IF TABELA-CARREGADA
               PERFORM SALVAR-NOVO-ARQUIVO

               DISPLAY "Novo arquivo salvo com sucesso"
               FOREGROUND-COLOR 2 AT 2028
           END-IF.
      *--------------------------------------------------------------*
       ENCERRAR-PROGRAMA.
           DISPLAY WS-LIMPAR-MENSAGEM AT 2028.

           DISPLAY "Obrigado por utilizar o programa...ate logo :)"
           FOREGROUND-COLOR 2 AT 2028.

           STOP RUN.
      *--------------------------------------------------------------*
       CARREGAR-LINHA-TABELA.

           READ FILE-PRODUTOS.

           IF NOT FLAG-FILE-EOF
               MOVE REG-PRODUTO TO PRODUTOS(INDICE)
               SET INDICE UP BY 1

               ADD 1 TO WS-TOTAL-ITENS
           END-IF.
      *--------------------------------------------------------------*
       VALIDAR-CARREGAMENTO-TABELA.
           DISPLAY WS-LIMPAR-MENSAGEM AT 2028.

           IF NOT TABELA-CARREGADA
               DISPLAY
               "Para executar essa funcao primeiro carregue a tabela"
               FOREGROUND-COLOR 6 AT 2028
           END-IF.
      *--------------------------------------------------------------*
       SALVAR-NOVO-ARQUIVO.
           OPEN OUTPUT FILE-PRODUTOS2.

           MOVE ZEROS TO WS-CONT.

           PERFORM VARYING WS-CONT FROM 1 BY 1
           UNTIL WS-CONT > WS-TOTAL-ITENS
               MOVE PRODUTOS(WS-CONT) TO REG-PRODUTO2
               WRITE REG-PRODUTO2
           END-PERFORM.

           CLOSE FILE-PRODUTOS2.
      *--------------------------------------------------------------*
       ENTRADA-CLASSE-PARA-AUMENTO.

           DISPLAY SC-LIMPA-TELA.
           DISPLAY SC-PROD-AUMENTO

           PERFORM ENTRADA-COD-CLASSE.
           PERFORM ENTRADA-PERC-AUMENTO.

           PERFORM APLICAR-REAJUSTE-AUMENTO.
           DISPLAY SC-LIMPA-TELA.
      *--------------------------------------------------------------*
       ENTRADA-CLASSE-PARA-DESCONTO.

           DISPLAY SC-LIMPA-TELA.
           DISPLAY SC-PROD-AUMENTO

           PERFORM ENTRADA-COD-CLASSE.
           PERFORM ENTRADA-PERC-AUMENTO.

           PERFORM APLICAR-REAJUSTE-DESCONTO.
           DISPLAY SC-LIMPA-TELA.
      *--------------------------------------------------------------*
       ENTRADA-COD-CLASSE.

           ACCEPT SC-COD-CLASSE.

           IF WS-COD-CLASSE < 0 OR WS-COD-CLASSE > 5
               DISPLAY "Classe invalida - Digite um valor de 1 a 5"
               AT 2128
               FOREGROUND-COLOR 6
               PERFORM ENTRADA-COD-CLASSE
           END-IF.

           DISPLAY WS-LIMPAR-MENSAGEM AT 2128.
      *--------------------------------------------------------------*
       ENTRADA-PERC-AUMENTO.

           ACCEPT SC-PERC-REAJUSTE.

           IF WS-PERC-REAJUSTE <= 0
               DISPLAY "Percentual Invalido - Escolha novamente" AT 2128
               FOREGROUND-COLOR 6
               PERFORM ENTRADA-PERC-AUMENTO
           END-IF.

           DISPLAY "                                       " AT 2128.
      *--------------------------------------------------------------*
       APLICAR-REAJUSTE-AUMENTO.

           MOVE ZEROS TO WS-CONT .

           PERFORM VARYING WS-CONT FROM 1 BY 1
           UNTIL WS-CONT > WS-TOTAL-ITENS

               IF PROD-SETOR(WS-CONT) = WS-COD-CLASSE
                   COMPUTE WS-CALC-NOVO-VALOR = PROD-VALOR(WS-CONT)
                                   * ( 1 + WS-PERC-REAJUSTE / 100)

                   MOVE WS-CALC-NOVO-VALOR TO PROD-VALOR(WS-CONT)
               END-IF
           END-PERFORM.
      *--------------------------------------------------------------*
       APLICAR-REAJUSTE-DESCONTO.

           MOVE ZEROS TO WS-CONT .

           PERFORM VARYING WS-CONT FROM 1 BY 1
           UNTIL WS-CONT > WS-TOTAL-ITENS

               IF PROD-SETOR(WS-CONT) = WS-COD-CLASSE
                   COMPUTE WS-CALC-NOVO-VALOR = PROD-VALOR(WS-CONT)
                                   * ( 1 - WS-PERC-REAJUSTE / 100)

                   MOVE WS-CALC-NOVO-VALOR TO PROD-VALOR(WS-CONT)
               END-IF
           END-PERFORM.
      *--------------------------------------------------------------*
