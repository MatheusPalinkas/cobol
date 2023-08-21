       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TP01.
       AUTHOR.        MATHEUS PALINKAS E JOAO TAVARES.
       INSTALLATION.  HOME.
       DATE-WRITTEN.  18/08/2023.
       DATE-COMPILED. 18/08/2023.

      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *--------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WSFICHA.
           05 WSNOME.
               10 WSPRIMEIRO-NOME      PIC A(20)      VALUE SPACE.
               10 WSSOBRENOME          PIC A(30)      VALUE SPACE.
           05 WSPRONTUARIO             PIC X(8)       VALUE SPACE.
           05 WSSALARIO.
               10 WSSALARIO-BRUTO      PIC 9(5)V9(2)  VALUE ZERO.
               10 WSSALARIO-LIQUIDO    PIC 9(5)V9(2)  VALUE ZERO.
               10 WSSALARIO-REFERENCIA PIC 9(5)V9(2)  VALUE ZERO.
               10 WSDESCONTO-INSS      PIC 9(5)V9(2)  VALUE ZERO.
               10 WSDESCONTO-IRRF      PIC 9(5)V9(2)  VALUE ZERO.
          

       77 WSHORA-TRABALHADA            PIC 9(3)V9(2)  VALUE ZERO. 
       77 WSQTD-HORAS                  PIC 9(3)       VALUE ZERO.
       77 WSQTD-DEPENTENDES            PIC 9(2)       VALUE ZERO.
       77 WSDESCONTO-DEPENDENTE        PIC 9(5)V9(2)  VALUE ZERO.

      *---- CONSTS --------------------------------------------------*  
       77 CONST-PORC-INSS              PIC 9V9(3)      VALUE 0,14.
       77 CONST-PORC-IRRF              PIC 9V9(3)      VALUE 0,275.
       77 CONST-DEDUCAO-IRRF           PIC 9(3)V9(2)   VALUE 869,36.
       77 CONST-DEDUCAO-DEPENDENTE     PIC 9(3)V9(2)   VALUE 189,59.
       
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.

       ENTRADA-DADOS.

           DISPLAY "Digite o primeiro nome: ".
           ACCEPT WSPRIMEIRO-NOME.

           DISPLAY "Digite o sobrenome: ".
           ACCEPT WSSOBRENOME.

           DISPLAY "Digite o prontuario: ".
           ACCEPT WSPRONTUARIO.

           DISPLAY "Digite o valor da hora trabalhada: ".
           ACCEPT WSHORA-TRABALHADA.
           
           DISPLAY "Digite a quantidade de horas trabalhadas: ".
           ACCEPT WSQTD-HORAS.

           DISPLAY "Digite a quantidade de dependentes: ".
           ACCEPT WSQTD-DEPENTENDES.
      
      *--------------------------------------------------------------*
       CALCULAR-SALARIO. 
                 
      *    CALCULO SALARIO BRUTO
           MULTIPLY WSHORA-TRABALHADA BY WSQTD-HORAS
           GIVING WSSALARIO-BRUTO.
      
      *    CALCULO INSS     
           MULTIPLY WSSALARIO-BRUTO BY CONST-PORC-INSS
           GIVING WSDESCONTO-INSS.
           
      *    CALCULO SALARIO DE REFERENCIA
           SUBTRACT WSDESCONTO-INSS FROM WSSALARIO-BRUTO
           GIVING WSSALARIO-REFERENCIA.

      *    CALCULO IRRF     
           MULTIPLY WSSALARIO-BRUTO BY CONST-PORC-IRRF
           GIVING WSDESCONTO-IRRF.
           
           MULTIPLY WSQTD-DEPENTENDES BY CONST-DEDUCAO-DEPENDENTE
           GIVING WSDESCONTO-DEPENDENTE.
            
           SUBTRACT CONST-DEDUCAO-IRRF WSDESCONTO-DEPENDENTE
           FROM WSDESCONTO-IRRF.
           
      *    CALCULO SALARIO LIQUIDO
           SUBTRACT WSDESCONTO-IRRF FROM WSSALARIO-REFERENCIA
           GIVING WSSALARIO-LIQUIDO.
      
      *--------------------------------------------------------------*
       SAIDA-DADOS. 
           DISPLAY "".
           DISPLAY "TP 01 - VARIAVEIS, ENTRADA DE DADOS E APRESENTACAO".
           DISPLAY "".
           DISPLAY "Nome:                     " WSPRIMEIRO-NOME.
           DISPLAY "Sobrenome:                " WSSOBRENOME.
           DISPLAY "Prontuario:               " WSPRONTUARIO.
           DISPLAY "Valor da hora:            " WSHORA-TRABALHADA.
           DISPLAY "Quantidade de horas:      " WSQTD-HORAS.
           DISPLAY "Quant. de dependentes:    " WSQTD-DEPENTENDES.
           DISPLAY "Salario Bruto:            " WSSALARIO-BRUTO.
           DISPLAY "INSS:                     " WSDESCONTO-INSS.
           DISPLAY "IRRF:                     " WSDESCONTO-IRRF.
           DISPLAY "Salario Liquido:          " WSSALARIO-LIQUIDO.
           DISPLAY "".
           DISPLAY WSFICHA.
           STOP RUN.
      *--------------------------------------------------------------*
      