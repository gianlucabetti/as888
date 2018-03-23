000100970526     H*------------------------------------------------------------------------*
000200120104     H* Gestione codici clienti strategi
000300970526     H*------------------------------------------------------------------------*
000400090714     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900170915     FtiS725d3  cf   e             WORKSTN INFDS(DEVDS1)
001000170915     F                                     SFILE(S7253DMS:MSGULT)
001100970527     F*------------
001200170915     Ftianv01L  UF A E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002000000215     D*------------
002100000215     D* DESCRIZIONI
002200000215     D*------------
002300000215     D DES             S             66    DIM(5) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700120327     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI TASTI FUNZIONALI
003000970526     D*------------
003100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* FUNZIONI DA CARICARE A VIDEO
003400970526     D*------------
003500970526     D FUV             S             19    DIM(24)
003600970526     D*------------------------------------------------------------------------*
003700970526     D* INPUT
003800970526     D*------------------------------------------------------------------------*
003900970526     D*------------
004000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004100970526     D*------------
004200970526     D FUDDS           DS
004300970526     D  FUDDE1                 1      1
004400970526     D  FUDFUN                 2      3
004500970526     D  FUDDE2                 4     19
004600970526     D  FUDFI0                20     34
004700970526     D  FUDATT                35     35
004800970526     D  FUDFI1                36     80
004900970526     D  FUDDES                 1     19
005000970526     D  FUDRIG                 1     80
005100970526     D*------------
005200970526     D* DS GESTIONE TASTI FUNZIONALI
005300970526     D*------------
005400970526     D DSKEY         E DS
005500970526     D DEVDS1          DS
005600970526     D  KEY                  369    369
005700970526     D*------------
005800970526     D* DS "XMSG" - GESTIONE MESSAGGI
005900970526     D*------------
006000970526     D DSMSG         E DS
006100000317     D*------------
006200000807     D* REPERIMENTO CLIENTI DA CLIENTE UNIFICANTE
006300000317     D*------------
006400000807     D tibs10ds      E DS
006500970528     D*------------------
006600970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006700970528     D*------------------
006800000317     D WLBDA8          DS                  INZ
006900970528     D  G08DAT                 1      8  0
007000970528     D  G08INV                 9     16  0
007100970528     D  G08ERR                17     17
007200970528     D  G08TGI                18     22  0
007300970526     D*------------
007400970526     D* DS DI PROCEDURA
007500970526     D*------------
007600991014     D TIBS02DS      E DS
007700170915     D TIS725DS3     e ds
007800970526     D*------------
007900970526     D* ARCHITETTURA
008000970526     D*------------
008100970526     D KPJBA         E DS
008200000807     D*------------
008300000807     D* TABELLE "XCLIR"- RICERCA CLIENTI
008400000807     D ksc             s              7  0 dim(30)
008500000807     D ksa             s              4    dim(30)
008600020212     D isvkun          s              2    dim(100)
008700050118     D isvtt           s              2    dim(100)
008800100913     D isvftp          s              2    dim(100)
008900100217     D isvKEYS         s              2    dim(100)
009000100217     D isvCTRL         s              4    dim(100)
009100101006     D isvPGMAFT       s              3    dim(100)
009200000807     D*------------------
009300000807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
009400000807     D*------------------
009500000807     DDSXC           e DS                  extname(XCLIRDS)
009600000807     D*---
009700000807     D                 DS
009800000807     D dsksn                   1      4p 0
009900000807     D dsksa                   1      4
010000000807     D*---
010100000807     D* VARIABILI NON RIFERITE AL DATABASE
010200000807     D*---
010300000807     D $esikun         S              1                                         *Esistenza KUN
010400000807     D chk             S              1    inz('1')
010500000807     D tipprf          s              3    inz(*blanks)
010600040422     D cmd             s            500
010700040422     D api             s              1    inz('''')
010800090611     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
010900091027     D sktle           S              2    DIM(20)
011000091027     D jtle            S              5  0                                      *indici
011100100914     D OPERAZ          S              3A   inz
011200100914     D savKPJBU        S                   like(KPJBU)
011300100914     D ds_param        S           1024    inz
011400100913     D*
011500100913     D                 ds
011600100913     D  val                          63A   INZ(' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123-
011700100913     D                                     456789abcdefghijklmnopqrstuvwxyz')
011800100913     D  valAry                        1A   DIM(63)
011900100913     D                                     OVERLAY(val)
012000000807     D*------------
012100000807     D* SCOMPOSIZIONE CODICI CLIENTI IN FORMATO "STRATEGI"
012200000807     D*------------
012300000807     D                 DS
012400000807     D  FILCLI                 5      7  0
012500000807     D  STRATECLI              1     11
012600020924     D*-------------------
012700020924     D* DS REPERIMENTO DATI UTENTE
012800020924     D*-------------------
012900020924     D TIBS34DS      E DS                                                       *Profili utente
013000020924     D DDATIUTE      E DS                                                       *Dati utente
013100020924     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
013200111216     D*------------------
013300111216     D* Variabili x PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
013400111216     D*------------------
013500111216     D currSysNeta     s              8A   inz('*NULL')
013600111216     D AS400_ok        s              1A
013700111216     D  PiOutSysNeta   S              8A
013800090610     D*-------------------
013900090610     D* DEFINIZIONI PROCEDURE ESTERNE
014000090610     D*-------------------
014100111216     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
014200970526     C*------------------------------------------------------------------------*
014300970526     C* MAIN LINES
014400970526     C*------------------------------------------------------------------------*
014500970526     C*
014600970526     C* NON CI SONO ERRORI IN ENTRATA
014700000915IF  1C*    ERRL00        IFEQ      '0'
014800970526     C*
014900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015000970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015300970526     C*
015400970526     C* EMETTE FORMATO VIDEO
015500970526     C                   EXSR      BUTFMR
015600970526     C*
015700970526     C* GESTIONE TASTI FUNZIONALI
015800970526     C                   EXSR      FUNCON
015900970526E   2C                   ENDDO
016000001116     C*
016100970526     C* FINE PGM
016200001116     C                   EXSR      FINSR
016300101022     C*
016400970526     C                   SETON                                        LR
016500100913     C*------------------------------------------------------------------------*
016600100913     C* RESETIN - RESET INDIRCATORI
016700100913     C*------------------------------------------------------------------------*
016800100913     C     RESETIN       BEGSR
016900100913     C*
017000100913     C* REIMPOSTA INDICATORI DI ERRORE
017100100913     C                   SETOFF                                         5859
017200100913     C                   SETOFF                                       606162
017300100913     C                   SETOFF                                       636465
017400100913     C                   SETOFF                                       666768
017500100913     C                   SETOFF                                       697071
017600100913     C                   SETOFF                                       727374
017700100913     C                   SETOFF                                       757677
017800100913     C                   SETOFF                                       787980
017900100913     C*
018000100913     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
018300970526     C*------------------------------------------------------------------------*
018400970526     C     BUTFMR        BEGSR
018500970526     C*
018600970526     C* EMETTE LA TESTATA
018700170915     C                   WRITE     S7253D01
018800970526     C*
018900970526     C* SPEDISCE EVENTUALE MESSAGGIO
019000970526     C                   EXSR      SNDMSG
019100970526     C*
019200970526     C* EMETTE LE FUNZIONI
019300170915     C                   WRITE     S7253D02
019400970526     C*
019500970526     C* EMETTE IL VIDEO
019600170915     C                   EXFMT     S7253D03
019700970526     C*
019800970526     C* REIMPOSTA FLAG DI ERRORE
019900970526     C                   MOVEL     '0'           ERRL00
020000100913     C*
020100100913     C* RESETTA INDICATORI DI CONTROLLO
020200100913     C                   EXSR      RESETIN
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
020700970526     C*------------------------------------------------------------------------*
020800970526     C     SNDMSG        BEGSR
020900970526     C*
021000970526     C* PULISCE IL SFILE MESSAGGI
021100970526     C                   MOVEL     '1'           *IN40
021200170915     C                   WRITE     S7253DMC
021300970526     C                   MOVEL     '0'           *IN40
021400970526     C*
021500970526     C* VISUALIZZA IL MESSAGGIO
021600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
021700970526     C                   CALL      'XMSG'        XMSGPA
021800970526IF  1C     DSMSER        IFEQ      *ZEROS
021900970526     C                   MOVEL     DSMSKE        MSGKEY
022000970526E   1C                   ENDIF
022100970526     C                   Z-ADD     1             MSGULT
022200170915     C                   WRITE     S7253DMS
022300170915     C                   WRITE     S7253DMC
022400970526     C*
022500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
022600970526     C                   MOVEL     *BLANKS       DSMSMS
022700970526     C*
022800970526     C                   ENDSR
022900970526     C*------------------------------------------------------------------------*
023000970526     C* ENTGES - GESTIONE ENTER -> INVIO
023100970526     C*------------------------------------------------------------------------*
023200970526     C     ENTGES        BEGSR
023300970526     C*
023400970526     C* CONTROLLA I CAMPI IMMESSI
023500970526     C                   EXSR      DECODI
023600970526     C*
023700970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
023800970526IF  1C     DEPOP1        IFEQ      'O05'
023900970526     C                   MOVEL     '1'           FINL00
024000970526E   1C                   ENDIF
024100970526     C*
024200970526     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400970526     C* F03GES - GESTIONE F03 -> FINE
024500970526     C*------------------------------------------------------------------------*
024600970526     C     F03GES        BEGSR
024700970526     C*
024800970526     C* IMPOSTA "PREMUTO F03"
024900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
025000170915     C                   MOVEL     '1'           S253F03                       *DI PROCEDURA
025100970526     C*
025200970526     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
025500970526     C*------------------------------------------------------------------------*
025600970526     C     F06GES        BEGSR
025700970526     C*
025800970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
025900970526IF  1C     DEPOP1        IFEQ      'F06'
026000100914     C                   EVAL      OPERAZ = 'INS'
026100970526     C                   EXSR      INSERI
026200970526E   1C                   ENDIF
026300970526IF  1C     DEPOP1        IFEQ      'O02'
026400100914     C                   EVAL      OPERAZ = 'UPD'
026500970526     C                   EXSR      MODIFI
026600970526E   1C                   ENDIF
026700991014IF  1C     DEPOP1        IFEQ      'O03'
026800170915     C                   EVAL      OPERAZ = 'INS'
026900170915     C***                EXSR      COPIA
027000991014E   1C                   ENDIF
027100021024IF  1C     DEPOP1        IFEQ      'O04'
027200100914     C                   EVAL      OPERAZ = 'DEL'
027300170915     C***                EXSR      ANNULL
027400021024E   1C                   ENDIF
027500101012     C*
027600101012     C                   ENDSR
027700970526     C*------------------------------------------------------------------------*
027800970526     C* F12GES - GESTIONE F12 -> RITORNO
027900970526     C*------------------------------------------------------------------------*
028000970526     C     F12GES        BEGSR
028100970526     C*
028200970526     C* IMPOSTA "PREMUTO F12"
028300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
028400170915     C                   MOVEL     '1'           S253F12                        *DI PROCEDURA
028500970526     C*
028600970526     C                   ENDSR
028700970526     C*------------------------------------------------------------------------*
028800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028900970526     C*------------------------------------------------------------------------*
029000970526     C     F24GES        BEGSR
029100970526     C*
029200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
029300970526     C*
029400970526     C                   ENDSR
029500970526     C*------------------------------------------------------------------------*
029600010605     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
029700970526     C*------------------------------------------------------------------------*
029800970526     C     INSERI        BEGSR
029900970526     C*
030000970526     C* CONTROLLA I CAMPI IMMESSI
030100970526     C                   EXSR      DECODI
030200970526     C*
030300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030400010605     C     ERRL00        IFEQ      '0'                                          *ANCORA NO ERRORI
030500170915     C                   eval      ANVCLI = S253KSU
030600170915     C                   eval      ANVDES = D03Des
030700170915     C                   eval      ANVCOD = D03Sito + D03Depo
030800170915     C                   eval      ANVCO1 = '199'
030900170915     C                   write     TIANV000
031000170915     C                   MOVEL     MSG(2)        S253MSG                        *MESSAGGIO
031100100914     C                   MOVEL     MSG(2)        DSMSMS                         *MESSAGGIO
031200170915     C                   MOVEL     '1'           FINL00                         *FINE PGM
031300170915     C                   ENDIF
031400970526     C*
031500970526     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* MODIFI - OPERAZIONI DI MODIFICA
031800970526     C*------------------------------------------------------------------------*
031900970526     C     MODIFI        BEGSR
032000970526     C*
032100970526     C* CONTROLLA I CAMPI IMMESSI
032200970526     C                   EXSR      DECODI
032300970526     C*
032400170915     C* AGGIORNO
032500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032600170915     C                   eval      ANVDES = D03Des
032700170915     C                   eval      ANVCOD = D03Sito + D03Depo
032800170915     C                   update    TIANV000
032900170915     C                   MOVEL     '1'           FINL00                         *FINE PGM
033000101022     C                   ENDIF
033100970526     C*
033200970526     C                   ENDSR
033300970526     C*------------------------------------------------------------------------*
033400970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
033500970526     C*------------------------------------------------------------------------*
033600970526     C     DECODI        BEGSR
033700000719     C*
033800000719     C                   MOVEL     '0'           ERRL00
033900000131     C*
034000000731     C*
034100970528     C                   ENDSR
034200991210     C*------------------------------------------------------------------------*
034300991210     C* FINSR - OPERAZIONI FINALI
034400991210     C*------------------------------------------------------------------------*
034500991210     C     FINSR         BEGSR
034600991210     C*
034700991210     C* CHIUDE I FILE DEI PGM CHIAMATI
034800991210     C*
034900991210     C                   ENDSR
035000970526     C*------------------------------------------------------------------------*
035100970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
035200970526     C*------------------------------------------------------------------------*
035300970526     C     IREVID        BEGSR
035400970526     C* CAMPI
035500170915     C                   eval      D03Des = ANVDes
035600170915     C                   eval      D03Sito = %subst(ANVCod:1:2)
035700170915     C                   eval      D03Depo = %subst(ANVCod:3:13)
035800970527     C*
035900970526     C                   ENDSR
036000970526     C*------------------------------------------------------------------------*
036100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
036200970526     C*------------------------------------------------------------------------*
036300970526     C     AZZVID        BEGSR
036400970526     C* CAMPI
036500170915     C                   clear                   D03Des
036600170915     C                   clear                   D03Sito
036700170915     C                   clear                   D03Depo
036800970526     C*
036900970526     C                   ENDSR
037000970526     C*------------------------------------------------------------------------*
037100970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
037200970526     C*------------------------------------------------------------------------*
037300970526     C     IMPFUN        BEGSR
037400970526     C*
037500970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
037600970526IF  1C     DEPOP1        IFEQ      'O05'
037700970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
037800970526     C                   MOVEL     'N'           FUDATT
037900970526     C                   MOVEL     FUDRIG        FUD(6)
038000970526E   1C                   ENDIF
038100970526     C*
038200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
038300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
038400970526     C                   EXSR      FUNGES
038500970526     C*
038600970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
038700970526     C*---
038800970526     C* 90 -> PROTEZIONE CHIAVE
038900970526     C* 91 -> PROTEZIONE TOTALE
039000030704     C* 92 -> PROTEZIONE PARZIALE
039100970526     C*---
039200970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
039300970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
039400970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
039500970526     C                   SETOFF                                       91
039600970526E   2C                   ENDIF
039700170915     C* OP1 non lo testo perché chi lo passa potrebbe non sapere come sono messo io come rcd
039800170915IF  2C***  DEPOP1        IFEQ      'O02'                                        *MODIFICA
039900170915     C***                SETON                                        90        *PROTEZIONE CHIAVE
040000170915     C***                SETOFF                                       91        *NO PROTEZIONE CAMPI
040100170915E   2C***                ENDIF
040200970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
040300970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
040400970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
040500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
040600970526     C                   SETON                                        91
040700970526E   2C                   ENDIF
040800970526X   1C                   ELSE                                                   *I N T E R R O G A
040900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
041000970526     C                   SETON                                        91
041100970526E   1C                   ENDIF
041200030704     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
041300030704     C                   SETON                                        92        *SPROTEZIONE SCAD.
041400030704     C                   ENDIF
041500970526     C*
041600970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
041700970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
041800170915     C     S253KSU       CHAIN     TIANV01L                           9998
041900001116IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
042000170915     C                   SELECT
042100170915     C                   when      DEPOP1 ='O05'
042200170915     C* non cambio l'opzione del chiamante, perché anche questo pgm deve aprirsi in visualizzazione
042300170915     C                   other
042400170915     C* determino ADESSO che l'operazione è INSERIMENTO
042500170915     C                   EVAL      DEPOP1 = 'F06'
042600170915     C                   endsl
042700970526X   2C                   ELSE
042800970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
042900170915     C                   SELECT
043000170915     C                   when      DEPOP1 ='O05'
043100170915     C* non cambio l'opzione del chiamante, perché anche questo pgm deve aprirsi in visualizzazione
043200170915     C                   other
043300170915     C* determino ADESSO che l'operazione è MODIFICA
043400170915     C                   EVAL      DEPOP1 = 'O02'
043500170915     C                   endsl
043600980128IF  3C     DEPOP1        IFEQ      'O05'
043700001116     C     DEPOP1        OREQ      'O02'
043800101022     C*
043900101022     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
044000980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
044100980128E   3C                   ENDIF
044200970526E   2C                   ENDIF
044300970526X   1C                   ELSE                                                   *INSERIMENTO
044400970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
044500970526E   1C                   ENDIF
044600970526     C*
044700970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
044800970526IF  1C     DEPOP1        IFEQ      'O02'
044900970526     C                   MOVEL     DEC(1)        VIDFUN
045000970526E   1C                   ENDIF
045100991014IF  1C     DEPOP1        IFEQ      'O03'
045200991014     C                   MOVEL     DEC(6)        VIDFUN
045300991014E   1C                   ENDIF
045400970526IF  1C     DEPOP1        IFEQ      'O04'
045500970526     C                   MOVEL     DEC(2)        VIDFUN
045600970526E   1C                   ENDIF
045700970526IF  1C     DEPOP1        IFEQ      'O05'
045800970526     C                   MOVEL     DEC(3)        VIDFUN
045900970526E   1C                   ENDIF
046000970526IF  1C     DEPOP1        IFEQ      'O07'
046100970526     C                   MOVEL     DEC(4)        VIDFUN
046200970526E   1C                   ENDIF
046300970526IF  1C     DEPOP1        IFEQ      'F06'
046400970526     C                   MOVEL     DEC(5)        VIDFUN
046500970526E   1C                   ENDIF
046600970526     C*
046700970526     C                   ENDSR
046800020212     C*--------------------------------------------------------------------------------------------*
046900020212     C* CARTAB - carica le tabelle occorrenti
047000020212     C*--------------------------------------------------------------------------------------------*
047100020212     C     CARTAB        BEGSR
047200020212     C*
047300020212     C                   ENDSR
047400020924     C*--------------------------------------------------------------------------------------------*
047500020924     C* REPERISCE I DATI UTENTE
047600020924     C*--------------------------------------------------------------------------------------------*
047700020924     C     REPDATIUTE    BEGSR
047800020924     C*
047900020924     C* INIZIALIZZA VARIABILI DI WRK
048000020924     C                   CLEAR                   TIBS34DS
048100020924     C                   CLEAR                   AZUTEDS
048200020924     C                   CLEAR                   DDATIUTE
048300020924     C*
048400020924     C     *DTAARA       DEFINE    §azute        azuteds
048500020924     C     *DTAARA       DEFINE    §datiute      ddatiute
048600020924     C                   IN(E)     *DTAARA
048700020924if  1C                   IF        %Error
048800020924     c                   EVAL      I34Tla = 'L'
048900020924     C                   CALL      'TIBS34R'
049000020924     C                   PARM                    Tibs34Ds
049100020924     C                   IN        *DTAARA
049200020924e   1C                   ENDIF
049300020924     C*
049400020924     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
049500020924     C                   MOVEL(P)  RSUT          DSFIRS           20
049600020924     C*
049700020924     C                   ENDSR
049800101022
049900101022
050000970526     C*------------------------------------------------------------------------*
050100970526     C* *INZSR - OPERAZIONI INIZIALI
050200970526     C*------------------------------------------------------------------------*
050300970526     C     *INZSR        BEGSR
050400970526     C*---
050500970526     C* RICEVIMENTO PARAMETRI
050600970526     C*---
050700970526     C     *ENTRY        PLIST
050800970612     C                   PARM                    KPJBA
050900170915     C                   MOVEL     KPJBU         TIS725DS3
051000020924     C*
051100020924     C* REPERISCE I DATI DELL'UTENTE
051200020924     C                   EXSR      REPDATIUTE
051300970526     C*
051400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
051500970526     C     XMSGPA        PLIST
051600970526     C                   PARM                    DSMSMS
051700970526     C                   PARM                    DSMSPG
051800970526     C                   PARM                    DSMSKE
051900970526     C                   PARM                    DSMSER
052000970526     C*
052100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
052200970526     C                   CALL      'XKEY'
052300970526     C                   PARM                    DSKEY
052400020212     C*
052500020212     C* CARICA LE TABELLE OCCORRENTI
052600020212     C                   EXSR      CARTAB
052700111216     C*
052800111216     C* Reperisco il sistema AS/400 corrente
052900111216     C                   EVAL      AS400_ok = *off
053000111216     C                   callp     UBRTVNETA_Rtv(currSysNeta)
053100111216     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
053200111216     C                   EVAL      AS400_ok = *on
053300111216     C                   ENDIF
053400001116     C*---
053500001116     C* VARIABILI NON RIFERITE AL DATA BASE
053600001116     C*---
053700001116     C     *LIKE         DEFINE    FUDFUN        FUNKEY
053800170915     C     *LIKE         DEFINE    S253OP0       DEPOP0
053900170915     C     *LIKE         DEFINE    S253OP1       DEPOP1
054000170915     C     *LIKE         DEFINE    S253OP0       WOP0
054100170915     C     *LIKE         DEFINE    S253OP1       WOP1
054200001116     C*---
054300001116     C* VARIABILI RIFERITE AL DATA BASE
054400001116     C*---
054500001116     C*---
054600001116     C* VARIABILI DI CONTROLLO
054700001116     C*---
054800001116     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
054900001116     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
055000001116     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
055100001116     C                   MOVEL     '0'           ERRL00            1            *ERRORE
055200010608     C                   MOVEL     '0'           ERRL01            1            *ERRORE (x TIVIR)
055300001116     C*---
055400001116     C* CHIAVI DI LETTURA
055500001116     C*---
055600970526     C*
055700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
055800170915     C                   MOVEL     'TIS7253R'    DSMSPG
055900170915     C                   MOVEL     'TIS7253R'    MSGQUE
056000970526     C                   MOVEL     *BLANKS       MSGKEY
056100970526     C                   MOVEL     *BLANKS       DSMSMS
056200970528     C*
056300970528     C* CALCOLA LA DATA CORRENTE
056400970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
056500970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
056600970528     C                   Z-ADD     WN8           G08DAT
056700051013     C                   Z-ADD     WN8           DATCORINV         8 0
056800970528     C                   Z-ADD     *ZEROS        G08INV
056900970528     C                   MOVEL     '0'           G08ERR
057000970528     C                   CALL      'XSRDA8'
057100970528     C                   PARM                    WLBDA8
057200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
057300970526     C*
057400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
057500170915     C                   MOVEL     S253OP0       DEPOP0                         *LIVELLO PROCEDURA
057600170915     C                   MOVEL     S253OP1       DEPOP1                         *LIVELLO PROGRAMMA
057700051013     C*
057800051013     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
057900051013     C                   IF        DEPOP1 = 'F06' OR
058000051013     C                             DEPOP1 = 'O03' OR
058100051013     C                             DEPOP1 = 'O07'
058200051013     C                   ENDIF
058300101022     C                   IF        DEPOP1 = 'F06' OR
058400101022     C                             DEPOP1 = 'O03'
058500101022     C                   SETOFF                                       81
058600101022     C                   ELSE
058700101022     C                   SETON                                        81
058800101022     C                   ENDIF
058900970526     C*
059000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
059100970526     C                   EXSR      IMPFUN
059200970526     C*
059300000131     C* ESEGUE SUBITO "DECODI" SE ENTRATI IN VISUALIZZAZIONE O MODIFICA
059400170915     C***  DEPOP1        IFEQ      'O05'
059500170915     C***  DEPOP1        OREQ      'O02'
059600170915     C***                EXSR      DECODI
059700170915     C***                ENDIF
059800000131     C*
059900970526     C                   ENDSR
060000970526     C*------------------------------------------------------------------------*
060100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
060200970526     C*------------------------------------------------------------------------*
060300970526     C     FUNGES        BEGSR
060400970526     C*
060500970526     C* PULISCO CAMPI DI LAVORO
060600970526     C                   Z-ADD     0             W                 5 0
060700970526     C*
060800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
060900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
061000970526     C     1             DO        24            I
061100970526     C                   MOVEL     FUD(I)        FUDRIG
061200970526     C*
061300970526     C* PULIZIA FUNZIONI PRECEDENTI
061400970526     C                   MOVEL     *BLANKS       FUV(I)
061500970526     C*
061600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
061700970526     C* (SALTA F24)
061800970526     C     FUDATT        IFEQ      'S'
061900970526     C     FUDFUN        ANDNE     '24'
062000970526     C                   ADD       1             W
062100970526     C                   MOVEL     FUDDES        FUV(W)
062200970526     C                   END
062300970526     C                   END                                                    DO WHILE
062400970526     C*
062500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
062600970526     C* ALTRIMENTI DISATTIVO F24
062700970526     C     1             DO        24            I
062800970526     C                   MOVEL     FUD(I)        FUDRIG
062900970526     C*
063000970526     C     FUDFUN        IFEQ      '24'
063100970526     C     W             IFGT      6
063200970526     C                   MOVEL     'S'           FUDATT
063300970526     C                   ELSE
063400970526     C                   MOVEL     *BLANKS       FUDATT
063500970526     C                   END                                                    W GT 8
063600970526     C                   MOVEL     FUDRIG        FUD(I)
063700970526     C                   END
063800970526     C                   END                                                    DO WHILE
063900970526     C*
064000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
064100970526     C     W             DIV       6             I
064200970526     C                   MVR                     Z
064300970526     C*
064400970526     C     Z             IFNE      0
064500970526     C     I             ADD       1             Z
064600970526     C                   ELSE
064700970526     C                   Z-ADD     I             Z                 5 0
064800970526     C                   END
064900970526     C*
065000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
065100970526     C                   ADD       1             FUNGRU
065200970526     C     FUNGRU        IFGT      Z
065300970526     C     FUNGRU        ORLT      1
065400970526     C                   Z-ADD     1             FUNGRU
065500970526     C                   END
065600970526     C*
065700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
065800970526     C     FUNGRU        IFEQ      1
065900970526     C                   MOVEL     FUV(1)        FUN001
066000970526     C                   MOVEL     FUV(2)        FUN002
066100970526     C                   MOVEL     FUV(3)        FUN003
066200970526     C                   MOVEL     FUV(4)        FUN004
066300970526     C                   MOVEL     FUV(5)        FUN005
066400970526     C                   MOVEL     FUV(6)        FUN006
066500970526     C                   END
066600970526     C*
066700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
066800970526     C     FUNGRU        IFEQ      2
066900970526     C                   MOVEL     FUV(7)        FUN001
067000970526     C                   MOVEL     FUV(8)        FUN002
067100970526     C                   MOVEL     FUV(9)        FUN003
067200970526     C                   MOVEL     FUV(10)       FUN004
067300970526     C                   MOVEL     FUV(11)       FUN005
067400970526     C                   MOVEL     FUV(12)       FUN006
067500970526     C                   END
067600970526     C*
067700970526     C* TERZO GRUPPO DI 6 FUNZIONI
067800970526     C     FUNGRU        IFEQ      3
067900970526     C                   MOVEL     FUV(13)       FUN001
068000970526     C                   MOVEL     FUV(14)       FUN002
068100970526     C                   MOVEL     FUV(15)       FUN003
068200970526     C                   MOVEL     FUV(16)       FUN004
068300970526     C                   MOVEL     FUV(17)       FUN005
068400970526     C                   MOVEL     FUV(18)       FUN006
068500970526     C                   END
068600970526     C*
068700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
068800970526     C     FUNGRU        IFEQ      4
068900970526     C                   MOVEL     FUV(19)       FUN001
069000970526     C                   MOVEL     FUV(20)       FUN002
069100970526     C                   MOVEL     FUV(21)       FUN003
069200970526     C                   MOVEL     FUV(22)       FUN004
069300970526     C                   MOVEL     *BLANKS       FUN005
069400970526     C                   MOVEL     *BLANKS       FUN006
069500970526     C                   END
069600970526     C*
069700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
069800970526     C                   MOVEL     *BLANKS       FUN007
069900970526     C                   MOVEL     *BLANKS       FUN008
070000970526     C*
070100970526     C     1             DO        24            I
070200970526     C                   MOVEL     FUD(I)        FUDRIG
070300970526     C     FUDFUN        IFEQ      '24'
070400970526     C     FUDATT        ANDEQ     'S'
070500970526     C                   MOVEL     FUDDES        FUN008
070600970526     C                   END
070700970526     C                   END                                                    DO
070800970526     C*
070900970526     C                   ENDSR
071000970526     C*------------------------------------------------------------------------*
071100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
071200970526     C*------------------------------------------------------------------------*
071300970526     C     FUNCON        BEGSR
071400970526     C*
071500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
071600970526     C                   EXSR      FUNVAR
071700970526     C*
071800970526     C* PULISCO CAMPI DI LAVORO
071900970526     C                   Z-ADD     0             I                 5 0
072000970526     C                   Z-ADD     0             W                 5 0
072100970526     C*
072200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
072300970526     C     FUNKEY        IFNE      *BLANKS
072400970526     C     1             DO        24            I
072500970526     C                   MOVEL     FUD(I)        FUDRIG
072600970526     C*
072700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
072800970526     C     FUNKEY        IFEQ      FUDFUN
072900970526     C     FUDATT        ANDEQ     'S'
073000970526     C                   Z-ADD     I             W
073100970526     C                   END
073200970526     C                   END                                                    DO WHILE
073300970526     C*
073400970526     C* FUNZIONE NON ATTIVA (ERRORE)
073500970526     C     W             IFEQ      0
073600970526     C*
073700970526     C* EMISSIONE MESSAGGIO DI ERRORE
073800970526     C                   MOVEL     MSG(1)        DSMSMS
073900970526     C*
074000970526     C                   END                                                    W=0
074100970526     C                   END                                                    FUNKEY NE *BLANKS
074200970526     C*
074300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
074400970526     C     W             IFNE      0
074500970526     C     FUNKEY        OREQ      *BLANKS
074600970526     C     FUNKEY        IFEQ      *BLANKS
074700970526     C                   EXSR      ENTGES
074800970526     C                   END
074900970526     C     FUNKEY        IFEQ      '03'
075000970526     C                   EXSR      F03GES
075100970526     C                   END
075200970526     C     FUNKEY        IFEQ      '06'
075300970526     C                   EXSR      F06GES
075400970526     C                   END
075500970526     C     FUNKEY        IFEQ      '12'
075600970526     C                   EXSR      F12GES
075700970526     C                   END
075800970526     C     FUNKEY        IFEQ      '24'
075900970526     C                   EXSR      F24GES
076000970526     C                   END
076100970526     C                   END
076200970526     C*
076300970526     C                   ENDSR
076400970526     C*------------------------------------------------------------------------*
076500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
076600970526     C*------------------------------------------------------------------------*
076700970526     C     FUNVAR        BEGSR
076800970526     C*
076900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
077000970526     C*
077100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
077200970526     C     KEY           IFEQ      ENTER
077300970526     C                   MOVEL     *BLANKS       FUNKEY
077400970526     C                   END
077500970526     C     KEY           IFEQ      F1
077600970526     C                   MOVEL     '01'          FUNKEY
077700970526     C                   END
077800970526     C     KEY           IFEQ      F2
077900970526     C                   MOVEL     '02'          FUNKEY
078000970526     C                   END
078100970526     C     KEY           IFEQ      F3
078200970526     C                   MOVEL     '03'          FUNKEY
078300970526     C                   END
078400970526     C     KEY           IFEQ      F4
078500970526     C                   MOVEL     '04'          FUNKEY
078600970526     C                   END
078700970526     C     KEY           IFEQ      F5
078800970526     C                   MOVEL     '05'          FUNKEY
078900970526     C                   END
079000970526     C     KEY           IFEQ      F6
079100970526     C                   MOVEL     '06'          FUNKEY
079200970526     C                   END
079300970526     C     KEY           IFEQ      F7
079400970526     C                   MOVEL     '07'          FUNKEY
079500970526     C                   END
079600970526     C     KEY           IFEQ      F8
079700970526     C                   MOVEL     '08'          FUNKEY
079800970526     C                   END
079900970526     C     KEY           IFEQ      F9
080000970526     C                   MOVEL     '09'          FUNKEY
080100970526     C                   END
080200970526     C     KEY           IFEQ      F10
080300970526     C                   MOVEL     '10'          FUNKEY
080400970526     C                   END
080500970526     C     KEY           IFEQ      F11
080600970526     C                   MOVEL     '11'          FUNKEY
080700970526     C                   END
080800970526     C     KEY           IFEQ      F12
080900970526     C                   MOVEL     '12'          FUNKEY
081000970526     C                   END
081100970526     C     KEY           IFEQ      F13
081200970526     C                   MOVEL     '13'          FUNKEY
081300970526     C                   END
081400970526     C     KEY           IFEQ      F14
081500970526     C                   MOVEL     '14'          FUNKEY
081600970526     C                   END
081700970526     C     KEY           IFEQ      F15
081800970526     C                   MOVEL     '15'          FUNKEY
081900970526     C                   END
082000970526     C     KEY           IFEQ      F16
082100970526     C                   MOVEL     '16'          FUNKEY
082200970526     C                   END
082300970526     C     KEY           IFEQ      F17
082400970526     C                   MOVEL     '17'          FUNKEY
082500970526     C                   END
082600970526     C     KEY           IFEQ      F18
082700970526     C                   MOVEL     '18'          FUNKEY
082800970526     C                   END
082900970526     C     KEY           IFEQ      F19
083000970526     C                   MOVEL     '19'          FUNKEY
083100970526     C                   END
083200970526     C     KEY           IFEQ      F20
083300970526     C                   MOVEL     '20'          FUNKEY
083400970526     C                   END
083500970526     C     KEY           IFEQ      F21
083600970526     C                   MOVEL     '21'          FUNKEY
083700970526     C                   END
083800970526     C     KEY           IFEQ      F22
083900970526     C                   MOVEL     '22'          FUNKEY
084000970526     C                   END
084100970526     C     KEY           IFEQ      F23
084200970526     C                   MOVEL     '23'          FUNKEY
084300970526     C                   END
084400970526     C     KEY           IFEQ      F24
084500970526     C                   MOVEL     '24'          FUNKEY
084600970526     C                   END
084700970526     C     KEY           IFEQ      ROLLUP
084800970526     C                   MOVEL     'RU'          FUNKEY
084900970526     C                   END
085000970526     C     KEY           IFEQ      HELP
085100970526     C                   MOVEL     'HP'          FUNKEY
085200970526     C                   END
085300970526     C                   ENDSR
085400970526     C*------------------------------------------------------------------------*
085500970526     C* NOTE COSTRUZIONE SCHIERE
085600970526     C*------------------------------------------------------------------------*
085700970526     C*
085800970526     C* -MSG-MESSAGGI
085900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
086000970526     C* (NON TOCCARE MSG,1 E MSG,2)
086100970526     C*
086200970526     C* -CMD-COMANDI
086300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
086400970526     C*
086500970526     C* -OPD-OPZIONI
086600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
086700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
086800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
086900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
087000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
087100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
087200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
087300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
087400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
087500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
087600970526     C*
087700970526     C* -FUD-FUNZIONI
087800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
087900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
088000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
088100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
088200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
088300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
088400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
088500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
088600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
088700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
088800970526     O*------------------------------------------------------------------------*
088900970526** DEC - DECODIFICHE
089000970526    MODIFICA                                                      1
089100970526  ANNULLAMENTO                                                    2
089200970526VISUALIZZAZIONE                                                   3
089300970526   RIPRISTINO                                                     4
089400970526  INSERIMENTO                                                     5
089500991105     COPIA                                                        6
089600000215** DES - DESCRIZIONI
089700000215ATTENZIONE: non esistono figli legati                             1
089800000215ATTENZIONE: esistono figli legati                                 2
089900020211                                                                  3
090000000215                                                                  4
090100000215                                                                  5
090200970526** MSG - MESSAGGI
090300991104TIS725R- Tasto funzionale non gestito                                          1
090400991104TIS725R- Inserimento effettuato                                                2
090500991104TIS725R- Modifica effettuata                                                   3
090600991104TIS725R- Annullamento effettuato                                               4
090700991104TIS725R- Ripristino effettuato                                                 5
090800991104TIS725R- Richiesta funzione per record inesistente                             6
090900991104TIS725R- Record momentaneamente allocato: riprovare più tardi                  7
091000991104TIS725R- Richiesta funzione per record esistente                               8
091100991104TIS725R- Codice tipo servizio non presente in tabella                          9
091200000322TIS725R- Msg: Cliente figlio di cliente già abilitato per lo stesso servizio   10
091300000317TIS725R- Il ciente è già abilitato per lo stesso servizio con altro utente     11
091400091117TIS725R- Tipo legame inesistente o non utilizzabile                            12
091500991104TIS725R- Codice padre errato                                                   13
091600991104TIS725R- Data errata                                                           14
091700991104TIS725R- La data scadenza deve essere maggiore della decorrenza                15
091800991104TIS725R- Il record non può scadere in un giorno già trascorso                  16
091900991104TIS725R- Il record immesso si sovrappone ad uno già in decorrenza              17
092000991104TIS725R- Codice figlio errato                                                  18
092100000705TIS725R- Codice padre obbligatorio                                             19
092200991104TIS725R- Il codice figlio non può essere uguale al codice padre                20
092300000719TIS725R- ATTENZIONE!!! Codice STRATEGI inesistente, F06 per forzatura.         21
092400000807TIS725R- Tipo legame non autorizzato.                                          22
092500000807TIS725R- Filiale non abilitata alla gestione dei clienti scelti.               23
092600000913TIS725R- Servizio DL già presente nel periodo per il cliente scelto.           24
092700010605TIS725R- Nel periodo esistono già abilitazioni per il cliente al file TO.      25
092800010605TIS725R- Abilitazione per il cliente al file TO già esistente.                 26
092900020211TIS725R- Cliente già presente nei legami come figlio.                          27
093000020214TIS725R- Per il servizio indicato non è consentito specificare un legame.      28
093100020802TIS725R- Il codice Strategi è incompatibile con il servizio richiesto.         29
093200090804TIS725R- Campo valido solamente per servizi 'TT'                               30
093300100217TIS725R- E' necessaria una identica abilitazione al servizio 'TT'              31
093400100913TIS725R- Password necessaria x attivazione servizio richiesto.                 32
093500100913TIS725R- Caratteri non validi nella password. Inserire unicamente: A-Z  0-9    33
093600100913TIS725R- Lunghezza minima password: 8 caratteri                                34
093700100914TIS725R- Tasto funzione non consentito per servizi diversi da 'IE'             35
093800101027TIS725R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       36
093900120327TIS725R- Cliente annullato o bloccato, impossibile abilitare EasyWeb           37
094000101027TIS725R-                                                                       38
094100101027TIS725R-                                                                       39
094200101027TIS725R-                                                                       40
094300970526** FUD - FUNZIONI
094400970526                   -        (S/N)=N                        funz.01¬
094500970526                   -        (S/N)=N                        funz.02¬
094600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
094700970526                   -        (S/N)=N                        funz.04¬
094800970526                   -        (S/N)=N                        funz.05¬
094900970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
095000970527                   -        (S/N)=N                        funz.07¬
095100970527                   -        (S/N)=N                        funz.08¬
095200970527                   -        (S/N)=N                        funz.09¬
095300970527                   -        (S/N)=N                        funz.10¬
095400970526                   -        (S/N)=N                        funz.11¬
095500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
095600970526                   -        (S/N)=N                        funz.13¬
095700970526                   -        (S/N)=N                        funz.14¬
095800970526                   -        (S/N)=N                        funz.15¬
095900970526                   -        (S/N)=N                        funz.16¬
096000970526                   -        (S/N)=N                        funz.17¬
096100170915                   -        (S/N)=N                        funz.18¬
096200970526                   -        (S/N)=N                        funz.19¬
096300970526                   -        (S/N)=N                        funz.20¬
096400970526                   -        (S/N)=N                        funz.21¬
096500970526                   -        (S/N)=N                        funz.22¬
096600970526                   -        (S/N)=N                        funz.23¬
096700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
