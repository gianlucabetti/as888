000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600170323     FUBLGOBA02DCF   E             WORKSTN INFDS(DEVDS1)
000700170323     F                                     SFILE(BA02DMS:MSGULT)
000800170301     FWAFOB01I  IF   E           K DISK
000900970526     D*------------------------------------------------------------------------*
001000970526     D* SCHIERE
001100970526     D*------------------------------------------------------------------------*
001200970526     D*------------
001300970526     D* DECODIFICHE
001400970526     D*------------
001500160914     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
001600970526     D*------------
001700970526     D* MESSAGGI
001800970526     D*------------
001900970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* DESCRIZIONI TASTI FUNZIONALI
002200970526     D*------------
002300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* FUNZIONI DA CARICARE A VIDEO
002600970526     D*------------
002700970526     D FUV             S             19    DIM(24)
002800970526     D*------------------------------------------------------------------------*
002900970526     D* INPUT
003000970526     D*------------------------------------------------------------------------*
003100970526     D*------------
003200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003300970526     D*------------
003400970526     D FUDDS           DS
003500970526     D  FUDDE1                 1      1
003600970526     D  FUDFUN                 2      3
003700970526     D  FUDDE2                 4     19
003800970526     D  FUDFI0                20     34
003900970526     D  FUDATT                35     35
004000970526     D  FUDFI1                36     80
004100970526     D  FUDDES                 1     19
004200970526     D  FUDRIG                 1     80
004300970526     D*------------
004400970526     D* DS GESTIONE TASTI FUNZIONALI
004500970526     D*------------
004600970526     D DSKEY         E DS
004700970526     D DEVDS1          DS
004800970526     D  KEY                  369    369
004900970526     D*------------
005000970526     D* DS "XMSG" - GESTIONE MESSAGGI
005100970526     D*------------
005200970526     D DSMSG         E DS
005300970526     D*------------------
005400970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500970526     D*------------------
005600970526     D WLBDA8          DS                  INZ
005700970526     D  G08DAT                 1      8  0
005800970526     D  G08INV                 9     16  0
005900970526     D  G08ERR                17     17
006000970526     D  G08TGI                18     22  0
006100970526     D*------------
006200970526     D* DS DI PROCEDURA
006300970526     D*------------
006400170323     D UBLGOBA02S    E DS
006500970526     D*------------
006600970526     D* ARCHITETTURA
006700970526     D*------------
006800970526     D KPJBA         E DS
006900160714     D*------------
007000160715     D* campi tipo data per le date a video
007100160715     D DDEISO          s               d
007200160715     D DSCISO          s               d
007300970526     C*------------------------------------------------------------------------*
007400970526     C* MAIN LINES
007500970526     C*------------------------------------------------------------------------*
007600970526     C*
007700970526     C* NON CI SONO ERRORI IN ENTRATA
007800970526IF  1C     ERRL00        IFEQ      '0'
007900970526     C*
008000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
008400970526     C*
008500970526     C* EMETTE FORMATO VIDEO
008600970526     C                   EXSR      BUTFMR
008700970526     C*
008800970526     C* GESTIONE TASTI FUNZIONALI
008900970526     C                   EXSR      FUNCON
009000970526E   2C                   ENDDO
009100970526     C*
009200970526     C* CI SONO ERRORI IN ENTRATA
009300970526X   1C                   ELSE
009400170323     C                   MOVEL     '1'           BA02ERR
009500170323     C                   MOVEL     DSMSMS        BA02MSG
009600970526E   1C                   ENDIF
009700970526     C* FINE PGM
009800970526     C                   SETON                                        LR
009900970526     C*------------------------------------------------------------------------*
010000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010100970526     C*------------------------------------------------------------------------*
010200970526     C     BUTFMR        BEGSR
010300970526     C*
010400970526     C* EMETTE LA TESTATA
010500170323     C                   WRITE     BA02D01
010600970526     C*
010700970526     C* SPEDISCE EVENTUALE MESSAGGIO
010800970526     C                   EXSR      SNDMSG
010900970526     C*
011000970526     C* EMETTE LE FUNZIONI
011100170323     C                   WRITE     BA02D02
011200970526     C*
011300970526     C* EMETTE IL VIDEO
011400170323     C                   EXFMT     BA02D03
011500970526     C*
011600970526     C* REIMPOSTA FLAG DI ERRORE
011700970526     C                   MOVEL     '0'           ERRL00
011800970526     C*
011900970526     C* REIMPOSTA INDICATORI DI ERRORE
012000970526     C                   SETOFF                                         5859
012100970526     C                   SETOFF                                       606162
012200970526     C                   SETOFF                                       636465
012300970526     C                   SETOFF                                       666768
012400970526     C                   SETOFF                                       697071
012500970526     C                   SETOFF                                       727374
012600970526     C                   SETOFF                                       757677
012700970526     C                   SETOFF                                       787980
012800970526     C                   SETOFF                                       818283
012900970526     C                   SETOFF                                       848586
013000970526     C                   SETOFF                                       878889
013100970526     C*
013200970526     C                   ENDSR
013300970526     C*------------------------------------------------------------------------*
013400970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013500970526     C*------------------------------------------------------------------------*
013600970526     C     SNDMSG        BEGSR
013700970526     C*
013800970526     C* PULISCE IL SFILE MESSAGGI
013900970526     C                   MOVEL     '1'           *IN40
014000170323     C                   WRITE     BA02DMC
014100970526     C                   MOVEL     '0'           *IN40
014200970526     C*
014300970526     C* VISUALIZZA IL MESSAGGIO
014400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014500970526     C                   CALL      'XMSG'        XMSGPA
014600970526IF  1C     DSMSER        IFEQ      *ZEROS
014700970526     C                   MOVEL     DSMSKE        MSGKEY
014800970526E   1C                   ENDIF
014900970526     C                   Z-ADD     1             MSGULT
015000170323     C                   WRITE     BA02DMS
015100170323     C                   WRITE     BA02DMC
015200970526     C*
015300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015400970526     C                   MOVEL     *BLANKS       DSMSMS
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* ENTGES - GESTIONE ENTER -> INVIO
015900970526     C*------------------------------------------------------------------------*
016000970526     C     ENTGES        BEGSR
016100970526     C*
016200970526     C* CONTROLLA I CAMPI IMMESSI
016300970526     C                   EXSR      DECODI
016400970526     C*
016500970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
016600970526IF  1C     DEPOP1        IFEQ      'O05'
016700970526     C                   MOVEL     '1'           FINL00
016800970526E   1C                   ENDIF
016900970526     C*
017000970526     C                   ENDSR
017100970526     C*------------------------------------------------------------------------*
017200970526     C* F03GES - GESTIONE F03 -> FINE
017300970526     C*------------------------------------------------------------------------*
017400970526     C     F03GES        BEGSR
017500970526     C*
017600970526     C* IMPOSTA "PREMUTO F03"
017700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017800170323     C                   MOVEL     '1'           BA02F03                         *DI PROCEDURA
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F06GES        BEGSR
018500970526     C*
018600970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
018700970526IF  1C     DEPOP1        IFEQ      'F06'
018800970526     C                   EXSR      INSERI
018900970526E   1C                   ENDIF
019000970526IF  1C     DEPOP1        IFEQ      'O02'
019100970526     C                   EXSR      MODIFI
019200970526E   1C                   ENDIF
019300160914IF  1C     DEPOP1        IFEQ      'O03'
019400160914     C                   EXSR      copia
019500160914E   1C                   ENDIF
019600970526IF  1C     DEPOP1        IFEQ      'O04'
019700970526     C                   EXSR      ANNULL
019800970526E   1C                   ENDIF
019900970526IF  1C     DEPOP1        IFEQ      'O07'
020000170301     C***                EXSR      RIPRIS
020100970526E   1C                   ENDIF
020200970526     C*
020300970526     C                   ENDSR
020400160715     C*------------------------------------------------------------------------*
020500160715     C* F12GES - GESTIONE F12 -> RITORNO
020600160715     C*------------------------------------------------------------------------*
020700160715     C     F12GES        BEGSR
020800160715     C*
020900160715     C* IMPOSTA "PREMUTO F12"
021000160715     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021100170323     C                   MOVEL     '1'           BA02F12                         *DI PROCEDURA
021200160715     C*
021300160715     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021600970526     C*------------------------------------------------------------------------*
021700970526     C     F24GES        BEGSR
021800970526     C*
021900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022000970526     C*
022100970526     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
022400970526     C*------------------------------------------------------------------------*
022500970526     C     INSERI        BEGSR
022600970526     C*
022700970526     C* CONTROLLA I CAMPI IMMESSI
022800970526     C                   EXSR      DECODI
022900970526     C*
023000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
023100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023200170301
023300170301     C                   EXSR      IMPFOB                                       *ALTRI CAMPI
023400170301     C***                WRITE     WAFOB000
023500170323     C                   MOVEL     MSG(2)        BA02MSG                         *MESSAGGIO
023600970602     C*
023700970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
023800970602     C                   EXSR      AZZVID
023900970526E   1C                   ENDIF
024000970526     C*
024100970526     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* MODIFI - OPERAZIONI DI MODIFICA
024400970526     C*------------------------------------------------------------------------*
024500970526     C     MODIFI        BEGSR
024600970526     C*
024700970526     C* CONTROLLA I CAMPI IMMESSI
024800970526     C                   EXSR      DECODI
024900970526     C*
025000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
025100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025200170301     C                   EXSR      LETFOB
025300970526     C*
025400970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
025500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025600170301     C                   EXSR      IMPFOB                                       *IMPOSTA I CAMPI
025700170301     C***                UPDATE    WAFOB000
025800170323     C                   MOVEL     MSG(3)        BA02MSG
025900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
026000970526E   2C                   ENDIF
026100970526E   1C                   ENDIF
026200970526     C*
026300970526     C                   ENDSR
026400160914     C*------------------------------------------------------------------------*
026500160914     C* copia  - OPERAZIONI DI copia
026600160914     C*------------------------------------------------------------------------*
026700160914     C     copia         BEGSR
026800160914     C*
026900160914     C* CONTROLLA I CAMPI IMMESSI
027000160914     C                   EXSR      DECODI
027100160914     C*
027200160914     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
027300160914IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027400170301
027500170301     C                   EXSR      IMPFOB                                       *ALTRI CAMPI
027600170301     C***                WRITE     WAFOB000
027700170323     C                   MOVEL     MSG(12)       BA02MSG                         *MESSAGGIO
027800160914     C                   MOVEL     '1'           FINL00                         *FINE PGM
027900160914     C*
028000160914     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
028100160914     C                   EXSR      AZZVID
028200160914E   1C                   ENDIF
028300160914     C*
028400160914     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
028700970526     C*------------------------------------------------------------------------*
028800970526     C     ANNULL        BEGSR
028900970526     C*
029000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
029100170301     C                   EXSR      LETFOB
029200160714     C*
029300160714IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029400970526     C*
029500160714     C* ANNULLA il rcd
029600170301
029700170301     C***                UPDATE    WAFOB000                                     *CANCELLA
029800170323     C                   MOVEL     MSG(4)        BA02MSG                         *MESSAGGIO
029900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
030000970526E   1C                   ENDIF
030100970526     C*
030200970526     C                   ENDSR
030300970526     C*------------------------------------------------------------------------*
030400170301     C* LETFOB - LEGGE IL RECORD PER L'AGGIORNAMENTO
030500970526     C*------------------------------------------------------------------------*
030600170301     C     LETFOB        BEGSR
030700970526     C*
030800170323     C                   EVAL      FOBDtEstr = BA02DtEstr
030900170323     C                   EVAL      FOBLogica = BA02Logica
031000170323     C                   EVAL      FOBObj    = BA02Obj
031100170323     C                   EVAL      FOBLib    = BA02Lib
031200170323     C                   EVAL      FOBObjT   = BA02ObjT
031300170301     C     K05FOB01      CHAIN     WAFOB01I                           9998
031400970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
031500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
031600170323     C                   MOVEL     MSG(7)        BA02MSG                         *MESSAGGIO
031700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031800970526E   1C                   ENDIF
031900970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
032000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
032100170323     C                   MOVEL     MSG(6)        BA02MSG                         *MESSAGGIO
032200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032300970526E   1C                   ENDIF
032400970526     C*
032500970526     C                   ENDSR
032600970528     C*------------------------------------------------------------------------*
032700970528     C* LANPGM - LANCIA UN PROGRAMMA
032800970528     C*------------------------------------------------------------------------*
032900970528     C     LANPGM        BEGSR
033000970528     C*
033100970528     C* IMPOSTA LA DS DI PROCEDURA
033200170323     C                   MOVEL     WOP0          BA02OP0                         *LIVELLO PROCEDURA
033300170323     C                   MOVEL     WOP1          BA02OP1                         *LIVELLO PGM
033400170323     C                   MOVEL     '0'           BA02F03                         *PREMUTO F03=FINE
033500170323     C                   MOVEL     '0'           BA02F12                         *PREMUTO F12=RIT
033600170323     C                   MOVEL     '0'           BA02ERR                         *'1'=ERRORE
033700170323     C                   MOVEL     *BLANKS       BA02MSG                         *MESSAGGIO DI RIT
033800970528     C*
033900160714     C*                  CALL      WPGM
034000160714     C*                  PARM                    KPJBA
034100170323     C*                  PARM                    UBLGOBA02S
034200970528     C*
034300970528     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
034400170323IF  1C     BA02F03       IFEQ      '1'                                          *PREMUTO F03
034500970528     C                   MOVEL     '1'           ERRL00                         *ERRORE
034600970528     C                   MOVEL     '1'           F03L00                         *FINE PGM
034700970528X   1C                   ELSE                                                   *NON PREMUTO F03
034800170323     C                   MOVEL     BA02MSG       DSMSMS                         *MESSAGGIO DI RIT
034900970528E   1C                   ENDIF
035000970528     C*
035100970528     C                   ENDSR
035200970526     C*------------------------------------------------------------------------*
035300970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
035400970526     C*------------------------------------------------------------------------*
035500970526     C     DECODI        BEGSR
035600970526     C*
035700170301     C*
035800970526     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
036100970526     C*------------------------------------------------------------------------*
036200970526     C     IREVID        BEGSR
036300970526     C* CAMPI
036400170301     C                   EVAL      D03DtEstr = %dec(FOBDtEstr:*eur)
036500170301     C                   EVAL      D03Logica = FOBLogica
036600170301     C                   EVAL      D03Obj    = FOBObj
036700170301     C                   EVAL      D03Lib    = FOBLib
036800170301     C                   EVAL      D03ObjT   = FOBObjT
036900170301     C                   EVAL      D03Attr   = FOBAttr
037000170504     C                   EVAL      D03ObjD   = FOBObjD
037100170301     C                   EVAL      D03CUP01  = %subst(FOBDati:1:10)
037200170301     C                   EVAL      D03DUC    = %subst(FOBDati:11:4)
037300170301     C                   EVAL      D03CDT06  = %dec(%date(%dec(
037400170301     C                                          %subst(FOBDati:15:8)
037500170301     C                                         :8:0):*iso):*eur)
037600170301     C                   EVAL      D03LUDT   = %dec(%date(%dec(
037700170301     C                                          %subst(FOBDati:23:8)
037800170301     C                                         :8:0):*iso):*eur)
037900170301     C                   EVAL      D03Num    = %subst(FOBDati:31:3)
038000170301        select;
038100170301         when %subst(FOBDati:34:2) = '*Y';
038200170301          D03Cosa = 'anni';
038300170301         when %subst(FOBDati:34:2) = '*M';
038400170301          D03Cosa = 'mesi';
038500170323         when %subst(FOBDati:34:2) = '*D';
038600170301          D03Cosa = 'giorni';
038700170301        endsl;
038800970526     C*
038900970526     C                   ENDSR
039000970526     C*------------------------------------------------------------------------*
039100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
039200970526     C*------------------------------------------------------------------------*
039300970526     C     AZZVID        BEGSR
039400970526     C*
039500170301     C                   CLEAR                   D03DtEstr
039600170301     C                   CLEAR                   D03Logica
039700170301     C                   CLEAR                   D03Obj
039800170301     C                   CLEAR                   D03Lib
039900170301     C                   CLEAR                   D03ObjT
040000170301     C                   CLEAR                   D03Attr
040100170504     C                   CLEAR                   D03ObjD
040200170301     C                   CLEAR                   D03CUP01
040300170301     C                   CLEAR                   D03DUC
040400170301     C                   CLEAR                   D03CDT06
040500170301     C                   CLEAR                   D03LUDT
040600170301     C                   CLEAR                   D03Num
040700170301     C                   CLEAR                   D03Cosa
040800970526     C*
040900970526     C                   ENDSR
041000970526     C*------------------------------------------------------------------------*
041100170301     C* IMPFOB - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
041200970526     C*------------------------------------------------------------------------*
041300170301     C     IMPFOB        BEGSR
041400970526     C*
041500970526     C*
041600970526     C                   ENDSR
041700970526     C*------------------------------------------------------------------------*
041800970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
041900970526     C*------------------------------------------------------------------------*
042000970526     C     IMPFUN        BEGSR
042100970526     C*
042200970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
042300970526IF  1C     DEPOP1        IFEQ      'O05'
042400970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
042500970526     C                   MOVEL     'N'           FUDATT
042600970526     C                   MOVEL     FUDRIG        FUD(6)
042700970526E   1C                   ENDIF
042800970526     C*
042900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
043000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
043100970526     C                   EXSR      FUNGES
043200970526     C*
043300970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
043400970526     C*---
043500970526     C* 90 -> PROTEZIONE CHIAVE
043600970526     C* 91 -> PROTEZIONE TOTALE
043700970526     C*---
043800970526IF  1C     DEPOP0        IFEQ      'R02'                                        G E S T I O N E
043900970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
044000160914IF  2C     DEPOP1        OREQ      'O03'                                        *COPIA
044100970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
044200970526     C                   SETOFF                                       91
044300970526E   2C                   ENDIF
044400970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
044500970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
044600970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
044700970526E   2C                   ENDIF
044800970526     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
044900970526IF  2C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
045000970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
045100970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
045200970526     C                   SETON                                        91
045300970526E   2C                   ENDIF
045400970526X   1C                   ELSE                                                   *I N T E R R O G A
045500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
045600970526     C                   SETON                                        91
045700970526E   1C                   ENDIF
045800970526     C*
045900970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
046000160914     C* LETTURA DEL RECORD DA COPIARE -NO IN INSERIMENTO-
046100970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
046200170323     C                   EVAL      FOBDtEstr = BA02DtEstr
046300170323     C                   EVAL      FOBLogica = BA02Logica
046400170323     C                   EVAL      FOBObj    = BA02Obj
046500170323     C                   EVAL      FOBLib    = BA02Lib
046600170323     C                   EVAL      FOBObjT   = BA02ObjT
046700170301     C***  K05FOB01      CHAIN(N)  WAFOB01I                           99
046800170301     C     K05FOB01      CHAIN     WAFOB01I                           99
046900970526IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
047000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
047100970526     C                   MOVEL     MSG(6)        DSMSMS
047200970526X   2C                   ELSE
047300970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
047400980128IF  3C     DEPOP1        IFEQ      'O05'
047500980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
047600980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
047700980128E   3C                   ENDIF
047800970526E   2C                   ENDIF
047900970526X   1C                   ELSE                                                   *INSERIMENTO
048000970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
048100970526E   1C                   ENDIF
048200970526     C*
048300970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
048400970526IF  1C     DEPOP1        IFEQ      'O02'
048500970526     C                   MOVEL     DEC(1)        VIDFUN
048600970526E   1C                   ENDIF
048700160914IF  1C     DEPOP1        IFEQ      'O03'
048800160914     C                   MOVEL     DEC(6)        VIDFUN
048900160914E   1C                   ENDIF
049000970526IF  1C     DEPOP1        IFEQ      'O04'
049100970526     C                   MOVEL     DEC(2)        VIDFUN
049200970526E   1C                   ENDIF
049300970526IF  1C     DEPOP1        IFEQ      'O05'
049400970526     C                   MOVEL     DEC(3)        VIDFUN
049500970526E   1C                   ENDIF
049600970526IF  1C     DEPOP1        IFEQ      'O07'
049700970526     C                   MOVEL     DEC(4)        VIDFUN
049800970526E   1C                   ENDIF
049900970526IF  1C     DEPOP1        IFEQ      'F06'
050000970526     C                   MOVEL     DEC(5)        VIDFUN
050100970526E   1C                   ENDIF
050200970526     C*
050300970526     C                   ENDSR
050400970526     C*------------------------------------------------------------------------*
050500970526     C* *INZSR - OPERAZIONI INIZIALI
050600970526     C*------------------------------------------------------------------------*
050700970526     C     *INZSR        BEGSR
050800970526     C*---
050900970526     C* RICEVIMENTO PARAMETRI
051000970526     C*---
051100970526     C     *ENTRY        PLIST
051200970612     C                   PARM                    KPJBA
051300170323     C                   PARM                    UBLGOBA02S
051400970526     C*
051500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
051600970526     C     XMSGPA        PLIST
051700970526     C                   PARM                    DSMSMS
051800970526     C                   PARM                    DSMSPG
051900970526     C                   PARM                    DSMSKE
052000970526     C                   PARM                    DSMSER
052100970526     C*
052200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
052300970526     C                   CALL      'XKEY'
052400970526     C                   PARM                    DSKEY
052500970526     C*---
052600970526     C* VARIABILI NON RIFERITE AL DATA BASE
052700970526     C*---
052800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
052900170323     C     *LIKE         DEFINE    BA02OP0       DEPOP0                         *DI DEPOSITO
053000170323     C     *LIKE         DEFINE    BA02OP1       DEPOP1
053100170323     C     *LIKE         DEFINE    BA02OP0       WOP0                           *DI LAVORO
053200170323     C     *LIKE         DEFINE    BA02OP1       WOP1
053300970526     C*---
053400970526     C* VARIABILI DI CONTROLLO
053500970526     C*---
053600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
053700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
053800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
053900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
054000970526     C*
054100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
054200170323     C                   MOVEL     'UBLGOBA02R'  DSMSPG
054300170323     C                   MOVEL     'UBLGOBA02R'  MSGQUE
054400970526     C                   MOVEL     *BLANKS       MSGKEY
054500970526     C                   MOVEL     *BLANKS       DSMSMS
054600970526     C*---
054700970526     C* VARIABILI RIFERITE AL DATA BASE
054800970526     C*---
054900970526     C* CHIAVI DI LETTURA
055000970526     C*---
055100170301     C* LETTURA WAFOB01I - COMPLETA
055200170301     C     K05FOB01      KLIST
055300170301     C                   KFLD                    FOBDtEstr
055400170301     C                   KFLD                    FOBLogica
055500170301     C                   KFLD                    FOBObj
055600170301     C                   KFLD                    FOBLib
055700170301     C                   KFLD                    FOBObjT
055800970526     C*
055900970526     C* CALCOLA LA DATA CORRENTE
056000970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
056100970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
056200970526     C                   Z-ADD     WN8           G08DAT
056300970526     C                   Z-ADD     *ZEROS        G08INV
056400970526     C                   MOVEL     '0'           G08ERR
056500970526     C                   CALL      'XSRDA8'
056600970526     C                   PARM                    WLBDA8
056700970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
056800970526     C*
056900970526     C* DEPOSITA LE OPZIONI DI ENTRATA
057000170323     C                   MOVEL     BA02OP0       DEPOP0                         *LIVELLO PROCEDURA
057100170323     C                   MOVEL     BA02OP1       DEPOP1                         *LIVELLO PROGRAMMA
057200970526     C*
057300970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
057400970526     C                   EXSR      IMPFUN
057500970526     C*
057600970526     C                   ENDSR
057700970526     C*------------------------------------------------------------------------*
057800970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
057900970526     C*------------------------------------------------------------------------*
058000970526     C     FUNGES        BEGSR
058100970526     C*
058200970526     C* PULISCO CAMPI DI LAVORO
058300970526     C                   Z-ADD     0             W                 5 0
058400970526     C*
058500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
058600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
058700970526     C     1             DO        24            I
058800970526     C                   MOVEL     FUD(I)        FUDRIG
058900970526     C*
059000970526     C* PULIZIA FUNZIONI PRECEDENTI
059100970526     C                   MOVEL     *BLANKS       FUV(I)
059200970526     C*
059300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
059400970526     C* (SALTA F24)
059500970526     C     FUDATT        IFEQ      'S'
059600970526     C     FUDFUN        ANDNE     '24'
059700970526     C                   ADD       1             W
059800970526     C                   MOVEL     FUDDES        FUV(W)
059900970526     C                   END
060000970526     C                   END                                                    DO WHILE
060100970526     C*
060200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
060300970526     C* ALTRIMENTI DISATTIVO F24
060400970526     C     1             DO        24            I
060500970526     C                   MOVEL     FUD(I)        FUDRIG
060600970526     C*
060700970526     C     FUDFUN        IFEQ      '24'
060800970526     C     W             IFGT      6
060900970526     C                   MOVEL     'S'           FUDATT
061000970526     C                   ELSE
061100970526     C                   MOVEL     *BLANKS       FUDATT
061200970526     C                   END                                                    W GT 8
061300970526     C                   MOVEL     FUDRIG        FUD(I)
061400970526     C                   END
061500970526     C                   END                                                    DO WHILE
061600970526     C*
061700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
061800970526     C     W             DIV       6             I
061900970526     C                   MVR                     Z
062000970526     C*
062100970526     C     Z             IFNE      0
062200970526     C     I             ADD       1             Z
062300970526     C                   ELSE
062400970526     C                   Z-ADD     I             Z                 5 0
062500970526     C                   END
062600970526     C*
062700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
062800970526     C                   ADD       1             FUNGRU
062900970526     C     FUNGRU        IFGT      Z
063000970526     C     FUNGRU        ORLT      1
063100970526     C                   Z-ADD     1             FUNGRU
063200970526     C                   END
063300970526     C*
063400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
063500970526     C     FUNGRU        IFEQ      1
063600970526     C                   MOVEL     FUV(1)        FUN001
063700970526     C                   MOVEL     FUV(2)        FUN002
063800970526     C                   MOVEL     FUV(3)        FUN003
063900970526     C                   MOVEL     FUV(4)        FUN004
064000970526     C                   MOVEL     FUV(5)        FUN005
064100970526     C                   MOVEL     FUV(6)        FUN006
064200970526     C                   END
064300970526     C*
064400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
064500970526     C     FUNGRU        IFEQ      2
064600970526     C                   MOVEL     FUV(7)        FUN001
064700970526     C                   MOVEL     FUV(8)        FUN002
064800970526     C                   MOVEL     FUV(9)        FUN003
064900970526     C                   MOVEL     FUV(10)       FUN004
065000970526     C                   MOVEL     FUV(11)       FUN005
065100970526     C                   MOVEL     FUV(12)       FUN006
065200970526     C                   END
065300970526     C*
065400970526     C* TERZO GRUPPO DI 6 FUNZIONI
065500970526     C     FUNGRU        IFEQ      3
065600970526     C                   MOVEL     FUV(13)       FUN001
065700970526     C                   MOVEL     FUV(14)       FUN002
065800970526     C                   MOVEL     FUV(15)       FUN003
065900970526     C                   MOVEL     FUV(16)       FUN004
066000970526     C                   MOVEL     FUV(17)       FUN005
066100970526     C                   MOVEL     FUV(18)       FUN006
066200970526     C                   END
066300970526     C*
066400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
066500970526     C     FUNGRU        IFEQ      4
066600970526     C                   MOVEL     FUV(19)       FUN001
066700970526     C                   MOVEL     FUV(20)       FUN002
066800970526     C                   MOVEL     FUV(21)       FUN003
066900970526     C                   MOVEL     FUV(22)       FUN004
067000970526     C                   MOVEL     *BLANKS       FUN005
067100970526     C                   MOVEL     *BLANKS       FUN006
067200970526     C                   END
067300970526     C*
067400970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
067500970526     C                   MOVEL     *BLANKS       FUN007
067600970526     C                   MOVEL     *BLANKS       FUN008
067700970526     C*
067800970526     C     1             DO        24            I
067900970526     C                   MOVEL     FUD(I)        FUDRIG
068000970526     C     FUDFUN        IFEQ      '24'
068100970526     C     FUDATT        ANDEQ     'S'
068200970526     C                   MOVEL     FUDDES        FUN008
068300970526     C                   END
068400970526     C                   END                                                    DO
068500970526     C*
068600970526     C                   ENDSR
068700970526     C*------------------------------------------------------------------------*
068800970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
068900970526     C*------------------------------------------------------------------------*
069000970526     C     FUNCON        BEGSR
069100970526     C*
069200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
069300970526     C                   EXSR      FUNVAR
069400970526     C*
069500970526     C* PULISCO CAMPI DI LAVORO
069600970526     C                   Z-ADD     0             I                 5 0
069700970526     C                   Z-ADD     0             W                 5 0
069800970526     C*
069900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
070000970526     C     FUNKEY        IFNE      *BLANKS
070100970526     C     1             DO        24            I
070200970526     C                   MOVEL     FUD(I)        FUDRIG
070300970526     C*
070400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
070500970526     C     FUNKEY        IFEQ      FUDFUN
070600970526     C     FUDATT        ANDEQ     'S'
070700970526     C                   Z-ADD     I             W
070800970526     C                   END
070900970526     C                   END                                                    DO WHILE
071000970526     C*
071100970526     C* FUNZIONE NON ATTIVA (ERRORE)
071200970526     C     W             IFEQ      0
071300970526     C*
071400970526     C* EMISSIONE MESSAGGIO DI ERRORE
071500970526     C                   MOVEL     MSG(1)        DSMSMS
071600970526     C*
071700970526     C                   END                                                    W=0
071800970526     C                   END                                                    FUNKEY NE *BLANKS
071900970526     C*
072000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
072100970526     C     W             IFNE      0
072200970526     C     FUNKEY        OREQ      *BLANKS
072300970526     C     FUNKEY        IFEQ      *BLANKS
072400970526     C                   EXSR      ENTGES
072500970526     C                   END
072600970526     C     FUNKEY        IFEQ      '03'
072700970526     C                   EXSR      F03GES
072800970526     C                   END
072900970526     C     FUNKEY        IFEQ      '06'
073000970526     C                   EXSR      F06GES
073100970526     C                   END
073200160715     C     FUNKEY        IFEQ      '12'
073300160715     C                   EXSR      F12GES
073400160715     C                   END
073500970526     C     FUNKEY        IFEQ      '24'
073600970526     C                   EXSR      F24GES
073700970526     C                   END
073800970526     C                   END
073900970526     C*
074000970526     C                   ENDSR
074100970526     C*------------------------------------------------------------------------*
074200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
074300970526     C*------------------------------------------------------------------------*
074400970526     C     FUNVAR        BEGSR
074500970526     C*
074600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
074700970526     C*
074800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
074900970526     C     KEY           IFEQ      ENTER
075000970526     C                   MOVEL     *BLANKS       FUNKEY
075100970526     C                   END
075200970526     C     KEY           IFEQ      F1
075300970526     C                   MOVEL     '01'          FUNKEY
075400970526     C                   END
075500970526     C     KEY           IFEQ      F2
075600970526     C                   MOVEL     '02'          FUNKEY
075700970526     C                   END
075800970526     C     KEY           IFEQ      F3
075900970526     C                   MOVEL     '03'          FUNKEY
076000970526     C                   END
076100970526     C     KEY           IFEQ      F4
076200970526     C                   MOVEL     '04'          FUNKEY
076300970526     C                   END
076400970526     C     KEY           IFEQ      F5
076500970526     C                   MOVEL     '05'          FUNKEY
076600970526     C                   END
076700970526     C     KEY           IFEQ      F6
076800970526     C                   MOVEL     '06'          FUNKEY
076900970526     C                   END
077000970526     C     KEY           IFEQ      F7
077100970526     C                   MOVEL     '07'          FUNKEY
077200970526     C                   END
077300970526     C     KEY           IFEQ      F8
077400970526     C                   MOVEL     '08'          FUNKEY
077500970526     C                   END
077600970526     C     KEY           IFEQ      F9
077700970526     C                   MOVEL     '09'          FUNKEY
077800970526     C                   END
077900970526     C     KEY           IFEQ      F10
078000970526     C                   MOVEL     '10'          FUNKEY
078100970526     C                   END
078200970526     C     KEY           IFEQ      F11
078300970526     C                   MOVEL     '11'          FUNKEY
078400970526     C                   END
078500970526     C     KEY           IFEQ      F12
078600970526     C                   MOVEL     '12'          FUNKEY
078700970526     C                   END
078800970526     C     KEY           IFEQ      F13
078900970526     C                   MOVEL     '13'          FUNKEY
079000970526     C                   END
079100970526     C     KEY           IFEQ      F14
079200970526     C                   MOVEL     '14'          FUNKEY
079300970526     C                   END
079400970526     C     KEY           IFEQ      F15
079500970526     C                   MOVEL     '15'          FUNKEY
079600970526     C                   END
079700970526     C     KEY           IFEQ      F16
079800970526     C                   MOVEL     '16'          FUNKEY
079900970526     C                   END
080000970526     C     KEY           IFEQ      F17
080100970526     C                   MOVEL     '17'          FUNKEY
080200970526     C                   END
080300970526     C     KEY           IFEQ      F18
080400970526     C                   MOVEL     '18'          FUNKEY
080500970526     C                   END
080600970526     C     KEY           IFEQ      F19
080700970526     C                   MOVEL     '19'          FUNKEY
080800970526     C                   END
080900970526     C     KEY           IFEQ      F20
081000970526     C                   MOVEL     '20'          FUNKEY
081100970526     C                   END
081200970526     C     KEY           IFEQ      F21
081300970526     C                   MOVEL     '21'          FUNKEY
081400970526     C                   END
081500970526     C     KEY           IFEQ      F23
081600970526     C                   MOVEL     '23'          FUNKEY
081700970526     C                   END
081800970526     C     KEY           IFEQ      F24
081900970526     C                   MOVEL     '24'          FUNKEY
082000970526     C                   END
082100970526     C     KEY           IFEQ      ROLLUP
082200970526     C                   MOVEL     'RU'          FUNKEY
082300970526     C                   END
082400970526     C     KEY           IFEQ      HELP
082500970526     C                   MOVEL     'HP'          FUNKEY
082600970526     C                   END
082700970526     C                   ENDSR
082800970526     C*------------------------------------------------------------------------*
082900970526     C* NOTE COSTRUZIONE SCHIERE
083000970526     C*------------------------------------------------------------------------*
083100970526     C*
083200970526     C* -MSG-MESSAGGI
083300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
083400970526     C* (NON TOCCARE MSG,1 E MSG,2)
083500970526     C*
083600970526     C* -CMD-COMANDI
083700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
083800970526     C*
083900970526     C* -OPD-OPZIONI
084000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
084100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
084200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
084300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
084400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
084500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
084600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
084700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
084800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
084900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
085000970526     C*
085100970526     C* -FUD-FUNZIONI
085200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
085300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
085400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
085500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
085600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
085700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
085800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
085900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
086000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
086100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
086200970526     O*------------------------------------------------------------------------*
086300970526** DEC - DECODIFICHE
086400970526    MODIFICA                                                      1
086500970526  ANNULLAMENTO                                                    2
086600970526VISUALIZZAZIONE                                                   3
086700970526   RIPRISTINO                                                     4
086800970526  INSERIMENTO                                                     5
086900160914     COPIA                                                        6
087000970526** MSG - MESSAGGI
087100170323UBLGOBA02R- Tasto funzionale non gestito                                       1
087200170323UBLGOBA02R- Inserimento effettuato                                             2
087300170323UBLGOBA02R- Modifica effettuata                                                3
087400170323UBLGOBA02R- Annullamento effettuato                                            4
087500170323UBLGOBA02R-                                                                    5
087600170323UBLGOBA02R-                                                                    6
087700170323UBLGOBA02R-                                                                    7
087800170323UBLGOBA02R-                                                                    8
087900170323UBLGOBA02R- Data errata                                                        9
088000170323UBLGOBA02R-                                                                   10
088100170323UBLGOBA02R-                                                                   11
088200170323UBLGOBA02R-                                                                   12
088300170323UBLGOBA02R-                                                                   13
088400170323UBLGOBA02R- Campo obbligatorio                                                14
088500170323UBLGOBA02R-                                                                   15
088600170323UBLGOBA02R-                                                                   16
088700170323UBLGOBA02R- 17
088800170323UBLGOBA02R- 18
088900170323UBLGOBA02R- 19
089000170323UBLGOBA02R- 20
089100970526** FUD - FUNZIONI
089200970526                   -        (S/N)=N                        funz.01¬
089300970526                   -        (S/N)=N                        funz.02¬
089400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
089500970526                   -        (S/N)=N                        funz.04¬
089600970526                   -        (S/N)=N                        funz.05¬
089700970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
089800970526                   -        (S/N)=N                        funz.07¬
089900160715                   -        (S/N)=N                        funz.08¬
090000160715                   -        (S/N)=N                        funz.09¬
090100160715                   -        (S/N)=N                        funz.10¬
090200160715                   -        (S/N)=N                        funz.11¬
090300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
090400160715                   -        (S/N)=N                        funz.13¬
090500970526                   -        (S/N)=N                        funz.14¬
090600970526                   -        (S/N)=N                        funz.15¬
090700970526                   -        (S/N)=N                        funz.16¬
090800970526                   -        (S/N)=N                        funz.17¬
090900970526                   -        (S/N)=N                        funz.18¬
091000970526                   -        (S/N)=N                        funz.19¬
091100970526                   -        (S/N)=N                        funz.20¬
091200970526                   -        (S/N)=N                        funz.21¬
091300970526                   -        (S/N)=N                        funz.22¬
091400970526                   -        (S/N)=N                        funz.23¬
091500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
