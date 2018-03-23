000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600161128     FTIS7VCP2D CF   E             WORKSTN INFDS(DEVDS1)
000700161128     F                                     SFILE(S7P2DMS:MSGULT)
000800161128     FTIVCP01I  UF A E           K DISK
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
006400161128     D TIS7VCP2DS    E DS
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
009400161128     C                   MOVEL     '1'           VCP2ERR
009500161128     C                   MOVEL     DSMSMS        VCP2MSG
009600970526E   1C                   ENDIF
009700970526     C* FINE PGM
009800970526     C                   SETON                                        LR
009900970526     C*------------------------------------------------------------------------*
010000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010100970526     C*------------------------------------------------------------------------*
010200970526     C     BUTFMR        BEGSR
010300970526     C*
010400970526     C* EMETTE LA TESTATA
010500161128     C                   WRITE     S7P2D01
010600970526     C*
010700970526     C* SPEDISCE EVENTUALE MESSAGGIO
010800970526     C                   EXSR      SNDMSG
010900970526     C*
011000970526     C* EMETTE LE FUNZIONI
011100161128     C                   WRITE     S7P2D02
011200970526     C*
011300970526     C* EMETTE IL VIDEO
011400161128     C                   EXFMT     S7P2D03
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
014000161128     C                   WRITE     S7P2DMC
014100970526     C                   MOVEL     '0'           *IN40
014200970526     C*
014300970526     C* VISUALIZZA IL MESSAGGIO
014400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014500970526     C                   CALL      'XMSG'        XMSGPA
014600970526IF  1C     DSMSER        IFEQ      *ZEROS
014700970526     C                   MOVEL     DSMSKE        MSGKEY
014800970526E   1C                   ENDIF
014900970526     C                   Z-ADD     1             MSGULT
015000161128     C                   WRITE     S7P2DMS
015100161128     C                   WRITE     S7P2DMC
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
017800161128     C                   MOVEL     '1'           VCP2F03                         *DI PROCEDURA
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
020000970526     C                   EXSR      RIPRIS
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
021100161128     C                   MOVEL     '1'           VCP2F12                         *DI PROCEDURA
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
023200161128     C                   EVAL      VCPAnn    = *blank
023300161128     C                   EVAL      VCPID     = D03ID
023400161128     C                   EVAL      VCPAnno   = D03Anno
023500161128     C                   EVAL      VCPPrg    = D03Prg
023600161128     C                   EVAL      VCPMax    = D03Max
023700161128     C                   EVAL      VCPIncr   = D03Incr
023800161128     C                   EXSR      IMPVCP                                       *ALTRI CAMPI
023900161128     C                   WRITE     TIVCP000
024000161128     C                   MOVEL     MSG(2)        VCP2MSG                         *MESSAGGIO
024100970602     C*
024200970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
024300970602     C                   EXSR      AZZVID
024400970526E   1C                   ENDIF
024500970526     C*
024600970526     C                   ENDSR
024700970526     C*------------------------------------------------------------------------*
024800970526     C* MODIFI - OPERAZIONI DI MODIFICA
024900970526     C*------------------------------------------------------------------------*
025000970526     C     MODIFI        BEGSR
025100970526     C*
025200970526     C* CONTROLLA I CAMPI IMMESSI
025300970526     C                   EXSR      DECODI
025400970526     C*
025500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
025600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025700161128     C                   EXSR      LETVCP
025800970526     C*
025900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026100161129     C                   EVAL      VCPID     = D03ID
026200161129     C                   EVAL      VCPAnno   = D03Anno
026300161129     C                   EVAL      VCPPrg    = D03Prg
026400161129     C                   EVAL      VCPMax    = D03Max
026500161129     C                   EVAL      VCPIncr   = D03Incr
026600161128     C                   EXSR      IMPVCP                                       *IMPOSTA I CAMPI
026700161128     C                   UPDATE    TIVCP000
026800161128     C                   MOVEL     MSG(3)        VCP2MSG
026900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
027000970526E   2C                   ENDIF
027100970526E   1C                   ENDIF
027200970526     C*
027300970526     C                   ENDSR
027400160914     C*------------------------------------------------------------------------*
027500160914     C* copia  - OPERAZIONI DI copia
027600160914     C*------------------------------------------------------------------------*
027700160914     C     copia         BEGSR
027800160914     C*
027900160914     C* CONTROLLA I CAMPI IMMESSI
028000160914     C                   EXSR      DECODI
028100160914     C*
028200160914     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028300160914IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028400161128     C                   EVAL      VCPAnn    = *blank
028500161128     C                   EVAL      VCPID     = D03ID
028600161128     C                   EVAL      VCPAnno   = D03Anno
028700161128     C                   EVAL      VCPPrg    = D03Prg
028800161128     C                   EVAL      VCPMax    = D03Max
028900161128     C                   EVAL      VCPIncr   = D03Incr
029000161128     C                   EXSR      IMPVCP                                       *ALTRI CAMPI
029100161128     C                   WRITE     TIVCP000
029200161128     C                   MOVEL     MSG(12)       VCP2MSG                         *MESSAGGIO
029300160914     C                   MOVEL     '1'           FINL00                         *FINE PGM
029400160914     C*
029500160914     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
029600160914     C                   EXSR      AZZVID
029700160914E   1C                   ENDIF
029800160914     C*
029900160914     C                   ENDSR
030000970526     C*------------------------------------------------------------------------*
030100970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
030200970526     C*------------------------------------------------------------------------*
030300970526     C     ANNULL        BEGSR
030400970526     C*
030500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030600161128     C                   EXSR      LETVCP
030700160714     C*
030800160714IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030900970526     C*
031000160714     C* ANNULLA il rcd
031100160718     C* L'annullamento  aggiorna il flag annullamento senza altri ragionamenti
031200161128     C                   EVAL      VCPAnn = 'A'
031300161128     C                   UPDATE    TIVCP000                                     *CANCELLA
031400161128     C                   MOVEL     MSG(4)        VCP2MSG                         *MESSAGGIO
031500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031600970526E   1C                   ENDIF
031700970526     C*
031800970526     C                   ENDSR
031900970526     C*------------------------------------------------------------------------*
032000970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
032100970526     C*------------------------------------------------------------------------*
032200970526     C     RIPRIS        BEGSR
032300970526     C*
032400970526     C* CONTROLLA I CAMPI IMMESSI
032500970526     C                   EXSR      DECODI
032600970526     C*
032700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032900161128     C                   EXSR      LETVCP
033000970526     C*
033100970526     C* IMPOSTA IL RECORD DA SCRIVERE
033200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033300161128     C                   MOVEL     *BLANKS       VCPAnn                         *FLAG ANNULLAMENTO
033400161128     C                   UPDATE    TIVCP000                                     *ANNULLA
033500970526E   2C                   ENDIF
033600161128     C                   MOVEL     MSG(5)        VCP2MSG                         *MESSAGGIO
033700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033800970526E   1C                   ENDIF
033900970526     C*
034000970526     C                   ENDSR
034100970526     C*------------------------------------------------------------------------*
034200161128     C* LETVCP - LEGGE IL RECORD PERL'AGGIORNAMENTO
034300970526     C*------------------------------------------------------------------------*
034400161128     C     LETVCP        BEGSR
034500970526     C*
034600161128     C     D03ID         CHAIN     TIVCP01I                           9998
034700970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
034800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
034900161128     C                   MOVEL     MSG(7)        VCP2MSG                         *MESSAGGIO
035000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035100970526E   1C                   ENDIF
035200970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
035300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035400161128     C                   MOVEL     MSG(6)        VCP2MSG                         *MESSAGGIO
035500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035600970526E   1C                   ENDIF
035700970526     C*
035800970526     C                   ENDSR
035900970528     C*------------------------------------------------------------------------*
036000970528     C* LANPGM - LANCIA UN PROGRAMMA
036100970528     C*------------------------------------------------------------------------*
036200970528     C     LANPGM        BEGSR
036300970528     C*
036400970528     C* IMPOSTA LA DS DI PROCEDURA
036500161128     C                   MOVEL     WOP0          VCP2OP0                         *LIVELLO PROCEDURA
036600161128     C                   MOVEL     WOP1          VCP2OP1                         *LIVELLO PGM
036700161128     C                   MOVEL     '0'           VCP2F03                         *PREMUTO F03=FINE
036800161128     C                   MOVEL     '0'           VCP2F12                         *PREMUTO F12=RIT
036900161128     C                   MOVEL     '0'           VCP2ERR                         *'1'=ERRORE
037000161128     C                   MOVEL     *BLANKS       VCP2MSG                         *MESSAGGIO DI RIT
037100970528     C*
037200160714     C*                  CALL      WPGM
037300160714     C*                  PARM                    KPJBA
037400161128     C*                  PARM                    TIS7VCP2DS
037500970528     C*
037600970528     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
037700161128IF  1C     VCP2F03       IFEQ      '1'                                          *PREMUTO F03
037800970528     C                   MOVEL     '1'           ERRL00                         *ERRORE
037900970528     C                   MOVEL     '1'           F03L00                         *FINE PGM
038000970528X   1C                   ELSE                                                   *NON PREMUTO F03
038100161128     C                   MOVEL     VCP2MSG       DSMSMS                         *MESSAGGIO DI RIT
038200970528E   1C                   ENDIF
038300970528     C*
038400970528     C                   ENDSR
038500970526     C*------------------------------------------------------------------------*
038600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
038700970526     C*------------------------------------------------------------------------*
038800970526     C     DECODI        BEGSR
038900970526     C*---
039000161129     C* Campi chiave obbligatori
039100970526     C*---
039200970526IF  1C     ERRL00        IFEQ      '0'
039300161128IF  2C                   IF        D03ID   = *blank
039400970526     C                   MOVEL     '1'           ERRL00
039500970526     C                   MOVEL     MSG(14)       DSMSMS
039600970526     C                   SETON                                        60
039700160714     C                   LEAVESR
039800970526E   2C                   ENDIF
039900970526E   1C                   ENDIF
040000161129     C*---
040100161129     C* se Copia il rcd non deve già esistere
040200161129     C*---
040300161129IF  1C     ERRL00        IFEQ      '0'
040400161129IF  1C                   IF        DepOp1 = 'O03'
040500161129IF  2C     D03ID         SETLL     TIVCP000
040600161129     C                   IF        %equal
040700161129     C                   MOVEL     '1'           ERRL00
040800161129     C                   MOVEL     MSG(16)       DSMSMS
040900161129     C                   SETON                                        60
041000161129     C                   LEAVESR
041100161129E   2C                   ENDIF
041200161129E   2C                   ENDIF
041300161129E   1C                   ENDIF
041400970526     C*
041500970526     C                   ENDSR
041600970526     C*------------------------------------------------------------------------*
041700970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
041800970526     C*------------------------------------------------------------------------*
041900970526     C     IREVID        BEGSR
042000970526     C* CAMPI
042100161128     C                   EVAL      D03Ann    = VCPAnn
042200161128     C                   EVAL      D03ID     = VCPID
042300161128     C                   EVAL      D03Anno   = VCPAnno
042400161128     C                   EVAL      D03Prg    = VCPPrg
042500161128     C                   EVAL      D03Max    = VCPMax
042600161128     C                   EVAL      D03Incr   = VCPIncr
042700970526     C*
042800970526     C                   ENDSR
042900970526     C*------------------------------------------------------------------------*
043000970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
043100970526     C*------------------------------------------------------------------------*
043200970526     C     AZZVID        BEGSR
043300970526     C*
043400161128     C                   CLEAR                   D03Ann
043500161128     C                   CLEAR                   D03ID
043600161128     C                   CLEAR                   D03Anno
043700161128     C                   CLEAR                   D03Prg
043800161128     C                   CLEAR                   D03Max
043900161128     C                   CLEAR                   D03Incr
044000970526     C*
044100970526     C                   ENDSR
044200970526     C*------------------------------------------------------------------------*
044300161128     C* IMPVCP - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
044400970526     C*------------------------------------------------------------------------*
044500161128     C     IMPVCP        BEGSR
044600970526     C*
044700161128     C                   EVAL      VCPAnn    = D03Ann
044800161128     C* se l'incremento è 0 lo forzo a 1
044900161128     C                   IF        VCPIncr = 0
045000161128     C                   EVAL      VCPIncr = 1
045100161128     C                   ENDIF
045200970526     C*
045300970526     C                   ENDSR
045400970526     C*------------------------------------------------------------------------*
045500970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
045600970526     C*------------------------------------------------------------------------*
045700970526     C     IMPFUN        BEGSR
045800970526     C*
045900970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
046000970526IF  1C     DEPOP1        IFEQ      'O05'
046100970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
046200970526     C                   MOVEL     'N'           FUDATT
046300970526     C                   MOVEL     FUDRIG        FUD(6)
046400970526E   1C                   ENDIF
046500970526     C*
046600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
046700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
046800970526     C                   EXSR      FUNGES
046900970526     C*
047000970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
047100970526     C*---
047200970526     C* 90 -> PROTEZIONE CHIAVE
047300970526     C* 91 -> PROTEZIONE TOTALE
047400970526     C*---
047500970526IF  1C     DEPOP0        IFEQ      'R02'                                        G E S T I O N E
047600970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
047700160914IF  2C     DEPOP1        OREQ      'O03'                                        *COPIA
047800970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
047900970526     C                   SETOFF                                       91
048000970526E   2C                   ENDIF
048100970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
048200970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
048300970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
048400970526E   2C                   ENDIF
048500970526     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
048600970526IF  2C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
048700970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
048800970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
048900970526     C                   SETON                                        91
049000970526E   2C                   ENDIF
049100970526X   1C                   ELSE                                                   *I N T E R R O G A
049200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
049300970526     C                   SETON                                        91
049400970526E   1C                   ENDIF
049500970526     C*
049600970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
049700160914     C* LETTURA DEL RECORD DA COPIARE -NO IN INSERIMENTO-
049800970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
049900161128     C                   EVAL      VCPID     = VCP2ID
050000161128     C     VCPID         CHAIN(N)  TIVCP01I                           99
050100970526IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
050200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
050300970526     C                   MOVEL     MSG(6)        DSMSMS
050400970526X   2C                   ELSE
050500970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
050600980128IF  3C     DEPOP1        IFEQ      'O05'
050700980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
050800980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
050900980128E   3C                   ENDIF
051000970526E   2C                   ENDIF
051100970526X   1C                   ELSE                                                   *INSERIMENTO
051200970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
051300970526E   1C                   ENDIF
051400970526     C*
051500970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
051600970526IF  1C     DEPOP1        IFEQ      'O02'
051700970526     C                   MOVEL     DEC(1)        VIDFUN
051800970526E   1C                   ENDIF
051900160914IF  1C     DEPOP1        IFEQ      'O03'
052000160914     C                   MOVEL     DEC(6)        VIDFUN
052100160914E   1C                   ENDIF
052200970526IF  1C     DEPOP1        IFEQ      'O04'
052300970526     C                   MOVEL     DEC(2)        VIDFUN
052400970526E   1C                   ENDIF
052500970526IF  1C     DEPOP1        IFEQ      'O05'
052600970526     C                   MOVEL     DEC(3)        VIDFUN
052700970526E   1C                   ENDIF
052800970526IF  1C     DEPOP1        IFEQ      'O07'
052900970526     C                   MOVEL     DEC(4)        VIDFUN
053000970526E   1C                   ENDIF
053100970526IF  1C     DEPOP1        IFEQ      'F06'
053200970526     C                   MOVEL     DEC(5)        VIDFUN
053300970526E   1C                   ENDIF
053400970526     C*
053500970526     C                   ENDSR
053600970526     C*------------------------------------------------------------------------*
053700970526     C* *INZSR - OPERAZIONI INIZIALI
053800970526     C*------------------------------------------------------------------------*
053900970526     C     *INZSR        BEGSR
054000970526     C*---
054100970526     C* RICEVIMENTO PARAMETRI
054200970526     C*---
054300970526     C     *ENTRY        PLIST
054400970612     C                   PARM                    KPJBA
054500161128     C                   PARM                    TIS7VCP2DS
054600970526     C*
054700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
054800970526     C     XMSGPA        PLIST
054900970526     C                   PARM                    DSMSMS
055000970526     C                   PARM                    DSMSPG
055100970526     C                   PARM                    DSMSKE
055200970526     C                   PARM                    DSMSER
055300970526     C*
055400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
055500970526     C                   CALL      'XKEY'
055600970526     C                   PARM                    DSKEY
055700970526     C*---
055800970526     C* VARIABILI NON RIFERITE AL DATA BASE
055900970526     C*---
056000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
056100161128     C     *LIKE         DEFINE    VCP2OP0       DEPOP0                         *DI DEPOSITO
056200161128     C     *LIKE         DEFINE    VCP2OP1       DEPOP1
056300161128     C     *LIKE         DEFINE    VCP2OP0       WOP0                           *DI LAVORO
056400161128     C     *LIKE         DEFINE    VCP2OP1       WOP1
056500970526     C*---
056600970526     C* VARIABILI DI CONTROLLO
056700970526     C*---
056800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
056900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
057000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
057100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
057200970526     C*
057300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
057400161128     C                   MOVEL     'TIS7VCP2R'   DSMSPG
057500161128     C                   MOVEL     'TIS7VCP2R'   MSGQUE
057600970526     C                   MOVEL     *BLANKS       MSGKEY
057700970526     C                   MOVEL     *BLANKS       DSMSMS
057800970526     C*---
057900970526     C* VARIABILI RIFERITE AL DATA BASE
058000970526     C*---
058100970526     C* CHIAVI DI LETTURA
058200970526     C*
058300161128     C*
058400970526     C* CALCOLA LA DATA CORRENTE
058500970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
058600970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
058700970526     C                   Z-ADD     WN8           G08DAT
058800970526     C                   Z-ADD     *ZEROS        G08INV
058900970526     C                   MOVEL     '0'           G08ERR
059000970526     C                   CALL      'XSRDA8'
059100970526     C                   PARM                    WLBDA8
059200970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
059300970526     C*
059400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
059500161128     C                   MOVEL     VCP2OP0       DEPOP0                         *LIVELLO PROCEDURA
059600161128     C                   MOVEL     VCP2OP1       DEPOP1                         *LIVELLO PROGRAMMA
059700970526     C*
059800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
059900970526     C                   EXSR      IMPFUN
060000970526     C*
060100970526     C                   ENDSR
060200970526     C*------------------------------------------------------------------------*
060300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
060400970526     C*------------------------------------------------------------------------*
060500970526     C     FUNGES        BEGSR
060600970526     C*
060700970526     C* PULISCO CAMPI DI LAVORO
060800970526     C                   Z-ADD     0             W                 5 0
060900970526     C*
061000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
061100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
061200970526     C     1             DO        24            I
061300970526     C                   MOVEL     FUD(I)        FUDRIG
061400970526     C*
061500970526     C* PULIZIA FUNZIONI PRECEDENTI
061600970526     C                   MOVEL     *BLANKS       FUV(I)
061700970526     C*
061800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
061900970526     C* (SALTA F24)
062000970526     C     FUDATT        IFEQ      'S'
062100970526     C     FUDFUN        ANDNE     '24'
062200970526     C                   ADD       1             W
062300970526     C                   MOVEL     FUDDES        FUV(W)
062400970526     C                   END
062500970526     C                   END                                                    DO WHILE
062600970526     C*
062700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
062800970526     C* ALTRIMENTI DISATTIVO F24
062900970526     C     1             DO        24            I
063000970526     C                   MOVEL     FUD(I)        FUDRIG
063100970526     C*
063200970526     C     FUDFUN        IFEQ      '24'
063300970526     C     W             IFGT      6
063400970526     C                   MOVEL     'S'           FUDATT
063500970526     C                   ELSE
063600970526     C                   MOVEL     *BLANKS       FUDATT
063700970526     C                   END                                                    W GT 8
063800970526     C                   MOVEL     FUDRIG        FUD(I)
063900970526     C                   END
064000970526     C                   END                                                    DO WHILE
064100970526     C*
064200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
064300970526     C     W             DIV       6             I
064400970526     C                   MVR                     Z
064500970526     C*
064600970526     C     Z             IFNE      0
064700970526     C     I             ADD       1             Z
064800970526     C                   ELSE
064900970526     C                   Z-ADD     I             Z                 5 0
065000970526     C                   END
065100970526     C*
065200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
065300970526     C                   ADD       1             FUNGRU
065400970526     C     FUNGRU        IFGT      Z
065500970526     C     FUNGRU        ORLT      1
065600970526     C                   Z-ADD     1             FUNGRU
065700970526     C                   END
065800970526     C*
065900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
066000970526     C     FUNGRU        IFEQ      1
066100970526     C                   MOVEL     FUV(1)        FUN001
066200970526     C                   MOVEL     FUV(2)        FUN002
066300970526     C                   MOVEL     FUV(3)        FUN003
066400970526     C                   MOVEL     FUV(4)        FUN004
066500970526     C                   MOVEL     FUV(5)        FUN005
066600970526     C                   MOVEL     FUV(6)        FUN006
066700970526     C                   END
066800970526     C*
066900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
067000970526     C     FUNGRU        IFEQ      2
067100970526     C                   MOVEL     FUV(7)        FUN001
067200970526     C                   MOVEL     FUV(8)        FUN002
067300970526     C                   MOVEL     FUV(9)        FUN003
067400970526     C                   MOVEL     FUV(10)       FUN004
067500970526     C                   MOVEL     FUV(11)       FUN005
067600970526     C                   MOVEL     FUV(12)       FUN006
067700970526     C                   END
067800970526     C*
067900970526     C* TERZO GRUPPO DI 6 FUNZIONI
068000970526     C     FUNGRU        IFEQ      3
068100970526     C                   MOVEL     FUV(13)       FUN001
068200970526     C                   MOVEL     FUV(14)       FUN002
068300970526     C                   MOVEL     FUV(15)       FUN003
068400970526     C                   MOVEL     FUV(16)       FUN004
068500970526     C                   MOVEL     FUV(17)       FUN005
068600970526     C                   MOVEL     FUV(18)       FUN006
068700970526     C                   END
068800970526     C*
068900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
069000970526     C     FUNGRU        IFEQ      4
069100970526     C                   MOVEL     FUV(19)       FUN001
069200970526     C                   MOVEL     FUV(20)       FUN002
069300970526     C                   MOVEL     FUV(21)       FUN003
069400970526     C                   MOVEL     FUV(22)       FUN004
069500970526     C                   MOVEL     *BLANKS       FUN005
069600970526     C                   MOVEL     *BLANKS       FUN006
069700970526     C                   END
069800970526     C*
069900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
070000970526     C                   MOVEL     *BLANKS       FUN007
070100970526     C                   MOVEL     *BLANKS       FUN008
070200970526     C*
070300970526     C     1             DO        24            I
070400970526     C                   MOVEL     FUD(I)        FUDRIG
070500970526     C     FUDFUN        IFEQ      '24'
070600970526     C     FUDATT        ANDEQ     'S'
070700970526     C                   MOVEL     FUDDES        FUN008
070800970526     C                   END
070900970526     C                   END                                                    DO
071000970526     C*
071100970526     C                   ENDSR
071200970526     C*------------------------------------------------------------------------*
071300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
071400970526     C*------------------------------------------------------------------------*
071500970526     C     FUNCON        BEGSR
071600970526     C*
071700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
071800970526     C                   EXSR      FUNVAR
071900970526     C*
072000970526     C* PULISCO CAMPI DI LAVORO
072100970526     C                   Z-ADD     0             I                 5 0
072200970526     C                   Z-ADD     0             W                 5 0
072300970526     C*
072400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
072500970526     C     FUNKEY        IFNE      *BLANKS
072600970526     C     1             DO        24            I
072700970526     C                   MOVEL     FUD(I)        FUDRIG
072800970526     C*
072900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
073000970526     C     FUNKEY        IFEQ      FUDFUN
073100970526     C     FUDATT        ANDEQ     'S'
073200970526     C                   Z-ADD     I             W
073300970526     C                   END
073400970526     C                   END                                                    DO WHILE
073500970526     C*
073600970526     C* FUNZIONE NON ATTIVA (ERRORE)
073700970526     C     W             IFEQ      0
073800970526     C*
073900970526     C* EMISSIONE MESSAGGIO DI ERRORE
074000970526     C                   MOVEL     MSG(1)        DSMSMS
074100970526     C*
074200970526     C                   END                                                    W=0
074300970526     C                   END                                                    FUNKEY NE *BLANKS
074400970526     C*
074500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
074600970526     C     W             IFNE      0
074700970526     C     FUNKEY        OREQ      *BLANKS
074800970526     C     FUNKEY        IFEQ      *BLANKS
074900970526     C                   EXSR      ENTGES
075000970526     C                   END
075100970526     C     FUNKEY        IFEQ      '03'
075200970526     C                   EXSR      F03GES
075300970526     C                   END
075400970526     C     FUNKEY        IFEQ      '06'
075500970526     C                   EXSR      F06GES
075600970526     C                   END
075700160715     C     FUNKEY        IFEQ      '12'
075800160715     C                   EXSR      F12GES
075900160715     C                   END
076000970526     C     FUNKEY        IFEQ      '24'
076100970526     C                   EXSR      F24GES
076200970526     C                   END
076300970526     C                   END
076400970526     C*
076500970526     C                   ENDSR
076600970526     C*------------------------------------------------------------------------*
076700970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
076800970526     C*------------------------------------------------------------------------*
076900970526     C     FUNVAR        BEGSR
077000970526     C*
077100970526     C* INIZIALIZZA VARIABILE -FUNKEY-
077200970526     C*
077300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
077400970526     C     KEY           IFEQ      ENTER
077500970526     C                   MOVEL     *BLANKS       FUNKEY
077600970526     C                   END
077700970526     C     KEY           IFEQ      F1
077800970526     C                   MOVEL     '01'          FUNKEY
077900970526     C                   END
078000970526     C     KEY           IFEQ      F2
078100970526     C                   MOVEL     '02'          FUNKEY
078200970526     C                   END
078300970526     C     KEY           IFEQ      F3
078400970526     C                   MOVEL     '03'          FUNKEY
078500970526     C                   END
078600970526     C     KEY           IFEQ      F4
078700970526     C                   MOVEL     '04'          FUNKEY
078800970526     C                   END
078900970526     C     KEY           IFEQ      F5
079000970526     C                   MOVEL     '05'          FUNKEY
079100970526     C                   END
079200970526     C     KEY           IFEQ      F6
079300970526     C                   MOVEL     '06'          FUNKEY
079400970526     C                   END
079500970526     C     KEY           IFEQ      F7
079600970526     C                   MOVEL     '07'          FUNKEY
079700970526     C                   END
079800970526     C     KEY           IFEQ      F8
079900970526     C                   MOVEL     '08'          FUNKEY
080000970526     C                   END
080100970526     C     KEY           IFEQ      F9
080200970526     C                   MOVEL     '09'          FUNKEY
080300970526     C                   END
080400970526     C     KEY           IFEQ      F10
080500970526     C                   MOVEL     '10'          FUNKEY
080600970526     C                   END
080700970526     C     KEY           IFEQ      F11
080800970526     C                   MOVEL     '11'          FUNKEY
080900970526     C                   END
081000970526     C     KEY           IFEQ      F12
081100970526     C                   MOVEL     '12'          FUNKEY
081200970526     C                   END
081300970526     C     KEY           IFEQ      F13
081400970526     C                   MOVEL     '13'          FUNKEY
081500970526     C                   END
081600970526     C     KEY           IFEQ      F14
081700970526     C                   MOVEL     '14'          FUNKEY
081800970526     C                   END
081900970526     C     KEY           IFEQ      F15
082000970526     C                   MOVEL     '15'          FUNKEY
082100970526     C                   END
082200970526     C     KEY           IFEQ      F16
082300970526     C                   MOVEL     '16'          FUNKEY
082400970526     C                   END
082500970526     C     KEY           IFEQ      F17
082600970526     C                   MOVEL     '17'          FUNKEY
082700970526     C                   END
082800970526     C     KEY           IFEQ      F18
082900970526     C                   MOVEL     '18'          FUNKEY
083000970526     C                   END
083100970526     C     KEY           IFEQ      F19
083200970526     C                   MOVEL     '19'          FUNKEY
083300970526     C                   END
083400970526     C     KEY           IFEQ      F20
083500970526     C                   MOVEL     '20'          FUNKEY
083600970526     C                   END
083700970526     C     KEY           IFEQ      F21
083800970526     C                   MOVEL     '21'          FUNKEY
083900970526     C                   END
084000970526     C     KEY           IFEQ      F23
084100970526     C                   MOVEL     '23'          FUNKEY
084200970526     C                   END
084300970526     C     KEY           IFEQ      F24
084400970526     C                   MOVEL     '24'          FUNKEY
084500970526     C                   END
084600970526     C     KEY           IFEQ      ROLLUP
084700970526     C                   MOVEL     'RU'          FUNKEY
084800970526     C                   END
084900970526     C     KEY           IFEQ      HELP
085000970526     C                   MOVEL     'HP'          FUNKEY
085100970526     C                   END
085200970526     C                   ENDSR
085300970526     C*------------------------------------------------------------------------*
085400970526     C* NOTE COSTRUZIONE SCHIERE
085500970526     C*------------------------------------------------------------------------*
085600970526     C*
085700970526     C* -MSG-MESSAGGI
085800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
085900970526     C* (NON TOCCARE MSG,1 E MSG,2)
086000970526     C*
086100970526     C* -CMD-COMANDI
086200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
086300970526     C*
086400970526     C* -OPD-OPZIONI
086500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
086600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
086700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
086800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
086900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
087000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
087100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
087200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
087300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
087400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
087500970526     C*
087600970526     C* -FUD-FUNZIONI
087700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
087800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
087900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
088000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
088100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
088200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
088300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
088400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
088500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
088600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
088700970526     O*------------------------------------------------------------------------*
088800970526** DEC - DECODIFICHE
088900970526    MODIFICA                                                      1
089000970526  ANNULLAMENTO                                                    2
089100970526VISUALIZZAZIONE                                                   3
089200970526   RIPRISTINO                                                     4
089300970526  INSERIMENTO                                                     5
089400160914     COPIA                                                        6
089500970526** MSG - MESSAGGI
089600161128TIS7VCP2R- Tasto funzionale non gestito                                        1
089700161128TIS7VCP2R- Inserimento effettuato                                              2
089800161128TIS7VCP2R- Modifica effettuata                                                 3
089900161128TIS7VCP2R- Annullamento effettuato                                             4
090000161128TIS7VCP2R- Ripristino effettuato                                               5
090100161128TIS7VCP2R- Richiesta funzione per record inesistente                           6
090200161128TIS7VCP2R- Record momentaneamente allocato: riprovare più tardi                7
090300161128TIS7VCP2R- Richiesta funzione per record esistente                             8
090400161128TIS7VCP2R- Data errata                                                         9
090500161128TIS7VCP2R- La data scadenza deve essere maggiore della decorrenza             10
090600161128TIS7VCP2R- La procedura non può scadere in un giorno già trascorso
090700161128TIS7VCP2R- Copia effettuata                                                   12
090800161128TIS7VCP2R- Descrizione obbligatoria                                           13
090900161128TIS7VCP2R- Campo obbligatorio                                                 14
091000161129TIS7VCP2R- 15
091100161129TIS7VCP2R- ID Contatore già esistente                                         16
091200161128TIS7VCP2R- 17
091300161128TIS7VCP2R- 18
091400161128TIS7VCP2R- 19
091500161128TIS7VCP2R- 20
091600970526** FUD - FUNZIONI
091700970526                   -        (S/N)=N                        funz.01¬
091800970526                   -        (S/N)=N                        funz.02¬
091900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
092000970526                   -        (S/N)=N                        funz.04¬
092100970526                   -        (S/N)=N                        funz.05¬
092200970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
092300970526                   -        (S/N)=N                        funz.07¬
092400160715                   -        (S/N)=N                        funz.08¬
092500160715                   -        (S/N)=N                        funz.09¬
092600160715                   -        (S/N)=N                        funz.10¬
092700160715                   -        (S/N)=N                        funz.11¬
092800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
092900160715                   -        (S/N)=N                        funz.13¬
093000970526                   -        (S/N)=N                        funz.14¬
093100970526                   -        (S/N)=N                        funz.15¬
093200970526                   -        (S/N)=N                        funz.16¬
093300970526                   -        (S/N)=N                        funz.17¬
093400970526                   -        (S/N)=N                        funz.18¬
093500970526                   -        (S/N)=N                        funz.19¬
093600970526                   -        (S/N)=N                        funz.20¬
093700970526                   -        (S/N)=N                        funz.21¬
093800970526                   -        (S/N)=N                        funz.22¬
093900970526                   -        (S/N)=N                        funz.23¬
094000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
