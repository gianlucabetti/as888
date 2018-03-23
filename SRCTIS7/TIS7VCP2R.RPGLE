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
023300170413     C                   EVAL      VCPTpID   = D03TpID
023400170413     C                   EVAL      VCPID     = D03ID
023500161128     C                   EVAL      VCPAnno   = D03Anno
023600161128     C                   EVAL      VCPPrg    = D03Prg
023700170413     C                   EVAL      VCPFrom   = D03From
023800170413     C                   EVAL      VCPTo     = D03To
023900161128     C                   EVAL      VCPIncr   = D03Incr
024000170413     C                   EVAL      VCPMaxPrg = D03MaxPrg
024100161128     C                   EXSR      IMPVCP                                       *ALTRI CAMPI
024200161128     C                   WRITE     TIVCP000
024300161128     C                   MOVEL     MSG(2)        VCP2MSG                         *MESSAGGIO
024400970602     C*
024500970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
024600970602     C                   EXSR      AZZVID
024700970526E   1C                   ENDIF
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* MODIFI - OPERAZIONI DI MODIFICA
025200970526     C*------------------------------------------------------------------------*
025300970526     C     MODIFI        BEGSR
025400970526     C*
025500970526     C* CONTROLLA I CAMPI IMMESSI
025600970526     C                   EXSR      DECODI
025700970526     C*
025800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
025900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026000161128     C                   EXSR      LETVCP
026100970526     C*
026200970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026400170413     C                   EVAL      VCPTpID   = D03TpID
026500170413     C                   EVAL      VCPID     = D03ID
026600161129     C                   EVAL      VCPAnno   = D03Anno
026700161129     C                   EVAL      VCPPrg    = D03Prg
026800170413     C                   EVAL      VCPFrom   = D03From
026900170413     C                   EVAL      VCPTo     = D03To
027000170413     C                   EVAL      VCPIncr   = D03Incr
027100170413     C                   EVAL      VCPMaxPrg = D03MaxPrg
027200161128     C                   EXSR      IMPVCP                                       *IMPOSTA I CAMPI
027300161128     C                   UPDATE    TIVCP000
027400161128     C                   MOVEL     MSG(3)        VCP2MSG
027500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
027600970526E   2C                   ENDIF
027700970526E   1C                   ENDIF
027800970526     C*
027900970526     C                   ENDSR
028000160914     C*------------------------------------------------------------------------*
028100160914     C* copia  - OPERAZIONI DI copia
028200160914     C*------------------------------------------------------------------------*
028300160914     C     copia         BEGSR
028400160914     C*
028500160914     C* CONTROLLA I CAMPI IMMESSI
028600160914     C                   EXSR      DECODI
028700160914     C*
028800160914     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028900160914IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029000161128     C                   EVAL      VCPAnn    = *blank
029100170413     C                   EVAL      VCPTpID   = D03TpID
029200161128     C                   EVAL      VCPID     = D03ID
029300161128     C                   EVAL      VCPAnno   = D03Anno
029400161128     C                   EVAL      VCPPrg    = D03Prg
029500170413     C                   EVAL      VCPFrom   = D03From
029600170413     C                   EVAL      VCPTo     = D03To
029700170413     C                   EVAL      VCPIncr   = D03Incr
029800170413     C                   EVAL      VCPMaxPrg = D03MaxPrg
029900161128     C                   EXSR      IMPVCP                                       *ALTRI CAMPI
030000161128     C                   WRITE     TIVCP000
030100161128     C                   MOVEL     MSG(12)       VCP2MSG                         *MESSAGGIO
030200160914     C                   MOVEL     '1'           FINL00                         *FINE PGM
030300160914     C*
030400160914     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
030500160914     C                   EXSR      AZZVID
030600160914E   1C                   ENDIF
030700160914     C*
030800160914     C                   ENDSR
030900970526     C*------------------------------------------------------------------------*
031000970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
031100970526     C*------------------------------------------------------------------------*
031200970526     C     ANNULL        BEGSR
031300970526     C*
031400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
031500161128     C                   EXSR      LETVCP
031600160714     C*
031700160714IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031800970526     C*
031900160714     C* ANNULLA il rcd
032000160718     C* L'annullamento  aggiorna il flag annullamento senza altri ragionamenti
032100161128     C                   EVAL      VCPAnn = 'A'
032200161128     C                   UPDATE    TIVCP000                                     *CANCELLA
032300161128     C                   MOVEL     MSG(4)        VCP2MSG                         *MESSAGGIO
032400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032500970526E   1C                   ENDIF
032600970526     C*
032700970526     C                   ENDSR
032800970526     C*------------------------------------------------------------------------*
032900970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
033000970526     C*------------------------------------------------------------------------*
033100970526     C     RIPRIS        BEGSR
033200970526     C*
033300970526     C* CONTROLLA I CAMPI IMMESSI
033400970526     C                   EXSR      DECODI
033500970526     C*
033600970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
033700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033800161128     C                   EXSR      LETVCP
033900970526     C*
034000970526     C* IMPOSTA IL RECORD DA SCRIVERE
034100970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034200161128     C                   MOVEL     *BLANKS       VCPAnn                         *FLAG ANNULLAMENTO
034300161128     C                   UPDATE    TIVCP000                                     *ANNULLA
034400970526E   2C                   ENDIF
034500161128     C                   MOVEL     MSG(5)        VCP2MSG                         *MESSAGGIO
034600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
034700970526E   1C                   ENDIF
034800970526     C*
034900970526     C                   ENDSR
035000970526     C*------------------------------------------------------------------------*
035100161128     C* LETVCP - LEGGE IL RECORD PERL'AGGIORNAMENTO
035200970526     C*------------------------------------------------------------------------*
035300161128     C     LETVCP        BEGSR
035400970526     C*
035500170413         chain(e)  (D03TPID:D03ID)  TIVCP000;
035600170413     C***  D03ID         CHAIN     TIVCP01I                           9998
035700170413IF  1C***  *IN98         IFEQ      *ON                                          *ALLOCATO
035800170413         if %error;
035900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
036000161128     C                   MOVEL     MSG(7)        VCP2MSG                         *MESSAGGIO
036100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
036200970526E   1C                   ENDIF
036300170413IF  1C***  *IN99         IFEQ      *ON                                          *INSESISTENTE
036400170413         if not %found();
036500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
036600161128     C                   MOVEL     MSG(6)        VCP2MSG                         *MESSAGGIO
036700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
036800970526E   1C                   ENDIF
036900970526     C*
037000970526     C                   ENDSR
037100970528     C*------------------------------------------------------------------------*
037200970528     C* LANPGM - LANCIA UN PROGRAMMA
037300970528     C*------------------------------------------------------------------------*
037400970528     C     LANPGM        BEGSR
037500970528     C*
037600970528     C* IMPOSTA LA DS DI PROCEDURA
037700161128     C                   MOVEL     WOP0          VCP2OP0                         *LIVELLO PROCEDURA
037800161128     C                   MOVEL     WOP1          VCP2OP1                         *LIVELLO PGM
037900161128     C                   MOVEL     '0'           VCP2F03                         *PREMUTO F03=FINE
038000161128     C                   MOVEL     '0'           VCP2F12                         *PREMUTO F12=RIT
038100161128     C                   MOVEL     '0'           VCP2ERR                         *'1'=ERRORE
038200161128     C                   MOVEL     *BLANKS       VCP2MSG                         *MESSAGGIO DI RIT
038300970528     C*
038400160714     C*                  CALL      WPGM
038500160714     C*                  PARM                    KPJBA
038600161128     C*                  PARM                    TIS7VCP2DS
038700970528     C*
038800970528     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
038900161128IF  1C     VCP2F03       IFEQ      '1'                                          *PREMUTO F03
039000970528     C                   MOVEL     '1'           ERRL00                         *ERRORE
039100970528     C                   MOVEL     '1'           F03L00                         *FINE PGM
039200970528X   1C                   ELSE                                                   *NON PREMUTO F03
039300161128     C                   MOVEL     VCP2MSG       DSMSMS                         *MESSAGGIO DI RIT
039400970528E   1C                   ENDIF
039500970528     C*
039600970528     C                   ENDSR
039700970526     C*------------------------------------------------------------------------*
039800970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
039900970526     C*------------------------------------------------------------------------*
040000970526     C     DECODI        BEGSR
040100970526     C*---
040200161129     C* Campi chiave obbligatori
040300970526     C*---
040400970526IF  1C     ERRL00        IFEQ      '0'
040500170413IF  2C                   IF        D03TpID   = *blank  and
040600170413IF  2C                             D03ID   = *blank
040700970526     C                   MOVEL     '1'           ERRL00
040800970526     C                   MOVEL     MSG(14)       DSMSMS
040900970526     C                   SETON                                        60
041000160714     C                   LEAVESR
041100970526E   2C                   ENDIF
041200970526E   1C                   ENDIF
041300161129     C*---
041400161129     C* se Copia il rcd non deve già esistere
041500161129     C*---
041600161129IF  1C     ERRL00        IFEQ      '0'
041700161129IF  1C                   IF        DepOp1 = 'O03'
041800170413IF  2C***  D03ID         SETLL     TIVCP000
041900170413IF  2      setll  (D03TpID:D03ID)  TIVCP000;
042000161129     C                   IF        %equal
042100161129     C                   MOVEL     '1'           ERRL00
042200161129     C                   MOVEL     MSG(16)       DSMSMS
042300161129     C                   SETON                                        60
042400161129     C                   LEAVESR
042500161129E   2C                   ENDIF
042600161129E   2C                   ENDIF
042700161129E   1C                   ENDIF
042800970526     C*
042900970526     C                   ENDSR
043000970526     C*------------------------------------------------------------------------*
043100970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
043200970526     C*------------------------------------------------------------------------*
043300970526     C     IREVID        BEGSR
043400970526     C* CAMPI
043500161128     C                   EVAL      D03Ann    = VCPAnn
043600170413     C                   EVAL      D03TpID   = VCPTpID
043700170413     C                   EVAL      D03ID     = VCPID
043800161128     C                   EVAL      D03Anno   = VCPAnno
043900161128     C                   EVAL      D03Prg    = VCPPrg
044000170413     C                   EVAL      D03From   = VCPFrom
044100170413     C                   EVAL      D03To     = VCPTo
044200170413     C                   EVAL      D03Incr   = VCPIncr
044300170413     C                   EVAL      D03MaxPrg = VCPMaxPrg
044400170426     C                   EVAL      D03FLO01  = %subst(VCPFLO:1:1)
044500170426          *in92 = *off;
044600170426          if D03Flo01 = 'A';
044700170426            *in92 = *on;
044800170426          endif;
044900970526     C*
045000970526     C                   ENDSR
045100970526     C*------------------------------------------------------------------------*
045200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
045300970526     C*------------------------------------------------------------------------*
045400970526     C     AZZVID        BEGSR
045500970526     C*
045600161128     C                   CLEAR                   D03Ann
045700170413     C                   CLEAR                   D03TpID
045800170413     C                   CLEAR                   D03ID
045900161128     C                   CLEAR                   D03Anno
046000161128     C                   CLEAR                   D03Prg
046100170413     C                   CLEAR                   D03From
046200170413     C                   CLEAR                   D03To
046300170413     C                   CLEAR                   D03Incr
046400170413     C                   CLEAR                   D03MaxPrg
046500170426     C                   CLEAR                   D03FLO01
046600970526     C*
046700970526     C                   ENDSR
046800970526     C*------------------------------------------------------------------------*
046900161128     C* IMPVCP - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
047000970526     C*------------------------------------------------------------------------*
047100161128     C     IMPVCP        BEGSR
047200970526     C*
047300161128     C                   EVAL      VCPAnn    = D03Ann
047400161128     C* se l'incremento è 0 lo forzo a 1
047500161128     C                   IF        VCPIncr = 0
047600161128     C                   EVAL      VCPIncr = 1
047700161128     C                   ENDIF
047800170427     C* se il nr. max stacchi è 0 lo forzo a 1
047900170427     C                   IF        VCPMaxPrg = 0
048000170427     C                   EVAL      VCPMaxPrg = 1
048100170427     C                   ENDIF
048200970526     C*
048300970526     C                   ENDSR
048400970526     C*------------------------------------------------------------------------*
048500970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
048600970526     C*------------------------------------------------------------------------*
048700970526     C     IMPFUN        BEGSR
048800970526     C*
048900970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
049000970526IF  1C     DEPOP1        IFEQ      'O05'
049100970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
049200970526     C                   MOVEL     'N'           FUDATT
049300970526     C                   MOVEL     FUDRIG        FUD(6)
049400970526E   1C                   ENDIF
049500970526     C*
049600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
049700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
049800970526     C                   EXSR      FUNGES
049900970526     C*
050000970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
050100970526     C*---
050200970526     C* 90 -> PROTEZIONE CHIAVE
050300970526     C* 91 -> PROTEZIONE TOTALE
050400170426     C* 92 -> PROTEZIONE se automatico
050500970526     C*---
050600970526IF  1C     DEPOP0        IFEQ      'R02'                                        G E S T I O N E
050700970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
050800160914IF  2C     DEPOP1        OREQ      'O03'                                        *COPIA
050900970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
051000970526     C                   SETOFF                                       91
051100970526E   2C                   ENDIF
051200970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
051300970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
051400970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
051500970526E   2C                   ENDIF
051600970526     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
051700970526IF  2C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
051800970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
051900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
052000970526     C                   SETON                                        91
052100970526E   2C                   ENDIF
052200970526X   1C                   ELSE                                                   *I N T E R R O G A
052300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
052400970526     C                   SETON                                        91
052500970526E   1C                   ENDIF
052600970526     C*
052700970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
052800160914     C* LETTURA DEL RECORD DA COPIARE -NO IN INSERIMENTO-
052900970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
053000170413     C                   EVAL      VCPTpID   = VCP2TpID
053100170413     C                   EVAL      VCPID     = VCP2ID
053200170413     C***  VCPID         CHAIN(N)  TIVCP01I                           99
053300170413IF  2C***  *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
053400170413         chain(n)  (VCPTPID:VCPID)  TIVCP000;
053500170413         if not %found();
053600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
053700970526     C                   MOVEL     MSG(6)        DSMSMS
053800970526X   2C                   ELSE
053900970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
054000980128IF  3C     DEPOP1        IFEQ      'O05'
054100980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
054200980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
054300980128E   3C                   ENDIF
054400970526E   2C                   ENDIF
054500970526X   1C                   ELSE                                                   *INSERIMENTO
054600970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
054700970526E   1C                   ENDIF
054800970526     C*
054900970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
055000970526IF  1C     DEPOP1        IFEQ      'O02'
055100970526     C                   MOVEL     DEC(1)        VIDFUN
055200970526E   1C                   ENDIF
055300160914IF  1C     DEPOP1        IFEQ      'O03'
055400160914     C                   MOVEL     DEC(6)        VIDFUN
055500160914E   1C                   ENDIF
055600970526IF  1C     DEPOP1        IFEQ      'O04'
055700970526     C                   MOVEL     DEC(2)        VIDFUN
055800970526E   1C                   ENDIF
055900970526IF  1C     DEPOP1        IFEQ      'O05'
056000970526     C                   MOVEL     DEC(3)        VIDFUN
056100970526E   1C                   ENDIF
056200970526IF  1C     DEPOP1        IFEQ      'O07'
056300970526     C                   MOVEL     DEC(4)        VIDFUN
056400970526E   1C                   ENDIF
056500970526IF  1C     DEPOP1        IFEQ      'F06'
056600970526     C                   MOVEL     DEC(5)        VIDFUN
056700970526E   1C                   ENDIF
056800970526     C*
056900970526     C                   ENDSR
057000970526     C*------------------------------------------------------------------------*
057100970526     C* *INZSR - OPERAZIONI INIZIALI
057200970526     C*------------------------------------------------------------------------*
057300970526     C     *INZSR        BEGSR
057400970526     C*---
057500970526     C* RICEVIMENTO PARAMETRI
057600970526     C*---
057700970526     C     *ENTRY        PLIST
057800970612     C                   PARM                    KPJBA
057900161128     C                   PARM                    TIS7VCP2DS
058000970526     C*
058100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
058200970526     C     XMSGPA        PLIST
058300970526     C                   PARM                    DSMSMS
058400970526     C                   PARM                    DSMSPG
058500970526     C                   PARM                    DSMSKE
058600970526     C                   PARM                    DSMSER
058700970526     C*
058800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
058900970526     C                   CALL      'XKEY'
059000970526     C                   PARM                    DSKEY
059100970526     C*---
059200970526     C* VARIABILI NON RIFERITE AL DATA BASE
059300970526     C*---
059400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
059500161128     C     *LIKE         DEFINE    VCP2OP0       DEPOP0                         *DI DEPOSITO
059600161128     C     *LIKE         DEFINE    VCP2OP1       DEPOP1
059700161128     C     *LIKE         DEFINE    VCP2OP0       WOP0                           *DI LAVORO
059800161128     C     *LIKE         DEFINE    VCP2OP1       WOP1
059900970526     C*---
060000970526     C* VARIABILI DI CONTROLLO
060100970526     C*---
060200970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
060300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
060400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
060500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
060600970526     C*
060700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
060800161128     C                   MOVEL     'TIS7VCP2R'   DSMSPG
060900161128     C                   MOVEL     'TIS7VCP2R'   MSGQUE
061000970526     C                   MOVEL     *BLANKS       MSGKEY
061100970526     C                   MOVEL     *BLANKS       DSMSMS
061200970526     C*---
061300970526     C* VARIABILI RIFERITE AL DATA BASE
061400970526     C*---
061500970526     C* CHIAVI DI LETTURA
061600970526     C*
061700161128     C*
061800970526     C* CALCOLA LA DATA CORRENTE
061900970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
062000970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
062100970526     C                   Z-ADD     WN8           G08DAT
062200970526     C                   Z-ADD     *ZEROS        G08INV
062300970526     C                   MOVEL     '0'           G08ERR
062400970526     C                   CALL      'XSRDA8'
062500970526     C                   PARM                    WLBDA8
062600970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
062700970526     C*
062800970526     C* DEPOSITA LE OPZIONI DI ENTRATA
062900161128     C                   MOVEL     VCP2OP0       DEPOP0                         *LIVELLO PROCEDURA
063000161128     C                   MOVEL     VCP2OP1       DEPOP1                         *LIVELLO PROGRAMMA
063100970526     C*
063200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
063300970526     C                   EXSR      IMPFUN
063400970526     C*
063500970526     C                   ENDSR
063600970526     C*------------------------------------------------------------------------*
063700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
063800970526     C*------------------------------------------------------------------------*
063900970526     C     FUNGES        BEGSR
064000970526     C*
064100970526     C* PULISCO CAMPI DI LAVORO
064200970526     C                   Z-ADD     0             W                 5 0
064300970526     C*
064400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
064500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
064600970526     C     1             DO        24            I
064700970526     C                   MOVEL     FUD(I)        FUDRIG
064800970526     C*
064900970526     C* PULIZIA FUNZIONI PRECEDENTI
065000970526     C                   MOVEL     *BLANKS       FUV(I)
065100970526     C*
065200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
065300970526     C* (SALTA F24)
065400970526     C     FUDATT        IFEQ      'S'
065500970526     C     FUDFUN        ANDNE     '24'
065600970526     C                   ADD       1             W
065700970526     C                   MOVEL     FUDDES        FUV(W)
065800970526     C                   END
065900970526     C                   END                                                    DO WHILE
066000970526     C*
066100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
066200970526     C* ALTRIMENTI DISATTIVO F24
066300970526     C     1             DO        24            I
066400970526     C                   MOVEL     FUD(I)        FUDRIG
066500970526     C*
066600970526     C     FUDFUN        IFEQ      '24'
066700970526     C     W             IFGT      6
066800970526     C                   MOVEL     'S'           FUDATT
066900970526     C                   ELSE
067000970526     C                   MOVEL     *BLANKS       FUDATT
067100970526     C                   END                                                    W GT 8
067200970526     C                   MOVEL     FUDRIG        FUD(I)
067300970526     C                   END
067400970526     C                   END                                                    DO WHILE
067500970526     C*
067600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
067700970526     C     W             DIV       6             I
067800970526     C                   MVR                     Z
067900970526     C*
068000970526     C     Z             IFNE      0
068100970526     C     I             ADD       1             Z
068200970526     C                   ELSE
068300970526     C                   Z-ADD     I             Z                 5 0
068400970526     C                   END
068500970526     C*
068600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
068700970526     C                   ADD       1             FUNGRU
068800970526     C     FUNGRU        IFGT      Z
068900970526     C     FUNGRU        ORLT      1
069000970526     C                   Z-ADD     1             FUNGRU
069100970526     C                   END
069200970526     C*
069300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
069400970526     C     FUNGRU        IFEQ      1
069500970526     C                   MOVEL     FUV(1)        FUN001
069600970526     C                   MOVEL     FUV(2)        FUN002
069700970526     C                   MOVEL     FUV(3)        FUN003
069800970526     C                   MOVEL     FUV(4)        FUN004
069900970526     C                   MOVEL     FUV(5)        FUN005
070000970526     C                   MOVEL     FUV(6)        FUN006
070100970526     C                   END
070200970526     C*
070300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
070400970526     C     FUNGRU        IFEQ      2
070500970526     C                   MOVEL     FUV(7)        FUN001
070600970526     C                   MOVEL     FUV(8)        FUN002
070700970526     C                   MOVEL     FUV(9)        FUN003
070800970526     C                   MOVEL     FUV(10)       FUN004
070900970526     C                   MOVEL     FUV(11)       FUN005
071000970526     C                   MOVEL     FUV(12)       FUN006
071100970526     C                   END
071200970526     C*
071300970526     C* TERZO GRUPPO DI 6 FUNZIONI
071400970526     C     FUNGRU        IFEQ      3
071500970526     C                   MOVEL     FUV(13)       FUN001
071600970526     C                   MOVEL     FUV(14)       FUN002
071700970526     C                   MOVEL     FUV(15)       FUN003
071800970526     C                   MOVEL     FUV(16)       FUN004
071900970526     C                   MOVEL     FUV(17)       FUN005
072000970526     C                   MOVEL     FUV(18)       FUN006
072100970526     C                   END
072200970526     C*
072300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
072400970526     C     FUNGRU        IFEQ      4
072500970526     C                   MOVEL     FUV(19)       FUN001
072600970526     C                   MOVEL     FUV(20)       FUN002
072700970526     C                   MOVEL     FUV(21)       FUN003
072800970526     C                   MOVEL     FUV(22)       FUN004
072900970526     C                   MOVEL     *BLANKS       FUN005
073000970526     C                   MOVEL     *BLANKS       FUN006
073100970526     C                   END
073200970526     C*
073300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
073400970526     C                   MOVEL     *BLANKS       FUN007
073500970526     C                   MOVEL     *BLANKS       FUN008
073600970526     C*
073700970526     C     1             DO        24            I
073800970526     C                   MOVEL     FUD(I)        FUDRIG
073900970526     C     FUDFUN        IFEQ      '24'
074000970526     C     FUDATT        ANDEQ     'S'
074100970526     C                   MOVEL     FUDDES        FUN008
074200970526     C                   END
074300970526     C                   END                                                    DO
074400970526     C*
074500970526     C                   ENDSR
074600970526     C*------------------------------------------------------------------------*
074700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
074800970526     C*------------------------------------------------------------------------*
074900970526     C     FUNCON        BEGSR
075000970526     C*
075100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
075200970526     C                   EXSR      FUNVAR
075300970526     C*
075400970526     C* PULISCO CAMPI DI LAVORO
075500970526     C                   Z-ADD     0             I                 5 0
075600970526     C                   Z-ADD     0             W                 5 0
075700970526     C*
075800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
075900970526     C     FUNKEY        IFNE      *BLANKS
076000970526     C     1             DO        24            I
076100970526     C                   MOVEL     FUD(I)        FUDRIG
076200970526     C*
076300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
076400970526     C     FUNKEY        IFEQ      FUDFUN
076500970526     C     FUDATT        ANDEQ     'S'
076600970526     C                   Z-ADD     I             W
076700970526     C                   END
076800970526     C                   END                                                    DO WHILE
076900970526     C*
077000970526     C* FUNZIONE NON ATTIVA (ERRORE)
077100970526     C     W             IFEQ      0
077200970526     C*
077300970526     C* EMISSIONE MESSAGGIO DI ERRORE
077400970526     C                   MOVEL     MSG(1)        DSMSMS
077500970526     C*
077600970526     C                   END                                                    W=0
077700970526     C                   END                                                    FUNKEY NE *BLANKS
077800970526     C*
077900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
078000970526     C     W             IFNE      0
078100970526     C     FUNKEY        OREQ      *BLANKS
078200970526     C     FUNKEY        IFEQ      *BLANKS
078300970526     C                   EXSR      ENTGES
078400970526     C                   END
078500970526     C     FUNKEY        IFEQ      '03'
078600970526     C                   EXSR      F03GES
078700970526     C                   END
078800970526     C     FUNKEY        IFEQ      '06'
078900970526     C                   EXSR      F06GES
079000970526     C                   END
079100160715     C     FUNKEY        IFEQ      '12'
079200160715     C                   EXSR      F12GES
079300160715     C                   END
079400970526     C     FUNKEY        IFEQ      '24'
079500970526     C                   EXSR      F24GES
079600970526     C                   END
079700970526     C                   END
079800970526     C*
079900970526     C                   ENDSR
080000970526     C*------------------------------------------------------------------------*
080100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
080200970526     C*------------------------------------------------------------------------*
080300970526     C     FUNVAR        BEGSR
080400970526     C*
080500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
080600970526     C*
080700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
080800970526     C     KEY           IFEQ      ENTER
080900970526     C                   MOVEL     *BLANKS       FUNKEY
081000970526     C                   END
081100970526     C     KEY           IFEQ      F1
081200970526     C                   MOVEL     '01'          FUNKEY
081300970526     C                   END
081400970526     C     KEY           IFEQ      F2
081500970526     C                   MOVEL     '02'          FUNKEY
081600970526     C                   END
081700970526     C     KEY           IFEQ      F3
081800970526     C                   MOVEL     '03'          FUNKEY
081900970526     C                   END
082000970526     C     KEY           IFEQ      F4
082100970526     C                   MOVEL     '04'          FUNKEY
082200970526     C                   END
082300970526     C     KEY           IFEQ      F5
082400970526     C                   MOVEL     '05'          FUNKEY
082500970526     C                   END
082600970526     C     KEY           IFEQ      F6
082700970526     C                   MOVEL     '06'          FUNKEY
082800970526     C                   END
082900970526     C     KEY           IFEQ      F7
083000970526     C                   MOVEL     '07'          FUNKEY
083100970526     C                   END
083200970526     C     KEY           IFEQ      F8
083300970526     C                   MOVEL     '08'          FUNKEY
083400970526     C                   END
083500970526     C     KEY           IFEQ      F9
083600970526     C                   MOVEL     '09'          FUNKEY
083700970526     C                   END
083800970526     C     KEY           IFEQ      F10
083900970526     C                   MOVEL     '10'          FUNKEY
084000970526     C                   END
084100970526     C     KEY           IFEQ      F11
084200970526     C                   MOVEL     '11'          FUNKEY
084300970526     C                   END
084400970526     C     KEY           IFEQ      F12
084500970526     C                   MOVEL     '12'          FUNKEY
084600970526     C                   END
084700970526     C     KEY           IFEQ      F13
084800970526     C                   MOVEL     '13'          FUNKEY
084900970526     C                   END
085000970526     C     KEY           IFEQ      F14
085100970526     C                   MOVEL     '14'          FUNKEY
085200970526     C                   END
085300970526     C     KEY           IFEQ      F15
085400970526     C                   MOVEL     '15'          FUNKEY
085500970526     C                   END
085600970526     C     KEY           IFEQ      F16
085700970526     C                   MOVEL     '16'          FUNKEY
085800970526     C                   END
085900970526     C     KEY           IFEQ      F17
086000970526     C                   MOVEL     '17'          FUNKEY
086100970526     C                   END
086200970526     C     KEY           IFEQ      F18
086300970526     C                   MOVEL     '18'          FUNKEY
086400970526     C                   END
086500970526     C     KEY           IFEQ      F19
086600970526     C                   MOVEL     '19'          FUNKEY
086700970526     C                   END
086800970526     C     KEY           IFEQ      F20
086900970526     C                   MOVEL     '20'          FUNKEY
087000970526     C                   END
087100970526     C     KEY           IFEQ      F21
087200970526     C                   MOVEL     '21'          FUNKEY
087300970526     C                   END
087400970526     C     KEY           IFEQ      F23
087500970526     C                   MOVEL     '23'          FUNKEY
087600970526     C                   END
087700970526     C     KEY           IFEQ      F24
087800970526     C                   MOVEL     '24'          FUNKEY
087900970526     C                   END
088000970526     C     KEY           IFEQ      ROLLUP
088100970526     C                   MOVEL     'RU'          FUNKEY
088200970526     C                   END
088300970526     C     KEY           IFEQ      HELP
088400970526     C                   MOVEL     'HP'          FUNKEY
088500970526     C                   END
088600970526     C                   ENDSR
088700970526     C*------------------------------------------------------------------------*
088800970526     C* NOTE COSTRUZIONE SCHIERE
088900970526     C*------------------------------------------------------------------------*
089000970526     C*
089100970526     C* -MSG-MESSAGGI
089200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
089300970526     C* (NON TOCCARE MSG,1 E MSG,2)
089400970526     C*
089500970526     C* -CMD-COMANDI
089600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
089700970526     C*
089800970526     C* -OPD-OPZIONI
089900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
090000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
090100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
090200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
090300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
090400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
090500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
090600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
090700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
090800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
090900970526     C*
091000970526     C* -FUD-FUNZIONI
091100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
091200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
091300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
091400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
091500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
091600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
091700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
091800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
091900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
092000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
092100970526     O*------------------------------------------------------------------------*
092200970526** DEC - DECODIFICHE
092300970526    MODIFICA                                                      1
092400970526  ANNULLAMENTO                                                    2
092500970526VISUALIZZAZIONE                                                   3
092600970526   RIPRISTINO                                                     4
092700970526  INSERIMENTO                                                     5
092800160914     COPIA                                                        6
092900970526** MSG - MESSAGGI
093000161128TIS7VCP2R- Tasto funzionale non gestito                                        1
093100161128TIS7VCP2R- Inserimento effettuato                                              2
093200161128TIS7VCP2R- Modifica effettuata                                                 3
093300161128TIS7VCP2R- Annullamento effettuato                                             4
093400161128TIS7VCP2R- Ripristino effettuato                                               5
093500161128TIS7VCP2R- Richiesta funzione per record inesistente                           6
093600161128TIS7VCP2R- Record momentaneamente allocato: riprovare più tardi                7
093700161128TIS7VCP2R- Richiesta funzione per record esistente                             8
093800161128TIS7VCP2R- Data errata                                                         9
093900161128TIS7VCP2R- La data scadenza deve essere maggiore della decorrenza             10
094000161128TIS7VCP2R- La procedura non può scadere in un giorno già trascorso
094100161128TIS7VCP2R- Copia effettuata                                                   12
094200161128TIS7VCP2R- Descrizione obbligatoria                                           13
094300161128TIS7VCP2R- Campo obbligatorio                                                 14
094400161129TIS7VCP2R- 15
094500170413TIS7VCP2R- Contatore già esistente                                            16
094600161128TIS7VCP2R- 17
094700161128TIS7VCP2R- 18
094800161128TIS7VCP2R- 19
094900161128TIS7VCP2R- 20
095000970526** FUD - FUNZIONI
095100970526                   -        (S/N)=N                        funz.01¬
095200970526                   -        (S/N)=N                        funz.02¬
095300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
095400970526                   -        (S/N)=N                        funz.04¬
095500970526                   -        (S/N)=N                        funz.05¬
095600970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
095700970526                   -        (S/N)=N                        funz.07¬
095800160715                   -        (S/N)=N                        funz.08¬
095900160715                   -        (S/N)=N                        funz.09¬
096000160715                   -        (S/N)=N                        funz.10¬
096100160715                   -        (S/N)=N                        funz.11¬
096200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
096300160715                   -        (S/N)=N                        funz.13¬
096400970526                   -        (S/N)=N                        funz.14¬
096500970526                   -        (S/N)=N                        funz.15¬
096600970526                   -        (S/N)=N                        funz.16¬
096700970526                   -        (S/N)=N                        funz.17¬
096800970526                   -        (S/N)=N                        funz.18¬
096900970526                   -        (S/N)=N                        funz.19¬
097000970526                   -        (S/N)=N                        funz.20¬
097100970526                   -        (S/N)=N                        funz.21¬
097200970526                   -        (S/N)=N                        funz.22¬
097300970526                   -        (S/N)=N                        funz.23¬
097400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
