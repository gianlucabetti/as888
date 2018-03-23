000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600160714     FTIS7VMS2D CF   E             WORKSTN INFDS(DEVDS1)
000700160714     F                                     SFILE(S7V2DMS:MSGULT)
000800160714     FTIVMS01I  UF A E           K DISK
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
006400160714     D TIS7VMS2DS    E DS
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
009400160714     C                   MOVEL     '1'           VMS2ERR
009500160714     C                   MOVEL     DSMSMS        VMS2MSG
009600970526E   1C                   ENDIF
009700970526     C* FINE PGM
009800970526     C                   SETON                                        LR
009900970526     C*------------------------------------------------------------------------*
010000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010100970526     C*------------------------------------------------------------------------*
010200970526     C     BUTFMR        BEGSR
010300970526     C*
010400970526     C* EMETTE LA TESTATA
010500160714     C                   WRITE     S7V2D01
010600970526     C*
010700970526     C* SPEDISCE EVENTUALE MESSAGGIO
010800970526     C                   EXSR      SNDMSG
010900970526     C*
011000970526     C* EMETTE LE FUNZIONI
011100160714     C                   WRITE     S7V2D02
011200970526     C*
011300970526     C* EMETTE IL VIDEO
011400160714     C                   EXFMT     S7V2D03
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
014000160714     C                   WRITE     S7V2DMC
014100970526     C                   MOVEL     '0'           *IN40
014200970526     C*
014300970526     C* VISUALIZZA IL MESSAGGIO
014400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014500970526     C                   CALL      'XMSG'        XMSGPA
014600970526IF  1C     DSMSER        IFEQ      *ZEROS
014700970526     C                   MOVEL     DSMSKE        MSGKEY
014800970526E   1C                   ENDIF
014900970526     C                   Z-ADD     1             MSGULT
015000160714     C                   WRITE     S7V2DMS
015100160714     C                   WRITE     S7V2DMC
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
017800160714     C                   MOVEL     '1'           VMS2F03                         *DI PROCEDURA
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
021100160715     C                   MOVEL     '1'           VMS2F12                         *DI PROCEDURA
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
023200160714     C                   EVAL      VMSATB    = *blank
023300160714     C                   EVAL      VMSPROC   = D03PROC
023400160714     C                   EVAL      VMSKSU    = D03KSU
023500160831     C                   EVAL      VMSCAUTIP = D03CAUTIP
023600160831     C                   EVAL      VMSCAUBRT = D03CAUBRT
023700160922     C                   EVAL      D03STSCLI = D03STSCLI1 + D03STSCLI2
023800160714     C                   EVAL      VMSSTSCLI = D03STSCLI
023900160715     C                   EVAL      VMSDDE    = %dec(DDEISO)
024000160714     C                   EXSR      IMPVMS                                       *ALTRI CAMPI
024100160714     C                   WRITE     TIVMS000
024200160714     C                   MOVEL     MSG(2)        VMS2MSG                         *MESSAGGIO
024300970602     C*
024400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
024500970602     C                   EXSR      AZZVID
024600970526E   1C                   ENDIF
024700970526     C*
024800970526     C                   ENDSR
024900970526     C*------------------------------------------------------------------------*
025000970526     C* MODIFI - OPERAZIONI DI MODIFICA
025100970526     C*------------------------------------------------------------------------*
025200970526     C     MODIFI        BEGSR
025300970526     C*
025400970526     C* CONTROLLA I CAMPI IMMESSI
025500970526     C                   EXSR      DECODI
025600970526     C*
025700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
025800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025900160714     C                   EXSR      LETVMS
026000970526     C*
026100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026300160714     C                   EXSR      IMPVMS                                       *IMPOSTA I CAMPI
026400160714     C                   UPDATE    TIVMS000
026500160714     C                   MOVEL     MSG(3)        VMS2MSG
026600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
026700970526E   2C                   ENDIF
026800970526E   1C                   ENDIF
026900970526     C*
027000970526     C                   ENDSR
027100160914     C*------------------------------------------------------------------------*
027200160914     C* copia  - OPERAZIONI DI copia
027300160914     C*------------------------------------------------------------------------*
027400160914     C     copia         BEGSR
027500160914     C*
027600160914     C* CONTROLLA I CAMPI IMMESSI
027700160914     C                   EXSR      DECODI
027800160914     C*
027900160914     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028000160914IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028100160914     C                   EVAL      VMSATB    = *blank
028200160914     C                   EVAL      VMSPROC   = D03PROC
028300160914     C                   EVAL      VMSKSU    = D03KSU
028400160914     C                   EVAL      VMSCAUTIP = D03CAUTIP
028500160914     C                   EVAL      VMSCAUBRT = D03CAUBRT
028600160922     C                   EVAL      D03STSCLI = D03STSCLI1 + D03STSCLI2
028700160914     C                   EVAL      VMSSTSCLI = D03STSCLI
028800160914     C                   EVAL      VMSDDE    = %dec(DDEISO)
028900160914     C                   EXSR      IMPVMS                                       *ALTRI CAMPI
029000160914     C                   WRITE     TIVMS000
029100160914     C                   MOVEL     MSG(12)       VMS2MSG                         *MESSAGGIO
029200160914     C                   MOVEL     '1'           FINL00                         *FINE PGM
029300160914     C*
029400160914     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
029500160914     C                   EXSR      AZZVID
029600160914E   1C                   ENDIF
029700160914     C*
029800160914     C                   ENDSR
029900970526     C*------------------------------------------------------------------------*
030000970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
030100970526     C*------------------------------------------------------------------------*
030200970526     C     ANNULL        BEGSR
030300970526     C*
030400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030500160714     C                   EXSR      LETVMS
030600160714     C*
030700160714IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030800970526     C*
030900160714     C* ANNULLA il rcd
031000160718     C* L'annullamento  aggiorna il flag annullamento senza altri ragionamenti
031100160718     C                   EVAL      VMSATB = 'A'
031200160718     C                   UPDATE    TIVMS000                                     *CANCELLA
031300160714     C                   MOVEL     MSG(4)        VMS2MSG                         *MESSAGGIO
031400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031500970526E   1C                   ENDIF
031600970526     C*
031700970526     C                   ENDSR
031800970526     C*------------------------------------------------------------------------*
031900970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
032000970526     C*------------------------------------------------------------------------*
032100970526     C     RIPRIS        BEGSR
032200970526     C*
032300970526     C* CONTROLLA I CAMPI IMMESSI
032400970526     C                   EXSR      DECODI
032500970526     C*
032600970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032800160714     C                   EXSR      LETVMS
032900970526     C*
033000970526     C* IMPOSTA IL RECORD DA SCRIVERE
033100970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033200160714     C                   MOVEL     *BLANKS       VMSATB                         *FLAG ANNULLAMENTO
033300160714     C                   UPDATE    TIVMS000                                     *ANNULLA
033400970526E   2C                   ENDIF
033500160714     C                   MOVEL     MSG(5)        VMS2MSG                         *MESSAGGIO
033600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033700970526E   1C                   ENDIF
033800970526     C*
033900970526     C                   ENDSR
034000970526     C*------------------------------------------------------------------------*
034100160714     C* LETVMS - LEGGE IL RECORD PERL'AGGIORNAMENTO
034200970526     C*------------------------------------------------------------------------*
034300160714     C     LETVMS        BEGSR
034400970526     C*
034500160714     C                   EVAL      VMSPROC   = VMS2PROC
034600160714     C                   EVAL      VMSKSU    = VMS2KSU
034700160913     C                   EVAL      VMSDDE    = %dec(VMS2DDE:8:0)
034800160831     C                   EVAL      VMSCAUTIP = VMS2CAUTIP
034900160831     C                   EVAL      VMSCAUBRT = VMS2CAUBRT
035000160914     C     K05VMS01      CHAIN     TIVMS01I                           9998
035100970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
035200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035300160714     C                   MOVEL     MSG(7)        VMS2MSG                         *MESSAGGIO
035400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035500970526E   1C                   ENDIF
035600970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
035700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035800160714     C                   MOVEL     MSG(6)        VMS2MSG                         *MESSAGGIO
035900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
036000970526E   1C                   ENDIF
036100970526     C*
036200970526     C                   ENDSR
036300970528     C*------------------------------------------------------------------------*
036400970528     C* LANPGM - LANCIA UN PROGRAMMA
036500970528     C*------------------------------------------------------------------------*
036600970528     C     LANPGM        BEGSR
036700970528     C*
036800970528     C* IMPOSTA LA DS DI PROCEDURA
036900160714     C                   MOVEL     WOP0          VMS2OP0                         *LIVELLO PROCEDURA
037000160714     C                   MOVEL     WOP1          VMS2OP1                         *LIVELLO PGM
037100160714     C                   MOVEL     '0'           VMS2F03                         *PREMUTO F03=FINE
037200160714     C                   MOVEL     '0'           VMS2F12                         *PREMUTO F12=RIT
037300160714     C                   MOVEL     '0'           VMS2ERR                         *'1'=ERRORE
037400160714     C                   MOVEL     *BLANKS       VMS2MSG                         *MESSAGGIO DI RIT
037500970528     C*
037600160714     C*                  CALL      WPGM
037700160714     C*                  PARM                    KPJBA
037800160714     C*                  PARM                    TIS7VMS2DS
037900970528     C*
038000970528     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
038100160714IF  1C     VMS2F03       IFEQ      '1'                                          *PREMUTO F03
038200970528     C                   MOVEL     '1'           ERRL00                         *ERRORE
038300970528     C                   MOVEL     '1'           F03L00                         *FINE PGM
038400970528X   1C                   ELSE                                                   *NON PREMUTO F03
038500160714     C                   MOVEL     VMS2MSG       DSMSMS                         *MESSAGGIO DI RIT
038600970528E   1C                   ENDIF
038700970528     C*
038800970528     C                   ENDSR
038900970526     C*------------------------------------------------------------------------*
039000970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
039100970526     C*------------------------------------------------------------------------*
039200970526     C     DECODI        BEGSR
039300970526     C*---
039400160714     C* Campi chiave
039500970526     C*---
039600970526IF  1C     ERRL00        IFEQ      '0'
039700160714IF  2C                   IF        D03Proc = *blank
039800970526     C                   MOVEL     '1'           ERRL00
039900970526     C                   MOVEL     MSG(14)       DSMSMS
040000970526     C                   SETON                                        60
040100160714     C                   LEAVESR
040200970526E   2C                   ENDIF
040300970526E   1C                   ENDIF
040400160831IF  1C     ERRL00        IFEQ      '0'
040500160831IF  2C                   IF        D03CauTip = *blank
040600160831     C                   MOVEL     '1'           ERRL00
040700160831     C                   MOVEL     MSG(14)       DSMSMS
040800160831     C                   SETON                                        68
040900160831     C                   LEAVESR
041000160831E   2C                   ENDIF
041100160831E   1C                   ENDIF
041200160714IF  1C     ERRL00        IFEQ      '0'
041300160714IF  2C                   IF        D03CauBRT = *blank
041400160714     C                   MOVEL     '1'           ERRL00
041500160714     C                   MOVEL     MSG(14)       DSMSMS
041600160714     C                   SETON                                        62
041700160714     C                   LEAVESR
041800160714E   2C                   ENDIF
041900160714E   1C                   ENDIF
042000160714IF  1C     ERRL00        IFEQ      '0'
042100160714IF  2C                   IF        D03DDE  = 0
042200160714     C                   MOVEL     '1'           ERRL00
042300160714     C                   MOVEL     MSG(14)       DSMSMS
042400160714     C                   SETON                                        64
042500160714     C                   LEAVESR
042600160714E   2C                   ENDIF
042700160714E   1C                   ENDIF
042800160714IF  1C     ERRL00        IFEQ      '0'
042900160714IF  2C                   IF        D03DSC  = 0
043000160714     C                   MOVEL     '1'           ERRL00
043100160714     C                   MOVEL     MSG(14)       DSMSMS
043200160714     C                   SETON                                        65
043300160714     C                   LEAVESR
043400160714E   2C                   ENDIF
043500160714E   1C                   ENDIF
043600970526     C*---
043700970526     C* DECORRENZA
043800970526     C*---
043900160714IF  1C     ERRL00        IFEQ      '0'
044000160714     C                   MONITOR
044100160715     C* se il valore della data è > 6 cifre
044200160715     C                   IF        %len(%trim(%char(D03DDE))) > 6
044300160715     C* presumo sia scritta in ggmmaaaa
044400160715IF  5C                   EVAL      DDEISO = %date(D03DDE:*eur)
044500160715     C* se il valore della data è <= 6 cifre
044600160715     C                   ELSE
044700160715     C* presumo sia scritta in ggmmaa
044800160715IF  5C                   EVAL      DDEISO = %date(D03DDE:*dmy)
044900160715     C                   ENDIF
045000160714IF  2C                   ON-ERROR
045100160714     C                   MOVEL     '1'           ERRL00
045200160714     C                   SETON                                        64
045300970526     C                   MOVEL     MSG(9)        DSMSMS
045400160714     C                   LEAVESR
045500160714     C                   ENDMON
045600160715     C* rivalorizzo il video con la data in ggmmaaaa
045700160715     C                   EVAL      D03DDE = %dec(DDEISO:*eur)
045800160714E   1C                   ENDIF
045900970526     C*---
046000970526     C* SCADENZA
046100970526     C*---
046200970526IF  1C     ERRL00        IFEQ      '0'
046300160714     C                   MONITOR
046400160715     C* se il valore della data è > 6 cifre
046500160715     C                   IF        %len(%trim(%char(D03DSC))) > 6
046600160715     C* presumo sia scritta in ggmmaaaa
046700160715IF  5C                   EVAL      DSCISO = %date(D03DSC:*eur)
046800160715     C* se il valore della data è <= 6 cifre
046900160715     C                   ELSE
047000160715     C* presumo sia scritta in ggmmaa
047100160715IF  5C                   EVAL      DSCISO = %date(D03DSC:*dmy)
047200160715     C                   ENDIF
047300160714IF  2C                   ON-ERROR
047400160714     C                   MOVEL     '1'           ERRL00
047500160714     C                   SETON                                        65
047600160714     C                   MOVEL     MSG(9)        DSMSMS
047700160714     C                   LEAVESR
047800160714     C                   ENDMON
047900160715     C* rivalorizzo il video con la data in ggmmaaaa
048000160715     C                   EVAL      D03DSC = %dec(DSCISO:*eur)
048100970526E   1C                   ENDIF
048200160715     C*---
048300160715     C* la SCADENZA deve essere > DECORRENZA (errore se decorrenza >= scadenza)
048400970526IF  1C     ERRL00        IFEQ      '0'
048500160715IF  2C                   IF        DDEISO >= DSCISO
048600160714     C                   SETON                                        6465
048700970526     C                   MOVEL     MSG(10)       DSMSMS
048800970526     C                   MOVEL     '1'           ERRL00
048900160714     C                   LEAVESR
049000970526E   2C                   ENDIF
049100970526E   1C                   ENDIF
049200160715     C* la SCADENZA deve essere >= DATA CORRENTE (errore se scadeza < oggi)
049300970526IF  1C     ERRL00        IFEQ      '0'
049400970526IF  2C     DEPOP1        IFEQ      'O02'
049500160914     C     DEPOP1        OREQ      'O03'
049600160914     C     DEPOP1        OREQ      'O07'
049700970528     C     DEPOP1        OREQ      'F06'
049800160715IF  2C                   IF        DSCISO < %date()
049900160714     C                   SETON                                        65
050000970526     C                   MOVEL     MSG(11)       DSMSMS
050100970526     C                   MOVEL     '1'           ERRL00
050200160714     C                   LEAVESR
050300970526E   3C                   ENDIF
050400970526E   2C                   ENDIF
050500970526E   1C                   ENDIF
050600160714     C*---
050700160714     C* INSERIMENTO -> RECORD INESISTENTE
050800160714IF  1C     ERRL00        IFEQ      '0'
050900160714IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
051000160914IF  2C     DEPOP1        OREQ      'O03'                                        *INSERIMENTO
051100160714     C                   EVAL      VMSPROC   = D03PROC
051200160714     C                   EVAL      VMSKSU    = D03KSU
051300160831     C                   EVAL      VMSDDE    = D03DDE
051400160831     C                   EVAL      VMSCAUTIP = D03CAUTIP
051500160831     C                   EVAL      VMSCAUBRT = D03CAUBRT
051600160914     C     K05VMS01      CHAIN(N)  TIVMS01I                           99
051700160714IF  3C     *IN99         IFEQ      *OFF
051800160714     C                   MOVEL     '1'           ERRL00
051900160714     C                   MOVEL     MSG(8)        DSMSMS
052000160714     C                   SETON                                        606162
052100160902     C                   SETON                                        636468
052200160714     C                   LEAVESR
052300160714E   3C                   ENDIF
052400160714E   2C                   ENDIF
052500160714E   1C                   ENDIF
052600970526     C*---
052700970526     C* ACCAVALLAMENTO DATI
052800160916     C* casi di accavallamento
052900160916     C*                   **********
053000160916     C*                +++++       |
053100160916     C*                   ++++     |
053200160916     C*                   |  +++++ |
053300160916     C*                   |     ++++
053400160916     C*                   |      ++++++++
053500160916     C*              +++++++++++++++++++
053600160916     C*                   +++++++++++++++
053700160916     C*            +++++++++++++++++
053800160916     C* casi di NON accavallamento
053900160916     C*                   **********
054000160916     C*          +++++++  |        |
054100160916     C*                   |        |  +++++++
054200160916     C*                   ++++++++++
054300160916     C*
054400970526IF  1C     ERRL00        IFEQ      '0'
054500160831     C                   EVAL      VMSPROC   = D03PROC
054600160831     C                   EVAL      VMSKSU    = D03KSU
054700160831     C     K02VMS01      CHAIN(N)  TIVMS01I                           99
054800160831     C* se non esistono rcd con Procedura/Cliente, allora non ci può essere accavallamento
054900160831DO  2C                   DOW       *in99 = *off
055000160714
055100160914     C* se non siamo in inserimento né in copia
055200160914     C* e la decorrenza a video è <> da quella del rcd letto
055300160714IF  3C                   IF        DEPOP1 <> 'F06'                         AND
055400160914    3C                             DEPOP1 <> 'O03'                         AND
055500160715     C                             DDEISO <> %date(VMSDDE)        OR
055600160914     C* oppure siamo in inserimento o copia
055700160914     C                             (DEPOP1 =  'F06' or
055800160914     C                              DEPOP1 =  'O03'   )
055900160714
056000160714IF  4C                   IF        VMSATB = *BLANKS
056100160714
056200160916     C* se non accade quanto sotto, siamo in accavallamento
056300160916     C* se la decorrenza a video è > della scadenza del rcd letto     oppure
056400160916IF  5C                   IF        not(
056500160916IF  5C                             DDEISO > %date(VMSDSC)         OR
056600160916     C*    la scadenza a video è < della decorrenza del rcd letto     oppure
056700160916     C                             DSCISO <= %date(VMSDDE)        OR
056800160916     C*    decorrenza e scadenza a video sono le stesse del rcd letto
056900160916     C                             DDEISO = %date(VMSDDE)     AND
057000160916     C                             DSCISO = %date(VMSDSC)
057100160916     C                             )
057200970526     C                   SETON                                        606162
057300160902     C                   SETON                                        636468
057400970526     C                   MOVEL     MSG(15)       DSMSMS
057500970526     C                   MOVEL     '1'           ERRL00
057600970526     C                   LEAVE
057700970526E   5C                   ENDIF
057800160714
057900970526E   4C                   ENDIF
058000970526E   3C                   ENDIF
058100160831     C     K02VMS01      READE(N)  TIVMS01I                               99
058200970526E   2C                   ENDDO
058300970526E   1C                   ENDIF
058400160718     C*---
058500160718     C* la FASE OPERATIVA può assumere solo questi valori:
058600970526     C*---
058700160831     C                   IF        D03FasOp <> '*ANOM' and
058800160831     C                             D03FasOp <> '*ARR' and
058900160831     C                             D03FasOp <> '*DEST' and
059000160831     C                             D03FasOp <> '*GEN' and
059100160831     C                             D03FasOp <> '*GIAC' and
059200160718     C                             D03FasOp <> '*PART' and
059300160831     C                             D03FasOp <> '*TRANS'
059400160718     C                   SETON                                        67
059500160718     C                   MOVEL     MSG(16)       DSMSMS
059600160718     C                   MOVEL     '1'           ERRL00
059700160718     C                   LEAVESR
059800160718E   1C                   ENDIF
059900160718     C*---
060000970526     C* DESCRIZIONE
060100970526     C*---
060200970526IF  1C     ERRL00        IFEQ      '0'
060300160913IF  2C     D03DESCRIZ    IFEQ      *BLANKS
060400970526     C                   MOVEL     '1'           ERRL00
060500970526     C                   MOVEL     MSG(13)       DSMSMS
060600160714     C                   SETON                                        66
060700160714     C                   LEAVESR
060800970526E   2C                   ENDIF
060900970526E   1C                   ENDIF
061000970526     C*
061100970526     C                   ENDSR
061200970526     C*------------------------------------------------------------------------*
061300970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
061400970526     C*------------------------------------------------------------------------*
061500970526     C     IREVID        BEGSR
061600970526     C* CAMPI
061700160714     C                   EVAL      D03ATB    = VMSATB
061800160714     C                   EVAL      D03PROC   = VMSPROC
061900160714     C                   EVAL      D03KSU    = VMSKSU
062000160831     C                   EVAL      D03CAUTIP = VMSCAUTIP
062100160831     C                   EVAL      D03CAUBRT = VMSCAUBRT
062200160714     C                   EVAL      D03STSCLI = VMSSTSCLI
062300160922     C                   EVAL      D03STSCLI1= %subst(VMSSTSCLI:1:10)
062400160922     C                   EVAL      D03STSCLI2= %subst(VMSSTSCLI:11:5)
062500160715     C                   EVAL      D03DDE    = %dec(%date(VMSDDE):*eur)
062600160714     C                   EVAL      D03CAUTT  = VMSCAUTT
062700170221     C                   EVAL      D03CAUTT1 = %subst(VMSCAUTT:1:10)
062800170221     C                   EVAL      D03CAUTT2 = %subst(VMSCAUTT:11:5)
062900160714     C                   EVAL      D03STSOPZ = VMSSTSOPZ
063000160714     C                   EVAL      D03FASOP  = VMSFASOP
063100160715     C                   EVAL      D03DSC    = %dec(%date(VMSDSC):*eur)
063200160714     C                   EVAL      D03FLO    = VMSFLO
063300160913     C                   EVAL      D03DESCRIZ= VMSDESCRIZ
063400160913     C                   EVAL      D03DESCIT = VMSDESCIT
063500160714     C                   EVAL      D03DESCEN = VMSDESCEN
063600160714     C                   EVAL      D03DESCFR = VMSDESCFR
063700160714     C                   EVAL      D03DESCDE = VMSDESCDE
063800160714     C                   EVAL      D03DESCES = VMSDESCES
063900170523     C                   EVAL      D03DESPROC= VMSDESPROC
064000970526     C*
064100970526     C                   ENDSR
064200970526     C*------------------------------------------------------------------------*
064300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
064400970526     C*------------------------------------------------------------------------*
064500970526     C     AZZVID        BEGSR
064600970526     C*
064700160714     C                   CLEAR                   D03ATB
064800160714     C                   CLEAR                   D03PROC
064900160714     C                   CLEAR                   D03KSU
065000160831     C                   CLEAR                   D03CAUTIP
065100160831     C                   CLEAR                   D03CAUBRT
065200160714     C                   CLEAR                   D03STSCLI
065300160922     C                   CLEAR                   D03STSCLI1
065400160922     C                   CLEAR                   D03STSCLI2
065500160714     C                   CLEAR                   D03DDE
065600160714     C                   CLEAR                   D03CAUTT
065700170221     C                   CLEAR                   D03CAUTT1
065800170221     C                   CLEAR                   D03CAUTT2
065900160714     C                   CLEAR                   D03STSOPZ
066000160714     C                   CLEAR                   D03FASOP
066100160714     C                   CLEAR                   D03DSC
066200160714     C                   CLEAR                   D03FLO
066300160913     C                   CLEAR                   D03DESCRIZ
066400160913     C                   CLEAR                   D03DESCIT
066500160714     C                   CLEAR                   D03DESCEN
066600160714     C                   CLEAR                   D03DESCFR
066700160714     C                   CLEAR                   D03DESCDE
066800160714     C                   CLEAR                   D03DESCES
066900170523     C                   CLEAR                   D03DESPROC
067000970526     C*
067100970526     C                   ENDSR
067200970526     C*------------------------------------------------------------------------*
067300160714     C* IMPVMS - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
067400970526     C*------------------------------------------------------------------------*
067500160714     C     IMPVMS        BEGSR
067600970526     C*
067700160922     C                   EVAL      D03STSCLI = D03STSCLI1 + D03STSCLI2
067800160922     C                   EVAL      VMSSTSCLI = D03STSCLI
067900170221     C                   EVAL      D03CAUTT  = D03CAUTT1 + D03CAUTT2
068000160714     C                   EVAL      VMSCAUTT  = D03CAUTT
068100160714     C                   EVAL      VMSSTSOPZ = D03STSOPZ
068200160714     C                   EVAL      VMSFASOP  = D03FASOP
068300160715     C                   EVAL      VMSDSC    = %dec(DSCISO)
068400160714     C                   EVAL      VMSFLO    = D03FLO
068500160913     C                   EVAL      VMSDESCRIZ= D03DESCRIZ
068600160714     C                   EVAL      VMSDESCIT = D03DESCIT
068700160714     C                   EVAL      VMSDESCEN = D03DESCEN
068800160714     C                   EVAL      VMSDESCFR = D03DESCFR
068900160714     C                   EVAL      VMSDESCDE = D03DESCDE
069000160714     C                   EVAL      VMSDESCES = D03DESCES
069100170523     C                   EVAL      VMSDESPROC= D03DESPROC
069200160831     C                   EVAL      VMSATB    = D03ATB
069300970526     C*
069400970526     C                   ENDSR
069500970526     C*------------------------------------------------------------------------*
069600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
069700970526     C*------------------------------------------------------------------------*
069800970526     C     IMPFUN        BEGSR
069900970526     C*
070000970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
070100970526IF  1C     DEPOP1        IFEQ      'O05'
070200970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
070300970526     C                   MOVEL     'N'           FUDATT
070400970526     C                   MOVEL     FUDRIG        FUD(6)
070500970526E   1C                   ENDIF
070600970526     C*
070700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
070800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
070900970526     C                   EXSR      FUNGES
071000970526     C*
071100970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
071200970526     C*---
071300970526     C* 90 -> PROTEZIONE CHIAVE
071400970526     C* 91 -> PROTEZIONE TOTALE
071500970526     C*---
071600970526IF  1C     DEPOP0        IFEQ      'R02'                                        G E S T I O N E
071700970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
071800160914IF  2C     DEPOP1        OREQ      'O03'                                        *COPIA
071900970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
072000970526     C                   SETOFF                                       91
072100970526E   2C                   ENDIF
072200970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
072300970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
072400970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
072500970526E   2C                   ENDIF
072600970526     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
072700970526IF  2C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
072800970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
072900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
073000970526     C                   SETON                                        91
073100970526E   2C                   ENDIF
073200970526X   1C                   ELSE                                                   *I N T E R R O G A
073300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
073400970526     C                   SETON                                        91
073500970526E   1C                   ENDIF
073600970526     C*
073700970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
073800160914     C* LETTURA DEL RECORD DA COPIARE -NO IN INSERIMENTO-
073900970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
074000160714     C                   EVAL      VMSPROC   = VMS2PROC
074100160714     C                   EVAL      VMSKSU    = VMS2KSU
074200160913     C                   EVAL      VMSDDE    = %dec(VMS2DDE:8:0)
074300160831     C                   EVAL      VMSCAUTIP = VMS2CAUTIP
074400160831     C                   EVAL      VMSCAUBRT = VMS2CAUBRT
074500160914     C     K05VMS01      CHAIN(N)  TIVMS01I                           99
074600970526IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
074700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
074800970526     C                   MOVEL     MSG(6)        DSMSMS
074900970526X   2C                   ELSE
075000970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
075100980128IF  3C     DEPOP1        IFEQ      'O05'
075200980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
075300980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
075400980128E   3C                   ENDIF
075500970526E   2C                   ENDIF
075600970526X   1C                   ELSE                                                   *INSERIMENTO
075700970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
075800970526E   1C                   ENDIF
075900970526     C*
076000970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
076100970526IF  1C     DEPOP1        IFEQ      'O02'
076200970526     C                   MOVEL     DEC(1)        VIDFUN
076300970526E   1C                   ENDIF
076400160914IF  1C     DEPOP1        IFEQ      'O03'
076500160914     C                   MOVEL     DEC(6)        VIDFUN
076600160914E   1C                   ENDIF
076700970526IF  1C     DEPOP1        IFEQ      'O04'
076800970526     C                   MOVEL     DEC(2)        VIDFUN
076900970526E   1C                   ENDIF
077000970526IF  1C     DEPOP1        IFEQ      'O05'
077100970526     C                   MOVEL     DEC(3)        VIDFUN
077200970526E   1C                   ENDIF
077300970526IF  1C     DEPOP1        IFEQ      'O07'
077400970526     C                   MOVEL     DEC(4)        VIDFUN
077500970526E   1C                   ENDIF
077600970526IF  1C     DEPOP1        IFEQ      'F06'
077700970526     C                   MOVEL     DEC(5)        VIDFUN
077800970526E   1C                   ENDIF
077900970526     C*
078000970526     C                   ENDSR
078100970526     C*------------------------------------------------------------------------*
078200970526     C* *INZSR - OPERAZIONI INIZIALI
078300970526     C*------------------------------------------------------------------------*
078400970526     C     *INZSR        BEGSR
078500970526     C*---
078600970526     C* RICEVIMENTO PARAMETRI
078700970526     C*---
078800970526     C     *ENTRY        PLIST
078900970612     C                   PARM                    KPJBA
079000160714     C                   PARM                    TIS7VMS2DS
079100970526     C*
079200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
079300970526     C     XMSGPA        PLIST
079400970526     C                   PARM                    DSMSMS
079500970526     C                   PARM                    DSMSPG
079600970526     C                   PARM                    DSMSKE
079700970526     C                   PARM                    DSMSER
079800970526     C*
079900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
080000970526     C                   CALL      'XKEY'
080100970526     C                   PARM                    DSKEY
080200970526     C*---
080300970526     C* VARIABILI NON RIFERITE AL DATA BASE
080400970526     C*---
080500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
080600160714     C     *LIKE         DEFINE    VMS2OP0       DEPOP0                         *DI DEPOSITO
080700160714     C     *LIKE         DEFINE    VMS2OP1       DEPOP1
080800160714     C     *LIKE         DEFINE    VMS2OP0       WOP0                           *DI LAVORO
080900160714     C     *LIKE         DEFINE    VMS2OP1       WOP1
081000970526     C*---
081100970526     C* VARIABILI DI CONTROLLO
081200970526     C*---
081300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
081400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
081500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
081600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
081700970526     C*
081800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
081900160714     C                   MOVEL     'TIS7VMS2R'   DSMSPG
082000160714     C                   MOVEL     'TIS7VMS2R'   MSGQUE
082100970526     C                   MOVEL     *BLANKS       MSGKEY
082200970526     C                   MOVEL     *BLANKS       DSMSMS
082300970526     C*---
082400970526     C* VARIABILI RIFERITE AL DATA BASE
082500970526     C*---
082600970526     C* CHIAVI DI LETTURA
082700970526     C*---
082800160714     C* LETTURA TIVMS01I - COMPLETA
082900160914     C     K05VMS01      KLIST
083000160714     C                   KFLD                    VMSProc
083100160714     C                   KFLD                    VMSKSU
083200160831     C                   KFLD                    VMSDDE
083300160831     C                   KFLD                    VMSCauTip
083400160831     C                   KFLD                    VMSCauBRT
083500160831     C* LETTURA TIVMS01I - parziale (per ctrl accavallamento)
083600160831     C     K02VMS01      KLIST
083700160831     C                   KFLD                    VMSProc
083800160831     C                   KFLD                    VMSKSU
083900970526     C*
084000970526     C* CALCOLA LA DATA CORRENTE
084100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
084200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
084300970526     C                   Z-ADD     WN8           G08DAT
084400970526     C                   Z-ADD     *ZEROS        G08INV
084500970526     C                   MOVEL     '0'           G08ERR
084600970526     C                   CALL      'XSRDA8'
084700970526     C                   PARM                    WLBDA8
084800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
084900970526     C*
085000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
085100160714     C                   MOVEL     VMS2OP0       DEPOP0                         *LIVELLO PROCEDURA
085200160714     C                   MOVEL     VMS2OP1       DEPOP1                         *LIVELLO PROGRAMMA
085300970526     C*
085400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
085500970526     C                   EXSR      IMPFUN
085600970526     C*
085700970526     C                   ENDSR
085800970526     C*------------------------------------------------------------------------*
085900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
086000970526     C*------------------------------------------------------------------------*
086100970526     C     FUNGES        BEGSR
086200970526     C*
086300970526     C* PULISCO CAMPI DI LAVORO
086400970526     C                   Z-ADD     0             W                 5 0
086500970526     C*
086600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
086700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
086800970526     C     1             DO        24            I
086900970526     C                   MOVEL     FUD(I)        FUDRIG
087000970526     C*
087100970526     C* PULIZIA FUNZIONI PRECEDENTI
087200970526     C                   MOVEL     *BLANKS       FUV(I)
087300970526     C*
087400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
087500970526     C* (SALTA F24)
087600970526     C     FUDATT        IFEQ      'S'
087700970526     C     FUDFUN        ANDNE     '24'
087800970526     C                   ADD       1             W
087900970526     C                   MOVEL     FUDDES        FUV(W)
088000970526     C                   END
088100970526     C                   END                                                    DO WHILE
088200970526     C*
088300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
088400970526     C* ALTRIMENTI DISATTIVO F24
088500970526     C     1             DO        24            I
088600970526     C                   MOVEL     FUD(I)        FUDRIG
088700970526     C*
088800970526     C     FUDFUN        IFEQ      '24'
088900970526     C     W             IFGT      6
089000970526     C                   MOVEL     'S'           FUDATT
089100970526     C                   ELSE
089200970526     C                   MOVEL     *BLANKS       FUDATT
089300970526     C                   END                                                    W GT 8
089400970526     C                   MOVEL     FUDRIG        FUD(I)
089500970526     C                   END
089600970526     C                   END                                                    DO WHILE
089700970526     C*
089800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
089900970526     C     W             DIV       6             I
090000970526     C                   MVR                     Z
090100970526     C*
090200970526     C     Z             IFNE      0
090300970526     C     I             ADD       1             Z
090400970526     C                   ELSE
090500970526     C                   Z-ADD     I             Z                 5 0
090600970526     C                   END
090700970526     C*
090800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
090900970526     C                   ADD       1             FUNGRU
091000970526     C     FUNGRU        IFGT      Z
091100970526     C     FUNGRU        ORLT      1
091200970526     C                   Z-ADD     1             FUNGRU
091300970526     C                   END
091400970526     C*
091500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
091600970526     C     FUNGRU        IFEQ      1
091700970526     C                   MOVEL     FUV(1)        FUN001
091800970526     C                   MOVEL     FUV(2)        FUN002
091900970526     C                   MOVEL     FUV(3)        FUN003
092000970526     C                   MOVEL     FUV(4)        FUN004
092100970526     C                   MOVEL     FUV(5)        FUN005
092200970526     C                   MOVEL     FUV(6)        FUN006
092300970526     C                   END
092400970526     C*
092500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
092600970526     C     FUNGRU        IFEQ      2
092700970526     C                   MOVEL     FUV(7)        FUN001
092800970526     C                   MOVEL     FUV(8)        FUN002
092900970526     C                   MOVEL     FUV(9)        FUN003
093000970526     C                   MOVEL     FUV(10)       FUN004
093100970526     C                   MOVEL     FUV(11)       FUN005
093200970526     C                   MOVEL     FUV(12)       FUN006
093300970526     C                   END
093400970526     C*
093500970526     C* TERZO GRUPPO DI 6 FUNZIONI
093600970526     C     FUNGRU        IFEQ      3
093700970526     C                   MOVEL     FUV(13)       FUN001
093800970526     C                   MOVEL     FUV(14)       FUN002
093900970526     C                   MOVEL     FUV(15)       FUN003
094000970526     C                   MOVEL     FUV(16)       FUN004
094100970526     C                   MOVEL     FUV(17)       FUN005
094200970526     C                   MOVEL     FUV(18)       FUN006
094300970526     C                   END
094400970526     C*
094500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
094600970526     C     FUNGRU        IFEQ      4
094700970526     C                   MOVEL     FUV(19)       FUN001
094800970526     C                   MOVEL     FUV(20)       FUN002
094900970526     C                   MOVEL     FUV(21)       FUN003
095000970526     C                   MOVEL     FUV(22)       FUN004
095100970526     C                   MOVEL     *BLANKS       FUN005
095200970526     C                   MOVEL     *BLANKS       FUN006
095300970526     C                   END
095400970526     C*
095500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
095600970526     C                   MOVEL     *BLANKS       FUN007
095700970526     C                   MOVEL     *BLANKS       FUN008
095800970526     C*
095900970526     C     1             DO        24            I
096000970526     C                   MOVEL     FUD(I)        FUDRIG
096100970526     C     FUDFUN        IFEQ      '24'
096200970526     C     FUDATT        ANDEQ     'S'
096300970526     C                   MOVEL     FUDDES        FUN008
096400970526     C                   END
096500970526     C                   END                                                    DO
096600970526     C*
096700970526     C                   ENDSR
096800970526     C*------------------------------------------------------------------------*
096900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
097000970526     C*------------------------------------------------------------------------*
097100970526     C     FUNCON        BEGSR
097200970526     C*
097300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
097400970526     C                   EXSR      FUNVAR
097500970526     C*
097600970526     C* PULISCO CAMPI DI LAVORO
097700970526     C                   Z-ADD     0             I                 5 0
097800970526     C                   Z-ADD     0             W                 5 0
097900970526     C*
098000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
098100970526     C     FUNKEY        IFNE      *BLANKS
098200970526     C     1             DO        24            I
098300970526     C                   MOVEL     FUD(I)        FUDRIG
098400970526     C*
098500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
098600970526     C     FUNKEY        IFEQ      FUDFUN
098700970526     C     FUDATT        ANDEQ     'S'
098800970526     C                   Z-ADD     I             W
098900970526     C                   END
099000970526     C                   END                                                    DO WHILE
099100970526     C*
099200970526     C* FUNZIONE NON ATTIVA (ERRORE)
099300970526     C     W             IFEQ      0
099400970526     C*
099500970526     C* EMISSIONE MESSAGGIO DI ERRORE
099600970526     C                   MOVEL     MSG(1)        DSMSMS
099700970526     C*
099800970526     C                   END                                                    W=0
099900970526     C                   END                                                    FUNKEY NE *BLANKS
100000970526     C*
100100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
100200970526     C     W             IFNE      0
100300970526     C     FUNKEY        OREQ      *BLANKS
100400970526     C     FUNKEY        IFEQ      *BLANKS
100500970526     C                   EXSR      ENTGES
100600970526     C                   END
100700970526     C     FUNKEY        IFEQ      '03'
100800970526     C                   EXSR      F03GES
100900970526     C                   END
101000970526     C     FUNKEY        IFEQ      '06'
101100970526     C                   EXSR      F06GES
101200970526     C                   END
101300160715     C     FUNKEY        IFEQ      '12'
101400160715     C                   EXSR      F12GES
101500160715     C                   END
101600970526     C     FUNKEY        IFEQ      '24'
101700970526     C                   EXSR      F24GES
101800970526     C                   END
101900970526     C                   END
102000970526     C*
102100970526     C                   ENDSR
102200970526     C*------------------------------------------------------------------------*
102300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
102400970526     C*------------------------------------------------------------------------*
102500970526     C     FUNVAR        BEGSR
102600970526     C*
102700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
102800970526     C*
102900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
103000970526     C     KEY           IFEQ      ENTER
103100970526     C                   MOVEL     *BLANKS       FUNKEY
103200970526     C                   END
103300970526     C     KEY           IFEQ      F1
103400970526     C                   MOVEL     '01'          FUNKEY
103500970526     C                   END
103600970526     C     KEY           IFEQ      F2
103700970526     C                   MOVEL     '02'          FUNKEY
103800970526     C                   END
103900970526     C     KEY           IFEQ      F3
104000970526     C                   MOVEL     '03'          FUNKEY
104100970526     C                   END
104200970526     C     KEY           IFEQ      F4
104300970526     C                   MOVEL     '04'          FUNKEY
104400970526     C                   END
104500970526     C     KEY           IFEQ      F5
104600970526     C                   MOVEL     '05'          FUNKEY
104700970526     C                   END
104800970526     C     KEY           IFEQ      F6
104900970526     C                   MOVEL     '06'          FUNKEY
105000970526     C                   END
105100970526     C     KEY           IFEQ      F7
105200970526     C                   MOVEL     '07'          FUNKEY
105300970526     C                   END
105400970526     C     KEY           IFEQ      F8
105500970526     C                   MOVEL     '08'          FUNKEY
105600970526     C                   END
105700970526     C     KEY           IFEQ      F9
105800970526     C                   MOVEL     '09'          FUNKEY
105900970526     C                   END
106000970526     C     KEY           IFEQ      F10
106100970526     C                   MOVEL     '10'          FUNKEY
106200970526     C                   END
106300970526     C     KEY           IFEQ      F11
106400970526     C                   MOVEL     '11'          FUNKEY
106500970526     C                   END
106600970526     C     KEY           IFEQ      F12
106700970526     C                   MOVEL     '12'          FUNKEY
106800970526     C                   END
106900970526     C     KEY           IFEQ      F13
107000970526     C                   MOVEL     '13'          FUNKEY
107100970526     C                   END
107200970526     C     KEY           IFEQ      F14
107300970526     C                   MOVEL     '14'          FUNKEY
107400970526     C                   END
107500970526     C     KEY           IFEQ      F15
107600970526     C                   MOVEL     '15'          FUNKEY
107700970526     C                   END
107800970526     C     KEY           IFEQ      F16
107900970526     C                   MOVEL     '16'          FUNKEY
108000970526     C                   END
108100970526     C     KEY           IFEQ      F17
108200970526     C                   MOVEL     '17'          FUNKEY
108300970526     C                   END
108400970526     C     KEY           IFEQ      F18
108500970526     C                   MOVEL     '18'          FUNKEY
108600970526     C                   END
108700970526     C     KEY           IFEQ      F19
108800970526     C                   MOVEL     '19'          FUNKEY
108900970526     C                   END
109000970526     C     KEY           IFEQ      F20
109100970526     C                   MOVEL     '20'          FUNKEY
109200970526     C                   END
109300970526     C     KEY           IFEQ      F21
109400970526     C                   MOVEL     '21'          FUNKEY
109500970526     C                   END
109600970526     C     KEY           IFEQ      F23
109700970526     C                   MOVEL     '23'          FUNKEY
109800970526     C                   END
109900970526     C     KEY           IFEQ      F24
110000970526     C                   MOVEL     '24'          FUNKEY
110100970526     C                   END
110200970526     C     KEY           IFEQ      ROLLUP
110300970526     C                   MOVEL     'RU'          FUNKEY
110400970526     C                   END
110500970526     C     KEY           IFEQ      HELP
110600970526     C                   MOVEL     'HP'          FUNKEY
110700970526     C                   END
110800970526     C                   ENDSR
110900970526     C*------------------------------------------------------------------------*
111000970526     C* NOTE COSTRUZIONE SCHIERE
111100970526     C*------------------------------------------------------------------------*
111200970526     C*
111300970526     C* -MSG-MESSAGGI
111400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
111500970526     C* (NON TOCCARE MSG,1 E MSG,2)
111600970526     C*
111700970526     C* -CMD-COMANDI
111800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
111900970526     C*
112000970526     C* -OPD-OPZIONI
112100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
112200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
112300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
112400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
112500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
112600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
112700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
112800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
112900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
113000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
113100970526     C*
113200970526     C* -FUD-FUNZIONI
113300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
113400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
113500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
113600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
113700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
113800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
113900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
114000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
114100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
114200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
114300970526     O*------------------------------------------------------------------------*
114400970526** DEC - DECODIFICHE
114500970526    MODIFICA                                                      1
114600970526  ANNULLAMENTO                                                    2
114700970526VISUALIZZAZIONE                                                   3
114800970526   RIPRISTINO                                                     4
114900970526  INSERIMENTO                                                     5
115000160914     COPIA                                                        6
115100970526** MSG - MESSAGGI
115200160714TIS7VMS2R- Tasto funzionale non gestito                                        1
115300160714TIS7VMS2R- Inserimento effettuato                                              2
115400160714TIS7VMS2R- Modifica effettuata                                                 3
115500160714TIS7VMS2R- Annullamento effettuato                                             4
115600160714TIS7VMS2R- Ripristino effettuato                                               5
115700160714TIS7VMS2R- Richiesta funzione per record inesistente                           6
115800160714TIS7VMS2R- Record momentaneamente allocato: riprovare più tardi                7
115900160714TIS7VMS2R- Richiesta funzione per record esistente                             8
116000160714TIS7VMS2R- Data errata                                                         9
116100160714TIS7VMS2R- La data scadenza deve essere maggiore della decorrenza             10
116200160715TIS7VMS2R- La procedura non può scadere in un giorno già trascorso
116300160914TIS7VMS2R- Copia effettuata                                                   12
116400160714TIS7VMS2R- Descrizione obbligatoria                                           13
116500160714TIS7VMS2R- Campo obbligatorio                                                 14
116600160831TIS7VMS2R- Il periodo di decorrenza si accavalla con un altro                 15
116700160718TIS7VMS2R- Fase Operativa errata                                              16
116800160714TIS7VMS2R- 17
116900160714TIS7VMS2R- 18
117000160714TIS7VMS2R- 19
117100160714TIS7VMS2R- 20
117200970526** FUD - FUNZIONI
117300970526                   -        (S/N)=N                        funz.01¬
117400970526                   -        (S/N)=N                        funz.02¬
117500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
117600970526                   -        (S/N)=N                        funz.04¬
117700970526                   -        (S/N)=N                        funz.05¬
117800970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
117900970526                   -        (S/N)=N                        funz.07¬
118000160715                   -        (S/N)=N                        funz.08¬
118100160715                   -        (S/N)=N                        funz.09¬
118200160715                   -        (S/N)=N                        funz.10¬
118300160715                   -        (S/N)=N                        funz.11¬
118400970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
118500160715                   -        (S/N)=N                        funz.13¬
118600970526                   -        (S/N)=N                        funz.14¬
118700970526                   -        (S/N)=N                        funz.15¬
118800970526                   -        (S/N)=N                        funz.16¬
118900970526                   -        (S/N)=N                        funz.17¬
119000970526                   -        (S/N)=N                        funz.18¬
119100970526                   -        (S/N)=N                        funz.19¬
119200970526                   -        (S/N)=N                        funz.20¬
119300970526                   -        (S/N)=N                        funz.21¬
119400970526                   -        (S/N)=N                        funz.22¬
119500970526                   -        (S/N)=N                        funz.23¬
119600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
