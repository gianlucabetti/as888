000100000210     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200000218     H*PARMS ACTGRP(*CALLER)
000300000000     H*------------------------------------------------------------------------*
000400000000     H* GESTIONE TRAINI - LISTINI - SUBFILE DI SCELTA
000500010822
000600010822      * ATTENZIONE: questo programma era il vecchio TNTL10R
000700010822
000800000000     H*------------------------------------------------------------------------*
000900000000     H DECEDIT('0,') DATEDIT(*DMY.)
001000000000     F*------------------------------------------------------------------------*
001100000000     F* DATA BASE
001200000000     F*------------------------------------------------------------------------*
001300010723     FTNTL38D   CF   E             WORKSTN INFDS(DEVDS1)
001400010723     F                                     SFILE(TL38DVS:SFLULT)
001500010723     F                                     SFILE(TL38DVM:MSGULT)
001600000000     FTABEL00F  IF   E           K DISK
001700000821     FANATB01L  IF   E           K DISK     USROPN
001800000000     FTNTLZ01L  IF   E           K DISK
001900000821     FTNTLT01L  UF A E           K DISK    COMMIT
002000010723     FTITLL01L  UF A E           K DISK    COMMIT
002100000000     D*------------------------------------------------------------------------*
002200000000     D* SCHIERE
002300000000     D*------------------------------------------------------------------------*
002400000000     D*-------------------
002500000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002600000000     D*-------------------
002700000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
002800000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
002900000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
003000000000     D FUN             S             40    DIM(31)
003100000000     D*-------------------
003200000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003300000000     D*-------------------
003400000000     D FFU             S              6    DIM(31)
003500000000     D FDE             S             19    DIM(31)
003600000000     D FAT             S              1    DIM(31)
003700000000     D*-------------------
003800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003900000000     D*-------------------
004000000000     D OPD             S              2    DIM(24)
004100000000     D OPS             S              2    DIM(24)
004200000000     D ODE             S             19    DIM(24)
004300000000     D OAT             S              1    DIM(24)
004400000000     D*-------------------
004500000000     D* TABELLA DESCRIZIONI A VIDEO
004600000000     D*-------------------
004700000000     D FUV             S             19    DIM(8)
004800000000     D OPV             S             19    DIM(8)
004900000000     D*-------------------
005000000000     D* TABELLA TASTI FUNZIONALI PREMUTI
005100000000     D*-------------------
005200000000     D KEYS            S              1    DIM(40)
005300000000     D*-------------------
005400000000     D* TIPI AUTOMEZZO
005500000000     D*-------------------
005600000000     D TMZ             S              2    DIM(20)
005700000000     D DTMZ            S             30    DIM(20)
005800000000     D*-------------------
005900000000     D* CAUSALI CONTABILIZZAZIONE
006000000000     D*-------------------
006100000000     D CAU             S              1    DIM(20)
006200000000     D FLG             S              1    DIM(20)
006300000000     D DCAU            S             30    DIM(20)
006400960424     D*-------------------
006500960424     D* SCHIERE ALFA/NUMERICHE DI LAVORO
006600960424     D*-------------------
006700960424     D SA0             S              1    DIM(10)
006800000000     D*------------------------------------------------------------------------*
006900000000     D* INPUT
007000000000     D*------------------------------------------------------------------------*
007100000000     D*---------------------
007200000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
007300000000     D*---------------------
007400000000     D DSOPZ         E DS
007500000000     D  OPZRIG                 1     40
007600000000     D*---------------------
007700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
007800000000     D*---------------------
007900000000     D DSFUN         E DS
008000000000     D  FUNRIG                 1     40
008100000000     D*---------------------
008200000000     D* DS PER GESTIONE TASTI FUNZIONALI
008300000000     D*---------------------
008400000000     D DSKEYS        E DS
008500000000     D  KEYRIG                 1     40
008600000000     D*---------------------
008700000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
008800000000     D*---------------------
008900000000     D                SDS
009000000000     D  USER                 254    263
009100000000     D*---------------------
009200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
009300000000     D*---------------------
009400000000     D DEVDS1          DS
009500000000     D  KEY                  369    369
009600000000     D  CURSOR               370    371
009700000000     D  FRTSFL               378    379B 0
009800000000     D*---------------------
009900000000     D* ARCHITETTURA
010000000000     D*---------------------
010100000000     D KPJBA         E DS
010200000000     D*---------------------
010300000000     D* DS ESTERNA DI PROCEDURA
010400000000     D*---------------------
010500000000     D DS00          E DS                  EXTNAME(TNTL00DS)
010600000000     D*---------------------
010700000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
010800000000     D*---------------------
010900000000     D DSMSG         E DS
011000000000     D*---------------------
011100000000     D* DS TIPI TRAINO
011200000000     D*---------------------
011300000000     D DSTV          E DS
011400000000     D*---------------------
011500000000     D* DS TIPI AUTOMEZZI
011600000000     D*---------------------
011700000000     D DSTU          E DS
011800000000     D*---------------------
011900000000     D* DS CAUSALI CONTABILIZZAZIONE
012000000000     D*---------------------
012100000210     D ANGY4ZDS      E DS
012200020716
012300020716     d DdatiUte      e ds
012400020716     d AzuteDs       e ds                  ExtName(Azute00f)
012500020716     d Tibs34Ds      e ds                  Inz
012600020716
012700000000     D*------------------
012800000000     D* DS CONTROLLO DATA (8)
012900000000     D*------------------
013000000000     D WLBDA8          DS                  INZ
013100000000     D  G08DAT                 1      8  0
013200000000     D  G08INV                 9     16  0
013300000000     D  G08ERR                17     17
013400000000     D  G08TGI                18     22  0
013500000000     D*------------------
013600000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
013700000000     D*------------------
013800000000     D* MENU
013900000000     D DESGES          C                   CONST('** GESTIONE **')
014000000000     D DESINT          C                   CONST('INTERROGAZIONE')
014100000000     D* COMMENTI
014200000000     D DESATB          C                   CONST('  ANNULLATO ')
014300000000     D DES999          C                   CONST('DI CARTELLO')
014400000000     C*------------------------------------------------------------------------*
014500951201     C* MAIN LINE
014600000000     C*------------------------------------------------------------------------*
014700000000     C*
014800000000     C* CICLO FINO A CHE:
014900000000DO  1C     $FINE         DOUEQ     '1'
015000000000     C*
015100000000     C                   MOVEL     '0'           $ERR
015200000000     C*
015300000000     C* CONTROLLA GESTIONE VIDEO
015400000000IF  2C     $GES          IFEQ      'VS'
015500000000     C                   EXSR      GESVS
015600000000E   2C                   ENDIF
015700000000     C*
015800000000E   1C                   ENDDO
015900000000     C*
016000000000IF  1C     $F03          IFEQ      '1'
016100000000     C                   MOVEL     '1'           D00F03
016200000000E   1C                   ENDIF
016300000000IF  1C     $F12          IFEQ      '1'
016400000000     C                   MOVEL     '1'           D00F12
016500000000E   1C                   ENDIF
016600000000     C* FINE PGM
016700000000     C                   MOVEL     *BLANKS       D00MSG
016800000000     C                   MOVEL     DS00          KPJBU
016900000000     C                   SETON                                        LR
017000000000     C*------------------------------------------------------------------------*
017100000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
017200000000     C*------------------------------------------------------------------------*
017300000000     C     CARVS         BEGSR
017400000000     C*
017500000000     C                   MOVEL     'VS'          $GES
017600000000     C*
017700000000     C                   EXSR      CAROPZ
017800000000     C                   EXSR      OUTOPZ
017900000000     C                   MOVEA     FUN2          FUN
018000000000     C                   EXSR      CARFUN
018100000000     C                   EXSR      OUTFUN
018200000000     C*
018300010723     C                   WRITE     TL38DVT
018400010723     C                   WRITE     TL38DVO
018500010723     C                   WRITE     TL38DVF
018600000000     C*
018700000000     C                   EXSR      POSSFL
018800000000     C*
018900000000     C                   ENDSR
019000000000     C*------------------------------------------------------------------------*
019100000000     C* GESVS  - GESTIONE SUBFILE
019200000000     C*------------------------------------------------------------------------*
019300000000     C     GESVS         BEGSR
019400000000     C*
019500000000IF  1C     $TESVD        IFEQ      '1'
019600010723     C                   WRITE     TL38DVT
019700010723     C                   WRITE     TL38DVO
019800010723     C                   WRITE     TL38DVF
019900000000E   1C                   ENDIF
020000000000     C*
020100000000     C                   EXSR      OUTSFL
020200000000     C*
020300000000     C                   EXSR      CONFUN
020400000000     C*
020500000000IF  1C     $ERR          IFEQ      '0'
020600000000SL  2C                   SELECT
020700000000     C     KEY           WHENEQ    F10
020800000000     C                   EXSR      F10GES
020900000000     C                   OTHER
021000000000     C                   EXSR      FUNSFL
021100000000E   2C                   ENDSL
021200000000E   1C                   ENDIF
021300000000     C*
021400000000     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000000     C* F10GES - GESTIONE TASTO FUNZIONALE F10 ==> INSERIMENTO
021700000000     C*------------------------------------------------------------------------*
021800000000     C     F10GES        BEGSR
021900000000     C*
022000000000     C* LANCIA L'INSERIMENTO
022100000000     C                   Z-ADD     D00TRN        WTRN
022200000000     C                   Z-ADD     D00DDE        WDDE
022300000000     C*
022400000000     C                   MOVEL     *BLANKS       WTMZ
022500000000     C                   Z-ADD     *ZEROS        WPDR
022600000000     C*
022700000000     C                   MOVEL     D00OP0        WOP0
022800000000     C                   MOVEL     'F10'         WOP1
022900010724     C                   MOVEL     'TNTL39R'     WPGM              8
023000000000     C                   EXSR      LANPGM
023100000000     C                   MOVEL     *BLANKS       VSCOPZ
023200000000     C*
023300000000     C                   EXSR      POSSFL
023400000000     C*
023500000000     C                   ENDSR
023600000000     C*------------------------------------------------------------------------*
023700000000     C* GESOPZ - GESTIONE OPZIONI
023800000000     C*------------------------------------------------------------------------*
023900000000     C     GESOPZ        BEGSR
024000000000     C*
024100000000SL  1C                   SELECT
024200000000     C     VSCOPZ        WHENEQ    ' 2'
024300000000     C                   EXSR      O02GES
024400000000     C     VSCOPZ        WHENEQ    ' 4'
024500000000     C                   EXSR      O04GES
024600000000     C     VSCOPZ        WHENEQ    ' 7'
024700000000     C                   EXSR      O07GES
024800000000     C                   OTHER
024900000000     C                   MOVEL     MSG(4)        $MSG
025000000000     C                   EXSR      CARMSG
025100000000E   1C                   ENDSL
025200000000     C*
025300000000     C                   ENDSR
025400000000     C*------------------------------------------------------------------------*
025500000000     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
025600000000     C*------------------------------------------------------------------------*
025700000000     C     O02GES        BEGSR
025800000000     C*
025900000000IF  1C     VSCATB        IFNE      *BLANKS
026000000000     C                   MOVEL     '1'           $ERR
026100000000     C                   MOVEL     MSG(29)       $MSG
026200000000     C                   EXSR      CARMSG
026300000000E   1C                   ENDIF
026400000000     C*
026500000000     C* LANCIA LA MODIFICA
026600000000IF  1C     $ERR          IFEQ      '0'
026700000000     C                   Z-ADD     D00TRN        WTRN
026800000000     C                   Z-ADD     D00DDE        WDDE
026900000000     C*
027000000000     C                   MOVEL     VSCTMZ        WTMZ
027100000000     C                   Z-ADD     VSCPDR        WPDR
027200000000     C*
027300000000     C                   MOVEL     D00OP0        WOP0
027400000000     C                   MOVEL     'O02'         WOP1
027500010724     C                   MOVEL     'TNTL39R'     WPGM
027600000000     C                   EXSR      LANPGM
027700000000     C*
027800000000E   1C                   ENDIF
027900951130     C                   MOVEL     '1'           $ERR
028000000000     C*
028100000000     C                   ENDSR
028200000000     C*------------------------------------------------------------------------*
028300000000     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
028400000000     C*------------------------------------------------------------------------*
028500000000     C     O04GES        BEGSR
028600000000     C*
028700000000IF  1C     VSCATB        IFNE      *BLANKS
028800000000     C                   MOVEL     '1'           $ERR
028900000000     C                   MOVEL     MSG(29)       $MSG
029000000000     C                   EXSR      CARMSG
029100000000E   1C                   ENDIF
029200000000     C*
029300000000     C* LANCIA L'ANNULLAMENTO
029400000000IF  1C     $ERR          IFEQ      '0'
029500000000     C                   Z-ADD     D00TRN        WTRN
029600000000     C                   Z-ADD     D00DDE        WDDE
029700000000     C*
029800000000     C                   MOVEL     VSCTMZ        WTMZ
029900000000     C                   Z-ADD     VSCPDR        WPDR
030000000000     C*
030100000000     C                   MOVEL     D00OP0        WOP0
030200000000     C                   MOVEL     'O04'         WOP1
030300010724     C                   MOVEL     'TNTL39R'     WPGM
030400000000     C                   EXSR      LANPGM
030500000000     C*
030600000000E   1C                   ENDIF
030700951130     C                   MOVEL     '1'           $ERR
030800000000     C*
030900000000     C                   ENDSR
031000000000     C*------------------------------------------------------------------------*
031100000000     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
031200000000     C*------------------------------------------------------------------------*
031300000000     C     O07GES        BEGSR
031400000000     C*
031500000000IF  1C     VSCATB        IFEQ      *BLANKS
031600000000     C                   MOVEL     '1'           $ERR
031700000000     C                   MOVEL     MSG(30)       $MSG
031800000000     C                   EXSR      CARMSG
031900000000E   1C                   ENDIF
032000000000     C*
032100000000     C* LANCIA IL RIPRISTINO
032200000000IF  1C     $ERR          IFEQ      '0'
032300000000     C                   Z-ADD     D00TRN        WTRN
032400000000     C                   Z-ADD     D00DDE        WDDE
032500000000     C*
032600000000     C                   MOVEL     VSCTMZ        WTMZ
032700000000     C                   Z-ADD     VSCPDR        WPDR
032800000000     C*
032900000000     C                   MOVEL     D00OP0        WOP0
033000000000     C                   MOVEL     'O07'         WOP1
033100010724     C                   MOVEL     'TNTL39R'     WPGM
033200000000     C                   EXSR      LANPGM
033300000000     C*
033400000000E   1C                   ENDIF
033500951130     C                   MOVEL     '1'           $ERR
033600000000     C*
033700000000     C                   ENDSR
033800000000     C*------------------------------------------------------------------------*
033900000000     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
034000000000     C*------------------------------------------------------------------------*
034100000000     C     NEWPOS        BEGSR
034200000000     C*
034300000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
034400000000     C*        1 = RICHIESTO POSIZIONAMENTO
034500000000     C                   Z-ADD     0             SWPOSI            7 0
034600000000     C*
034700000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
034800000000     C*            NON OCCORRE RICARICARE IL SUBFILE
034900000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
035000000000     C                   Z-ADD     0             SWMODI            1 0
035100000000     C*
035200000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLA I PARAMETRI
035300000000     C                   EXSR      CHKPAR
035400000000     C*
035500000000     C                   ENDSR
035600000000     C*------------------------------------------------------------------------*
035700000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
035800000000     C*------------------------------------------------------------------------*
035900000000     C     CHKPAR        BEGSR
036000000000     C*
036100000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
036200000000     C* CURSORE SUL CAMPO DEL CONTROL
036300000000IF  1C     $ERR          IFEQ      '1'
036400000000     C                   MOVEL     '0'           *IN43
036500000000X   1C                   ELSE
036600000000     C                   MOVEL     '1'           *IN43
036700000000E   1C                   ENDIF
036800000000     C*
036900000000     C                   Z-ADD     1             SWMODI
037000000000     C*
037100000000     C                   ENDSR
037200000000     C*------------------------------------------------------------------------*
037300000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
037400000000     C*------------------------------------------------------------------------*
037500000000     C     SETFIL        BEGSR
037600000000     C*
037700000000     C                   MOVEL     '1'           *IN43
037800000000     C*
037900000000     C                   Z-ADD     D00TRN        KLLTRN
038000000000     C                   Z-ADD     D00DDE        KLLDDE
038100010723     C     KEYTLL        SETLL     TITLL01L                           99
038200000000     C*
038300000000     C  N99              EXSR      LETFIL
038400000000     C*
038500000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
038600000000IF  1C     *IN99         IFEQ      *ON
038700000000     C                   Z-ADD     1             SFLEOF
038800000000E   1C                   ENDIF
038900000000     C*
039000000000     C                   ENDSR
039100000000     C*------------------------------------------------------------------------*
039200000000     C* LETFIL - LETTURA RECORD SUCESSIVO
039300000000     C*------------------------------------------------------------------------*
039400000000     C     LETFIL        BEGSR
039500000000     C*
039600000000     C* LEGGO FINO A:
039700000000     C*     - FINE FILE
039800000000     C*     - TROVATO RECORD VALIDO
039900000000     C                   MOVEL     'N'           WRECOK            1
040000000000DO  1C     *IN99         DOWEQ     *OFF
040100000000     C     WRECOK        ANDEQ     'N'
040200010723     C     KEYTLL        READE(N)  TITLL01L                               99
040300000000     C*
040400000000IF  2C     *IN99         IFEQ      *ON
040500000000     C                   Z-ADD     1             SFLEOF
040600000000X   2C                   ELSE
040700000000     C                   EXSR      CHKREC
040800000000E   2C                   ENDIF
040900000000     C*
041000000000E   1C                   ENDDO
041100000000     C*
041200000000     C                   ENDSR
041300000000     C*------------------------------------------------------------------------*
041400000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
041500000000     C*------------------------------------------------------------------------*
041600000000     C     CHKREC        BEGSR
041700000000     C*
041800000000     C                   MOVEL     'S'           WRECOK
041900000000     C*
042000000000     C                   ENDSR
042100000000     C*------------------------------------------------------------------------*
042200000000     C* IRESFL - IMPOSTA I CAMPI DEL SUBFILE DAL FILE FISICO
042300000000     C*------------------------------------------------------------------------*
042400000000     C     IRESFL        BEGSR
042500000000     C*
042600000000     C                   MOVEL     *BLANKS       VSCOPZ
042700000000     C                   MOVEL     TLLATB        VSCATB
042800000000     C                   MOVEL     TLLTMZ        VSCTMZ
042900000000     C                   Z-ADD     TLLPDR        VSCPDR
043000000000     C                   Z-ADD     TLLILI        VSCILI
043100000000     C                   Z-ADD     TLLIES        VSCIES
043200000000     C                   MOVEL     TLLCAU        VSCCAU
043300010723     C                   movel     tlldiv        vscdiv
043400000000     C* DECODIFICHE
043500000000     C                   MOVEL     *BLANKS       VSDTMZ
043600000000     C                   SETOFF                                           28
043700000000     C                   Z-ADD     1             J
043800000000     C     VSCTMZ        LOOKUP    TMZ(J)                                 28
043900000000IF  1C     *IN28         IFEQ      *ON
044000000000     C                   MOVEL     DTMZ(J)       VSDTMZ
044100000000X   1C                   ELSE
044200000000IF  2C     VSCTMZ        IFEQ      '99'
044300000000     C                   MOVEL     DES999        VSDTMZ
044400000000E   2C                   ENDIF
044500000000E   1C                   ENDIF
044600000000     C*
044700000000     C                   MOVEL     *BLANKS       VSDPDR
044800000821     C                   EVAL      KLZTIP = 'T '
044900000000     C                   Z-ADD     VSCPDR        KLZPDR
045000000000     C     KEYTLZ        CHAIN     TNTLZ01L                           28
045100000000IF  1C     *IN28         IFEQ      *OFF
045200000000     C                   MOVEL     TLZRSC        VSDPDR
045300000000X   1C                   ELSE
045400000000IF  2C     VSCPDR        IFEQ      9999999
045500000000     C                   MOVEL     DES999        VSDPDR
045600000000E   2C                   ENDIF
045700000000E   1C                   ENDIF
045800000000     C*
045900000000     C                   MOVEL     *BLANKS       VSDCAU
046000000000     C                   SETOFF                                           28
046100000000     C                   Z-ADD     1             J
046200000000     C     VSCCAU        LOOKUP    CAU(J)                                 28
046300000000IF  1C     *IN28         IFEQ      *ON
046400000000     C                   MOVEL     DCAU(J)       VSDCAU
046500000000E   1C                   ENDIF
046600000000     C*
046700000000     C                   ENDSR
046800000000     C*------------------------------------------------------------------------*
046900000000     C* LANPGM - LANCIA UN PROGRAMMA
047000000000     C*------------------------------------------------------------------------*
047100000000     C     LANPGM        BEGSR
047200000000     C***
047300000000     C* PARAMETRI INFORMATIVI
047400000000     C***
047500000000     C                   MOVEL     WOP0          D00OP0
047600000000     C                   MOVEL     WOP1          D00OP1
047700000000     C                   MOVEL     '0'           D00F03
047800000000     C                   MOVEL     '0'           D00F12
047900000000     C                   MOVEL     '0'           D00ERR
048000000000     C                   MOVEL     *BLANKS       D00MSG
048100000000     C***
048200000000     C* PARAMETRI
048300000000     C***
048400000000     C                   Z-ADD     WTRN          D00TRN
048500000000     C                   Z-ADD     WDDE          D00DDE
048600000000     C                   MOVEL     WTMZ          D00TMZ
048700000000     C                   Z-ADD     WPDR          D00PDR
048800000000     C*--------------------
048900000000     C* L A N C I O
049000000000     C*--------------------
049100000000     C                   MOVEL     DS00          KPJBU
049200000210     C                   CALL      WPGM                                 99
049300000000     C                   PARM                    KPJBA
049400000000     C                   MOVEL     KPJBU         DS00
049500000000     C*
049600000000IF  1C     D00F03        IFEQ      '1'
049700000000     C                   MOVEL     '1'           $F03
049800000000     C                   MOVEL     '1'           $FINE
049900000000E   1C                   ENDIF
050000000000IF  1C     D00F12        IFEQ      '1'
050100000000     C                   MOVEL     '1'           $F12
050200000000E   1C                   ENDIF
050300000000     C*
050400000000     C                   MOVEL     D00MSG        $MSG
050500000000     C                   EXSR      CARMSG
050600000000     C*
050700000000     C                   ENDSR
050800000000     C*------------------------------------------------------------------------*
050900000000     C* IMPVID - IMPOSTAZIONI VIDEO
051000000000     C*------------------------------------------------------------------------*
051100000000     C     IMPVID        BEGSR
051200000000     C*
051300000000     C* CALCOLA LA DATA CORRENTE
051400000000     C                   TIME                    WNUM14           14 0
051500000000     C                   MOVE      WNUM14        WNUM8             8 0
051600000000     C                   Z-ADD     WNUM8         G08DAT
051700000000     C                   Z-ADD     *ZEROS        G08INV
051800000000     C                   MOVEL     '0'           G08ERR
051900000000     C                   CALL      'XSRDA8'
052000000000     C                   PARM                    WLBDA8
052100000000     C                   Z-ADD     G08INV        DATCOR            8 0
052200000000     C* DATI TRAINO
052300000000     C                   Z-ADD     D00TRN        VCCTRN
052400000000     C                   Z-ADD     D00DDE        G08INV
052500000000     C                   Z-ADD     *ZEROS        G08DAT
052600000000     C                   MOVEL     '3'           G08ERR
052700000000     C                   CALL      'XSRDA8'
052800000000     C                   PARM                    WLBDA8
052900000000     C                   Z-ADD     G08DAT        VCCDDE
053000000000     C* LEGGE IL TRAINO
053100000000     C                   Z-ADD     D00TRN        KLTTRN
053200000000     C                   Z-ADD     D00DDE        KLTDDE
053300000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
053400020716     c                   Z-Add     1             KtbKut
053500000000     C                   MOVEL     'TV'          KTBCOD
053600000000     C                   MOVEL(P)  TLTTTR        KTBKEY
053700000000     C     KEYTAB        CHAIN     TABEL00F                           99
053800000000     C                   MOVEL     TBLUNI        DSTV
053900000000     C                   MOVEL     §TVDES        VCCTTR
054000000000     C                   Z-ADD     TLTDSC        G08INV
054100000000     C                   Z-ADD     *ZEROS        G08DAT
054200000000     C                   MOVEL     '3'           G08ERR
054300000000     C                   CALL      'XSRDA8'
054400000000     C                   PARM                    WLBDA8
054500000000     C                   Z-ADD     G08DAT        VCCDSC
054600000000     C                   Z-ADD     TLTTFP        VCCTFP
054700000000     C                   Z-ADD     TLTTFA        VCCTFA
054800960424     C*
054900960424     C* SE TIPO TRAINO INOLTRO METTE NELLA DESCRIZIONE IL TIPO TRAINO ASSOCIATO
055000960424IF  1C     TLTTTR        IFEQ      'I'
055100960424     C                   CLEAR                   SA0
055200960424     C                   MOVEA     'INOLTRO'     SA0
055300960424     C                   MOVEL     '('           SA0(8)
055400960424     C                   MOVEL     D00INL        SA0(9)
055500960424     C                   MOVEL     ')'           SA0(10)
055600960424     C                   MOVEA     SA0           VCCTTR
055700960424E   1C                   ENDIF
055800000000     C*
055900000000     C* EMETTE UN MESSAGGIO SE I LISTINI SONO ANNULLATI (UNICO CASO QUANDO
056000000000     C* ARRIVA DALL'INTERROGAZIONE DI UN TRAINO ANNULLATO)
056100000000     C                   MOVEL     *BLANKS       VCCATB
056200000000     C                   MOVEL     '0'           *IN48
056300000000IF  1C     TLTATB        IFNE      *BLANKS
056400000000     C                   MOVEL     DESATB        VCCATB
056500000000     C                   MOVEL     '1'           *IN48
056600000000E   1C                   ENDIF
056700000000     C*
056800000000     C                   ENDSR
056900000000     C*------------------------------------------------------------------------*
057000000000     C* CARTAB - CARICA LE TABELLE
057100000000     C*------------------------------------------------------------------------*
057200000000     C     CARTAB        BEGSR
057300000000     C*--------------------
057400000000     C* TIPI AUTOMEZZI
057500000000     C*--------------------
057600000000     C                   Z-ADD     *ZEROS        J                 4 0
057700020716     c                   Z-Add     1             KtbKut
057800000000     C                   MOVEL     'TU'          KTBCOD
057900000000     C     KE2TAB        SETLL     TABEL00F
058000000000     C     KE2TAB        READE     TABEL00F                               99
058100000000DO  1C     *IN99         DOWEQ     *OFF
058200000000IF  2C     TBLFLG        IFEQ      *BLANKS
058300000000     C                   ADD       1             J
058400000000     C                   MOVEL     TBLKEY        TMZ(J)
058500000000     C                   MOVEL     TBLUNI        DSTU
058600000000     C                   MOVEL     §TUDES        DTMZ(J)
058700000000E   2C                   ENDIF
058800000000     C     KE2TAB        READE     TABEL00F                               99
058900000000E   1C                   ENDDO
059000000000     C*--------------------
059100000000     C* CAUSALI CONTABILIZZAZIONE
059200000000     C*--------------------
059300020716     c                   If        DutLpo = 'S'
059400000821     c                   OPEN      ANATB01L
059500000000     C                   Z-ADD     *ZEROS        J
059600000218     C                   MOVE      KNSIF         WCOMO4            4
059700000218     C                   MOVEL     WCOMO4        WSOC              3
059800000218     C                   movel     WSOC          KAtazi
059900000210     C                   MOVEL     'Y4Z'         KatCOD
060000000210     C     KEATAB        CHAIN     ANATB01L                           99
060100000821if  2C     *IN99         IFEQ      *ON
060200000218     C                   movel     '888'         KAtazi
060300000210     C     KEATAB        CHAIN     ANATB01L                           99
060400000821E   2C                   ENDIF
060500000821DO  2C     *IN99         DOWEQ     *OFF
060600000000     C                   ADD       1             J
060700000210     C                   MOVEL     ATBKEY        CAU(J)
060800000210     C                   MOVEL     ATBUNI        ANGY4ZDS
060900000000     C                   MOVEL     §4ZFLG        FLG(J)
061000000000     C                   MOVEL     §4ZDES        DCAU(J)
061100000210     C     KEATAB        READE     ANATB01L                               99
061200000821E   2C                   ENDDO
061300000821     c                   CLOSE     ANATB01L
061400000821E   1C                   ENDIF
061500000000     C*
061600000000     C                   ENDSR
061700000000     C*------------------------------------------------------------------------*
061800000000     C* *INZSR - ROUTINE INIZIALE
061900000000     C*------------------------------------------------------------------------*
062000000000     C     *INZSR        BEGSR
062100000000     C*--------------------
062200000000     C* RICEVIMENTO PARAMETRI
062300000000     C*--------------------
062400000000     C     *ENTRY        PLIST
062500000000     C                   PARM                    KPJBA
062600000000     C                   MOVEL     KPJBU         DS00
062700000000     C*--------------------
062800000000     C* VARIABILI RIFERITE AL DATA BASE
062900000000     C*--------------------
063000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
063100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
063200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
063300000210     C     *LIKE         DEFINE    ATBCOD        KATCOD
063400000210     C     *LIKE         DEFINE    ATBAZI        KATAZI
063500000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN
063600000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
063700000000     C     *LIKE         DEFINE    TLLTRN        KLLTRN
063800000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
063900000000     C     *LIKE         DEFINE    TLZPDR        KLZPDR
064000000821     C     *LIKE         DEFINE    TLZTIP        KLZTIP
064100000000     C     *LIKE         DEFINE    D00OP0        WOP0
064200000000     C     *LIKE         DEFINE    D00OP1        WOP1
064300000000     C     *LIKE         DEFINE    D00F03        WF03
064400000000     C     *LIKE         DEFINE    D00F12        WF12
064500000000     C     *LIKE         DEFINE    D00ERR        WERR
064600000000     C     *LIKE         DEFINE    D00MSG        WMSG
064700000000     C     *LIKE         DEFINE    D00TRN        WTRN
064800000000     C     *LIKE         DEFINE    D00DDE        WDDE
064900000000     C     *LIKE         DEFINE    D00TMZ        WTMZ
065000000000     C     *LIKE         DEFINE    D00PDR        WPDR
065100000000     C*--------------------
065200000000     C* CHIAVI DI LETTURA
065300000000     C*--------------------
065400000000     C* LETTURA TABEL00F - COMPLETA
065500000000     C     KEYTAB        KLIST
065600000000     C                   KFLD                    KTBKUT
065700000000     C                   KFLD                    KTBCOD
065800000000     C                   KFLD                    KTBKEY
065900000000     C* LETTURA TABEL00F - PARZIALE
066000000000     C     KE2TAB        KLIST
066100000000     C                   KFLD                    KTBKUT
066200000000     C                   KFLD                    KTBCOD
066300000210     C* LETTURA ANATB01L - PARZIALE
066400000210     C     KEATAB        KLIST
066500000210     C                   KFLD                    KATCOD
066600000210     C                   KFLD                    KATAZI
066700000000     C* LETTURA TNTLT01L - COMPLETA
066800000000     C     KEYTLT        KLIST
066900000000     C                   KFLD                    KLTTRN
067000000000     C                   KFLD                    KLTDDE
067100010723     C* LETTURA TITLL01L - PARZIALE
067200000000     C     KEYTLL        KLIST
067300000000     C                   KFLD                    KLLTRN
067400000000     C                   KFLD                    KLLDDE
067500000000     C* LETTURA TNTLZ01L - COMPLETA
067600000000     C     KEYTLZ        KLIST
067700000821     C                   KFLD                    KLZTIP
067800000821     C                   KFLD                    KLZPDR
067900000000     C*--------------------
068000000000     C* VARIABILI DI CONTROLLO
068100000000     C*--------------------
068200000000     C                   MOVEL     'N'           WRECOK            1
068300000000     C***
068400000000     C* IMPOSTAZIONI VIDEO
068500000000     C***
068600000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
068700000000     C                   MOVE      D00OP0        INTGES            2
068800000000IF  1C     INTGES        IFEQ      '02'
068900000000     C                   MOVEL     'G'           $TGES             1
069000000000     C                   MOVEL     DESGES        VTDOPE
069100000000X   1C                   ELSE
069200000000     C                   MOVEL     'I'           $TGES             1
069300000000     C                   MOVEL     DESINT        VTDOPE
069400000000E   1C                   ENDIF
069500000000     C*
069600000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
069700000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
069800000000IF  1C     *IN99         IFEQ      *ON
069900000000     C     *IN99         ANDEQ     *OFF
070000000000     C* TRAINI
070100000000     C                   UPDATE    TNTLT000
070200000000     C                   WRITE     TNTLT000
070300000000     C* LISTINI
070400010723     C                   UPDATE    TITLL000
070500010723     C                   WRITE     TITLL000
070600000000E   1C                   ENDIF
070700000000     C*
070800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
070900000000     C                   EXSR      INZSTD
071000000000     C*
071100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
071200000000     C                   EXSR      INZSFL
071300000000     C*
071400000000     C* IMPOSTAZIONE INIZIALE VIDEO
071500000000     C                   EXSR      IMPVID
071600000000     C*
071700000000     C* CARICA LE TABELLE
071800000000     C                   EXSR      CARTAB
071900000000     C*
072000000000     C* EMETTE VIDEATA SUBFILE
072100000000     C                   EXSR      CHKPAR
072200000000     C                   EXSR      CARVS
072300000000     C*
072400000000     C                   ENDSR
072500000000     C*------------------------------------------------------------------------*
072600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072800000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
072900000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
073000000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
073100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073300000000     C*------------------------------------------------------------------------*
073400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
073500000000     C*------------------------------------------------------------------------*
073600000000     C     INZSTD        BEGSR
073700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
073800000000     C                   CALL      'XKEYS'
073900000000     C                   PARM                    DSKEYS
074000000000     C                   MOVEA     KEYRIG        KEYS
074100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
074200000000     C     XMSGPA        PLIST
074300000000     C                   PARM                    DSMSMS
074400000000     C                   PARM                    DSMSPG
074500000000     C                   PARM                    DSMSKE
074600000000     C                   PARM                    DSMSER
074700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
074800010723     C                   MOVEL     'TNTL38R'     DSMSPG
074900010723     C                   MOVEL     'TNTL38R'     VMCQUE
075000000000     C                   MOVEL     *BLANKS       VMCKEY
075100000000     C                   MOVEL     *BLANKS       DSMSMS
075200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
075300000000     C                   MOVEL     '0'           $ERR              1
075400000000     C                   MOVEL     '0'           $F03              1
075500000000     C                   MOVEL     '0'           $F12              1
075600000000     C                   MOVEL     *BLANKS       $GES              2
075700000000     C                   MOVEL     *BLANKS       $MSG             80
075800000000     C                   MOVEL     '0'           $FINE             1
075900000000     C                   MOVEL     '0'           $TESVD            1
076000000000     C* LANCIO ROUTINE PER VERIFICARE SE
076100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
076200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
076300020716
076400020716     c     *dtaara       define    §azute        azuteds
076500020716     c     *dtaara       define    §datiute      ddatiute
076600020716
076700020716     c                   in(E)     *dtaara
076800020716     c                   if        %Error or rsut = *blanks
076900020716     c                   clear                   tibs34ds
077000020716     c                   call      'TIBS34R'
077100020716     c                   parm                    Tibs34Ds
077200020716     c                   in        *dtaara
077300020716     c                   endif
077400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
077500000000     C                   BITOFF    '01234567'    POSMI1            1
077600000000     C                   BITOFF    '01234567'    POSMI2            1
077700000000     C                   BITOFF    '01234567'    POSMA1            1
077800000000     C                   BITOFF    '01234567'    POSMA2            1
077900000000     C                   BITON     '34'          POSMI1            1
078000000000     C                   BITON     '7'           POSMI2            1
078100000000     C                   BITON     '34'          POSMA1            1
078200000000     C                   BITON     '0'           POSMA2            1
078300000000     C                   MOVE      POSMI2        POSMIN            2
078400000000     C                   MOVEL     POSMI1        POSMIN
078500000000     C                   MOVE      POSMA2        POSMAX            2
078600000000     C                   MOVEL     POSMA1        POSMAX
078700000000     C                   ENDSR
078800000000     C*------------------------------------------------------------------------*
078900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
079000000000     C*------------------------------------------------------------------------*
079100000000     C     FUNSTD        BEGSR
079200000000     C                   SELECT
079300000000     C     KEY           WHENEQ    F3
079400000000     C                   EXSR      F03STD
079500000000     C     KEY           WHENEQ    F12
079600000000     C                   EXSR      F12STD
079700000000     C     KEY           WHENEQ    F24
079800000000     C                   EXSR      F24STD
079900000000     C                   OTHER
080000000000     C                   MOVEL     MSG(2)        $MSG
080100000000     C                   EXSR      CARMSG
080200000000     C                   ENDSL
080300000000     C                   ENDSR
080400000000     C*------------------------------------------------------------------------*
080500000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
080600000000     C*------------------------------------------------------------------------*
080700000000     C     F03STD        BEGSR
080800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
080900000000     C                   MOVEL     '1'           $F03
081000000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
081100000000     C                   MOVEL     '1'           $FINE
081200951212     C* PULISCE IL MESSAGGIO
081300951212     C                   MOVEL     *BLANKS       D00MSG
081400000000     C                   ENDSR
081500000000     C*------------------------------------------------------------------------*
081600000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
081700000000     C*------------------------------------------------------------------------*
081800000000     C     F12STD        BEGSR
081900000000     C*
082000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
082100000000     C                   MOVEL     '1'           $F12
082200000000     C* IMPOSTO FLAG DI FIN PGM
082300951212     C                   MOVEL     '1'           $FINE
082400951212     C* PULISCE IL MESSAGGIO
082500951212     C                   MOVEL     *BLANKS       D00MSG
082600000000     C                   ENDSR
082700000000     C*------------------------------------------------------------------------*
082800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
082900000000     C*------------------------------------------------------------------------*
083000000000     C     F24STD        BEGSR
083100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
083200000000     C                   EXSR      OUTFUN
083300000000     C                   ENDSR
083400000000     C*------------------------------------------------------------------------*
083500000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
083600000000     C*------------------------------------------------------------------------*
083700000000     C     INZSFL        BEGSR
083800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
083900000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
084000000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
084100000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
084200000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
084300000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
084400000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
084500000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
084600000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
084700000000     C                   Z-ADD     09            SFLPAG
084800000000     C                   ENDSR
084900000000     C*------------------------------------------------------------------------*
085000000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
085100000000     C*------------------------------------------------------------------------*
085200000000     C     FUNSFL        BEGSR
085300000000     C                   SELECT
085400000000     C     KEY           WHENEQ    ENTER
085500000000     C                   EXSR      ENTSFL
085600000000     C     KEY           WHENEQ    F3
085700000000     C                   EXSR      F03STD
085800000000     C     KEY           WHENEQ    F5
085900000000     C                   EXSR      F05STD
086000000000     C     KEY           WHENEQ    F12
086100000000     C                   EXSR      F12STD
086200000000     C     KEY           WHENEQ    F17
086300000000     C                   EXSR      F17STD
086400000000     C     KEY           WHENEQ    F18
086500000000     C                   EXSR      F18STD
086600000000     C     KEY           WHENEQ    F23
086700000000     C                   EXSR      F23STD
086800000000     C     KEY           WHENEQ    F24
086900000000     C                   EXSR      F24STD
087000000000     C     KEY           WHENEQ    ROLLUP
087100000000     C                   EXSR      RUPSTD
087200000000     C     KEY           WHENEQ    ROLLDN
087300000000     C                   EXSR      RDNSTD
087400000000     C                   OTHER
087500000000     C                   MOVEL     MSG(2)        $MSG
087600000000     C                   EXSR      CARMSG
087700000000     C                   ENDSL
087800000000     C                   ENDSR
087900000000     C*------------------------------------------------------------------------*
088000000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
088100000000     C*------------------------------------------------------------------------*
088200000000     C     F05STD        BEGSR
088300000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
088400000000     C                   EXSR      POSSFL
088500000000     C                   ENDSR
088600000000     C*------------------------------------------------------------------------*
088700000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
088800000000     C*------------------------------------------------------------------------*
088900000000     C     F17STD        BEGSR
089000000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
089100000000     C                   Z-ADD     1             VCCNBR
089200000000     C                   ENDSR
089300000000     C*------------------------------------------------------------------------*
089400000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
089500000000     C*------------------------------------------------------------------------*
089600000000     C     F18STD        BEGSR
089700000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
089800000000     C                   Z-ADD     SFLULT        VCCNBR
089900000000     C                   ENDSR
090000000000     C*------------------------------------------------------------------------*
090100000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
090200000000     C*------------------------------------------------------------------------*
090300000000     C     F23STD        BEGSR
090400000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
090500000000     C                   EXSR      OUTOPZ
090600000000     C                   ENDSR
090700000000     C*------------------------------------------------------------------------*
090800000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
090900000000     C*------------------------------------------------------------------------*
091000000000     C     RUPSTD        BEGSR
091100000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
091200000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
091300000000     C     CURSOR        IFGE      POSMIN
091400000000     C     CURSOR        ANDLE     POSMAX
091500000000     C                   ELSE
091600000000     C                   EXSR      CARSFL
091700000000     C                   ENDIF
091800000000     C                   ENDSR
091900000000     C*------------------------------------------------------------------------*
092000000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
092100000000     C*------------------------------------------------------------------------*
092200000000     C     RDNSTD        BEGSR
092300000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
092400000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
092500000000     C     CURSOR        IFGE      POSMIN
092600000000     C     CURSOR        ANDLE     POSMAX
092700000000     C                   ELSE
092800000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
092900000000     C                   MOVEL     '1'           $ERR
093000000000     C                   MOVEL     MSG(5)        $MSG
093100000000     C                   EXSR      CARMSG
093200000000     C                   ENDIF
093300000000     C                   ENDSR
093400000000     C*------------------------------------------------------------------------*
093500000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
093600000000     C*------------------------------------------------------------------------*
093700000000     C     ENTSFL        BEGSR
093800000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
093900000000     C                   EXSR      NEWPOS
094000000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
094100000000     C     $ERR          IFEQ      '0'
094200000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
094300000000     C     SWPOSI        IFNE      0
094400000000     C                   EXSR      POSSFL
094500000000     C                   ELSE
094600000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
094700000000     C     SFLULT        IFNE      0
094800000000     C                   EXSR      OPZSFL
094900000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
095000000000     C     SWMODI        IFNE      0
095100000000     C                   EXSR      POSSFL
095200000000     C                   ENDIF
095300000000     C                   ENDIF
095400000000     C                   ENDIF
095500000000     C                   ENDIF
095600000000     C                   ENDSR
095700000000     C*------------------------------------------------------------------------*
095800000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
095900000000     C*------------------------------------------------------------------------*
096000000000     C     POSSFL        BEGSR
096100000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
096200000000     C                   EXSR      CLRSFL
096300000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
096400000000     C                   EXSR      SETFIL
096500000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
096600000000     C     SFLEOF        IFEQ      0
096700000000     C                   EXSR      CARSFL
096800000000     C                   ENDIF
096900000000     C                   ENDSR
097000000000     C*------------------------------------------------------------------------*
097100000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
097200000000     C*------------------------------------------------------------------------*
097300000000     C     OPZSFL        BEGSR
097400000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
097500000000     C                   MOVEL     'S'           OPZOK             1
097600000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
097700000000     C                   Z-ADD     0             SFLVAR            5 0
097800000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
097900000000     C                   Z-ADD     0             SFLERR
098000000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
098100000000     C                   Z-ADD     SFLULT        WFLULT
098200000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
098300010723     C                   READC     TL38DVS                                49
098400000000     C     *IN49         DOWEQ     '0'
098500000000     C     $ERR          ANDEQ     '0'
098600000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
098700000000     C                   ADD       1             SFLVAR
098800000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
098900000000     C     OPZOK         IFEQ      'S'
099000000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
099100000000     C     VSCOPZ        IFNE      *BLANKS
099200000000     C                   EXSR      CONOPZ
099300000000     C     OPZOK         IFEQ      'S'
099400000000     C     $ERR          ANDEQ     '0'
099500000000     C                   EXSR      GESOPZ
099600000000     C                   ENDIF
099700000000     C                   ENDIF
099800000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
099900000000     C*            2. PULIZIA SCELTA IN SFL
100000000000     C     OPZOK         IFEQ      'S'
100100000000     C                   MOVEL     '0'           *IN41
100200000000     C                   MOVEL     *BLANKS       VSCOPZ
100300000000     C                   ELSE
100400000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
100500000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
100600000000     C                   MOVEL     '1'           *IN41
100700000000     C                   Z-ADD     SFLULT        SFLERR
100800000000     C                   ENDIF
100900000000     C                   ELSE
101000000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
101100000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
101200000000     C                   MOVEL     '1'           *IN41
101300000000     C                   ENDIF
101400000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
101500000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
101600000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
101700000000     C     OPZOK         IFNE      'S'
101800000000     C                   Z-ADD     SFLERR        VCCNBR
101900000000     C                   ELSE
102000000000     C                   Z-ADD     SFLULT        VCCNBR
102100000000     C                   ENDIF
102200000000     C* AGGIORNO RECORD SFL
102300010723     C                   UPDATE    TL38DVS
102400000000     C     $ERR          IFEQ      '0'
102500010723     C                   READC     TL38DVS                                49
102600000000     C                   ENDIF
102700000000     C                   ENDDO
102800000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
102900000000     C                   Z-ADD     WFLULT        SFLULT
103000000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
103100000000     C     SFLVAR        IFEQ      0
103200000000     C                   MOVEL     '1'           $FINE
103300000000     C                   ENDIF
103400000000     C                   ENDSR
103500000000     C*------------------------------------------------------------------------*
103600000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
103700000000     C*------------------------------------------------------------------------*
103800000000     C     CLRSFL        BEGSR
103900000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
104000000000     C* (0=NON FINE FILE  1=FINE FILE)
104100000000     C                   Z-ADD     0             SFLEOF
104200000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
104300000000     C                   Z-ADD     0             SFLULT
104400000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
104500000000     C                   MOVEL     '1'           *IN40
104600010723     C                   WRITE     TL38DVC
104700000000     C                   MOVEL     '0'           *IN40
104800000000     C                   ENDSR
104900000000     C*------------------------------------------------------------------------*
105000000000     C* CARSFL - CARICAMENTO SFL
105100000000     C*------------------------------------------------------------------------*
105200000000     C     CARSFL        BEGSR
105300000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
105400000000     C     SFLULT        IFEQ      0
105500000000     C                   EXSR      SETFIL
105600000000     C                   ENDIF
105700000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
105800000000     C                   Z-ADD     SFLULT        WFLULT
105900000000     C* CICLO:
106000000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
106100000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
106200000000     C                   Z-ADD     1             SFLRIG
106300000000     C     SFLRIG        DOWLE     SFLPAG
106400000000     C     SFLEOF        ANDEQ     0
106500000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
106600000000     C                   MOVEL     *BLANKS       VSCOPZ
106700000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
106800000000     C                   EXSR      IRESFL
106900000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
107000000000     C                   ADD       1             SFLULT
107100000000     C* SCRITTURA RECORD SFL
107200010723     C                   WRITE     TL38DVS
107300000000     C                   ADD       1             SFLRIG
107400000000     C* LETTURA FILE DA CARICARE
107500000000     C                   EXSR      LETFIL
107600000000     C                   ENDDO
107700000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
107800000000     C     WFLULT        IFNE      SFLULT
107900000000     C                   Z-ADD     WFLULT        VCCNBR
108000000000     C                   ADD       1             VCCNBR
108100000000     C                   ENDIF
108200000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
108300000000     C     SFLEOF        IFEQ      1
108400000000     C     SFLULT        ANDGT     *ZEROS
108500000000     C                   MOVEL     '1'           $ERR
108600000000     C                   MOVEL     MSG(6)        $MSG
108700000000     C                   EXSR      CARMSG
108800000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
108900010723     C     SFLULT        CHAIN     TL38DVS                            99
109000000000     C     *IN99         IFEQ      '0'
109100000000     C                   SETON                                        45
109200010723     C                   UPDATE    TL38DVS
109300000000     C                   SETOFF                                       45
109400000000     C                   ENDIF
109500000000     C                   ENDIF
109600000000     C                   ENDSR
109700000000     C*------------------------------------------------------------------------*
109800000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
109900000000     C*------------------------------------------------------------------------*
110000000000     C     OUTSFL        BEGSR
110100000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
110200000000     C     MSGULT        IFNE      *ZEROS
110300000000     C                   EXSR      OUTMSG
110400000000     C                   ENDIF
110500000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
110600000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
110700000000     C     SFLEOF        IFEQ      0
110800000000     C                   MOVEL     '0'           *IN42
110900000000     C                   ELSE
111000000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
111100000000     C                   MOVEL     '1'           *IN42
111200000000     C                   ENDIF
111300000000     C* EMISSIONE SUBFILE
111400000000     C     SFLULT        IFLE      0
111500000000     C*  --> CASO DI SFL VUOTO
111600000000     C                   MOVEL     '0'           *IN43
111700010723     C                   WRITE     TL38DVV
111800010723     C                   WRITE     TL38DVC
111900010723     C                   READ      TL38DVC                                99
112000000000     C                   ELSE
112100000000     C*  --> CASO DI SFL CON RECORDS
112200010723     C                   WRITE     TL38DVC
112300010723     C                   READ      TL38DVC                                99
112400000000     C                   ENDIF
112500000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
112600000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
112700000000     C                   Z-ADD     FRTSFL        VCCNBR
112800000000     C                   ENDSR
112900000000     C*------------------------------------------------------------------------*
113000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
113100000000     C*          CODA MESSAGGI DEL PGM
113200000000     C*------------------------------------------------------------------------*
113300000000     C     CARMSG        BEGSR
113400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
113500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
113600000000     C     MSGULT        IFEQ      0
113700000000     C                   MOVEL     '1'           *IN40
113800010723     C                   WRITE     TL38DVK
113900000000     C                   MOVEL     '0'           *IN40
114000000000     C                   ENDIF
114100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
114200000000     C                   MOVEL     $MSG          DSMSMS
114300000000     C                   CALL      'XMSG'        XMSGPA
114400000000     C     DSMSER        IFEQ      0
114500000000     C                   MOVEL     DSMSKE        VMCKEY
114600000000     C                   ENDIF
114700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
114800000000     C                   ADD       1             MSGULT            4 0
114900010723     C                   WRITE     TL38DVM
115000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
115100000000     C                   MOVEL     *BLANKS       $MSG
115200000000     C                   ENDSR
115300000000     C*------------------------------------------------------------------------*
115400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
115500000000     C*------------------------------------------------------------------------*
115600000000     C     OUTMSG        BEGSR
115700000000     C* VISUALIZZAZIONE MESSAGGIO
115800000000     C                   MOVEL     '1'           *IN44
115900010723     C                   WRITE     TL38DVK
116000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
116100000000     C                   Z-ADD     *ZEROS        MSGULT
116200000000     C                   ENDSR
116300000000     C*------------------------------------------------------------------------*
116400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
116500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
116600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
116700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
116800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
116900000000     C*------------------------------------------------------------------------*
117000000000     C     CARFUN        BEGSR
117100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
117200000000     C     1             DO        31            I                 5 0
117300000000     C                   MOVEL     FUN(I)        FUNRIG
117400000000     C                   MOVEL     FUNFUN        FFU(I)
117500000000     C                   MOVEL     FUNDES        FDE(I)
117600020716     c                   If        DutLpo = 'S'
117700000000     C     $TGES         IFEQ      'G'
117800000000     C                   MOVEL     FUNASG        FAT(I)
117900000000     C                   ELSE
118000000000     C                   MOVEL     FUNASI        FAT(I)
118100000000     C                   ENDIF
118200000000     C                   ELSE
118300000000     C     $TGES         IFEQ      'G'
118400000000     C                   MOVEL     FUNAFG        FAT(I)
118500000000     C                   ELSE
118600000000     C                   MOVEL     FUNAFI        FAT(I)
118700000000     C                   ENDIF
118800000000     C                   ENDIF
118900000000     C     FAT(I)        IFEQ      'E'
119000000000     C                   MOVEL     USER          WEDP              3
119100000000     C     WEDP          IFEQ      'EDP'
119200000000     C                   MOVEL     'S'           FAT(I)
119300000000     C                   ELSE
119400000000     C                   MOVEL     'N'           FAT(I)
119500000000     C                   ENDIF
119600000000     C                   ENDIF
119700000000     C                   ENDDO
119800000000     C                   ENDSR
119900000000     C*------------------------------------------------------------------------*
120000000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
120100000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
120200000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
120300000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
120400000000     C*------------------------------------------------------------------------*
120500000000     C     OUTFUN        BEGSR
120600000000     C     1             DO        8             I
120700000000     C                   MOVEL     *BLANKS       FUV(I)
120800000000     C                   ENDDO
120900000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
121000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
121100000000     C     1             DO        31            I
121200000000     C     FAT(I)        IFEQ      'S'
121300000000     C     FDE(I)        ANDNE     *BLANKS
121400000000     C                   ADD       1             FUNNAT
121500000000     C                   ENDIF
121600000000     C                   ENDDO
121700000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
121800000000     C     FUNNAT        IFGT      8
121900000000     C                   MOVEL     'S'           FAT(24)
122000000000     C                   ENDIF
122100000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
122200000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
122300000000     C                   Z-ADD     8             FUNVIS            5 0
122400000000     C     FAT(23)       IFEQ      'S'
122500000000     C                   SUB       1             FUNVIS
122600000000     C                   ENDIF
122700000000     C     FAT(24)       IFEQ      'S'
122800000000     C                   SUB       1             FUNVIS
122900000000     C                   ENDIF
123000000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
123100000000     C                   Z-ADD     *ZEROS        W                 5 0
123200000000     C                   ADD       1             FUNULT
123300000000     C     FUNULT        IFGT      31
123400000000     C                   Z-ADD     1             FUNULT
123500000000     C                   ENDIF
123600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
123700000000     C     FUNULT        DO        31            I
123800000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
123900000000     C     W             IFLT      FUNVIS
124000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
124100000000     C     FAT(I)        IFEQ      'S'
124200000000     C     FDE(I)        ANDNE     *BLANKS
124300000000     C     I             IFEQ      23
124400000000     C     I             OREQ      24
124500000000     C                   ELSE
124600000000     C                   ADD       1             W
124700000000     C                   MOVEL     FDE(I)        FUV(W)
124800000000     C                   ENDIF
124900000000     C                   ENDIF
125000000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
125100000000     C                   Z-ADD     I             FUNULT
125200000000     C                   ENDIF
125300000000     C                   ENDDO
125400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
125500000000     C                   MOVEL     FUV(1)        VFD001
125600000000     C                   MOVEL     FUV(2)        VFD002
125700000000     C                   MOVEL     FUV(3)        VFD003
125800000000     C                   MOVEL     FUV(4)        VFD004
125900000000     C                   MOVEL     FUV(5)        VFD005
126000000000     C                   MOVEL     FUV(6)        VFD006
126100000000     C     FAT(23)       IFEQ      'N'
126200000000     C     FAT(24)       ANDEQ     'N'
126300000000     C                   MOVEL     FUV(7)        VFD007
126400000000     C                   MOVEL     FUV(8)        VFD008
126500000000     C                   ELSE
126600000000     C     FAT(23)       IFEQ      'S'
126700000000     C     FAT(24)       ANDEQ     'S'
126800000000     C                   MOVEL     FDE(23)       VFD007
126900000000     C                   MOVEL     FDE(24)       VFD008
127000000000     C                   ELSE
127100000000     C                   MOVEL     FUV(7)        VFD007
127200000000     C     FAT(23)       IFEQ      'S'
127300000000     C                   MOVEL     FDE(23)       VFD008
127400000000     C                   ELSE
127500000000     C                   MOVEL     FDE(24)       VFD008
127600000000     C                   ENDIF
127700000000     C                   ENDIF
127800000000     C                   ENDIF
127900000000     C                   MOVEL     '1'           $TESVD
128000000000     C                   ENDSR
128100000000     C*------------------------------------------------------------------------*
128200000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
128300000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
128400000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
128500000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
128600000000     C*------------------------------------------------------------------------*
128700000000     C     CONFUN        BEGSR
128800000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
128900000000     C     1             DO        31            I
129000000000     C     KEY           IFEQ      KEYS(I)
129100000000     C* CONTROLLO SE IL TASTO E' ATTIVO
129200000000     C     FAT(I)        IFNE      'S'
129300000000     C                   MOVEL     MSG(1)        $MSG
129400000000     C                   EXSR      CARMSG
129500000000     C                   MOVEL     '1'           $ERR
129600000000     C                   ENDIF
129700000000     C                   ENDIF
129800000000     C                   ENDDO
129900000000     C                   ENDSR
130000000000     C*------------------------------------------------------------------------*
130100000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
130200000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
130300000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
130400000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
130500000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
130600000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
130700000000     C*------------------------------------------------------------------------*
130800000000     C     CAROPZ        BEGSR
130900000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
131000000000     C     1             DO        24            I
131100000000     C                   MOVEL     OPZ(I)        OPZRIG
131200000000     C                   MOVEL     OPZOPD        OPD(I)
131300000000     C                   MOVEL     OPZOPS        OPS(I)
131400000000     C                   MOVEL     OPZDES        ODE(I)
131500020716     c                   If        DutLpo = 'S'
131600000000     C     $TGES         IFEQ      'G'
131700000000     C                   MOVEL     OPZASG        OAT(I)
131800000000     C                   ELSE
131900000000     C                   MOVEL     OPZASI        OAT(I)
132000000000     C                   ENDIF
132100000000     C                   ELSE
132200000000     C     $TGES         IFEQ      'G'
132300000000     C                   MOVEL     OPZAFG        OAT(I)
132400000000     C                   ELSE
132500000000     C                   MOVEL     OPZAFI        OAT(I)
132600000000     C                   ENDIF
132700000000     C                   ENDIF
132800000000     C     OAT(I)        IFEQ      'E'
132900000000     C                   MOVEL     USER          WEDP              3
133000000000     C     WEDP          IFEQ      'EDP'
133100000000     C                   MOVEL     'S'           OAT(I)
133200000000     C                   ELSE
133300000000     C                   MOVEL     'N'           OAT(I)
133400000000     C                   ENDIF
133500000000     C                   ENDIF
133600000000     C                   ENDDO
133700000000     C                   ENDSR
133800000000     C*------------------------------------------------------------------------*
133900000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
134000000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
134100000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
134200000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
134300000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
134400000000     C*------------------------------------------------------------------------*
134500000000     C     OUTOPZ        BEGSR
134600000000     C     1             DO        8             I
134700000000     C                   MOVEL     *BLANKS       OPV(I)
134800000000     C                   ENDDO
134900000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
135000000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
135100000000     C                   ADD       1             OPZULT
135200000000     C     OPZULT        IFGT      24
135300000000     C                   Z-ADD     1             OPZULT
135400000000     C                   END
135500000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
135600000000     C     1             DO        24            I
135700000000     C     OAT(I)        IFEQ      'S'
135800000000     C     ODE(I)        ANDNE     *BLANKS
135900000000     C                   ADD       1             OPZNAT
136000000000     C                   ENDIF
136100000000     C                   ENDDO
136200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
136300000000     C                   Z-ADD     0             W
136400000000     C     OPZULT        DO        24            I
136500000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
136600000000     C     W             IFLT      8
136700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
136800000000     C     OAT(I)        IFEQ      'S'
136900000000     C     ODE(I)        ANDNE     *BLANKS
137000000000     C                   ADD       1             W
137100000000     C                   MOVEL     ODE(I)        OPV(W)
137200000000     C                   ENDIF
137300000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
137400000000     C                   Z-ADD     I             OPZULT
137500000000     C                   ENDIF
137600000000     C                   ENDDO
137700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
137800000000     C                   MOVEL     OPV(1)        VOD001
137900000000     C                   MOVEL     OPV(2)        VOD002
138000000000     C                   MOVEL     OPV(3)        VOD003
138100000000     C                   MOVEL     OPV(4)        VOD004
138200000000     C                   MOVEL     OPV(5)        VOD005
138300000000     C                   MOVEL     OPV(6)        VOD006
138400000000     C                   MOVEL     OPV(7)        VOD007
138500000000     C                   MOVEL     OPV(8)        VOD008
138600000000     C                   MOVEL     '1'           $TESVD
138700000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
138800000000     C     OPZNAT        IFGT      8
138900000000     C                   MOVEL     'S'           FAT(23)
139000000000     C                   ENDIF
139100000000     C                   ENDSR
139200000000     C*------------------------------------------------------------------------*
139300000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
139400000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
139500000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
139600000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
139700000000     C*------------------------------------------------------------------------*
139800000000     C     CONOPZ        BEGSR
139900000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
140000000000     C                   MOVEL     *BLANKS       OPZOK             1
140100000000     C     1             DO        24            I                 5 0
140200000000     C     VSCOPZ        IFEQ      OPD(I)
140300000000     C     VSCOPZ        OREQ      OPS(I)
140400000000     C     OAT(I)        IFEQ      'S'
140500000000     C                   MOVEL     OPD(I)        VSCOPZ
140600000000     C                   MOVEL     'S'           OPZOK
140700000000     C                   ELSE
140800000000     C                   MOVEL     'N'           OPZOK             1
140900000000     C                   ENDIF
141000000000     C                   ENDIF
141100000000     C                   ENDDO
141200000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
141300000000     C     OPZOK         IFNE      'S'
141400000000     C                   MOVEL     MSG(3)        $MSG
141500000000     C                   EXSR      CARMSG
141600000000     C                   MOVEL     '1'           $ERR
141700000000     C                   MOVEL     'N'           OPZOK             1
141800000000     C                   ENDIF
141900000000     C                   ENDSR
142000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
142100010723TNTL38R- Tasto funzionale non valido.                                          1
142200010723TNTL38R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
142300010723TNTL38R- Opzione non valida.                                                   3
142400010723TNTL38R- Opzione abilitata ma non gestita. Avvertire CED.                      4
142500010723TNTL38R- Inizio elenco.                                                        5
142600010723TNTL38R- Fine elenco.                                                          6
142700010723TNTL38R- 7
142800010723TNTL38R- 8
142900010723TNTL38R- 9
143000010723TNTL38R- 10
143100010723TNTL38R- 11
143200010723TNTL38R- 12
143300010723TNTL38R- 13
143400010723TNTL38R- 14
143500010723TNTL38R- 15
143600010723TNTL38R- 16
143700010723TNTL38R- 17
143800010723TNTL38R- 18
143900010723TNTL38R- 19
144000010723TNTL38R- 20
144100010723TNTL38R- 21
144200010723TNTL38R- 22
144300010723TNTL38R- 23
144400010723TNTL38R- 24
144500010723TNTL38R- 25
144600010723TNTL38R- 26
144700010723TNTL38R- 27
144800010723TNTL38R- 28
144900010723TNTL38R- Record annullato, consentito solo il ripristino                      29
145000010723TNTL38R- Record non annullato, impossibile ripristinare                       30
145100000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
145200000000- 1-1 -                   -    NN     NN
145300000000- 2-2 -2=Modifica         -    SN     SN
145400000000- 3-3 -                   -    NN     NN
145500000000- 4-4 -4=Annullamento     -    SN     SN
145600000000- 5-5 -                   -    NN     NN
145700000000- 6-6 -                   -    NN     NN
145800130529- 7-7 -7=Ripristino       -    NN     NN
145900000000- 8-8 -                   -    NN     NN
146000000000- 9-9 -                   -    NN     NN
146100000000-10-  -                   -    NN     NN
146200000000-11-  -                   -    NN     NN
146300000000-12-  -                   -    NN     NN
146400000000-13-  -                   -    NN     NN
146500000000-14-  -                   -    NN     NN
146600000000-15-  -                   -    NN     NN
146700000000-16-  -                   -    NN     NN
146800000000-17-  -                   -    NN     NN
146900000000-18-  -                   -    NN     NN
147000000000-19-  -                   -    NN     NN
147100000000-20-  -                   -    NN     NN
147200000000-21-  -                   -    NN     NN
147300000000-22-  -                   -    NN     NN
147400000000-23-  -                   -    NN     NN
147500000000-24-  -                   -    NN     NN
147600000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
147700000000F1    -                   -    NN     NN
147800000000F2    -                   -    NN     NN
147900000000F3    -F3=Fine            -    NN     NN
148000000000F4    -                   -    NN     NN
148100000000F5    -                   -    NN     NN
148200000000F6    -                   -    NN     NN
148300000000F7    -                   -    NN     NN
148400000000F8    -                   -    NN     NN
148500000000F9    -                   -    NN     NN
148600000000F10   -F10=Inserimento    -    SN     SN
148700000000F11   -                   -    NN     NN
148800000000F12   -F12=Ritorno        -    SS     SS
148900000000F13   -                   -    NN     NN
149000000000F14   -                   -    NN     NN
149100000000F15   -                   -    NN     NN
149200000000F16   -                   -    NN     NN
149300000000F17   -                   -    NN     NN
149400000000F18   -                   -    NN     NN
149500000000F19   -                   -    NN     NN
149600000000F20   -                   -    NN     NN
149700000000F21   -                   -    NN     NN
149800000000F22   -                   -    NN     NN
149900000000F23   -F23=Altre opzioni  -    NN     NN
150000000000F24   -F24=Altri tasti    -    NN     NN
150100000000ENTER -                   -    SS     SS
150200000000ROLLUP-                   -    SS     SS
150300000000ROLLDN-                   -    SS     SS
150400000000HELP  -                   -    NN     NN
150500000000PRINT -                   -    NN     NN
150600000000BSPACE-                   -    NN     NN
150700000000CLEAR -                   -    NN     NN
