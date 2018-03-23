000100970526     H*------------------------------------------------------------------------*
000200991025     H* Gestione - tipi file per internet -
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800991025     FTIS720D   CF   E             WORKSTN INFDS(DEVDS1)
000900991025     F                                     SFILE(s720D05:SFLULT)
001000991025     F                                     SFILE(s720DMS:MSGULT)
001100970526     F*------------
001200991025     Ftivtf01l  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400970528     D*------------
002500970528     D* COMANDI
002600970528     D*------------
002700970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI OPZIONI
003000970526     D*------------
003100970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI TASTI FUNZIONALI
003400970526     D*------------
003500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* OPZIONI DA CARICARE A VIDEO
003800970526     D*------------
003900970526     D OPV             S             19    DIM(24)
004000970526     D*------------
004100970526     D* FUNZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970527     D FUV             S             19    DIM(24)
004400970526     D*------------------------------------------------------------------------*
004500970526     D* INPUT
004600970526     D*------------------------------------------------------------------------*
004700970526     D*------------
004800970526     D* RIDEFINIZIONE OPZIONI VIDEO
004900970526     D*------------
005000970526     D OPDDS           DS
005100970526     D  OPDOPZ                 1      2
005200970526     D  OPDDE1                 3     19
005300970526     D  OPDOP1                21     22
005400970526     D  OPDFI0                23     34
005500970526     D  OPDATT                35     35
005600970526     D  OPDFI1                36     80
005700970526     D  OPDDES                 1     19
005800970526     D  OPDRIG                 1     80
005900970526     D*------------
006000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006100970526     D*------------
006200970526     D FUDDS           DS
006300970526     D  FUDDE1                 1      1
006400970526     D  FUDFUN                 2      3
006500970526     D  FUDDE2                 4     19
006600970526     D  FUDFI0                20     34
006700970526     D  FUDATT                35     35
006800970526     D  FUDFI1                36     80
006900970526     D  FUDDES                 1     19
007000970526     D  FUDRIG                 1     80
007100970526     D*------------
007200970526     D* DS GESTIONE TASTI FUNZIONALI
007300970526     D*------------
007400970526     D DSKEY         E DS
007500970526     D DEVDS1          DS
007600970526     D  KEY                  369    369
007700970526     D*------------
007800970526     D* DS "XMSG" - GESTIONE MESSAGGI
007900970526     D*------------
008000970526     D DSMSG         E DS
008100970526     D*------------
008200020916     D* DS REPERIMENTO DATI UTENTE
008300970526     D*------------
008400020916     D TIBS34DS      E DS                                                       *Profili utente
008500020916     D DDATIUTE      E DS                                                       *Dati utente
008600020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008700020916     D*------------
008800020916     D* DS X REPERIMENTO/CONTROLLO TABELLE
008900020916     D*------------
009000991108     D TIBS02DS      E DS
009100970527     D*------------------
009200970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009300970527     D*------------------
009400970527     D WLBDA8          DS                  INZ
009500970527     D  G08DAT                 1      8  0
009600970527     D  G08INV                 9     16  0
009700970527     D  G08ERR                17     17
009800970527     D  G08TGI                18     22  0
009900970526     D*------------
010000970526     D* DS DI PROCEDURA
010100991025     d tis720        e ds                  extname(tis720ds)
010200970526     D*------------
010300970526     D* ARCHITETTURA
010400970526     D*------------
010500970526     D KPJBA         E DS
010600970526     C*------------------------------------------------------------------------*
010700970526     C* MAIN LINES
010800970526     C*------------------------------------------------------------------------*
010900970526     C*
011000970526     C* SE NON CI SONO ERRORI, PROSEGUE
011100970526IF  1C     ERRL00        IFNE      '1'
011200970526     C*
011300970526     C* CARICA IL SFILE
011400970526     C                   EXSR      POSSFL
011500970526     C*
011600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011700970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012000970526     C*
012100970526     C* REIMPOSTA ERRORE DI PROCERURA
012200970526     C                   MOVEL     '0'           ERRL00
012300970526     C*
012400970526     C* EMETTE IL SFILE
012500970526     C                   EXSR      OUTSFL
012600970526     C*
012700970526     C* GESTIONE TASTI FUNZIONALI
012800970526     C                   EXSR      FUNCON
012900970526     C*
013000970526E   2C                   ENDDO
013100970527     C*
013200970527     C* CI SONO ERRORI IN ENTRATA
013300970527X   1C                   ELSE
013400991025     C                   MOVEL     '1'           s20ERR
013500991025     C                   MOVEL     DSMSMS        s20MSG
013600970526E   1C                   ENDIF
013700970526     C* FINE PGM
013800970526     C                   SETON                                        LR
013900970526     C*------------------------------------------------------------------------*
014000970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014100970526     C*------------------------------------------------------------------------*
014200970526     C     F03GES        BEGSR
014300970526     C*
014400970526     C* IMPOSTA "PREMUTO F03"
014500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014600991025     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
014700970526     C*
014800970526     C                   ENDSR
014900970526     C*------------------------------------------------------------------------*
015000970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015100970526     C*------------------------------------------------------------------------*
015200970526     C     F05GES        BEGSR
015300970526     C*
015400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
015900970526     C*------------------------------------------------------------------------*
016000970526     C     F06GES        BEGSR
016100970526     C*
016200970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
016300020916IF  1C     DUTLPO        IFNE      'S'
016400970526     C                   MOVEL     '1'           ERRL00
016500970526     C                   MOVEL     MSG(3)        DSMSMS
016600970526E   1C                   ENDIF
016700970526     C*
016800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016900991025     C                   MOVEL     *BLANKS       Dtip
017000970602     C                   MOVEL     DEPOP0        WOP0
017100970529     C                   MOVEL     'F06'         WOP1
017200991025     C                   MOVEL     'TIS721R'     WPGM              8
017300970526     C                   EXSR      LANPGM                                       *LANCIA PGM
017400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017500970526E   1C                   ENDIF
017600970526     C*
017700970526     C                   ENDSR
017800970526     C*------------------------------------------------------------------------*
017900970526     C* F12GES - GESTIONE F12 -> RITORNO
018000970526     C*------------------------------------------------------------------------*
018100970526     C     F12GES        BEGSR
018200970526     C*
018300970526     C* IMPOSTA "PREMUTO F12"
018400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018500991025     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
018600970526     C*
018700970526     C                   ENDSR
018800970528     C*------------------------------------------------------------------------*
018900970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
019000970528     C*------------------------------------------------------------------------*
019100970528     C     F21GES        BEGSR
019200970612     C*
019300991025     C                   MOVEL     *BLANKS       Dtip
019400970612     C                   MOVEL     DEPOP0        WOP0
019500970612     C                   MOVE      '03'          WOP0
019600970612     C                   MOVEL     'F21'         WOP1
019700991025     C*                  MOVEL     'TIBS14R'     WPGM              8
019800991025     C*                  EXSR      LANPGM
019900970528     C*
020000970528     C                   ENDSR
020100970528     C*------------------------------------------------------------------------*
020200970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
020300970528     C*------------------------------------------------------------------------*
020400970528     C     F22GES        BEGSR
020500970528     C*
020600970528     C                   Z-ADD     80            LENGH            15 5
020700970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
020800970528     C                   PARM                    CMD(1)
020900970528     C                   PARM                    LENGH
021000970528     C*
021100970528     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021400970526     C*------------------------------------------------------------------------*
021500970526     C     F23GES        BEGSR
021600970526     C*
021700970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
021800970526     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022200970526     C*------------------------------------------------------------------------*
022300970526     C     F24GES        BEGSR
022400970526     C*
022500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022600970526     C*
022700970526     C                   ENDSR
022800970526     C*------------------------------------------------------------------------*
022900970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023000970526     C*------------------------------------------------------------------------*
023100970526     C     RUPGES        BEGSR
023200970526     C*
023300970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023400970526     C*
023500970526     C                   ENDSR
023600970526     C*------------------------------------------------------------------------*
023700970526     C* ENTGES - GESTIONE ENTER -> INVIO
023800970526     C*------------------------------------------------------------------------*
023900970526     C     ENTGES        BEGSR
024000970526     C*
024100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024200970526     C                   EXSR      NEWPOS
024300970526     C*
024400970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
024500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024600970526IF  2C     SWPOSI        IFNE      0
024700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
024800970526X   2C                   ELSE
024900970526IF  3C     SFLULT        IFNE      0
025000970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025100970526     C*
025200970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
025300970526IF  4C     SWMODI        IFNE      0
025400970526     C                   EXSR      POSSFL
025500970526E   4C                   ENDIF
025600970526E   3C                   ENDIF
025700970526E   2C                   ENDIF
025800970526E   1C                   ENDIF
025900970526     C*
026000970526     C                   ENDSR
026100970526     C*------------------------------------------------------------------------*
026200970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
026300970526     C*------------------------------------------------------------------------*
026400970526     C     O01GES        BEGSR
026500970526     C*
026600970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
026700991025     C                   MOVEL     vidtip        s20tip
026800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
026900970526     C*
027000970526     C                   ENDSR
027100970526     C*------------------------------------------------------------------------*
027200970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
027300970526     C*------------------------------------------------------------------------*
027400970526     C     O02GES        BEGSR
027500970526     C*
027600970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
027700991025IF  1C*    VIDvat        IFEQ      'A'
027800991025     C*                  MOVEL     '1'           ERRL00
027900991025     C*                  MOVEL     MSG(4)        DSMSMS
028000991025E   1C*                  ENDIF
028100970527     C* MODIFICA
028200970526IF  1C     ERRL00        IFEQ      '0'
028300991025     C                   MOVEL     VIDtIP        DTIP
028400970602     C                   MOVEL     DEPOP0        WOP0
028500970526     C                   MOVEL     'O02'         WOP1
028600991025     C                   MOVEL     'TIS721R'     WPGM
028700970526     C                   EXSR      LANPGM
028800991012     c                   z-add     1             swmodi
028900970526E   1C                   ENDIF
029000970526     C*
029100970526     C                   ENDSR
029200991014     C*------------------------------------------------------------------------*
029300991014     C* O03GES - GESTIONE OPZIONE 3=copia
029400991014     C*------------------------------------------------------------------------*
029500991014     C     O03GES        BEGSR
029600991014     C*
029700991014     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
029800991014     C* copia
029900991014IF  1C     ERRL00        IFEQ      '0'
030000991025     C                   MOVEL     VIDtIP        DTIP
030100991014     C                   MOVEL     DEPOP0        WOP0
030200991014     C                   MOVEL     'O03'         WOP1
030300991025     C                   MOVEL     'TIS721R'     WPGM
030400991014     C                   EXSR      LANPGM
030500991014     c                   z-add     1             swmodi
030600991014E   1C                   ENDIF
030700991014     C*
030800991014     C                   ENDSR
030900970526     C*------------------------------------------------------------------------*
031000970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
031100970526     C*------------------------------------------------------------------------*
031200991104     C*    O04GES        BEGSR
031300970526     C*
031400970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
031500970527     C* ANNULLAMENTO
031600991104IF  1C*    ERRL00        IFEQ      '0'
031700991104     C*                  MOVEL     VIDtIP        DTIP
031800991104     C*                  MOVEL     DEPOP0        WOP0
031900991104     C*                  MOVEL     'O04'         WOP1
032000991104     C*                  MOVEL     'TIS721R'     WPGM
032100991104     C*                  EXSR      LANPGM
032200991104     c*                  z-add     1             swmodi
032300991104E   1C*                  ENDIF
032400970526     C*
032500991104     C*                  ENDSR
032600970526     C*------------------------------------------------------------------------*
032700970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
032800970526     C*------------------------------------------------------------------------*
032900970526     C     O05GES        BEGSR
033000970526     C*
033100991025     C                   MOVEL     VIDtIP        DTIP
033200970602     C                   MOVEL     DEPOP0        WOP0
033300970526     C                   MOVEL     'O05'         WOP1
033400991025     C                   MOVEL     'TIS721R'     WPGM
033500970526     C                   EXSR      LANPGM
033600970526     C*
033700970526     C                   ENDSR
033800970526     C*------------------------------------------------------------------------*
033900970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
034000970526     C*------------------------------------------------------------------------*
034100970526     C     O07GES        BEGSR
034200970526     C*
034300970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
034400970526     C* RIPRISITINO
034500991025IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
034600991025     c*                  exsr      movcamp
034700991025     C*                  MOVEL     DEPOP0        WOP0
034800991025     C*                  MOVEL     'O07'         WOP1
034900991025     C*                  MOVEL     'TIBS13R'     WPGM
035000991025     C*                  EXSR      LANPGM
035100991025     C*                  Z-ADD     1             SWMODI                     L
035200991025E   1C*                  ENDIF
035300970526     C*
035400970526     C                   ENDSR
035500991001      *
035600991025     C*------------------------------------------------------------------------*
035700991025     C* O09GES - GESTIONE OPZIONE 9=Regole clienti
035800991025     C*------------------------------------------------------------------------*
035900991025     C     O09GES        BEGSR
036000991025     C*
036100991025     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
036200991025     C* RIPRISITINO
036300991025IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036400991025     c                   move      vidtip        dtip
036500991025     C                   MOVEL     DEPOP0        WOP0
036600991025     C                   MOVEL     'O09'         WOP1
036700991025     C                   MOVEL     'TIS722R'     WPGM
036800991025     C                   EXSR      LANPGM
036900991025     C                   Z-ADD     1             SWMODI                     L
037000991025E   1C                   ENDIF
037100991025     C
037200991025     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
037500970526     C*------------------------------------------------------------------------*
037600970526     C     NEWPOS        BEGSR
037700970526     C*
037800970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
037900970526     C*         1= RICHIESTO POSIZIONAMENTO
038000970526     C                   Z-ADD     0             SWPOSI            7 0
038100970526     C*
038200970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
038300970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
038400970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
038500970526     C                   Z-ADD     0             SWMODI            1 0
038600970530     C*
038700970530     C* REIMPOSTA INDICATORI DI ERRORE
038800970530     C                   SETOFF                                       808182
038900970530     C                   SETOFF                                       83
039000970526     C*
039100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
039200991025IF  1C     VIDti2        IFNE      depti2
039300000731     C     VIDti1        ORNE      depti1
039400000731     C     VIDop1        ORNE      depope
039401070424     C     VIDfi1        ORNE      depfi1
039500970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
039600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
039700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
039800991025     C                   MOVEL     VIDti2        DEPti2                         *DEPOSITI
039900000731     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
040000000731     C                   MOVEL     VIDop1        DEPope                         *DEPOSITI
040001070424     C                   MOVEL     VIDfi1        DEPfi1                         *DEPOSITI
040100970526E   2C                   ENDIF
040200970526E   1C                   ENDIF
040300970526     C*
040400970526     C                   ENDSR
040500970526     C*------------------------------------------------------------------------*
040600970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
040700970526     C*------------------------------------------------------------------------*
040800970528     C     SETFIL        BEGSR
040900970526     C*
041000970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
041100991025     C                   MOVEL     VIDti2        KCEtip
041200991025IF  1C     WTIPRI        IFEQ      'TIP'                                        *PER PARTENZA
041300991025     C     KEYvtf        SETLL     tivtf01l                           99
041400970528E   1C                   ENDIF
041500980107     C*
041600970526     C* LETTURA RECORD
041700970526IF  1C     *IN99         IFEQ      *OFF
041800970526     C                   EXSR      LETFIL
041900970526E   1C                   ENDIF
042000970526     C*
042100970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
042200970526IF  1C     *IN99         IFEQ      *ON
042300970526     C                   Z-ADD     1             SFLEOF
042400970526E   1C                   ENDIF
042500970526     C*
042600970526     C                   ENDSR
042700970526     C*------------------------------------------------------------------------*
042800970526     C* LETFIL - LETTURA RECORD
042900970526     C*------------------------------------------------------------------------*
043000970526     C     LETFIL        BEGSR
043100970526     C*
043200970526     C* LEGGE FINO A:
043300970526     C*     - FINE FILE
043400970526     C*     - TROVATO RECORD VALIDO
043500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
043600970526DO  1C     *IN99         DOWEQ     *OFF
043700970526     C     WRECOK        ANDEQ     'N'
043800970526     C*
043900970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
044000991025IF  2C     WTIPRI        IFEQ      'TIP'                                        *PER PARTENZA
044100991025     C                   READ      tivtf01l                               99
044200970528E   2C                   ENDIF
044300970528     C*
044400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
044500970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
044600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
044700970530X   2C                   ELSE
044800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
044900970530E   2C                   ENDIF
045000970526E   1C                   ENDDO
045100970526     C*
045200970526     C                   ENDSR
045300970526     C*------------------------------------------------------------------------*
045400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
045500970526     C*------------------------------------------------------------------------*
045600970526     C     CHKREC        BEGSR
045700970526     C*
045800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
045900000731     C*
046000010117     C     WRECOK        IFEQ      'S'
046100010117     C     vidti1        ifeq      *blank                                       *ALLA DATA
046200010117     C                   else
046300010117     C                   if        %subst(vidti1:1:1) = '*'
046400010117     C                   if        %subst(vidti1:2:1) <>
046500010117     C                             %subst(vtftip:2:1)
046600010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
046700010117     C                   endif
046800010117     C                   else
046900010117     C                   if        %subst(vidti1:2:1) = '*'
047000010117     C                   if        %subst(vidti1:1:1) <>
047100010117     C                             %subst(vtftip:1:1)
047200010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
047300010117     C                   endif
047400010117     C                   else
047500010117     C                   if        vidti1 <> vtftip
047600010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
047700010117     C                   endif
047800010117     C                   endif
047900010117     C                   endif
048000010117     C                   endif
048100010117     C                   ENDIF
048200000731     C*
048300000731     C     WRECOK        IFEQ      'S'
048400000731     C     VIDop1        IFNE      *blanks
048500000731     C     VTFUD         IFNE      VIDop1
048600000731     C                   MOVEL     'N'           WRECOK
048700000731     C                   ENDIF
048800000731     C                   ENDIF
048900000731     C                   ENDIF
048901070424     C*
048902070424     C     WRECOK        IFEQ      'S'
048903070424     C     VIDfi1        IFNE      *blanks
048904070424     C                   IF        %scan(%trim(VIDfi1):VTFFID) > 0 OR
048905070424     C                             %scan(%trim(VIDfi1):VTFFIS) > 0 OR
048906070424     C                             %scan(%trim(VIDfi1):VTFFIT) > 0
048916070424     C                   ELSE
048917070424     C                   MOVEL     'N'           WRECOK
048918070424     C                   ENDIF
048919070424     C                   ENDIF
048920070424     C                   ENDIF
049000000731     C*
049100970526     C                   ENDSR
049200970526     C*------------------------------------------------------------------------*
049300970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
049400970526     C*------------------------------------------------------------------------*
049500970526     C     IRESFL        BEGSR
049600970526     C*
049700970526     C                   MOVEL     *BLANKS       OPZKEY
049800991025     C                   MOVEL     vtftip        VIDtip
049900991025     C                   MOVEL     vtfdes        VIDdes
050000991025     C                   MOVEL     vtfud         VIDud
050100991025     C                   MOVEL     vtffid        VIDfid
050200991025     C                   MOVEL     vtffis        VIDfis
050300991025     C                   MOVEL     vtfsnd        VIDsnd
050400991025     C                   MOVEL     vtffit        VIDfit
050500970526     C                   ENDSR
050600970526     C*------------------------------------------------------------------------*
050700970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
050800970526     C*------------------------------------------------------------------------*
050900970526     C     CHKPAR        BEGSR
050901070424     C*
050902070424     C                   setoff                                       868788
051000970528     C*
051100970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
051200991025     C                   MOVEL     'TIP'         WTIPRI            3
051300991025IF  1C*    VIDti2        IFEQ      *BLANKS
051400991025     C*                  MOVEL     'XXX'         WTIPRI
051500991025E   1C*                  ENDIF                                                  *ORDINAMENTO RICHIES
051600970528     C*
051700970528     C* CONTROLLA IL TIPO RICERCA
051800991012IF  1C     ERRL00        IFEQ      '0'
051900991025IF  2C     VIDti2        IFNE      *blank
052000010117     C                   if        %scan('*':VIDti2) = 0
052100000731     C                   movel     'TIP'         WTIPRI
052200991108     c                   move      vidti2        wvidti2           2
052300991108     c                   exsr      cnttip
052400010117     C                   IF        ERRL00 ='1'
052500010117     C                   seton                                        86
052600010117     C                   ENDIF
052700991108     c                   move      wvidti2       vidti2
052800010117     C                   endif
052900991012E   2C                   ENDIF
053000010117     C*
053100000731IF  2C     VIDti1        IFNE      *blank
053200010117     C                   if        %scan('*':VIDti1) = 0
053300000731     C                   movel     'TIP'         WTIPRI
053400000731     c                   move      vidti1        wvidti2           2
053500000731     c                   exsr      cnttip
053600010117     C                   IF        ERRL00 ='1'
053700010117     C                   seton                                        87
053800010117     C                   ENDIF
053900000731     c                   move      wvidti2       vidti1
054000010117     C                   endif
054100000731E   2C                   ENDIF
054200991012E   1C                   ENDIF
054300010117     C*
054400970526     C                   ENDSR
054500991108     C*------------------------------------------------------------------------*
054600010117     C* Cnttip - CONTROLLA codice tipo file
054700991108     C*------------------------------------------------------------------------*
054800991108     c     cnttip        begsr
054900991108     c                   clear                   TIBS02DS
055000991108      *
055100991108     C     '?'           SCAN      wVidti2                                30
055200991108      *  Ricerca
055300991108     c                   eval      t02tla = 'L'
055400991108    1C                   IF        *IN30 = *ON
055500991108     c                   movel     'R'           t02mod
055600991108     c                   else
055700991108     c                   movel     'C'           t02mod
055800991108     c                   movel(p)  Wvidti2       t02ke1
055900991108     c                   end
056000991108     C                   movel     'ITF'         t02cod
056100991108     c                   movel     knsif         t02sif
056200991108     C                   CALL      'TIBS02R'
056300991108     c                   parm                    KPJBA
056400991108     c                   parm                    TIBS02DS
056500991108     c
056600991108      *
056700991108    2C                   IF        T02err = *blanks
056800991108     C                   movel     T02ke1        wVidti2
056900991108     c                   else
057000991108     c                   movel     '1'           errl00
057100991108     C                   MOVEL     msg(11)       DSMSMS
057200991108     C                   ENDIF
057300991108     c
057400991108     c                   endsr
057500970526     C*------------------------------------------------------------------------*
057600970526     C* LANPGM - LANCIA UN PROGRAMMA
057700970526     C*------------------------------------------------------------------------*
057800970526     C     LANPGM        BEGSR
057900970526     C*
058000970526     C* IMPOSTA LA DS DI PROCEDURA
058100991025     C                   MOVEL     WOP0          s20OP0                         *LIVELLO PROCEDURA
058200991025     C                   MOVEL     WOP1          s20OP1                         *LIVELLO PGM
058300991025     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
058400991025     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
058500991025     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
058600991025     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
058700991025     C                   MOVEL     Dtip          s20tip                         *PARTENZA/ARRIVO
058800970526     C*
058900991028     c                   movel     tis720        kpjbu
059000970526     C                   CALL      WPGM
059100970612     C                   PARM                    KPJBA
059200991028     C*                  PARM                    tis720
059300970526     C*
059400970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
059500991025IF  1C     s20F03        IFEQ      '1'                                          *PREMUTO F03
059600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
059700970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
059800970526X   1C                   ELSE                                                   *NON PREMUTO F03
059900991025     C                   MOVEL     s20MSG        DSMSMS                         *MESSAGGIO DI RITORN
060000970526E   1C                   ENDIF
060100970526     C*
060200970526     C                   ENDSR
060300970526     C*------------------------------------------------------------------------*
060400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
060500970526     C*------------------------------------------------------------------------*
060600970526     C     CHKFOP        BEGSR
060700970626     C*
060800970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
060900970626IF  1C     DEPOP0        IFEQ      'P01'
061000970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
061100970626     C                   MOVEL     'S'           OPDATT
061200970626     C                   MOVEL     OPDRIG        OPD(1)
061300970626X   1C                   ELSE
061400970626     C* OPZIONI
061500970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
061600970626     C                   MOVEL     'S'           OPDATT
061700970626     C                   MOVEL     OPDRIG        OPD(5)
061800970626     C* FUNZIONI
061900970626E   1C                   ENDIF
062000970626     C*
062100970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
062200970626IF  1C     DEPOP0        IFEQ      'P02'
062300970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
062400970626     C                   MOVEL     'S'           OPDATT
062500970626     C                   MOVEL     OPDRIG        OPD(2)
062600991014     C                   MOVEL     OPD(3)        OPDRIG                         *MODIFICA
062700991014     C                   MOVEL     'S'           OPDATT
062800991014     C                   MOVEL     OPDRIG        OPD(3)
062900991104     C*                  MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
063000991104     C*                  MOVEL     'S'           OPDATT
063100991104     C*                  MOVEL     OPDRIG        OPD(4)
063200970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
063300970626     C                   MOVEL     'S'           OPDATT
063400970626     C                   MOVEL     OPDRIG        OPD(7)
063500991025     C                   MOVEL     OPD(9)        OPDRIG                         *RIPRISTINO
063600991025     C                   MOVEL     'S'           OPDATT
063700991025     C                   MOVEL     OPDRIG        OPD(9)
063800970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
063900970626     C                   MOVEL     'S'           FUDATT
064000970626     C                   MOVEL     FUDRIG        FUD(6)
064100970626E   1C                   ENDIF
064200970626     C*
064300970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
064400020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
064500991104     C*                  MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
064600991104     C*                  MOVEL     'N'           OPDATT
064700991104     C*                  MOVEL     OPDRIG        OPD(4)
064800970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
064900970626     C                   MOVEL     'N'           OPDATT
065000970626     C                   MOVEL     OPDRIG        OPD(7)
065100970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
065200970626     C                   MOVEL     'N'           FUDATT
065300970626     C                   MOVEL     FUDRIG        FUD(6)
065400970626E   1C                   ENDIF
065500970526     C*
065600970526     C* GESTIONE OPZIONI A VIDEO
065700970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
065800970526     C                   EXSR      OPZGES
065900970526     C*
066000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
066100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
066200970526     C                   EXSR      FUNGES
066300970526     C*
066400970526     C                   ENDSR
066500020916     C*--------------------------------------------------------------------------------------------*
066600020916     C* REPERISCE I DATI UTENTE
066700020916     C*--------------------------------------------------------------------------------------------*
066800020916     C     REPDATIUTE    BEGSR
066900020916     C*
067000020916     C* INIZIALIZZA VARIABILI DI WRK
067100020916     C                   CLEAR                   TIBS34DS
067200020916     C                   CLEAR                   AZUTEDS
067300020916     C                   CLEAR                   DDATIUTE
067400020916     C*
067500020916     C     *DTAARA       DEFINE    §azute        azuteds
067600020916     C     *DTAARA       DEFINE    §datiute      ddatiute
067700020916     C                   IN(E)     *DTAARA
067800020916if  1C                   IF        %Error
067900020916     c                   EVAL      I34Tla = 'L'
068000020916     C                   CALL      'TIBS34R'
068100020916     C                   PARM                    Tibs34Ds
068200020916     C                   IN        *DTAARA
068300020916e   1C                   ENDIF
068400020916     C*
068500020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
068600020916     C                   MOVEL(P)  RSUT          DSFIRS           20
068700020916     C*
068800020916     C                   ENDSR
068900970526     C*------------------------------------------------------------------------*
069000970526     C* *INZSR - OPERAZIONI INIZIALI
069100970526     C*------------------------------------------------------------------------*
069200970526     C     *INZSR        BEGSR
069300970526     C*---
069400970526     C* RICEVIMENTO PARAMETRI
069500970526     C*---
069600970526     C     *ENTRY        PLIST
069700970612     C                   PARM                    KPJBA
069800991025     c                   clear                   tis720
069900991025     c                   movel     kpjbu         s20op0
070000970526     C*
070100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
070200970526     C     XMSGPA        PLIST
070300970526     C                   PARM                    DSMSMS
070400970526     C                   PARM                    DSMSPG
070500970526     C                   PARM                    DSMSKE
070600970526     C                   PARM                    DSMSER
070700970526     C*
070800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
070900970526     C                   CALL      'XKEY'
071000970526     C                   PARM                    DSKEY
071100970526     C*---
071200970526     C* VARIABILI NON RIFERITE AL DATA BASE
071300970526     C*---
071400970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
071500970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
071600970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
071700970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
071800970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
071900970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
072000970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
072100970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
072200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
072300991025     C     *LIKE         DEFINE    s20OP0        DEPOP0                         *DI DEPOSITO
072400991025     C     *LIKE         DEFINE    s20OP1        DEPOP1
072500991025     C     *LIKE         DEFINE    s20OP0        WOP0                           *DI LAVORO
072600991025     C     *LIKE         DEFINE    s20OP1        WOP1
072700991025     C     *LIKE         DEFINE    s20tip        Dtip
072800970526     C*---
072900991025     C     *LIKE         DEFINE    s20tip        Deptip
073000991025     C     *LIKE         DEFINE    s20tip        Depti2
073100000731     C     *LIKE         DEFINE    s20tip        Depti1
073200991025     C     *LIKE         DEFINE    s20tip        kcetip
073201070424     C     *LIKE         DEFINE    VIDfi1        Depfi1
073300970526     C* VARIABILI DI CONTROLLO
073400970526     C*---
073500970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
073600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
073700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
073800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
073900970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
074000000731     C                   MOVEL     *blanks       Depope            1            *TIPO OPERAZIONE
074100970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
074200970526     C*
074300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
074400991025     C                   MOVEL     'TIS720R'     DSMSPG
074500991025     C                   MOVEL     'TIS720R'     MSGQUE
074600970526     C                   MOVEL     *BLANKS       MSGKEY
074700970526     C                   MOVEL     *BLANKS       DSMSMS
074800970526     C                   MOVEL     *BLANKS       DSMSKE
074900970526     C                   Z-ADD     *ZEROS        DSMSER
075000970526     C*---
075100970526     C* VARIABILI RIFERITE AL DATA BASE
075200970526     C*---
075300970526     C*---
075400970526     C* CHIAVI DI LETTURA
075500970526     C*---
075600040901     C* LETTURA tivtf01L -
075700991025     C     KEYvtf        KLIST
075800991025     C                   KFLD                    KCEtip                         *PARTENZA/ARRIVO
075900970527     C*
076000970527     C* REPERIMENTO DATI UTENTE
076100020916     C                   EXSR      REPDATIUTE
076200970527     C*
076300970527     C* CARICA LE TABELLE OCCORRENTI
076400991025     C*                  EXSR      CARTAB
076500970526     C*
076600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
076700991025     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
076800991025     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
076900970526     C*
077000970526     C* CALCOLA LA DATA CORRENTE
077100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
077200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
077300970526     C                   Z-ADD     WN8           G08DAT
077400970526     C                   Z-ADD     *ZEROS        G08INV
077500970526     C                   MOVEL     '0'           G08ERR
077600970526     C                   CALL      'XSRDA8'
077700970526     C                   PARM                    WLBDA8
077800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
077900970526     C*
078000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
078100000731     C                   MOVE      *blank        VIDti1
078200000731     C                   MOVE      *blank        VIDop1
078300000731     C                   MOVE      *blank        VIDti2
078301070424     C                   MOVE      *blank        VIDfi1
078400970526     C                   EXSR      CHKPAR
078500970526IF  1C     ERRL00        IFEQ      '1'
078600991025     C                   MOVEL     '1'           s20ERR
078700991025     C                   MOVEL     DSMSMS        s20MSG
078800970526X   1C                   ELSE
078900991025     C                   MOVEL     VIDtip        DEPtip                         *DEPOSITI
079000970526E   1C                   ENDIF
079100970526     C*
079200970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
079300970526     C                   EXSR      CHKFOP
079400970526     C*
079500970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
079600970530IF  1C     DEPOP0        IFEQ      'P01'
079700970530     C                   MOVEL     DEC(3)        VIDING
079800970530E   1C                   ENDIF
079900970530IF  1C     DEPOP0        IFEQ      'P02'
080000970526     C                   MOVEL     DEC(1)        VIDING
080100970530E   1C                   ENDIF
080200970530IF  1C     DEPOP0        IFEQ      'P05'
080300970526     C                   MOVEL     DEC(2)        VIDING
080400970526E   1C                   ENDIF
080500970526     C*
080600970526     C                   ENDSR
080700970526     C*------------------------------------------------------------------------*
080800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
081000970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
081100970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
081200970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
081300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
081400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
081500970526     C*------------------------------------------------------------------------*
081600970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
081700970526     C*------------------------------------------------------------------------*
081800970526     C     POSSFL        BEGSR
081900970526     C*
082000970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
082100970526     C                   EXSR      CLRSFL
082200970526     C*
082300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
082400970526     C                   EXSR      SETFIL
082500970526     C*
082600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
082700970526     C     SFLEOF        IFEQ      0
082800970526     C                   EXSR      CARSFL
082900970526     C                   END
083000970526     C*
083100970526     C                   ENDSR
083200970526     C*------------------------------------------------------------------------*
083300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
083400970526     C*------------------------------------------------------------------------*
083500970526     C     ENTATT        BEGSR
083600970526     C*
083700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
083800970526     C                   Z-ADD     0             OPZOK             1 0
083900970526     C*
084000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
084100970526     C                   Z-ADD     0             SFLVAR            5 0
084200970526     C*
084300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
084400970526     C                   Z-ADD     0             SFLERR
084500970526     C*
084600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
084700970526     C                   Z-ADD     SFLULT        WFLULT
084800970526     C*
084900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
085000991025     C                   READC     s720D05                                98
085100970526     C     *IN98         DOWEQ     '0'
085200970526     C*
085300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
085400970526     C                   ADD       1             SFLVAR
085500970526     C*
085600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
085700970526     C     OPZOK         IFEQ      0
085800970526     C*
085900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
086000970526     C     OPZKEY        IFNE      *BLANKS
086100970526     C                   EXSR      OPZCON
086200970526     C                   END
086300970526     C*
086400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
086500970526     C*            2. PULIZIA SCELTA IN SFL
086600970526     C     OPZOK         IFEQ      0
086700970526     C                   MOVEL     '0'           *IN41
086800970526     C                   MOVEL     *BLANKS       OPZKEY
086900970526     C                   ELSE
087000970526     C*
087100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
087200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
087300970526     C                   MOVEL     '1'           *IN41
087400970526     C                   Z-ADD     SFLULT        SFLERR
087500970526     C                   END
087600970526     C                   ELSE
087700970526     C*
087800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
087900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
088000970526     C                   MOVEL     '1'           *IN41
088100970526     C                   END                                                    OPZOK=0
088200970526     C*
088300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
088400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
088500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
088600970526     C     OPZOK         IFNE      0
088700970526     C                   Z-ADD     SFLERR        SFLNBR
088800970526     C                   ELSE
088900970526     C                   Z-ADD     SFLULT        SFLNBR
089000970526     C                   END
089100970526     C*
089200970526     C* AGGIORNO RECORD SFL
089300991025     C                   UPDATE    s720D05
089400970526     C*
089500991025     C                   READC     s720D05                                98
089600970526     C                   END                                                    DO WHILE
089700970526     C*
089800970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
089900970526     C                   Z-ADD     WFLULT        SFLULT
090000970526     C*
090100970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
090200970526     C     SFLVAR        IFEQ      0
090300970526     C*---                 MOVEL'1'       FINL00
090400970526     C                   END
090500970526     C*
090600970526     C                   ENDSR
090700970526     C*------------------------------------------------------------------------*
090800970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
090900970526     C*------------------------------------------------------------------------*
091000970526     C     CLRSFL        BEGSR
091100970526     C*
091200970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
091300970526     C* (0=NON FINE FILE  1=FINE FILE)
091400970526     C                   Z-ADD     0             SFLEOF
091500970526     C*
091600970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
091700970526     C                   Z-ADD     0             SFLULT
091800970526     C*
091900970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
092000970526     C                   MOVEL     '1'           *IN40
092100991025     C                   WRITE     s720D06
092200970526     C                   MOVEL     '0'           *IN40
092300970526     C*
092400970526     C                   ENDSR
092500970526     C*------------------------------------------------------------------------*
092600970526     C* CARSFL - CARICAMENTO SFL
092700970526     C*------------------------------------------------------------------------*
092800970526     C     CARSFL        BEGSR
092900970526     C*
093000970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
093100970526     C     SFLULT        IFEQ      0
093200970526     C                   EXSR      SETFIL
093300970526     C                   END
093400970526     C*
093500970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
093600970526     C                   Z-ADD     SFLULT        WFLULT
093700970526     C*
093800970526     C* CICLO:
093900970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
094000970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
094100970526     C                   Z-ADD     1             SFLRIG
094200970526     C     SFLRIG        DOWLE     SFLPAG
094300970526     C     SFLEOF        ANDEQ     0
094400970526     C*
094500970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
094600970526     C                   MOVEL     *BLANKS       OPZKEY
094700970526     C*
094800970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
094900970526     C                   EXSR      IRESFL
095000970526     C*
095100970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
095200970526     C                   ADD       1             SFLULT
095300970526     C*
095400970526     C* SCRITTURA RECORD SFL
095500991025     C                   WRITE     s720D05
095600970526     C                   ADD       1             SFLRIG
095700970526     C*
095800970526     C* LETTURA FILE DA CARICARE
095900970526     C                   EXSR      LETFIL
096000970526     C                   END                                                    DO WHILE
096100970526     C*
096200970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
096300970526     C     WFLULT        IFNE      SFLULT
096400970526     C                   Z-ADD     WFLULT        SFLNBR
096500970526     C                   ADD       1             SFLNBR
096600970526     C                   END
096700970526     C*
096800970526     C                   ENDSR
096900970526     C*------------------------------------------------------------------------*
097000970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
097100970526     C*------------------------------------------------------------------------*
097200970526     C     OUTSFL        BEGSR
097300970526     C*
097400970526     C* WRITE RECORD TESTATA VIDEO
097500991025     C                   WRITE     s720D01
097600970526     C*
097700970526     C* VISUALIZZAZIONE MESSAGGIO
097800970526     C                   EXSR      SNDMSG
097900970526     C*
098000970526     C* WRITE RECORD OPZIONI VIDEO
098100991025     C                   WRITE     s720D02
098200970526     C*
098300970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
098400991025     C                   WRITE     s720D03
098500970526     C*
098600970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
098700991025     C                   WRITE     s720D04
098800970526     C*
098900970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
099000970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
099100970526     C     SFLEOF        IFEQ      0
099200970526     C                   MOVEL     '0'           *IN42
099300970526     C                   ELSE
099400970526     C*
099500970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
099600970526     C                   MOVEL     '1'           *IN42
099700970526     C                   END
099800970526     C*
099900970526     C* WRITE RECORD TASTI FUNZIONALI
100000991025     C                   WRITE     s720D09
100100970526     C*
100200970526     C* CASO DI SFL VUOTO
100300970526     C     SFLULT        IFLE      0
100400970526     C*
100500970526     C*    - WRITE RECORD VUOTO
100600991025     C                   WRITE     s720D0A
100700991025     C                   READ      s720D0A                                99
100800970526     C*
100900970526     C                   ELSE
101000970526     C*
101100970526     C* CASO DI SFL CON RECORDS
101200970526     C*
101300970526     C*    - WRITE RECORD CONTROLLO SFL
101400991025     C                   WRITE     s720D06
101500991025     C                   READ      s720D06                                99
101600970526     C                   END
101700970526     C*
101800970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
101900991025     C                   READ      s720D03                                99
102000970526     C*
102100970526     C                   ENDSR
102200970526     C*------------------------------------------------------------------------*
102300970526     C* SNDMSG - MANDA MESSAGGIO
102400970526     C*------------------------------------------------------------------------*
102500970526     C     SNDMSG        BEGSR
102600970526     C*
102700970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
102800970526     C                   MOVEL     '1'           *IN40
102900991025     C                   WRITE     s720DMC
103000970526     C                   MOVEL     '0'           *IN40
103100970526     C*
103200970526     C* VISUALIZZAZIONE MESSAGGIO
103300970526     C                   Z-ADD     0             MSGULT
103400970526     C                   CALL      'XMSG'        XMSGPA
103500970526     C     DSMSER        IFEQ      0
103600970526     C                   MOVEL     DSMSKE        MSGKEY
103700970526     C                   END
103800970526     C                   Z-ADD     1             MSGULT
103900991025     C                   WRITE     s720DMS                                      SFL MESS
104000991025     C                   WRITE     s720DMC                                      CONTR MESS
104100970526     C*
104200970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
104300970526     C                   MOVEL     *BLANKS       DSMSMS
104400970526     C*
104500970526     C                   ENDSR
104600970526     C*------------------------------------------------------------------------*
104700970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
104800970526     C*------------------------------------------------------------------------*
104900970526     C     FUNCON        BEGSR
105000970526     C*
105100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
105200970526     C                   EXSR      FUNVAR
105300970526     C*
105400970526     C* PULISCO CAMPI DI LAVORO
105500970526     C                   Z-ADD     0             I                 5 0
105600970526     C                   Z-ADD     0             W                 5 0
105700970526     C*
105800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
105900970526     C     FUNKEY        IFNE      *BLANKS
106000970526     C     1             DO        24            I
106100970526     C                   MOVEL     FUD(I)        FUDRIG
106200970526     C*
106300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
106400970526     C     FUNKEY        IFEQ      FUDFUN
106500970526     C     FUDATT        ANDEQ     'S'
106600970526     C                   Z-ADD     I             W
106700970526     C                   END
106800970526     C                   END                                                    DO WHILE
106900970526     C*
107000970526     C* FUNZIONE NON ATTIVA (ERRORE)
107100970526     C     W             IFEQ      0
107200970526     C*
107300970526     C* EMISSIONE MESSAGGIO DI ERRORE
107400970526     C                   MOVEL     MSG(2)        DSMSMS
107500970526     C                   END                                                    W=0
107600970526     C                   END                                                    FUNKEY NE *BLANKS
107700970526     C*
107800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
107900970526     C     W             IFNE      0
108000970526     C     FUNKEY        OREQ      *BLANKS
108100970526     C*
108200970526     C     FUNKEY        IFEQ      *BLANKS
108300970526     C                   EXSR      ENTGES
108400970526     C                   END
108500970526     C     FUNKEY        IFEQ      '03'
108600970526     C                   EXSR      F03GES
108700970526     C                   END
108800970526     C     FUNKEY        IFEQ      '05'
108900970526     C                   EXSR      F05GES
109000970526     C                   END
109100970526     C     FUNKEY        IFEQ      '06'
109200970526     C                   EXSR      F06GES
109300970526     C                   END
109400970526     C     FUNKEY        IFEQ      '12'
109500970526     C                   EXSR      F12GES
109600970526     C                   END
109700970528     C     FUNKEY        IFEQ      '21'
109800970528     C                   EXSR      F21GES
109900970528     C                   END
110000970528     C     FUNKEY        IFEQ      '22'
110100970528     C                   EXSR      F22GES
110200970528     C                   END
110300970526     C     FUNKEY        IFEQ      '23'
110400970526     C                   EXSR      F23GES
110500970526     C                   END
110600970526     C     FUNKEY        IFEQ      '24'
110700970526     C                   EXSR      F24GES
110800970526     C                   END
110900970526     C     FUNKEY        IFEQ      'RU'
111000970526     C                   EXSR      RUPGES
111100970526     C                   END
111200970526     C*
111300970526     C                   END
111400970526     C*
111500970526     C                   ENDSR
111600970526     C*------------------------------------------------------------------------*
111700970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
111800970526     C*------------------------------------------------------------------------*
111900970526     C     FUNVAR        BEGSR
112000970526     C*
112100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
112200970526     C     KEY           IFEQ      ENTER
112300970526     C                   MOVEL     *BLANKS       FUNKEY
112400970526     C                   END
112500970526     C     KEY           IFEQ      F1
112600970526     C                   MOVEL     '01'          FUNKEY
112700970526     C                   END
112800970526     C     KEY           IFEQ      F2
112900970526     C                   MOVEL     '02'          FUNKEY
113000970526     C                   END
113100970526     C     KEY           IFEQ      F3
113200970526     C                   MOVEL     '03'          FUNKEY
113300970526     C                   END
113400970526     C     KEY           IFEQ      F4
113500970526     C                   MOVEL     '04'          FUNKEY
113600970526     C                   END
113700970526     C     KEY           IFEQ      F5
113800970526     C                   MOVEL     '05'          FUNKEY
113900970526     C                   END
114000970526     C     KEY           IFEQ      F6
114100970526     C                   MOVEL     '06'          FUNKEY
114200970526     C                   END
114300970526     C     KEY           IFEQ      F7
114400970526     C                   MOVEL     '07'          FUNKEY
114500970526     C                   END
114600970526     C     KEY           IFEQ      F8
114700970526     C                   MOVEL     '08'          FUNKEY
114800970526     C                   END
114900970526     C     KEY           IFEQ      F9
115000970526     C                   MOVEL     '09'          FUNKEY
115100970526     C                   END
115200970526     C     KEY           IFEQ      F10
115300970526     C                   MOVEL     '10'          FUNKEY
115400970526     C                   END
115500970526     C     KEY           IFEQ      F11
115600970526     C                   MOVEL     '11'          FUNKEY
115700970526     C                   END
115800970526     C     KEY           IFEQ      F12
115900970526     C                   MOVEL     '12'          FUNKEY
116000970526     C                   END
116100970526     C     KEY           IFEQ      F13
116200970526     C                   MOVEL     '13'          FUNKEY
116300970526     C                   END
116400970526     C     KEY           IFEQ      F14
116500970526     C                   MOVEL     '14'          FUNKEY
116600970526     C                   END
116700970526     C     KEY           IFEQ      F15
116800970526     C                   MOVEL     '15'          FUNKEY
116900970526     C                   END
117000970526     C     KEY           IFEQ      F16
117100970526     C                   MOVEL     '16'          FUNKEY
117200970526     C                   END
117300970526     C     KEY           IFEQ      F17
117400970526     C                   MOVEL     '17'          FUNKEY
117500970526     C                   END
117600970526     C     KEY           IFEQ      F18
117700970526     C                   MOVEL     '18'          FUNKEY
117800970526     C                   END
117900970526     C     KEY           IFEQ      F19
118000970526     C                   MOVEL     '19'          FUNKEY
118100970526     C                   END
118200970526     C     KEY           IFEQ      F20
118300970526     C                   MOVEL     '20'          FUNKEY
118400970526     C                   END
118500970526     C     KEY           IFEQ      F21
118600970526     C                   MOVEL     '21'          FUNKEY
118700970526     C                   END
118800970526     C     KEY           IFEQ      F22
118900970526     C                   MOVEL     '22'          FUNKEY
119000970526     C                   END
119100970526     C     KEY           IFEQ      F23
119200970526     C                   MOVEL     '23'          FUNKEY
119300970526     C                   END
119400970526     C     KEY           IFEQ      F24
119500970526     C                   MOVEL     '24'          FUNKEY
119600970526     C                   END
119700970526     C     KEY           IFEQ      ROLLUP
119800970526     C                   MOVEL     'RU'          FUNKEY
119900970526     C                   END
120000970526     C     KEY           IFEQ      HELP
120100970526     C                   MOVEL     'HP'          FUNKEY
120200970526     C                   END
120300970526     C*
120400970526     C                   ENDSR
120500970526     C*------------------------------------------------------------------------*
120600970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
120700970526     C*------------------------------------------------------------------------*
120800970526     C     FUNGES        BEGSR
120900970526     C*
121000970526     C* PULISCO CAMPI DI LAVORO
121100970526     C                   Z-ADD     0             W                 5 0
121200970526     C*
121300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
121400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
121500970526     C     1             DO        24            I
121600970526     C                   MOVEL     FUD(I)        FUDRIG
121700970526     C*
121800970526     C* PULIZIA FUNZIONI PRECEDENTI
121900970526     C                   MOVEL     *BLANKS       FUV(I)
122000970526     C*
122100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
122200970526     C* (SALTA F23 E F24)
122300970526     C     FUDATT        IFEQ      'S'
122400970526     C     FUDFUN        ANDNE     '23'
122500970526     C     FUDFUN        ANDNE     '24'
122600970526     C                   ADD       1             W
122700970526     C                   MOVEL     FUDDES        FUV(W)
122800970526     C                   END
122900970526     C                   END                                                    DO WHILE
123000970526     C*
123100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
123200970526     C* ALTRIMENTI DISATTIVO F24
123300970526     C     1             DO        24            I
123400970526     C                   MOVEL     FUD(I)        FUDRIG
123500970526     C*
123600970526     C     FUDFUN        IFEQ      '24'
123700970526     C     W             IFGT      6
123800970526     C                   MOVEL     'S'           FUDATT
123900970526     C                   ELSE
124000970526     C                   MOVEL     *BLANKS       FUDATT
124100970526     C                   END                                                    W GT 8
124200970526     C                   MOVEL     FUDRIG        FUD(I)
124300970526     C                   END
124400970526     C                   END                                                    DO WHILE
124500970526     C*
124600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
124700970526     C     W             DIV       6             I
124800970526     C                   MVR                     Z
124900970526     C*
125000970526     C     Z             IFNE      0
125100970526     C     I             ADD       1             Z
125200970526     C                   ELSE
125300970526     C                   Z-ADD     I             Z
125400970526     C                   END
125500970526     C*
125600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
125700970526     C                   ADD       1             FUNGRU
125800970526     C     FUNGRU        IFGT      Z
125900970526     C     FUNGRU        ORLT      1
126000970526     C                   Z-ADD     1             FUNGRU
126100970526     C                   END
126200970526     C*
126300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
126400970526     C     FUNGRU        IFEQ      1
126500970526     C                   MOVEL     FUV(1)        FUN001
126600970526     C                   MOVEL     FUV(2)        FUN002
126700970526     C                   MOVEL     FUV(3)        FUN003
126800970526     C                   MOVEL     FUV(4)        FUN004
126900970526     C                   MOVEL     FUV(5)        FUN005
127000970526     C                   MOVEL     FUV(6)        FUN006
127100970526     C                   END
127200970526     C*
127300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
127400970526     C     FUNGRU        IFEQ      2
127500970526     C                   MOVEL     FUV(7)        FUN001
127600970526     C                   MOVEL     FUV(8)        FUN002
127700970526     C                   MOVEL     FUV(9)        FUN003
127800970526     C                   MOVEL     FUV(10)       FUN004
127900970526     C                   MOVEL     FUV(11)       FUN005
128000970526     C                   MOVEL     FUV(12)       FUN006
128100970526     C                   END
128200970526     C*
128300970526     C* TERZO GRUPPO DI 6 FUNZIONI
128400970526     C     FUNGRU        IFEQ      3
128500970526     C                   MOVEL     FUV(13)       FUN001
128600970526     C                   MOVEL     FUV(14)       FUN002
128700970526     C                   MOVEL     FUV(15)       FUN003
128800970526     C                   MOVEL     FUV(16)       FUN004
128900970526     C                   MOVEL     FUV(17)       FUN005
129000970526     C                   MOVEL     FUV(18)       FUN006
129100970526     C                   END
129200970526     C*
129300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
129400970526     C     FUNGRU        IFEQ      4
129500970526     C                   MOVEL     FUV(19)       FUN001
129600970526     C                   MOVEL     FUV(20)       FUN002
129700970526     C                   MOVEL     FUV(21)       FUN003
129800970526     C                   MOVEL     FUV(22)       FUN004
129900970526     C                   MOVEL     *BLANKS       FUN005
130000970526     C                   MOVEL     *BLANKS       FUN006
130100970526     C                   END
130200970526     C*
130300970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
130400970526     C                   MOVEL     *BLANKS       FUN007
130500970526     C                   MOVEL     *BLANKS       FUN008
130600970526     C*
130700970526     C     1             DO        24            I
130800970526     C                   MOVEL     FUD(I)        FUDRIG
130900970526     C     FUDFUN        IFEQ      '23'
131000970526     C     FUDATT        ANDEQ     'S'
131100970526     C                   MOVEL     FUDDES        FUN007
131200970526     C                   END
131300970526     C     FUDFUN        IFEQ      '24'
131400970526     C     FUDATT        ANDEQ     'S'
131500970526     C                   MOVEL     FUDDES        FUN008
131600970526     C                   END
131700970526     C                   END                                                    DO
131800970526     C*
131900970526     C                   ENDSR
132000970526     C*------------------------------------------------------------------------*
132100970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
132200970526     C*------------------------------------------------------------------------*
132300970526     C     OPZGES        BEGSR
132400970526     C*
132500970526     C* PULISCO CAMPI DI LAVORO
132600970526     C                   Z-ADD     0             W                 5 0
132700970526     C*
132800970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
132900970526     C* E CARICO TABELLA OPZIONI ATTIVE
133000970526     C     1             DO        24            I
133100970526     C                   MOVEL     OPD(I)        OPDRIG
133200970526     C*
133300970526     C* PULIZIA OPZIONI PRECEDENTI
133400970526     C                   MOVEL     *BLANKS       OPV(I)
133500970526     C*
133600970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
133700970526     C     OPDATT        IFEQ      'S'
133800970526     C                   ADD       1             W
133900970526     C                   MOVEL     OPDDES        OPV(W)
134000970526     C                   END
134100970526     C                   END                                                    DO WHILE
134200970526     C*
134300970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
134400970526     C* ALTRIMENTI DISATTIVO F23
134500970526     C     1             DO        24            I
134600970526     C                   MOVEL     FUD(I)        FUDRIG
134700970526     C*
134800970526     C     FUDFUN        IFEQ      '23'
134900970526     C     W             IFGT      8
135000970526     C                   MOVEL     'S'           FUDATT
135100970526     C                   ELSE
135200970526     C                   MOVEL     *BLANKS       FUDATT
135300970526     C                   END                                                    W GT 8
135400970526     C                   MOVEL     FUDRIG        FUD(I)
135500970526     C                   END
135600970526     C                   END                                                    DO WHILE
135700970526     C*
135800970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
135900970526     C     W             DIV       8             I
136000970526     C                   MVR                     Z                 5 0
136100970526     C*
136200970526     C     Z             IFNE      0
136300970526     C     I             ADD       1             Z
136400970526     C                   ELSE
136500970526     C                   Z-ADD     I             Z
136600970526     C                   END
136700970526     C*
136800970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
136900970526     C                   ADD       1             OPZGRU
137000970526     C     OPZGRU        IFGT      Z
137100970526     C     OPZGRU        ORLT      1
137200970526     C                   Z-ADD     1             OPZGRU
137300970526     C                   END
137400970526     C*
137500970526     C* PRIMO GRUPPO DI 8 OPZIONI
137600970526     C     OPZGRU        IFEQ      1
137700970526     C                   MOVEL     OPV(1)        OPZ001
137800970526     C                   MOVEL     OPV(2)        OPZ002
137900970526     C                   MOVEL     OPV(3)        OPZ003
138000970526     C                   MOVEL     OPV(4)        OPZ004
138100970526     C                   MOVEL     OPV(5)        OPZ005
138200970526     C                   MOVEL     OPV(6)        OPZ006
138300970526     C                   MOVEL     OPV(7)        OPZ007
138400970526     C                   MOVEL     OPV(8)        OPZ008
138500970526     C                   END
138600970526     C*
138700970526     C* SECONDO GRUPPO DI 8 OPZIONI
138800970526     C     OPZGRU        IFEQ      2
138900970526     C                   MOVEL     OPV(9)        OPZ001
139000970526     C                   MOVEL     OPV(10)       OPZ002
139100970526     C                   MOVEL     OPV(11)       OPZ003
139200970526     C                   MOVEL     OPV(12)       OPZ004
139300970526     C                   MOVEL     OPV(13)       OPZ005
139400970526     C                   MOVEL     OPV(14)       OPZ006
139500970526     C                   MOVEL     OPV(15)       OPZ007
139600970526     C                   MOVEL     OPV(16)       OPZ008
139700970526     C                   END
139800970526     C*
139900970526     C* TERZO GRUPPO DI 8 OPZIONI
140000970526     C     OPZGRU        IFEQ      3
140100970526     C                   MOVEL     OPV(17)       OPZ001
140200970526     C                   MOVEL     OPV(18)       OPZ002
140300970526     C                   MOVEL     OPV(19)       OPZ003
140400970526     C                   MOVEL     OPV(20)       OPZ004
140500970526     C                   MOVEL     OPV(21)       OPZ005
140600970526     C                   MOVEL     OPV(22)       OPZ006
140700970526     C                   MOVEL     OPV(23)       OPZ007
140800970526     C                   MOVEL     OPV(24)       OPZ008
140900970526     C                   END
141000970526     C*
141100970526     C                   ENDSR
141200970526     C*------------------------------------------------------------------------*
141300970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
141400970526     C*------------------------------------------------------------------------*
141500970526     C     OPZCON        BEGSR
141600970526     C*
141700970526     C* PULISCO CAMPI DI LAVORO
141800970526     C                   Z-ADD     0             I                 5 0
141900970526     C                   Z-ADD     0             W                 5 0
142000970526     C*
142100970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
142200970526     C     1             DO        24            I
142300970526     C                   MOVEL     OPD(I)        OPDRIG
142400970526     C*
142500970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
142600970526     C     OPZKEY        IFEQ      OPDOPZ
142700970526     C     OPZKEY        OREQ      OPDOP1
142800970526     C*
142900970526     C     OPDATT        IFEQ      'S'
143000970526     C                   MOVEL     OPDOPZ        OPZKEY
143100970526     C                   Z-ADD     I             W
143200970526     C                   END
143300970526     C                   END
143400970526     C*
143500970526     C                   END                                                    DO WHILE
143600970526     C*
143700970526     C* OPZIONE NON ATTIVA (ERRORE)
143800970526     C     W             IFEQ      0
143900970526     C*
144000970526     C* OPZIONE NON OK
144100970526     C                   Z-ADD     1             OPZOK
144200970526     C*
144300970526     C* EMISSIONE MESSAGGIO DI ERRORE
144400970526     C                   MOVEL     MSG(1)        DSMSMS
144500970526     C                   ELSE
144600970526     C*
144700970526     C* OPZIONE ATTIVA (PROCEDI ...)
144800970526     C*
144900970526     C     OPZKEY        IFEQ      ' 1'
145000970526     C                   EXSR      O01GES
145100970526     C                   END
145200970526     C     OPZKEY        IFEQ      ' 2'
145300970526     C                   EXSR      O02GES
145400970526     C                   END
145500991014     C     OPZKEY        IFEQ      ' 3'
145600991014     C                   EXSR      O03GES
145700991014     C                   END
145800991104     C*    OPZKEY        IFEQ      ' 4'
145900991104     C*                  EXSR      O04GES
146000991104     C*                  END
146100970526     C     OPZKEY        IFEQ      ' 5'
146200970526     C                   EXSR      O05GES
146300970526     C                   END
146400970526     C     OPZKEY        IFEQ      ' 7'
146500970526     C                   EXSR      O07GES
146600970526     C                   END
146700991025     C     OPZKEY        IFEQ      ' 9'
146800991025     C                   EXSR      O09GES
146900991025     C                   END
147000970526     C                   END                                                    OPZ.NON ATTIVA
147100970526     C*
147200970526     C                   ENDSR
147300970526     C*------------------------------------------------------------------------*
147400970526     C* NOTE COSTRUZIONE SCHIERE
147500970526     C*------------------------------------------------------------------------*
147600970526     C*
147700970526     C* -MSG-MESSAGGI
147800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
147900970526     C* (NON TOCCARE MSG,1 E MSG,2)
148000970526     C*
148100970526     C* -CMD-COMANDI
148200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
148300970526     C*
148400970526     C* -OPD-OPZIONI
148500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
148600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
148700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
148800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
148900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
149000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
149100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
149200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
149300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
149400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
149500970526     C*
149600970526     C* -FUD-FUNZIONI
149700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
149800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
149900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
150000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
150100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
150200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
150300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
150400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
150500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
150600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
150700970526     O*------------------------------------------------------------------------*
150800970526** DEC - DECODIFICHE
150900970526    GESTIONE                                                     1
151000970526INTERROGAZIONE                                                   2
151100970530     SCELTA                                                      3
1512009705264
1513009705265
151400970526** MSG - MESSAGGI
151500991025TIS720R- Opzione non gestita                                                   1
151600991025TIS720R- Tasto funzionale non gestito                                          2
151700991025TIS720R- Non abilitato alla gestione di questo CAP                             3
151800991025TIS720R- Record annullato. Consentito solo il ripristino                       4
151900991025TIS720R- Record non annullato. Impossibile ripristinare                        5
152000991025TIS720R- Accettato solo un parametro di inizio lista                           6
152100991025TIS720R- 7                                                                     7
152200991025TIS720R- 8                                                                     8
152300991025TIS720R- 9                                                                     9
152400991025TIS720R- 10                                                                   10
152500991108TIS720R- Tipo file non trovato in tabella
152600991025TIS720R- Data errata                                                          12
152700991025TIS720R- 13                                                                   13
152800991025TIS720R- 14                                                                   14
152900991025TIS720R- 15                                                                   15
153000991025TIS720R- 16                                                                   16
153100991025TIS720R- 17                                                                   17
153200991025TIS720R- 18                                                                   18
153300991025TIS720R- 19                                                                   19
153400991025TIS720R- 20                                                                   20
153500970528** CMD - COMANDI CL
153600970528WRKSPLF
153700970528
153800970528
153900970528
154000970528
154100970526** OPD - OPZIONI
154200970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
154300970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
154400991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
154500991104 4=                -4 -     (S/N)=N                        opz. 04¬
154600970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
154700970526                   -6 -     (S/N)=N                        opz. 06¬
154800970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
154900970527                   -8 -     (S/N)=N                        opz. 08¬
155000991025 9=Reg.clienti     -9 -     (S/N)=N                        opz. 09¬
155100970527                   -10-     (S/N)=N                        opz. 10¬
155200970526                   -  -     (S/N)=N                        opz. 11¬
155300970526                   -  -     (S/N)=N                        opz. 12¬
155400970526                   -  -     (S/N)=N                        opz. 13¬
155500970526                   -  -     (S/N)=N                        opz. 14¬
155600970526                   -  -     (S/N)=N                        opz. 15¬
155700970526                   -  -     (S/N)=N                        opz. 16¬
155800970526                   -  -     (S/N)=N                        opz. 17¬
155900970526                   -  -     (S/N)=N                        opz. 18¬
156000970526                   -  -     (S/N)=N                        opz. 19¬
156100970526                   -  -     (S/N)=N                        opz. 20¬
156200970526                   -  -     (S/N)=N                        opz. 21¬
156300970526                   -  -     (S/N)=N                        opz. 22¬
156400970526                   -  -     (S/N)=N                        opz. 23¬
156500970526                   -  -     (S/N)=N                        opz. 24¬
156600970526** FUD - FUNZIONI
156700970526                   -        (S/N)=N                        funz.01¬
156800970526                   -        (S/N)=N                        funz.02¬
156900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
157000970526                   -        (S/N)=N                        funz.04¬
157100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
157200970526F06=Inserimento    -        (S/N)=N                        funz.06¬
157300970526                   -        (S/N)=N                        funz.07¬
157400970527                   -        (S/N)=N                        funz.08¬
157500970527                   -        (S/N)=N                        funz.09¬
157600970527                   -        (S/N)=N                        funz.10¬
157700970526                   -        (S/N)=N                        funz.11¬
157800970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
157900970526                   -        (S/N)=N                        funz.13¬
158000970526                   -        (S/N)=N                        funz.14¬
158100970526                   -        (S/N)=N                        funz.15¬
158200970526                   -        (S/N)=N                        funz.16¬
158300970526                   -        (S/N)=N                        funz.17¬
158400970526                   -        (S/N)=N                        funz.18¬
158500970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
158600970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
158700980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
158800970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
158900970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
159000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
