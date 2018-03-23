000100970526     H*------------------------------------------------------------------------*
000200991025     H* Gestione - regole file internet x cliente -
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800991025     FTIS722D   CF   E             WORKSTN INFDS(DEVDS1)
000900991025     F                                     SFILE(s722D05:SFLULT)
001000140619     F                                     SFILE(s722DMS:MSGULT)
001100970526     F*------------
001200051013     Ftivtf01l  IF   E           K DISK
001300080305     Ftivir05l  IF   E           K DISK
001400000728     Ftianv01l  IF   E           K DISK
001500140618     Ftabel00f  IF   E           K DISK    usropn
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D*------------
002000970526     D* DECODIFICHE
002100970526     D*------------
002200970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* MESSAGGI
002500970526     D*------------
002600970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002700970528     D*------------
002800970528     D* COMANDI
002900970528     D*------------
003000970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* DESCRIZIONI OPZIONI
003300970526     D*------------
003400970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* DESCRIZIONI TASTI FUNZIONALI
003700970526     D*------------
003800970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003900970526     D*------------
004000970526     D* OPZIONI DA CARICARE A VIDEO
004100970526     D*------------
004200970526     D OPV             S             19    DIM(24)
004300970526     D*------------
004400970526     D* FUNZIONI DA CARICARE A VIDEO
004500970526     D*------------
004600970527     D FUV             S             19    DIM(24)
004700970526     D*------------------------------------------------------------------------*
004800970526     D* INPUT
004900970526     D*------------------------------------------------------------------------*
005000970526     D*------------
005100970526     D* RIDEFINIZIONE OPZIONI VIDEO
005200970526     D*------------
005300970526     D OPDDS           DS
005400970526     D  OPDOPZ                 1      2
005500970526     D  OPDDE1                 3     19
005600970526     D  OPDOP1                21     22
005700970526     D  OPDFI0                23     34
005800970526     D  OPDATT                35     35
005900970526     D  OPDFI1                36     80
006000970526     D  OPDDES                 1     19
006100970526     D  OPDRIG                 1     80
006200970526     D*------------
006300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006400970526     D*------------
006500970526     D FUDDS           DS
006600970526     D  FUDDE1                 1      1
006700970526     D  FUDFUN                 2      3
006800970526     D  FUDDE2                 4     19
006900970526     D  FUDFI0                20     34
007000970526     D  FUDATT                35     35
007100970526     D  FUDFI1                36     80
007200970526     D  FUDDES                 1     19
007300970526     D  FUDRIG                 1     80
007400970526     D*------------
007500970526     D* DS GESTIONE TASTI FUNZIONALI
007600970526     D*------------
007700970526     D DSKEY         E DS
007800970526     D DEVDS1          DS
007900970526     D  KEY                  369    369
008000970526     D*------------
008100970526     D* DS "XMSG" - GESTIONE MESSAGGI
008200970526     D*------------
008300970526     D DSMSG         E DS
008400970526     D*------------
008500020916     D* DS REPERIMENTO DATI UTENTE
008600970526     D*------------
008700020916     D TIBS34DS      E DS                                                       *Profili utente
008800020916     D DDATIUTE      E DS                                                       *Dati utente
008900020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009000020916     D*------------
009100020916     D* DS X REPERIMENTO/CONTROLLO TABELLE
009200020916     D*------------
009300991108     D TIBS02DS      E DS
009400991122     D*------------------
009500991122     D* DS REPERIMENTO DATI UTENTE
009600991122     D*-------------------
009700991122     DBS69DS         E DS                  extname(TIBS69DS)
009800991122     DACODS          E DS                  extname(CNACO00F)
009900991122     DINDDS          E DS                  extname(CNIND00F)
010000991122     DCLPDS          E DS                  extname(CNCLP00F)
010100991122     DCLSDS          E DS                  extname(FNCLS00F)
010200970527     D*------------------
010300970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
010400970527     D*------------------
010500970527     D WLBDA8          DS                  INZ
010600970527     D  G08DAT                 1      8  0
010700970527     D  G08INV                 9     16  0
010800970527     D  G08ERR                17     17
010900970527     D  G08TGI                18     22  0
011000970526     D*------------
011100970526     D* DS DI PROCEDURA
011200001020     D*------------
011300001020     D tis720        e ds                  extname(tis720ds)
011400001020     D tib12         E DS                  extname(tibs12ds)
011500140618     D DS3C          E DS
011600130201     D TRTB06DS      e ds
011700970526     D*------------
011800970526     D* ARCHITETTURA
011900970526     D*------------
012000970526     D KPJBA         E DS
012100051013     D*------------
012200051013     D* PASSAGGIO PARAMETRI IN INGRESSO
012300051013     D*------------
012400051013     D PARAM           DS
012500051013     D  DEPOP0
012600051013     D  VIDKSC
012700051013     D  VIDTIP
012800970526     C*------------------------------------------------------------------------*
012900970526     C* MAIN LINES
013000970526     C*------------------------------------------------------------------------*
013100970526     C*
013200970526     C* SE NON CI SONO ERRORI, PROSEGUE
013300970526IF  1C     ERRL00        IFNE      '1'
013400970526     C*
013500970526     C* CARICA IL SFILE
013600970526     C                   EXSR      POSSFL
013700970526     C*
013800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013900970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014000000703     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014200970526     C*
014300970526     C* REIMPOSTA ERRORE DI PROCERURA
014400970526     C                   MOVEL     '0'           ERRL00
014500970526     C*
014600970526     C* EMETTE IL SFILE
014700970526     C                   EXSR      OUTSFL
014800970526     C*
014900970526     C* GESTIONE TASTI FUNZIONALI
015000970526     C                   EXSR      FUNCON
015100970526     C*
015200970526E   2C                   ENDDO
015300970527     C*
015400970527     C* CI SONO ERRORI IN ENTRATA
015500970527X   1C                   ELSE
015600991025     C                   MOVEL     '1'           s20ERR
015700991025     C                   MOVEL     DSMSMS        s20MSG
015800970526E   1C                   ENDIF
015900991122     C*
016000991122     C                   EXSR      FINSR
016100970526     C* FINE PGM
016200970526     C                   SETON                                        LR
016300970526     C*------------------------------------------------------------------------*
016400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016500970526     C*------------------------------------------------------------------------*
016600970526     C     F03GES        BEGSR
016700970526     C*
016800970526     C* IMPOSTA "PREMUTO F03"
016900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017000991025     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F05GES        BEGSR
017700970526     C*
017800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F06GES        BEGSR
018500970526     C*
018600970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
018700020916IF  1C     DUTLPO        IFNE      'S'
018800970526     C                   MOVEL     '1'           ERRL00
018900970526     C                   MOVEL     MSG(3)        DSMSMS
019000970526E   1C                   ENDIF
019100970526     C*
019200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019300991025     C                   MOVEL     *BLANKS       Dtip
019400991026     C                   MOVEL     *BLANKS       Dksc
019500000420     C                   Z-ADD     *ZEROS        Ddti
019600970602     C                   MOVEL     DEPOP0        WOP0
019700970529     C                   MOVEL     'F06'         WOP1
019800991025     C                   MOVEL     'TIS723R'     WPGM              8
019900970526     C                   EXSR      LANPGM                                       *LANCIA PGM
020000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
020100970526E   1C                   ENDIF
020200970526     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500140618     C* F07GES - Reperisce il cliente unificante da tab.3C
020600970526     C*------------------------------------------------------------------------*
020700140618     C     F07GES        BEGSR
020800970526     C*
020900140618     C                   IF        not %open(TABEL00F)
021000140618     C                   OPEN      TABEL00F
021100140618     C                   ENDIF
021200140618     C*
021300140618     c                   move      '3C'          tblcod
021400140618     c                   EVAL      tblkey = %subst(VIDKS1:2:7)
021500140618     c     ktab          chain     tabel00f
021600140618     c                   if        %found(tabel00f) and tblflg = *blanks
021700140618     c                   movel     tbluni        ds3c
021800140618     c                   EVAL      VIDKS1 = '0' + %editc(§3CCKS:'X')
021900140618     C*
022000140618     C* eseguo un ENTER forzato
022100140618     C                   EXSR      EntGes
022200140618     C                   ENDIF
022300140618     C*
022400140618     C                   ENDSR
022500140618     C*------------------------------------------------------------------------*
022600140618     C* F12GES - GESTIONE F12 -> RITORNO
022700140618     C*------------------------------------------------------------------------*
022800140618     C     F12GES        BEGSR
022900140618     C*
023000970526     C* IMPOSTA "PREMUTO F12"
023100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023200991025     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
023300970526     C*
023400970526     C                   ENDSR
023500970528     C*------------------------------------------------------------------------*
023600970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
023700970528     C*------------------------------------------------------------------------*
023800970528     C     F21GES        BEGSR
023900970612     C*
024000991025     C                   MOVEL     *BLANKS       Dtip
024100991026     C                   MOVEL     *BLANKS       Dksc
024200000420     C                   Z-ADD     *ZEROS        Ddti
024300970612     C                   MOVEL     DEPOP0        WOP0
024400970612     C                   MOVE      '03'          WOP0
024500970612     C                   MOVEL     'F21'         WOP1
024600991025     C*                  MOVEL     'XXXXXXR'     WPGM              8
024700991025     C*                  EXSR      LANPGM
024800970528     C*
024900970528     C                   ENDSR
025000970528     C*------------------------------------------------------------------------*
025100970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
025200970528     C*------------------------------------------------------------------------*
025300970528     C     F22GES        BEGSR
025400970528     C*
025500970528     C                   Z-ADD     80            LENGH            15 5
025600970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
025700970528     C                   PARM                    CMD(1)
025800970528     C                   PARM                    LENGH
025900970528     C*
026000970528     C                   ENDSR
026100970526     C*------------------------------------------------------------------------*
026200970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
026300970526     C*------------------------------------------------------------------------*
026400970526     C     F23GES        BEGSR
026500970526     C*
026600970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
026700970526     C*
026800970526     C                   ENDSR
026900970526     C*------------------------------------------------------------------------*
027000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
027100970526     C*------------------------------------------------------------------------*
027200970526     C     F24GES        BEGSR
027300970526     C*
027400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
027500970526     C*
027600970526     C                   ENDSR
027700970526     C*------------------------------------------------------------------------*
027800970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
027900970526     C*------------------------------------------------------------------------*
028000970526     C     RUPGES        BEGSR
028100970526     C*
028200970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
028300970526     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600970526     C* ENTGES - GESTIONE ENTER -> INVIO
028700970526     C*------------------------------------------------------------------------*
028800970526     C     ENTGES        BEGSR
028900000703     C*
029000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
029100970526     C                   EXSR      NEWPOS
029200970526     C*
029300970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
029400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029500970526IF  2C     SWPOSI        IFNE      0
029600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
029700970526X   2C                   ELSE
029800970526IF  3C     SFLULT        IFNE      0
029900970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
030000970526     C*
030100970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
030200970526IF  4C     SWMODI        IFNE      0
030300970526     C                   EXSR      POSSFL
030400970526E   4C                   ENDIF
030500970526E   3C                   ENDIF
030600970526E   2C                   ENDIF
030700970526E   1C                   ENDIF
030800970526     C*
030900970526     C                   ENDSR
031000000703     C*------------------------------------------------------------------------*
031100970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
031200970526     C*------------------------------------------------------------------------*
031300970526     C     O01GES        BEGSR
031400970526     C*
031500970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
031600991025     C                   MOVEL     vidtip        s20tip
031700991026     C                   MOVEL     vidksc        s20ksc
031800000420     C                   Z-ADD     viddti        s20dti
031900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032000970526     C*
032100970526     C                   ENDSR
032200970526     C*------------------------------------------------------------------------*
032300970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
032400970526     C*------------------------------------------------------------------------*
032500970526     C     O02GES        BEGSR
032600970526     C*
032700970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
032800991025IF  1C*    VIDvat        IFEQ      'A'
032900991025     C*                  MOVEL     '1'           ERRL00
033000991025     C*                  MOVEL     MSG(4)        DSMSMS
033100991025E   1C*                  ENDIF
033200970527     C* MODIFICA
033300970526IF  1C     ERRL00        IFEQ      '0'
033400991025     C                   MOVEL     VIDtIP        DTIP
033500991026     C                   MOVEL     VIDksc        Dksc
033600000420     C                   Z-ADD     VIDdti        Ddti
033700970602     C                   MOVEL     DEPOP0        WOP0
033800970526     C                   MOVEL     'O02'         WOP1
033900991025     C                   MOVEL     'TIS723R'     WPGM
034000970526     C                   EXSR      LANPGM
034100991012     c                   z-add     1             swmodi
034200970526E   1C                   ENDIF
034300970526     C*
034400970526     C                   ENDSR
034500991014     C*------------------------------------------------------------------------*
034600991014     C* O03GES - GESTIONE OPZIONE 3=copia
034700991014     C*------------------------------------------------------------------------*
034800991014     C     O03GES        BEGSR
034900991014     C*
035000991014     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
035100991014     C* copia
035200991014IF  1C     ERRL00        IFEQ      '0'
035300991025     C                   MOVEL     VIDtIP        DTIP
035400991026     C                   MOVEL     VIDksc        Dksc
035500000420     C                   Z-ADD     VIDdti        Ddti
035600991014     C                   MOVEL     DEPOP0        WOP0
035700991014     C                   MOVEL     'O03'         WOP1
035800991025     C                   MOVEL     'TIS723R'     WPGM
035900991014     C                   EXSR      LANPGM
036000991014     c                   z-add     1             swmodi
036100991014E   1C                   ENDIF
036200991014     C*
036300991014     C                   ENDSR
036400970526     C*------------------------------------------------------------------------*
036500970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
036600970526     C*------------------------------------------------------------------------*
036700991104     C*    O04GES        BEGSR
036800970526     C*
036900970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
037000970527     C* ANNULLAMENTO
037100991104IF  1C*    ERRL00        IFEQ      '0'
037200991104     C*                  MOVEL     VIDtIP        DTIP
037300991104     C*                  MOVEL     VIDksc        Dksc
037400000420     C*                  Z-ADD     VIDdti        Ddti
037500991104     C*                  MOVEL     DEPOP0        WOP0
037600991104     C*                  MOVEL     'O04'         WOP1
037700991104     C*                  MOVEL     'TIS723R'     WPGM
037800991104     C*                  EXSR      LANPGM
037900991104     c*                  z-add     1             swmodi
038000991104E   1C*                  ENDIF
038100970526     C*
038200991104     C*                  ENDSR
038300970526     C*------------------------------------------------------------------------*
038400970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
038500970526     C*------------------------------------------------------------------------*
038600970526     C     O05GES        BEGSR
038700970526     C*
038800991025     C                   MOVEL     VIDtIP        DTIP
038900991026     C                   MOVEL     VIDksc        Dksc
039000000420     C                   Z-ADD     VIDdti        Ddti
039100970602     C                   MOVEL     DEPOP0        WOP0
039200970526     C                   MOVEL     'O05'         WOP1
039300991025     C                   MOVEL     'TIS723R'     WPGM
039400970526     C                   EXSR      LANPGM
039500970526     C*
039600970526     C                   ENDSR
039700970526     C*------------------------------------------------------------------------*
039800970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
039900970526     C*------------------------------------------------------------------------*
040000970526     C     O07GES        BEGSR
040100970526     C*
040200970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
040300970526     C* RIPRISITINO
040400991025IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
040500991025     c*                  exsr      movcamp
040600991025     C*                  MOVEL     DEPOP0        WOP0
040700991025     C*                  MOVEL     'O07'         WOP1
040800991025     C*                  MOVEL     'TIBS13R'     WPGM
040900991025     C*                  EXSR      LANPGM
041000991025     C*                  Z-ADD     1             SWMODI                     L
041100991025E   1C*                  ENDIF
041200970526     C*
041300970526     C                   ENDSR
041400991001      *
041500991028     C*------------------------------------------------------------------------*
041600991028     C* O09GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
041700991028     C*------------------------------------------------------------------------*
041800991028     C     O09GES        BEGSR
041900991028     C*
042000991028     C                   MOVEL     VIDksc        Dksc
042100991028     C                   MOVEL     DEPOP0        WOP0
042200991028     C                   MOVEL     'O09'         WOP1
042300991028     C                   MOVEL     'TIBS12R'     WPGM
042400991028     C                   EXSR      LANPGM
042500991028     C*
042600991028     C                   ENDSR
042700051013     C*------------------------------------------------------------------------*
042800051013     C* O10GES - GESTIONE OPZIONE 10=VISUALIZZAZIONE LOGS RELATIVI
042900051013     C*------------------------------------------------------------------------*
043000051013     C     O10GES        BEGSR
043100051013     C*
043200051013     C* AGGANCIO L'ANAGRAFICO STANDARD TIPI FILE X DETERMINARE SE TRATTASI D FILE IN UPLOAD
043300051013     C* O D DOWNLOAD
043400051013     C     VIDTIP        CHAIN     TIVTF01L
043500051013     C                   IF        %found(TIVTF01L)
043600051013     C                   IF        VTFUD = 'U'
043700051013     C                   EVAL      KPJBU = PARAM
043800051013     C                   CALL      'TIS760R'
043900051013     C                   PARM                    KPJBA
044000051013     C                   ENDIF
044100051013     C                   IF        VTFUD = 'D'
044200051013     C                   EVAL      KPJBU = PARAM
044300051013     C                   CALL      'TIS7GIR'
044400051013     C                   PARM                    KPJBA
044500051013     C                   ENDIF
044600051013     C                   ENDIF
044700051013     C*
044800051013     C                   ENDSR
044900000703     C*------------------------------------------------------------------------*
045000001020     C* O11GES - GESTIONE OPZIONE 11=AGGIUNGI LEGAME FILE
045100000703     C*------------------------------------------------------------------------*
045200001020     C     O11GES        BEGSR
045300000703     C*
045400000703     C                   MOVEL     VIDksc        Dksc
045500000703     C                   MOVEL     VIDtip        Dtip
045600000704     C                   Z-ADD     VIDdti        Ddti
045700000705     C                   MOVEL     VIDrag        Drag             25
045800000703     C                   MOVEL     DEPOP0        WOP0
045900001020     C                   MOVEL     'O11'         WOP1
046000000703     C                   MOVEL     'TIS727R'     WPGM
046100000703     C                   EXSR      LANPGM
046200000703     C*
046300000703     C                   ENDSR
046400130129     C*------------------------------------------------------------------------*
046500130129     C* O22GES - GESTIONE OPZIONE 22=VISUALIZZA TABELLE VAS
046600130129     C*------------------------------------------------------------------------*
046700130129     C     O22GES        BEGSR
046800130129     C*
046900130201     C                   CLEAR                   TRTB06DS
047000130201     C                   MOVE      VIDksc        D06KSC
047100130201     C                   MOVE      'G'           D06OP0
047200130201     C                   MOVEL     TRTB06DS      KPJBU
047300130129     C                   CALL      'TRTB06R'
047400130129     C                   PARM                    KPJBA
047500130129     C*
047600130129     C                   ENDSR
047700970526     C*------------------------------------------------------------------------*
047800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
047900970526     C*------------------------------------------------------------------------*
048000970526     C     NEWPOS        BEGSR
048100970526     C*
048200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
048300970526     C*         1= RICHIESTO POSIZIONAMENTO
048400970526     C                   Z-ADD     0             SWPOSI            7 0
048500970526     C*
048600970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
048700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
048800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
048900970526     C                   Z-ADD     0             SWMODI            1 0
049000970530     C*
049100970530     C* REIMPOSTA INDICATORI DI ERRORE
049200970530     C                   SETOFF                                       808182
049300970530     C                   SETOFF                                       83
049400970526     C*
049500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
049600010423     C     VIDti2        IFNE      depti2
049700010423     C     VIDks2        orne      depks2
049800010423     C     VIDks1        orne      depks1
049900010423     C     VIDti1        orne      depti1
050000010423     C     VIDsr1        orne      depsr1
050100010423     C     VIDpo1        orne      deppo1
050200010423     C     VIDsrs        orne      depsrs
050300040618     C     VIDpgt        orne      deppgt
050400050831     C     VIDdat        orne      depdat
050500970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
050600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
050700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
050800991025     C                   MOVEL     VIDti2        DEPti2                         *DEPOSITI
050900991025     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
051000000801     C                   MOVEL     VIDsr1        DEPsr1                         *DEPOSITI
051100010306     C                   MOVEL     VIDpo1        DEPpo1                         *DEPOSITI
051200991025     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
051300991025     C                   MOVEL     VIDks2        DEPks2                         *DEPOSITI
051400010423     C                   MOVEL     VIDsrs        DEPsrs                         *DEPOSITI
051500040618     C                   MOVEL     VIDpgt        DEPpgt                         *DEPOSITI
051600050831     C                   MOVEL     VIDdat        DEPdat                         *DEPOSITI
051700970526E   2C                   ENDIF
051800970526E   1C                   ENDIF
051900970526     C*
052000970526     C                   ENDSR
052100970526     C*------------------------------------------------------------------------*
052200970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
052300970526     C*------------------------------------------------------------------------*
052400970528     C     SETFIL        BEGSR
052500970526     C*
052600970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
052700991025     C                   MOVEL     VIDks2        KCEksc
052800080305     C                   MOVEL     VIDsr1        KCEisv
052900991025     C                   MOVEL     VIDti2        KCEtip
053000991025IF  1C     WTIPRI        IFEQ      'TIP'                                        *PER PARTENZA
053100080305     C     KEYvir        SETLL     tivir05l                           99
053200970528E   1C                   ENDIF
053300980107     C*
053400970526     C* LETTURA RECORD
053500970526IF  1C     *IN99         IFEQ      *OFF
053600970526     C                   EXSR      LETFIL
053700970526E   1C                   ENDIF
053800970526     C*
053900970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
054000970526IF  1C     *IN99         IFEQ      *ON
054100970526     C                   Z-ADD     1             SFLEOF
054200970526E   1C                   ENDIF
054300970526     C*
054400970526     C                   ENDSR
054500970526     C*------------------------------------------------------------------------*
054600970526     C* LETFIL - LETTURA RECORD
054700970526     C*------------------------------------------------------------------------*
054800970526     C     LETFIL        BEGSR
054900970526     C*
055000970526     C* LEGGE FINO A:
055100970526     C*     - FINE FILE
055200970526     C*     - TROVATO RECORD VALIDO
055300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
055400010306     C     *IN99         DOWEQ     *OFF
055500970526     C     WRECOK        ANDEQ     'N'
055600970526     C*
055700970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
055800010306     C     WTIPRI        IFEQ      'TIP'                                        *PER PARTENZA
055900080305     C                   READ      tivir05l                               99
056000010306     C                   ENDIF
056100970528     C*
056200970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
056300010306     C     *IN99         IFEQ      *ON                                          *FINE FILE
056400970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
056500010306     C                   ELSE
056600970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
056700010306     C                   ENDIF
056800010306     C                   ENDDO
056900970526     C*
057000970526     C                   ENDSR
057100970526     C*------------------------------------------------------------------------*
057200970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
057300970526     C*------------------------------------------------------------------------*
057400970526     C     CHKREC        BEGSR
057500970526     C*
057600970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
057700100218     C                   setoff                                       75
057800010117     C*
057900010117     C*filtra se impostato tipo file
058000010427     C                   if        WRECOK = 'S'
058100010117     C     vidti1        ifeq      *blank                                       *ALLA DATA
058200010117     C                   else
058300010117     C                   if        %subst(vidti1:1:1) = '*'
058400010117     C                   if        %subst(vidti1:2:1) <>
058500010117     C                             %subst(virtip:2:1)
058600010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
058700010117     C                   endif
058800010117     C                   else
058900010117     C                   if        %subst(vidti1:2:1) = '*'
059000010117     C                   if        %subst(vidti1:1:1) <>
059100010117     C                             %subst(virtip:1:1)
059200010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
059300010117     C                   endif
059400010117     C                   else
059500010117     C                   if        vidti1 <> virtip
059600010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
059700010117     C                   endif
059800010117     C                   endif
059900010117     C                   endif
060000010117     C                   endif
060100010427     C                   endif
060200010117     C*
060300000801     C*filtra se impostato codice cliente
060400010427     C                   if        WRECOK = 'S'
060500000801     C     vidks1        ifeq      *zeros
060600000801     C     vidks1        oreq      *blank
060700010306     C                   ELSE
060800000801     C     vidks1        ifne      virksc
060900991026     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
061000010306     C                   ENDIF
061100010306     C                   ENDIF
061200010427     C                   ENDIF
061300010117     C*
061400010117     C*filtra se impostato il tipo servizio
061500010427     C                   if        WRECOK = 'S'
061600010117     C     vidsr1        ifeq      *blank
061700010117     C                   else
061800010117     C                   if        %subst(vidsr1:1:1) = '*'
061900010117     C                   if        %subst(vidsr1:2:1) <>
062000010117     C                             %subst(virfi1:2:1)
062100010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
062200010117     C                   endif
062300010117     C                   else
062400010117     C                   if        %subst(vidsr1:2:1) = '*'
062500010117     C                   if        %subst(vidsr1:1:1) <>
062600010117     C                             %subst(virfi1:1:1)
062700010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
062800010117     C                   endif
062900010117     C                   else
063000010117     C                   if        vidsr1 <> virfi1
063100010117     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
063200010117     C                   endif
063300010117     C                   endif
063400010117     C                   endif
063500010117     C                   endif
063600010427     C                   endif
063700010306     C*
063800010306     C*filtra se impostato il Punto Operativo
063900010427     C                   if        WRECOK = 'S'
064000010306     C     vidpo1        ifeq      *zeros
064100010306     C                   else
064200010306     C     vidpo1        ifne      virpoi
064300010306     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
064400010306     C                   endif
064500010306     C                   endif
064600010427     C                   endif
064700010427     C*
064800010427     C* RAGIONE SOCIALE DIVERSA DA QUELLA SCELTA
064900010427     C                   if        WRECOK = 'S'
065000010427     C                   move      virksc        wcli              7 0          -codice
065100010427     C                   exsr      dectib
065200010709     C     VIDsrs        ifeq      *blanks                                      *ALLA DATA
065300010709     C                   else
065400010709     C*                  if        %subst(wddksc:1:%len(%trim(VIDsrs)))<>
065500010709     C*                            %trim(VIDsrs)
065600010709     C                   z-add     *zeros        pos               2 0
065700010709     C                   eval      pos = %scan(%trim(VIDsrs):wddksc)
065800010709     C                   if        pos = *zeros
065900010427     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
066000010427     C                   endif
066100010427     C                   endif
066200010427     C                   endif
066300040618     C*
066400040618     C*filtra se impostato il Traduttore
066500040618     C                   if        WRECOK = 'S'
066600040618     C     vidpgt        ifeq      *blanks
066700080319     C                   else
066800100408     C                   if        %scan(%trim(vidPGT):virPGT) > *zeros OR
066900100408     C                             %scan(%trim(vidPGT):virPGI) > *zeros
067000080319     C                   else
067100080319     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
067200080319     C                   endif
067300040618     C                   endif
067400040618     C                   endif
067500050831     C*
067600050831     C*filtra se impostata data validità al
067700050831     C                   if        WRECOK = 'S'
067800050831     C     viddat        ifeq      *zeros
067900050831     C                   ELSE
068000050831     C                   if        viddat >= virdti and
068100050831     C                             viddat <= virdtf
068200050831     C                   ELSE
068300050831     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
068400050831     C                   ENDIF
068500050831     C                   ENDIF
068600050831     C                   ENDIF
068700970526     C*
068800100218     C                   if        WRECOK = 'S'
068900100218     C                   if        VIRDTF < datcor
069000100218     C                   seton                                        75
069100100218     C                   endif
069200100218     C                   ENDIF
069300100218     C*
069400970526     C                   ENDSR
069500010423     C*------------------------------------------------------------------------*
069600010423     C* dectib - Controlla esistenza codice cliente
069700010423     C*------------------------------------------------------------------------*
069800010423     C     dectib        begsr
069900010913     C*
070000010913     C     virksc        chain     tianv01l
070100010913     C                   if        %found(tianv01l)
070200010913     C                   movel(p)  anvdes        wddksc
070300010913     C                   else
070400010913     C*
070500010423     C                   CLEAR                   BS69DS
070600010423     C                   CLEAR                   ACODS
070700010423     C                   MOVEL     KNSIF         I69SIF
070800010423     C                   Z-ADD     WCLI          I69KAC
070900010423     C                   CALL      'TIBS69R'
071000010423     C                   PARM                    BS69DS
071100010423     C                   PARM                    ACODS
071200010423     C                   PARM                    INDDS
071300010423     C                   PARM                    CLPDS
071400010423     C                   PARM                    CLSDS
071500010423     C     O69ERR        IFNE      '1'
071600010423     C                   MOVEL     ACORAG        wddksc           40
071700010423     C                   ELSE
071800010423     C                   MOVEL     *ALL'*'       Wddksc
071900010913     C                   MOVE      '1'           errl00
072000010913     C                   ENDIF
072100010423     C                   ENDIF
072200010913     C*
072300010913     C                   endsr
072400970526     C*------------------------------------------------------------------------*
072500970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
072600970526     C*------------------------------------------------------------------------*
072700970526     C     IRESFL        BEGSR
072800970526     C*
072900970526     C                   MOVEL     *BLANKS       OPZKEY
073000991025     C                   MOVEL     virtip        VIDtip
073100991025     C                   MOVEL     virdes        VIDdes
073200991025     C                   MOVEL     virksc        VIDksc
073300010423     C                   MOVEL     wddksc        VIDrag
073400991025     C                   z-add     virseq        VIDseq
073500991025     C                   z-add     virpoi        VIDpoi
073600991025     C                   Z-ADD     virdti        G08INV
073700991025     C                   Z-ADD     *ZEROS        G08DAT
073800991025     C                   MOVEL     '3'           G08ERR
073900991025     C                   CALL      'XSRDA8'
074000991025     C                   PARM                    WLBDA8
074100991025     C                   Z-ADD     G08DAT        VIDDti
074200991025     C                   Z-ADD     virdtf        G08INV
074300991025     C                   Z-ADD     *ZEROS        G08DAT
074400991025     C                   MOVEL     '3'           G08ERR
074500991025     C                   CALL      'XSRDA8'
074600991025     C                   PARM                    WLBDA8
074700991025     C                   Z-ADD     G08DAT        VIDDtf
074800000626     C                   MOVEL     VIRFI1        VIDISV
074900011129     C*
075000011129     C* Verifico che il cliente/tipo file sia attivo nella rispettiva tabella
075100011129     C                   CALL      'TIS70UR'
075200011129     C                   PARM                    KPJBA
075300011129     C                   PARM                    VIDTIP
075400011129     C                   PARM                    VIDKSC
075500011129     C                   PARM      '0'           flgope            1
075600011129     C*
075700011129     C                   IF        flgope = '2'
075800011129     C                   SETON                                        55
075900011129     C                   ELSE
076000011129     C                   SETOFF                                       55
076100011129     C                   ENDIF
076200011129     C*
076300970526     C                   ENDSR
076400970526     C*------------------------------------------------------------------------*
076500970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
076600970526     C*------------------------------------------------------------------------*
076700970526     C     CHKPAR        BEGSR
076800970528     C*
076900970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
077000991025     C                   MOVEL     'TIP'         WTIPRI            3
077100991025IF  1C     VIDti2        IFEQ      *BLANKS
077200991025     C     VIDks2        ANDEQ     *ZEROS
077300000801IF  1C     VIDti2        OREQ      *BLANKS
077400991025     C     VIDks2        ANDEQ     *blanks
077500991025     C                   MOVEL     'TIP'         WTIPRI
077600991025E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
077700970528     C*
077800970528     C* CONTROLLA IL TIPO RICERCA
077900000801     C                   setoff                                       86
078000991012IF  1C     ERRL00        IFEQ      '0'
078100991025IF  2C     VIDti2        IFNE      *blank
078200010117     C                   if        %scan('*':VIDti2) = 0
078300991025     C                   mOVEL     'TIP'         WTIPRI
078400000801     C                   move      vidti2        wvidti2           2
078500000801     C                   exsr      cnttip
078600991108     C                   IF        ERRL00 ='1'
078700000801     C                   seton                                        86
078800010117     C                   ENDIF
078900000801     C                   move      wvidti2       vidti2
079000010117     C                   endif
079100991012E   2C                   ENDIF
079200991012E   1C                   ENDIF
079300000801     C*
079400991108     C* CONTROLLA IL TIPO RICERCA
079500000801     C                   setoff                                       88
079600991108IF  1C     ERRL00        IFEQ      '0'
079700991108IF  2C     VIDti1        IFNE      *blank
079800010117     C                   if        %scan('*':VIDti1) = 0
079900000801     C                   movel     'TIP'         WTIPRI
080000000801     C                   move      vidti1        wvidti2           2
080100000801     C                   exsr      cnttip
080200991108     C                   IF        ERRL00 ='1'
080300000801     C                   seton                                        88
080400000801     C                   end
080500000801     C                   move      wvidti2       vidti1
080600010117     C                   endif
080700991108E   2C                   ENDIF
080800991108E   1C                   ENDIF
080900000801     C*
081000000801     C* CONTROLLA IL TIPO RICERCA
081100000801     C                   setoff                                       89
081200000801IF  1C     ERRL00        IFEQ      '0'
081300000801IF  2C     VIDsr1        IFNE      *blank
081400010117     C                   if        %scan('*':VIDsr1) = 0
081500000801     C                   movel     'TIP'         WTIPRI
081600000801     C                   move      VIDsr1        wvidsr1           2
081700000801     C                   exsr      cntisv
081800000801     C                   IF        ERRL00 ='1'
081900000801     C                   seton                                        89
082000000801     C                   end
082100000801     C                   move      wvidsr1       VIDsr1
082200010117     C                   endif
082300000801E   2C                   ENDIF
082400000801E   1C                   ENDIF
082500000801     C*
082600970526     C                   ENDSR
082700991108     C*------------------------------------------------------------------------*
082800991108     C* Cnttip - CONTROLLA codice tipo legame
082900991108     C*------------------------------------------------------------------------*
083000000801     C     cnttip        begsr
083100000801     C                   clear                   TIBS02DS
083200000801     C*
083300000801     C     '?'           SCAN      wVidti2                                30
083400000801     C*  Ricerca
083500000801     C                   eval      t02tla = 'L'
083600000801    1C                   IF        *IN30 = *ON
083700000801     C                   movel     'R'           t02mod
083800000801     C                   else
083900000801     C                   movel     'C'           t02mod
084000000801     C                   movel(p)  Wvidti2       t02ke1
084100000801     C                   end
084200000801     C                   movel     'ITF'         t02cod
084300000801     C                   movel     knsif         t02sif
084400991108     C                   CALL      'TIBS02R'
084500000801     C                   parm                    KPJBA
084600000801     C                   parm                    TIBS02DS
084700000801     C
084800000801     C*
084900991108    2C                   IF        T02err = *blanks
085000991108     C                   movel     T02ke1        wVidti2
085100000801     C                   else
085200000801     C                   movel     '1'           errl00
085300991108     C                   MOVEL     msg(11)       DSMSMS
085400991108     C                   ENDIF
085500000801     C
085600000801     C                   endsr
085700000801     C*------------------------------------------------------------------------*
085800000801     C* Cntisv - CONTROLLA codice tipo legame
085900000801     C*------------------------------------------------------------------------*
086000000801     C     cntisv        begsr
086100000801     C*
086200000801     C                   clear                   TIBS02DS
086300000801     C*
086400000801     C     '?'           SCAN      wvidsr1                                30
086500000801     C*  Ricerca
086600000801     C                   eval      t02tla = 'L'
086700000801    1C                   IF        *IN30 = *ON
086800000801     C                   movel     'R'           t02mod
086900000801     C                   else
087000000801     C                   movel     'C'           t02mod
087100000801     C                   movel(p)  wvidsr1       t02ke1
087200000801     C                   end
087300000801     C                   movel     'ISV'         t02cod
087400000801     C                   movel     knsif         t02sif
087500000801     C                   CALL      'TIBS02R'
087600000801     C                   parm                    KPJBA
087700000801     C                   parm                    TIBS02DS
087800000801     C
087900000801     C*
088000000801    2C                   IF        T02err = *blanks
088100000801     C                   movel     T02ke1        wvidsr1
088200000801     C                   else
088300000801     C                   movel     '1'           errl00
088400000801     C                   seton                                        62
088500000801     C                   MOVEL     MSG(9)        DSMSMS
088600000801     C                   ENDIF
088700000801     C*
088800000801     C                   endsr
088900970526     C*------------------------------------------------------------------------*
089000970526     C* LANPGM - LANCIA UN PROGRAMMA
089100970526     C*------------------------------------------------------------------------*
089200970526     C     LANPGM        BEGSR
089300970526     C*
089400970526     C* IMPOSTA LA DS DI PROCEDURA
089500000801     C                   if        opzkey <> ' 9'
089600991025     C                   MOVEL     WOP0          s20OP0                         *LIVELLO PROCEDURA
089700991025     C                   MOVEL     WOP1          s20OP1                         *LIVELLO PGM
089800991025     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
089900991025     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
090000991025     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
090100991025     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
090200991025     C                   MOVEL     Dtip          s20tip                         *PARTENZA/ARRIVO
090300991026     C                   MOVEL     Dksc          s20ksc                         *PARTENZA/ARRIVO
090400000420     C                   Z-ADD     Ddti          s20dti                         *PARTENZA/ARRIVO
090500000801     C                   movel(p)  tis720        kpjbu
090600991028     C*ds cliente unificante
090700000801     C                   else
090800000801     C                   clear                   tib12
090900991028     C                   MOVEL     WOP0          k12OP0                         *LIVELLO PROCEDURA
091000991028     C                   MOVEL     WOP1          k12OP1                         *LIVELLO PGM
091100991028     C                   MOVEL     '0'           k12F03                         *PREMUTO F03=FINE
091200991028     C                   MOVEL     '0'           k12F12                         *PREMUTO F12=RITORNO
091300991028     C                   MOVEL     '0'           k12ERR                         *'1'=ERRORE
091400991028     C                   MOVEL     *BLANKS       k12MSG                         *MESSAGGIO DI RITORN
091500991028     C                   MOVEL     'WW'          k12tle                         *PARTENZA/ARRIVO
091600991028     C                   move      *zeros        k12cop                         *TERMINAL PARTENZA
091700991028     C                   MOVE      Dksc          k12cop                         *PARTENZA/ARRIVO
091800000705     C                   movel(p)  tib12         kpjbu
091900000705     C                   endif
092000000705     C*
092100001020     C                   if        opzkey = '11'
092200000705     C                   CALL      WPGM
092300000705     C                   PARM                    KPJBA
092400000705     C                   PARM                    Drag
092500000705     C                   ELSE
092600970526     C                   CALL      WPGM
092700970612     C                   PARM                    KPJBA
092800000705     C                   ENDIF
092900970526     C*
093000970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
093100000801     C                   if        opzkey <> ' 9'
093200991025IF  1C     s20F03        IFEQ      '1'                                          *PREMUTO F03
093300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
093400970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
093500970526X   1C                   ELSE                                                   *NON PREMUTO F03
093600991025     C                   MOVEL     s20MSG        DSMSMS                         *MESSAGGIO DI RITORN
093700970526E   1C                   ENDIF
093800000801     C                   else
093900991028IF  1C     k12F03        IFEQ      '1'                                          *PREMUTO F03
094000991028     C                   MOVEL     '1'           ERRL00                         *ERRORE
094100991028     C                   MOVEL     '1'           F03L00                         *FINE PGM
094200991028X   1C                   ELSE                                                   *NON PREMUTO F03
094300991028     C                   MOVEL     k12msg        DSMSMS                         *MESSAGGIO DI RITORN
094400991028E   1C                   ENDIF
094500000801     C                   endif
094600970526     C*
094700970526     C                   ENDSR
094800970526     C*------------------------------------------------------------------------*
094900970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
095000970526     C*------------------------------------------------------------------------*
095100970526     C     CHKFOP        BEGSR
095200970626     C*
095300970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
095400970626IF  1C     DEPOP0        IFEQ      'P01'
095500970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
095600970626     C                   MOVEL     'S'           OPDATT
095700970626     C                   MOVEL     OPDRIG        OPD(1)
095800970626X   1C                   ELSE
095900970626     C* OPZIONI
096000970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
096100970626     C                   MOVEL     'S'           OPDATT
096200970626     C                   MOVEL     OPDRIG        OPD(5)
096300000703     C                   MOVEL     OPD(9)        OPDRIG
096400000703     C                   MOVEL     'S'           OPDATT
096500000703     C                   MOVEL     OPDRIG        OPD(9)
096600001020     C                   MOVEL     OPD(11)       OPDRIG
096700000703     C                   MOVEL     'S'           OPDATT
096800000703     C                   MOVEL     OPDRIG        OPD(11)
096900051013     C                   MOVEL     OPD(10)       OPDRIG                         *VAI A LOGS
097000051013     C                   MOVEL     'S'           OPDATT
097100051013     C                   MOVEL     OPDRIG        OPD(10)
097200970626     C* FUNZIONI
097300970626E   1C                   ENDIF
097400970626     C*
097500970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
097600970626IF  1C     DEPOP0        IFEQ      'P02'
097700051013     C     DUTLPO        ANDEQ     'S'                                          *SOLO SEDE
097800970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
097900970626     C                   MOVEL     'S'           OPDATT
098000970626     C                   MOVEL     OPDRIG        OPD(2)
098100991014     C                   MOVEL     OPD(3)        OPDRIG                         *MODIFICA
098200991014     C                   MOVEL     'S'           OPDATT
098300991014     C                   MOVEL     OPDRIG        OPD(3)
098400991104     C*                  MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
098500991104     C*                  MOVEL     'S'           OPDATT
098600991104     C*                  MOVEL     OPDRIG        OPD(4)
098700000711     C*                  MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
098800000711     C*                  MOVEL     'S'           OPDATT
098900000711     C*                  MOVEL     OPDRIG        OPD(7)
099000970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
099100970626     C                   MOVEL     'S'           FUDATT
099200970626     C                   MOVEL     FUDRIG        FUD(6)
099300000703     C                   MOVEL     OPD(9)        OPDRIG                         *CLI. UNIF.
099400991028     C                   MOVEL     'S'           OPDATT
099500991028     C                   MOVEL     OPDRIG        OPD(9)
099600130129     C                   MOVEL     OPD(22)       OPDRIG                         *TABELLE VAS
099700130129     C                   MOVEL     'S'           OPDATT
099800130129     C                   MOVEL     OPDRIG        OPD(22)
099900970626E   1C                   ENDIF
100000970526     C*
100100970526     C* GESTIONE OPZIONI A VIDEO
100200970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
100300970526     C                   EXSR      OPZGES
100400970526     C*
100500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
100600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
100700970526     C                   EXSR      FUNGES
100800970526     C*
100900970526     C                   ENDSR
101000991122     C*------------------------------------------------------------------------*
101100991122     C* FINSR - OPERAZIONI FINALI
101200991122     C*------------------------------------------------------------------------*
101300991122     C     FINSR         BEGSR
101400991122     C*
101500991122     C* CHIUDE I FILE DEI PGM CHIAMATI
101600991122     C                   CLEAR                   BS69DS
101700991122     C                   CLEAR                   ACODS
101800991122     C                   CLEAR                   INDDS
101900991122     C                   CLEAR                   CLPDS
102000991122     C                   CLEAR                   CLSDS
102100991122     C                   MOVEL     'C'           I69TLA
102200991122     C                   MOVEL     KNSIF         I69SIF
102300991122     C                   CALL      'TIBS69R'
102400991122     C                   PARM                    BS69DS
102500991122     C                   PARM                    ACODS
102600991122     C                   PARM                    INDDS
102700991122     C                   PARM                    CLPDS
102800991122     C                   PARM                    CLSDS
102900991122     C*
103000991122     C                   ENDSR
103100020916     C*--------------------------------------------------------------------------------------------*
103200020916     C* REPERISCE I DATI UTENTE
103300020916     C*--------------------------------------------------------------------------------------------*
103400020916     C     REPDATIUTE    BEGSR
103500020916     C*
103600020916     C* INIZIALIZZA VARIABILI DI WRK
103700020916     C                   CLEAR                   TIBS34DS
103800020916     C                   CLEAR                   AZUTEDS
103900020916     C                   CLEAR                   DDATIUTE
104000020916     C*
104100020916     C     *DTAARA       DEFINE    §azute        azuteds
104200020916     C     *DTAARA       DEFINE    §datiute      ddatiute
104300020916     C                   IN(E)     *DTAARA
104400020916if  1C                   IF        %Error
104500020916     c                   EVAL      I34Tla = 'L'
104600020916     C                   CALL      'TIBS34R'
104700020916     C                   PARM                    Tibs34Ds
104800020916     C                   IN        *DTAARA
104900020916e   1C                   ENDIF
105000020916     C*
105100020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
105200020916     C                   MOVEL(P)  RSUT          DSFIRS           20
105300020916     C*
105400020916     C                   ENDSR
105500970526     C*------------------------------------------------------------------------*
105600970526     C* *INZSR - OPERAZIONI INIZIALI
105700970526     C*------------------------------------------------------------------------*
105800970526     C     *INZSR        BEGSR
105900970526     C*---
106000970526     C* RICEVIMENTO PARAMETRI
106100970526     C*---
106200970526     C     *ENTRY        PLIST
106300970612     C                   PARM                    KPJBA
106400001020     C                   MOVEL     kpjbu         tis720
106500970526     C*
106600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
106700970526     C     XMSGPA        PLIST
106800970526     C                   PARM                    DSMSMS
106900970526     C                   PARM                    DSMSPG
107000970526     C                   PARM                    DSMSKE
107100970526     C                   PARM                    DSMSER
107200970526     C*
107300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
107400970526     C                   CALL      'XKEY'
107500970526     C                   PARM                    DSKEY
107600970526     C*---
107700970526     C* VARIABILI NON RIFERITE AL DATA BASE
107800970526     C*---
107900970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
108000970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
108100970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
108200970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
108300970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
108400970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
108500970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
108600970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
108700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
108800991025     C     *LIKE         DEFINE    s20OP0        DEPOP0                         *DI DEPOSITO
108900991025     C     *LIKE         DEFINE    s20OP1        DEPOP1
109000991025     C     *LIKE         DEFINE    s20OP0        WOP0                           *DI LAVORO
109100991025     C     *LIKE         DEFINE    s20OP1        WOP1
109200991025     C     *LIKE         DEFINE    s20tip        Dtip
109300991026     C     *LIKE         DEFINE    s20ksc        Dksc
109400000420     C     *LIKE         DEFINE    s20dti        Ddti
109500970526     C*---
109600991025     C     *LIKE         DEFINE    s20tip        Deptip
109700991025     C     *LIKE         DEFINE    s20tip        kcetip
109800991025     C     *LIKE         DEFINE    s20tip        Depti1
109900070919     C     *LIKE         DEFINE    vidti2        Depti2
110000991025     C     *LIKE         DEFINE    vidks2        Depks2
110100991025     C     *LIKE         DEFINE    vidks1        Depks1
110200000801     C     *LIKE         DEFINE    vidsr1        Depsr1
110300010306     C     *LIKE         DEFINE    vidpo1        Deppo1
110400010423     C     *LIKE         DEFINE    vidsrs        Depsrs
110500040618     C     *LIKE         DEFINE    vidpgt        Deppgt
110600991025     C     *LIKE         DEFINE    virksc        kceksc
110700050831     C     *LIKE         DEFINE    viddat        Depdat
110800080305     C     *LIKE         DEFINE    virfi1        kceisv
110900001020     C*---
111000970526     C* VARIABILI DI CONTROLLO
111100970526     C*---
111200970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
111300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
111400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
111500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
111600970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
111700050831     C                   Z-ADD     9             SFLPAG                         *N°RIGHE X PAG
111800970526     C*
111900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
112000991025     C                   MOVEL     'TIS722R'     DSMSPG
112100991025     C                   MOVEL     'TIS722R'     MSGQUE
112200970526     C                   MOVEL     *BLANKS       MSGKEY
112300970526     C                   MOVEL     *BLANKS       DSMSMS
112400970526     C                   MOVEL     *BLANKS       DSMSKE
112500970526     C                   Z-ADD     *ZEROS        DSMSER
112600970526     C*---
112700970526     C* VARIABILI RIFERITE AL DATA BASE
112800970526     C*---
112900970526     C*---
113000970526     C* CHIAVI DI LETTURA
113100970526     C*---
113200080305     C* LETTURA tivir05L -
113300991025     C     KEYvir        KLIST
113400991122     C                   KFLD                    KCEksc                         *PARTENZA/ARRIVO
113500080305     C                   KFLD                    KCEisv                         *PARTENZA/ARRIVO
113600080305     C                   KFLD                    KCEtip                         *PARTENZA/ARRIVO
113700140618
113800140618     C     ktab          KLIST
113900140618     C                   KFLD                    tblkut
114000140618     C                   KFLD                    tblcod
114100140618     C                   KFLD                    tblkey
114200140618     c                   move      1             tblkut
114300970527     C*
114400970527     C* REPERIMENTO DATI UTENTE
114500020916     C                   EXSR      REPDATIUTE
114600970527     C*
114700970527     C* CARICA LE TABELLE OCCORRENTI
114800991025     C*                  EXSR      CARTAB
114900970526     C*
115000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
115100991025     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
115200991025     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
115300970526     C*
115400970526     C* CALCOLA LA DATA CORRENTE
115500970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
115600970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
115700970526     C                   Z-ADD     WN8           G08DAT
115800970526     C                   Z-ADD     *ZEROS        G08INV
115900970526     C                   MOVEL     '0'           G08ERR
116000970526     C                   CALL      'XSRDA8'
116100970526     C                   PARM                    WLBDA8
116200970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
116300970526     C*
116400001020     C* IMPOSTA I PARAMETRI DI INGRESSO
116500070919     C                   MOVE      s20tip        VIDti1
116600070919     C                   MOVE      s20tip        DEPti1
116700001020     C                   MOVE      s20ksc        VIDks1
116800001020     C                   MOVE      s20ksc        DEPks1
116900970526     C                   EXSR      CHKPAR
117000970526IF  1C     ERRL00        IFEQ      '1'
117100991025     C                   MOVEL     '1'           s20ERR
117200991025     C                   MOVEL     DSMSMS        s20MSG
117300970526X   1C                   ELSE
117400991025     C                   MOVEL     VIDtip        DEPtip                         *DEPOSITI
117500970526E   1C                   ENDIF
117600970526     C*
117700970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
117800970526     C                   EXSR      CHKFOP
117900970526     C*
118000970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
118100970530IF  1C     DEPOP0        IFEQ      'P01'
118200970530     C                   MOVEL     DEC(3)        VIDING
118300970530E   1C                   ENDIF
118400970530IF  1C     DEPOP0        IFEQ      'P02'
118500970526     C                   MOVEL     DEC(1)        VIDING
118600970530E   1C                   ENDIF
118700970530IF  1C     DEPOP0        IFEQ      'P05'
118800970526     C                   MOVEL     DEC(2)        VIDING
118900970526E   1C                   ENDIF
119000970526     C*
119100970526     C                   ENDSR
119200970526     C*------------------------------------------------------------------------*
119300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
119400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
119500970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
119600970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
119700970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
119800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
119900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
120000970526     C*------------------------------------------------------------------------*
120100970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
120200970526     C*------------------------------------------------------------------------*
120300970526     C     POSSFL        BEGSR
120400970526     C*
120500970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
120600970526     C                   EXSR      CLRSFL
120700970526     C*
120800970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
120900970526     C                   EXSR      SETFIL
121000970526     C*
121100970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
121200970526     C     SFLEOF        IFEQ      0
121300970526     C                   EXSR      CARSFL
121400970526     C                   END
121500970526     C*
121600970526     C                   ENDSR
121700970526     C*------------------------------------------------------------------------*
121800970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
121900970526     C*------------------------------------------------------------------------*
122000970526     C     ENTATT        BEGSR
122100970526     C*
122200970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
122300970526     C                   Z-ADD     0             OPZOK             1 0
122400970526     C*
122500970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
122600970526     C                   Z-ADD     0             SFLVAR            5 0
122700970526     C*
122800970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
122900970526     C                   Z-ADD     0             SFLERR
123000970526     C*
123100970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
123200970526     C                   Z-ADD     SFLULT        WFLULT
123300970526     C*
123400970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
123500991025     C                   READC     s722D05                                98
123600970526     C     *IN98         DOWEQ     '0'
123700970526     C*
123800970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
123900970526     C                   ADD       1             SFLVAR
124000970526     C*
124100970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
124200970526     C     OPZOK         IFEQ      0
124300970526     C*
124400970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
124500970526     C     OPZKEY        IFNE      *BLANKS
124600970526     C                   EXSR      OPZCON
124700970526     C                   END
124800970526     C*
124900970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
125000970526     C*            2. PULIZIA SCELTA IN SFL
125100970526     C     OPZOK         IFEQ      0
125200970526     C                   MOVEL     '0'           *IN41
125300970526     C                   MOVEL     *BLANKS       OPZKEY
125400970526     C                   ELSE
125500970526     C*
125600970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
125700970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
125800970526     C                   MOVEL     '1'           *IN41
125900970526     C                   Z-ADD     SFLULT        SFLERR
126000970526     C                   END
126100970526     C                   ELSE
126200970526     C*
126300970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
126400970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
126500970526     C                   MOVEL     '1'           *IN41
126600970526     C                   END                                                    OPZOK=0
126700970526     C*
126800970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
126900970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
127000970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
127100970526     C     OPZOK         IFNE      0
127200970526     C                   Z-ADD     SFLERR        SFLNBR
127300970526     C                   ELSE
127400970526     C                   Z-ADD     SFLULT        SFLNBR
127500970526     C                   END
127600970526     C*
127700970526     C* AGGIORNO RECORD SFL
127800991025     C                   UPDATE    s722D05
127900970526     C*
128000991025     C                   READC     s722D05                                98
128100970526     C                   END                                                    DO WHILE
128200970526     C*
128300970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
128400970526     C                   Z-ADD     WFLULT        SFLULT
128500970526     C*
128600970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
128700970526     C     SFLVAR        IFEQ      0
128800970526     C*---                 MOVEL'1'       FINL00
128900970526     C                   END
129000970526     C*
129100970526     C                   ENDSR
129200970526     C*------------------------------------------------------------------------*
129300970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
129400970526     C*------------------------------------------------------------------------*
129500970526     C     CLRSFL        BEGSR
129600970526     C*
129700970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
129800970526     C* (0=NON FINE FILE  1=FINE FILE)
129900970526     C                   Z-ADD     0             SFLEOF
130000970526     C*
130100970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
130200970526     C                   Z-ADD     0             SFLULT
130300970526     C*
130400970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
130500970526     C                   MOVEL     '1'           *IN40
130600991025     C                   WRITE     s722D06
130700970526     C                   MOVEL     '0'           *IN40
130800970526     C*
130900970526     C                   ENDSR
131000970526     C*------------------------------------------------------------------------*
131100970526     C* CARSFL - CARICAMENTO SFL
131200970526     C*------------------------------------------------------------------------*
131300970526     C     CARSFL        BEGSR
131400970526     C*
131500970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
131600970526     C     SFLULT        IFEQ      0
131700970526     C                   EXSR      SETFIL
131800970526     C                   END
131900970526     C*
132000970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
132100970526     C                   Z-ADD     SFLULT        WFLULT
132200970526     C*
132300970526     C* CICLO:
132400970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
132500970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
132600970526     C                   Z-ADD     1             SFLRIG
132700970526     C     SFLRIG        DOWLE     SFLPAG
132800970526     C     SFLEOF        ANDEQ     0
132900970526     C*
133000970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
133100970526     C                   MOVEL     *BLANKS       OPZKEY
133200970526     C*
133300970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
133400970526     C                   EXSR      IRESFL
133500970526     C*
133600970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
133700970526     C                   ADD       1             SFLULT
133800970526     C*
133900970526     C* SCRITTURA RECORD SFL
134000991025     C                   WRITE     s722D05
134100970526     C                   ADD       1             SFLRIG
134200970526     C*
134300970526     C* LETTURA FILE DA CARICARE
134400970526     C                   EXSR      LETFIL
134500970526     C                   END                                                    DO WHILE
134600970526     C*
134700970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
134800970526     C     WFLULT        IFNE      SFLULT
134900970526     C                   Z-ADD     WFLULT        SFLNBR
135000970526     C                   ADD       1             SFLNBR
135100970526     C                   END
135200970526     C*
135300970526     C                   ENDSR
135400970526     C*------------------------------------------------------------------------*
135500970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
135600970526     C*------------------------------------------------------------------------*
135700970526     C     OUTSFL        BEGSR
135800970526     C*
135900970526     C* WRITE RECORD TESTATA VIDEO
136000991025     C                   WRITE     s722D01
136100970526     C*
136200970526     C* VISUALIZZAZIONE MESSAGGIO
136300970526     C                   EXSR      SNDMSG
136400970526     C*
136500970526     C* WRITE RECORD OPZIONI VIDEO
136600991025     C                   WRITE     s722D02
136700970526     C*
136800970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
136900991025     C                   WRITE     s722D03
137000970526     C*
137100970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
137200991025     C                   WRITE     s722D04
137300970526     C*
137400970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
137500970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
137600970526     C     SFLEOF        IFEQ      0
137700970526     C                   MOVEL     '0'           *IN42
137800970526     C                   ELSE
137900970526     C*
138000970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
138100970526     C                   MOVEL     '1'           *IN42
138200970526     C                   END
138300970526     C*
138400970526     C* WRITE RECORD TASTI FUNZIONALI
138500991025     C                   WRITE     s722D09
138600970526     C*
138700970526     C* CASO DI SFL VUOTO
138800970526     C     SFLULT        IFLE      0
138900970526     C*
139000970526     C*    - WRITE RECORD VUOTO
139100991025     C                   WRITE     s722D0A
139200991025     C                   READ      s722D0A                                99
139300970526     C*
139400970526     C                   ELSE
139500970526     C*
139600970526     C* CASO DI SFL CON RECORDS
139700970526     C*
139800970526     C*    - WRITE RECORD CONTROLLO SFL
139900991025     C                   WRITE     s722D06
140000991025     C                   READ      s722D06                                99
140100970526     C                   END
140200970526     C*
140300970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
140400991025     C                   READ      s722D03                                99
140500970526     C*
140600970526     C                   ENDSR
140700970526     C*------------------------------------------------------------------------*
140800970526     C* SNDMSG - MANDA MESSAGGIO
140900970526     C*------------------------------------------------------------------------*
141000970526     C     SNDMSG        BEGSR
141100970526     C*
141200970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
141300970526     C                   MOVEL     '1'           *IN40
141400991025     C                   WRITE     s722DMC
141500970526     C                   MOVEL     '0'           *IN40
141600970526     C*
141700970526     C* VISUALIZZAZIONE MESSAGGIO
141800970526     C                   Z-ADD     0             MSGULT
141900970526     C                   CALL      'XMSG'        XMSGPA
142000970526     C     DSMSER        IFEQ      0
142100970526     C                   MOVEL     DSMSKE        MSGKEY
142200970526     C                   END
142300970526     C                   Z-ADD     1             MSGULT
142400991025     C                   WRITE     s722DMS                                      SFL MESS
142500991025     C                   WRITE     s722DMC                                      CONTR MESS
142600970526     C*
142700970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
142800970526     C                   MOVEL     *BLANKS       DSMSMS
142900970526     C*
143000970526     C                   ENDSR
143100970526     C*------------------------------------------------------------------------*
143200970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
143300970526     C*------------------------------------------------------------------------*
143400970526     C     FUNCON        BEGSR
143500970526     C*
143600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
143700970526     C                   EXSR      FUNVAR
143800970526     C*
143900970526     C* PULISCO CAMPI DI LAVORO
144000970526     C                   Z-ADD     0             I                 5 0
144100970526     C                   Z-ADD     0             W                 5 0
144200970526     C*
144300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
144400970526     C     FUNKEY        IFNE      *BLANKS
144500970526     C     1             DO        24            I
144600970526     C                   MOVEL     FUD(I)        FUDRIG
144700970526     C*
144800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
144900970526     C     FUNKEY        IFEQ      FUDFUN
145000970526     C     FUDATT        ANDEQ     'S'
145100970526     C                   Z-ADD     I             W
145200970526     C                   END
145300970526     C                   END                                                    DO WHILE
145400970526     C*
145500970526     C* FUNZIONE NON ATTIVA (ERRORE)
145600970526     C     W             IFEQ      0
145700970526     C*
145800970526     C* EMISSIONE MESSAGGIO DI ERRORE
145900970526     C                   MOVEL     MSG(2)        DSMSMS
146000970526     C                   END                                                    W=0
146100970526     C                   END                                                    FUNKEY NE *BLANKS
146200970526     C*
146300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
146400970526     C     W             IFNE      0
146500970526     C     FUNKEY        OREQ      *BLANKS
146600970526     C*
146700970526     C     FUNKEY        IFEQ      *BLANKS
146800970526     C                   EXSR      ENTGES
146900970526     C                   END
147000970526     C     FUNKEY        IFEQ      '03'
147100970526     C                   EXSR      F03GES
147200970526     C                   END
147300970526     C     FUNKEY        IFEQ      '05'
147400970526     C                   EXSR      F05GES
147500970526     C                   END
147600970526     C     FUNKEY        IFEQ      '06'
147700970526     C                   EXSR      F06GES
147800970526     C                   END
147900140618     C     FUNKEY        IFEQ      '07'
148000140618     C                   EXSR      F07GES
148100140618     C                   END
148200970526     C     FUNKEY        IFEQ      '12'
148300970526     C                   EXSR      F12GES
148400970526     C                   END
148500970528     C     FUNKEY        IFEQ      '21'
148600970528     C                   EXSR      F21GES
148700970528     C                   END
148800970528     C     FUNKEY        IFEQ      '22'
148900970528     C                   EXSR      F22GES
149000970528     C                   END
149100970526     C     FUNKEY        IFEQ      '23'
149200970526     C                   EXSR      F23GES
149300970526     C                   END
149400970526     C     FUNKEY        IFEQ      '24'
149500970526     C                   EXSR      F24GES
149600970526     C                   END
149700970526     C     FUNKEY        IFEQ      'RU'
149800970526     C                   EXSR      RUPGES
149900970526     C                   END
150000970526     C*
150100970526     C                   END
150200970526     C*
150300970526     C                   ENDSR
150400970526     C*------------------------------------------------------------------------*
150500970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
150600970526     C*------------------------------------------------------------------------*
150700970526     C     FUNVAR        BEGSR
150800970526     C*
150900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
151000970526     C     KEY           IFEQ      ENTER
151100970526     C                   MOVEL     *BLANKS       FUNKEY
151200970526     C                   END
151300970526     C     KEY           IFEQ      F1
151400970526     C                   MOVEL     '01'          FUNKEY
151500970526     C                   END
151600970526     C     KEY           IFEQ      F2
151700970526     C                   MOVEL     '02'          FUNKEY
151800970526     C                   END
151900970526     C     KEY           IFEQ      F3
152000970526     C                   MOVEL     '03'          FUNKEY
152100970526     C                   END
152200970526     C     KEY           IFEQ      F4
152300970526     C                   MOVEL     '04'          FUNKEY
152400970526     C                   END
152500970526     C     KEY           IFEQ      F5
152600970526     C                   MOVEL     '05'          FUNKEY
152700970526     C                   END
152800970526     C     KEY           IFEQ      F6
152900970526     C                   MOVEL     '06'          FUNKEY
153000970526     C                   END
153100970526     C     KEY           IFEQ      F7
153200970526     C                   MOVEL     '07'          FUNKEY
153300970526     C                   END
153400970526     C     KEY           IFEQ      F8
153500970526     C                   MOVEL     '08'          FUNKEY
153600970526     C                   END
153700970526     C     KEY           IFEQ      F9
153800970526     C                   MOVEL     '09'          FUNKEY
153900970526     C                   END
154000970526     C     KEY           IFEQ      F10
154100970526     C                   MOVEL     '10'          FUNKEY
154200970526     C                   END
154300970526     C     KEY           IFEQ      F11
154400970526     C                   MOVEL     '11'          FUNKEY
154500970526     C                   END
154600970526     C     KEY           IFEQ      F12
154700970526     C                   MOVEL     '12'          FUNKEY
154800970526     C                   END
154900970526     C     KEY           IFEQ      F13
155000970526     C                   MOVEL     '13'          FUNKEY
155100970526     C                   END
155200970526     C     KEY           IFEQ      F14
155300970526     C                   MOVEL     '14'          FUNKEY
155400970526     C                   END
155500970526     C     KEY           IFEQ      F15
155600970526     C                   MOVEL     '15'          FUNKEY
155700970526     C                   END
155800970526     C     KEY           IFEQ      F16
155900970526     C                   MOVEL     '16'          FUNKEY
156000970526     C                   END
156100970526     C     KEY           IFEQ      F17
156200970526     C                   MOVEL     '17'          FUNKEY
156300970526     C                   END
156400970526     C     KEY           IFEQ      F18
156500970526     C                   MOVEL     '18'          FUNKEY
156600970526     C                   END
156700970526     C     KEY           IFEQ      F19
156800970526     C                   MOVEL     '19'          FUNKEY
156900970526     C                   END
157000970526     C     KEY           IFEQ      F20
157100970526     C                   MOVEL     '20'          FUNKEY
157200970526     C                   END
157300970526     C     KEY           IFEQ      F21
157400970526     C                   MOVEL     '21'          FUNKEY
157500970526     C                   END
157600970526     C     KEY           IFEQ      F22
157700970526     C                   MOVEL     '22'          FUNKEY
157800970526     C                   END
157900970526     C     KEY           IFEQ      F23
158000970526     C                   MOVEL     '23'          FUNKEY
158100970526     C                   END
158200970526     C     KEY           IFEQ      F24
158300970526     C                   MOVEL     '24'          FUNKEY
158400970526     C                   END
158500970526     C     KEY           IFEQ      ROLLUP
158600970526     C                   MOVEL     'RU'          FUNKEY
158700970526     C                   END
158800970526     C     KEY           IFEQ      HELP
158900970526     C                   MOVEL     'HP'          FUNKEY
159000970526     C                   END
159100970526     C*
159200970526     C                   ENDSR
159300970526     C*------------------------------------------------------------------------*
159400970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
159500970526     C*------------------------------------------------------------------------*
159600970526     C     FUNGES        BEGSR
159700970526     C*
159800970526     C* PULISCO CAMPI DI LAVORO
159900970526     C                   Z-ADD     0             W                 5 0
160000970526     C*
160100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
160200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
160300970526     C     1             DO        24            I
160400970526     C                   MOVEL     FUD(I)        FUDRIG
160500970526     C*
160600970526     C* PULIZIA FUNZIONI PRECEDENTI
160700970526     C                   MOVEL     *BLANKS       FUV(I)
160800970526     C*
160900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
161000970526     C* (SALTA F23 E F24)
161100970526     C     FUDATT        IFEQ      'S'
161200970526     C     FUDFUN        ANDNE     '23'
161300970526     C     FUDFUN        ANDNE     '24'
161400970526     C                   ADD       1             W
161500970526     C                   MOVEL     FUDDES        FUV(W)
161600970526     C                   END
161700970526     C                   END                                                    DO WHILE
161800970526     C*
161900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
162000970526     C* ALTRIMENTI DISATTIVO F24
162100970526     C     1             DO        24            I
162200970526     C                   MOVEL     FUD(I)        FUDRIG
162300970526     C*
162400970526     C     FUDFUN        IFEQ      '24'
162500970526     C     W             IFGT      6
162600970526     C                   MOVEL     'S'           FUDATT
162700970526     C                   ELSE
162800970526     C                   MOVEL     *BLANKS       FUDATT
162900970526     C                   END                                                    W GT 8
163000970526     C                   MOVEL     FUDRIG        FUD(I)
163100970526     C                   END
163200970526     C                   END                                                    DO WHILE
163300970526     C*
163400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
163500970526     C     W             DIV       6             I
163600970526     C                   MVR                     Z
163700970526     C*
163800970526     C     Z             IFNE      0
163900970526     C     I             ADD       1             Z
164000970526     C                   ELSE
164100970526     C                   Z-ADD     I             Z
164200970526     C                   END
164300970526     C*
164400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
164500970526     C                   ADD       1             FUNGRU
164600970526     C     FUNGRU        IFGT      Z
164700970526     C     FUNGRU        ORLT      1
164800970526     C                   Z-ADD     1             FUNGRU
164900970526     C                   END
165000970526     C*
165100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
165200970526     C     FUNGRU        IFEQ      1
165300970526     C                   MOVEL     FUV(1)        FUN001
165400970526     C                   MOVEL     FUV(2)        FUN002
165500970526     C                   MOVEL     FUV(3)        FUN003
165600970526     C                   MOVEL     FUV(4)        FUN004
165700970526     C                   MOVEL     FUV(5)        FUN005
165800970526     C                   MOVEL     FUV(6)        FUN006
165900970526     C                   END
166000970526     C*
166100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
166200970526     C     FUNGRU        IFEQ      2
166300970526     C                   MOVEL     FUV(7)        FUN001
166400970526     C                   MOVEL     FUV(8)        FUN002
166500970526     C                   MOVEL     FUV(9)        FUN003
166600970526     C                   MOVEL     FUV(10)       FUN004
166700970526     C                   MOVEL     FUV(11)       FUN005
166800970526     C                   MOVEL     FUV(12)       FUN006
166900970526     C                   END
167000970526     C*
167100970526     C* TERZO GRUPPO DI 6 FUNZIONI
167200970526     C     FUNGRU        IFEQ      3
167300970526     C                   MOVEL     FUV(13)       FUN001
167400970526     C                   MOVEL     FUV(14)       FUN002
167500970526     C                   MOVEL     FUV(15)       FUN003
167600970526     C                   MOVEL     FUV(16)       FUN004
167700970526     C                   MOVEL     FUV(17)       FUN005
167800970526     C                   MOVEL     FUV(18)       FUN006
167900970526     C                   END
168000970526     C*
168100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
168200970526     C     FUNGRU        IFEQ      4
168300970526     C                   MOVEL     FUV(19)       FUN001
168400970526     C                   MOVEL     FUV(20)       FUN002
168500970526     C                   MOVEL     FUV(21)       FUN003
168600970526     C                   MOVEL     FUV(22)       FUN004
168700970526     C                   MOVEL     *BLANKS       FUN005
168800970526     C                   MOVEL     *BLANKS       FUN006
168900970526     C                   END
169000970526     C*
169100970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
169200970526     C                   MOVEL     *BLANKS       FUN007
169300970526     C                   MOVEL     *BLANKS       FUN008
169400970526     C*
169500970526     C     1             DO        24            I
169600970526     C                   MOVEL     FUD(I)        FUDRIG
169700970526     C     FUDFUN        IFEQ      '23'
169800970526     C     FUDATT        ANDEQ     'S'
169900970526     C                   MOVEL     FUDDES        FUN007
170000970526     C                   END
170100970526     C     FUDFUN        IFEQ      '24'
170200970526     C     FUDATT        ANDEQ     'S'
170300970526     C                   MOVEL     FUDDES        FUN008
170400970526     C                   END
170500970526     C                   END                                                    DO
170600970526     C*
170700970526     C                   ENDSR
170800970526     C*------------------------------------------------------------------------*
170900970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
171000970526     C*------------------------------------------------------------------------*
171100970526     C     OPZGES        BEGSR
171200970526     C*
171300970526     C* PULISCO CAMPI DI LAVORO
171400970526     C                   Z-ADD     0             W                 5 0
171500970526     C*
171600970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
171700970526     C* E CARICO TABELLA OPZIONI ATTIVE
171800970526     C     1             DO        24            I
171900970526     C                   MOVEL     OPD(I)        OPDRIG
172000970526     C*
172100970526     C* PULIZIA OPZIONI PRECEDENTI
172200970526     C                   MOVEL     *BLANKS       OPV(I)
172300970526     C*
172400970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
172500970526     C     OPDATT        IFEQ      'S'
172600970526     C                   ADD       1             W
172700970526     C                   MOVEL     OPDDES        OPV(W)
172800970526     C                   END
172900970526     C                   END                                                    DO WHILE
173000970526     C*
173100970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
173200970526     C* ALTRIMENTI DISATTIVO F23
173300970526     C     1             DO        24            I
173400970526     C                   MOVEL     FUD(I)        FUDRIG
173500970526     C*
173600970526     C     FUDFUN        IFEQ      '23'
173700970526     C     W             IFGT      8
173800970526     C                   MOVEL     'S'           FUDATT
173900970526     C                   ELSE
174000970526     C                   MOVEL     *BLANKS       FUDATT
174100970526     C                   END                                                    W GT 8
174200970526     C                   MOVEL     FUDRIG        FUD(I)
174300970526     C                   END
174400970526     C                   END                                                    DO WHILE
174500970526     C*
174600970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
174700970526     C     W             DIV       8             I
174800970526     C                   MVR                     Z                 5 0
174900970526     C*
175000970526     C     Z             IFNE      0
175100970526     C     I             ADD       1             Z
175200970526     C                   ELSE
175300970526     C                   Z-ADD     I             Z
175400970526     C                   END
175500970526     C*
175600970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
175700970526     C                   ADD       1             OPZGRU
175800970526     C     OPZGRU        IFGT      Z
175900970526     C     OPZGRU        ORLT      1
176000970526     C                   Z-ADD     1             OPZGRU
176100970526     C                   END
176200970526     C*
176300970526     C* PRIMO GRUPPO DI 8 OPZIONI
176400970526     C     OPZGRU        IFEQ      1
176500970526     C                   MOVEL     OPV(1)        OPZ001
176600970526     C                   MOVEL     OPV(2)        OPZ002
176700970526     C                   MOVEL     OPV(3)        OPZ003
176800970526     C                   MOVEL     OPV(4)        OPZ004
176900970526     C                   MOVEL     OPV(5)        OPZ005
177000970526     C                   MOVEL     OPV(6)        OPZ006
177100970526     C                   MOVEL     OPV(7)        OPZ007
177200970526     C                   MOVEL     OPV(8)        OPZ008
177300970526     C                   END
177400970526     C*
177500970526     C* SECONDO GRUPPO DI 8 OPZIONI
177600970526     C     OPZGRU        IFEQ      2
177700970526     C                   MOVEL     OPV(9)        OPZ001
177800970526     C                   MOVEL     OPV(10)       OPZ002
177900970526     C                   MOVEL     OPV(11)       OPZ003
178000970526     C                   MOVEL     OPV(12)       OPZ004
178100970526     C                   MOVEL     OPV(13)       OPZ005
178200970526     C                   MOVEL     OPV(14)       OPZ006
178300970526     C                   MOVEL     OPV(15)       OPZ007
178400970526     C                   MOVEL     OPV(16)       OPZ008
178500970526     C                   END
178600970526     C*
178700970526     C* TERZO GRUPPO DI 8 OPZIONI
178800970526     C     OPZGRU        IFEQ      3
178900970526     C                   MOVEL     OPV(17)       OPZ001
179000970526     C                   MOVEL     OPV(18)       OPZ002
179100970526     C                   MOVEL     OPV(19)       OPZ003
179200970526     C                   MOVEL     OPV(20)       OPZ004
179300970526     C                   MOVEL     OPV(21)       OPZ005
179400970526     C                   MOVEL     OPV(22)       OPZ006
179500970526     C                   MOVEL     OPV(23)       OPZ007
179600970526     C                   MOVEL     OPV(24)       OPZ008
179700970526     C                   END
179800970526     C*
179900970526     C                   ENDSR
180000970526     C*------------------------------------------------------------------------*
180100970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
180200970526     C*------------------------------------------------------------------------*
180300970526     C     OPZCON        BEGSR
180400970526     C*
180500970526     C* PULISCO CAMPI DI LAVORO
180600970526     C                   Z-ADD     0             I                 5 0
180700970526     C                   Z-ADD     0             W                 5 0
180800970526     C*
180900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
181000970526     C     1             DO        24            I
181100970526     C                   MOVEL     OPD(I)        OPDRIG
181200970526     C*
181300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
181400970526     C     OPZKEY        IFEQ      OPDOPZ
181500970526     C     OPZKEY        OREQ      OPDOP1
181600970526     C*
181700970526     C     OPDATT        IFEQ      'S'
181800970526     C                   MOVEL     OPDOPZ        OPZKEY
181900970526     C                   Z-ADD     I             W
182000970526     C                   END
182100970526     C                   END
182200970526     C*
182300970526     C                   END                                                    DO WHILE
182400970526     C*
182500970526     C* OPZIONE NON ATTIVA (ERRORE)
182600970526     C     W             IFEQ      0
182700970526     C*
182800970526     C* OPZIONE NON OK
182900970526     C                   Z-ADD     1             OPZOK
183000970526     C*
183100970526     C* EMISSIONE MESSAGGIO DI ERRORE
183200970526     C                   MOVEL     MSG(1)        DSMSMS
183300970526     C                   ELSE
183400970526     C*
183500970526     C* OPZIONE ATTIVA (PROCEDI ...)
183600970526     C*
183700970526     C     OPZKEY        IFEQ      ' 1'
183800970526     C                   EXSR      O01GES
183900970526     C                   END
184000970526     C     OPZKEY        IFEQ      ' 2'
184100970526     C                   EXSR      O02GES
184200970526     C                   END
184300991014     C     OPZKEY        IFEQ      ' 3'
184400991014     C                   EXSR      O03GES
184500991014     C                   END
184600991104     C*    OPZKEY        IFEQ      ' 4'
184700991104     C*                  EXSR      O04GES
184800991104     C*                  END
184900970526     C     OPZKEY        IFEQ      ' 5'
185000970526     C                   EXSR      O05GES
185100970526     C                   END
185200970526     C     OPZKEY        IFEQ      ' 7'
185300970526     C                   EXSR      O07GES
185400970526     C                   END
185500051013     C     OPZKEY        IFEQ      '10'
185600051013     C                   EXSR      O10GES
185700051013     C                   END
185800991028     C     OPZKEY        IFEQ      ' 9'
185900991028     C                   EXSR      O09GES
186000991028     C                   END
186100001020     C     OPZKEY        IFEQ      '11'
186200001020     C                   EXSR      O11GES
186300000703     C                   END
186400130129     C     OPZKEY        IFEQ      '22'
186500130129     C                   EXSR      O22GES
186600130129     C                   END
186700970526     C                   END                                                    OPZ.NON ATTIVA
186800970526     C*
186900970526     C                   ENDSR
187000970526     C*------------------------------------------------------------------------*
187100970526     C* NOTE COSTRUZIONE SCHIERE
187200970526     C*------------------------------------------------------------------------*
187300970526     C*
187400970526     C* -MSG-MESSAGGI
187500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
187600970526     C* (NON TOCCARE MSG,1 E MSG,2)
187700970526     C*
187800970526     C* -CMD-COMANDI
187900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
188000970526     C*
188100970526     C* -OPD-OPZIONI
188200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
188300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
188400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
188500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
188600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
188700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
188800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
188900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
189000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
189100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
189200970526     C*
189300970526     C* -FUD-FUNZIONI
189400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
189500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
189600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
189700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
189800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
189900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
190000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
190100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
190200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
190300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
190400970526     O*------------------------------------------------------------------------*
190500970526** DEC - DECODIFICHE
190600970526    GESTIONE                                                     1
190700970526INTERROGAZIONE                                                   2
190800970530     SCELTA                                                      3
1909009705264
1910009705265
191100970526** MSG - MESSAGGI
191200991025TIS722R- Opzione non gestita                                                   1
191300991025TIS722R- Tasto funzionale non gestito                                          2
191400991025TIS722R- Non abilitato alla gestione di questo CAP                             3
191500991025TIS722R- Record annullato. Consentito solo il ripristino                       4
191600991025TIS722R- Record non annullato. Impossibile ripristinare                        5
191700991025TIS722R- Accettato solo un parametro di inizio lista                           6
191800991025TIS722R- 7                                                                     7
191900991025TIS722R- 8                                                                     8
192000000801TIS722R- Tipo servizio errato o inesistente.                                   9
192100991025TIS722R- 10                                                                   10
192200000801TIS722R- Codice tipo file inesistente in tabella                              11
192300991025TIS722R- Data errata                                                          12
192400991025TIS722R- 13                                                                   13
192500991025TIS722R- 14                                                                   14
192600991025TIS722R- 15                                                                   15
192700991025TIS722R- 16                                                                   16
192800991025TIS722R- 17                                                                   17
192900991025TIS722R- 18                                                                   18
193000991025TIS722R- 19                                                                   19
193100991025TIS722R- 20                                                                   20
193200970528** CMD - COMANDI CL
193300970528WRKSPLF
193400970528
193500970528
193600970528
193700970528
193800970526** OPD - OPZIONI
193900970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
194000970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
194100991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
194200991104 4=                -4 -     (S/N)=N                        opz. 04¬
194300970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
194400000703                   -6 -     (S/N)=N                        opz. 06¬
194500970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
194600970527                   -8 -     (S/N)=N                        opz. 08¬
194700991028 9=Cliente Unif.   -9 -     (S/N)=N                        opz. 09¬
19480005101310=Visualizza Logs -10-     (S/N)=N                        opz. 10¬
19490013012911=Legami tipifile -11-     (S/N)=N                        opz. 11¬
195000970526                   -  -     (S/N)=N                        opz. 12¬
195100970526                   -  -     (S/N)=N                        opz. 13¬
195200970526                   -  -     (S/N)=N                        opz. 14¬
195300970526                   -  -     (S/N)=N                        opz. 15¬
195400970526                   -  -     (S/N)=N                        opz. 16¬
195500970526                   -  -     (S/N)=N                        opz. 17¬
195600970526                   -  -     (S/N)=N                        opz. 18¬
195700970526                   -  -     (S/N)=N                        opz. 19¬
195800970526                   -  -     (S/N)=N                        opz. 20¬
195900970526                   -  -     (S/N)=N                        opz. 21¬
19600013012922=Tabelle VAS     -22-     (S/N)=N                        opz. 22¬
196100970526                   -  -     (S/N)=N                        opz. 23¬
196200970526                   -  -     (S/N)=N                        opz. 24¬
196300970526** FUD - FUNZIONI
196400970526                   -        (S/N)=N                        funz.01¬
196500970526                   -        (S/N)=N                        funz.02¬
196600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
196700970526                   -        (S/N)=N                        funz.04¬
196800970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
196900970526F06=Inserimento    -        (S/N)=N                        funz.06¬
197000140618F07=Unif. 3C       -        (S/N)=S                        funz.07¬
197100970527                   -        (S/N)=N                        funz.08¬
197200970527                   -        (S/N)=N                        funz.09¬
197300970527                   -        (S/N)=N                        funz.10¬
197400970526                   -        (S/N)=N                        funz.11¬
197500970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
197600970526                   -        (S/N)=N                        funz.13¬
197700970526                   -        (S/N)=N                        funz.14¬
197800970526                   -        (S/N)=N                        funz.15¬
197900970526                   -        (S/N)=N                        funz.16¬
198000970526                   -        (S/N)=N                        funz.17¬
198100970526                   -        (S/N)=N                        funz.18¬
198200970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
198300970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
198400980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
198500970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
198600970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
198700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
