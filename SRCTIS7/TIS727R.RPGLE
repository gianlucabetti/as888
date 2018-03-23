000100970526     H*------------------------------------------------------------------------*
000200991025     H* Gestione - regole file internet x cliente -
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800000703     FTIS727D   CF   E             WORKSTN INFDS(DEVDS1)
000900000703     F                                     SFILE(S727D05:SFLULT)
001000000703     F                                     SFILE(S727DMS:MSGULT)
001100970526     F*------------
001200000707     Ftivre01l  UF A E           K DISK
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
008700000705     D*------------------
008800000705     D* DS REPERIMENTO DATI UTENTE
008900000705     D*-------------------
009000000705     DBS69DS         E DS                  extname(TIBS69DS)
009100000705     DACODS          E DS                  extname(CNACO00F)
009200000705     DINDDS          E DS                  extname(CNIND00F)
009300000705     DCLPDS          E DS                  extname(CNCLP00F)
009400000705     DCLSDS          E DS                  extname(FNCLS00F)
009500970527     D*------------------
009600970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009700970527     D*------------------
009800970527     D WLBDA8          DS                  INZ
009900970527     D  G08DAT                 1      8  0
010000970527     D  G08INV                 9     16  0
010100970527     D  G08ERR                17     17
010200970527     D  G08TGI                18     22  0
010300970526     D*------------
010400970526     D* DS DI PROCEDURA
010500000703     D*------------
010600000703     D tis720        e ds                  extname(tis720ds)
010700000703     D tibs02ds      e ds
010800970526     D*------------
010900970526     D* ARCHITETTURA
011000970526     D*------------
011100970526     D KPJBA         E DS
011200970526     C*------------------------------------------------------------------------*
011300970526     C* MAIN LINES
011400970526     C*------------------------------------------------------------------------*
011500970526     C*
011600970526     C* SE NON CI SONO ERRORI, PROSEGUE
011700970526IF  1C     ERRL00        IFNE      '1'
011800970526     C*
011900970526     C* CARICA IL SFILE
012000970526     C                   EXSR      POSSFL
012100970526     C*
012200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012400000705     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012600970526     C*
012700970526     C* REIMPOSTA ERRORE DI PROCERURA
012800970526     C                   MOVEL     '0'           ERRL00
012900970526     C*
013000970526     C* EMETTE IL SFILE
013100970526     C                   EXSR      OUTSFL
013200970526     C*
013300970526     C* GESTIONE TASTI FUNZIONALI
013400970526     C                   EXSR      FUNCON
013500970526     C*
013600970526E   2C                   ENDDO
013700970527     C*
013800970527     C* CI SONO ERRORI IN ENTRATA
013900970527X   1C                   ELSE
014000991025     C                   MOVEL     '1'           s20ERR
014100991025     C                   MOVEL     DSMSMS        s20MSG
014200970526E   1C                   ENDIF
014300991122     C*
014400991122     C                   EXSR      FINSR
014500970526     C* FINE PGM
014600970526     C                   SETON                                        LR
014700970526     C*------------------------------------------------------------------------*
014800970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014900970526     C*------------------------------------------------------------------------*
015000970526     C     F03GES        BEGSR
015100970526     C*
015200970526     C* IMPOSTA "PREMUTO F03"
015300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015400991025     C                   MOVEL     '1'           s20F03                         *DI PROCEDURA
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015900970526     C*------------------------------------------------------------------------*
016000970526     C     F05GES        BEGSR
016100970526     C*
016200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016300970526     C*
016400970526     C                   ENDSR
016500000705     C*------------------------------------------------------------------------*
016600000705     C* F06GES - GESTIONE F06 -> INSERIMENTO
016700000705     C*------------------------------------------------------------------------*
016800000705     C     F06GES        BEGSR
016900000705     C*
017000000705     C* INSERIMENTO CONSENTITO SOLO IN SEDE
017100020916IF  1C     DUTLPO        IFNE      'S'
017200000705     C                   MOVEL     '1'           ERRL00
017300000705     C                   MOVEL     MSG(3)        DSMSMS
017400000705E   1C                   ENDIF
017500000705     C*
017600000705IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017700000706     C                   MOVEL     *blanks       Dtfl
017800000707     C                   Z-ADD     *zeros        Dord
017900000705     C                   MOVEL     DEPOP0        WOP0
018000000705     C                   MOVEL     'F06'         WOP1
018100000705     C                   MOVEL     'TIS728R'     WPGM              8
018200000705     C                   EXSR      LANPGM                                       *LANCIA PGM
018300000705     C                   EXSR      POSSFL                                       *RICARICA SFILE
018400000705E   1C                   ENDIF
018500000705     C*
018600000705     C                   ENDSR
018700970526     C*------------------------------------------------------------------------*
018800970526     C* F12GES - GESTIONE F12 -> RITORNO
018900970526     C*------------------------------------------------------------------------*
019000970526     C     F12GES        BEGSR
019100970526     C*
019200970526     C* IMPOSTA "PREMUTO F12"
019300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019400991025     C                   MOVEL     '1'           s20F12                         *DI PROCEDURA
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
019900970526     C*------------------------------------------------------------------------*
020000970526     C     F23GES        BEGSR
020100970526     C*
020200970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F24GES        BEGSR
020900970526     C*
021000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
021500970526     C*------------------------------------------------------------------------*
021600970526     C     RUPGES        BEGSR
021700970526     C*
021800970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
021900970526     C*
022000970526     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200970526     C* ENTGES - GESTIONE ENTER -> INVIO
022300970526     C*------------------------------------------------------------------------*
022400970526     C     ENTGES        BEGSR
022500000703     C*
022600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
022700970526     C                   EXSR      NEWPOS
022800970526     C*
022900970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
023000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023100970526IF  2C     SWPOSI        IFNE      0
023200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
023300970526X   2C                   ELSE
023400970526IF  3C     SFLULT        IFNE      0
023500970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
023600970526     C*
023700970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
023800970526IF  4C     SWMODI        IFNE      0
023900970526     C                   EXSR      POSSFL
024000970526E   4C                   ENDIF
024100970526E   3C                   ENDIF
024200970526E   2C                   ENDIF
024300970526E   1C                   ENDIF
024400970526     C*
024500970526     C                   ENDSR
024600000705     C*------------------------------------------------------------------------*
024700000705     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
024800000705     C*------------------------------------------------------------------------*
024900000705     C     O02GES        BEGSR
025000000705     C*
025100000705     C* MODIFICA
025200000705IF  1C     ERRL00        IFEQ      '0'
025300000706     C                   MOVEL     VIDtfl        Dtfl
025400000707     C                   Z-ADD     VIDord        Dord
025500000705     C                   MOVEL     DEPOP0        WOP0
025600000705     C                   MOVEL     'O02'         WOP1
025700000706     C                   MOVEL     'TIS728R'     WPGM
025800000705     C                   EXSR      LANPGM
025900000705     c                   Z-ADD     1             swmodi
026000000705E   1C                   ENDIF
026100000705     C*
026200000705     C                   ENDSR
026300000707     C*------------------------------------------------------------------------*
026400000707     C* O04GES - GESTIONE OPZIONE 4=ELIMINA
026500000707     C*------------------------------------------------------------------------*
026600000707     C     O04GES        BEGSR
026700000707     C*
026800000707     C* ELIMINA
026900000707IF  1C     ERRL00        IFEQ      '0'
027000000707     C                   MOVEL     VIDtfl        Dtfl
027100000707     C                   Z-ADD     VIDord        Dord
027200000707     C                   MOVEL     DEPOP0        WOP0
027300000707     C                   MOVEL     'O04'         WOP1
027400000707     C                   MOVEL     'TIS728R'     WPGM
027500000707     C                   EXSR      LANPGM
027600000707     c                   Z-ADD     1             swmodi
027700000707E   1C                   ENDIF
027800000707     C*
027900000707     C                   ENDSR
028000000705     C*------------------------------------------------------------------------*
028100000705     C* LANPGM - LANCIA UN PROGRAMMA
028200000705     C*------------------------------------------------------------------------*
028300000705     C     LANPGM        BEGSR
028400000705     C*
028500000705     C* IMPOSTA IL PASSAGGIO PARAMETRI
028600000705     C                   MOVEL     WOP0          s20OP0                         *LIVELLO PROCEDURA
028700000705     C                   MOVEL     WOP1          s20OP1                         *LIVELLO PGM
028800000705     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
028900000705     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
029000000705     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
029100000705     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
029200000705     C                   MOVEL     VIDti1        s20tip                         *PARTENZA/ARRIVO
029300000705     C                   MOVEL     VIDks1        s20ksc                         *PARTENZA/ARRIVO
029400000705     C                   Z-ADD     Ddti          s20dti                         *PARTENZA/ARRIVO
029500000705     C                   MOVEL(p)  tis720        kpjbu
029600000705     C*
029700000705     C                   CALL      WPGM
029800000705     C                   PARM                    KPJBA
029900000705     C                   PARM                    VIDrag
030000000706     C                   PARM                    Dtfl              2
030100000707     C                   PARM                    Dord              3 0
030200000705     C*
030300000705     C                   ENDSR
030400970526     C*------------------------------------------------------------------------*
030500970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
030600970526     C*------------------------------------------------------------------------*
030700970526     C     NEWPOS        BEGSR
030800970526     C*
030900970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
031000970526     C*         1= RICHIESTO POSIZIONAMENTO
031100970526     C                   Z-ADD     0             SWPOSI            7 0
031200970526     C*
031300970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
031400970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
031500970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
031600970526     C                   Z-ADD     0             SWMODI            1 0
031700970530     C*
031800970530     C* REIMPOSTA INDICATORI DI ERRORE
031900970530     C                   SETOFF                                       808182
032000970530     C                   SETOFF                                       83
032100970526     C*
032200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
032300000703IF  1C     VIDks1        IFne      depks1
032400991025IF  1C     VIDti1        orne      depti1
032500970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
032600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
032700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
032800991025     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
032900991025     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
033000970526E   2C                   ENDIF
033100970526E   1C                   ENDIF
033200970526     C*
033300970526     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
033600970526     C*------------------------------------------------------------------------*
033700970528     C     SETFIL        BEGSR
033800970526     C*
033900970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
034000000703     C                   MOVEL     VIDks1        KCEksc
034100000703     C                   MOVEL     VIDti1        KCEtip
034200991025IF  1C     WTIPRI        IFEQ      'TIP'                                        *PER PARTENZA
034300000707     C     KEYvre        SETLL     tivre01l                           99
034400970528E   1C                   ENDIF
034500980107     C*
034600970526     C* LETTURA RECORD
034700970526IF  1C     *IN99         IFEQ      *OFF
034800970526     C                   EXSR      LETFIL
034900970526E   1C                   ENDIF
035000970526     C*
035100970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
035200970526IF  1C     *IN99         IFEQ      *ON
035300970526     C                   Z-ADD     1             SFLEOF
035400970526E   1C                   ENDIF
035500970526     C*
035600970526     C                   ENDSR
035700970526     C*------------------------------------------------------------------------*
035800970526     C* LETFIL - LETTURA RECORD
035900970526     C*------------------------------------------------------------------------*
036000970526     C     LETFIL        BEGSR
036100970526     C*
036200970526     C* LEGGE FINO A:
036300970526     C*     - FINE FILE
036400970526     C*     - TROVATO RECORD VALIDO
036500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
036600970526DO  1C     *IN99         DOWEQ     *OFF
036700970526     C     WRECOK        ANDEQ     'N'
036800970526     C*
036900970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
037000991025IF  2C     WTIPRI        IFEQ      'TIP'                                        *PER PARTENZA
037100000707     C                   READ      tivre01l                               99
037200970528E   2C                   ENDIF
037300970528     C*
037400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
037500970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
037600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
037700970530X   2C                   ELSE
037800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
037900970530E   2C                   ENDIF
038000970526E   1C                   ENDDO
038100970526     C*
038200970526     C                   ENDSR
038300970526     C*------------------------------------------------------------------------*
038400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
038500970526     C*------------------------------------------------------------------------*
038600970526     C     CHKREC        BEGSR
038700970526     C*
038800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
038900991026     C*filtra se impostato tipo file
039000991026     C     vidti1        ifeq      *blank                                       *ALLA DATA
039100991026X   2C                   ELSE
039200000703     C     vidti1        ifne      vretip                                       *ALLA DATA
039300991026     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
039400991026E   2C                   ENDIF
039500991026E   2C                   ENDIF
039600991026      *filtra se impostato codice cliente
039700991026     c     vidks1        ifeq      *zeros                                       *ALLA DATA
039800991026     C     vidks1        oreq      *blank                                       *ALLA DATA
039900991026X   2C                   ELSE
040000000703     C     vidks1        ifne      vreksc                                       *ALLA DATA
040100991026     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
040200991026E   2C                   ENDIF
040300991026E   2C                   ENDIF
040400970526     C*
040500970526     C                   ENDSR
040600970526     C*------------------------------------------------------------------------*
040700970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
040800970526     C*------------------------------------------------------------------------*
040900970526     C     IRESFL        BEGSR
041000970526     C*
041100970526     C                   MOVEL     *BLANKS       OPZKEY
041200000703     C                   MOVEL     vretip        VIDtip
041300000703     C                   MOVEL     vretfl        VIDtfl
041400000707     C                   Z-ADD     vreord        VIDord
041500000703     C                   MOVEL     vreksc        VIDksc
041600991122     C* reperimento "Ragione Sociale" del Cliente Unificante
041700991122     C                   CLEAR                   BS69DS
041800991122     C                   CLEAR                   ACODS
041900991122     C                   MOVEL     KNSIF         I69SIF
042000991122     C                   MOVE      VIDksc        I69KAC
042100991122     C                   CALL      'TIBS69R'
042200991122     C                   PARM                    BS69DS
042300991122     C                   PARM                    ACODS
042400991122     C                   PARM                    INDDS
042500991122     C                   PARM                    CLPDS
042600991122     C                   PARM                    CLSDS
042700991122     C     O69ERR        IFNE      '1'
042800991122     C                   MOVEL     ACORAG        VIDRAG
042900991122     C                   ELSE
043000991122     C                   MOVEL     *ALL'*'       VIDRAG
043100991122     C                   ENDIF
043200000703     C                   Z-ADD     vredti        G08INV
043300991025     C                   Z-ADD     *ZEROS        G08DAT
043400991025     C                   MOVEL     '3'           G08ERR
043500991025     C                   CALL      'XSRDA8'
043600991025     C                   PARM                    WLBDA8
043700991025     C                   Z-ADD     G08DAT        VIDDti
043800970526     C                   ENDSR
043900970526     C*------------------------------------------------------------------------*
044000970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
044100970526     C*------------------------------------------------------------------------*
044200970526     C     CHKPAR        BEGSR
044300970528     C*
044400970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
044500991025     C                   MOVEL     'TIP'         WTIPRI            3
044600000703IF  1C     VIDti1        IFEQ      *BLANKS
044700000703     C     VIDks1        ANDEQ     *ZEROS
044800000705IF  1C     VIDti1        OREQ      *BLANKS
044900000703     C     VIDks1        ANDEQ     *blanks
045000991025     C                   MOVEL     'TIP'         WTIPRI
045100991025E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
045200970528     C*
045300000705     C* CONTROLLA IL TIPO FILE
045400000705     c                   SETOFF                                       86
045500991012IF  1C     ERRL00        IFEQ      '0'
045600000703IF  2C     VIDti1        IFNE      *blank
045700000705     C                   MOVEL     'TIP'         WTIPRI
045800000705     c                   MOVE      vidti1        wvidti1           2
045900000705     c                   EXSR      cnttip
046000991108     C                   IF        ERRL00 ='1'
046100000705     c                   SETON                                        86
046200000705     c                   ELSE
046300000705     c                   MOVE      wvidti1       vidti1
046400000705     c                   ENDIF
046500991012E   2C                   ENDIF
046600991012E   1C                   ENDIF
046700991012      *
046800970526     C                   ENDSR
046900991108     C*------------------------------------------------------------------------*
047000991108     C* Cnttip - CONTROLLA codice tipo legame
047100991108     C*------------------------------------------------------------------------*
047200991108     c     cnttip        begsr
047300991108     c                   clear                   TIBS02DS
047400991108      *
047500000703     C     '?'           SCAN      wVidti1                                30
047600991108      *  Ricerca
047700991108     c                   eval      t02tla = 'L'
047800991108    1C                   IF        *IN30 = *ON
047900991108     c                   movel     'R'           t02mod
048000991108     c                   else
048100991108     c                   movel     'C'           t02mod
048200000703     c                   movel(p)  Wvidti1       t02ke1
048300991108     c                   end
048400991108     C                   movel     'ITF'         t02cod
048500991108     c                   movel     knsif         t02sif
048600991108     C                   CALL      'TIBS02R'
048700991108     c                   parm                    KPJBA
048800991108     c                   parm                    TIBS02DS
048900991108     c
049000991108      *
049100991108    2C                   IF        T02err = *blanks
049200000703     C                   movel     T02ke1        wVidti1
049300991108     c                   else
049400991108     c                   movel     '1'           errl00
049500991108     C                   MOVEL     msg(11)       DSMSMS
049600991108     C                   ENDIF
049700991108     c
049800991108     c                   endsr
049900970526     C*------------------------------------------------------------------------*
050000970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
050100970526     C*------------------------------------------------------------------------*
050200970526     C     CHKFOP        BEGSR
050300970626     C*
050400000703     C*
050500970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
050600970626IF  1C     DEPOP0        IFEQ      'P02'
050700970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
050800970626     C                   MOVEL     'S'           OPDATT
050900970626     C                   MOVEL     OPDRIG        OPD(2)
051000000707     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLA
051100000707     C                   MOVEL     'S'           OPDATT
051200000707     C                   MOVEL     OPDRIG        OPD(4)
051300970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
051400970626     C                   MOVEL     'S'           FUDATT
051500970626     C                   MOVEL     FUDRIG        FUD(6)
051600000705     C                   MOVEL     FUD(11)       FUDRIG                         *INSERIMENTO
051700000705     C                   MOVEL     'S'           FUDATT
051800000705     C                   MOVEL     FUDRIG        FUD(11)
051900970626E   1C                   ENDIF
052000970626     C*
052100970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
052200020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
052300970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
052400970626     C                   MOVEL     'N'           FUDATT
052500970626     C                   MOVEL     FUDRIG        FUD(6)
052600970626E   1C                   ENDIF
052700970526     C*
052800970526     C* GESTIONE OPZIONI A VIDEO
052900970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
053000970526     C                   EXSR      OPZGES
053100970526     C*
053200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
053300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
053400970526     C                   EXSR      FUNGES
053500970526     C*
053600970526     C                   ENDSR
053700991122     C*------------------------------------------------------------------------*
053800991122     C* FINSR - OPERAZIONI FINALI
053900991122     C*------------------------------------------------------------------------*
054000991122     C     FINSR         BEGSR
054100991122     C*
054200991122     C* CHIUDE I FILE DEI PGM CHIAMATI
054300991122     C                   CLEAR                   BS69DS
054400991122     C                   CLEAR                   ACODS
054500991122     C                   CLEAR                   INDDS
054600991122     C                   CLEAR                   CLPDS
054700991122     C                   CLEAR                   CLSDS
054800991122     C                   MOVEL     'C'           I69TLA
054900991122     C                   MOVEL     KNSIF         I69SIF
055000991122     C                   CALL      'TIBS69R'
055100991122     C                   PARM                    BS69DS
055200991122     C                   PARM                    ACODS
055300991122     C                   PARM                    INDDS
055400991122     C                   PARM                    CLPDS
055500991122     C                   PARM                    CLSDS
055600991122     C*
055700991122     C                   ENDSR
055800020916     C*--------------------------------------------------------------------------------------------*
055900020916     C* REPERISCE I DATI UTENTE
056000020916     C*--------------------------------------------------------------------------------------------*
056100020916     C     REPDATIUTE    BEGSR
056200020916     C*
056300020916     C* INIZIALIZZA VARIABILI DI WRK
056400020916     C                   CLEAR                   TIBS34DS
056500020916     C                   CLEAR                   AZUTEDS
056600020916     C                   CLEAR                   DDATIUTE
056700020916     C*
056800020916     C     *DTAARA       DEFINE    §azute        azuteds
056900020916     C     *DTAARA       DEFINE    §datiute      ddatiute
057000020916     C                   IN(E)     *DTAARA
057100020916if  1C                   IF        %Error
057200020916     c                   EVAL      I34Tla = 'L'
057300020916     C                   CALL      'TIBS34R'
057400020916     C                   PARM                    Tibs34Ds
057500020916     C                   IN        *DTAARA
057600020916e   1C                   ENDIF
057700020916     C*
057800020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
057900020916     C                   MOVEL(P)  RSUT          DSFIRS           20
058000020916     C*
058100020916     C                   ENDSR
058200970526     C*------------------------------------------------------------------------*
058300970526     C* *INZSR - OPERAZIONI INIZIALI
058400970526     C*------------------------------------------------------------------------*
058500970526     C     *INZSR        BEGSR
058600970526     C*---
058700970526     C* RICEVIMENTO PARAMETRI
058800970526     C*---
058900970526     C     *ENTRY        PLIST
059000000703     C                   PARM                    KPJBA
059100000705     C                   PARM                    VIDRAG
059200000710     C                   MOVEL     kpjbu         tis720
059300970526     C*
059400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
059500970526     C     XMSGPA        PLIST
059600970526     C                   PARM                    DSMSMS
059700970526     C                   PARM                    DSMSPG
059800970526     C                   PARM                    DSMSKE
059900970526     C                   PARM                    DSMSER
060000970526     C*
060100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
060200970526     C                   CALL      'XKEY'
060300970526     C                   PARM                    DSKEY
060400970526     C*---
060500970526     C* VARIABILI NON RIFERITE AL DATA BASE
060600970526     C*---
060700970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
060800970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
060900970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
061000970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
061100970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
061200970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
061300970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
061400970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
061500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
061600991025     C     *LIKE         DEFINE    s20OP0        DEPOP0                         *DI DEPOSITO
061700991025     C     *LIKE         DEFINE    s20OP1        DEPOP1
061800991025     C     *LIKE         DEFINE    s20OP0        WOP0                           *DI LAVORO
061900991025     C     *LIKE         DEFINE    s20OP1        WOP1
062000991025     C     *LIKE         DEFINE    s20tip        Dtip
062100000705     C     *LIKE         DEFINE    s20tip        Dtfl
062200000420     C     *LIKE         DEFINE    s20dti        Ddti
062300970526     C*---
062400991025     C     *LIKE         DEFINE    s20tip        Deptip
062500991025     C     *LIKE         DEFINE    s20tip        Depti1
062600991025     C     *LIKE         DEFINE    vidks1        Depks1
062700000703     C     *LIKE         DEFINE    vreksc        kceksc
062800000703     C     *LIKE         DEFINE    vredti        kcedde
062900000705     C     *LIKE         DEFINE    vretip        kcetip
063000000705     C     *LIKE         DEFINE    vretfl        kcetfl
063100970526     C* VARIABILI DI CONTROLLO
063200970526     C*---
063300970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
063400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
063500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
063600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
063700970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
063800970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
063900970526     C*
064000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
064100000703     C                   MOVEL     'TIS727R'     DSMSPG
064200000703     C                   MOVEL     'TIS727R'     MSGQUE
064300970526     C                   MOVEL     *BLANKS       MSGKEY
064400970526     C                   MOVEL     *BLANKS       DSMSMS
064500970526     C                   MOVEL     *BLANKS       DSMSKE
064600970526     C                   Z-ADD     *ZEROS        DSMSER
064700970526     C*---
064800970526     C* VARIABILI RIFERITE AL DATA BASE
064900970526     C*---
065000970526     C*---
065100970526     C* CHIAVI DI LETTURA
065200970526     C*---
065300000707     C* LETTURA tivre01L - INCOMPLETA
065400000703     C     KEYvre        KLIST
065500000703     C                   KFLD                    KCEksc
065600000703     C                   KFLD                    KCEtip
065700000703     C                   KFLD                    KCEdde
065800000705     C*
065900000705     C     KE2vre        KLIST
066000000705     C                   KFLD                    KCEksc
066100000705     C                   KFLD                    KCEtip
066200000705     C                   KFLD                    KCEdde
066300000705     C                   KFLD                    VIDtfl
066400970527     C*
066500970527     C* REPERIMENTO DATI UTENTE
066600020916     C                   EXSR      REPDATIUTE
066700970526     C*
066800970526     C* DEPOSITA LE OPZIONI DI ENTRATA
066900000703     C                   MOVEL     s20OP0        DEPOP0                         *LIVELLO PROCEDURA
067000000703     C                   MOVEL     s20OP1        DEPOP1                         *LIVELLO PROGRAMMA
067100970526     C*
067200000705     C* GIRA LE DATE PRESE IN INPUT
067300000705     C                   Z-ADD     s20DTI        G08DAT
067400970526     C                   Z-ADD     *ZEROS        G08INV
067500970526     C                   MOVEL     '0'           G08ERR
067600970526     C                   CALL      'XSRDA8'
067700970526     C                   PARM                    WLBDA8
067800000705     C                   Z-ADD     G08INV        Ddti                           *Formato AAAA/MM/GG
067900970526     C*
068000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
068100000703     C                   MOVE      s20KSC        VIDks1
068200000705     C                   MOVE      s20KSC        VIDksc
068300000703     C                   MOVE      s20TIP        VIDti1
068400000705     C                   MOVE      s20TIP        VIDtip
068500000703     C                   MOVE      s20TIP        depti1
068600000703     C                   Z-ADD     s20DTI        VIDdt1
068700970526     C                   EXSR      CHKPAR
068800970526IF  1C     ERRL00        IFEQ      '1'
068900991025     C                   MOVEL     '1'           s20ERR
069000991025     C                   MOVEL     DSMSMS        s20MSG
069100970526X   1C                   ELSE
069200991025     C                   MOVEL     VIDtip        DEPtip                         *DEPOSITI
069300970526E   1C                   ENDIF
069400970526     C*
069500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
069600970526     C                   EXSR      CHKFOP
069700970526     C*
069800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
069900970530IF  1C     DEPOP0        IFEQ      'P01'
070000970530     C                   MOVEL     DEC(3)        VIDING
070100970530E   1C                   ENDIF
070200970530IF  1C     DEPOP0        IFEQ      'P02'
070300970526     C                   MOVEL     DEC(1)        VIDING
070400970530E   1C                   ENDIF
070500970530IF  1C     DEPOP0        IFEQ      'P05'
070600970526     C                   MOVEL     DEC(2)        VIDING
070700970526E   1C                   ENDIF
070800970526     C*
070900970526     C                   ENDSR
071000970526     C*------------------------------------------------------------------------*
071100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
071400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
071500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
071600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071800970526     C*------------------------------------------------------------------------*
071900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
072000970526     C*------------------------------------------------------------------------*
072100970526     C     POSSFL        BEGSR
072200970526     C*
072300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
072400970526     C                   EXSR      CLRSFL
072500970526     C*
072600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
072700970526     C                   EXSR      SETFIL
072800970526     C*
072900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
073000970526     C     SFLEOF        IFEQ      0
073100970526     C                   EXSR      CARSFL
073200970526     C                   END
073300970526     C*
073400970526     C                   ENDSR
073500970526     C*------------------------------------------------------------------------*
073600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
073700970526     C*------------------------------------------------------------------------*
073800970526     C     ENTATT        BEGSR
073900970526     C*
074000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
074100970526     C                   Z-ADD     0             OPZOK             1 0
074200970526     C*
074300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
074400970526     C                   Z-ADD     0             SFLVAR            5 0
074500970526     C*
074600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
074700970526     C                   Z-ADD     0             SFLERR
074800970526     C*
074900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
075000970526     C                   Z-ADD     SFLULT        WFLULT
075100970526     C*
075200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
075300000703     C                   READC     S727D05                                98
075400970526     C     *IN98         DOWEQ     '0'
075500970526     C*
075600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
075700970526     C                   ADD       1             SFLVAR
075800970526     C*
075900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
076000970526     C     OPZOK         IFEQ      0
076100970526     C*
076200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
076300970526     C     OPZKEY        IFNE      *BLANKS
076400970526     C                   EXSR      OPZCON
076500970526     C                   END
076600970526     C*
076700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
076800970526     C*            2. PULIZIA SCELTA IN SFL
076900970526     C     OPZOK         IFEQ      0
077000970526     C                   MOVEL     '0'           *IN41
077100970526     C                   MOVEL     *BLANKS       OPZKEY
077200970526     C                   ELSE
077300970526     C*
077400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
077500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
077600970526     C                   MOVEL     '1'           *IN41
077700970526     C                   Z-ADD     SFLULT        SFLERR
077800970526     C                   END
077900970526     C                   ELSE
078000970526     C*
078100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
078200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
078300970526     C                   MOVEL     '1'           *IN41
078400970526     C                   END                                                    OPZOK=0
078500970526     C*
078600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
078700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
078800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
078900970526     C     OPZOK         IFNE      0
079000970526     C                   Z-ADD     SFLERR        SFLNBR
079100970526     C                   ELSE
079200970526     C                   Z-ADD     SFLULT        SFLNBR
079300970526     C                   END
079400970526     C*
079500970526     C* AGGIORNO RECORD SFL
079600000703     C                   UPDATE    S727D05
079700970526     C*
079800000703     C                   READC     S727D05                                98
079900970526     C                   END                                                    DO WHILE
080000970526     C*
080100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
080200970526     C                   Z-ADD     WFLULT        SFLULT
080300970526     C*
080400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
080500970526     C     SFLVAR        IFEQ      0
080600970526     C*---                 MOVEL'1'       FINL00
080700970526     C                   END
080800970526     C*
080900970526     C                   ENDSR
081000970526     C*------------------------------------------------------------------------*
081100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
081200970526     C*------------------------------------------------------------------------*
081300970526     C     CLRSFL        BEGSR
081400970526     C*
081500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
081600970526     C* (0=NON FINE FILE  1=FINE FILE)
081700970526     C                   Z-ADD     0             SFLEOF
081800970526     C*
081900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
082000970526     C                   Z-ADD     0             SFLULT
082100970526     C*
082200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
082300970526     C                   MOVEL     '1'           *IN40
082400000703     C                   WRITE     S727D06
082500970526     C                   MOVEL     '0'           *IN40
082600970526     C*
082700970526     C                   ENDSR
082800970526     C*------------------------------------------------------------------------*
082900970526     C* CARSFL - CARICAMENTO SFL
083000970526     C*------------------------------------------------------------------------*
083100970526     C     CARSFL        BEGSR
083200970526     C*
083300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
083400970526     C     SFLULT        IFEQ      0
083500970526     C                   EXSR      SETFIL
083600970526     C                   END
083700970526     C*
083800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
083900970526     C                   Z-ADD     SFLULT        WFLULT
084000970526     C*
084100970526     C* CICLO:
084200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
084300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
084400970526     C                   Z-ADD     1             SFLRIG
084500970526     C     SFLRIG        DOWLE     SFLPAG
084600970526     C     SFLEOF        ANDEQ     0
084700970526     C*
084800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
084900970526     C                   MOVEL     *BLANKS       OPZKEY
085000970526     C*
085100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
085200970526     C                   EXSR      IRESFL
085300970526     C*
085400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
085500970526     C                   ADD       1             SFLULT
085600970526     C*
085700970526     C* SCRITTURA RECORD SFL
085800000703     C                   WRITE     S727D05
085900970526     C                   ADD       1             SFLRIG
086000970526     C*
086100970526     C* LETTURA FILE DA CARICARE
086200970526     C                   EXSR      LETFIL
086300970526     C                   END                                                    DO WHILE
086400970526     C*
086500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
086600970526     C     WFLULT        IFNE      SFLULT
086700970526     C                   Z-ADD     WFLULT        SFLNBR
086800970526     C                   ADD       1             SFLNBR
086900970526     C                   END
087000970526     C*
087100970526     C                   ENDSR
087200970526     C*------------------------------------------------------------------------*
087300970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
087400970526     C*------------------------------------------------------------------------*
087500970526     C     OUTSFL        BEGSR
087600970526     C*
087700970526     C* WRITE RECORD TESTATA VIDEO
087800000703     C                   WRITE     S727D01
087900970526     C*
088000970526     C* VISUALIZZAZIONE MESSAGGIO
088100970526     C                   EXSR      SNDMSG
088200970526     C*
088300970526     C* WRITE RECORD OPZIONI VIDEO
088400000703     C                   WRITE     S727D02
088500970526     C*
088600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
088700000703     C                   WRITE     S727D03
088800970526     C*
088900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
089000000703     C                   WRITE     S727D04
089100970526     C*
089200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
089300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
089400970526     C     SFLEOF        IFEQ      0
089500970526     C                   MOVEL     '0'           *IN42
089600970526     C                   ELSE
089700970526     C*
089800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
089900970526     C                   MOVEL     '1'           *IN42
090000970526     C                   END
090100970526     C*
090200970526     C* WRITE RECORD TASTI FUNZIONALI
090300000703     C                   WRITE     S727D09
090400970526     C*
090500970526     C* CASO DI SFL VUOTO
090600970526     C     SFLULT        IFLE      0
090700970526     C*
090800970526     C*    - WRITE RECORD VUOTO
090900000703     C                   WRITE     S727D0A
091000000703     C                   READ      S727D0A                                99
091100970526     C*
091200970526     C                   ELSE
091300970526     C*
091400970526     C* CASO DI SFL CON RECORDS
091500970526     C*
091600970526     C*    - WRITE RECORD CONTROLLO SFL
091700000703     C                   WRITE     S727D06
091800000703     C                   READ      S727D06                                99
091900970526     C                   END
092000970526     C*
092100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
092200000703     C                   READ      S727D03                                99
092300970526     C*
092400970526     C                   ENDSR
092500970526     C*------------------------------------------------------------------------*
092600970526     C* SNDMSG - MANDA MESSAGGIO
092700970526     C*------------------------------------------------------------------------*
092800970526     C     SNDMSG        BEGSR
092900970526     C*
093000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
093100970526     C                   MOVEL     '1'           *IN40
093200000703     C                   WRITE     S727DMC
093300970526     C                   MOVEL     '0'           *IN40
093400970526     C*
093500970526     C* VISUALIZZAZIONE MESSAGGIO
093600970526     C                   Z-ADD     0             MSGULT
093700970526     C                   CALL      'XMSG'        XMSGPA
093800970526     C     DSMSER        IFEQ      0
093900970526     C                   MOVEL     DSMSKE        MSGKEY
094000970526     C                   END
094100970526     C                   Z-ADD     1             MSGULT
094200000703     C                   WRITE     S727DMS                                      SFL MESS
094300000703     C                   WRITE     S727DMC                                      CONTR MESS
094400970526     C*
094500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
094600970526     C                   MOVEL     *BLANKS       DSMSMS
094700970526     C*
094800970526     C                   ENDSR
094900970526     C*------------------------------------------------------------------------*
095000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
095100970526     C*------------------------------------------------------------------------*
095200970526     C     FUNCON        BEGSR
095300970526     C*
095400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
095500970526     C                   EXSR      FUNVAR
095600970526     C*
095700970526     C* PULISCO CAMPI DI LAVORO
095800970526     C                   Z-ADD     0             I                 5 0
095900970526     C                   Z-ADD     0             W                 5 0
096000970526     C*
096100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
096200970526     C     FUNKEY        IFNE      *BLANKS
096300970526     C     1             DO        24            I
096400970526     C                   MOVEL     FUD(I)        FUDRIG
096500970526     C*
096600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
096700970526     C     FUNKEY        IFEQ      FUDFUN
096800970526     C     FUDATT        ANDEQ     'S'
096900970526     C                   Z-ADD     I             W
097000970526     C                   END
097100970526     C                   END                                                    DO WHILE
097200970526     C*
097300970526     C* FUNZIONE NON ATTIVA (ERRORE)
097400970526     C     W             IFEQ      0
097500970526     C*
097600970526     C* EMISSIONE MESSAGGIO DI ERRORE
097700970526     C                   MOVEL     MSG(2)        DSMSMS
097800970526     C                   END                                                    W=0
097900970526     C                   END                                                    FUNKEY NE *BLANKS
098000970526     C*
098100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
098200970526     C     W             IFNE      0
098300970526     C     FUNKEY        OREQ      *BLANKS
098400970526     C*
098500970526     C     FUNKEY        IFEQ      *BLANKS
098600970526     C                   EXSR      ENTGES
098700970526     C                   END
098800970526     C     FUNKEY        IFEQ      '03'
098900970526     C                   EXSR      F03GES
099000970526     C                   END
099100970526     C     FUNKEY        IFEQ      '05'
099200970526     C                   EXSR      F05GES
099300970526     C                   END
099400970526     C     FUNKEY        IFEQ      '06'
099500970526     C                   EXSR      F06GES
099600970526     C                   END
099700970526     C     FUNKEY        IFEQ      '12'
099800970526     C                   EXSR      F12GES
099900970526     C                   END
100000970526     C     FUNKEY        IFEQ      '23'
100100970526     C                   EXSR      F23GES
100200970526     C                   END
100300970526     C     FUNKEY        IFEQ      '24'
100400970526     C                   EXSR      F24GES
100500970526     C                   END
100600970526     C     FUNKEY        IFEQ      'RU'
100700970526     C                   EXSR      RUPGES
100800970526     C                   END
100900970526     C*
101000970526     C                   END
101100970526     C*
101200970526     C                   ENDSR
101300970526     C*------------------------------------------------------------------------*
101400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
101500970526     C*------------------------------------------------------------------------*
101600970526     C     FUNVAR        BEGSR
101700970526     C*
101800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
101900970526     C     KEY           IFEQ      ENTER
102000970526     C                   MOVEL     *BLANKS       FUNKEY
102100970526     C                   END
102200970526     C     KEY           IFEQ      F1
102300970526     C                   MOVEL     '01'          FUNKEY
102400970526     C                   END
102500970526     C     KEY           IFEQ      F2
102600970526     C                   MOVEL     '02'          FUNKEY
102700970526     C                   END
102800970526     C     KEY           IFEQ      F3
102900970526     C                   MOVEL     '03'          FUNKEY
103000970526     C                   END
103100970526     C     KEY           IFEQ      F4
103200970526     C                   MOVEL     '04'          FUNKEY
103300970526     C                   END
103400970526     C     KEY           IFEQ      F5
103500970526     C                   MOVEL     '05'          FUNKEY
103600970526     C                   END
103700970526     C     KEY           IFEQ      F6
103800970526     C                   MOVEL     '06'          FUNKEY
103900970526     C                   END
104000970526     C     KEY           IFEQ      F7
104100970526     C                   MOVEL     '07'          FUNKEY
104200970526     C                   END
104300970526     C     KEY           IFEQ      F8
104400970526     C                   MOVEL     '08'          FUNKEY
104500970526     C                   END
104600970526     C     KEY           IFEQ      F9
104700970526     C                   MOVEL     '09'          FUNKEY
104800970526     C                   END
104900970526     C     KEY           IFEQ      F10
105000970526     C                   MOVEL     '10'          FUNKEY
105100970526     C                   END
105200970526     C     KEY           IFEQ      F11
105300970526     C                   MOVEL     '11'          FUNKEY
105400970526     C                   END
105500970526     C     KEY           IFEQ      F12
105600970526     C                   MOVEL     '12'          FUNKEY
105700970526     C                   END
105800970526     C     KEY           IFEQ      F13
105900970526     C                   MOVEL     '13'          FUNKEY
106000970526     C                   END
106100970526     C     KEY           IFEQ      F14
106200970526     C                   MOVEL     '14'          FUNKEY
106300970526     C                   END
106400970526     C     KEY           IFEQ      F15
106500970526     C                   MOVEL     '15'          FUNKEY
106600970526     C                   END
106700970526     C     KEY           IFEQ      F16
106800970526     C                   MOVEL     '16'          FUNKEY
106900970526     C                   END
107000970526     C     KEY           IFEQ      F17
107100970526     C                   MOVEL     '17'          FUNKEY
107200970526     C                   END
107300970526     C     KEY           IFEQ      F18
107400970526     C                   MOVEL     '18'          FUNKEY
107500970526     C                   END
107600970526     C     KEY           IFEQ      F19
107700970526     C                   MOVEL     '19'          FUNKEY
107800970526     C                   END
107900970526     C     KEY           IFEQ      F20
108000970526     C                   MOVEL     '20'          FUNKEY
108100970526     C                   END
108200970526     C     KEY           IFEQ      F21
108300970526     C                   MOVEL     '21'          FUNKEY
108400970526     C                   END
108500970526     C     KEY           IFEQ      F22
108600970526     C                   MOVEL     '22'          FUNKEY
108700970526     C                   END
108800970526     C     KEY           IFEQ      F23
108900970526     C                   MOVEL     '23'          FUNKEY
109000970526     C                   END
109100970526     C     KEY           IFEQ      F24
109200970526     C                   MOVEL     '24'          FUNKEY
109300970526     C                   END
109400970526     C     KEY           IFEQ      ROLLUP
109500970526     C                   MOVEL     'RU'          FUNKEY
109600970526     C                   END
109700970526     C     KEY           IFEQ      HELP
109800970526     C                   MOVEL     'HP'          FUNKEY
109900970526     C                   END
110000970526     C*
110100970526     C                   ENDSR
110200970526     C*------------------------------------------------------------------------*
110300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
110400970526     C*------------------------------------------------------------------------*
110500970526     C     FUNGES        BEGSR
110600970526     C*
110700970526     C* PULISCO CAMPI DI LAVORO
110800970526     C                   Z-ADD     0             W                 5 0
110900970526     C*
111000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
111100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
111200970526     C     1             DO        24            I
111300970526     C                   MOVEL     FUD(I)        FUDRIG
111400970526     C*
111500970526     C* PULIZIA FUNZIONI PRECEDENTI
111600970526     C                   MOVEL     *BLANKS       FUV(I)
111700970526     C*
111800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
111900970526     C* (SALTA F23 E F24)
112000970526     C     FUDATT        IFEQ      'S'
112100970526     C     FUDFUN        ANDNE     '23'
112200970526     C     FUDFUN        ANDNE     '24'
112300970526     C                   ADD       1             W
112400970526     C                   MOVEL     FUDDES        FUV(W)
112500970526     C                   END
112600970526     C                   END                                                    DO WHILE
112700970526     C*
112800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
112900970526     C* ALTRIMENTI DISATTIVO F24
113000970526     C     1             DO        24            I
113100970526     C                   MOVEL     FUD(I)        FUDRIG
113200970526     C*
113300970526     C     FUDFUN        IFEQ      '24'
113400970526     C     W             IFGT      6
113500970526     C                   MOVEL     'S'           FUDATT
113600970526     C                   ELSE
113700970526     C                   MOVEL     *BLANKS       FUDATT
113800970526     C                   END                                                    W GT 8
113900970526     C                   MOVEL     FUDRIG        FUD(I)
114000970526     C                   END
114100970526     C                   END                                                    DO WHILE
114200970526     C*
114300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
114400970526     C     W             DIV       6             I
114500970526     C                   MVR                     Z
114600970526     C*
114700970526     C     Z             IFNE      0
114800970526     C     I             ADD       1             Z
114900970526     C                   ELSE
115000970526     C                   Z-ADD     I             Z
115100970526     C                   END
115200970526     C*
115300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
115400970526     C                   ADD       1             FUNGRU
115500970526     C     FUNGRU        IFGT      Z
115600970526     C     FUNGRU        ORLT      1
115700970526     C                   Z-ADD     1             FUNGRU
115800970526     C                   END
115900970526     C*
116000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
116100970526     C     FUNGRU        IFEQ      1
116200970526     C                   MOVEL     FUV(1)        FUN001
116300970526     C                   MOVEL     FUV(2)        FUN002
116400970526     C                   MOVEL     FUV(3)        FUN003
116500970526     C                   MOVEL     FUV(4)        FUN004
116600970526     C                   MOVEL     FUV(5)        FUN005
116700970526     C                   MOVEL     FUV(6)        FUN006
116800970526     C                   END
116900970526     C*
117000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
117100970526     C     FUNGRU        IFEQ      2
117200970526     C                   MOVEL     FUV(7)        FUN001
117300970526     C                   MOVEL     FUV(8)        FUN002
117400970526     C                   MOVEL     FUV(9)        FUN003
117500970526     C                   MOVEL     FUV(10)       FUN004
117600970526     C                   MOVEL     FUV(11)       FUN005
117700970526     C                   MOVEL     FUV(12)       FUN006
117800970526     C                   END
117900970526     C*
118000970526     C* TERZO GRUPPO DI 6 FUNZIONI
118100970526     C     FUNGRU        IFEQ      3
118200970526     C                   MOVEL     FUV(13)       FUN001
118300970526     C                   MOVEL     FUV(14)       FUN002
118400970526     C                   MOVEL     FUV(15)       FUN003
118500970526     C                   MOVEL     FUV(16)       FUN004
118600970526     C                   MOVEL     FUV(17)       FUN005
118700970526     C                   MOVEL     FUV(18)       FUN006
118800970526     C                   END
118900970526     C*
119000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
119100970526     C     FUNGRU        IFEQ      4
119200970526     C                   MOVEL     FUV(19)       FUN001
119300970526     C                   MOVEL     FUV(20)       FUN002
119400970526     C                   MOVEL     FUV(21)       FUN003
119500970526     C                   MOVEL     FUV(22)       FUN004
119600970526     C                   MOVEL     *BLANKS       FUN005
119700970526     C                   MOVEL     *BLANKS       FUN006
119800970526     C                   END
119900970526     C*
120000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
120100970526     C                   MOVEL     *BLANKS       FUN007
120200970526     C                   MOVEL     *BLANKS       FUN008
120300970526     C*
120400970526     C     1             DO        24            I
120500970526     C                   MOVEL     FUD(I)        FUDRIG
120600970526     C     FUDFUN        IFEQ      '23'
120700970526     C     FUDATT        ANDEQ     'S'
120800970526     C                   MOVEL     FUDDES        FUN007
120900970526     C                   END
121000970526     C     FUDFUN        IFEQ      '24'
121100970526     C     FUDATT        ANDEQ     'S'
121200970526     C                   MOVEL     FUDDES        FUN008
121300970526     C                   END
121400970526     C                   END                                                    DO
121500970526     C*
121600970526     C                   ENDSR
121700970526     C*------------------------------------------------------------------------*
121800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
121900970526     C*------------------------------------------------------------------------*
122000970526     C     OPZGES        BEGSR
122100970526     C*
122200970526     C* PULISCO CAMPI DI LAVORO
122300970526     C                   Z-ADD     0             W                 5 0
122400970526     C*
122500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
122600970526     C* E CARICO TABELLA OPZIONI ATTIVE
122700970526     C     1             DO        24            I
122800970526     C                   MOVEL     OPD(I)        OPDRIG
122900970526     C*
123000970526     C* PULIZIA OPZIONI PRECEDENTI
123100970526     C                   MOVEL     *BLANKS       OPV(I)
123200970526     C*
123300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
123400970526     C     OPDATT        IFEQ      'S'
123500970526     C                   ADD       1             W
123600970526     C                   MOVEL     OPDDES        OPV(W)
123700970526     C                   END
123800970526     C                   END                                                    DO WHILE
123900970526     C*
124000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
124100970526     C* ALTRIMENTI DISATTIVO F23
124200970526     C     1             DO        24            I
124300970526     C                   MOVEL     FUD(I)        FUDRIG
124400970526     C*
124500970526     C     FUDFUN        IFEQ      '23'
124600970526     C     W             IFGT      8
124700970526     C                   MOVEL     'S'           FUDATT
124800970526     C                   ELSE
124900970526     C                   MOVEL     *BLANKS       FUDATT
125000970526     C                   END                                                    W GT 8
125100970526     C                   MOVEL     FUDRIG        FUD(I)
125200970526     C                   END
125300970526     C                   END                                                    DO WHILE
125400970526     C*
125500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
125600970526     C     W             DIV       8             I
125700970526     C                   MVR                     Z                 5 0
125800970526     C*
125900970526     C     Z             IFNE      0
126000970526     C     I             ADD       1             Z
126100970526     C                   ELSE
126200970526     C                   Z-ADD     I             Z
126300970526     C                   END
126400970526     C*
126500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
126600970526     C                   ADD       1             OPZGRU
126700970526     C     OPZGRU        IFGT      Z
126800970526     C     OPZGRU        ORLT      1
126900970526     C                   Z-ADD     1             OPZGRU
127000970526     C                   END
127100970526     C*
127200970526     C* PRIMO GRUPPO DI 8 OPZIONI
127300970526     C     OPZGRU        IFEQ      1
127400970526     C                   MOVEL     OPV(1)        OPZ001
127500970526     C                   MOVEL     OPV(2)        OPZ002
127600970526     C                   MOVEL     OPV(3)        OPZ003
127700970526     C                   MOVEL     OPV(4)        OPZ004
127800970526     C                   MOVEL     OPV(5)        OPZ005
127900970526     C                   MOVEL     OPV(6)        OPZ006
128000970526     C                   MOVEL     OPV(7)        OPZ007
128100970526     C                   MOVEL     OPV(8)        OPZ008
128200970526     C                   END
128300970526     C*
128400970526     C* SECONDO GRUPPO DI 8 OPZIONI
128500970526     C     OPZGRU        IFEQ      2
128600970526     C                   MOVEL     OPV(9)        OPZ001
128700970526     C                   MOVEL     OPV(10)       OPZ002
128800970526     C                   MOVEL     OPV(11)       OPZ003
128900970526     C                   MOVEL     OPV(12)       OPZ004
129000970526     C                   MOVEL     OPV(13)       OPZ005
129100970526     C                   MOVEL     OPV(14)       OPZ006
129200970526     C                   MOVEL     OPV(15)       OPZ007
129300970526     C                   MOVEL     OPV(16)       OPZ008
129400970526     C                   END
129500970526     C*
129600970526     C* TERZO GRUPPO DI 8 OPZIONI
129700970526     C     OPZGRU        IFEQ      3
129800970526     C                   MOVEL     OPV(17)       OPZ001
129900970526     C                   MOVEL     OPV(18)       OPZ002
130000970526     C                   MOVEL     OPV(19)       OPZ003
130100970526     C                   MOVEL     OPV(20)       OPZ004
130200970526     C                   MOVEL     OPV(21)       OPZ005
130300970526     C                   MOVEL     OPV(22)       OPZ006
130400970526     C                   MOVEL     OPV(23)       OPZ007
130500970526     C                   MOVEL     OPV(24)       OPZ008
130600970526     C                   END
130700970526     C*
130800970526     C                   ENDSR
130900970526     C*------------------------------------------------------------------------*
131000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
131100970526     C*------------------------------------------------------------------------*
131200970526     C     OPZCON        BEGSR
131300970526     C*
131400970526     C* PULISCO CAMPI DI LAVORO
131500970526     C                   Z-ADD     0             I                 5 0
131600970526     C                   Z-ADD     0             W                 5 0
131700970526     C*
131800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
131900970526     C     1             DO        24            I
132000970526     C                   MOVEL     OPD(I)        OPDRIG
132100970526     C*
132200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
132300970526     C     OPZKEY        IFEQ      OPDOPZ
132400970526     C     OPZKEY        OREQ      OPDOP1
132500970526     C*
132600970526     C     OPDATT        IFEQ      'S'
132700970526     C                   MOVEL     OPDOPZ        OPZKEY
132800970526     C                   Z-ADD     I             W
132900970526     C                   END
133000970526     C                   END
133100970526     C*
133200970526     C                   END                                                    DO WHILE
133300970526     C*
133400970526     C* OPZIONE NON ATTIVA (ERRORE)
133500970526     C     W             IFEQ      0
133600970526     C*
133700970526     C* OPZIONE NON OK
133800970526     C                   Z-ADD     1             OPZOK
133900970526     C*
134000970526     C* EMISSIONE MESSAGGIO DI ERRORE
134100970526     C                   MOVEL     MSG(1)        DSMSMS
134200970526     C                   ELSE
134300970526     C*
134400970526     C* OPZIONE ATTIVA (PROCEDI ...)
134500970526     C*
134600970526     C     OPZKEY        IFEQ      ' 2'
134700970526     C                   EXSR      O02GES
134800970526     C                   END
134900000707     C     OPZKEY        IFEQ      ' 4'
135000000707     C                   EXSR      O04GES
135100000707     C                   END
135200970526     C                   END                                                    OPZ.NON ATTIVA
135300970526     C*
135400970526     C                   ENDSR
135500970526     C*------------------------------------------------------------------------*
135600970526     C* NOTE COSTRUZIONE SCHIERE
135700970526     C*------------------------------------------------------------------------*
135800970526     C*
135900970526     C* -MSG-MESSAGGI
136000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
136100970526     C* (NON TOCCARE MSG,1 E MSG,2)
136200970526     C*
136300970526     C* -CMD-COMANDI
136400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
136500970526     C*
136600970526     C* -OPD-OPZIONI
136700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
136800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
136900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
137000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
137100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
137200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
137300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
137400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
137500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
137600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
137700970526     C*
137800970526     C* -FUD-FUNZIONI
137900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
138000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
138100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
138200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
138300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
138400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
138500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
138600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
138700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
138800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
138900970526     O*------------------------------------------------------------------------*
139000970526** DEC - DECODIFICHE
139100970526    GESTIONE                                                     1
139200970526INTERROGAZIONE                                                   2
139300970530     SCELTA                                                      3
1394009705264
1395009705265
139600970526** MSG - MESSAGGI
139700000703TIS727R- Opzione non gestita                                                   1
139800000703TIS727R- Tasto funzionale non gestito                                          2
139900000703TIS727R- Non abilitato alla gestione di questo CAP                             3
140000000703TIS727R- Record annullato. Consentito solo il ripristino                       4
140100000703TIS727R- Record non annullato. Impossibile ripristinare                        5
140200000703TIS727R- Accettato solo un parametro di inizio lista                           6
140300000705TIS727R- Impossibile effettuare inserimento, record già esistente              7
140400000705TIS727R- Impossibile effettuare modifica, record inesistente                   8
140500000703TIS727R- 9                                                                     9
140600000703TIS727R- 10                                                                   10
140700000703TIS727R- Codice tipo file inesistente in tabella
140800000703TIS727R- Data errata                                                          12
140900000703TIS727R- 13                                                                   13
141000000703TIS727R- 14                                                                   14
141100000703TIS727R- 15                                                                   15
141200000703TIS727R- 16                                                                   16
141300000703TIS727R- 17                                                                   17
141400000703TIS727R- 18                                                                   18
141500000703TIS727R- 19                                                                   19
141600000703TIS727R- 20                                                                   20
141700970528** CMD - COMANDI CL
141800970528WRKSPLF
141900970528
142000970528
142100970528
142200970528
142300970526** OPD - OPZIONI
142400000703                   -1 -     (S/N)=N                        opz. 01¬
142500970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
142600991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
142700000707 4=Elimina         -4 -     (S/N)=N                        opz. 04¬
142800000703                   -5 -     (S/N)=N                        opz. 05¬
142900000703                   -6 -     (S/N)=N                        opz. 06¬
143000970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
143100000703                   -  -     (S/N)=N                        opz. 08¬
143200000703                   -  -     (S/N)=N                        opz. 09¬
143300000703                   -  -     (S/N)=N                        opz. 10¬
143400000703                   -  -     (S/N)=N                        opz. 11¬
143500970526                   -  -     (S/N)=N                        opz. 12¬
143600970526                   -  -     (S/N)=N                        opz. 13¬
143700970526                   -  -     (S/N)=N                        opz. 14¬
143800970526                   -  -     (S/N)=N                        opz. 15¬
143900970526                   -  -     (S/N)=N                        opz. 16¬
144000970526                   -  -     (S/N)=N                        opz. 17¬
144100970526                   -  -     (S/N)=N                        opz. 18¬
144200970526                   -  -     (S/N)=N                        opz. 19¬
144300970526                   -  -     (S/N)=N                        opz. 20¬
144400970526                   -  -     (S/N)=N                        opz. 21¬
144500970526                   -  -     (S/N)=N                        opz. 22¬
144600970526                   -  -     (S/N)=N                        opz. 23¬
144700970526                   -  -     (S/N)=N                        opz. 24¬
144800970526** FUD - FUNZIONI
144900970526                   -        (S/N)=N                        funz.01¬
145000970526                   -        (S/N)=N                        funz.02¬
145100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
145200970526                   -        (S/N)=N                        funz.04¬
145300970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
145400000703F06=Inserimento    -        (S/N)=N                        funz.06¬
145500970526                   -        (S/N)=N                        funz.07¬
145600970527                   -        (S/N)=N                        funz.08¬
145700970527                   -        (S/N)=N                        funz.09¬
145800970527                   -        (S/N)=N                        funz.10¬
145900970526                   -        (S/N)=N                        funz.11¬
146000970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
146100970526                   -        (S/N)=N                        funz.13¬
146200970526                   -        (S/N)=N                        funz.14¬
146300970526                   -        (S/N)=N                        funz.15¬
146400970526                   -        (S/N)=N                        funz.16¬
146500970526                   -        (S/N)=N                        funz.17¬
146600970526                   -        (S/N)=N                        funz.18¬
146700970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
146800970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
146900980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
147000970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
147100970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
147200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
