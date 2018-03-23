000100020319     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020319      *------------------------------------------------------------------------*
000300020319      * GESTIONE UNITA' EDP - ROUTINE
000400020319      *------------------------------------------------------------------------*
000500020319
000600020319      *------------------------------------------------------------------------*
000700020319      * DATA BASE
000800020319      *------------------------------------------------------------------------*
000900941115     FTNSDU2D   CF   E             WORKSTN INFDS(DEVDS1)
001000941115     F                                     SFILE(SDU2DMS:MSGULT)
001100971013     FUNMOV01L  UF A E           K DISK
001200160923     FUNMOV00S  o    E             DISK    rename(unmov000:unmov0s)
001300160923     FUNana00S  o    E             DISK    rename(unana000:unana0s)
001400930709     FUNANA01L  UF A E           K DISK
001500020320     funana08l  if   e           k disk    rename(unana000:unana8)
001600091113     funana02l  if   e           k disk    rename(unana000:unana2)
001700020529     FAZORG21l  IF   E           K DISK
001800020529     fazorg01l  if   e           k disk
001900020319
002000020319      *------------------------------------------------------------------------*
002100020319      * SCHIERE
002200020319      *------------------------------------------------------------------------*
002300020319      * MESSAGGI
002400160923     D MSG             S             78    DIM(29) CTDATA PERRCD(1)
002500020319      * COMANDI
002600000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002700020319      * DESCRIZIONI TASTI FUNZIONALI
002800000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002900020319      * TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003000000000     D FUV             S             19    DIM(24)
003100020319
003200020322     d wvdtip          s                   like(unatip)
003300020320     d kunmod          s                   like(unamod)
003400020320     d kunmat          s                   like(unamat)
003500091113     d kuntip          s                   like(unatip)
003600020403     d savmod          s                   like(unamod)
003700020403     d savmat          s                   like(unamat)
003800020321     d paxccc          s                   LIKE(ACOkcc)
003900020321     d paxdmt          s                   LIKE(ACOrag)
004000020405     d sav_dsunit      s                   like(dsunit)
004100020321     d PAXdut          s             30
004200020321     d PAXdit          s              3
004300020321     d PAXflr          s             90
004400020321     d PAXsta          s              1  0
004500020709     d codut           s              1  0 Inz(1)
004600021104     d Wmsginfo        s              1    Inz('0')
004700120412     d alfaord         s              6
004800130404     d wmag            s              3
004900130404     d wdmag           s             20
005000130409     d wtorg           s              3
005100151120     d wtrov           s              1
005200151120     d wrkgetlista     s           4096    varying
005300020320
005400020319      *------------------------------------------------------------------------*
005500020319      * INPUT
005600020319      *------------------------------------------------------------------------*
005700020319      * RIDEFINIZIONE SCHIERA -FUD-
005800000000     D FUDDS           DS
005900000000     D  FUDDE1                 1      1
006000000000     D  FUDFUN                 2      3
006100000000     D  FUDDE2                 4     19
006200000000     D  FUDFI0                20     34
006300000000     D  FUDATT                35     35
006400000000     D  FUDFI1                36     80
006500000000     D  FUDDES                 1     19
006600000000     D  FUDRIG                 1     80
006700020319      *------------------
006800020319      * DS "XSRDA8" - CONTROLLA DATA (8)
006900020319      *------------------
007000971010     D WLBDA8          DS
007100971010     D  G08DAT                 1      8  0
007200971010     D  G08INV                 9     16  0
007300971010     D  G08ERR                17     17
007400971010     D  G08TGI                18     22  0
007500020320
007600020319      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
007700930201     D DSMSG         E DS
007800020319      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
007900930709     D DSUNIT        E DS
008000070503      * Parametri ricevuti
008100070503     d UNANAds       e ds                  extname(UNANA00F)
008200070503     d                                     prefix(UIM:3)
008300070503     d UNMOVds         ds
008400070509     d  UIMdoc                             like(UNMdoc)
008500070509     d  UIMddo                             like(UNMddo)
008600020319      * DS PER GESTIONE TASTI FUNZIONALI
008700930201     D DSKEY         E DS
008800930201     D DEVDS1          DS
008900930201     D  KEY                  369    369
009000020319      * ARCHITETTURA
009100930201     D KPJBA         E DS
009200020320
009300020319     d trul33ds      e ds
009400020320     d tibs02ds      e ds
009500020327     d tibs69ds      e ds
009600020327     d ds_cnaco      e ds                  extname(cnaco00f)
009700020327     d ds_cnind      e ds                  extname(cnind00f)
009800020327     d ds_cnclp      e ds                  extname(cnclp00f)
009900020327     d ds_fncls      e ds                  extname(fncls00f)
010000020709
010100020709     d ddatiute      e ds
010200020709     d azuteds       e ds                  extname(azute00f)
010300020709     d tibs34ds      e ds                  inz
010400021104
010500021104     d dunt          e ds
010600130404     d tnsdmagds     e ds
010700070503
010800070503     d Status         sds
010900070503     d   SDSprm          *parms
011000020319
011100020319      *------------------------------------------------------------------------*
011200020319      * MAIN LINE
011300020319      *------------------------------------------------------------------------*
011400000000     C                   EXSR      RUTINZ
011500000000     C*
011600000000     C* CICLO FINO A CHE:
011700000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
011800000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
011900930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
012000070509     c                   DOW            F03L00 = *off
012100070509     c                             and  F12L00 = *off
012200070509     c                             and  FLFINE = *off
012300000000     C*
012400000000     C* EMETTE FORMATO VIDEO
012500000000     C                   EXSR      BUTFMR
012600000000     C*
012700000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
012800000000     C                   EXSR      FUNCON
012900000000     C*
013000070509     C                   ENDDO                                                  <-- 2 ---
013100000000     C*
013200020320
013300020320     c                   clear                   trul33ds
013400020320     c                   eval      i33tla = 'C'
013500020320     c                   movel(p)  trul33ds      kpjbu
013600020320     c                   call      'TRUL33R'
013700020320     c                   parm                    kpjba
013800020320
013900940105     C                   MOVEL     DSUNIT        KPJBU
014000000000     C                   SETON                                        LR
014100020320
014200000000     C*FINE MAIN DEL PGM
014300000000     C*================================================================
014400000000     C* EMISSIONE FORMATO A VIDEO
014500000000     C*================================================================
014600000000     C     BUTFMR        BEGSR
014700930204     C*
014800930204     C* WRITE RECORD TESTATA
014900941115     C                   WRITE     SDU2D01
015000930205     C*
015100930205     C* VISUALIZZAZIONE MESSAGGIO
015200930205     C                   EXSR      SNDMSG
015300930204     C*
015400930204     C* WRITE RECORD FUNZIONI
015500941115     C                   WRITE     SDU2D02
015600000000     C*
015700930204     C* EXFMT RECORD DETTAGLIO
015800941115     C                   EXFMT     SDU2D03
015900000000     C*
016000000000     C* AZZERO FLAG DI ERRORE LIVELLO
016100000000     C                   MOVEL     '0'           ERRL00
016200000000     C*
016300000000     C* AZZERO FLAG DI ERRORE
016400930709     C                   SETOFF                                       606162
016500020320     C                   SETOFF                                       636465
016600930709     C                   SETOFF                                       666768
016700930709     C                   SETOFF                                       697071
016800930709     C                   SETOFF                                       727374
016900020320     C                   SETOFF                                       757677
017000120522     C                   SETOFF                                         79
017100000000     C*
017200000000     C                   ENDSR
017300000000     C*================================================================
017400000000     C* SNDMSG - MANDA MESSAGGIO
017500000000     C*================================================================
017600000000     C     SNDMSG        BEGSR
017700000000     C*
017800000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
017900000000     C                   MOVEL     '1'           *IN40
018000941115     C                   WRITE     SDU2DMC
018100000000     C                   MOVEL     '0'           *IN40
018200000000     C*
018300000000     C* VISUALIZZAZIONE MESSAGGIO
018400000000     C                   Z-ADD     0             MSGULT            4 0
018500930202     C                   CALL      'XMSG'        XMSGPA
018600930201     C     DSMSER        IFEQ      0
018700930201     C                   MOVEL     DSMSKE        MSGKEY
018800000000     C                   END
018900000000     C                   Z-ADD     1             MSGULT
019000941115     C                   WRITE     SDU2DMS                                      SFL MESS
019100941115     C                   WRITE     SDU2DMC                                      CONTR MESS
019200000000     C*
019300000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
019400930201     C                   MOVEL     *BLANKS       DSMSMS
019500000000     C*
019600000000     C                   ENDSR
019700000000     C*================================================================
019800000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
019900000000     C*================================================================
020000000000     C     ENTGES        BEGSR
020100930201     C*
020200930201     C* CONTROLLA I CAMPI IMMESSI
020300930201     C                   EXSR      DECODI
020400930202     C*
020500930202     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
020600930709     C     DSUOP1        IFEQ      'O05'
020700930202     C                   MOVEL     '1'           FLFINE
020800930202     C                   END
020900000000     C*
021000000000     C                   ENDSR
021100000000     C*================================================================
021200930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
021300000000     C*================================================================
021400000000     C     F01GES        BEGSR
021500000000     C                   ENDSR
021600000000     C*================================================================
021700930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
021800000000     C*================================================================
021900000000     C     F03GES        BEGSR
022000000000     C*
022100930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
022200000000     C                   MOVEL     '1'           F03L00
022300930202     C* IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
022400930709     C                   MOVEL     '1'           DSUF03
022500000000     C*
022600000000     C                   ENDSR
022700930201     C*================================================================
022800930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
022900930201     C*================================================================
023000930201     C     F06GES        BEGSR
023100930201     C*
023200930201     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
023300930709     C     DSUOP1        IFEQ      'F06'
023400070509     c     DSUop1        oreq      'F6N'
023500971015     C     DSUOP1        OREQ      'O03'
023600930201     C                   EXSR      INSERI
023700930201     C                   END
023800930709     C     DSUOP1        IFEQ      'O02'
023900930202     C                   EXSR      MODIFI
024000930201     C                   END
024100930709     C     DSUOP1        IFEQ      'O04'
024200930201     C                   EXSR      CANCEL
024300930201     C                   END
024400930709     C     DSUOP1        IFEQ      'O07'
024500930201     C                   EXSR      RIPRIS
024600930201     C                   END
024700930201     C*
024800930201     C                   ENDSR
024900000000     C*================================================================
025000120411     C* F10GES -GESTIONE TASTO FUNZIONALE F10 -
025100000000     C*================================================================
025200120411     C     F10GES        BEGSR
025300120521     c                   seton                                        1112
025400000000     C                   ENDSR
025500160923     C*================================================================
025600160923     C* F17GES -GESTIONE TASTO FUNZIONALE F17 - storicizzazione
025700160923     C*================================================================
025800160923     C     F17GES        BEGSR
025900160923
026000160923     C                   EXSR      DECODI
026100160923     C*
026200160923     C     ERRL00        IFEQ      '0'                                          --- 1 -->
026300160923     c
026400160923     c* Storicizza movimenti
026500160923     c                   z-add     unatip        kumtip
026600160923     C                   Z-ADD     UNACOD        KUMCOD
026700160923     C     KEYMOV        SETLL     UNMOV01L
026800160923     C     KEYMOV        READE     UNMOV01L                               97
026900160923DO  1C     *IN97         DOWEQ     *OFF
027000160923     C                   write     UNMOV0s
027100160923
027200160923     C     KEYMOV        READE     UNMOV01L                               97
027300160923    1c                   enddo
027400160923     c
027500160923     c                   write     unana0s
027600160923     c
027700160923     C* poi ANNULLA MOVIMENTI UNITA'
027800160923     c                   z-add     unatip        kumtip
027900160923     C                   Z-ADD     UNACOD        KUMCOD
028000160923     C     KEYMOV        SETLL     UNMOV01L
028100160923     C     KEYMOV        READE     UNMOV01L                               97
028200160923DO  1C     *IN97         DOWEQ     *OFF
028300160923     C                   DELETE    UNMOV000
028400160923     C     KEYMOV        READE     UNMOV01L                               97
028500160923E   1C                   ENDDO
028600160923     C* ANNULLA UNITA'
028700160923     C                   DELETE    UNANA000
028800160923     C*
028900160923     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
029000160923     C                   MOVEL     '1'           FLFINE
029100160923     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI CANC. EFFETTUATA
029200160923     C                   MOVEL     MSG(28)       DSUMSG
029300160923     c                   endif
029400160923     c
029500160923     C                   ENDSR
029600000000     C*================================================================
029700930709     C* F08GES -GESTIONE TASTO FUNZIONALE F08 - GEST.MOVIMENTI
029800000000     C*================================================================
029900000000     C     F08GES        BEGSR
030000930201     C*
030100930709     C* LANCIO IL PGM PER LA GESTIONE DEI MOVIMENTI
030200940105     C                   UNLOCK    UNANA01L                                     *SBLOCCA IL REC.ANAG
030300941115     C                   MOVEL     'TNSDU4R'     WPGM              8
030400930201     C                   EXSR      LANPGM
030500020405      * riaggancio il file
030600020405     c                   eval      unatip = dsutip
030700020405     c                   eval      unacod = dsucod
030800150604     C* se non siamo in visualizzazione, alloco il rcd
030900150604     C                   IF        DSUOP1 <> 'O05'
031000150604     c     keyuna        chain     unana01l
031100150604     C* se siamo in visualizzazione, NON alloco il rcd
031200150604     C                   ELSE
031300150604     c     keyuna        chain(n)  unana01l
031400150604     C                   ENDIF
031500140509     c                   exsr      azzvid
031600140509     c                   exsr      recvid
031700930201     C*
031800000000     C                   ENDSR
031900000000     C*================================================================
032000930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
032100000000     C*================================================================
032200000000     C     F12GES        BEGSR
032300000000     C*
032400000000     C* IMPOSTO FLAG DI LIVELLO
032500000000     C                   MOVEL     '1'           F12L00
032600070503      * IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
032700070509if  1c                   if            (DSUop1  = 'F06'
032800070509     c                              or  DSUop1  = 'F6N')
032900070503     c                             and  SDSprm  > 1
033000070503     C                   MOVEL     '2'           DSUF03
033100070503e   1c                   endif
033200000000     C*
033300000000     C                   ENDSR
033400930201     C*================================================================
033500930709     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA ANAGR.UNITA
033600930201     C*================================================================
033700930201     C     F21GES        BEGSR
033800020405
033900020405      * salvo la ds prima di richiamare un altro pgm
034000020405     c                   eval      sav_dsunit = dsunit
034100020405
034200930201     C*
034300930709     C* LANCIO IL PGM PER LA STAMPA DELL'ANAGRAFICA UNITA
034400940105     C                   MOVEL     DSUNIT        KPJBU
034500941115     C                   CALL      'TNSDU6R'
034600930209     C                   PARM                    KPJBA
034700940105     C                   MOVEL     KPJBU         DSUNIT
034800940105     C*
034900940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
035000020405     C**!!!DSUF03        IFEQ      '1'
035100020405     C**!!!              MOVEL     '1'           F03L00
035200020405     C**!!!              MOVEL     '1'           ERRL00
035300020405     C**!!!              ENDIF
035400020405
035500020405      * reimposto la ds dopo aver richiamato un altro programma
035600020405     c                   eval      dsunit = sav_dsunit
035700020405
035800930201     C*
035900930201     C                   ENDSR
036000000000     C*================================================================
036100930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
036200000000     C*================================================================
036300000000     C     F22GES        BEGSR
036400000000     C*
036500000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
036600000000     C                   Z-ADD     80            LENGH            15 5
036700000000     C                   CALL      'QCAEXEC'
036800000000     C                   PARM                    CMD(1)
036900000000     C                   PARM                    LENGH
037000000000     C*
037100000000     C                   ENDSR
037200000000     C*================================================================
037300930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
037400000000     C*================================================================
037500000000     C     F24GES        BEGSR
037600000000     C*
037700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
037800000000     C                   EXSR      FUNGES
037900000000     C*
038000000000     C                   ENDSR
038100000000     C*================================================================
038200000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
038300000000     C*================================================================
038400000000     C     HELGES        BEGSR
038500000000     C                   ENDSR
038600930201     C*================================================================
038700930201     C* INSERI - FUNZIONE DI INSERIMENTO
038800930201     C*================================================================
038900930201     C     INSERI        BEGSR
039000930201     C*
039100930201     C* CONTROLLA I CAMPI IMMESSI
039200930201     C                   EXSR      DECODI
039300930201     C*
039400930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
039500020320
039600020320      * aggiorno il numeratore
039700151120     c* se il numero già presente sul file--> ne stacco un altro
039800151120     c                   eval      wtrov='1'
039900151120    1c                   dow       errl00 = *off and wtrov='1'
040000020320     c                   clear                   trul33ds
040100020320     c                   eval      i33ope = 0
040200020320     c                   eval      i33cnu = 22
040300020320     c                   eval      i33num = 1
040400020320     c                   eval      i33unp = vidcod
040500020320     c                   movel(p)  trul33ds      kpjbu
040600020320     c                   call      'TRUL33R'
040700020320     c                   parm                    kpjba
040800020320     c                   movel(p)  kpjbu         trul33ds
040900151120     c
041000151120    2c                   if        o33err = *zeros
041100151120     c
041200151120     c* Se NON presente posso uscire dal ciclo
041300151120      /free
041400151120        wrkgetlista ='select unacod from unana00f where unacod=' +
041500151120                                                   %editc(o33nri:'X')   ;
041600151120       EXEC sql  prepare s1 from : wrkgetlista ;
041700151120       EXEC sql  declare a1 cursor for s1      ;
041800151120       EXEC sql  open    a1                    ;
041900151120
042000151120       EXEC sql fetch next from a1 into :vidcod ;
042100151120       if        sqlcod=100 ;
042200151120         vidcod = o33nri  ;
042300151120         wtrov='0'        ;
042400151120    3  endif              ;
042500151120       EXEC sql  close   a1                    ;
042600151120
042700151120      //end-free
042800151120
042900151120    2c                   endif
043000151120     c
043100020320     c                   if        o33err = 01
043200020320     c                   eval      errl00 = *on
043300020320     c                   eval      *in61 = *on
043400020320     c                   eval      dsmsms = msg(18)
043500020320     c                   endif
043600020320     c                   if        o33err = 02
043700020320     c                   eval      errl00 = *on
043800020320     c                   eval      *in61 = *on
043900020320     c                   eval      dsmsms = msg(19)
044000020320     c                   endif
044100020320     c                   if        o33err = 03
044200020320     c                   eval      errl00 = *on
044300020320     c                   eval      *in61 = *on
044400020320     c                   eval      dsmsms = msg(20)
044500020320     c                   endif
044600151120     c
044700151120    1c                   enddo
044800020320
044900151120     c                   if        errl00 = *off
045000020320
045100930709     C* IMPOSTO IL RECORD DAL VIDEO
045200930709     C                   EXSR      VIDREC
045300971014     C                   MOVEL     *BLANKS       UNAFTR
045400971014     C                   Z-ADD     *ZEROS        UNADTR
045500971014     C                   MOVEL     *BLANKS       UNAATB
045600930709     C                   WRITE     UNANA000
045700940504     C                   EXSR      SCRMOV                                       *SCRIVE 1° MOVIMENTO
045800940504     C* AZZERA IL CAMPO MATRICOLA PER EVITARE DOPPIO INSERIMENTO
045900940504     C                   MOVEL     *BLANKS       VIDMAT
046000941215     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
046100941215     C                   MOVEL     '1'           FLFINE
046200930518     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
046300020320     C                   MOVEL     MSG(3)        DSumsg
046400930201     C*
046500020320     c                   endif
046600930201     C                   END                                                    <-- 1 ---
046700930201     C*
046800930201     C                   ENDSR
046900930201     C*================================================================
047000930201     C* MODIFI - FUNZIONE DI MODIFICA
047100930201     C*================================================================
047200930201     C     MODIFI        BEGSR
047300930201     C*
047400930201     C* CONTROLLA I CAMPI IMMESSI
047500930201     C                   EXSR      DECODI
047600930201     C*
047700930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
047800930201     C*
047900930709     C* IMPOSTO IL RECORD DAL VIDEO E RISCRIVO
048000930709     C                   EXSR      VIDREC
048100971014     C                   MOVEL     *BLANKS       UNAFTR
048200971014     C                   MOVEL     *BLANKS       UNAATB
048300930709     C                   UPDATE    UNANA000
048400930518     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
048500930518     C                   MOVEL     '1'           FLFINE
048600930518     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI MODIF. EFFETTUATA
048700930709     C                   MOVEL     MSG(4)        DSUMSG
048800930201     C*
048900930201     C                   END                                                    <-- 1 ---
049000930201     C*
049100930201     C                   ENDSR
049200930201     C*================================================================
049300930201     C* CANCEL - FUNZIONE DI CANCELLAZIONE
049400930201     C*================================================================
049500930201     C     CANCEL        BEGSR
049600971013     C*
049700971013     C* ANNULLA MOVIMENTI UNITA'
049800020322     c                   z-add     unatip        kumtip
049900971013     C                   Z-ADD     UNACOD        KUMCOD
050000971013     C     KEYMOV        SETLL     UNMOV01L
050100971013     C     KEYMOV        READE     UNMOV01L                               97
050200971013DO  1C     *IN97         DOWEQ     *OFF
050300100907IF  2C     UNMDTR        IFEQ      *ZEROS
050400971014     C                   DELETE    UNMOV000
050500971014X   2C                   ELSE                                                   *GIA' TRASMESSO
050600971014     C                   MOVEL     'A'           UNMATB
050700971013     C                   UPDATE    UNMOV000
050800971014E   2C                   ENDIF
050900971013     C     KEYMOV        READE     UNMOV01L                               97
051000971013E   1C                   ENDDO
051100971013     C* ANNULLA UNITA'
051200101027IF  1C**   UNADTR        IFEQ      *ZEROS                                       *MAI TRASMESSO
051300971014     C                   DELETE    UNANA000
051400101027X   1C**                 ELSE                                                   *GIA' TRASMESSO
051500101027     C**                 MOVEL     *BLANKS       UNAFTR
051600101027     C**                 MOVEL     'A'           UNAATB
051700101027     C**                 UPDATE    UNANA000
051800101027E   1C**                 ENDIF
051900930201     C*
052000930201     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
052100930201     C                   MOVEL     '1'           FLFINE
052200930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI CANC. EFFETTUATA
052300930709     C                   MOVEL     MSG(5)        DSUMSG
052400930201     C*
052500930201     C                   ENDSR
052600930201     C*================================================================
052700930201     C* RIPRIS - FUNZIONE DI RIPRISTINO
052800930201     C*================================================================
052900930201     C     RIPRIS        BEGSR
053000930202     C*
053100930709     C* RIPRISTINO IL RECORD
053200930709     C                   MOVEL     *BLANKS       UNAFTR
053300971014     C                   MOVEL     *BLANKS       UNAATB
053400930709     C                   UPDATE    UNANA000
053500930202     C*
053600930202     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
053700930202     C                   MOVEL     '1'           FLFINE
053800930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI RIPR. EFFETTUATO
053900930709     C                   MOVEL     MSG(6)        DSUMSG
054000930202     C*
054100930201     C                   ENDSR
054200930202     C*================================================================
054300930709     C* SCRREC - SCRITTURA DEL RECORD DEL FILE
054400930202     C*================================================================
054500930709     C     SCRREC        BEGSR
054600930202     C*
054700020322     c                   z-add     wvdtip        unatip
054800930709     C                   Z-ADD     VIDCOD        UNACOD
054900930709     C     KEYUNA        CHAIN     UNANA01L                           99
055000930202     C     *IN99         IFEQ      '0'
055100930709     C                   EXSR      VIDREC
055200930709     C                   UPDATE    UNANA000
055300930202     C                   ELSE
055400930709     C                   EXSR      VIDREC
055500930709     C                   WRITE     UNANA000
055600930202     C                   END
055700930202     C*
055800930202     C                   ENDSR
055900940504     C*================================================================
056000940504     C*  SCRMOV - SCRIVE IL MOVIMENTO DI CARICO
056100940504     C*================================================================
056200940504     C     SCRMOV        BEGSR
056300940504     C*
056400020326     C                   Z-ADD     UNAtip        UNMtip
056500940504     C                   Z-ADD     UNACOD        UNMCOD
056600110407     c                   movel     unamat        unmmat
056700940504     C                   MOVEL     'CARI'        UNMCAU
056800940504     C                   MOVEL     'E'           UNMTMO
056900971010     C                   Z-ADD     DATCOR        UNMDTM
057000941216     C                   Z-ADD     20000         UNMPRG
057100940504     C                   Z-ADD     UNAMAG        UNMMAG
057200940504     C                   Z-ADD     *ZEROS        UNMMGC
057300940504     C                   MOVEL     'CARICO'      UNMDES
057400940504     C                   Z-ADD     VIDDOC        UNMDOC
057500971010     C                   Z-ADD     VIDDDO        G08DAT
057600971010     C                   Z-ADD     *ZEROS        G08INV
057700971010     C                   MOVEL     '0'           G08ERR
057800971010     C                   CALL      'XSRDA8'
057900971010     C                   PARM                    WLBDA8
058000971010     C                   Z-ADD     G08INV        UNMDDO
058100940504     C                   Z-ADD     *ZEROS        UNMCNT
058200020320     c                   eval      unmksc = unaksc
058300940504     C                   WRITE     UNMOV000
058400940504     C*
058500940504     C                   ENDSR
058600930201     C*================================================================
058700930201     C* LANPGM - LANCIA UN PROGRAMMA
058800930201     C*================================================================
058900930201     C     LANPGM        BEGSR
059000020405
059100020405      * salvo la ds prima di richiamare un altro pgm
059200020405     c                   eval      sav_dsunit = dsunit
059300020405
059400930201     C*
059500930201     C* IMPOSTO PARAMETRI PER IL PGM
059600930709     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
059700930709     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
059800930709     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
059900930709     C                   Z-ADD     UNATIP        DSUTIP                         TIPO UNITA
060000930709     C                   Z-ADD     UNACOD        DSUCOD                         COD.UNITA
060100930716     C                   Z-ADD     *ZEROS        DSUMAG                         MAGAZZINO
060200930712     C                   Z-ADD     *ZEROS        DSUPRG                         PROGRESS. MOV.
060300930906     C                   Z-ADD     *ZEROS        DSUDTM                         DATA MOVIMENTO
060400930201     C*
060500930201     C* LANCIO PROGRAMMA
060600940105     C                   MOVEL     DSUNIT        KPJBU
060700930312     C                   CALL      WPGM
060800940105     C                   PARM                    KPJBA
060900940105     C                   MOVEL     KPJBU         DSUNIT
061000930201     C*
061100930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
061200020405     C     DSUF03        IFEQ      '1'
061300020405     C**!!!              MOVEL     '1'           F03L00
061400020405     C**!!!              MOVEL     '1'           ERRL00
061500930201     C                   ELSE
061600930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
061700930709     C                   MOVEL     DSUMSG        DSMSMS
061800930201     C                   END
061900020405
062000020405      * reimposto la ds dopo aver richiamato un altro programma
062100020405     c                   eval      dsunit = sav_dsunit
062200020405
062300930201     C*
062400930201     C                   ENDSR
062500000000     C*================================================================
062600930201     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
062700000000     C*================================================================
062800000000     C     DECODI        BEGSR
062900020403
063000020403      * faccio i controlli solo per inserimento/manutenzione/copia
063100020403     c                   if        dsuop1 = 'F06' or dsuop1 = 'O02' or
063200070509     c                             DSUop1 = 'F6N' or
063300020403     c                             dsuop1 = 'O03'
063400020320
063500020320      * CONTROLLO E DECODIFICO IL TIPO UNITA
063600930709     C                   EXSR      CHKTIP
063700021104     c                   if        errl00 = *on and wmsginfo = *Off
063800930709     C                   MOVEL     MSG(7)        DSMSMS
063900930709     C                   SETON                                        60
064000930709     C                   END
064100021104      * Emetto msg. info
064200021104     c                   If        Wmsginfo = *On and *In78 = *Off
064300021104     c                   Movel     Msg(24)       DsmSms
064400091113     c                   Eval      *In65 = *On
064500021104     c                   Eval      *In78 = *On
064600021104     c                   EndIf
064700020321
064800020320      * CONTROLLO LA MARCA
064900020320     c                   if        errl00 = *off
065000930709     C     VIDMAR        IFEQ      *BLANKS
065100930709     C                   MOVEL     '1'           ERRL00
065200930709     C                   MOVEL     MSG(9)        DSMSMS
065300930709     C                   SETON                                        62
065400930709     C                   END
065500930709     C                   END
065600120521     c
065700120521     c* Numero ordine obbligatorio
065800120521      * faccio i controlli solo per inserimento/copia
065900120521     c                   if        dsuop1 = 'F06' or
066000120521     c                             DSUop1 = 'F6N' or
066100120521     c                             dsuop1 = 'O03'
066200120521     c                   if        vidord<=*zeros  and errl00=*off
066300120521     C                   MOVEL     '1'           ERRL00
066400120521     C                   MOVEL     MSG(27)       DSMSMS
066500120521     C                   SETON                                        79
066600120521     c                   leavesr
066700120521     C                   END
066800120521     C                   END
066900020320
067000020320      * CONTROLLO IL MODELLO
067100020320     c                   if        errl00 = *off
067200930709     C     VIDMOD        IFEQ      *BLANKS
067300930709     C                   MOVEL     '1'           ERRL00
067400930709     C                   MOVEL     MSG(10)       DSMSMS
067500930709     C                   SETON                                        63
067600930709     C                   END
067700930709     C                   END
067800020320
067900020320      * CONTROLLO LA MATRICOLA
068000020320     c                   if        errl00 = *off
068100930709     C     VIDMAT        IFEQ      *BLANKS
068200930709     C                   MOVEL     '1'           ERRL00
068300930709     C                   MOVEL     MSG(11)       DSMSMS
068400930709     C                   SETON                                        64
068500930709     C                   END
068600930709     C                   END
068700020403
068800020403     c                   endif
068900020321
069000020321      * controllo se già inserito un codice con stesso modello e matricola
069100091113     c                   if        errl00 = *off
069200020321     c                   eval      kunmod = vidmod
069300020321     c                   eval      kunmat = vidmat
069400020321     c     keyun8        setll     unana08l
069500020403      * immissione/copia
069600020403     c                   if        dsuop1 = 'F06' or dsuop1 = 'O03'             --- 1 -->
069700070509     c                                            or DSUop1 = 'F6N'
069800020321     c                   if        %equal(unana08l)                             --- 2 -->
069900020321     c                   eval      errl00 = *on
070000020321     c                   eval      *in63 = *on
070100020321     c                   eval      *in64 = *on
070200020321     c                   eval      dsmsms = msg(21)
070300020321     c                   endif                                                  <-- 2 ---
070400020403      * manutenzione
070500020403     c                   else                                                   <-- 1 -->
070600020403     c                   if        vidmod <> savmod or vidmat <> savmat         --- 2 -->
070700020403     c                   if        %equal(unana08l)                             --- 3 -->
070800020403     c                   eval      errl00 = *on
070900020403     c                   eval      *in63 = *on
071000020403     c                   eval      *in64 = *on
071100020403     c                   eval      dsmsms = msg(21)
071200020403     c                   endif                                                  <-- 3 ---
071300020403     c                   endif                                                  <-- 2 ---
071400020403     c                   endif                                                  <-- 1 ---
071500091113     c                   endif                                                  <-- 1 ---
071600160923     c*
071700091113
071800091113      * 13 11 09 EDPES --> da oggi è obbligatoria la matricola univoca
071900091113      *  all'interno dell'unità
072000091113     c                   if        errl00 = *off
072100091113     c                   eval      kuntip = wvdtip
072200091113     c                   eval      kunmat = vidmat
072300091113     c     kana2         setll     unana02l
072400091113      * immissione/copia
072500091113     c                   if        dsuop1 = 'F06' or dsuop1 = 'O03'             --- 1 -->
072600091113     c                                            or DSUop1 = 'F6N'
072700091113     c                   if        %equal(unana02l)                             --- 2 -->
072800091113     c                   eval      errl00 = *on
072900091113     c                   eval      *in64 = *on
073000091113     c                   eval      dsmsms = msg(25)
073100091113     c                   endif                                                  <-- 2 ---
073200091113      * manutenzione
073300091113     c                   else                                                   <-- 1 -->
073400091113     c                   if        vidmat <> savmat                             --- 2 -->
073500091113     c                   if        %equal(unana02l)                             --- 3 -->
073600091113     c                   eval      errl00 = *on
073700091113     c                   eval      *in64 = *on
073800091113     c                   eval      dsmsms = msg(25)
073900091113     c                   endif                                                  <-- 2 ---
074000091113     c                   endif                                                  <-- 1 ---
074100091113     c                   endif                                                  <-- 2 ---
074200091113     c                   endif                                                  <-- 2 ---
074300020321
074400020320      * controllo il 2° livello di raggruppamento
074500020320     c                   if        errl00 = *off
074600020321     c                   if        (vidrg2 = *blanks or vidrg2 = *zeros) and
074700070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
074800070509     c                                             or DSUop1 = 'F6N')
074900020320     c                   eval      errl00 = *on
075000020320     c                   eval      *in65 = *on
075100020320     c                   eval      dsmsms = msg(22)
075200020320     c                   endif
075300020320     c                   if        errl00 = *off
075400020320     c                   exsr      chkrg2
075500020320     c                   if        errl00 = *on
075600020320     c                   eval      *in65 = *on
075700020320     c                   eval      dsmsms = msg(22)
075800020320     c                   endif
075900020320     c                   endif
076000020320     c                   endif
076100020320
076200020320      * CONTROLLO LA DATA DI CONSEGNA
076300020320     c                   if        errl00 = *off
076400930709     C     VIDDCN        IFNE      *ZEROS
076500971010     C                   Z-ADD     VIDDCN        WDAT              8 0
076600930709     C                   EXSR      CHKDAT
076700971013     C                   Z-ADD     WDAT          VIDDCN
076800930709     C     ERRL00        IFEQ      '1'
076900930709     C                   SETON                                        66
077000930709     C                   END
077100930709     C                   END
077200020320     c                   if        viddcn = *zeros and
077300070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
077400070509     c                                             or DSUop1 = 'F6N')
077500020320     c                   eval      errl00 = *on
077600020320     c                   eval      *in66 = *on
077700020320     c                   eval      dsmsms = msg(12)
077800020320     c                   endif
077900930709     C                   END
078000020320
078100020320      * CONTROLLO LA DATA DI SCADENZA MANUTENZIONE
078200020320     c                   if        errl00 = *off
078300930709     C     VIDDSM        IFNE      *ZEROS
078400930709     C                   Z-ADD     VIDDSM        WDAT
078500930709     C                   EXSR      CHKDAT
078600971013     C                   Z-ADD     WDAT          VIDDSM
078700930709     C     ERRL00        IFEQ      '1'
078800930709     C                   SETON                                        67
078900930709     C                   END
079000930709     C                   END
079100020320     c                   if        viddsm = *zeros and
079200070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
079300070509     c                                             or DSUop1 = 'F6N')
079400020320     c                   eval      errl00 = *on
079500020320     c                   eval      *in67 = *on
079600020320     c                   eval      dsmsms = msg(12)
079700020320     c                   endif
079800930709     C                   END
079900020320
080000020320      * CONTROLLO LA DATA DI SCADENZA GARANZIA
080100020320     c                   if        errl00 = *off
080200930709     C     VIDDSG        IFNE      *ZEROS
080300930709     C                   Z-ADD     VIDDSG        WDAT
080400930709     C                   EXSR      CHKDAT
080500971013     C                   Z-ADD     WDAT          VIDDSG
080600930709     C     ERRL00        IFEQ      '1'
080700930709     C                   SETON                                        68
080800930709     C                   END
080900930709     C                   END
081000020320     c                   if        viddsg = *zeros and
081100070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
081200070509     c                                             or DSUop1 = 'F6N')
081300020320     c                   eval      errl00 = *on
081400020320     c                   eval      *in68 = *on
081500020320     c                   eval      dsmsms = msg(12)
081600020320     c                   endif
081700930709     C                   END
081800020320
081900020320      * CONTROLLO LA DATA DI SCADENZA LEASING
082000020320     c                   if        errl00 = *off
082100930709     C     VIDDSL        IFNE      *ZEROS
082200930709     C                   Z-ADD     VIDDSL        WDAT
082300930709     C                   EXSR      CHKDAT
082400971013     C                   Z-ADD     WDAT          VIDDSL
082500930709     C     ERRL00        IFEQ      '1'
082600930709     C                   SETON                                        69
082700930709     C                   END
082800930709     C                   END
082900930709     C                   END
083000020320
083100020320      * CONTROLLO DITTA PROPRIETA'
083200971126IF  1C     ERRL00        IFEQ      '0'
083300971126IF  2C     VIDSIS        IFEQ      *BLANKS
083400971126IF  3C     DSUOP1        IFEQ      'F06'
083500070509     c     DSUop1        oreq      'F6N'
083600971015     C     DSUOP1        OREQ      'O03'
083700940504     C                   MOVEL     '1'           ERRL00
083800940504     C                   MOVEL     MSG(13)       DSMSMS
083900940504     C                   SETON                                        70
084000971126E   3C                   ENDIF
084100971126X   2C                   ELSE
084200971126     C                   MOVE      VIDSIS        WMAG
084300130409     c* qualsiasi codice dell'organigranna di appoggio
084400130410     c                   eval      wtorg='SD '
084500971126     C                   EXSR      CHKMAG
084600971126IF  3C     ERRL00        IFEQ      '1'
084700971126     C                   MOVEL     MSG(13)       DSMSMS
084800971126     C                   SETON                                        70
084900971126X   3C                   ELSE
085000130404     c                   movel     wmag          vidsis
085100971126     C                   MOVEL     WDMAG         VDDSIS
085200971126E   3C                   END
085300971126E   2C                   END
085400971126E   1C                   END
085500020320
085600020320      * CONTROLLO DITTA MANUTENZIONE SEDE
085700930709     C     ERRL00        IFEQ      '0'
085800930709     C     VIDDMS        IFEQ      *BLANKS
085900930709     C     VIDDMS        OREQ      *ZEROS
086000940504     C     DSUOP1        IFEQ      'F06'
086100070509     c     DSUop1        oreq      'F6N'
086200971015     C     DSUOP1        OREQ      'O03'
086300940504     C                   MOVEL     '1'           ERRL00
086400940504     C                   MOVEL     MSG(14)       DSMSMS
086500940504     C                   SETON                                        71
086600940504     C                   ENDIF
086700930709     C                   ELSE
086800930709     C                   MOVE      VIDDMS        WMAG
086900130409     c* qualsiasi codice dell'organigranna di appoggio
087000130410     c                   eval      wtorg='SD '
087100930709     C                   EXSR      CHKMAG
087200930709     C     ERRL00        IFEQ      '1'
087300930709     C                   MOVEL     MSG(14)       DSMSMS
087400930709     C                   SETON                                        71
087500930709     C                   ELSE
087600930709     C                   MOVE      WMAG          VIDDMS
087700930709     C                   MOVEL     WDMAG         VDDDMS
087800930709     C                   END
087900930709     C                   END
088000930709     C                   END
088100020320
088200020320      * CONTROLLO DITTA MANUTENZIONE FILIALE
088300020320     c                   if        errl00 = *off                                --- 1 -->
088400020320     C     VIDDMF        IFEQ      *BLANKS                                      --- 2 -->
088500930709     C     VIDDMF        OREQ      *ZEROS
088600020320     c                   if        dsuop1 = 'F06' or dsuop1 = 'O03'
088700070509     c                                            or DSUop1 = 'F6N'
088800020320     c                   eval      errl00 = *on
088900020320     c                   eval      *in72 = *on
089000020320     c                   eval      dsmsms = msg(14)
089100020320     c                   endif
089200020320     C                   ELSE                                                   <-- 2 -->
089300930709     C                   MOVE      VIDDMF        WMAG
089400130409     c* qualsiasi codice dell'organigranna di appoggio
089500130410     c                   eval      wtorg='SD '
089600930709     C                   EXSR      CHKMAG
089700020320     C     ERRL00        IFEQ      '1'                                          --- 3 -->
089800930709     C                   MOVEL     MSG(14)       DSMSMS
089900930709     C                   SETON                                        72
090000020320     C                   else                                                   <-- 3 -->
090100930709     C                   MOVE      WMAG          VIDDMF
090200930709     C                   MOVEL     WDMAG         VDDDMF
090300020320     C                   END                                                    <-- 3 ---
090400020320     C                   END                                                    <-- 2 ---
090500020320     C                   END                                                    <-- 1 ---
090600020320
090700020320      * CONTROLLO DITTA LEASING
090800020320if  1c                   if        errl00 = *off
090900971126IF  2C     VIDLEA        IFNE      *BLANKS
091000971126     C                   MOVE      VIDLEA        WMAG
091100130409     c* qualsiasi codice dell'organigranna di appoggio
091200130410     c                   eval      wtorg='SD '
091300971126     C                   EXSR      CHKMAG
091400971126IF  3C     ERRL00        IFEQ      '1'
091500971126     C                   MOVEL     MSG(13)       DSMSMS
091600971126     C                   SETON                                        73
091700971126X   3C                   ELSE
091800130404     c                   movel     wmag          vidlea
091900971126     C                   MOVEL     WDMAG         VDDLEA
092000971126E   3C                   END
092100971126E   2C                   END
092200971126E   1C                   END
092300020320
092400020320      * CONTROLLO MAGAZZINO UBICAZIONE
092500930709     C     ERRL00        IFEQ      '0'
092600930709     C                   MOVE      VIDMAG        WMAG
092700130409     c                   eval      wtorg='E  '
092800930709     C                   EXSR      CHKMAG
092900930709     C     ERRL00        IFEQ      '1'
093000930709     C                   MOVEL     MSG(15)       DSMSMS
093100930709     C                   SETON                                        74
093200930709     C                   ELSE
093300930709     C                   MOVE      WMAG          VIDMAG
093400930709     C                   MOVEL     WDMAG         VDDMAG
093500930709     C                   END
093600930709     C                   END
093700110401     c
093800110401     c* non accetto magazzino 990 se dittà proprietà 923
093900110401     c                   if        vidmag='990' and vidsis='923'
094000110401     c                   eval      errl00='1'
094100110401     C                   MOVEL     MSG(26)       DSMSMS
094200110401     C                   SETON                                        74
094300110401     c                   endif
094400020320
094500020320      * controllo il cliente
094600020320     c                   if        errl00 = *off
094700020320     c                   if        vidksc <> *zeros or vddksc <> *blanks
094800020320     c                   exsr      chkksc
094900020320     c                   if        errl00 = *on
095000020320     c                   eval      *in77 = *on
095100020320     c                   eval      dsmsms = msg(23)
095200020320     c                   endif
095300020320     c                   endif
095400020321     c                   if        vidksc = *zeros and vidmag = '951'
095500020320     c                   eval      errl00 = *on
095600020320     c                   eval      *in77 = *on
095700020320     c                   eval      dsmsms = msg(23)
095800020320     c                   endif
095900020320     c                   endif
096000020320
096100020320      * CONTROLLO IL N. DEL DOCUMENTO (SOLO IN INSERIMENTO)
096200940504     C     ERRL00        IFEQ      '0'
096300940504     C     DSUOP1        IFEQ      'F06'
096400070509     c     DSUop1        oreq      'F6N'
096500971015     C     DSUOP1        OREQ      'O03'
096600940504     C     VIDDOC        IFEQ      *ZEROS
096700940504     C                   MOVEL     '1'           ERRL00
096800940504     C                   MOVEL     MSG(17)       DSMSMS
096900940504     C                   SETON                                        75
097000940504     C                   END
097100940504     C                   END
097200940504     C                   END
097300020320
097400020320      * CONTROLLO LA DATA DEL DOCUMENTO (SOLO IN INSERIMENTO)
097500940504     C     ERRL00        IFEQ      '0'
097600940504     C     DSUOP1        IFEQ      'F06'
097700070509     c     DSUop1        oreq      'F6N'
097800971015     C     DSUOP1        OREQ      'O03'
097900940504     C     VIDDDO        IFNE      *ZEROS
098000971010     C                   Z-ADD     VIDDDO        WDAT
098100940504     C                   EXSR      CHKDAT
098200971013     C                   Z-ADD     WDAT          VIDDDO
098300940504     C     ERRL00        IFEQ      '1'
098400940504     C                   SETON                                        76
098500940504     C                   END
098600940504     C                   ELSE
098700940504     C                   MOVEL     '1'           ERRL00
098800940504     C                   MOVEL     MSG(8)        DSMSMS
098900940504     C                   SETON                                        76
099000940504     C                   END
099100940504     C                   END
099200940504     C                   END
099300160923     c* in caso di storicizzazione non posso se l'unità non è in mag 990/981/923
099400160923     c                   if        dsuop1 = 'O0S'
099500160923     c                   if        vidmag<>'990' and vidmag<>'981' and
099600161010     c                             vidmag<>'923' and vidmag<>'980'
099700160923     c                   eval      errl00 = *on
099800160923     c                   eval      *in74 = *on
099900160923     c                   eval      dsmsms = msg(29)
100000160923     c                   endif                                                  <-- 1 ---
100100160923     c                   endif                                                  <-- 1 ---
100200120521     c
100300120521     C     DSUOP1        IFne      'F06'                                        *INS.
100400120521     C     DSUOP1        andne     'O03'                                        *DUPLICA
100500120521     C                   SETOFF                                       1112
100600120521     c                   endif
100700020320
100800000000     C                   ENDSR
100900020320      *================================================================
101000020320      *  CHKTIP - CONTROLLA IL TIPO DI UNITA
101100020320      *================================================================
101200930709     C     CHKTIP        BEGSR
101300021104
101400021104     c                   Eval      Wmsginfo = *Off
101500020319
101600020319      * CONTROLLO SE E' STATO INSERITO '?'
101700020319     C     '?'           SCAN      VIDTIP                                 99
101800020319
101900020319      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
102000020319     c                   if        *in99                                        --- 1 -->
102100021104     c                   Clear                   dunt
102200020319     c                   clear                   TIBS02DS
102300020319     c                   movel     'R'           t02mod
102400020319     c                   movel     knsif         t02sif
102500020321     c                   movel     'UNT'         t02cod
102600020319     c                   call      'TIBS02R'
102700020319     c                   parm                    KPJBA
102800020319     c                   parm                    TIBS02DS
102900020319     c                   movel     T02ke1        vidtip
103000020322     c                   movel     T02ke1        wvdtip
103100021104     c                   movel     t02uni        dunt
103200021104     c                   movel     d§untdes      vddtip
103300020319     c                   endif                                                  <-- 1 ---
103400020319
103500020319      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
103600021104     c                   Clear                   dunt
103700020319     c                   clear                   TIBS02DS
103800020319     c                   movel     'C'           T02mod
103900020319     c                   movel     knsif         t02sif
104000020321     c                   movel     'UNT'         t02cod
104100020320     c                   movel(p)  vidtip        T02ke1
104200020322     c                   move      vidtip        wvdtip
104300020319     c                   call      'TIBS02R'
104400020319     c                   parm                    KPJBA
104500020319     c                   parm                    TIBS02DS
104600020319     c                   if        t02err <> *blanks
104700020319     c                   eval      errl00 = '1'
104800020319     c                   clear                   vddtip
104900020319     c                   else
105000021104     c                   movel     t02uni        dunt
105100021104     c                   movel     d§untdes      vddtip
105200020319     c                   endif
105300021104
105400031204      * controllo flag x msg info
105500021104     c                   If        Errl00 = *Off  and  d§untfl1 = 'S'
105600021104     c                             and *In78 = *Off
105700070509     c                             and (DSUop1 <> 'F6N'
105800070509     c                              or  SDSprm <= 1)
105900021104     c                   Eval      Wmsginfo = *On
106000021104     c                   Eval      Errl00 = *On
106100021104     c                   EndIf
106200020319
106300930709     C                   ENDSR
106400020320      *================================================================
106500020320      *  CHKRG2 - CONTROLLA IL SECONDO LIVELLO DI RAGGRUPPAMENTO
106600020320      *================================================================
106700020320     c     chkrg2        begsr
106800020320
106900020320      * controllo se è stato inserito '?'
107000020320     c     '?'           scan      vidrg2                                 99
107100020320
107200020320      * è stato inserito '?', lancio il pgm di interr. tabelle
107300020320     c                   if        *in99                                        --- 1 -->
107400020320     c                   clear                   TIBS02DS
107500020320     c                   movel     'R'           t02mod
107600020320     c                   movel     knsif         t02sif
107700020321     c                   movel     'UNR'         t02cod
107800020320     c                   call      'TIBS02R'
107900020320     c                   parm                    KPJBA
108000020320     c                   parm                    TIBS02DS
108100020320     c                   movel     T02ke1        vidrg2
108200020320     c                   movel     t02uni        vddrg2
108300020320     c                   endif                                                  <-- 1 ---
108400020320
108500020320      * non è stato inserito '?', controllo che esista in tabella
108600020320     c                   clear                   TIBS02DS
108700020320     c                   movel     'C'           T02mod
108800020320     c                   movel     knsif         t02sif
108900020321     c                   movel     'UNR'         t02cod
109000020320     c                   movel(p)  vidrg2        T02ke1
109100020320     c                   call      'TIBS02R'
109200020320     c                   parm                    KPJBA
109300020320     c                   parm                    TIBS02DS
109400020320     c                   if        t02err <> *blanks
109500020320     c                   eval      errl00 = '1'
109600020320     c                   clear                   vddrg2
109700020320     c                   else
109800020320     c                   movel     t02uni        vddrg2
109900020320     c                   endif
110000020320
110100020320     c                   endsr
110200020320      *================================================================
110300020320      *  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
110400020320      *================================================================
110500930709     C     CHKMAG        BEGSR
110600020320
110700020320      * CONTROLLO SE E' STATO INSERITO '?'
110800130404     C**   '?'           SCAN      WMAG                                   99
110900020320
111000020320      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
111100130404     C**   *IN99         IFEQ      '1'                                          --- 1 -->
111200130404     C**                 MOVEL     *BLANKS       §COD1             3
111300130404     C**                 MOVE      *BLANKS       §DES1
111400130404     C**                 CALL      'TNSD20R'
111500130404     C**                 PARM                    §COD1
111600130404     C**                 PARM                    §TIP              1
111700130404     C**                 PARM                    §DES1            25
111800130404     C**                 MOVEL     §COD1         WMAG              3
111900130404     C*+                 MOVEL     §DES1         WDMAG            30
112000020320
112100130404     C**                 END                                                    <-- 1 ---
112200020320
112300020320      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
112400130404     C**                 MOVE      WMAG          ORGFIL
112500130404     C**   KEYORG        CHAIN     AZORG21l                           99
112600130404     C**   *IN99         IFEQ      '0'                                          --- 2 -->
112700130404     C**                 MOVEL     ORGFIL        WMAG
112800130404     C**                 MOVEL     ORGDES        WDMAG
112900130404     C*+                 ELSE                                                   <-- 2 -->
113000130404     c**   keyorg        chain     azorg01l
113100130404     c**                 if        %found(azorg01l)
113200130404     c**                 movel     orgfil        wmag
113300130404     c**                 movel     orgdes        wdmag
113400130404     c*+                 else
113500130404     C**                 MOVEL     '1'           ERRL00
113600130404     C**                 MOVEL     *ALL'*'       WDMAG
113700130404     c**                 endif
113800130404     C**                 END                                                    <-- 2 ---
113900020320
114000130404     c                   clear                   tnsdmagds
114100130404     c                   eval      §magmag=wmag
114200130409     c****               eval      §imagtorg='E  '
114300130409     c                   eval      §imagtorg=wtorg
114400130404     c                   call      'TNSDMAGR'
114500130404     c                   parm                    kpjba
114600130404     c                   parm                    tnsdmagds
114700130404     c
114800130404     c* Eseguita Ricerca
114900130404    1c                   if        §omagerr='R'
115000130404     c                   eval      wmag=§magmag
115100130404     c                   eval      wdmag=§omagdes
115200130404   x1c                   else
115300130404     c* Errore nel magazzino
115400130404    2c                   if        §omagerr='E'
115500130404     C                   MOVEL     '1'           ERRL00
115600130404     C                   MOVEL     *ALL'*'       WDMAG
115700130404     c* se tutto ok prendo decodifica e flag
115800130404   x2c                   else
115900130404     c                   eval      wdmag=§omagdes
116000130404    2c                   endif
116100130404    1c                   endif
116200130404     c
116300930709     C                   ENDSR
116400020320      *================================================================
116500020320      * CHKDAT - CONTROLLA LA DATA
116600020320      *================================================================
116700930709     C     CHKDAT        BEGSR
116800020320
116900971010     C                   Z-ADD     WDAT          G08DAT
117000971010     C                   Z-ADD     *ZEROS        G08INV
117100971010     C                   MOVEL     '0'           G08ERR
117200971010     C                   CALL      'XSRDA8'
117300971010     C                   PARM                    WLBDA8
117400971013     C                   Z-ADD     G08DAT        WDAT
117500971010     C     G08ERR        IFNE      '0'
117600930709     C                   MOVEL     '1'           ERRL00
117700930709     C                   MOVEL     MSG(12)       DSMSMS
117800930709     C                   END
117900020320
118000930709     C                   ENDSR
118100020320      *================================================================
118200020320      * CHKKSC - CONTROLLA IL CLIENTE
118300020320      *================================================================
118400020320     c     chkksc        begsr
118500020320
118600020320      * se non inserito il codice ma inserita una parte della ragione sociale
118700020320      * richiamo il programma della ricerca alfabetica
118800020320     c                   if        vidksc = *zeros and vddksc <> *blanks
118900020320     c                   exsr      ricalf
119000020320     c                   endif
119100020320
119200020320      * se inserito il codice controllo che esista sul piano dei conti
119300020320     c                   if        vidksc <> *zeros
119400020327     c                   clear                   TIBS69DS
119500020327     c                   z-add     vidksc        I69kac
119600020327     c                   call      'TIBS69R'
119700020327     c                   parm                    tibs69DS
119800020327     c                   parm                    ds_cnaco
119900020327     c                   parm                    ds_cnind
120000020327     c                   parm                    ds_cnclp
120100020327     c                   parm                    ds_fncls
120200020327
120300020327     c                   if        o69err <> '1'
120400020327     c                   movel     acorag        vddksc
120500020327     c                   else
120600020320     c                   eval      errl00 = *on
120700020320     c                   endif
120800020320     c                   endif
120900020320
121000020321     c                   endsr
121100020320      *================================================================
121200020320      *  RICALF - RICERCA ALFABETICA
121300020320      *================================================================
121400020320     c     ricalf        begsr
121500020320
121600020320     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
121700080922     C****               Z-ADD     9             PAXsta                         9=Escludo Annullati
121800080922     c* visulizzare anche i bloccati
121900080922     C                   Z-ADD     0             PAXsta                         9=Escludo Annullati
122000020709     C                   Z-ADD     DutKci        PAXccc                         Capoconto Clienti
122100020320     c                   movel(P)  vddksc        PAXdmt
122200020320     C                   z-add     1             PAXnum
122300020320
122400020320     C                   CALL      'XALFA3BR'
122500020320     C                   PARM                    PAXDUT
122600020320     C                   PARM                    CODUT
122700020320     C                   PARM                    PAXDMT
122800020320     C                   PARM                    PAXCCC
122900020320     C                   PARM                    PAXSTA
123000020320     C                   PARM      *blanks       PAXFLR
123100020320     C                   PARM      *blanks       PAXDIT
123200020320     C                   PARM                    PAXNUM            2 0
123300020320     C                   PARM                    PAXKCM           80
123400020320     C                   PARM                    PAXKSM          140
123500020320     C                   PARM                    PAXKDM           60
123600020320
123700020320      * NON TROVATO O PREMUTO CMD12
123800020320    2C     paxsta        IFGT      -1
123900020320     C                   MOVEL     PAXKSM        vidksc
124000020320    2C                   ENDIF
124100020320
124200020320     c                   endsr
124300930201     C*================================================================
124400930709     C*  RECVID - PORTA I CAMPI DAL RECORD AL VIDEO
124500930201     C*================================================================
124600930709     C     RECVID        BEGSR
124700930201     C*
124800930709     C                   MOVE      UNATIP        VIDTIP
124900930709     C                   Z-ADD     UNACOD        VIDCOD
125000930709     C                   MOVEL     UNAMAR        VIDMAR
125100930709     C                   MOVEL     UNAMOD        VIDMOD
125200930709     C                   MOVEL     UNAMAT        VIDMAT
125300020403     C                   MOVEL     UNAMOD        savMOD
125400020403     C                   MOVEL     UNAMAT        savMAT
125500930709     C                   MOVEL     UNADES        VIDDES
125600930709     C                   MOVEL     UNANOT        VIDNOT
125700971010     C                   Z-ADD     UNADCN        G08INV
125800971010     C                   Z-ADD     *ZEROS        G08DAT
125900971010     C                   MOVEL     '3'           G08ERR
126000971010     C                   CALL      'XSRDA8'
126100971010     C                   PARM                    WLBDA8
126200971010     C                   Z-ADD     G08DAT        VIDDCN
126300971010     C                   Z-ADD     UNADSG        G08INV
126400971010     C                   Z-ADD     *ZEROS        G08DAT
126500971010     C                   MOVEL     '3'           G08ERR
126600971010     C                   CALL      'XSRDA8'
126700971010     C                   PARM                    WLBDA8
126800971010     C                   Z-ADD     G08DAT        VIDDSG
126900930709     C                   MOVEL     UNASIS        VIDSIS
127000930709     C                   MOVE      UNADMS        VIDDMS
127100930709     C                   MOVE      UNADMF        VIDDMF
127200971010     C                   Z-ADD     UNADSM        G08INV
127300971010     C                   Z-ADD     *ZEROS        G08DAT
127400971010     C                   MOVEL     '3'           G08ERR
127500971010     C                   CALL      'XSRDA8'
127600971010     C                   PARM                    WLBDA8
127700971010     C                   Z-ADD     G08DAT        VIDDSM
127800930709     C                   MOVEL     UNALEA        VIDLEA
127900971010     C                   Z-ADD     UNADSL        G08INV
128000971010     C                   Z-ADD     *ZEROS        G08DAT
128100971010     C                   MOVEL     '3'           G08ERR
128200971010     C                   CALL      'XSRDA8'
128300971010     C                   PARM                    WLBDA8
128400971010     C                   Z-ADD     G08DAT        VIDDSL
128500930709     C                   MOVE      UNAMAG        VIDMAG
128600120411     C***                MOVEL     UNAEMU        VIDEMU
128700120412     c                   eval      videmu=%subst(unaemu:7:9)
128800120411     c
128900120411     c* Nel campo UNAEMU nelle prime 5 posizioni verrà memorizzato il numero ordine
129000120411     c                   clear                   alfaord
129100120412     c                   if        %subst(unaemu:1:6)>*zeros
129200120412     c                   eval      alfaord=%subst(unaemu:1:6)
129300120411     c                   movel     alfaord       vidord
129400120411     c                   else
129500120411     c                   clear                   vidord
129600120411     c                   endif
129700120411     c
129800930709     C                   MOVEL     UNAMVD        VIDMVD
129900150514     C                   CLEAR                   WLBDA8
130000150511     c                   IF        %subst(UNAMDR:8:8) <> *blank
130100150511     c                   eval      G08inv = %dec(%subst(UNAMDR:8:8):8:0)
130200150511     C                   Z-ADD     *ZEROS        G08DAT
130300150511     C                   MOVEL     '3'           G08ERR
130400150511     C                   CALL      'XSRDA8'
130500150511     C                   PARM                    WLBDA8
130600150514     C                   ENDIF
130700150511     C                   Z-ADD     G08DAT        VIDMDR2
130800930709     C                   MOVEL     UNAMTA        VIDMTA
130900930709     C                   MOVEL     UNAMDF        VIDMDF
131000930709     C                   MOVEL     UNAMRA        VIDMRA
131100150511     C                   EVAL      VIDMDR = %subst(UNAMDR:1:7)
131200930709     C                   MOVEL     UNAX25        VIDX25
131300930709     C                   MOVEL     UNALSW        VIDLSW
131400100903     C                   MOVE      UNALSW        VIDRESO
131500110104     C                   MOVE      UNALSW        savreso           1
131600020319     c                   move      unarg2        vidrg2
131700020319     c                   move      unaksc        vidksc
131800101027     c* Data dal reso
131900101027     C                   Z-ADD     UNADTR        G08INV
132000101027     C                   Z-ADD     *ZEROS        G08DAT
132100101027     C                   MOVEL     '3'           G08ERR
132200101027     C                   CALL      'XSRDA8'
132300101027     C                   PARM                    WLBDA8
132400101027     C                   Z-ADD     G08DAT        VIDdalr
132500930202     C*
132600930709     C     UNAATB        IFEQ      'A'
132700930202     C                   MOVEL     'ANNULLAT'    VIDATB
132800930202     C                   MOVE      'O'           VIDATB
132900930202     C                   ELSE
133000930202     C                   MOVEL     *BLANKS       VIDATB
133100930202     C                   END
133200930201     C*
133300930201     C                   ENDSR
133400930201     C*================================================================
133500930201     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
133600930201     C*================================================================
133700930201     C     AZZVID        BEGSR
133800930201     C*
133900120521     C                   SETOFF                                       1112
134000930709     C                   MOVEL     *BLANKS       VIDTIP
134100930709     C                   Z-ADD     *ZEROS        VIDCOD
134200930709     C                   MOVEL     *BLANKS       VIDMAR
134300930709     C                   MOVEL     *BLANKS       VIDMOD
134400930709     C                   MOVEL     *BLANKS       VIDMAT
134500020320     c                   clear                   vidrg2
134600020320     c                   clear                   vddrg2
134700930709     C                   MOVEL     *BLANKS       VIDDES
134800930709     C                   MOVEL     *BLANKS       VIDNOT
134900930709     C                   Z-ADD     *ZEROS        VIDDCN
135000930709     C                   Z-ADD     *ZEROS        VIDDSM
135100930709     C                   Z-ADD     *ZEROS        VIDDSG
135200930709     C                   Z-ADD     *ZEROS        VIDDSL
135300930709     C                   MOVEL     *BLANKS       VIDSIS
135400930709     C                   MOVEL     *BLANKS       VIDDMS
135500930709     C                   MOVEL     *BLANKS       VIDDMF
135600930709     C                   MOVEL     *BLANKS       VIDLEA
135700930709     C                   MOVEL     *BLANKS       VIDMAG
135800020320     c                   clear                   vidksc
135900020320     c                   clear                   vddksc
136000930709     C                   MOVEL     *BLANKS       VIDEMU
136100930709     C                   MOVEL     *BLANKS       VIDMVD
136200930709     C                   MOVEL     *BLANKS       VIDMTA
136300930709     C                   MOVEL     *BLANKS       VIDMDF
136400930709     C                   MOVEL     *BLANKS       VIDMRA
136500930709     C                   MOVEL     *BLANKS       VIDMDR
136600930709     C                   MOVEL     *BLANKS       VIDX25
136700930709     C                   MOVEL     *BLANKS       VIDLSW
136800100903     C                   MOVEL     *BLANKS       VIDRESO
136900110104     C                   MOVEL     *BLANKS       savRESO
137000940504     C                   Z-ADD     *ZEROS        VIDDOC
137100940504     C                   Z-ADD     *ZEROS        VIDDDO
137200101027     C                   Z-ADD     *ZEROS        VIDdalr
137300120411     C                   Z-ADD     *ZEROS        VIDord
137400930201     C*
137500930201     C                   ENDSR
137600070509      *===============================================================*
137700070509      *  RIEVID - IMPOSTAZIONE CAMPI A VIDEO CON QUELLI RICEVUTI      *
137800070509      *===============================================================*
137900070509     c     RieVid        BEGSR
138000070509      *
138100070509      * Programma richiamato da TNSDUKR x inserimento multiplo unità
138200070509      *   da codici a barre precedentemente inseriti in UNAIM00F
138300070509     c                   move      UIMtip        VIDtip
138400070509     c                   eval      VIDcod  = UIMcod
138500070509     c                   eval      VIDmar  = UIMmar
138600070509     c                   eval      VIDmod  = UIMmod
138700070509     c                   eval      VIDmat  = UIMmat
138800070509     c                   move      UIMrg2        VIDrg2
138900070509     c                   eval      VIDdes  = UIMdes
139000070509     c                   eval      VIDnot  = UIMnot
139100070509     c***                eval      VIDdcn  = UIMdcn
139200070509     c                   clear                   WLBda8
139300070509     c                   eval      G08inv  = UIMdcn
139400070509     c                   eval      G08err  = '3'
139500070509     c                   call      'XSRDA8'
139600070509     c                   parm                    WLBda8
139700070509     c                   eval      VIDdcn  = G08dat
139800070509     c***                eval      VIDdsm  = UIMdsm
139900070509     c                   clear                   WLBda8
140000070509     c                   eval      G08inv  = UIMdsm
140100070509     c                   eval      G08err  = '3'
140200070509     c                   call      'XSRDA8'
140300070509     c                   parm                    WLBda8
140400070509     c                   eval      VIDdsm  = G08dat
140500070509     c***                eval      VIDdsg  = UIMdsg
140600070509     c                   clear                   WLBda8
140700070509     c                   eval      G08inv  = UIMdsg
140800070509     c                   eval      G08err  = '3'
140900070509     c                   call      'XSRDA8'
141000070509     c                   parm                    WLBda8
141100070509     c                   eval      VIDdsg  = G08dat
141200070509     c***                eval      VIDdsl  = UIMdsl
141300070509     c                   clear                   WLBda8
141400070509     c                   eval      G08inv  = UIMdsl
141500070509     c                   eval      G08err  = '3'
141600070509     c                   call      'XSRDA8'
141700070509     c                   parm                    WLBda8
141800070509     c                   eval      VIDdsl  = G08dat
141900070509     c                   move      UIMsis        VIDsis
142000070509     c                   move      UIMlea        VIDlea
142100070509     c                   move      UIMdms        VIDdms
142200070509     c                   move      UIMdmf        VIDdmf
142300070509     c                   move      UIMmag        VIDmag
142400070509     c                   eval      VIDksc  = UIMksc
142500070509     c                   eval      VIDmdf  = UIMmdf
142600070509     c                   eval      VIDmra  = UIMmra
142700070509     c                   eval      VIDmdr  = UIMmdr
142800070509     c                   eval      VIDx25  = UIMx25
142900070509     c                   eval      VIDlsw  = UIMlsw
143000100907     c                   clear                   vidreso
143100101027     c                   clear                   viddalr
143200070509     c                   eval      VIDmvd  = UIMmvd
143300070509     c                   eval      VIDmta  = UIMmta
143400120412     c                   eval      VIDemu  = %subst(UIMemu:7:9)
143500120412     c                   if        %subst(UIMemu:1:6)>*zeros
143600120412     c                   movel     UIMemu        vidord
143700120412     c                   endif
143800070509     c                   eval      VIDdoc  = UIMdoc
143900070509     c***                eval      VIDddo  = UIMddo
144000070509     c                   clear                   WLBda8
144100070509     c                   eval      G08inv  = UIMddo
144200070509     c                   eval      G08err  = '3'
144300070509     c                   call      'XSRDA8'
144400070509     c                   parm                    WLBda8
144500070509     c                   eval      VIDddo  = G08dat
144600070509      *
144700070509     c                   ENDSR
144800971015     C*================================================================
144900971015     C* DUPVID - REIMPOSTA IL VIDEO PER LA DUPLICA
145000971015     C*================================================================
145100971015     C     DUPVID        BEGSR
145200971015     C*
145300971015     C* LEGGE IL RECORD DA DUPLICARE
145400020322     c                   z-add     dsutip        unatip
145500971015     C                   Z-ADD     DSUCOD        UNACOD
145600971015     C     KEYUNA        CHAIN     UNANA01L                           99
145700971015     C     *IN99         IFEQ      *OFF                                         *TROVATO
145800971015     C                   EXSR      RECVID                                       *RIEMPE CAMPI
145900971015     C                   ELSE                                                   *NON TROVATO
146000971015     C                   EXSR      AZZVID                                       *AZZERA CAMPI
146100971015     C                   ENDIF
146200971015     C*
146300971015     C* REIMPOSTA I CAMPI DA NON DUPLICARE
146400971015     C                   Z-ADD     *ZEROS        VIDCOD                         *CHIAVE
146500971015     C                   MOVEL     *BLANKS       VIDATB                         *ANNULLAMENTO
146600120521     c                   clear                   vidord
146700971015     C*
146800971015     C                   ENDSR
146900930201     C*================================================================
147000930709     C*  VIDREC - IMPOSTA CAMPI DA VIDEO A RECORD
147100930201     C*================================================================
147200930709     C     VIDREC        BEGSR
147300930201     C*
147400930709     C                   MOVE      VIDTIP        UNATIP
147500930709     C                   Z-ADD     VIDCOD        UNACOD
147600930709     C                   MOVEL     VIDMAR        UNAMAR
147700930709     C                   MOVEL     VIDMOD        UNAMOD
147800930709     C                   MOVEL     VIDMAT        UNAMAT
147900020320     c                   movel     vidrg2        unarg2
148000930709     C                   MOVEL     VIDDES        UNADES
148100930709     C                   MOVEL     VIDNOT        UNANOT
148200971010     C                   Z-ADD     VIDDCN        G08DAT
148300971010     C                   Z-ADD     *ZEROS        G08INV
148400971010     C                   MOVEL     '0'           G08ERR
148500971010     C                   CALL      'XSRDA8'
148600971010     C                   PARM                    WLBDA8
148700971010     C                   Z-ADD     G08INV        UNADCN
148800971010     C                   Z-ADD     VIDDSG        G08DAT
148900971010     C                   Z-ADD     *ZEROS        G08INV
149000971010     C                   MOVEL     '0'           G08ERR
149100971010     C                   CALL      'XSRDA8'
149200971010     C                   PARM                    WLBDA8
149300971010     C                   Z-ADD     G08INV        UNADSG
149400930709     C                   MOVEL     VIDSIS        UNASIS
149500930709     C                   MOVE      VIDDMS        UNADMS
149600930709     C                   MOVE      VIDDMF        UNADMF
149700971010     C                   Z-ADD     VIDDSM        G08DAT
149800971010     C                   Z-ADD     *ZEROS        G08INV
149900971010     C                   MOVEL     '0'           G08ERR
150000971010     C                   CALL      'XSRDA8'
150100971010     C                   PARM                    WLBDA8
150200971010     C                   Z-ADD     G08INV        UNADSM
150300930709     C                   MOVEL     VIDLEA        UNALEA
150400971010     C                   Z-ADD     VIDDSL        G08DAT
150500971010     C                   Z-ADD     *ZEROS        G08INV
150600971010     C                   MOVEL     '0'           G08ERR
150700971010     C                   CALL      'XSRDA8'
150800971010     C                   PARM                    WLBDA8
150900971010     C                   Z-ADD     G08INV        UNADSL
151000930709     C                   MOVE      VIDMAG        UNAMAG
151100020320     c                   z-add     vidksc        unaksc
151200120411     C*****              MOVEL     VIDEMU        UNAEMU
151300120411     c                   clear                   unaemu
151400120412     c                   eval      %subst(unaemu:7:9)=videmu
151500120411     c                   if        vidord>0
151600120412     c                   eval      %subst(unaemu:1:6)=%editc(vidord:'X')
151700120411     c                   else
151800120521     c                   eval      %subst(unaemu:1:6)='      '
151900120411     c                   endif
152000120411     c
152100930709     C                   MOVEL     VIDMVD        UNAMVD
152200930709     C                   MOVEL     VIDMTA        UNAMTA
152300930709     C                   MOVEL     VIDMDF        UNAMDF
152400930709     C                   MOVEL     VIDMRA        UNAMRA
152500930709     C                   MOVEL     VIDMDR        UNAMDR
152600930709     C                   MOVEL     VIDX25        UNAX25
152700930709     C                   MOVEL     VIDLSW        UNALSW
152800100903     C                   MOVE      VIDreso       UNALSW
152900101027     c
153000101027     c* se tolta la S di reso, tolgo anche la data
153100101027     c                   if        vidreso=*blanks
153200101027     c                   clear                   unadtr
153300101027     c                   else
153400101027     c* se data piena la reimposto altrimenti metto udate
153500110104     c*  Metto udate anche se reimposto il flag di reso
153600110104     c                   if        viddalr>0  and savreso=vidreso
153700101027     C                   Z-ADD     VIDdalr       G08DAT
153800101027     C                   Z-ADD     *ZEROS        G08INV
153900101027     C                   MOVEL     '0'           G08ERR
154000101027     C                   CALL      'XSRDA8'
154100101027     C                   PARM                    WLBDA8
154200101027     C                   Z-ADD     G08INV        UNADTR
154300101027     c                   else
154400101027     c                   movel     datcor        UNADTR
154500101027     c                   endif
154600101027     c                   endif
154700930201     C*
154800930201     C                   ENDSR
154900000000     C*================================================================
155000000000     C*  RUTINZ - ROUTINE INIZIALE
155100000000     C*================================================================
155200000000     C     RUTINZ        BEGSR
155300930201     C*
155400930201     C* RICEVIMENTO PARAMETRI
155500930201     C     *ENTRY        PLIST
155600940105     C                   PARM                    KPJBA
155700070503     C                   PARM                    UNANAds
155800070503     C                   PARM                    UNMOVds
155900940105     C                   MOVEL     KPJBU         DSUNIT
156000070503      * - impostazione iniziale dei parametri di output per chiamante
156100070503     c                   eval      DSUf03 = *off                                PREMUTO F03
156200070503     c                   eval      DSUerr = *off                                '1'=ERRORE
156300070503     c                   eval      DSUmsg = *blanks                             MESSAGGIO
156400930201     C*
156500930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
156600930201     C     XMSGPA        PLIST
156700930201     C                   PARM                    DSMSMS
156800930201     C                   PARM                    DSMSPG
156900930201     C                   PARM                    DSMSKE
157000930201     C                   PARM                    DSMSER
157100971013     C* DEFINIZIONI VARIABILI
157200020322     c     *like         define    unmtip        kumtip
157300971013     C     *LIKE         DEFINE    UNMCOD        KUMCOD
157400930201     C*
157500020320      * CHIAVE LETTURA UNANA01L
157600930709     C     KEYUNA        KLIST
157700020322     c                   kfld                    unatip
157800930709     C                   KFLD                    UNACOD
157900151120     C     KEYUNA_I      KLIST
158000151120     c                   kfld                    vidtip
158100151120     C                   KFLD                    vidCOD
158200020320
158300020320     c     keyun8        klist
158400020320     c                   kfld                    kunmod
158500020320     c                   kfld                    kunmat
158600091113     c     kana2         klist
158700091113     c                   kfld                    kuntip
158800091113     c                   kfld                    kunmat
158900020320
159000020320      * CHIAVE LETTURA UNMOV01L
159100971013     C     KEYMOV        KLIST
159200020322     c                   kfld                    kumtip
159300971013     C                   KFLD                    KUMCOD
159400020320
159500930201     C*
159600020529     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
159700930201     C     KEYORG        KLIST
159800930709     C                   KFLD                    ORGFIL
159900930201     C*
160000930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
160100930201     C                   CALL      'XKEY'
160200930201     C                   PARM                    DSKEY
160300930201     C*
160400930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
160500930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
160600930201     C*
160700930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
160800930201     C*   -NOME PGM CHIAMANTE
160900930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
161000941115     C                   MOVEL     'TNSDU2R'     DSMSPG
161100941115     C                   MOVEL     'TNSDU2R'     MSGQUE
161200930201     C                   MOVEL     *BLANKS       MSGKEY
161300930201     C                   MOVEL     *BLANKS       DSMSMS
161400930202     C*
161500160923     C* SE no gestione     TOLGO FUNZIONE AGGIORNAMENTO
161600160923     C     DSUOP1        IFeq      'O05'
161700160923     C     DSUOP1        oreq      'O0S'
161800930202     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
161900930202     C                   MOVEL     'N'           FUDATT
162000930202     C                   MOVEL     FUDRIG        FUD(6)
162100160923     c
162200120411     C                   MOVEL     FUD(10)       FUDRIG                         *sblocca ordine
162300120411     C                   MOVEL     'N'           FUDATT
162400120411     C                   MOVEL     FUDRIG        FUD(10)
162500930202     C                   END
162600160923     c*
162700160923     c* se storicizzazione abilito tasto funz
162800160923     c                   if        dsuop1='O0S'
162900160923     C                   MOVEL     FUD(17)       FUDRIG                         *storicizzazione
163000160923     C                   MOVEL     'S'           FUDATT
163100160923     C                   MOVEL     FUDRIG        FUD(10)
163200160923     C                   END
163300940105     C*
163400940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DEI MOVIMENTI UNITA'
163500940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
163600940105     C     DSUPRO        IFEQ      'M'
163700940105     C                   MOVEL     FUD(8)        FUDRIG                         *GEST.MOVIMENTI
163800940105     C                   MOVEL     'N'           FUDATT
163900940105     C                   MOVEL     FUDRIG        FUD(8)
164000940105     C                   END
164100160923     c
164200160923     c* se il richiamo è int dello storico, disabilito tutto tranne i movimenti
164300160923     c                   if        dsupro='S'
164400160923     C                   MOVEL     FUD(8)        FUDRIG                         *GEST.MOVIMENTI
164500160923     C                   MOVEL     'N'           FUDATT
164600160923     C                   MOVEL     FUDRIG        FUD(8)
164700160923     C                   END
164800930201     C*
164900930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
165000930201     C                   Z-ADD     0             FUNGRU            5 0
165100930201     C*
165200930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
165300930201     C                   EXSR      FUNGES
165400930201     C*
165500930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
165600930201     C                   MOVEL     '0'           F12L00            1
165700930201     C                   MOVEL     '0'           F03L00            1
165800930201     C                   MOVEL     '0'           FINL00            1
165900930201     C                   MOVEL     '0'           ERRL00            1
166000930201     C                   MOVEL     '0'           FLFINE            1
166100930204     C*
166200971010     C* REPERIMENTO DATI UTENTE
166300020709
166400020709     c     *dtaara       define    §azute        azuteds
166500020709     c     *dtaara       define    §datiute      ddatiute
166600020709
166700020709     c                   in(E)     *dtaara
166800020709     c                   if        %Error or rsut = *blanks
166900020709     c                   clear                   tibs34ds
167000020709     c                   call      'TIBS34R'
167100020709     c                   parm                    Tibs34Ds
167200020709     c                   in        *dtaara
167300020709     c                   endif
167400971010     C*
167500971010     C* REPERISCE LA DATA CORRENTE
167600971010     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
167700971010     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
167800971010     C                   Z-ADD     WN8           G08DAT
167900971010     C                   Z-ADD     *ZEROS        G08INV
168000971010     C                   MOVEL     '0'           G08ERR
168100971010     C                   CALL      'XSRDA8'
168200971010     C                   PARM                    WLBDA8
168300971010     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
168400930201     C*
168500930201     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
168600930201     C* 0=NON PROTETTO, 1=PROTETTO
168700930709     C*   90 - DATI FILE
168800070503     C*   91 - CAMPI CHIAVE
168900940504     C*   92 - CAMPI NUM. E DATA DOC.
169000070503     C*   93 - NUMERO MATRICOLA
169100070503      *
169200070503     c                   eval      *in93 = *off
169300070503      *
169400930201     C* --> UTENTE ENTRATO IN GESTIONE
169500070503if  1C     DSUOP0        IFEQ      ' 2'                                         --- 1 -->
169600930201     C*
169700930709     C     DSUOP1        IFEQ      'F06'                                        *INS.
169800070509     c     DSUop1        oreq      'F6N'                                        *DUPLICA
169900971015     C     DSUOP1        OREQ      'O03'                                        *DUPLICA
170000940504     C                   SETOFF                                       909192
170100120521     C                   END
170200930201     C*
170300070503if  2C     DSUOP1        IFEQ      'O02'                                        *MOD.
170400930709     C                   SETON                                        91
170500020709     c                   If        DutLpo = 'S'
170600930709     C                   SETOFF                                       90
170700930201     C                   ELSE                                                   *FIL.
170800930709     C                   SETON                                        90
170900930201     C                   END
171000070503e   2C                   END
171100930201     C*
171200930709     C     DSUOP1        IFEQ      'O05'                                        *VIS+CANC+RIPR
171300930709     C     DSUOP1        OREQ      'O04'
171400930709     C     DSUOP1        OREQ      'O07'
171500940504     C                   SETON                                        909192
171600930201     C                   END
171700930201     C*
171800070503e   1C                   END                                                    <-- 1 ---
171900160923     C* --> UTENTE ENTRATO IN INTERROGAZIONE o storicizzazione
172000940105     C     DSUOP0        IFEQ      ' 5'
172100160923     C     DSUOP0        oreq      ' S'
172200940504     C                   SETON                                        909192
172300930201     C                   END
172400930201     C*
172500930709     C* SE SONO IN MOD. O VIS. O CANC. O RIPR. LEGGO RECORD
172600070503if  1C     DSUOP1        IFEQ      'O02'                                        --- 1 -->
172700971015     C     DSUOP1        OREQ      'O04'
172800930709     C     DSUOP1        OREQ      'O05'
172900930709     C     DSUOP1        OREQ      'O07'
173000160923     C     DSUOP1        OREQ      'O0S'
173100930709     C* SE IL REC.NON ESISTE RIMANDO ERRORE AL PGM CHIAMANTE
173200020322     c                   z-add     dsutip        unatip
173300930709     C                   Z-ADD     DSUCOD        UNACOD
173400150604     C* se non siamo in visualizzazione, alloco il rcd
173500150604     C                   IF        DSUOP1 <> 'O05'
173600930709     C     KEYUNA        CHAIN     UNANA01L                           99
173700150604     C* se siamo in visualizzazione, NON alloco il rcd
173800150604     C                   ELSE
173900150604     C     KEYUNA        CHAIN(N)  UNANA01L                           99
174000150604     C                   ENDIF
174100930201     C     *IN99         IFEQ      '1'
174200930201     C                   MOVEL     '1'           ERRL00
174300930709     C                   MOVEL     MSG(16)       DSMSMS
174400930202     C                   ELSE
174500930709     C                   EXSR      RECVID
174600930201     C                   END
174700930208     C*
174800930208     C* SE TUTTO OK, DECODIFICO I CAMPI A VIDEO
174900930208     C                   EXSR      DECODI
175000930201     C*
175100070503e   1C                   END                                                    <-- 1 ---
175200930202     C*
175300930202     C* SE INSERIMENTO AZZERO TUTTI I CAMPI DEL VIDEO
175400930709     C     DSUOP1        IFEQ      'F06'
175500070509     c     DSUop1        oreq      'F6N'
175600930202     C                   EXSR      AZZVID
175700070509if  1c                   if            (DSUop1  = 'F06'
175800070509     c                              or  DSUop1  = 'F6N')
175900070509     c                             and  SDSprm  > 1
176000070509     c                   exsr      RieVid
176100070509e   1c                   endif
176200930202     C                   END
176300971015     C*
176400971015     C* SE DUPLICA REIMPOSTA IL VIDEO
176500971015     C     DSUOP1        IFEQ      'O03'
176600971015     C                   EXSR      DUPVID
176700971015     C                   ENDIF
176800120521
176900120521     C     DSUOP1        IFEQ      'F06'                                        *INS.
177000120521     C     DSUOP1        OREQ      'O03'                                        *DUPLICA
177100120521     C                   SETOn                                        11
177200120521     C                   END
177300930201     C*
177400930201     C* IMPOSTO DESCRIZIONE DELLA FUNZIONE IN CORSO A VIDEO
177500930709     C     DSUOP1        IFEQ      'F06'
177600070509     c     DSUop1        oreq      'F6N'
177700971015     C     DSUOP1        OREQ      'O03'
177800930201     C                   MOVEL     ' INSERIM'    VIDFUN
177900930201     C                   MOVE      'ENTO   '     VIDFUN
178000930201     C                   END
178100930709     C     DSUOP1        IFEQ      'O02'
178200930201     C                   MOVEL     '   MODIF'    VIDFUN
178300930201     C                   MOVE      'ICA    '     VIDFUN
178400930201     C                   END
178500930709     C     DSUOP1        IFEQ      'O04'
178600930201     C                   MOVEL     ' CANCELL'    VIDFUN
178700930201     C                   MOVE      'AZIONE '     VIDFUN
178800930201     C                   END
178900930709     C     DSUOP1        IFEQ      'O05'
179000930201     C                   MOVEL     'VISUALIZ'    VIDFUN
179100930201     C                   MOVE      'ZAZIONE'     VIDFUN
179200930201     C                   END
179300930709     C     DSUOP1        IFEQ      'O07'
179400930201     C                   MOVEL     ' RIPRIST'    VIDFUN
179500930201     C                   MOVE      'INO    '     VIDFUN
179600930201     C                   END
179700160923     C     DSUOP1        IFEQ      'O0S'
179800160923     C                   MOVEL     'STORICIZ'    VIDFUN
179900160923     C                   MOVE      'ZAZIONE'     VIDFUN
180000160923     C                   END
180100070509      *
180200070509      * SE pgm. richiamato da TNSDUKR per l'immissione di un record
180300070509      *  del quale vengono passati i valori per ogni campo,
180400070509      * ALLORA imposto flag per ritorno al pgm. chiamante
180500070509if  1c                   IF             DSUop1  = 'F6N'
180600070509     c                             and  SDSprm  > 1
180700070509     c                   exsr      INSERI
180800070611     c                   eval      DSUerr = ErrL00                              '1'=ERRORE
180900070611     c                   eval      DSUmsg = DSmsms                              MESSAGGIO
181000070509     c                   eval      FLfine = *on
181100070509e   1c                   ENDIF
181200000000     C*
181300000000     C                   ENDSR
181400000000     C*================================================================
181500000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
181600000000     C*================================================================
181700000000     C     FUNGES        BEGSR
181800000000     C*
181900000000     C* PULISCO CAMPI DI LAVORO
182000000000     C                   Z-ADD     0             W                 5 0
182100000000     C*
182200000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
182300000000     C* E CARICO TABELLA FUNZIONI ATTIVE
182400000000     C     1             DO        24            I
182500000000     C                   MOVEL     FUD(I)        FUDRIG
182600000000     C*
182700000000     C* PULIZIA FUNZIONI PRECEDENTI
182800000000     C                   MOVEL     *BLANKS       FUV(I)
182900000000     C*
183000000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
183100000000     C* (SALTA F24)
183200000000     C     FUDATT        IFEQ      'S'
183300000000     C     FUDFUN        ANDNE     '24'
183400000000     C                   ADD       1             W
183500000000     C                   MOVEL     FUDDES        FUV(W)
183600000000     C                   END
183700000000     C                   END                                                    DO WHILE
183800000000     C*
183900000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
184000000000     C* ALTRIMENTI DISATTIVO F24
184100000000     C     1             DO        24            I
184200000000     C                   MOVEL     FUD(I)        FUDRIG
184300000000     C*
184400000000     C     FUDFUN        IFEQ      '24'
184500000000     C     W             IFGT      6
184600000000     C                   MOVEL     'S'           FUDATT
184700000000     C                   ELSE
184800000000     C                   MOVEL     *BLANKS       FUDATT
184900000000     C                   END                                                    W GT 8
185000000000     C                   MOVEL     FUDRIG        FUD(I)
185100000000     C                   END
185200000000     C                   END                                                    DO WHILE
185300000000     C*
185400000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
185500000000     C     W             DIV       6             I
185600000000     C                   MVR                     Z
185700000000     C*
185800000000     C     Z             IFNE      0
185900000000     C     I             ADD       1             Z
186000000000     C                   ELSE
186100000000     C                   Z-ADD     I             Z                 5 0
186200000000     C                   END
186300000000     C*
186400000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
186500000000     C                   ADD       1             FUNGRU
186600000000     C     FUNGRU        IFGT      Z
186700000000     C     FUNGRU        ORLT      1
186800000000     C                   Z-ADD     1             FUNGRU
186900000000     C                   END
187000000000     C*
187100000000     C* PRIMO GRUPPO DI 6 FUNZIONI
187200000000     C     FUNGRU        IFEQ      1
187300000000     C                   MOVEL     FUV(1)        FUN001
187400000000     C                   MOVEL     FUV(2)        FUN002
187500000000     C                   MOVEL     FUV(3)        FUN003
187600000000     C                   MOVEL     FUV(4)        FUN004
187700000000     C                   MOVEL     FUV(5)        FUN005
187800000000     C                   MOVEL     FUV(6)        FUN006
187900000000     C                   END
188000000000     C*
188100000000     C* SECONDO GRUPPO DI 6 FUNZIONI
188200000000     C     FUNGRU        IFEQ      2
188300000000     C                   MOVEL     FUV(7)        FUN001
188400000000     C                   MOVEL     FUV(8)        FUN002
188500000000     C                   MOVEL     FUV(9)        FUN003
188600000000     C                   MOVEL     FUV(10)       FUN004
188700000000     C                   MOVEL     FUV(11)       FUN005
188800000000     C                   MOVEL     FUV(12)       FUN006
188900000000     C                   END
189000000000     C*
189100000000     C* TERZO GRUPPO DI 6 FUNZIONI
189200000000     C     FUNGRU        IFEQ      3
189300000000     C                   MOVEL     FUV(13)       FUN001
189400000000     C                   MOVEL     FUV(14)       FUN002
189500000000     C                   MOVEL     FUV(15)       FUN003
189600000000     C                   MOVEL     FUV(16)       FUN004
189700000000     C                   MOVEL     FUV(17)       FUN005
189800000000     C                   MOVEL     FUV(18)       FUN006
189900000000     C                   END
190000000000     C*
190100000000     C* QUARTO GRUPPO DI 6 FUNZIONI
190200000000     C     FUNGRU        IFEQ      4
190300000000     C                   MOVEL     FUV(19)       FUN001
190400000000     C                   MOVEL     FUV(20)       FUN002
190500000000     C                   MOVEL     FUV(21)       FUN003
190600000000     C                   MOVEL     FUV(22)       FUN004
190700000000     C                   MOVEL     *BLANKS       FUN005
190800000000     C                   MOVEL     *BLANKS       FUN006
190900000000     C                   END
191000000000     C*
191100000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
191200000000     C                   MOVEL     *BLANKS       FUN007
191300000000     C                   MOVEL     *BLANKS       FUN008
191400000000     C*
191500000000     C     1             DO        24            I
191600000000     C                   MOVEL     FUD(I)        FUDRIG
191700000000     C     FUDFUN        IFEQ      '24'
191800000000     C     FUDATT        ANDEQ     'S'
191900000000     C                   MOVEL     FUDDES        FUN008
192000000000     C                   END
192100000000     C                   END                                                    DO
192200000000     C                   ENDSR
192300000000     C*================================================================
192400000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
192500000000     C*================================================================
192600000000     C     FUNCON        BEGSR
192700000000     C*
192800000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
192900000000     C                   EXSR      FUNVAR
193000000000     C*
193100000000     C* PULISCO CAMPI DI LAVORO
193200000000     C                   Z-ADD     0             I                 5 0
193300000000     C                   Z-ADD     0             W                 5 0
193400000000     C*
193500000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
193600000000     C     FUNKEY        IFNE      *BLANKS
193700000000     C     1             DO        24            I
193800000000     C                   MOVEL     FUD(I)        FUDRIG
193900000000     C*
194000000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
194100000000     C     FUNKEY        IFEQ      FUDFUN
194200000000     C     FUDATT        ANDEQ     'S'
194300000000     C                   Z-ADD     I             W
194400000000     C                   END
194500000000     C                   END                                                    DO WHILE
194600000000     C*
194700000000     C* FUNZIONE NON ATTIVA (ERRORE)
194800000000     C     W             IFEQ      0
194900000000     C*
195000000000     C* EMISSIONE MESSAGGIO DI ERRORE
195100930201     C                   MOVEL     MSG(1)        DSMSMS
195200930201     C*
195300930201     C                   END                                                    W=0
195400000000     C                   END                                                    FUNKEY NE *BLANKS
195500000000     C*
195600000000     C* FUNZIONE ATTIVA (PROCEDI ...)
195700000000     C     W             IFNE      0
195800000000     C     FUNKEY        OREQ      *BLANKS
195900000000     C*
196000000000     C     FUNKEY        IFEQ      *BLANKS
196100000000     C                   EXSR      ENTGES
196200000000     C                   END
196300000000     C     FUNKEY        IFEQ      '01'
196400000000     C                   EXSR      F01GES
196500000000     C                   END
196600000000     C     FUNKEY        IFEQ      '03'
196700000000     C                   EXSR      F03GES
196800000000     C                   END
196900930202     C     FUNKEY        IFEQ      '06'
197000930202     C                   EXSR      F06GES
197100930202     C                   END
197200000000     C     FUNKEY        IFEQ      '08'
197300000000     C                   EXSR      F08GES
197400000000     C                   END
197500120411     C     FUNKEY        IFEQ      '10'
197600120411     C                   EXSR      F10GES
197700120411     C                   END
197800000000     C     FUNKEY        IFEQ      '12'
197900000000     C                   EXSR      F12GES
198000000000     C                   END
198100160923     C     FUNKEY        IFEQ      '17'
198200160923     C                   EXSR      F17GES
198300160923     C                   END
198400930201     C     FUNKEY        IFEQ      '21'
198500930201     C                   EXSR      F21GES
198600930201     C                   END
198700000000     C     FUNKEY        IFEQ      '22'
198800000000     C                   EXSR      F22GES
198900000000     C                   END
199000000000     C     FUNKEY        IFEQ      '24'
199100000000     C                   EXSR      F24GES
199200000000     C                   END
199300000000     C     FUNKEY        IFEQ      'HP'
199400000000     C                   EXSR      HELGES
199500000000     C                   END
199600000000     C*
199700000000     C                   END
199800000000     C*
199900000000     C                   ENDSR
200000000000     C*================================================================
200100000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
200200000000     C*================================================================
200300000000     C     FUNVAR        BEGSR
200400000000     C*
200500000000     C* INIZIALIZZA VARIABILE -FUNKEY-
200600000000     C*
200700000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
200800000000     C     KEY           IFEQ      ENTER
200900000000     C                   MOVEL     *BLANKS       FUNKEY
201000000000     C                   END
201100000000     C     KEY           IFEQ      F1
201200000000     C                   MOVEL     '01'          FUNKEY
201300000000     C                   END
201400000000     C     KEY           IFEQ      F2
201500000000     C                   MOVEL     '02'          FUNKEY
201600000000     C                   END
201700000000     C     KEY           IFEQ      F3
201800000000     C                   MOVEL     '03'          FUNKEY
201900000000     C                   END
202000000000     C     KEY           IFEQ      F4
202100000000     C                   MOVEL     '04'          FUNKEY
202200000000     C                   END
202300000000     C     KEY           IFEQ      F5
202400000000     C                   MOVEL     '05'          FUNKEY
202500000000     C                   END
202600000000     C     KEY           IFEQ      F6
202700000000     C                   MOVEL     '06'          FUNKEY
202800000000     C                   END
202900000000     C     KEY           IFEQ      F7
203000000000     C                   MOVEL     '07'          FUNKEY
203100000000     C                   END
203200000000     C     KEY           IFEQ      F8
203300000000     C                   MOVEL     '08'          FUNKEY
203400000000     C                   END
203500000000     C     KEY           IFEQ      F9
203600000000     C                   MOVEL     '09'          FUNKEY
203700000000     C                   END
203800000000     C     KEY           IFEQ      F10
203900000000     C                   MOVEL     '10'          FUNKEY
204000000000     C                   END
204100000000     C     KEY           IFEQ      F11
204200000000     C                   MOVEL     '11'          FUNKEY
204300000000     C                   END
204400000000     C     KEY           IFEQ      F12
204500000000     C                   MOVEL     '12'          FUNKEY
204600000000     C                   END
204700000000     C     KEY           IFEQ      F13
204800000000     C                   MOVEL     '13'          FUNKEY
204900000000     C                   END
205000000000     C     KEY           IFEQ      F14
205100000000     C                   MOVEL     '14'          FUNKEY
205200000000     C                   END
205300000000     C     KEY           IFEQ      F15
205400000000     C                   MOVEL     '15'          FUNKEY
205500000000     C                   END
205600000000     C     KEY           IFEQ      F16
205700000000     C                   MOVEL     '16'          FUNKEY
205800000000     C                   END
205900000000     C     KEY           IFEQ      F17
206000000000     C                   MOVEL     '17'          FUNKEY
206100000000     C                   END
206200000000     C     KEY           IFEQ      F18
206300000000     C                   MOVEL     '18'          FUNKEY
206400000000     C                   END
206500000000     C     KEY           IFEQ      F19
206600000000     C                   MOVEL     '19'          FUNKEY
206700000000     C                   END
206800000000     C     KEY           IFEQ      F20
206900000000     C                   MOVEL     '20'          FUNKEY
207000000000     C                   END
207100000000     C     KEY           IFEQ      F21
207200000000     C                   MOVEL     '21'          FUNKEY
207300000000     C                   END
207400000000     C     KEY           IFEQ      F22
207500000000     C                   MOVEL     '22'          FUNKEY
207600000000     C                   END
207700000000     C     KEY           IFEQ      F23
207800000000     C                   MOVEL     '23'          FUNKEY
207900000000     C                   END
208000000000     C     KEY           IFEQ      F24
208100000000     C                   MOVEL     '24'          FUNKEY
208200000000     C                   END
208300000000     C     KEY           IFEQ      ROLLUP
208400000000     C                   MOVEL     'RU'          FUNKEY
208500000000     C                   END
208600000000     C     KEY           IFEQ      HELP
208700000000     C                   MOVEL     'HP'          FUNKEY
208800000000     C                   END
208900000000     C                   ENDSR
209000000000     C*================================================================
209100000000     C* NOTE PER COMPILAZIONE SCHIERE
209200000000     C*================================================================
209300000000     C* -MSG-
209400000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
209500000000     C* (NON TOCCARE MSG,1)
209600000000     C*
209700000000     C* -CMD-
209800000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
209900000000     C*
210000000000     C* -FUD-
210100000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
210200000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
210300000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
210400000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
210500000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
210600000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
210700000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
210800000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
210900000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
211000000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
211100000000     C*
211200000000     C*================================================================
211300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
211400941115TNSDU2R- Tasto funzionale non gestito                                          1
211500941115TNSDU2R- Codice già esistente.                                                 2
211600941115TNSDU2R- Inserimento effettuato.                                               3
211700941115TNSDU2R- Modifica effettuata.                                                  4
211800941115TNSDU2R- Cancellazione effettuata.                                             5
211900941115TNSDU2R- Ripristino effettuato.                                                6
212000941115TNSDU2R- Tipo unità non accettabile.                                           7
212100941115TNSDU2R- Data documento obbligatoria in inserimento.                           8
212200941115TNSDU2R- Marca unità non accettabile.                                          9
212300941115TNSDU2R- Modello unità non accettabile.                                       10
212400941115TNSDU2R- Matricola unità non accettabile.                                     11
212500941115TNSDU2R- Data non accettabile.                                                12
212600941115TNSDU2R- Ditta non accettabile.                                               13
212700941115TNSDU2R- Ditta manutenzione non accettabile.                                  14
212800941115TNSDU2R- Magazzino ubicazione non accettabile.                                15
212900941115TNSDU2R- Richiesta gestione di record inesistente.                            16
213000941115TNSDU2R- N. documento obbligatorio in inserimento.                            17
213100020320TNSDU2R- Numeratore non trovato.                                              18
213200020320TNSDU2R- Numeratore già impegnato riprovare più tardi.                        19
213300020320TNSDU2R- Oltrepassato numero massimo di codici.                               20
213400020320TNSDU2R- Esiste già una unità con lo stesso modello e la stessa matricola.    21
213500020320TNSDU2R- Secondo livello di raggruppamento non accettabile.                   22
213600020320TNSDU2R- Cliente non accettabile.                                             23
213700031204TNSDU2R- Hai indicato il tipo di PC ???????????????????                       24
213800091113TNSDU2R- LA matricola deve essere univoca per Tipo Unità!                     25
213900110401Se unità in proprietà alla 923 non posso accettare magazzino ubicazione "990" 26
214000120521Numero ordine obbligatorio in inserimento/copia                               27
214100160923TNSDU2R- Storicizzazione effettuata.                                          28
214200160923Il magazzino di ubicazione non permette la storicizzazione di questa unità    29
214300000000** ======== SCHIERA: CMD  (COMANDI)                        ================
214400000000WRKSPLF
214500000000
214600000000
214700000000
214800000000
214900000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
215000930201                   -        (S/N)=N                        funz.01¬
215100000000                   -        (S/N)=N                        funz.02¬
215200000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
215300000000                   -        (S/N)=N                        funz.04¬
215400000000                   -        (S/N)=N                        funz.05¬
215500930202F06=Aggiornamento  -        (S/N)=S                        funz.06¬
215600930201                   -        (S/N)=N                        funz.07¬
215700971013F08=Movimenti      -        (S/N)=S                        funz.08¬
215800000000                   -        (S/N)=N                        funz.09¬
215900120411F10=Sblocca ORDINE -        (S/N)=S                        funz.10¬
216000000000                   -        (S/N)=N                        funz.11¬
216100930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
216200000000                   -        (S/N)=N                        funz.13¬
216300000000                   -        (S/N)=N                        funz.14¬
216400000000                   -        (S/N)=N                        funz.15¬
216500000000                   -        (S/N)=N                        funz.16¬
216600160923F17=STORICIZZA     -        (S/N)=N                        funz.17¬
216700000000                   -        (S/N)=N                        funz.18¬
216800000000                   -        (S/N)=N                        funz.19¬
216900000000                   -        (S/N)=N                        funz.20¬
217000971013F21=Stampa         -        (S/N)=S                        funz.21¬
217100940105F22=Gestione stampe-        (S/N)=N                        funz.22¬
217200000000                   -        (S/N)=N                        funz.23¬
217300000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
