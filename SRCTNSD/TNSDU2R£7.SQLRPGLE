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
001200930709     FUNANA01L  UF A E           K DISK
001300020320     funana08l  if   e           k disk    rename(unana000:unana8)
001400091113     funana02l  if   e           k disk    rename(unana000:unana2)
001500020529     FAZORG21l  IF   E           K DISK
001600020529     fazorg01l  if   e           k disk
001700020319
001800020319      *------------------------------------------------------------------------*
001900020319      * SCHIERE
002000020319      *------------------------------------------------------------------------*
002100020319      * MESSAGGI
002200120521     D MSG             S             78    DIM(27) CTDATA PERRCD(1)
002300020319      * COMANDI
002400000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002500020319      * DESCRIZIONI TASTI FUNZIONALI
002600000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002700020319      * TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002800000000     D FUV             S             19    DIM(24)
002900020319
003000020322     d wvdtip          s                   like(unatip)
003100020320     d kunmod          s                   like(unamod)
003200020320     d kunmat          s                   like(unamat)
003300091113     d kuntip          s                   like(unatip)
003400020403     d savmod          s                   like(unamod)
003500020403     d savmat          s                   like(unamat)
003600020321     d paxccc          s                   LIKE(ACOkcc)
003700020321     d paxdmt          s                   LIKE(ACOrag)
003800020405     d sav_dsunit      s                   like(dsunit)
003900020321     d PAXdut          s             30
004000020321     d PAXdit          s              3
004100020321     d PAXflr          s             90
004200020321     d PAXsta          s              1  0
004300020709     d codut           s              1  0 Inz(1)
004400021104     d Wmsginfo        s              1    Inz('0')
004500120412     d alfaord         s              6
004600130404     d wmag            s              3
004700130404     d wdmag           s             20
004800130409     d wtorg           s              3
004900151120     d wtrov           s              1
005000151120     d wrkgetlista     s           4096    varying
005100020320
005200020319      *------------------------------------------------------------------------*
005300020319      * INPUT
005400020319      *------------------------------------------------------------------------*
005500020319      * RIDEFINIZIONE SCHIERA -FUD-
005600000000     D FUDDS           DS
005700000000     D  FUDDE1                 1      1
005800000000     D  FUDFUN                 2      3
005900000000     D  FUDDE2                 4     19
006000000000     D  FUDFI0                20     34
006100000000     D  FUDATT                35     35
006200000000     D  FUDFI1                36     80
006300000000     D  FUDDES                 1     19
006400000000     D  FUDRIG                 1     80
006500020319      *------------------
006600020319      * DS "XSRDA8" - CONTROLLA DATA (8)
006700020319      *------------------
006800971010     D WLBDA8          DS
006900971010     D  G08DAT                 1      8  0
007000971010     D  G08INV                 9     16  0
007100971010     D  G08ERR                17     17
007200971010     D  G08TGI                18     22  0
007300020320
007400020319      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
007500930201     D DSMSG         E DS
007600020319      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
007700930709     D DSUNIT        E DS
007800070503      * Parametri ricevuti
007900070503     d UNANAds       e ds                  extname(UNANA00F)
008000070503     d                                     prefix(UIM:3)
008100070503     d UNMOVds         ds
008200070509     d  UIMdoc                             like(UNMdoc)
008300070509     d  UIMddo                             like(UNMddo)
008400020319      * DS PER GESTIONE TASTI FUNZIONALI
008500930201     D DSKEY         E DS
008600930201     D DEVDS1          DS
008700930201     D  KEY                  369    369
008800020319      * ARCHITETTURA
008900930201     D KPJBA         E DS
009000020320
009100020319     d trul33ds      e ds
009200020320     d tibs02ds      e ds
009300020327     d tibs69ds      e ds
009400020327     d ds_cnaco      e ds                  extname(cnaco00f)
009500020327     d ds_cnind      e ds                  extname(cnind00f)
009600020327     d ds_cnclp      e ds                  extname(cnclp00f)
009700020327     d ds_fncls      e ds                  extname(fncls00f)
009800020709
009900020709     d ddatiute      e ds
010000020709     d azuteds       e ds                  extname(azute00f)
010100020709     d tibs34ds      e ds                  inz
010200021104
010300021104     d dunt          e ds
010400130404     d tnsdmagds     e ds
010500070503
010600070503     d Status         sds
010700070503     d   SDSprm          *parms
010800020319
010900020319      *------------------------------------------------------------------------*
011000020319      * MAIN LINE
011100020319      *------------------------------------------------------------------------*
011200000000     C                   EXSR      RUTINZ
011300000000     C*
011400000000     C* CICLO FINO A CHE:
011500000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
011600000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
011700930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
011800070509     c                   DOW            F03L00 = *off
011900070509     c                             and  F12L00 = *off
012000070509     c                             and  FLFINE = *off
012100000000     C*
012200000000     C* EMETTE FORMATO VIDEO
012300000000     C                   EXSR      BUTFMR
012400000000     C*
012500000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
012600000000     C                   EXSR      FUNCON
012700000000     C*
012800070509     C                   ENDDO                                                  <-- 2 ---
012900000000     C*
013000020320
013100020320     c                   clear                   trul33ds
013200020320     c                   eval      i33tla = 'C'
013300020320     c                   movel(p)  trul33ds      kpjbu
013400020320     c                   call      'TRUL33R'
013500020320     c                   parm                    kpjba
013600020320
013700940105     C                   MOVEL     DSUNIT        KPJBU
013800000000     C                   SETON                                        LR
013900020320
014000000000     C*FINE MAIN DEL PGM
014100000000     C*================================================================
014200000000     C* EMISSIONE FORMATO A VIDEO
014300000000     C*================================================================
014400000000     C     BUTFMR        BEGSR
014500930204     C*
014600930204     C* WRITE RECORD TESTATA
014700941115     C                   WRITE     SDU2D01
014800930205     C*
014900930205     C* VISUALIZZAZIONE MESSAGGIO
015000930205     C                   EXSR      SNDMSG
015100930204     C*
015200930204     C* WRITE RECORD FUNZIONI
015300941115     C                   WRITE     SDU2D02
015400000000     C*
015500930204     C* EXFMT RECORD DETTAGLIO
015600941115     C                   EXFMT     SDU2D03
015700000000     C*
015800000000     C* AZZERO FLAG DI ERRORE LIVELLO
015900000000     C                   MOVEL     '0'           ERRL00
016000000000     C*
016100000000     C* AZZERO FLAG DI ERRORE
016200930709     C                   SETOFF                                       606162
016300020320     C                   SETOFF                                       636465
016400930709     C                   SETOFF                                       666768
016500930709     C                   SETOFF                                       697071
016600930709     C                   SETOFF                                       727374
016700020320     C                   SETOFF                                       757677
016800120522     C                   SETOFF                                         79
016900000000     C*
017000000000     C                   ENDSR
017100000000     C*================================================================
017200000000     C* SNDMSG - MANDA MESSAGGIO
017300000000     C*================================================================
017400000000     C     SNDMSG        BEGSR
017500000000     C*
017600000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
017700000000     C                   MOVEL     '1'           *IN40
017800941115     C                   WRITE     SDU2DMC
017900000000     C                   MOVEL     '0'           *IN40
018000000000     C*
018100000000     C* VISUALIZZAZIONE MESSAGGIO
018200000000     C                   Z-ADD     0             MSGULT            4 0
018300930202     C                   CALL      'XMSG'        XMSGPA
018400930201     C     DSMSER        IFEQ      0
018500930201     C                   MOVEL     DSMSKE        MSGKEY
018600000000     C                   END
018700000000     C                   Z-ADD     1             MSGULT
018800941115     C                   WRITE     SDU2DMS                                      SFL MESS
018900941115     C                   WRITE     SDU2DMC                                      CONTR MESS
019000000000     C*
019100000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
019200930201     C                   MOVEL     *BLANKS       DSMSMS
019300000000     C*
019400000000     C                   ENDSR
019500000000     C*================================================================
019600000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
019700000000     C*================================================================
019800000000     C     ENTGES        BEGSR
019900930201     C*
020000930201     C* CONTROLLA I CAMPI IMMESSI
020100930201     C                   EXSR      DECODI
020200930202     C*
020300930202     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
020400930709     C     DSUOP1        IFEQ      'O05'
020500930202     C                   MOVEL     '1'           FLFINE
020600930202     C                   END
020700000000     C*
020800000000     C                   ENDSR
020900000000     C*================================================================
021000930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
021100000000     C*================================================================
021200000000     C     F01GES        BEGSR
021300000000     C                   ENDSR
021400000000     C*================================================================
021500930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
021600000000     C*================================================================
021700000000     C     F03GES        BEGSR
021800000000     C*
021900930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
022000000000     C                   MOVEL     '1'           F03L00
022100930202     C* IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
022200930709     C                   MOVEL     '1'           DSUF03
022300000000     C*
022400000000     C                   ENDSR
022500930201     C*================================================================
022600930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
022700930201     C*================================================================
022800930201     C     F06GES        BEGSR
022900930201     C*
023000930201     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
023100930709     C     DSUOP1        IFEQ      'F06'
023200070509     c     DSUop1        oreq      'F6N'
023300971015     C     DSUOP1        OREQ      'O03'
023400930201     C                   EXSR      INSERI
023500930201     C                   END
023600930709     C     DSUOP1        IFEQ      'O02'
023700930202     C                   EXSR      MODIFI
023800930201     C                   END
023900930709     C     DSUOP1        IFEQ      'O04'
024000930201     C                   EXSR      CANCEL
024100930201     C                   END
024200930709     C     DSUOP1        IFEQ      'O07'
024300930201     C                   EXSR      RIPRIS
024400930201     C                   END
024500930201     C*
024600930201     C                   ENDSR
024700000000     C*================================================================
024800120411     C* F10GES -GESTIONE TASTO FUNZIONALE F10 -
024900000000     C*================================================================
025000120411     C     F10GES        BEGSR
025100120521     c                   seton                                        1112
025200000000     C                   ENDSR
025300000000     C*================================================================
025400930709     C* F08GES -GESTIONE TASTO FUNZIONALE F08 - GEST.MOVIMENTI
025500000000     C*================================================================
025600000000     C     F08GES        BEGSR
025700930201     C*
025800930709     C* LANCIO IL PGM PER LA GESTIONE DEI MOVIMENTI
025900940105     C                   UNLOCK    UNANA01L                                     *SBLOCCA IL REC.ANAG
026000941115     C                   MOVEL     'TNSDU4R'     WPGM              8
026100930201     C                   EXSR      LANPGM
026200020405      * riaggancio il file
026300020405     c                   eval      unatip = dsutip
026400020405     c                   eval      unacod = dsucod
026500150604     C* se non siamo in visualizzazione, alloco il rcd
026600150604     C                   IF        DSUOP1 <> 'O05'
026700150604     c     keyuna        chain     unana01l
026800150604     C* se siamo in visualizzazione, NON alloco il rcd
026900150604     C                   ELSE
027000150604     c     keyuna        chain(n)  unana01l
027100150604     C                   ENDIF
027200140509     c                   exsr      azzvid
027300140509     c                   exsr      recvid
027400930201     C*
027500000000     C                   ENDSR
027600000000     C*================================================================
027700930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
027800000000     C*================================================================
027900000000     C     F12GES        BEGSR
028000000000     C*
028100000000     C* IMPOSTO FLAG DI LIVELLO
028200000000     C                   MOVEL     '1'           F12L00
028300070503      * IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
028400070509if  1c                   if            (DSUop1  = 'F06'
028500070509     c                              or  DSUop1  = 'F6N')
028600070503     c                             and  SDSprm  > 1
028700070503     C                   MOVEL     '2'           DSUF03
028800070503e   1c                   endif
028900000000     C*
029000000000     C                   ENDSR
029100930201     C*================================================================
029200930709     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA ANAGR.UNITA
029300930201     C*================================================================
029400930201     C     F21GES        BEGSR
029500020405
029600020405      * salvo la ds prima di richiamare un altro pgm
029700020405     c                   eval      sav_dsunit = dsunit
029800020405
029900930201     C*
030000930709     C* LANCIO IL PGM PER LA STAMPA DELL'ANAGRAFICA UNITA
030100940105     C                   MOVEL     DSUNIT        KPJBU
030200941115     C                   CALL      'TNSDU6R'
030300930209     C                   PARM                    KPJBA
030400940105     C                   MOVEL     KPJBU         DSUNIT
030500940105     C*
030600940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
030700020405     C**!!!DSUF03        IFEQ      '1'
030800020405     C**!!!              MOVEL     '1'           F03L00
030900020405     C**!!!              MOVEL     '1'           ERRL00
031000020405     C**!!!              ENDIF
031100020405
031200020405      * reimposto la ds dopo aver richiamato un altro programma
031300020405     c                   eval      dsunit = sav_dsunit
031400020405
031500930201     C*
031600930201     C                   ENDSR
031700000000     C*================================================================
031800930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
031900000000     C*================================================================
032000000000     C     F22GES        BEGSR
032100000000     C*
032200000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
032300000000     C                   Z-ADD     80            LENGH            15 5
032400000000     C                   CALL      'QCAEXEC'
032500000000     C                   PARM                    CMD(1)
032600000000     C                   PARM                    LENGH
032700000000     C*
032800000000     C                   ENDSR
032900000000     C*================================================================
033000930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
033100000000     C*================================================================
033200000000     C     F24GES        BEGSR
033300000000     C*
033400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
033500000000     C                   EXSR      FUNGES
033600000000     C*
033700000000     C                   ENDSR
033800000000     C*================================================================
033900000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
034000000000     C*================================================================
034100000000     C     HELGES        BEGSR
034200000000     C                   ENDSR
034300930201     C*================================================================
034400930201     C* INSERI - FUNZIONE DI INSERIMENTO
034500930201     C*================================================================
034600930201     C     INSERI        BEGSR
034700930201     C*
034800930201     C* CONTROLLA I CAMPI IMMESSI
034900930201     C                   EXSR      DECODI
035000930201     C*
035100930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
035200020320
035300020320      * aggiorno il numeratore
035400151120     c* se il numero già presente sul file--> ne stacco un altro
035500151120     c                   eval      wtrov='1'
035600151120    1c                   dow       errl00 = *off and wtrov='1'
035700020320     c                   clear                   trul33ds
035800020320     c                   eval      i33ope = 0
035900020320     c                   eval      i33cnu = 22
036000020320     c                   eval      i33num = 1
036100020320     c                   eval      i33unp = vidcod
036200020320     c                   movel(p)  trul33ds      kpjbu
036300020320     c                   call      'TRUL33R'
036400020320     c                   parm                    kpjba
036500020320     c                   movel(p)  kpjbu         trul33ds
036600151120     c
036700151120    2c                   if        o33err = *zeros
036800151120     c
036900151120     c* Se NON presente posso uscire dal ciclo
037000151120      /free
037100151120        wrkgetlista ='select unacod from unana00f where unacod=' +
037200151120                                                   %editc(o33nri:'X')   ;
037300151120       EXEC sql  prepare s1 from : wrkgetlista ;
037400151120       EXEC sql  declare a1 cursor for s1      ;
037500151120       EXEC sql  open    a1                    ;
037600151120
037700151120       EXEC sql fetch next from a1 into :vidcod ;
037800151120       if        sqlcod=100 ;
037900151120         vidcod = o33nri  ;
038000151120         wtrov='0'        ;
038100151120    3  endif              ;
038200151120       EXEC sql  close   a1                    ;
038300151120
038400151120      //end-free
038500151120
038600151120    2c                   endif
038700151120     c
038800020320     c                   if        o33err = 01
038900020320     c                   eval      errl00 = *on
039000020320     c                   eval      *in61 = *on
039100020320     c                   eval      dsmsms = msg(18)
039200020320     c                   endif
039300020320     c                   if        o33err = 02
039400020320     c                   eval      errl00 = *on
039500020320     c                   eval      *in61 = *on
039600020320     c                   eval      dsmsms = msg(19)
039700020320     c                   endif
039800020320     c                   if        o33err = 03
039900020320     c                   eval      errl00 = *on
040000020320     c                   eval      *in61 = *on
040100020320     c                   eval      dsmsms = msg(20)
040200020320     c                   endif
040300151120     c
040400151120    1c                   enddo
040500020320
040600151120     c                   if        errl00 = *off
040700020320
040800930709     C* IMPOSTO IL RECORD DAL VIDEO
040900930709     C                   EXSR      VIDREC
041000971014     C                   MOVEL     *BLANKS       UNAFTR
041100971014     C                   Z-ADD     *ZEROS        UNADTR
041200971014     C                   MOVEL     *BLANKS       UNAATB
041300930709     C                   WRITE     UNANA000
041400940504     C                   EXSR      SCRMOV                                       *SCRIVE 1° MOVIMENTO
041500940504     C* AZZERA IL CAMPO MATRICOLA PER EVITARE DOPPIO INSERIMENTO
041600940504     C                   MOVEL     *BLANKS       VIDMAT
041700941215     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
041800941215     C                   MOVEL     '1'           FLFINE
041900930518     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
042000020320     C                   MOVEL     MSG(3)        DSumsg
042100930201     C*
042200020320     c                   endif
042300930201     C                   END                                                    <-- 1 ---
042400930201     C*
042500930201     C                   ENDSR
042600930201     C*================================================================
042700930201     C* MODIFI - FUNZIONE DI MODIFICA
042800930201     C*================================================================
042900930201     C     MODIFI        BEGSR
043000930201     C*
043100930201     C* CONTROLLA I CAMPI IMMESSI
043200930201     C                   EXSR      DECODI
043300930201     C*
043400930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
043500930201     C*
043600930709     C* IMPOSTO IL RECORD DAL VIDEO E RISCRIVO
043700930709     C                   EXSR      VIDREC
043800971014     C                   MOVEL     *BLANKS       UNAFTR
043900971014     C                   MOVEL     *BLANKS       UNAATB
044000930709     C                   UPDATE    UNANA000
044100930518     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
044200930518     C                   MOVEL     '1'           FLFINE
044300930518     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI MODIF. EFFETTUATA
044400930709     C                   MOVEL     MSG(4)        DSUMSG
044500930201     C*
044600930201     C                   END                                                    <-- 1 ---
044700930201     C*
044800930201     C                   ENDSR
044900930201     C*================================================================
045000930201     C* CANCEL - FUNZIONE DI CANCELLAZIONE
045100930201     C*================================================================
045200930201     C     CANCEL        BEGSR
045300971013     C*
045400971013     C* ANNULLA MOVIMENTI UNITA'
045500020322     c                   z-add     unatip        kumtip
045600971013     C                   Z-ADD     UNACOD        KUMCOD
045700971013     C     KEYMOV        SETLL     UNMOV01L
045800971013     C     KEYMOV        READE     UNMOV01L                               97
045900971013DO  1C     *IN97         DOWEQ     *OFF
046000100907IF  2C     UNMDTR        IFEQ      *ZEROS
046100971014     C                   DELETE    UNMOV000
046200971014X   2C                   ELSE                                                   *GIA' TRASMESSO
046300971014     C                   MOVEL     'A'           UNMATB
046400971013     C                   UPDATE    UNMOV000
046500971014E   2C                   ENDIF
046600971013     C     KEYMOV        READE     UNMOV01L                               97
046700971013E   1C                   ENDDO
046800971013     C* ANNULLA UNITA'
046900101027IF  1C**   UNADTR        IFEQ      *ZEROS                                       *MAI TRASMESSO
047000971014     C                   DELETE    UNANA000
047100101027X   1C**                 ELSE                                                   *GIA' TRASMESSO
047200101027     C**                 MOVEL     *BLANKS       UNAFTR
047300101027     C**                 MOVEL     'A'           UNAATB
047400101027     C**                 UPDATE    UNANA000
047500101027E   1C**                 ENDIF
047600930201     C*
047700930201     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
047800930201     C                   MOVEL     '1'           FLFINE
047900930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI CANC. EFFETTUATA
048000930709     C                   MOVEL     MSG(5)        DSUMSG
048100930201     C*
048200930201     C                   ENDSR
048300930201     C*================================================================
048400930201     C* RIPRIS - FUNZIONE DI RIPRISTINO
048500930201     C*================================================================
048600930201     C     RIPRIS        BEGSR
048700930202     C*
048800930709     C* RIPRISTINO IL RECORD
048900930709     C                   MOVEL     *BLANKS       UNAFTR
049000971014     C                   MOVEL     *BLANKS       UNAATB
049100930709     C                   UPDATE    UNANA000
049200930202     C*
049300930202     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
049400930202     C                   MOVEL     '1'           FLFINE
049500930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI RIPR. EFFETTUATO
049600930709     C                   MOVEL     MSG(6)        DSUMSG
049700930202     C*
049800930201     C                   ENDSR
049900930202     C*================================================================
050000930709     C* SCRREC - SCRITTURA DEL RECORD DEL FILE
050100930202     C*================================================================
050200930709     C     SCRREC        BEGSR
050300930202     C*
050400020322     c                   z-add     wvdtip        unatip
050500930709     C                   Z-ADD     VIDCOD        UNACOD
050600930709     C     KEYUNA        CHAIN     UNANA01L                           99
050700930202     C     *IN99         IFEQ      '0'
050800930709     C                   EXSR      VIDREC
050900930709     C                   UPDATE    UNANA000
051000930202     C                   ELSE
051100930709     C                   EXSR      VIDREC
051200930709     C                   WRITE     UNANA000
051300930202     C                   END
051400930202     C*
051500930202     C                   ENDSR
051600940504     C*================================================================
051700940504     C*  SCRMOV - SCRIVE IL MOVIMENTO DI CARICO
051800940504     C*================================================================
051900940504     C     SCRMOV        BEGSR
052000940504     C*
052100020326     C                   Z-ADD     UNAtip        UNMtip
052200940504     C                   Z-ADD     UNACOD        UNMCOD
052300110407     c                   movel     unamat        unmmat
052400940504     C                   MOVEL     'CARI'        UNMCAU
052500940504     C                   MOVEL     'E'           UNMTMO
052600971010     C                   Z-ADD     DATCOR        UNMDTM
052700941216     C                   Z-ADD     20000         UNMPRG
052800940504     C                   Z-ADD     UNAMAG        UNMMAG
052900940504     C                   Z-ADD     *ZEROS        UNMMGC
053000940504     C                   MOVEL     'CARICO'      UNMDES
053100940504     C                   Z-ADD     VIDDOC        UNMDOC
053200971010     C                   Z-ADD     VIDDDO        G08DAT
053300971010     C                   Z-ADD     *ZEROS        G08INV
053400971010     C                   MOVEL     '0'           G08ERR
053500971010     C                   CALL      'XSRDA8'
053600971010     C                   PARM                    WLBDA8
053700971010     C                   Z-ADD     G08INV        UNMDDO
053800940504     C                   Z-ADD     *ZEROS        UNMCNT
053900020320     c                   eval      unmksc = unaksc
054000940504     C                   WRITE     UNMOV000
054100940504     C*
054200940504     C                   ENDSR
054300930201     C*================================================================
054400930201     C* LANPGM - LANCIA UN PROGRAMMA
054500930201     C*================================================================
054600930201     C     LANPGM        BEGSR
054700020405
054800020405      * salvo la ds prima di richiamare un altro pgm
054900020405     c                   eval      sav_dsunit = dsunit
055000020405
055100930201     C*
055200930201     C* IMPOSTO PARAMETRI PER IL PGM
055300930709     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
055400930709     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
055500930709     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
055600930709     C                   Z-ADD     UNATIP        DSUTIP                         TIPO UNITA
055700930709     C                   Z-ADD     UNACOD        DSUCOD                         COD.UNITA
055800930716     C                   Z-ADD     *ZEROS        DSUMAG                         MAGAZZINO
055900930712     C                   Z-ADD     *ZEROS        DSUPRG                         PROGRESS. MOV.
056000930906     C                   Z-ADD     *ZEROS        DSUDTM                         DATA MOVIMENTO
056100930201     C*
056200930201     C* LANCIO PROGRAMMA
056300940105     C                   MOVEL     DSUNIT        KPJBU
056400930312     C                   CALL      WPGM
056500940105     C                   PARM                    KPJBA
056600940105     C                   MOVEL     KPJBU         DSUNIT
056700930201     C*
056800930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
056900020405     C     DSUF03        IFEQ      '1'
057000020405     C**!!!              MOVEL     '1'           F03L00
057100020405     C**!!!              MOVEL     '1'           ERRL00
057200930201     C                   ELSE
057300930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
057400930709     C                   MOVEL     DSUMSG        DSMSMS
057500930201     C                   END
057600020405
057700020405      * reimposto la ds dopo aver richiamato un altro programma
057800020405     c                   eval      dsunit = sav_dsunit
057900020405
058000930201     C*
058100930201     C                   ENDSR
058200000000     C*================================================================
058300930201     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
058400000000     C*================================================================
058500000000     C     DECODI        BEGSR
058600020403
058700020403      * faccio i controlli solo per inserimento/manutenzione/copia
058800020403     c                   if        dsuop1 = 'F06' or dsuop1 = 'O02' or
058900070509     c                             DSUop1 = 'F6N' or
059000020403     c                             dsuop1 = 'O03'
059100020320
059200020320      * CONTROLLO E DECODIFICO IL TIPO UNITA
059300930709     C                   EXSR      CHKTIP
059400021104     c                   if        errl00 = *on and wmsginfo = *Off
059500930709     C                   MOVEL     MSG(7)        DSMSMS
059600930709     C                   SETON                                        60
059700930709     C                   END
059800021104      * Emetto msg. info
059900021104     c                   If        Wmsginfo = *On and *In78 = *Off
060000021104     c                   Movel     Msg(24)       DsmSms
060100091113     c                   Eval      *In65 = *On
060200021104     c                   Eval      *In78 = *On
060300021104     c                   EndIf
060400020321
060500020320      * CONTROLLO LA MARCA
060600020320     c                   if        errl00 = *off
060700930709     C     VIDMAR        IFEQ      *BLANKS
060800930709     C                   MOVEL     '1'           ERRL00
060900930709     C                   MOVEL     MSG(9)        DSMSMS
061000930709     C                   SETON                                        62
061100930709     C                   END
061200930709     C                   END
061300120521     c
061400120521     c* Numero ordine obbligatorio
061500120521      * faccio i controlli solo per inserimento/copia
061600120521     c                   if        dsuop1 = 'F06' or
061700120521     c                             DSUop1 = 'F6N' or
061800120521     c                             dsuop1 = 'O03'
061900120521     c                   if        vidord<=*zeros  and errl00=*off
062000120521     C                   MOVEL     '1'           ERRL00
062100120521     C                   MOVEL     MSG(27)       DSMSMS
062200120521     C                   SETON                                        79
062300120521     c                   leavesr
062400120521     C                   END
062500120521     C                   END
062600020320
062700020320      * CONTROLLO IL MODELLO
062800020320     c                   if        errl00 = *off
062900930709     C     VIDMOD        IFEQ      *BLANKS
063000930709     C                   MOVEL     '1'           ERRL00
063100930709     C                   MOVEL     MSG(10)       DSMSMS
063200930709     C                   SETON                                        63
063300930709     C                   END
063400930709     C                   END
063500020320
063600020320      * CONTROLLO LA MATRICOLA
063700020320     c                   if        errl00 = *off
063800930709     C     VIDMAT        IFEQ      *BLANKS
063900930709     C                   MOVEL     '1'           ERRL00
064000930709     C                   MOVEL     MSG(11)       DSMSMS
064100930709     C                   SETON                                        64
064200930709     C                   END
064300930709     C                   END
064400020403
064500020403     c                   endif
064600020321
064700020321      * controllo se già inserito un codice con stesso modello e matricola
064800091113     c                   if        errl00 = *off
064900020321     c                   eval      kunmod = vidmod
065000020321     c                   eval      kunmat = vidmat
065100020321     c     keyun8        setll     unana08l
065200020403      * immissione/copia
065300020403     c                   if        dsuop1 = 'F06' or dsuop1 = 'O03'             --- 1 -->
065400070509     c                                            or DSUop1 = 'F6N'
065500020321     c                   if        %equal(unana08l)                             --- 2 -->
065600020321     c                   eval      errl00 = *on
065700020321     c                   eval      *in63 = *on
065800020321     c                   eval      *in64 = *on
065900020321     c                   eval      dsmsms = msg(21)
066000020321     c                   endif                                                  <-- 2 ---
066100020403      * manutenzione
066200020403     c                   else                                                   <-- 1 -->
066300020403     c                   if        vidmod <> savmod or vidmat <> savmat         --- 2 -->
066400020403     c                   if        %equal(unana08l)                             --- 3 -->
066500020403     c                   eval      errl00 = *on
066600020403     c                   eval      *in63 = *on
066700020403     c                   eval      *in64 = *on
066800020403     c                   eval      dsmsms = msg(21)
066900020403     c                   endif                                                  <-- 3 ---
067000020403     c                   endif                                                  <-- 2 ---
067100020403     c                   endif                                                  <-- 1 ---
067200091113     c                   endif                                                  <-- 1 ---
067300091113
067400091113      * 13 11 09 EDPES --> da oggi è obbligatoria la matricola univoca
067500091113      *  all'interno dell'unità
067600091113     c                   if        errl00 = *off
067700091113     c                   eval      kuntip = wvdtip
067800091113     c                   eval      kunmat = vidmat
067900091113     c     kana2         setll     unana02l
068000091113      * immissione/copia
068100091113     c                   if        dsuop1 = 'F06' or dsuop1 = 'O03'             --- 1 -->
068200091113     c                                            or DSUop1 = 'F6N'
068300091113     c                   if        %equal(unana02l)                             --- 2 -->
068400091113     c                   eval      errl00 = *on
068500091113     c                   eval      *in64 = *on
068600091113     c                   eval      dsmsms = msg(25)
068700091113     c                   endif                                                  <-- 2 ---
068800091113      * manutenzione
068900091113     c                   else                                                   <-- 1 -->
069000091113     c                   if        vidmat <> savmat                             --- 2 -->
069100091113     c                   if        %equal(unana02l)                             --- 3 -->
069200091113     c                   eval      errl00 = *on
069300091113     c                   eval      *in64 = *on
069400091113     c                   eval      dsmsms = msg(25)
069500091113     c                   endif                                                  <-- 2 ---
069600091113     c                   endif                                                  <-- 1 ---
069700091113     c                   endif                                                  <-- 2 ---
069800091113     c                   endif                                                  <-- 2 ---
069900020321
070000020320      * controllo il 2° livello di raggruppamento
070100020320     c                   if        errl00 = *off
070200020321     c                   if        (vidrg2 = *blanks or vidrg2 = *zeros) and
070300070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
070400070509     c                                             or DSUop1 = 'F6N')
070500020320     c                   eval      errl00 = *on
070600020320     c                   eval      *in65 = *on
070700020320     c                   eval      dsmsms = msg(22)
070800020320     c                   endif
070900020320     c                   if        errl00 = *off
071000020320     c                   exsr      chkrg2
071100020320     c                   if        errl00 = *on
071200020320     c                   eval      *in65 = *on
071300020320     c                   eval      dsmsms = msg(22)
071400020320     c                   endif
071500020320     c                   endif
071600020320     c                   endif
071700020320
071800020320      * CONTROLLO LA DATA DI CONSEGNA
071900020320     c                   if        errl00 = *off
072000930709     C     VIDDCN        IFNE      *ZEROS
072100971010     C                   Z-ADD     VIDDCN        WDAT              8 0
072200930709     C                   EXSR      CHKDAT
072300971013     C                   Z-ADD     WDAT          VIDDCN
072400930709     C     ERRL00        IFEQ      '1'
072500930709     C                   SETON                                        66
072600930709     C                   END
072700930709     C                   END
072800020320     c                   if        viddcn = *zeros and
072900070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
073000070509     c                                             or DSUop1 = 'F6N')
073100020320     c                   eval      errl00 = *on
073200020320     c                   eval      *in66 = *on
073300020320     c                   eval      dsmsms = msg(12)
073400020320     c                   endif
073500930709     C                   END
073600020320
073700020320      * CONTROLLO LA DATA DI SCADENZA MANUTENZIONE
073800020320     c                   if        errl00 = *off
073900930709     C     VIDDSM        IFNE      *ZEROS
074000930709     C                   Z-ADD     VIDDSM        WDAT
074100930709     C                   EXSR      CHKDAT
074200971013     C                   Z-ADD     WDAT          VIDDSM
074300930709     C     ERRL00        IFEQ      '1'
074400930709     C                   SETON                                        67
074500930709     C                   END
074600930709     C                   END
074700020320     c                   if        viddsm = *zeros and
074800070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
074900070509     c                                             or DSUop1 = 'F6N')
075000020320     c                   eval      errl00 = *on
075100020320     c                   eval      *in67 = *on
075200020320     c                   eval      dsmsms = msg(12)
075300020320     c                   endif
075400930709     C                   END
075500020320
075600020320      * CONTROLLO LA DATA DI SCADENZA GARANZIA
075700020320     c                   if        errl00 = *off
075800930709     C     VIDDSG        IFNE      *ZEROS
075900930709     C                   Z-ADD     VIDDSG        WDAT
076000930709     C                   EXSR      CHKDAT
076100971013     C                   Z-ADD     WDAT          VIDDSG
076200930709     C     ERRL00        IFEQ      '1'
076300930709     C                   SETON                                        68
076400930709     C                   END
076500930709     C                   END
076600020320     c                   if        viddsg = *zeros and
076700070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
076800070509     c                                             or DSUop1 = 'F6N')
076900020320     c                   eval      errl00 = *on
077000020320     c                   eval      *in68 = *on
077100020320     c                   eval      dsmsms = msg(12)
077200020320     c                   endif
077300930709     C                   END
077400020320
077500020320      * CONTROLLO LA DATA DI SCADENZA LEASING
077600020320     c                   if        errl00 = *off
077700930709     C     VIDDSL        IFNE      *ZEROS
077800930709     C                   Z-ADD     VIDDSL        WDAT
077900930709     C                   EXSR      CHKDAT
078000971013     C                   Z-ADD     WDAT          VIDDSL
078100930709     C     ERRL00        IFEQ      '1'
078200930709     C                   SETON                                        69
078300930709     C                   END
078400930709     C                   END
078500930709     C                   END
078600020320
078700020320      * CONTROLLO DITTA PROPRIETA'
078800971126IF  1C     ERRL00        IFEQ      '0'
078900971126IF  2C     VIDSIS        IFEQ      *BLANKS
079000971126IF  3C     DSUOP1        IFEQ      'F06'
079100070509     c     DSUop1        oreq      'F6N'
079200971015     C     DSUOP1        OREQ      'O03'
079300940504     C                   MOVEL     '1'           ERRL00
079400940504     C                   MOVEL     MSG(13)       DSMSMS
079500940504     C                   SETON                                        70
079600971126E   3C                   ENDIF
079700971126X   2C                   ELSE
079800971126     C                   MOVE      VIDSIS        WMAG
079900130409     c* qualsiasi codice dell'organigranna di appoggio
080000130410     c                   eval      wtorg='SD '
080100971126     C                   EXSR      CHKMAG
080200971126IF  3C     ERRL00        IFEQ      '1'
080300971126     C                   MOVEL     MSG(13)       DSMSMS
080400971126     C                   SETON                                        70
080500971126X   3C                   ELSE
080600130404     c                   movel     wmag          vidsis
080700971126     C                   MOVEL     WDMAG         VDDSIS
080800971126E   3C                   END
080900971126E   2C                   END
081000971126E   1C                   END
081100020320
081200020320      * CONTROLLO DITTA MANUTENZIONE SEDE
081300930709     C     ERRL00        IFEQ      '0'
081400930709     C     VIDDMS        IFEQ      *BLANKS
081500930709     C     VIDDMS        OREQ      *ZEROS
081600940504     C     DSUOP1        IFEQ      'F06'
081700070509     c     DSUop1        oreq      'F6N'
081800971015     C     DSUOP1        OREQ      'O03'
081900940504     C                   MOVEL     '1'           ERRL00
082000940504     C                   MOVEL     MSG(14)       DSMSMS
082100940504     C                   SETON                                        71
082200940504     C                   ENDIF
082300930709     C                   ELSE
082400930709     C                   MOVE      VIDDMS        WMAG
082500130409     c* qualsiasi codice dell'organigranna di appoggio
082600130410     c                   eval      wtorg='SD '
082700930709     C                   EXSR      CHKMAG
082800930709     C     ERRL00        IFEQ      '1'
082900930709     C                   MOVEL     MSG(14)       DSMSMS
083000930709     C                   SETON                                        71
083100930709     C                   ELSE
083200930709     C                   MOVE      WMAG          VIDDMS
083300930709     C                   MOVEL     WDMAG         VDDDMS
083400930709     C                   END
083500930709     C                   END
083600930709     C                   END
083700020320
083800020320      * CONTROLLO DITTA MANUTENZIONE FILIALE
083900020320     c                   if        errl00 = *off                                --- 1 -->
084000020320     C     VIDDMF        IFEQ      *BLANKS                                      --- 2 -->
084100930709     C     VIDDMF        OREQ      *ZEROS
084200020320     c                   if        dsuop1 = 'F06' or dsuop1 = 'O03'
084300070509     c                                            or DSUop1 = 'F6N'
084400020320     c                   eval      errl00 = *on
084500020320     c                   eval      *in72 = *on
084600020320     c                   eval      dsmsms = msg(14)
084700020320     c                   endif
084800020320     C                   ELSE                                                   <-- 2 -->
084900930709     C                   MOVE      VIDDMF        WMAG
085000130409     c* qualsiasi codice dell'organigranna di appoggio
085100130410     c                   eval      wtorg='SD '
085200930709     C                   EXSR      CHKMAG
085300020320     C     ERRL00        IFEQ      '1'                                          --- 3 -->
085400930709     C                   MOVEL     MSG(14)       DSMSMS
085500930709     C                   SETON                                        72
085600020320     C                   else                                                   <-- 3 -->
085700930709     C                   MOVE      WMAG          VIDDMF
085800930709     C                   MOVEL     WDMAG         VDDDMF
085900020320     C                   END                                                    <-- 3 ---
086000020320     C                   END                                                    <-- 2 ---
086100020320     C                   END                                                    <-- 1 ---
086200020320
086300020320      * CONTROLLO DITTA LEASING
086400020320if  1c                   if        errl00 = *off
086500971126IF  2C     VIDLEA        IFNE      *BLANKS
086600971126     C                   MOVE      VIDLEA        WMAG
086700130409     c* qualsiasi codice dell'organigranna di appoggio
086800130410     c                   eval      wtorg='SD '
086900971126     C                   EXSR      CHKMAG
087000971126IF  3C     ERRL00        IFEQ      '1'
087100971126     C                   MOVEL     MSG(13)       DSMSMS
087200971126     C                   SETON                                        73
087300971126X   3C                   ELSE
087400130404     c                   movel     wmag          vidlea
087500971126     C                   MOVEL     WDMAG         VDDLEA
087600971126E   3C                   END
087700971126E   2C                   END
087800971126E   1C                   END
087900020320
088000020320      * CONTROLLO MAGAZZINO UBICAZIONE
088100930709     C     ERRL00        IFEQ      '0'
088200930709     C                   MOVE      VIDMAG        WMAG
088300130409     c                   eval      wtorg='E  '
088400930709     C                   EXSR      CHKMAG
088500930709     C     ERRL00        IFEQ      '1'
088600930709     C                   MOVEL     MSG(15)       DSMSMS
088700930709     C                   SETON                                        74
088800930709     C                   ELSE
088900930709     C                   MOVE      WMAG          VIDMAG
089000930709     C                   MOVEL     WDMAG         VDDMAG
089100930709     C                   END
089200930709     C                   END
089300110401     c
089400110401     c* non accetto magazzino 990 se dittà proprietà 923
089500110401     c                   if        vidmag='990' and vidsis='923'
089600110401     c                   eval      errl00='1'
089700110401     C                   MOVEL     MSG(26)       DSMSMS
089800110401     C                   SETON                                        74
089900110401     c                   endif
090000020320
090100020320      * controllo il cliente
090200020320     c                   if        errl00 = *off
090300020320     c                   if        vidksc <> *zeros or vddksc <> *blanks
090400020320     c                   exsr      chkksc
090500020320     c                   if        errl00 = *on
090600020320     c                   eval      *in77 = *on
090700020320     c                   eval      dsmsms = msg(23)
090800020320     c                   endif
090900020320     c                   endif
091000020321     c                   if        vidksc = *zeros and vidmag = '951'
091100020320     c                   eval      errl00 = *on
091200020320     c                   eval      *in77 = *on
091300020320     c                   eval      dsmsms = msg(23)
091400020320     c                   endif
091500020320     c                   endif
091600020320
091700020320      * CONTROLLO IL N. DEL DOCUMENTO (SOLO IN INSERIMENTO)
091800940504     C     ERRL00        IFEQ      '0'
091900940504     C     DSUOP1        IFEQ      'F06'
092000070509     c     DSUop1        oreq      'F6N'
092100971015     C     DSUOP1        OREQ      'O03'
092200940504     C     VIDDOC        IFEQ      *ZEROS
092300940504     C                   MOVEL     '1'           ERRL00
092400940504     C                   MOVEL     MSG(17)       DSMSMS
092500940504     C                   SETON                                        75
092600940504     C                   END
092700940504     C                   END
092800940504     C                   END
092900020320
093000020320      * CONTROLLO LA DATA DEL DOCUMENTO (SOLO IN INSERIMENTO)
093100940504     C     ERRL00        IFEQ      '0'
093200940504     C     DSUOP1        IFEQ      'F06'
093300070509     c     DSUop1        oreq      'F6N'
093400971015     C     DSUOP1        OREQ      'O03'
093500940504     C     VIDDDO        IFNE      *ZEROS
093600971010     C                   Z-ADD     VIDDDO        WDAT
093700940504     C                   EXSR      CHKDAT
093800971013     C                   Z-ADD     WDAT          VIDDDO
093900940504     C     ERRL00        IFEQ      '1'
094000940504     C                   SETON                                        76
094100940504     C                   END
094200940504     C                   ELSE
094300940504     C                   MOVEL     '1'           ERRL00
094400940504     C                   MOVEL     MSG(8)        DSMSMS
094500940504     C                   SETON                                        76
094600940504     C                   END
094700940504     C                   END
094800940504     C                   END
094900120521     c
095000120521     C     DSUOP1        IFne      'F06'                                        *INS.
095100120521     C     DSUOP1        andne     'O03'                                        *DUPLICA
095200120521     C                   SETOFF                                       1112
095300120521     c                   endif
095400020320
095500000000     C                   ENDSR
095600020320      *================================================================
095700020320      *  CHKTIP - CONTROLLA IL TIPO DI UNITA
095800020320      *================================================================
095900930709     C     CHKTIP        BEGSR
096000021104
096100021104     c                   Eval      Wmsginfo = *Off
096200020319
096300020319      * CONTROLLO SE E' STATO INSERITO '?'
096400020319     C     '?'           SCAN      VIDTIP                                 99
096500020319
096600020319      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
096700020319     c                   if        *in99                                        --- 1 -->
096800021104     c                   Clear                   dunt
096900020319     c                   clear                   TIBS02DS
097000020319     c                   movel     'R'           t02mod
097100020319     c                   movel     knsif         t02sif
097200020321     c                   movel     'UNT'         t02cod
097300020319     c                   call      'TIBS02R'
097400020319     c                   parm                    KPJBA
097500020319     c                   parm                    TIBS02DS
097600020319     c                   movel     T02ke1        vidtip
097700020322     c                   movel     T02ke1        wvdtip
097800021104     c                   movel     t02uni        dunt
097900021104     c                   movel     d§untdes      vddtip
098000020319     c                   endif                                                  <-- 1 ---
098100020319
098200020319      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
098300021104     c                   Clear                   dunt
098400020319     c                   clear                   TIBS02DS
098500020319     c                   movel     'C'           T02mod
098600020319     c                   movel     knsif         t02sif
098700020321     c                   movel     'UNT'         t02cod
098800020320     c                   movel(p)  vidtip        T02ke1
098900020322     c                   move      vidtip        wvdtip
099000020319     c                   call      'TIBS02R'
099100020319     c                   parm                    KPJBA
099200020319     c                   parm                    TIBS02DS
099300020319     c                   if        t02err <> *blanks
099400020319     c                   eval      errl00 = '1'
099500020319     c                   clear                   vddtip
099600020319     c                   else
099700021104     c                   movel     t02uni        dunt
099800021104     c                   movel     d§untdes      vddtip
099900020319     c                   endif
100000021104
100100031204      * controllo flag x msg info
100200021104     c                   If        Errl00 = *Off  and  d§untfl1 = 'S'
100300021104     c                             and *In78 = *Off
100400070509     c                             and (DSUop1 <> 'F6N'
100500070509     c                              or  SDSprm <= 1)
100600021104     c                   Eval      Wmsginfo = *On
100700021104     c                   Eval      Errl00 = *On
100800021104     c                   EndIf
100900020319
101000930709     C                   ENDSR
101100020320      *================================================================
101200020320      *  CHKRG2 - CONTROLLA IL SECONDO LIVELLO DI RAGGRUPPAMENTO
101300020320      *================================================================
101400020320     c     chkrg2        begsr
101500020320
101600020320      * controllo se è stato inserito '?'
101700020320     c     '?'           scan      vidrg2                                 99
101800020320
101900020320      * è stato inserito '?', lancio il pgm di interr. tabelle
102000020320     c                   if        *in99                                        --- 1 -->
102100020320     c                   clear                   TIBS02DS
102200020320     c                   movel     'R'           t02mod
102300020320     c                   movel     knsif         t02sif
102400020321     c                   movel     'UNR'         t02cod
102500020320     c                   call      'TIBS02R'
102600020320     c                   parm                    KPJBA
102700020320     c                   parm                    TIBS02DS
102800020320     c                   movel     T02ke1        vidrg2
102900020320     c                   movel     t02uni        vddrg2
103000020320     c                   endif                                                  <-- 1 ---
103100020320
103200020320      * non è stato inserito '?', controllo che esista in tabella
103300020320     c                   clear                   TIBS02DS
103400020320     c                   movel     'C'           T02mod
103500020320     c                   movel     knsif         t02sif
103600020321     c                   movel     'UNR'         t02cod
103700020320     c                   movel(p)  vidrg2        T02ke1
103800020320     c                   call      'TIBS02R'
103900020320     c                   parm                    KPJBA
104000020320     c                   parm                    TIBS02DS
104100020320     c                   if        t02err <> *blanks
104200020320     c                   eval      errl00 = '1'
104300020320     c                   clear                   vddrg2
104400020320     c                   else
104500020320     c                   movel     t02uni        vddrg2
104600020320     c                   endif
104700020320
104800020320     c                   endsr
104900020320      *================================================================
105000020320      *  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
105100020320      *================================================================
105200930709     C     CHKMAG        BEGSR
105300020320
105400020320      * CONTROLLO SE E' STATO INSERITO '?'
105500130404     C**   '?'           SCAN      WMAG                                   99
105600020320
105700020320      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
105800130404     C**   *IN99         IFEQ      '1'                                          --- 1 -->
105900130404     C**                 MOVEL     *BLANKS       §COD1             3
106000130404     C**                 MOVE      *BLANKS       §DES1
106100130404     C**                 CALL      'TNSD20R'
106200130404     C**                 PARM                    §COD1
106300130404     C**                 PARM                    §TIP              1
106400130404     C**                 PARM                    §DES1            25
106500130404     C**                 MOVEL     §COD1         WMAG              3
106600130404     C*+                 MOVEL     §DES1         WDMAG            30
106700020320
106800130404     C**                 END                                                    <-- 1 ---
106900020320
107000020320      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
107100130404     C**                 MOVE      WMAG          ORGFIL
107200130404     C**   KEYORG        CHAIN     AZORG21l                           99
107300130404     C**   *IN99         IFEQ      '0'                                          --- 2 -->
107400130404     C**                 MOVEL     ORGFIL        WMAG
107500130404     C**                 MOVEL     ORGDES        WDMAG
107600130404     C*+                 ELSE                                                   <-- 2 -->
107700130404     c**   keyorg        chain     azorg01l
107800130404     c**                 if        %found(azorg01l)
107900130404     c**                 movel     orgfil        wmag
108000130404     c**                 movel     orgdes        wdmag
108100130404     c*+                 else
108200130404     C**                 MOVEL     '1'           ERRL00
108300130404     C**                 MOVEL     *ALL'*'       WDMAG
108400130404     c**                 endif
108500130404     C**                 END                                                    <-- 2 ---
108600020320
108700130404     c                   clear                   tnsdmagds
108800130404     c                   eval      §magmag=wmag
108900130409     c****               eval      §imagtorg='E  '
109000130409     c                   eval      §imagtorg=wtorg
109100130404     c                   call      'TNSDMAGR'
109200130404     c                   parm                    kpjba
109300130404     c                   parm                    tnsdmagds
109400130404     c
109500130404     c* Eseguita Ricerca
109600130404    1c                   if        §omagerr='R'
109700130404     c                   eval      wmag=§magmag
109800130404     c                   eval      wdmag=§omagdes
109900130404   x1c                   else
110000130404     c* Errore nel magazzino
110100130404    2c                   if        §omagerr='E'
110200130404     C                   MOVEL     '1'           ERRL00
110300130404     C                   MOVEL     *ALL'*'       WDMAG
110400130404     c* se tutto ok prendo decodifica e flag
110500130404   x2c                   else
110600130404     c                   eval      wdmag=§omagdes
110700130404    2c                   endif
110800130404    1c                   endif
110900130404     c
111000930709     C                   ENDSR
111100020320      *================================================================
111200020320      * CHKDAT - CONTROLLA LA DATA
111300020320      *================================================================
111400930709     C     CHKDAT        BEGSR
111500020320
111600971010     C                   Z-ADD     WDAT          G08DAT
111700971010     C                   Z-ADD     *ZEROS        G08INV
111800971010     C                   MOVEL     '0'           G08ERR
111900971010     C                   CALL      'XSRDA8'
112000971010     C                   PARM                    WLBDA8
112100971013     C                   Z-ADD     G08DAT        WDAT
112200971010     C     G08ERR        IFNE      '0'
112300930709     C                   MOVEL     '1'           ERRL00
112400930709     C                   MOVEL     MSG(12)       DSMSMS
112500930709     C                   END
112600020320
112700930709     C                   ENDSR
112800020320      *================================================================
112900020320      * CHKKSC - CONTROLLA IL CLIENTE
113000020320      *================================================================
113100020320     c     chkksc        begsr
113200020320
113300020320      * se non inserito il codice ma inserita una parte della ragione sociale
113400020320      * richiamo il programma della ricerca alfabetica
113500020320     c                   if        vidksc = *zeros and vddksc <> *blanks
113600020320     c                   exsr      ricalf
113700020320     c                   endif
113800020320
113900020320      * se inserito il codice controllo che esista sul piano dei conti
114000020320     c                   if        vidksc <> *zeros
114100020327     c                   clear                   TIBS69DS
114200020327     c                   z-add     vidksc        I69kac
114300020327     c                   call      'TIBS69R'
114400020327     c                   parm                    tibs69DS
114500020327     c                   parm                    ds_cnaco
114600020327     c                   parm                    ds_cnind
114700020327     c                   parm                    ds_cnclp
114800020327     c                   parm                    ds_fncls
114900020327
115000020327     c                   if        o69err <> '1'
115100020327     c                   movel     acorag        vddksc
115200020327     c                   else
115300020320     c                   eval      errl00 = *on
115400020320     c                   endif
115500020320     c                   endif
115600020320
115700020321     c                   endsr
115800020320      *================================================================
115900020320      *  RICALF - RICERCA ALFABETICA
116000020320      *================================================================
116100020320     c     ricalf        begsr
116200020320
116300020320     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
116400080922     C****               Z-ADD     9             PAXsta                         9=Escludo Annullati
116500080922     c* visulizzare anche i bloccati
116600080922     C                   Z-ADD     0             PAXsta                         9=Escludo Annullati
116700020709     C                   Z-ADD     DutKci        PAXccc                         Capoconto Clienti
116800020320     c                   movel(P)  vddksc        PAXdmt
116900020320     C                   z-add     1             PAXnum
117000020320
117100020320     C                   CALL      'XALFA3BR'
117200020320     C                   PARM                    PAXDUT
117300020320     C                   PARM                    CODUT
117400020320     C                   PARM                    PAXDMT
117500020320     C                   PARM                    PAXCCC
117600020320     C                   PARM                    PAXSTA
117700020320     C                   PARM      *blanks       PAXFLR
117800020320     C                   PARM      *blanks       PAXDIT
117900020320     C                   PARM                    PAXNUM            2 0
118000020320     C                   PARM                    PAXKCM           80
118100020320     C                   PARM                    PAXKSM          140
118200020320     C                   PARM                    PAXKDM           60
118300020320
118400020320      * NON TROVATO O PREMUTO CMD12
118500020320    2C     paxsta        IFGT      -1
118600020320     C                   MOVEL     PAXKSM        vidksc
118700020320    2C                   ENDIF
118800020320
118900020320     c                   endsr
119000930201     C*================================================================
119100930709     C*  RECVID - PORTA I CAMPI DAL RECORD AL VIDEO
119200930201     C*================================================================
119300930709     C     RECVID        BEGSR
119400930201     C*
119500930709     C                   MOVE      UNATIP        VIDTIP
119600930709     C                   Z-ADD     UNACOD        VIDCOD
119700930709     C                   MOVEL     UNAMAR        VIDMAR
119800930709     C                   MOVEL     UNAMOD        VIDMOD
119900930709     C                   MOVEL     UNAMAT        VIDMAT
120000020403     C                   MOVEL     UNAMOD        savMOD
120100020403     C                   MOVEL     UNAMAT        savMAT
120200930709     C                   MOVEL     UNADES        VIDDES
120300930709     C                   MOVEL     UNANOT        VIDNOT
120400971010     C                   Z-ADD     UNADCN        G08INV
120500971010     C                   Z-ADD     *ZEROS        G08DAT
120600971010     C                   MOVEL     '3'           G08ERR
120700971010     C                   CALL      'XSRDA8'
120800971010     C                   PARM                    WLBDA8
120900971010     C                   Z-ADD     G08DAT        VIDDCN
121000971010     C                   Z-ADD     UNADSG        G08INV
121100971010     C                   Z-ADD     *ZEROS        G08DAT
121200971010     C                   MOVEL     '3'           G08ERR
121300971010     C                   CALL      'XSRDA8'
121400971010     C                   PARM                    WLBDA8
121500971010     C                   Z-ADD     G08DAT        VIDDSG
121600930709     C                   MOVEL     UNASIS        VIDSIS
121700930709     C                   MOVE      UNADMS        VIDDMS
121800930709     C                   MOVE      UNADMF        VIDDMF
121900971010     C                   Z-ADD     UNADSM        G08INV
122000971010     C                   Z-ADD     *ZEROS        G08DAT
122100971010     C                   MOVEL     '3'           G08ERR
122200971010     C                   CALL      'XSRDA8'
122300971010     C                   PARM                    WLBDA8
122400971010     C                   Z-ADD     G08DAT        VIDDSM
122500930709     C                   MOVEL     UNALEA        VIDLEA
122600971010     C                   Z-ADD     UNADSL        G08INV
122700971010     C                   Z-ADD     *ZEROS        G08DAT
122800971010     C                   MOVEL     '3'           G08ERR
122900971010     C                   CALL      'XSRDA8'
123000971010     C                   PARM                    WLBDA8
123100971010     C                   Z-ADD     G08DAT        VIDDSL
123200930709     C                   MOVE      UNAMAG        VIDMAG
123300120411     C***                MOVEL     UNAEMU        VIDEMU
123400120412     c                   eval      videmu=%subst(unaemu:7:9)
123500120411     c
123600120411     c* Nel campo UNAEMU nelle prime 5 posizioni verrà memorizzato il numero ordine
123700120411     c                   clear                   alfaord
123800120412     c                   if        %subst(unaemu:1:6)>*zeros
123900120412     c                   eval      alfaord=%subst(unaemu:1:6)
124000120411     c                   movel     alfaord       vidord
124100120411     c                   else
124200120411     c                   clear                   vidord
124300120411     c                   endif
124400120411     c
124500930709     C                   MOVEL     UNAMVD        VIDMVD
124600150514     C                   CLEAR                   WLBDA8
124700150511     c                   IF        %subst(UNAMDR:8:8) <> *blank
124800150511     c                   eval      G08inv = %dec(%subst(UNAMDR:8:8):8:0)
124900150511     C                   Z-ADD     *ZEROS        G08DAT
125000150511     C                   MOVEL     '3'           G08ERR
125100150511     C                   CALL      'XSRDA8'
125200150511     C                   PARM                    WLBDA8
125300150514     C                   ENDIF
125400150511     C                   Z-ADD     G08DAT        VIDMDR2
125500930709     C                   MOVEL     UNAMTA        VIDMTA
125600930709     C                   MOVEL     UNAMDF        VIDMDF
125700930709     C                   MOVEL     UNAMRA        VIDMRA
125800150511     C                   EVAL      VIDMDR = %subst(UNAMDR:1:7)
125900930709     C                   MOVEL     UNAX25        VIDX25
126000930709     C                   MOVEL     UNALSW        VIDLSW
126100100903     C                   MOVE      UNALSW        VIDRESO
126200110104     C                   MOVE      UNALSW        savreso           1
126300020319     c                   move      unarg2        vidrg2
126400020319     c                   move      unaksc        vidksc
126500101027     c* Data dal reso
126600101027     C                   Z-ADD     UNADTR        G08INV
126700101027     C                   Z-ADD     *ZEROS        G08DAT
126800101027     C                   MOVEL     '3'           G08ERR
126900101027     C                   CALL      'XSRDA8'
127000101027     C                   PARM                    WLBDA8
127100101027     C                   Z-ADD     G08DAT        VIDdalr
127200930202     C*
127300930709     C     UNAATB        IFEQ      'A'
127400930202     C                   MOVEL     'ANNULLAT'    VIDATB
127500930202     C                   MOVE      'O'           VIDATB
127600930202     C                   ELSE
127700930202     C                   MOVEL     *BLANKS       VIDATB
127800930202     C                   END
127900930201     C*
128000930201     C                   ENDSR
128100930201     C*================================================================
128200930201     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
128300930201     C*================================================================
128400930201     C     AZZVID        BEGSR
128500930201     C*
128600120521     C                   SETOFF                                       1112
128700930709     C                   MOVEL     *BLANKS       VIDTIP
128800930709     C                   Z-ADD     *ZEROS        VIDCOD
128900930709     C                   MOVEL     *BLANKS       VIDMAR
129000930709     C                   MOVEL     *BLANKS       VIDMOD
129100930709     C                   MOVEL     *BLANKS       VIDMAT
129200020320     c                   clear                   vidrg2
129300020320     c                   clear                   vddrg2
129400930709     C                   MOVEL     *BLANKS       VIDDES
129500930709     C                   MOVEL     *BLANKS       VIDNOT
129600930709     C                   Z-ADD     *ZEROS        VIDDCN
129700930709     C                   Z-ADD     *ZEROS        VIDDSM
129800930709     C                   Z-ADD     *ZEROS        VIDDSG
129900930709     C                   Z-ADD     *ZEROS        VIDDSL
130000930709     C                   MOVEL     *BLANKS       VIDSIS
130100930709     C                   MOVEL     *BLANKS       VIDDMS
130200930709     C                   MOVEL     *BLANKS       VIDDMF
130300930709     C                   MOVEL     *BLANKS       VIDLEA
130400930709     C                   MOVEL     *BLANKS       VIDMAG
130500020320     c                   clear                   vidksc
130600020320     c                   clear                   vddksc
130700930709     C                   MOVEL     *BLANKS       VIDEMU
130800930709     C                   MOVEL     *BLANKS       VIDMVD
130900930709     C                   MOVEL     *BLANKS       VIDMTA
131000930709     C                   MOVEL     *BLANKS       VIDMDF
131100930709     C                   MOVEL     *BLANKS       VIDMRA
131200930709     C                   MOVEL     *BLANKS       VIDMDR
131300930709     C                   MOVEL     *BLANKS       VIDX25
131400930709     C                   MOVEL     *BLANKS       VIDLSW
131500100903     C                   MOVEL     *BLANKS       VIDRESO
131600110104     C                   MOVEL     *BLANKS       savRESO
131700940504     C                   Z-ADD     *ZEROS        VIDDOC
131800940504     C                   Z-ADD     *ZEROS        VIDDDO
131900101027     C                   Z-ADD     *ZEROS        VIDdalr
132000120411     C                   Z-ADD     *ZEROS        VIDord
132100930201     C*
132200930201     C                   ENDSR
132300070509      *===============================================================*
132400070509      *  RIEVID - IMPOSTAZIONE CAMPI A VIDEO CON QUELLI RICEVUTI      *
132500070509      *===============================================================*
132600070509     c     RieVid        BEGSR
132700070509      *
132800070509      * Programma richiamato da TNSDUKR x inserimento multiplo unità
132900070509      *   da codici a barre precedentemente inseriti in UNAIM00F
133000070509     c                   move      UIMtip        VIDtip
133100070509     c                   eval      VIDcod  = UIMcod
133200070509     c                   eval      VIDmar  = UIMmar
133300070509     c                   eval      VIDmod  = UIMmod
133400070509     c                   eval      VIDmat  = UIMmat
133500070509     c                   move      UIMrg2        VIDrg2
133600070509     c                   eval      VIDdes  = UIMdes
133700070509     c                   eval      VIDnot  = UIMnot
133800070509     c***                eval      VIDdcn  = UIMdcn
133900070509     c                   clear                   WLBda8
134000070509     c                   eval      G08inv  = UIMdcn
134100070509     c                   eval      G08err  = '3'
134200070509     c                   call      'XSRDA8'
134300070509     c                   parm                    WLBda8
134400070509     c                   eval      VIDdcn  = G08dat
134500070509     c***                eval      VIDdsm  = UIMdsm
134600070509     c                   clear                   WLBda8
134700070509     c                   eval      G08inv  = UIMdsm
134800070509     c                   eval      G08err  = '3'
134900070509     c                   call      'XSRDA8'
135000070509     c                   parm                    WLBda8
135100070509     c                   eval      VIDdsm  = G08dat
135200070509     c***                eval      VIDdsg  = UIMdsg
135300070509     c                   clear                   WLBda8
135400070509     c                   eval      G08inv  = UIMdsg
135500070509     c                   eval      G08err  = '3'
135600070509     c                   call      'XSRDA8'
135700070509     c                   parm                    WLBda8
135800070509     c                   eval      VIDdsg  = G08dat
135900070509     c***                eval      VIDdsl  = UIMdsl
136000070509     c                   clear                   WLBda8
136100070509     c                   eval      G08inv  = UIMdsl
136200070509     c                   eval      G08err  = '3'
136300070509     c                   call      'XSRDA8'
136400070509     c                   parm                    WLBda8
136500070509     c                   eval      VIDdsl  = G08dat
136600070509     c                   move      UIMsis        VIDsis
136700070509     c                   move      UIMlea        VIDlea
136800070509     c                   move      UIMdms        VIDdms
136900070509     c                   move      UIMdmf        VIDdmf
137000070509     c                   move      UIMmag        VIDmag
137100070509     c                   eval      VIDksc  = UIMksc
137200070509     c                   eval      VIDmdf  = UIMmdf
137300070509     c                   eval      VIDmra  = UIMmra
137400070509     c                   eval      VIDmdr  = UIMmdr
137500070509     c                   eval      VIDx25  = UIMx25
137600070509     c                   eval      VIDlsw  = UIMlsw
137700100907     c                   clear                   vidreso
137800101027     c                   clear                   viddalr
137900070509     c                   eval      VIDmvd  = UIMmvd
138000070509     c                   eval      VIDmta  = UIMmta
138100120412     c                   eval      VIDemu  = %subst(UIMemu:7:9)
138200120412     c                   if        %subst(UIMemu:1:6)>*zeros
138300120412     c                   movel     UIMemu        vidord
138400120412     c                   endif
138500070509     c                   eval      VIDdoc  = UIMdoc
138600070509     c***                eval      VIDddo  = UIMddo
138700070509     c                   clear                   WLBda8
138800070509     c                   eval      G08inv  = UIMddo
138900070509     c                   eval      G08err  = '3'
139000070509     c                   call      'XSRDA8'
139100070509     c                   parm                    WLBda8
139200070509     c                   eval      VIDddo  = G08dat
139300070509      *
139400070509     c                   ENDSR
139500971015     C*================================================================
139600971015     C* DUPVID - REIMPOSTA IL VIDEO PER LA DUPLICA
139700971015     C*================================================================
139800971015     C     DUPVID        BEGSR
139900971015     C*
140000971015     C* LEGGE IL RECORD DA DUPLICARE
140100020322     c                   z-add     dsutip        unatip
140200971015     C                   Z-ADD     DSUCOD        UNACOD
140300971015     C     KEYUNA        CHAIN     UNANA01L                           99
140400971015     C     *IN99         IFEQ      *OFF                                         *TROVATO
140500971015     C                   EXSR      RECVID                                       *RIEMPE CAMPI
140600971015     C                   ELSE                                                   *NON TROVATO
140700971015     C                   EXSR      AZZVID                                       *AZZERA CAMPI
140800971015     C                   ENDIF
140900971015     C*
141000971015     C* REIMPOSTA I CAMPI DA NON DUPLICARE
141100971015     C                   Z-ADD     *ZEROS        VIDCOD                         *CHIAVE
141200971015     C                   MOVEL     *BLANKS       VIDATB                         *ANNULLAMENTO
141300120521     c                   clear                   vidord
141400971015     C*
141500971015     C                   ENDSR
141600930201     C*================================================================
141700930709     C*  VIDREC - IMPOSTA CAMPI DA VIDEO A RECORD
141800930201     C*================================================================
141900930709     C     VIDREC        BEGSR
142000930201     C*
142100930709     C                   MOVE      VIDTIP        UNATIP
142200930709     C                   Z-ADD     VIDCOD        UNACOD
142300930709     C                   MOVEL     VIDMAR        UNAMAR
142400930709     C                   MOVEL     VIDMOD        UNAMOD
142500930709     C                   MOVEL     VIDMAT        UNAMAT
142600020320     c                   movel     vidrg2        unarg2
142700930709     C                   MOVEL     VIDDES        UNADES
142800930709     C                   MOVEL     VIDNOT        UNANOT
142900971010     C                   Z-ADD     VIDDCN        G08DAT
143000971010     C                   Z-ADD     *ZEROS        G08INV
143100971010     C                   MOVEL     '0'           G08ERR
143200971010     C                   CALL      'XSRDA8'
143300971010     C                   PARM                    WLBDA8
143400971010     C                   Z-ADD     G08INV        UNADCN
143500971010     C                   Z-ADD     VIDDSG        G08DAT
143600971010     C                   Z-ADD     *ZEROS        G08INV
143700971010     C                   MOVEL     '0'           G08ERR
143800971010     C                   CALL      'XSRDA8'
143900971010     C                   PARM                    WLBDA8
144000971010     C                   Z-ADD     G08INV        UNADSG
144100930709     C                   MOVEL     VIDSIS        UNASIS
144200930709     C                   MOVE      VIDDMS        UNADMS
144300930709     C                   MOVE      VIDDMF        UNADMF
144400971010     C                   Z-ADD     VIDDSM        G08DAT
144500971010     C                   Z-ADD     *ZEROS        G08INV
144600971010     C                   MOVEL     '0'           G08ERR
144700971010     C                   CALL      'XSRDA8'
144800971010     C                   PARM                    WLBDA8
144900971010     C                   Z-ADD     G08INV        UNADSM
145000930709     C                   MOVEL     VIDLEA        UNALEA
145100971010     C                   Z-ADD     VIDDSL        G08DAT
145200971010     C                   Z-ADD     *ZEROS        G08INV
145300971010     C                   MOVEL     '0'           G08ERR
145400971010     C                   CALL      'XSRDA8'
145500971010     C                   PARM                    WLBDA8
145600971010     C                   Z-ADD     G08INV        UNADSL
145700930709     C                   MOVE      VIDMAG        UNAMAG
145800020320     c                   z-add     vidksc        unaksc
145900120411     C*****              MOVEL     VIDEMU        UNAEMU
146000120411     c                   clear                   unaemu
146100120412     c                   eval      %subst(unaemu:7:9)=videmu
146200120411     c                   if        vidord>0
146300120412     c                   eval      %subst(unaemu:1:6)=%editc(vidord:'X')
146400120411     c                   else
146500120521     c                   eval      %subst(unaemu:1:6)='      '
146600120411     c                   endif
146700120411     c
146800930709     C                   MOVEL     VIDMVD        UNAMVD
146900930709     C                   MOVEL     VIDMTA        UNAMTA
147000930709     C                   MOVEL     VIDMDF        UNAMDF
147100930709     C                   MOVEL     VIDMRA        UNAMRA
147200930709     C                   MOVEL     VIDMDR        UNAMDR
147300930709     C                   MOVEL     VIDX25        UNAX25
147400930709     C                   MOVEL     VIDLSW        UNALSW
147500100903     C                   MOVE      VIDreso       UNALSW
147600101027     c
147700101027     c* se tolta la S di reso, tolgo anche la data
147800101027     c                   if        vidreso=*blanks
147900101027     c                   clear                   unadtr
148000101027     c                   else
148100101027     c* se data piena la reimposto altrimenti metto udate
148200110104     c*  Metto udate anche se reimposto il flag di reso
148300110104     c                   if        viddalr>0  and savreso=vidreso
148400101027     C                   Z-ADD     VIDdalr       G08DAT
148500101027     C                   Z-ADD     *ZEROS        G08INV
148600101027     C                   MOVEL     '0'           G08ERR
148700101027     C                   CALL      'XSRDA8'
148800101027     C                   PARM                    WLBDA8
148900101027     C                   Z-ADD     G08INV        UNADTR
149000101027     c                   else
149100101027     c                   movel     datcor        UNADTR
149200101027     c                   endif
149300101027     c                   endif
149400930201     C*
149500930201     C                   ENDSR
149600000000     C*================================================================
149700000000     C*  RUTINZ - ROUTINE INIZIALE
149800000000     C*================================================================
149900000000     C     RUTINZ        BEGSR
150000930201     C*
150100930201     C* RICEVIMENTO PARAMETRI
150200930201     C     *ENTRY        PLIST
150300940105     C                   PARM                    KPJBA
150400070503     C                   PARM                    UNANAds
150500070503     C                   PARM                    UNMOVds
150600940105     C                   MOVEL     KPJBU         DSUNIT
150700070503      * - impostazione iniziale dei parametri di output per chiamante
150800070503     c                   eval      DSUf03 = *off                                PREMUTO F03
150900070503     c                   eval      DSUerr = *off                                '1'=ERRORE
151000070503     c                   eval      DSUmsg = *blanks                             MESSAGGIO
151100930201     C*
151200930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
151300930201     C     XMSGPA        PLIST
151400930201     C                   PARM                    DSMSMS
151500930201     C                   PARM                    DSMSPG
151600930201     C                   PARM                    DSMSKE
151700930201     C                   PARM                    DSMSER
151800971013     C* DEFINIZIONI VARIABILI
151900020322     c     *like         define    unmtip        kumtip
152000971013     C     *LIKE         DEFINE    UNMCOD        KUMCOD
152100930201     C*
152200020320      * CHIAVE LETTURA UNANA01L
152300930709     C     KEYUNA        KLIST
152400020322     c                   kfld                    unatip
152500930709     C                   KFLD                    UNACOD
152600151120     C     KEYUNA_I      KLIST
152700151120     c                   kfld                    vidtip
152800151120     C                   KFLD                    vidCOD
152900020320
153000020320     c     keyun8        klist
153100020320     c                   kfld                    kunmod
153200020320     c                   kfld                    kunmat
153300091113     c     kana2         klist
153400091113     c                   kfld                    kuntip
153500091113     c                   kfld                    kunmat
153600020320
153700020320      * CHIAVE LETTURA UNMOV01L
153800971013     C     KEYMOV        KLIST
153900020322     c                   kfld                    kumtip
154000971013     C                   KFLD                    KUMCOD
154100020320
154200930201     C*
154300020529     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
154400930201     C     KEYORG        KLIST
154500930709     C                   KFLD                    ORGFIL
154600930201     C*
154700930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
154800930201     C                   CALL      'XKEY'
154900930201     C                   PARM                    DSKEY
155000930201     C*
155100930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
155200930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
155300930201     C*
155400930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
155500930201     C*   -NOME PGM CHIAMANTE
155600930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
155700941115     C                   MOVEL     'TNSDU2R'     DSMSPG
155800941115     C                   MOVEL     'TNSDU2R'     MSGQUE
155900930201     C                   MOVEL     *BLANKS       MSGKEY
156000930201     C                   MOVEL     *BLANKS       DSMSMS
156100930202     C*
156200930202     C* SE VISUALIZZAZIONE TOLGO FUNZIONE AGGIORNAMENTO
156300930709     C     DSUOP1        IFEQ      'O05'
156400930202     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
156500930202     C                   MOVEL     'N'           FUDATT
156600930202     C                   MOVEL     FUDRIG        FUD(6)
156700120411     c
156800120411     C                   MOVEL     FUD(10)       FUDRIG                         *sblocca ordine
156900120411     C                   MOVEL     'N'           FUDATT
157000120411     C                   MOVEL     FUDRIG        FUD(10)
157100930202     C                   END
157200940105     C*
157300940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DEI MOVIMENTI UNITA'
157400940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
157500940105     C     DSUPRO        IFEQ      'M'
157600940105     C                   MOVEL     FUD(8)        FUDRIG                         *GEST.MOVIMENTI
157700940105     C                   MOVEL     'N'           FUDATT
157800940105     C                   MOVEL     FUDRIG        FUD(8)
157900940105     C                   END
158000930201     C*
158100930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
158200930201     C                   Z-ADD     0             FUNGRU            5 0
158300930201     C*
158400930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
158500930201     C                   EXSR      FUNGES
158600930201     C*
158700930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
158800930201     C                   MOVEL     '0'           F12L00            1
158900930201     C                   MOVEL     '0'           F03L00            1
159000930201     C                   MOVEL     '0'           FINL00            1
159100930201     C                   MOVEL     '0'           ERRL00            1
159200930201     C                   MOVEL     '0'           FLFINE            1
159300930204     C*
159400971010     C* REPERIMENTO DATI UTENTE
159500020709
159600020709     c     *dtaara       define    §azute        azuteds
159700020709     c     *dtaara       define    §datiute      ddatiute
159800020709
159900020709     c                   in(E)     *dtaara
160000020709     c                   if        %Error or rsut = *blanks
160100020709     c                   clear                   tibs34ds
160200020709     c                   call      'TIBS34R'
160300020709     c                   parm                    Tibs34Ds
160400020709     c                   in        *dtaara
160500020709     c                   endif
160600971010     C*
160700971010     C* REPERISCE LA DATA CORRENTE
160800971010     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
160900971010     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
161000971010     C                   Z-ADD     WN8           G08DAT
161100971010     C                   Z-ADD     *ZEROS        G08INV
161200971010     C                   MOVEL     '0'           G08ERR
161300971010     C                   CALL      'XSRDA8'
161400971010     C                   PARM                    WLBDA8
161500971010     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
161600930201     C*
161700930201     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
161800930201     C* 0=NON PROTETTO, 1=PROTETTO
161900930709     C*   90 - DATI FILE
162000070503     C*   91 - CAMPI CHIAVE
162100940504     C*   92 - CAMPI NUM. E DATA DOC.
162200070503     C*   93 - NUMERO MATRICOLA
162300070503      *
162400070503     c                   eval      *in93 = *off
162500070503      *
162600930201     C* --> UTENTE ENTRATO IN GESTIONE
162700070503if  1C     DSUOP0        IFEQ      ' 2'                                         --- 1 -->
162800930201     C*
162900930709     C     DSUOP1        IFEQ      'F06'                                        *INS.
163000070509     c     DSUop1        oreq      'F6N'                                        *DUPLICA
163100971015     C     DSUOP1        OREQ      'O03'                                        *DUPLICA
163200940504     C                   SETOFF                                       909192
163300120521     C                   END
163400930201     C*
163500070503if  2C     DSUOP1        IFEQ      'O02'                                        *MOD.
163600930709     C                   SETON                                        91
163700020709     c                   If        DutLpo = 'S'
163800930709     C                   SETOFF                                       90
163900930201     C                   ELSE                                                   *FIL.
164000930709     C                   SETON                                        90
164100930201     C                   END
164200070503e   2C                   END
164300930201     C*
164400930709     C     DSUOP1        IFEQ      'O05'                                        *VIS+CANC+RIPR
164500930709     C     DSUOP1        OREQ      'O04'
164600930709     C     DSUOP1        OREQ      'O07'
164700940504     C                   SETON                                        909192
164800930201     C                   END
164900930201     C*
165000070503e   1C                   END                                                    <-- 1 ---
165100930201     C* --> UTENTE ENTRATO IN INTERROGAZIONE
165200940105     C     DSUOP0        IFEQ      ' 5'
165300940504     C                   SETON                                        909192
165400930201     C                   END
165500930201     C*
165600930709     C* SE SONO IN MOD. O VIS. O CANC. O RIPR. LEGGO RECORD
165700070503if  1C     DSUOP1        IFEQ      'O02'                                        --- 1 -->
165800971015     C     DSUOP1        OREQ      'O04'
165900930709     C     DSUOP1        OREQ      'O05'
166000930709     C     DSUOP1        OREQ      'O07'
166100930709     C* SE IL REC.NON ESISTE RIMANDO ERRORE AL PGM CHIAMANTE
166200020322     c                   z-add     dsutip        unatip
166300930709     C                   Z-ADD     DSUCOD        UNACOD
166400150604     C* se non siamo in visualizzazione, alloco il rcd
166500150604     C                   IF        DSUOP1 <> 'O05'
166600930709     C     KEYUNA        CHAIN     UNANA01L                           99
166700150604     C* se siamo in visualizzazione, NON alloco il rcd
166800150604     C                   ELSE
166900150604     C     KEYUNA        CHAIN(N)  UNANA01L                           99
167000150604     C                   ENDIF
167100930201     C     *IN99         IFEQ      '1'
167200930201     C                   MOVEL     '1'           ERRL00
167300930709     C                   MOVEL     MSG(16)       DSMSMS
167400930202     C                   ELSE
167500930709     C                   EXSR      RECVID
167600930201     C                   END
167700930208     C*
167800930208     C* SE TUTTO OK, DECODIFICO I CAMPI A VIDEO
167900930208     C                   EXSR      DECODI
168000930201     C*
168100070503e   1C                   END                                                    <-- 1 ---
168200930202     C*
168300930202     C* SE INSERIMENTO AZZERO TUTTI I CAMPI DEL VIDEO
168400930709     C     DSUOP1        IFEQ      'F06'
168500070509     c     DSUop1        oreq      'F6N'
168600930202     C                   EXSR      AZZVID
168700070509if  1c                   if            (DSUop1  = 'F06'
168800070509     c                              or  DSUop1  = 'F6N')
168900070509     c                             and  SDSprm  > 1
169000070509     c                   exsr      RieVid
169100070509e   1c                   endif
169200930202     C                   END
169300971015     C*
169400971015     C* SE DUPLICA REIMPOSTA IL VIDEO
169500971015     C     DSUOP1        IFEQ      'O03'
169600971015     C                   EXSR      DUPVID
169700971015     C                   ENDIF
169800120521
169900120521     C     DSUOP1        IFEQ      'F06'                                        *INS.
170000120521     C     DSUOP1        OREQ      'O03'                                        *DUPLICA
170100120521     C                   SETOn                                        11
170200120521     C                   END
170300930201     C*
170400930201     C* IMPOSTO DESCRIZIONE DELLA FUNZIONE IN CORSO A VIDEO
170500930709     C     DSUOP1        IFEQ      'F06'
170600070509     c     DSUop1        oreq      'F6N'
170700971015     C     DSUOP1        OREQ      'O03'
170800930201     C                   MOVEL     ' INSERIM'    VIDFUN
170900930201     C                   MOVE      'ENTO   '     VIDFUN
171000930201     C                   END
171100930709     C     DSUOP1        IFEQ      'O02'
171200930201     C                   MOVEL     '   MODIF'    VIDFUN
171300930201     C                   MOVE      'ICA    '     VIDFUN
171400930201     C                   END
171500930709     C     DSUOP1        IFEQ      'O04'
171600930201     C                   MOVEL     ' CANCELL'    VIDFUN
171700930201     C                   MOVE      'AZIONE '     VIDFUN
171800930201     C                   END
171900930709     C     DSUOP1        IFEQ      'O05'
172000930201     C                   MOVEL     'VISUALIZ'    VIDFUN
172100930201     C                   MOVE      'ZAZIONE'     VIDFUN
172200930201     C                   END
172300930709     C     DSUOP1        IFEQ      'O07'
172400930201     C                   MOVEL     ' RIPRIST'    VIDFUN
172500930201     C                   MOVE      'INO    '     VIDFUN
172600930201     C                   END
172700070509      *
172800070509      * SE pgm. richiamato da TNSDUKR per l'immissione di un record
172900070509      *  del quale vengono passati i valori per ogni campo,
173000070509      * ALLORA imposto flag per ritorno al pgm. chiamante
173100070509if  1c                   IF             DSUop1  = 'F6N'
173200070509     c                             and  SDSprm  > 1
173300070509     c                   exsr      INSERI
173400070611     c                   eval      DSUerr = ErrL00                              '1'=ERRORE
173500070611     c                   eval      DSUmsg = DSmsms                              MESSAGGIO
173600070509     c                   eval      FLfine = *on
173700070509e   1c                   ENDIF
173800000000     C*
173900000000     C                   ENDSR
174000000000     C*================================================================
174100000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
174200000000     C*================================================================
174300000000     C     FUNGES        BEGSR
174400000000     C*
174500000000     C* PULISCO CAMPI DI LAVORO
174600000000     C                   Z-ADD     0             W                 5 0
174700000000     C*
174800000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
174900000000     C* E CARICO TABELLA FUNZIONI ATTIVE
175000000000     C     1             DO        24            I
175100000000     C                   MOVEL     FUD(I)        FUDRIG
175200000000     C*
175300000000     C* PULIZIA FUNZIONI PRECEDENTI
175400000000     C                   MOVEL     *BLANKS       FUV(I)
175500000000     C*
175600000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
175700000000     C* (SALTA F24)
175800000000     C     FUDATT        IFEQ      'S'
175900000000     C     FUDFUN        ANDNE     '24'
176000000000     C                   ADD       1             W
176100000000     C                   MOVEL     FUDDES        FUV(W)
176200000000     C                   END
176300000000     C                   END                                                    DO WHILE
176400000000     C*
176500000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
176600000000     C* ALTRIMENTI DISATTIVO F24
176700000000     C     1             DO        24            I
176800000000     C                   MOVEL     FUD(I)        FUDRIG
176900000000     C*
177000000000     C     FUDFUN        IFEQ      '24'
177100000000     C     W             IFGT      6
177200000000     C                   MOVEL     'S'           FUDATT
177300000000     C                   ELSE
177400000000     C                   MOVEL     *BLANKS       FUDATT
177500000000     C                   END                                                    W GT 8
177600000000     C                   MOVEL     FUDRIG        FUD(I)
177700000000     C                   END
177800000000     C                   END                                                    DO WHILE
177900000000     C*
178000000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
178100000000     C     W             DIV       6             I
178200000000     C                   MVR                     Z
178300000000     C*
178400000000     C     Z             IFNE      0
178500000000     C     I             ADD       1             Z
178600000000     C                   ELSE
178700000000     C                   Z-ADD     I             Z                 5 0
178800000000     C                   END
178900000000     C*
179000000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
179100000000     C                   ADD       1             FUNGRU
179200000000     C     FUNGRU        IFGT      Z
179300000000     C     FUNGRU        ORLT      1
179400000000     C                   Z-ADD     1             FUNGRU
179500000000     C                   END
179600000000     C*
179700000000     C* PRIMO GRUPPO DI 6 FUNZIONI
179800000000     C     FUNGRU        IFEQ      1
179900000000     C                   MOVEL     FUV(1)        FUN001
180000000000     C                   MOVEL     FUV(2)        FUN002
180100000000     C                   MOVEL     FUV(3)        FUN003
180200000000     C                   MOVEL     FUV(4)        FUN004
180300000000     C                   MOVEL     FUV(5)        FUN005
180400000000     C                   MOVEL     FUV(6)        FUN006
180500000000     C                   END
180600000000     C*
180700000000     C* SECONDO GRUPPO DI 6 FUNZIONI
180800000000     C     FUNGRU        IFEQ      2
180900000000     C                   MOVEL     FUV(7)        FUN001
181000000000     C                   MOVEL     FUV(8)        FUN002
181100000000     C                   MOVEL     FUV(9)        FUN003
181200000000     C                   MOVEL     FUV(10)       FUN004
181300000000     C                   MOVEL     FUV(11)       FUN005
181400000000     C                   MOVEL     FUV(12)       FUN006
181500000000     C                   END
181600000000     C*
181700000000     C* TERZO GRUPPO DI 6 FUNZIONI
181800000000     C     FUNGRU        IFEQ      3
181900000000     C                   MOVEL     FUV(13)       FUN001
182000000000     C                   MOVEL     FUV(14)       FUN002
182100000000     C                   MOVEL     FUV(15)       FUN003
182200000000     C                   MOVEL     FUV(16)       FUN004
182300000000     C                   MOVEL     FUV(17)       FUN005
182400000000     C                   MOVEL     FUV(18)       FUN006
182500000000     C                   END
182600000000     C*
182700000000     C* QUARTO GRUPPO DI 6 FUNZIONI
182800000000     C     FUNGRU        IFEQ      4
182900000000     C                   MOVEL     FUV(19)       FUN001
183000000000     C                   MOVEL     FUV(20)       FUN002
183100000000     C                   MOVEL     FUV(21)       FUN003
183200000000     C                   MOVEL     FUV(22)       FUN004
183300000000     C                   MOVEL     *BLANKS       FUN005
183400000000     C                   MOVEL     *BLANKS       FUN006
183500000000     C                   END
183600000000     C*
183700000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
183800000000     C                   MOVEL     *BLANKS       FUN007
183900000000     C                   MOVEL     *BLANKS       FUN008
184000000000     C*
184100000000     C     1             DO        24            I
184200000000     C                   MOVEL     FUD(I)        FUDRIG
184300000000     C     FUDFUN        IFEQ      '24'
184400000000     C     FUDATT        ANDEQ     'S'
184500000000     C                   MOVEL     FUDDES        FUN008
184600000000     C                   END
184700000000     C                   END                                                    DO
184800000000     C                   ENDSR
184900000000     C*================================================================
185000000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
185100000000     C*================================================================
185200000000     C     FUNCON        BEGSR
185300000000     C*
185400000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
185500000000     C                   EXSR      FUNVAR
185600000000     C*
185700000000     C* PULISCO CAMPI DI LAVORO
185800000000     C                   Z-ADD     0             I                 5 0
185900000000     C                   Z-ADD     0             W                 5 0
186000000000     C*
186100000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
186200000000     C     FUNKEY        IFNE      *BLANKS
186300000000     C     1             DO        24            I
186400000000     C                   MOVEL     FUD(I)        FUDRIG
186500000000     C*
186600000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
186700000000     C     FUNKEY        IFEQ      FUDFUN
186800000000     C     FUDATT        ANDEQ     'S'
186900000000     C                   Z-ADD     I             W
187000000000     C                   END
187100000000     C                   END                                                    DO WHILE
187200000000     C*
187300000000     C* FUNZIONE NON ATTIVA (ERRORE)
187400000000     C     W             IFEQ      0
187500000000     C*
187600000000     C* EMISSIONE MESSAGGIO DI ERRORE
187700930201     C                   MOVEL     MSG(1)        DSMSMS
187800930201     C*
187900930201     C                   END                                                    W=0
188000000000     C                   END                                                    FUNKEY NE *BLANKS
188100000000     C*
188200000000     C* FUNZIONE ATTIVA (PROCEDI ...)
188300000000     C     W             IFNE      0
188400000000     C     FUNKEY        OREQ      *BLANKS
188500000000     C*
188600000000     C     FUNKEY        IFEQ      *BLANKS
188700000000     C                   EXSR      ENTGES
188800000000     C                   END
188900000000     C     FUNKEY        IFEQ      '01'
189000000000     C                   EXSR      F01GES
189100000000     C                   END
189200000000     C     FUNKEY        IFEQ      '03'
189300000000     C                   EXSR      F03GES
189400000000     C                   END
189500930202     C     FUNKEY        IFEQ      '06'
189600930202     C                   EXSR      F06GES
189700930202     C                   END
189800000000     C     FUNKEY        IFEQ      '08'
189900000000     C                   EXSR      F08GES
190000000000     C                   END
190100120411     C     FUNKEY        IFEQ      '10'
190200120411     C                   EXSR      F10GES
190300120411     C                   END
190400000000     C     FUNKEY        IFEQ      '12'
190500000000     C                   EXSR      F12GES
190600000000     C                   END
190700930201     C     FUNKEY        IFEQ      '21'
190800930201     C                   EXSR      F21GES
190900930201     C                   END
191000000000     C     FUNKEY        IFEQ      '22'
191100000000     C                   EXSR      F22GES
191200000000     C                   END
191300000000     C     FUNKEY        IFEQ      '24'
191400000000     C                   EXSR      F24GES
191500000000     C                   END
191600000000     C     FUNKEY        IFEQ      'HP'
191700000000     C                   EXSR      HELGES
191800000000     C                   END
191900000000     C*
192000000000     C                   END
192100000000     C*
192200000000     C                   ENDSR
192300000000     C*================================================================
192400000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
192500000000     C*================================================================
192600000000     C     FUNVAR        BEGSR
192700000000     C*
192800000000     C* INIZIALIZZA VARIABILE -FUNKEY-
192900000000     C*
193000000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
193100000000     C     KEY           IFEQ      ENTER
193200000000     C                   MOVEL     *BLANKS       FUNKEY
193300000000     C                   END
193400000000     C     KEY           IFEQ      F1
193500000000     C                   MOVEL     '01'          FUNKEY
193600000000     C                   END
193700000000     C     KEY           IFEQ      F2
193800000000     C                   MOVEL     '02'          FUNKEY
193900000000     C                   END
194000000000     C     KEY           IFEQ      F3
194100000000     C                   MOVEL     '03'          FUNKEY
194200000000     C                   END
194300000000     C     KEY           IFEQ      F4
194400000000     C                   MOVEL     '04'          FUNKEY
194500000000     C                   END
194600000000     C     KEY           IFEQ      F5
194700000000     C                   MOVEL     '05'          FUNKEY
194800000000     C                   END
194900000000     C     KEY           IFEQ      F6
195000000000     C                   MOVEL     '06'          FUNKEY
195100000000     C                   END
195200000000     C     KEY           IFEQ      F7
195300000000     C                   MOVEL     '07'          FUNKEY
195400000000     C                   END
195500000000     C     KEY           IFEQ      F8
195600000000     C                   MOVEL     '08'          FUNKEY
195700000000     C                   END
195800000000     C     KEY           IFEQ      F9
195900000000     C                   MOVEL     '09'          FUNKEY
196000000000     C                   END
196100000000     C     KEY           IFEQ      F10
196200000000     C                   MOVEL     '10'          FUNKEY
196300000000     C                   END
196400000000     C     KEY           IFEQ      F11
196500000000     C                   MOVEL     '11'          FUNKEY
196600000000     C                   END
196700000000     C     KEY           IFEQ      F12
196800000000     C                   MOVEL     '12'          FUNKEY
196900000000     C                   END
197000000000     C     KEY           IFEQ      F13
197100000000     C                   MOVEL     '13'          FUNKEY
197200000000     C                   END
197300000000     C     KEY           IFEQ      F14
197400000000     C                   MOVEL     '14'          FUNKEY
197500000000     C                   END
197600000000     C     KEY           IFEQ      F15
197700000000     C                   MOVEL     '15'          FUNKEY
197800000000     C                   END
197900000000     C     KEY           IFEQ      F16
198000000000     C                   MOVEL     '16'          FUNKEY
198100000000     C                   END
198200000000     C     KEY           IFEQ      F17
198300000000     C                   MOVEL     '17'          FUNKEY
198400000000     C                   END
198500000000     C     KEY           IFEQ      F18
198600000000     C                   MOVEL     '18'          FUNKEY
198700000000     C                   END
198800000000     C     KEY           IFEQ      F19
198900000000     C                   MOVEL     '19'          FUNKEY
199000000000     C                   END
199100000000     C     KEY           IFEQ      F20
199200000000     C                   MOVEL     '20'          FUNKEY
199300000000     C                   END
199400000000     C     KEY           IFEQ      F21
199500000000     C                   MOVEL     '21'          FUNKEY
199600000000     C                   END
199700000000     C     KEY           IFEQ      F22
199800000000     C                   MOVEL     '22'          FUNKEY
199900000000     C                   END
200000000000     C     KEY           IFEQ      F23
200100000000     C                   MOVEL     '23'          FUNKEY
200200000000     C                   END
200300000000     C     KEY           IFEQ      F24
200400000000     C                   MOVEL     '24'          FUNKEY
200500000000     C                   END
200600000000     C     KEY           IFEQ      ROLLUP
200700000000     C                   MOVEL     'RU'          FUNKEY
200800000000     C                   END
200900000000     C     KEY           IFEQ      HELP
201000000000     C                   MOVEL     'HP'          FUNKEY
201100000000     C                   END
201200000000     C                   ENDSR
201300000000     C*================================================================
201400000000     C* NOTE PER COMPILAZIONE SCHIERE
201500000000     C*================================================================
201600000000     C* -MSG-
201700000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
201800000000     C* (NON TOCCARE MSG,1)
201900000000     C*
202000000000     C* -CMD-
202100000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
202200000000     C*
202300000000     C* -FUD-
202400000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
202500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
202600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
202700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
202800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
202900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
203000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
203100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
203200000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
203300000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
203400000000     C*
203500000000     C*================================================================
203600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
203700941115TNSDU2R- Tasto funzionale non gestito                                          1
203800941115TNSDU2R- Codice già esistente.                                                 2
203900941115TNSDU2R- Inserimento effettuato.                                               3
204000941115TNSDU2R- Modifica effettuata.                                                  4
204100941115TNSDU2R- Cancellazione effettuata.                                             5
204200941115TNSDU2R- Ripristino effettuato.                                                6
204300941115TNSDU2R- Tipo unità non accettabile.                                           7
204400941115TNSDU2R- Data documento obbligatoria in inserimento.                           8
204500941115TNSDU2R- Marca unità non accettabile.                                          9
204600941115TNSDU2R- Modello unità non accettabile.                                       10
204700941115TNSDU2R- Matricola unità non accettabile.                                     11
204800941115TNSDU2R- Data non accettabile.                                                12
204900941115TNSDU2R- Ditta non accettabile.                                               13
205000941115TNSDU2R- Ditta manutenzione non accettabile.                                  14
205100941115TNSDU2R- Magazzino ubicazione non accettabile.                                15
205200941115TNSDU2R- Richiesta gestione di record inesistente.                            16
205300941115TNSDU2R- N. documento obbligatorio in inserimento.                            17
205400020320TNSDU2R- Numeratore non trovato.                                              18
205500020320TNSDU2R- Numeratore già impegnato riprovare più tardi.                        19
205600020320TNSDU2R- Oltrepassato numero massimo di codici.                               20
205700020320TNSDU2R- Esiste già una unità con lo stesso modello e la stessa matricola.    21
205800020320TNSDU2R- Secondo livello di raggruppamento non accettabile.                   22
205900020320TNSDU2R- Cliente non accettabile.                                             23
206000031204TNSDU2R- Hai indicato il tipo di PC ???????????????????                       24
206100091113TNSDU2R- LA matricola deve essere univoca per Tipo Unità!                     25
206200110401Se unità in proprietà alla 923 non posso accettare magazzino ubicazione "990" 26
206300120521Numero ordine obbligatorio in inserimento/copia                               27
206400000000** ======== SCHIERA: CMD  (COMANDI)                        ================
206500000000WRKSPLF
206600000000
206700000000
206800000000
206900000000
207000000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
207100930201                   -        (S/N)=N                        funz.01¬
207200000000                   -        (S/N)=N                        funz.02¬
207300000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
207400000000                   -        (S/N)=N                        funz.04¬
207500000000                   -        (S/N)=N                        funz.05¬
207600930202F06=Aggiornamento  -        (S/N)=S                        funz.06¬
207700930201                   -        (S/N)=N                        funz.07¬
207800971013F08=Movimenti      -        (S/N)=S                        funz.08¬
207900000000                   -        (S/N)=N                        funz.09¬
208000120411F10=Sblocca ORDINE -        (S/N)=S                        funz.10¬
208100000000                   -        (S/N)=N                        funz.11¬
208200930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
208300000000                   -        (S/N)=N                        funz.13¬
208400000000                   -        (S/N)=N                        funz.14¬
208500000000                   -        (S/N)=N                        funz.15¬
208600000000                   -        (S/N)=N                        funz.16¬
208700000000                   -        (S/N)=N                        funz.17¬
208800000000                   -        (S/N)=N                        funz.18¬
208900000000                   -        (S/N)=N                        funz.19¬
209000000000                   -        (S/N)=N                        funz.20¬
209100971013F21=Stampa         -        (S/N)=S                        funz.21¬
209200940105F22=Gestione stampe-        (S/N)=N                        funz.22¬
209300000000                   -        (S/N)=N                        funz.23¬
209400000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
