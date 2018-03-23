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
004900020320
005000020319      *------------------------------------------------------------------------*
005100020319      * INPUT
005200020319      *------------------------------------------------------------------------*
005300020319      * RIDEFINIZIONE SCHIERA -FUD-
005400000000     D FUDDS           DS
005500000000     D  FUDDE1                 1      1
005600000000     D  FUDFUN                 2      3
005700000000     D  FUDDE2                 4     19
005800000000     D  FUDFI0                20     34
005900000000     D  FUDATT                35     35
006000000000     D  FUDFI1                36     80
006100000000     D  FUDDES                 1     19
006200000000     D  FUDRIG                 1     80
006300020319      *------------------
006400020319      * DS "XSRDA8" - CONTROLLA DATA (8)
006500020319      *------------------
006600971010     D WLBDA8          DS
006700971010     D  G08DAT                 1      8  0
006800971010     D  G08INV                 9     16  0
006900971010     D  G08ERR                17     17
007000971010     D  G08TGI                18     22  0
007100020320
007200020319      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
007300930201     D DSMSG         E DS
007400020319      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
007500930709     D DSUNIT        E DS
007600070503      * Parametri ricevuti
007700070503     d UNANAds       e ds                  extname(UNANA00F)
007800070503     d                                     prefix(UIM:3)
007900070503     d UNMOVds         ds
008000070509     d  UIMdoc                             like(UNMdoc)
008100070509     d  UIMddo                             like(UNMddo)
008200020319      * DS PER GESTIONE TASTI FUNZIONALI
008300930201     D DSKEY         E DS
008400930201     D DEVDS1          DS
008500930201     D  KEY                  369    369
008600020319      * ARCHITETTURA
008700930201     D KPJBA         E DS
008800020320
008900020319     d trul33ds      e ds
009000020320     d tibs02ds      e ds
009100020327     d tibs69ds      e ds
009200020327     d ds_cnaco      e ds                  extname(cnaco00f)
009300020327     d ds_cnind      e ds                  extname(cnind00f)
009400020327     d ds_cnclp      e ds                  extname(cnclp00f)
009500020327     d ds_fncls      e ds                  extname(fncls00f)
009600020709
009700020709     d ddatiute      e ds
009800020709     d azuteds       e ds                  extname(azute00f)
009900020709     d tibs34ds      e ds                  inz
010000021104
010100021104     d dunt          e ds
010200130404     d tnsdmagds     e ds
010300070503
010400070503     d Status         sds
010500070503     d   SDSprm          *parms
010600020319
010700020319      *------------------------------------------------------------------------*
010800020319      * MAIN LINE
010900020319      *------------------------------------------------------------------------*
011000000000     C                   EXSR      RUTINZ
011100000000     C*
011200000000     C* CICLO FINO A CHE:
011300000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
011400000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
011500930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
011600070509     c                   DOW            F03L00 = *off
011700070509     c                             and  F12L00 = *off
011800070509     c                             and  FLFINE = *off
011900000000     C*
012000000000     C* EMETTE FORMATO VIDEO
012100000000     C                   EXSR      BUTFMR
012200000000     C*
012300000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
012400000000     C                   EXSR      FUNCON
012500000000     C*
012600070509     C                   ENDDO                                                  <-- 2 ---
012700000000     C*
012800020320
012900020320     c                   clear                   trul33ds
013000020320     c                   eval      i33tla = 'C'
013100020320     c                   movel(p)  trul33ds      kpjbu
013200020320     c                   call      'TRUL33R'
013300020320     c                   parm                    kpjba
013400020320
013500940105     C                   MOVEL     DSUNIT        KPJBU
013600000000     C                   SETON                                        LR
013700020320
013800000000     C*FINE MAIN DEL PGM
013900000000     C*================================================================
014000000000     C* EMISSIONE FORMATO A VIDEO
014100000000     C*================================================================
014200000000     C     BUTFMR        BEGSR
014300930204     C*
014400930204     C* WRITE RECORD TESTATA
014500941115     C                   WRITE     SDU2D01
014600930205     C*
014700930205     C* VISUALIZZAZIONE MESSAGGIO
014800930205     C                   EXSR      SNDMSG
014900930204     C*
015000930204     C* WRITE RECORD FUNZIONI
015100941115     C                   WRITE     SDU2D02
015200000000     C*
015300930204     C* EXFMT RECORD DETTAGLIO
015400941115     C                   EXFMT     SDU2D03
015500000000     C*
015600000000     C* AZZERO FLAG DI ERRORE LIVELLO
015700000000     C                   MOVEL     '0'           ERRL00
015800000000     C*
015900000000     C* AZZERO FLAG DI ERRORE
016000930709     C                   SETOFF                                       606162
016100020320     C                   SETOFF                                       636465
016200930709     C                   SETOFF                                       666768
016300930709     C                   SETOFF                                       697071
016400930709     C                   SETOFF                                       727374
016500020320     C                   SETOFF                                       757677
016600120522     C                   SETOFF                                         79
016700000000     C*
016800000000     C                   ENDSR
016900000000     C*================================================================
017000000000     C* SNDMSG - MANDA MESSAGGIO
017100000000     C*================================================================
017200000000     C     SNDMSG        BEGSR
017300000000     C*
017400000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
017500000000     C                   MOVEL     '1'           *IN40
017600941115     C                   WRITE     SDU2DMC
017700000000     C                   MOVEL     '0'           *IN40
017800000000     C*
017900000000     C* VISUALIZZAZIONE MESSAGGIO
018000000000     C                   Z-ADD     0             MSGULT            4 0
018100930202     C                   CALL      'XMSG'        XMSGPA
018200930201     C     DSMSER        IFEQ      0
018300930201     C                   MOVEL     DSMSKE        MSGKEY
018400000000     C                   END
018500000000     C                   Z-ADD     1             MSGULT
018600941115     C                   WRITE     SDU2DMS                                      SFL MESS
018700941115     C                   WRITE     SDU2DMC                                      CONTR MESS
018800000000     C*
018900000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
019000930201     C                   MOVEL     *BLANKS       DSMSMS
019100000000     C*
019200000000     C                   ENDSR
019300000000     C*================================================================
019400000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
019500000000     C*================================================================
019600000000     C     ENTGES        BEGSR
019700930201     C*
019800930201     C* CONTROLLA I CAMPI IMMESSI
019900930201     C                   EXSR      DECODI
020000930202     C*
020100930202     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
020200930709     C     DSUOP1        IFEQ      'O05'
020300930202     C                   MOVEL     '1'           FLFINE
020400930202     C                   END
020500000000     C*
020600000000     C                   ENDSR
020700000000     C*================================================================
020800930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
020900000000     C*================================================================
021000000000     C     F01GES        BEGSR
021100000000     C                   ENDSR
021200000000     C*================================================================
021300930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
021400000000     C*================================================================
021500000000     C     F03GES        BEGSR
021600000000     C*
021700930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
021800000000     C                   MOVEL     '1'           F03L00
021900930202     C* IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
022000930709     C                   MOVEL     '1'           DSUF03
022100000000     C*
022200000000     C                   ENDSR
022300930201     C*================================================================
022400930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
022500930201     C*================================================================
022600930201     C     F06GES        BEGSR
022700930201     C*
022800930201     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
022900930709     C     DSUOP1        IFEQ      'F06'
023000070509     c     DSUop1        oreq      'F6N'
023100971015     C     DSUOP1        OREQ      'O03'
023200930201     C                   EXSR      INSERI
023300930201     C                   END
023400930709     C     DSUOP1        IFEQ      'O02'
023500930202     C                   EXSR      MODIFI
023600930201     C                   END
023700930709     C     DSUOP1        IFEQ      'O04'
023800930201     C                   EXSR      CANCEL
023900930201     C                   END
024000930709     C     DSUOP1        IFEQ      'O07'
024100930201     C                   EXSR      RIPRIS
024200930201     C                   END
024300930201     C*
024400930201     C                   ENDSR
024500000000     C*================================================================
024600120411     C* F10GES -GESTIONE TASTO FUNZIONALE F10 -
024700000000     C*================================================================
024800120411     C     F10GES        BEGSR
024900120521     c                   seton                                        1112
025000000000     C                   ENDSR
025100000000     C*================================================================
025200930709     C* F08GES -GESTIONE TASTO FUNZIONALE F08 - GEST.MOVIMENTI
025300000000     C*================================================================
025400000000     C     F08GES        BEGSR
025500930201     C*
025600930709     C* LANCIO IL PGM PER LA GESTIONE DEI MOVIMENTI
025700940105     C                   UNLOCK    UNANA01L                                     *SBLOCCA IL REC.ANAG
025800941115     C                   MOVEL     'TNSDU4R'     WPGM              8
025900930201     C                   EXSR      LANPGM
026000020405      * riaggancio il file
026100020405     c                   eval      unatip = dsutip
026200020405     c                   eval      unacod = dsucod
026300150604     C* se non siamo in visualizzazione, alloco il rcd
026400150604     C                   IF        DSUOP1 <> 'O05'
026500150604     c     keyuna        chain     unana01l
026600150604     C* se siamo in visualizzazione, NON alloco il rcd
026700150604     C                   ELSE
026800150604     c     keyuna        chain(n)  unana01l
026900150604     C                   ENDIF
027000140509     c                   exsr      azzvid
027100140509     c                   exsr      recvid
027200930201     C*
027300000000     C                   ENDSR
027400000000     C*================================================================
027500930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
027600000000     C*================================================================
027700000000     C     F12GES        BEGSR
027800000000     C*
027900000000     C* IMPOSTO FLAG DI LIVELLO
028000000000     C                   MOVEL     '1'           F12L00
028100070503      * IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
028200070509if  1c                   if            (DSUop1  = 'F06'
028300070509     c                              or  DSUop1  = 'F6N')
028400070503     c                             and  SDSprm  > 1
028500070503     C                   MOVEL     '2'           DSUF03
028600070503e   1c                   endif
028700000000     C*
028800000000     C                   ENDSR
028900930201     C*================================================================
029000930709     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA ANAGR.UNITA
029100930201     C*================================================================
029200930201     C     F21GES        BEGSR
029300020405
029400020405      * salvo la ds prima di richiamare un altro pgm
029500020405     c                   eval      sav_dsunit = dsunit
029600020405
029700930201     C*
029800930709     C* LANCIO IL PGM PER LA STAMPA DELL'ANAGRAFICA UNITA
029900940105     C                   MOVEL     DSUNIT        KPJBU
030000941115     C                   CALL      'TNSDU6R'
030100930209     C                   PARM                    KPJBA
030200940105     C                   MOVEL     KPJBU         DSUNIT
030300940105     C*
030400940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
030500020405     C**!!!DSUF03        IFEQ      '1'
030600020405     C**!!!              MOVEL     '1'           F03L00
030700020405     C**!!!              MOVEL     '1'           ERRL00
030800020405     C**!!!              ENDIF
030900020405
031000020405      * reimposto la ds dopo aver richiamato un altro programma
031100020405     c                   eval      dsunit = sav_dsunit
031200020405
031300930201     C*
031400930201     C                   ENDSR
031500000000     C*================================================================
031600930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
031700000000     C*================================================================
031800000000     C     F22GES        BEGSR
031900000000     C*
032000000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
032100000000     C                   Z-ADD     80            LENGH            15 5
032200000000     C                   CALL      'QCAEXEC'
032300000000     C                   PARM                    CMD(1)
032400000000     C                   PARM                    LENGH
032500000000     C*
032600000000     C                   ENDSR
032700000000     C*================================================================
032800930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
032900000000     C*================================================================
033000000000     C     F24GES        BEGSR
033100000000     C*
033200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
033300000000     C                   EXSR      FUNGES
033400000000     C*
033500000000     C                   ENDSR
033600000000     C*================================================================
033700000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
033800000000     C*================================================================
033900000000     C     HELGES        BEGSR
034000000000     C                   ENDSR
034100930201     C*================================================================
034200930201     C* INSERI - FUNZIONE DI INSERIMENTO
034300930201     C*================================================================
034400930201     C     INSERI        BEGSR
034500930201     C*
034600930201     C* CONTROLLA I CAMPI IMMESSI
034700930201     C                   EXSR      DECODI
034800930201     C*
034900930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
035000020320
035100020320      * aggiorno il numeratore
035200020320     c                   clear                   trul33ds
035300020320     c                   eval      i33ope = 0
035400020320     c                   eval      i33cnu = 22
035500020320     c                   eval      i33num = 1
035600020320     c                   eval      i33unp = vidcod
035700020320     c                   movel(p)  trul33ds      kpjbu
035800020320     c                   call      'TRUL33R'
035900020320     c                   parm                    kpjba
036000020320     c                   movel(p)  kpjbu         trul33ds
036100020326     c                   if        o33err = *zeros
036200020326     c                   eval      vidcod = o33nri
036300020326     c                   endif
036400020320     c                   if        o33err = 01
036500020320     c                   eval      errl00 = *on
036600020320     c                   eval      *in61 = *on
036700020320     c                   eval      dsmsms = msg(18)
036800020320     c                   endif
036900020320     c                   if        o33err = 02
037000020320     c                   eval      errl00 = *on
037100020320     c                   eval      *in61 = *on
037200020320     c                   eval      dsmsms = msg(19)
037300020320     c                   endif
037400020320     c                   if        o33err = 03
037500020320     c                   eval      errl00 = *on
037600020320     c                   eval      *in61 = *on
037700020320     c                   eval      dsmsms = msg(20)
037800020320     c                   endif
037900020320
038000020320     c                   if        errl00 = *off
038100020320
038200930709     C* IMPOSTO IL RECORD DAL VIDEO
038300930709     C                   EXSR      VIDREC
038400971014     C                   MOVEL     *BLANKS       UNAFTR
038500971014     C                   Z-ADD     *ZEROS        UNADTR
038600971014     C                   MOVEL     *BLANKS       UNAATB
038700930709     C                   WRITE     UNANA000
038800940504     C                   EXSR      SCRMOV                                       *SCRIVE 1° MOVIMENTO
038900940504     C* AZZERA IL CAMPO MATRICOLA PER EVITARE DOPPIO INSERIMENTO
039000940504     C                   MOVEL     *BLANKS       VIDMAT
039100941215     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
039200941215     C                   MOVEL     '1'           FLFINE
039300930518     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
039400020320     C                   MOVEL     MSG(3)        DSumsg
039500930201     C*
039600020320     c                   endif
039700930201     C                   END                                                    <-- 1 ---
039800930201     C*
039900930201     C                   ENDSR
040000930201     C*================================================================
040100930201     C* MODIFI - FUNZIONE DI MODIFICA
040200930201     C*================================================================
040300930201     C     MODIFI        BEGSR
040400930201     C*
040500930201     C* CONTROLLA I CAMPI IMMESSI
040600930201     C                   EXSR      DECODI
040700930201     C*
040800930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
040900930201     C*
041000930709     C* IMPOSTO IL RECORD DAL VIDEO E RISCRIVO
041100930709     C                   EXSR      VIDREC
041200971014     C                   MOVEL     *BLANKS       UNAFTR
041300971014     C                   MOVEL     *BLANKS       UNAATB
041400930709     C                   UPDATE    UNANA000
041500930518     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
041600930518     C                   MOVEL     '1'           FLFINE
041700930518     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI MODIF. EFFETTUATA
041800930709     C                   MOVEL     MSG(4)        DSUMSG
041900930201     C*
042000930201     C                   END                                                    <-- 1 ---
042100930201     C*
042200930201     C                   ENDSR
042300930201     C*================================================================
042400930201     C* CANCEL - FUNZIONE DI CANCELLAZIONE
042500930201     C*================================================================
042600930201     C     CANCEL        BEGSR
042700971013     C*
042800971013     C* ANNULLA MOVIMENTI UNITA'
042900020322     c                   z-add     unatip        kumtip
043000971013     C                   Z-ADD     UNACOD        KUMCOD
043100971013     C     KEYMOV        SETLL     UNMOV01L
043200971013     C     KEYMOV        READE     UNMOV01L                               97
043300971013DO  1C     *IN97         DOWEQ     *OFF
043400100907IF  2C     UNMDTR        IFEQ      *ZEROS
043500971014     C                   DELETE    UNMOV000
043600971014X   2C                   ELSE                                                   *GIA' TRASMESSO
043700971014     C                   MOVEL     'A'           UNMATB
043800971013     C                   UPDATE    UNMOV000
043900971014E   2C                   ENDIF
044000971013     C     KEYMOV        READE     UNMOV01L                               97
044100971013E   1C                   ENDDO
044200971013     C* ANNULLA UNITA'
044300101027IF  1C**   UNADTR        IFEQ      *ZEROS                                       *MAI TRASMESSO
044400971014     C                   DELETE    UNANA000
044500101027X   1C**                 ELSE                                                   *GIA' TRASMESSO
044600101027     C**                 MOVEL     *BLANKS       UNAFTR
044700101027     C**                 MOVEL     'A'           UNAATB
044800101027     C**                 UPDATE    UNANA000
044900101027E   1C**                 ENDIF
045000930201     C*
045100930201     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
045200930201     C                   MOVEL     '1'           FLFINE
045300930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI CANC. EFFETTUATA
045400930709     C                   MOVEL     MSG(5)        DSUMSG
045500930201     C*
045600930201     C                   ENDSR
045700930201     C*================================================================
045800930201     C* RIPRIS - FUNZIONE DI RIPRISTINO
045900930201     C*================================================================
046000930201     C     RIPRIS        BEGSR
046100930202     C*
046200930709     C* RIPRISTINO IL RECORD
046300930709     C                   MOVEL     *BLANKS       UNAFTR
046400971014     C                   MOVEL     *BLANKS       UNAATB
046500930709     C                   UPDATE    UNANA000
046600930202     C*
046700930202     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
046800930202     C                   MOVEL     '1'           FLFINE
046900930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI RIPR. EFFETTUATO
047000930709     C                   MOVEL     MSG(6)        DSUMSG
047100930202     C*
047200930201     C                   ENDSR
047300930202     C*================================================================
047400930709     C* SCRREC - SCRITTURA DEL RECORD DEL FILE
047500930202     C*================================================================
047600930709     C     SCRREC        BEGSR
047700930202     C*
047800020322     c                   z-add     wvdtip        unatip
047900930709     C                   Z-ADD     VIDCOD        UNACOD
048000930709     C     KEYUNA        CHAIN     UNANA01L                           99
048100930202     C     *IN99         IFEQ      '0'
048200930709     C                   EXSR      VIDREC
048300930709     C                   UPDATE    UNANA000
048400930202     C                   ELSE
048500930709     C                   EXSR      VIDREC
048600930709     C                   WRITE     UNANA000
048700930202     C                   END
048800930202     C*
048900930202     C                   ENDSR
049000940504     C*================================================================
049100940504     C*  SCRMOV - SCRIVE IL MOVIMENTO DI CARICO
049200940504     C*================================================================
049300940504     C     SCRMOV        BEGSR
049400940504     C*
049500020326     C                   Z-ADD     UNAtip        UNMtip
049600940504     C                   Z-ADD     UNACOD        UNMCOD
049700110407     c                   movel     unamat        unmmat
049800940504     C                   MOVEL     'CARI'        UNMCAU
049900940504     C                   MOVEL     'E'           UNMTMO
050000971010     C                   Z-ADD     DATCOR        UNMDTM
050100941216     C                   Z-ADD     20000         UNMPRG
050200940504     C                   Z-ADD     UNAMAG        UNMMAG
050300940504     C                   Z-ADD     *ZEROS        UNMMGC
050400940504     C                   MOVEL     'CARICO'      UNMDES
050500940504     C                   Z-ADD     VIDDOC        UNMDOC
050600971010     C                   Z-ADD     VIDDDO        G08DAT
050700971010     C                   Z-ADD     *ZEROS        G08INV
050800971010     C                   MOVEL     '0'           G08ERR
050900971010     C                   CALL      'XSRDA8'
051000971010     C                   PARM                    WLBDA8
051100971010     C                   Z-ADD     G08INV        UNMDDO
051200940504     C                   Z-ADD     *ZEROS        UNMCNT
051300020320     c                   eval      unmksc = unaksc
051400940504     C                   WRITE     UNMOV000
051500940504     C*
051600940504     C                   ENDSR
051700930201     C*================================================================
051800930201     C* LANPGM - LANCIA UN PROGRAMMA
051900930201     C*================================================================
052000930201     C     LANPGM        BEGSR
052100020405
052200020405      * salvo la ds prima di richiamare un altro pgm
052300020405     c                   eval      sav_dsunit = dsunit
052400020405
052500930201     C*
052600930201     C* IMPOSTO PARAMETRI PER IL PGM
052700930709     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
052800930709     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
052900930709     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
053000930709     C                   Z-ADD     UNATIP        DSUTIP                         TIPO UNITA
053100930709     C                   Z-ADD     UNACOD        DSUCOD                         COD.UNITA
053200930716     C                   Z-ADD     *ZEROS        DSUMAG                         MAGAZZINO
053300930712     C                   Z-ADD     *ZEROS        DSUPRG                         PROGRESS. MOV.
053400930906     C                   Z-ADD     *ZEROS        DSUDTM                         DATA MOVIMENTO
053500930201     C*
053600930201     C* LANCIO PROGRAMMA
053700940105     C                   MOVEL     DSUNIT        KPJBU
053800930312     C                   CALL      WPGM
053900940105     C                   PARM                    KPJBA
054000940105     C                   MOVEL     KPJBU         DSUNIT
054100930201     C*
054200930201     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
054300020405     C     DSUF03        IFEQ      '1'
054400020405     C**!!!              MOVEL     '1'           F03L00
054500020405     C**!!!              MOVEL     '1'           ERRL00
054600930201     C                   ELSE
054700930201     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
054800930709     C                   MOVEL     DSUMSG        DSMSMS
054900930201     C                   END
055000020405
055100020405      * reimposto la ds dopo aver richiamato un altro programma
055200020405     c                   eval      dsunit = sav_dsunit
055300020405
055400930201     C*
055500930201     C                   ENDSR
055600000000     C*================================================================
055700930201     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
055800000000     C*================================================================
055900000000     C     DECODI        BEGSR
056000020403
056100020403      * faccio i controlli solo per inserimento/manutenzione/copia
056200020403     c                   if        dsuop1 = 'F06' or dsuop1 = 'O02' or
056300070509     c                             DSUop1 = 'F6N' or
056400020403     c                             dsuop1 = 'O03'
056500020320
056600020320      * CONTROLLO E DECODIFICO IL TIPO UNITA
056700930709     C                   EXSR      CHKTIP
056800021104     c                   if        errl00 = *on and wmsginfo = *Off
056900930709     C                   MOVEL     MSG(7)        DSMSMS
057000930709     C                   SETON                                        60
057100930709     C                   END
057200021104      * Emetto msg. info
057300021104     c                   If        Wmsginfo = *On and *In78 = *Off
057400021104     c                   Movel     Msg(24)       DsmSms
057500091113     c                   Eval      *In65 = *On
057600021104     c                   Eval      *In78 = *On
057700021104     c                   EndIf
057800020321
057900020320      * CONTROLLO LA MARCA
058000020320     c                   if        errl00 = *off
058100930709     C     VIDMAR        IFEQ      *BLANKS
058200930709     C                   MOVEL     '1'           ERRL00
058300930709     C                   MOVEL     MSG(9)        DSMSMS
058400930709     C                   SETON                                        62
058500930709     C                   END
058600930709     C                   END
058700120521     c
058800120521     c* Numero ordine obbligatorio
058900120521      * faccio i controlli solo per inserimento/copia
059000120521     c                   if        dsuop1 = 'F06' or
059100120521     c                             DSUop1 = 'F6N' or
059200120521     c                             dsuop1 = 'O03'
059300120521     c                   if        vidord<=*zeros  and errl00=*off
059400120521     C                   MOVEL     '1'           ERRL00
059500120521     C                   MOVEL     MSG(27)       DSMSMS
059600120521     C                   SETON                                        79
059700120521     c                   leavesr
059800120521     C                   END
059900120521     C                   END
060000020320
060100020320      * CONTROLLO IL MODELLO
060200020320     c                   if        errl00 = *off
060300930709     C     VIDMOD        IFEQ      *BLANKS
060400930709     C                   MOVEL     '1'           ERRL00
060500930709     C                   MOVEL     MSG(10)       DSMSMS
060600930709     C                   SETON                                        63
060700930709     C                   END
060800930709     C                   END
060900020320
061000020320      * CONTROLLO LA MATRICOLA
061100020320     c                   if        errl00 = *off
061200930709     C     VIDMAT        IFEQ      *BLANKS
061300930709     C                   MOVEL     '1'           ERRL00
061400930709     C                   MOVEL     MSG(11)       DSMSMS
061500930709     C                   SETON                                        64
061600930709     C                   END
061700930709     C                   END
061800020403
061900020403     c                   endif
062000020321
062100020321      * controllo se già inserito un codice con stesso modello e matricola
062200091113     c                   if        errl00 = *off
062300020321     c                   eval      kunmod = vidmod
062400020321     c                   eval      kunmat = vidmat
062500020321     c     keyun8        setll     unana08l
062600020403      * immissione/copia
062700020403     c                   if        dsuop1 = 'F06' or dsuop1 = 'O03'             --- 1 -->
062800070509     c                                            or DSUop1 = 'F6N'
062900020321     c                   if        %equal(unana08l)                             --- 2 -->
063000020321     c                   eval      errl00 = *on
063100020321     c                   eval      *in63 = *on
063200020321     c                   eval      *in64 = *on
063300020321     c                   eval      dsmsms = msg(21)
063400020321     c                   endif                                                  <-- 2 ---
063500020403      * manutenzione
063600020403     c                   else                                                   <-- 1 -->
063700020403     c                   if        vidmod <> savmod or vidmat <> savmat         --- 2 -->
063800020403     c                   if        %equal(unana08l)                             --- 3 -->
063900020403     c                   eval      errl00 = *on
064000020403     c                   eval      *in63 = *on
064100020403     c                   eval      *in64 = *on
064200020403     c                   eval      dsmsms = msg(21)
064300020403     c                   endif                                                  <-- 3 ---
064400020403     c                   endif                                                  <-- 2 ---
064500020403     c                   endif                                                  <-- 1 ---
064600091113     c                   endif                                                  <-- 1 ---
064700091113
064800091113      * 13 11 09 EDPES --> da oggi è obbligatoria la matricola univoca
064900091113      *  all'interno dell'unità
065000091113     c                   if        errl00 = *off
065100091113     c                   eval      kuntip = wvdtip
065200091113     c                   eval      kunmat = vidmat
065300091113     c     kana2         setll     unana02l
065400091113      * immissione/copia
065500091113     c                   if        dsuop1 = 'F06' or dsuop1 = 'O03'             --- 1 -->
065600091113     c                                            or DSUop1 = 'F6N'
065700091113     c                   if        %equal(unana02l)                             --- 2 -->
065800091113     c                   eval      errl00 = *on
065900091113     c                   eval      *in64 = *on
066000091113     c                   eval      dsmsms = msg(25)
066100091113     c                   endif                                                  <-- 2 ---
066200091113      * manutenzione
066300091113     c                   else                                                   <-- 1 -->
066400091113     c                   if        vidmat <> savmat                             --- 2 -->
066500091113     c                   if        %equal(unana02l)                             --- 3 -->
066600091113     c                   eval      errl00 = *on
066700091113     c                   eval      *in64 = *on
066800091113     c                   eval      dsmsms = msg(25)
066900091113     c                   endif                                                  <-- 2 ---
067000091113     c                   endif                                                  <-- 1 ---
067100091113     c                   endif                                                  <-- 2 ---
067200091113     c                   endif                                                  <-- 2 ---
067300020321
067400020320      * controllo il 2° livello di raggruppamento
067500020320     c                   if        errl00 = *off
067600020321     c                   if        (vidrg2 = *blanks or vidrg2 = *zeros) and
067700070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
067800070509     c                                             or DSUop1 = 'F6N')
067900020320     c                   eval      errl00 = *on
068000020320     c                   eval      *in65 = *on
068100020320     c                   eval      dsmsms = msg(22)
068200020320     c                   endif
068300020320     c                   if        errl00 = *off
068400020320     c                   exsr      chkrg2
068500020320     c                   if        errl00 = *on
068600020320     c                   eval      *in65 = *on
068700020320     c                   eval      dsmsms = msg(22)
068800020320     c                   endif
068900020320     c                   endif
069000020320     c                   endif
069100020320
069200020320      * CONTROLLO LA DATA DI CONSEGNA
069300020320     c                   if        errl00 = *off
069400930709     C     VIDDCN        IFNE      *ZEROS
069500971010     C                   Z-ADD     VIDDCN        WDAT              8 0
069600930709     C                   EXSR      CHKDAT
069700971013     C                   Z-ADD     WDAT          VIDDCN
069800930709     C     ERRL00        IFEQ      '1'
069900930709     C                   SETON                                        66
070000930709     C                   END
070100930709     C                   END
070200020320     c                   if        viddcn = *zeros and
070300070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
070400070509     c                                             or DSUop1 = 'F6N')
070500020320     c                   eval      errl00 = *on
070600020320     c                   eval      *in66 = *on
070700020320     c                   eval      dsmsms = msg(12)
070800020320     c                   endif
070900930709     C                   END
071000020320
071100020320      * CONTROLLO LA DATA DI SCADENZA MANUTENZIONE
071200020320     c                   if        errl00 = *off
071300930709     C     VIDDSM        IFNE      *ZEROS
071400930709     C                   Z-ADD     VIDDSM        WDAT
071500930709     C                   EXSR      CHKDAT
071600971013     C                   Z-ADD     WDAT          VIDDSM
071700930709     C     ERRL00        IFEQ      '1'
071800930709     C                   SETON                                        67
071900930709     C                   END
072000930709     C                   END
072100020320     c                   if        viddsm = *zeros and
072200070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
072300070509     c                                             or DSUop1 = 'F6N')
072400020320     c                   eval      errl00 = *on
072500020320     c                   eval      *in67 = *on
072600020320     c                   eval      dsmsms = msg(12)
072700020320     c                   endif
072800930709     C                   END
072900020320
073000020320      * CONTROLLO LA DATA DI SCADENZA GARANZIA
073100020320     c                   if        errl00 = *off
073200930709     C     VIDDSG        IFNE      *ZEROS
073300930709     C                   Z-ADD     VIDDSG        WDAT
073400930709     C                   EXSR      CHKDAT
073500971013     C                   Z-ADD     WDAT          VIDDSG
073600930709     C     ERRL00        IFEQ      '1'
073700930709     C                   SETON                                        68
073800930709     C                   END
073900930709     C                   END
074000020320     c                   if        viddsg = *zeros and
074100070509     c                             (dsuop1 = 'F06' or dsuop1 = 'O03'
074200070509     c                                             or DSUop1 = 'F6N')
074300020320     c                   eval      errl00 = *on
074400020320     c                   eval      *in68 = *on
074500020320     c                   eval      dsmsms = msg(12)
074600020320     c                   endif
074700930709     C                   END
074800020320
074900020320      * CONTROLLO LA DATA DI SCADENZA LEASING
075000020320     c                   if        errl00 = *off
075100930709     C     VIDDSL        IFNE      *ZEROS
075200930709     C                   Z-ADD     VIDDSL        WDAT
075300930709     C                   EXSR      CHKDAT
075400971013     C                   Z-ADD     WDAT          VIDDSL
075500930709     C     ERRL00        IFEQ      '1'
075600930709     C                   SETON                                        69
075700930709     C                   END
075800930709     C                   END
075900930709     C                   END
076000020320
076100020320      * CONTROLLO DITTA PROPRIETA'
076200971126IF  1C     ERRL00        IFEQ      '0'
076300971126IF  2C     VIDSIS        IFEQ      *BLANKS
076400971126IF  3C     DSUOP1        IFEQ      'F06'
076500070509     c     DSUop1        oreq      'F6N'
076600971015     C     DSUOP1        OREQ      'O03'
076700940504     C                   MOVEL     '1'           ERRL00
076800940504     C                   MOVEL     MSG(13)       DSMSMS
076900940504     C                   SETON                                        70
077000971126E   3C                   ENDIF
077100971126X   2C                   ELSE
077200971126     C                   MOVE      VIDSIS        WMAG
077300130409     c* qualsiasi codice dell'organigranna di appoggio
077400130410     c                   eval      wtorg='SD '
077500971126     C                   EXSR      CHKMAG
077600971126IF  3C     ERRL00        IFEQ      '1'
077700971126     C                   MOVEL     MSG(13)       DSMSMS
077800971126     C                   SETON                                        70
077900971126X   3C                   ELSE
078000130404     c                   movel     wmag          vidsis
078100971126     C                   MOVEL     WDMAG         VDDSIS
078200971126E   3C                   END
078300971126E   2C                   END
078400971126E   1C                   END
078500020320
078600020320      * CONTROLLO DITTA MANUTENZIONE SEDE
078700930709     C     ERRL00        IFEQ      '0'
078800930709     C     VIDDMS        IFEQ      *BLANKS
078900930709     C     VIDDMS        OREQ      *ZEROS
079000940504     C     DSUOP1        IFEQ      'F06'
079100070509     c     DSUop1        oreq      'F6N'
079200971015     C     DSUOP1        OREQ      'O03'
079300940504     C                   MOVEL     '1'           ERRL00
079400940504     C                   MOVEL     MSG(14)       DSMSMS
079500940504     C                   SETON                                        71
079600940504     C                   ENDIF
079700930709     C                   ELSE
079800930709     C                   MOVE      VIDDMS        WMAG
079900130409     c* qualsiasi codice dell'organigranna di appoggio
080000130410     c                   eval      wtorg='SD '
080100930709     C                   EXSR      CHKMAG
080200930709     C     ERRL00        IFEQ      '1'
080300930709     C                   MOVEL     MSG(14)       DSMSMS
080400930709     C                   SETON                                        71
080500930709     C                   ELSE
080600930709     C                   MOVE      WMAG          VIDDMS
080700930709     C                   MOVEL     WDMAG         VDDDMS
080800930709     C                   END
080900930709     C                   END
081000930709     C                   END
081100020320
081200020320      * CONTROLLO DITTA MANUTENZIONE FILIALE
081300020320     c                   if        errl00 = *off                                --- 1 -->
081400020320     C     VIDDMF        IFEQ      *BLANKS                                      --- 2 -->
081500930709     C     VIDDMF        OREQ      *ZEROS
081600020320     c                   if        dsuop1 = 'F06' or dsuop1 = 'O03'
081700070509     c                                            or DSUop1 = 'F6N'
081800020320     c                   eval      errl00 = *on
081900020320     c                   eval      *in72 = *on
082000020320     c                   eval      dsmsms = msg(14)
082100020320     c                   endif
082200020320     C                   ELSE                                                   <-- 2 -->
082300930709     C                   MOVE      VIDDMF        WMAG
082400130409     c* qualsiasi codice dell'organigranna di appoggio
082500130410     c                   eval      wtorg='SD '
082600930709     C                   EXSR      CHKMAG
082700020320     C     ERRL00        IFEQ      '1'                                          --- 3 -->
082800930709     C                   MOVEL     MSG(14)       DSMSMS
082900930709     C                   SETON                                        72
083000020320     C                   else                                                   <-- 3 -->
083100930709     C                   MOVE      WMAG          VIDDMF
083200930709     C                   MOVEL     WDMAG         VDDDMF
083300020320     C                   END                                                    <-- 3 ---
083400020320     C                   END                                                    <-- 2 ---
083500020320     C                   END                                                    <-- 1 ---
083600020320
083700020320      * CONTROLLO DITTA LEASING
083800020320if  1c                   if        errl00 = *off
083900971126IF  2C     VIDLEA        IFNE      *BLANKS
084000971126     C                   MOVE      VIDLEA        WMAG
084100130409     c* qualsiasi codice dell'organigranna di appoggio
084200130410     c                   eval      wtorg='SD '
084300971126     C                   EXSR      CHKMAG
084400971126IF  3C     ERRL00        IFEQ      '1'
084500971126     C                   MOVEL     MSG(13)       DSMSMS
084600971126     C                   SETON                                        73
084700971126X   3C                   ELSE
084800130404     c                   movel     wmag          vidlea
084900971126     C                   MOVEL     WDMAG         VDDLEA
085000971126E   3C                   END
085100971126E   2C                   END
085200971126E   1C                   END
085300020320
085400020320      * CONTROLLO MAGAZZINO UBICAZIONE
085500930709     C     ERRL00        IFEQ      '0'
085600930709     C                   MOVE      VIDMAG        WMAG
085700130409     c                   eval      wtorg='E  '
085800930709     C                   EXSR      CHKMAG
085900930709     C     ERRL00        IFEQ      '1'
086000930709     C                   MOVEL     MSG(15)       DSMSMS
086100930709     C                   SETON                                        74
086200930709     C                   ELSE
086300930709     C                   MOVE      WMAG          VIDMAG
086400930709     C                   MOVEL     WDMAG         VDDMAG
086500930709     C                   END
086600930709     C                   END
086700110401     c
086800110401     c* non accetto magazzino 990 se dittà proprietà 923
086900110401     c                   if        vidmag='990' and vidsis='923'
087000110401     c                   eval      errl00='1'
087100110401     C                   MOVEL     MSG(26)       DSMSMS
087200110401     C                   SETON                                        74
087300110401     c                   endif
087400020320
087500020320      * controllo il cliente
087600020320     c                   if        errl00 = *off
087700020320     c                   if        vidksc <> *zeros or vddksc <> *blanks
087800020320     c                   exsr      chkksc
087900020320     c                   if        errl00 = *on
088000020320     c                   eval      *in77 = *on
088100020320     c                   eval      dsmsms = msg(23)
088200020320     c                   endif
088300020320     c                   endif
088400020321     c                   if        vidksc = *zeros and vidmag = '951'
088500020320     c                   eval      errl00 = *on
088600020320     c                   eval      *in77 = *on
088700020320     c                   eval      dsmsms = msg(23)
088800020320     c                   endif
088900020320     c                   endif
089000020320
089100020320      * CONTROLLO IL N. DEL DOCUMENTO (SOLO IN INSERIMENTO)
089200940504     C     ERRL00        IFEQ      '0'
089300940504     C     DSUOP1        IFEQ      'F06'
089400070509     c     DSUop1        oreq      'F6N'
089500971015     C     DSUOP1        OREQ      'O03'
089600940504     C     VIDDOC        IFEQ      *ZEROS
089700940504     C                   MOVEL     '1'           ERRL00
089800940504     C                   MOVEL     MSG(17)       DSMSMS
089900940504     C                   SETON                                        75
090000940504     C                   END
090100940504     C                   END
090200940504     C                   END
090300020320
090400020320      * CONTROLLO LA DATA DEL DOCUMENTO (SOLO IN INSERIMENTO)
090500940504     C     ERRL00        IFEQ      '0'
090600940504     C     DSUOP1        IFEQ      'F06'
090700070509     c     DSUop1        oreq      'F6N'
090800971015     C     DSUOP1        OREQ      'O03'
090900940504     C     VIDDDO        IFNE      *ZEROS
091000971010     C                   Z-ADD     VIDDDO        WDAT
091100940504     C                   EXSR      CHKDAT
091200971013     C                   Z-ADD     WDAT          VIDDDO
091300940504     C     ERRL00        IFEQ      '1'
091400940504     C                   SETON                                        76
091500940504     C                   END
091600940504     C                   ELSE
091700940504     C                   MOVEL     '1'           ERRL00
091800940504     C                   MOVEL     MSG(8)        DSMSMS
091900940504     C                   SETON                                        76
092000940504     C                   END
092100940504     C                   END
092200940504     C                   END
092300120521     c
092400120521     C     DSUOP1        IFne      'F06'                                        *INS.
092500120521     C     DSUOP1        andne     'O03'                                        *DUPLICA
092600120521     C                   SETOFF                                       1112
092700120521     c                   endif
092800020320
092900000000     C                   ENDSR
093000020320      *================================================================
093100020320      *  CHKTIP - CONTROLLA IL TIPO DI UNITA
093200020320      *================================================================
093300930709     C     CHKTIP        BEGSR
093400021104
093500021104     c                   Eval      Wmsginfo = *Off
093600020319
093700020319      * CONTROLLO SE E' STATO INSERITO '?'
093800020319     C     '?'           SCAN      VIDTIP                                 99
093900020319
094000020319      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
094100020319     c                   if        *in99                                        --- 1 -->
094200021104     c                   Clear                   dunt
094300020319     c                   clear                   TIBS02DS
094400020319     c                   movel     'R'           t02mod
094500020319     c                   movel     knsif         t02sif
094600020321     c                   movel     'UNT'         t02cod
094700020319     c                   call      'TIBS02R'
094800020319     c                   parm                    KPJBA
094900020319     c                   parm                    TIBS02DS
095000020319     c                   movel     T02ke1        vidtip
095100020322     c                   movel     T02ke1        wvdtip
095200021104     c                   movel     t02uni        dunt
095300021104     c                   movel     d§untdes      vddtip
095400020319     c                   endif                                                  <-- 1 ---
095500020319
095600020319      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
095700021104     c                   Clear                   dunt
095800020319     c                   clear                   TIBS02DS
095900020319     c                   movel     'C'           T02mod
096000020319     c                   movel     knsif         t02sif
096100020321     c                   movel     'UNT'         t02cod
096200020320     c                   movel(p)  vidtip        T02ke1
096300020322     c                   move      vidtip        wvdtip
096400020319     c                   call      'TIBS02R'
096500020319     c                   parm                    KPJBA
096600020319     c                   parm                    TIBS02DS
096700020319     c                   if        t02err <> *blanks
096800020319     c                   eval      errl00 = '1'
096900020319     c                   clear                   vddtip
097000020319     c                   else
097100021104     c                   movel     t02uni        dunt
097200021104     c                   movel     d§untdes      vddtip
097300020319     c                   endif
097400021104
097500031204      * controllo flag x msg info
097600021104     c                   If        Errl00 = *Off  and  d§untfl1 = 'S'
097700021104     c                             and *In78 = *Off
097800070509     c                             and (DSUop1 <> 'F6N'
097900070509     c                              or  SDSprm <= 1)
098000021104     c                   Eval      Wmsginfo = *On
098100021104     c                   Eval      Errl00 = *On
098200021104     c                   EndIf
098300020319
098400930709     C                   ENDSR
098500020320      *================================================================
098600020320      *  CHKRG2 - CONTROLLA IL SECONDO LIVELLO DI RAGGRUPPAMENTO
098700020320      *================================================================
098800020320     c     chkrg2        begsr
098900020320
099000020320      * controllo se è stato inserito '?'
099100020320     c     '?'           scan      vidrg2                                 99
099200020320
099300020320      * è stato inserito '?', lancio il pgm di interr. tabelle
099400020320     c                   if        *in99                                        --- 1 -->
099500020320     c                   clear                   TIBS02DS
099600020320     c                   movel     'R'           t02mod
099700020320     c                   movel     knsif         t02sif
099800020321     c                   movel     'UNR'         t02cod
099900020320     c                   call      'TIBS02R'
100000020320     c                   parm                    KPJBA
100100020320     c                   parm                    TIBS02DS
100200020320     c                   movel     T02ke1        vidrg2
100300020320     c                   movel     t02uni        vddrg2
100400020320     c                   endif                                                  <-- 1 ---
100500020320
100600020320      * non è stato inserito '?', controllo che esista in tabella
100700020320     c                   clear                   TIBS02DS
100800020320     c                   movel     'C'           T02mod
100900020320     c                   movel     knsif         t02sif
101000020321     c                   movel     'UNR'         t02cod
101100020320     c                   movel(p)  vidrg2        T02ke1
101200020320     c                   call      'TIBS02R'
101300020320     c                   parm                    KPJBA
101400020320     c                   parm                    TIBS02DS
101500020320     c                   if        t02err <> *blanks
101600020320     c                   eval      errl00 = '1'
101700020320     c                   clear                   vddrg2
101800020320     c                   else
101900020320     c                   movel     t02uni        vddrg2
102000020320     c                   endif
102100020320
102200020320     c                   endsr
102300020320      *================================================================
102400020320      *  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
102500020320      *================================================================
102600930709     C     CHKMAG        BEGSR
102700020320
102800020320      * CONTROLLO SE E' STATO INSERITO '?'
102900130404     C**   '?'           SCAN      WMAG                                   99
103000020320
103100020320      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
103200130404     C**   *IN99         IFEQ      '1'                                          --- 1 -->
103300130404     C**                 MOVEL     *BLANKS       §COD1             3
103400130404     C**                 MOVE      *BLANKS       §DES1
103500130404     C**                 CALL      'TNSD20R'
103600130404     C**                 PARM                    §COD1
103700130404     C**                 PARM                    §TIP              1
103800130404     C**                 PARM                    §DES1            25
103900130404     C**                 MOVEL     §COD1         WMAG              3
104000130404     C*+                 MOVEL     §DES1         WDMAG            30
104100020320
104200130404     C**                 END                                                    <-- 1 ---
104300020320
104400020320      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
104500130404     C**                 MOVE      WMAG          ORGFIL
104600130404     C**   KEYORG        CHAIN     AZORG21l                           99
104700130404     C**   *IN99         IFEQ      '0'                                          --- 2 -->
104800130404     C**                 MOVEL     ORGFIL        WMAG
104900130404     C**                 MOVEL     ORGDES        WDMAG
105000130404     C*+                 ELSE                                                   <-- 2 -->
105100130404     c**   keyorg        chain     azorg01l
105200130404     c**                 if        %found(azorg01l)
105300130404     c**                 movel     orgfil        wmag
105400130404     c**                 movel     orgdes        wdmag
105500130404     c*+                 else
105600130404     C**                 MOVEL     '1'           ERRL00
105700130404     C**                 MOVEL     *ALL'*'       WDMAG
105800130404     c**                 endif
105900130404     C**                 END                                                    <-- 2 ---
106000020320
106100130404     c                   clear                   tnsdmagds
106200130404     c                   eval      §magmag=wmag
106300130409     c****               eval      §imagtorg='E  '
106400130409     c                   eval      §imagtorg=wtorg
106500130404     c                   call      'TNSDMAGR'
106600130404     c                   parm                    kpjba
106700130404     c                   parm                    tnsdmagds
106800130404     c
106900130404     c* Eseguita Ricerca
107000130404    1c                   if        §omagerr='R'
107100130404     c                   eval      wmag=§magmag
107200130404     c                   eval      wdmag=§omagdes
107300130404   x1c                   else
107400130404     c* Errore nel magazzino
107500130404    2c                   if        §omagerr='E'
107600130404     C                   MOVEL     '1'           ERRL00
107700130404     C                   MOVEL     *ALL'*'       WDMAG
107800130404     c* se tutto ok prendo decodifica e flag
107900130404   x2c                   else
108000130404     c                   eval      wdmag=§omagdes
108100130404    2c                   endif
108200130404    1c                   endif
108300130404     c
108400930709     C                   ENDSR
108500020320      *================================================================
108600020320      * CHKDAT - CONTROLLA LA DATA
108700020320      *================================================================
108800930709     C     CHKDAT        BEGSR
108900020320
109000971010     C                   Z-ADD     WDAT          G08DAT
109100971010     C                   Z-ADD     *ZEROS        G08INV
109200971010     C                   MOVEL     '0'           G08ERR
109300971010     C                   CALL      'XSRDA8'
109400971010     C                   PARM                    WLBDA8
109500971013     C                   Z-ADD     G08DAT        WDAT
109600971010     C     G08ERR        IFNE      '0'
109700930709     C                   MOVEL     '1'           ERRL00
109800930709     C                   MOVEL     MSG(12)       DSMSMS
109900930709     C                   END
110000020320
110100930709     C                   ENDSR
110200020320      *================================================================
110300020320      * CHKKSC - CONTROLLA IL CLIENTE
110400020320      *================================================================
110500020320     c     chkksc        begsr
110600020320
110700020320      * se non inserito il codice ma inserita una parte della ragione sociale
110800020320      * richiamo il programma della ricerca alfabetica
110900020320     c                   if        vidksc = *zeros and vddksc <> *blanks
111000020320     c                   exsr      ricalf
111100020320     c                   endif
111200020320
111300020320      * se inserito il codice controllo che esista sul piano dei conti
111400020320     c                   if        vidksc <> *zeros
111500020327     c                   clear                   TIBS69DS
111600020327     c                   z-add     vidksc        I69kac
111700020327     c                   call      'TIBS69R'
111800020327     c                   parm                    tibs69DS
111900020327     c                   parm                    ds_cnaco
112000020327     c                   parm                    ds_cnind
112100020327     c                   parm                    ds_cnclp
112200020327     c                   parm                    ds_fncls
112300020327
112400020327     c                   if        o69err <> '1'
112500020327     c                   movel     acorag        vddksc
112600020327     c                   else
112700020320     c                   eval      errl00 = *on
112800020320     c                   endif
112900020320     c                   endif
113000020320
113100020321     c                   endsr
113200020320      *================================================================
113300020320      *  RICALF - RICERCA ALFABETICA
113400020320      *================================================================
113500020320     c     ricalf        begsr
113600020320
113700020320     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
113800080922     C****               Z-ADD     9             PAXsta                         9=Escludo Annullati
113900080922     c* visulizzare anche i bloccati
114000080922     C                   Z-ADD     0             PAXsta                         9=Escludo Annullati
114100020709     C                   Z-ADD     DutKci        PAXccc                         Capoconto Clienti
114200020320     c                   movel(P)  vddksc        PAXdmt
114300020320     C                   z-add     1             PAXnum
114400020320
114500020320     C                   CALL      'XALFA3BR'
114600020320     C                   PARM                    PAXDUT
114700020320     C                   PARM                    CODUT
114800020320     C                   PARM                    PAXDMT
114900020320     C                   PARM                    PAXCCC
115000020320     C                   PARM                    PAXSTA
115100020320     C                   PARM      *blanks       PAXFLR
115200020320     C                   PARM      *blanks       PAXDIT
115300020320     C                   PARM                    PAXNUM            2 0
115400020320     C                   PARM                    PAXKCM           80
115500020320     C                   PARM                    PAXKSM          140
115600020320     C                   PARM                    PAXKDM           60
115700020320
115800020320      * NON TROVATO O PREMUTO CMD12
115900020320    2C     paxsta        IFGT      -1
116000020320     C                   MOVEL     PAXKSM        vidksc
116100020320    2C                   ENDIF
116200020320
116300020320     c                   endsr
116400930201     C*================================================================
116500930709     C*  RECVID - PORTA I CAMPI DAL RECORD AL VIDEO
116600930201     C*================================================================
116700930709     C     RECVID        BEGSR
116800930201     C*
116900930709     C                   MOVE      UNATIP        VIDTIP
117000930709     C                   Z-ADD     UNACOD        VIDCOD
117100930709     C                   MOVEL     UNAMAR        VIDMAR
117200930709     C                   MOVEL     UNAMOD        VIDMOD
117300930709     C                   MOVEL     UNAMAT        VIDMAT
117400020403     C                   MOVEL     UNAMOD        savMOD
117500020403     C                   MOVEL     UNAMAT        savMAT
117600930709     C                   MOVEL     UNADES        VIDDES
117700930709     C                   MOVEL     UNANOT        VIDNOT
117800971010     C                   Z-ADD     UNADCN        G08INV
117900971010     C                   Z-ADD     *ZEROS        G08DAT
118000971010     C                   MOVEL     '3'           G08ERR
118100971010     C                   CALL      'XSRDA8'
118200971010     C                   PARM                    WLBDA8
118300971010     C                   Z-ADD     G08DAT        VIDDCN
118400971010     C                   Z-ADD     UNADSG        G08INV
118500971010     C                   Z-ADD     *ZEROS        G08DAT
118600971010     C                   MOVEL     '3'           G08ERR
118700971010     C                   CALL      'XSRDA8'
118800971010     C                   PARM                    WLBDA8
118900971010     C                   Z-ADD     G08DAT        VIDDSG
119000930709     C                   MOVEL     UNASIS        VIDSIS
119100930709     C                   MOVE      UNADMS        VIDDMS
119200930709     C                   MOVE      UNADMF        VIDDMF
119300971010     C                   Z-ADD     UNADSM        G08INV
119400971010     C                   Z-ADD     *ZEROS        G08DAT
119500971010     C                   MOVEL     '3'           G08ERR
119600971010     C                   CALL      'XSRDA8'
119700971010     C                   PARM                    WLBDA8
119800971010     C                   Z-ADD     G08DAT        VIDDSM
119900930709     C                   MOVEL     UNALEA        VIDLEA
120000971010     C                   Z-ADD     UNADSL        G08INV
120100971010     C                   Z-ADD     *ZEROS        G08DAT
120200971010     C                   MOVEL     '3'           G08ERR
120300971010     C                   CALL      'XSRDA8'
120400971010     C                   PARM                    WLBDA8
120500971010     C                   Z-ADD     G08DAT        VIDDSL
120600930709     C                   MOVE      UNAMAG        VIDMAG
120700120411     C***                MOVEL     UNAEMU        VIDEMU
120800120412     c                   eval      videmu=%subst(unaemu:7:9)
120900120411     c
121000120411     c* Nel campo UNAEMU nelle prime 5 posizioni verrà memorizzato il numero ordine
121100120411     c                   clear                   alfaord
121200120412     c                   if        %subst(unaemu:1:6)>*zeros
121300120412     c                   eval      alfaord=%subst(unaemu:1:6)
121400120411     c                   movel     alfaord       vidord
121500120411     c                   else
121600120411     c                   clear                   vidord
121700120411     c                   endif
121800120411     c
121900930709     C                   MOVEL     UNAMVD        VIDMVD
122000150514     C                   CLEAR                   WLBDA8
122100150511     c                   IF        %subst(UNAMDR:8:8) <> *blank
122200150511     c                   eval      G08inv = %dec(%subst(UNAMDR:8:8):8:0)
122300150511     C                   Z-ADD     *ZEROS        G08DAT
122400150511     C                   MOVEL     '3'           G08ERR
122500150511     C                   CALL      'XSRDA8'
122600150511     C                   PARM                    WLBDA8
122700150514     C                   ENDIF
122800150511     C                   Z-ADD     G08DAT        VIDMDR2
122900930709     C                   MOVEL     UNAMTA        VIDMTA
123000930709     C                   MOVEL     UNAMDF        VIDMDF
123100930709     C                   MOVEL     UNAMRA        VIDMRA
123200150511     C                   EVAL      VIDMDR = %subst(UNAMDR:1:7)
123300930709     C                   MOVEL     UNAX25        VIDX25
123400930709     C                   MOVEL     UNALSW        VIDLSW
123500100903     C                   MOVE      UNALSW        VIDRESO
123600110104     C                   MOVE      UNALSW        savreso           1
123700020319     c                   move      unarg2        vidrg2
123800020319     c                   move      unaksc        vidksc
123900101027     c* Data dal reso
124000101027     C                   Z-ADD     UNADTR        G08INV
124100101027     C                   Z-ADD     *ZEROS        G08DAT
124200101027     C                   MOVEL     '3'           G08ERR
124300101027     C                   CALL      'XSRDA8'
124400101027     C                   PARM                    WLBDA8
124500101027     C                   Z-ADD     G08DAT        VIDdalr
124600930202     C*
124700930709     C     UNAATB        IFEQ      'A'
124800930202     C                   MOVEL     'ANNULLAT'    VIDATB
124900930202     C                   MOVE      'O'           VIDATB
125000930202     C                   ELSE
125100930202     C                   MOVEL     *BLANKS       VIDATB
125200930202     C                   END
125300930201     C*
125400930201     C                   ENDSR
125500930201     C*================================================================
125600930201     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
125700930201     C*================================================================
125800930201     C     AZZVID        BEGSR
125900930201     C*
126000120521     C                   SETOFF                                       1112
126100930709     C                   MOVEL     *BLANKS       VIDTIP
126200930709     C                   Z-ADD     *ZEROS        VIDCOD
126300930709     C                   MOVEL     *BLANKS       VIDMAR
126400930709     C                   MOVEL     *BLANKS       VIDMOD
126500930709     C                   MOVEL     *BLANKS       VIDMAT
126600020320     c                   clear                   vidrg2
126700020320     c                   clear                   vddrg2
126800930709     C                   MOVEL     *BLANKS       VIDDES
126900930709     C                   MOVEL     *BLANKS       VIDNOT
127000930709     C                   Z-ADD     *ZEROS        VIDDCN
127100930709     C                   Z-ADD     *ZEROS        VIDDSM
127200930709     C                   Z-ADD     *ZEROS        VIDDSG
127300930709     C                   Z-ADD     *ZEROS        VIDDSL
127400930709     C                   MOVEL     *BLANKS       VIDSIS
127500930709     C                   MOVEL     *BLANKS       VIDDMS
127600930709     C                   MOVEL     *BLANKS       VIDDMF
127700930709     C                   MOVEL     *BLANKS       VIDLEA
127800930709     C                   MOVEL     *BLANKS       VIDMAG
127900020320     c                   clear                   vidksc
128000020320     c                   clear                   vddksc
128100930709     C                   MOVEL     *BLANKS       VIDEMU
128200930709     C                   MOVEL     *BLANKS       VIDMVD
128300930709     C                   MOVEL     *BLANKS       VIDMTA
128400930709     C                   MOVEL     *BLANKS       VIDMDF
128500930709     C                   MOVEL     *BLANKS       VIDMRA
128600930709     C                   MOVEL     *BLANKS       VIDMDR
128700930709     C                   MOVEL     *BLANKS       VIDX25
128800930709     C                   MOVEL     *BLANKS       VIDLSW
128900100903     C                   MOVEL     *BLANKS       VIDRESO
129000110104     C                   MOVEL     *BLANKS       savRESO
129100940504     C                   Z-ADD     *ZEROS        VIDDOC
129200940504     C                   Z-ADD     *ZEROS        VIDDDO
129300101027     C                   Z-ADD     *ZEROS        VIDdalr
129400120411     C                   Z-ADD     *ZEROS        VIDord
129500930201     C*
129600930201     C                   ENDSR
129700070509      *===============================================================*
129800070509      *  RIEVID - IMPOSTAZIONE CAMPI A VIDEO CON QUELLI RICEVUTI      *
129900070509      *===============================================================*
130000070509     c     RieVid        BEGSR
130100070509      *
130200070509      * Programma richiamato da TNSDUKR x inserimento multiplo unità
130300070509      *   da codici a barre precedentemente inseriti in UNAIM00F
130400070509     c                   move      UIMtip        VIDtip
130500070509     c                   eval      VIDcod  = UIMcod
130600070509     c                   eval      VIDmar  = UIMmar
130700070509     c                   eval      VIDmod  = UIMmod
130800070509     c                   eval      VIDmat  = UIMmat
130900070509     c                   move      UIMrg2        VIDrg2
131000070509     c                   eval      VIDdes  = UIMdes
131100070509     c                   eval      VIDnot  = UIMnot
131200070509     c***                eval      VIDdcn  = UIMdcn
131300070509     c                   clear                   WLBda8
131400070509     c                   eval      G08inv  = UIMdcn
131500070509     c                   eval      G08err  = '3'
131600070509     c                   call      'XSRDA8'
131700070509     c                   parm                    WLBda8
131800070509     c                   eval      VIDdcn  = G08dat
131900070509     c***                eval      VIDdsm  = UIMdsm
132000070509     c                   clear                   WLBda8
132100070509     c                   eval      G08inv  = UIMdsm
132200070509     c                   eval      G08err  = '3'
132300070509     c                   call      'XSRDA8'
132400070509     c                   parm                    WLBda8
132500070509     c                   eval      VIDdsm  = G08dat
132600070509     c***                eval      VIDdsg  = UIMdsg
132700070509     c                   clear                   WLBda8
132800070509     c                   eval      G08inv  = UIMdsg
132900070509     c                   eval      G08err  = '3'
133000070509     c                   call      'XSRDA8'
133100070509     c                   parm                    WLBda8
133200070509     c                   eval      VIDdsg  = G08dat
133300070509     c***                eval      VIDdsl  = UIMdsl
133400070509     c                   clear                   WLBda8
133500070509     c                   eval      G08inv  = UIMdsl
133600070509     c                   eval      G08err  = '3'
133700070509     c                   call      'XSRDA8'
133800070509     c                   parm                    WLBda8
133900070509     c                   eval      VIDdsl  = G08dat
134000070509     c                   move      UIMsis        VIDsis
134100070509     c                   move      UIMlea        VIDlea
134200070509     c                   move      UIMdms        VIDdms
134300070509     c                   move      UIMdmf        VIDdmf
134400070509     c                   move      UIMmag        VIDmag
134500070509     c                   eval      VIDksc  = UIMksc
134600070509     c                   eval      VIDmdf  = UIMmdf
134700070509     c                   eval      VIDmra  = UIMmra
134800070509     c                   eval      VIDmdr  = UIMmdr
134900070509     c                   eval      VIDx25  = UIMx25
135000070509     c                   eval      VIDlsw  = UIMlsw
135100100907     c                   clear                   vidreso
135200101027     c                   clear                   viddalr
135300070509     c                   eval      VIDmvd  = UIMmvd
135400070509     c                   eval      VIDmta  = UIMmta
135500120412     c                   eval      VIDemu  = %subst(UIMemu:7:9)
135600120412     c                   if        %subst(UIMemu:1:6)>*zeros
135700120412     c                   movel     UIMemu        vidord
135800120412     c                   endif
135900070509     c                   eval      VIDdoc  = UIMdoc
136000070509     c***                eval      VIDddo  = UIMddo
136100070509     c                   clear                   WLBda8
136200070509     c                   eval      G08inv  = UIMddo
136300070509     c                   eval      G08err  = '3'
136400070509     c                   call      'XSRDA8'
136500070509     c                   parm                    WLBda8
136600070509     c                   eval      VIDddo  = G08dat
136700070509      *
136800070509     c                   ENDSR
136900971015     C*================================================================
137000971015     C* DUPVID - REIMPOSTA IL VIDEO PER LA DUPLICA
137100971015     C*================================================================
137200971015     C     DUPVID        BEGSR
137300971015     C*
137400971015     C* LEGGE IL RECORD DA DUPLICARE
137500020322     c                   z-add     dsutip        unatip
137600971015     C                   Z-ADD     DSUCOD        UNACOD
137700971015     C     KEYUNA        CHAIN     UNANA01L                           99
137800971015     C     *IN99         IFEQ      *OFF                                         *TROVATO
137900971015     C                   EXSR      RECVID                                       *RIEMPE CAMPI
138000971015     C                   ELSE                                                   *NON TROVATO
138100971015     C                   EXSR      AZZVID                                       *AZZERA CAMPI
138200971015     C                   ENDIF
138300971015     C*
138400971015     C* REIMPOSTA I CAMPI DA NON DUPLICARE
138500971015     C                   Z-ADD     *ZEROS        VIDCOD                         *CHIAVE
138600971015     C                   MOVEL     *BLANKS       VIDATB                         *ANNULLAMENTO
138700120521     c                   clear                   vidord
138800971015     C*
138900971015     C                   ENDSR
139000930201     C*================================================================
139100930709     C*  VIDREC - IMPOSTA CAMPI DA VIDEO A RECORD
139200930201     C*================================================================
139300930709     C     VIDREC        BEGSR
139400930201     C*
139500930709     C                   MOVE      VIDTIP        UNATIP
139600930709     C                   Z-ADD     VIDCOD        UNACOD
139700930709     C                   MOVEL     VIDMAR        UNAMAR
139800930709     C                   MOVEL     VIDMOD        UNAMOD
139900930709     C                   MOVEL     VIDMAT        UNAMAT
140000020320     c                   movel     vidrg2        unarg2
140100930709     C                   MOVEL     VIDDES        UNADES
140200930709     C                   MOVEL     VIDNOT        UNANOT
140300971010     C                   Z-ADD     VIDDCN        G08DAT
140400971010     C                   Z-ADD     *ZEROS        G08INV
140500971010     C                   MOVEL     '0'           G08ERR
140600971010     C                   CALL      'XSRDA8'
140700971010     C                   PARM                    WLBDA8
140800971010     C                   Z-ADD     G08INV        UNADCN
140900971010     C                   Z-ADD     VIDDSG        G08DAT
141000971010     C                   Z-ADD     *ZEROS        G08INV
141100971010     C                   MOVEL     '0'           G08ERR
141200971010     C                   CALL      'XSRDA8'
141300971010     C                   PARM                    WLBDA8
141400971010     C                   Z-ADD     G08INV        UNADSG
141500930709     C                   MOVEL     VIDSIS        UNASIS
141600930709     C                   MOVE      VIDDMS        UNADMS
141700930709     C                   MOVE      VIDDMF        UNADMF
141800971010     C                   Z-ADD     VIDDSM        G08DAT
141900971010     C                   Z-ADD     *ZEROS        G08INV
142000971010     C                   MOVEL     '0'           G08ERR
142100971010     C                   CALL      'XSRDA8'
142200971010     C                   PARM                    WLBDA8
142300971010     C                   Z-ADD     G08INV        UNADSM
142400930709     C                   MOVEL     VIDLEA        UNALEA
142500971010     C                   Z-ADD     VIDDSL        G08DAT
142600971010     C                   Z-ADD     *ZEROS        G08INV
142700971010     C                   MOVEL     '0'           G08ERR
142800971010     C                   CALL      'XSRDA8'
142900971010     C                   PARM                    WLBDA8
143000971010     C                   Z-ADD     G08INV        UNADSL
143100930709     C                   MOVE      VIDMAG        UNAMAG
143200020320     c                   z-add     vidksc        unaksc
143300120411     C*****              MOVEL     VIDEMU        UNAEMU
143400120411     c                   clear                   unaemu
143500120412     c                   eval      %subst(unaemu:7:9)=videmu
143600120411     c                   if        vidord>0
143700120412     c                   eval      %subst(unaemu:1:6)=%editc(vidord:'X')
143800120411     c                   else
143900120521     c                   eval      %subst(unaemu:1:6)='      '
144000120411     c                   endif
144100120411     c
144200930709     C                   MOVEL     VIDMVD        UNAMVD
144300930709     C                   MOVEL     VIDMTA        UNAMTA
144400930709     C                   MOVEL     VIDMDF        UNAMDF
144500930709     C                   MOVEL     VIDMRA        UNAMRA
144600930709     C                   MOVEL     VIDMDR        UNAMDR
144700930709     C                   MOVEL     VIDX25        UNAX25
144800930709     C                   MOVEL     VIDLSW        UNALSW
144900100903     C                   MOVE      VIDreso       UNALSW
145000101027     c
145100101027     c* se tolta la S di reso, tolgo anche la data
145200101027     c                   if        vidreso=*blanks
145300101027     c                   clear                   unadtr
145400101027     c                   else
145500101027     c* se data piena la reimposto altrimenti metto udate
145600110104     c*  Metto udate anche se reimposto il flag di reso
145700110104     c                   if        viddalr>0  and savreso=vidreso
145800101027     C                   Z-ADD     VIDdalr       G08DAT
145900101027     C                   Z-ADD     *ZEROS        G08INV
146000101027     C                   MOVEL     '0'           G08ERR
146100101027     C                   CALL      'XSRDA8'
146200101027     C                   PARM                    WLBDA8
146300101027     C                   Z-ADD     G08INV        UNADTR
146400101027     c                   else
146500101027     c                   movel     datcor        UNADTR
146600101027     c                   endif
146700101027     c                   endif
146800930201     C*
146900930201     C                   ENDSR
147000000000     C*================================================================
147100000000     C*  RUTINZ - ROUTINE INIZIALE
147200000000     C*================================================================
147300000000     C     RUTINZ        BEGSR
147400930201     C*
147500930201     C* RICEVIMENTO PARAMETRI
147600930201     C     *ENTRY        PLIST
147700940105     C                   PARM                    KPJBA
147800070503     C                   PARM                    UNANAds
147900070503     C                   PARM                    UNMOVds
148000940105     C                   MOVEL     KPJBU         DSUNIT
148100070503      * - impostazione iniziale dei parametri di output per chiamante
148200070503     c                   eval      DSUf03 = *off                                PREMUTO F03
148300070503     c                   eval      DSUerr = *off                                '1'=ERRORE
148400070503     c                   eval      DSUmsg = *blanks                             MESSAGGIO
148500930201     C*
148600930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
148700930201     C     XMSGPA        PLIST
148800930201     C                   PARM                    DSMSMS
148900930201     C                   PARM                    DSMSPG
149000930201     C                   PARM                    DSMSKE
149100930201     C                   PARM                    DSMSER
149200971013     C* DEFINIZIONI VARIABILI
149300020322     c     *like         define    unmtip        kumtip
149400971013     C     *LIKE         DEFINE    UNMCOD        KUMCOD
149500930201     C*
149600020320      * CHIAVE LETTURA UNANA01L
149700930709     C     KEYUNA        KLIST
149800020322     c                   kfld                    unatip
149900930709     C                   KFLD                    UNACOD
150000020320
150100020320     c     keyun8        klist
150200020320     c                   kfld                    kunmod
150300020320     c                   kfld                    kunmat
150400091113     c     kana2         klist
150500091113     c                   kfld                    kuntip
150600091113     c                   kfld                    kunmat
150700020320
150800020320      * CHIAVE LETTURA UNMOV01L
150900971013     C     KEYMOV        KLIST
151000020322     c                   kfld                    kumtip
151100971013     C                   KFLD                    KUMCOD
151200020320
151300930201     C*
151400020529     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
151500930201     C     KEYORG        KLIST
151600930709     C                   KFLD                    ORGFIL
151700930201     C*
151800930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
151900930201     C                   CALL      'XKEY'
152000930201     C                   PARM                    DSKEY
152100930201     C*
152200930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
152300930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
152400930201     C*
152500930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
152600930201     C*   -NOME PGM CHIAMANTE
152700930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
152800941115     C                   MOVEL     'TNSDU2R'     DSMSPG
152900941115     C                   MOVEL     'TNSDU2R'     MSGQUE
153000930201     C                   MOVEL     *BLANKS       MSGKEY
153100930201     C                   MOVEL     *BLANKS       DSMSMS
153200930202     C*
153300930202     C* SE VISUALIZZAZIONE TOLGO FUNZIONE AGGIORNAMENTO
153400930709     C     DSUOP1        IFEQ      'O05'
153500930202     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
153600930202     C                   MOVEL     'N'           FUDATT
153700930202     C                   MOVEL     FUDRIG        FUD(6)
153800120411     c
153900120411     C                   MOVEL     FUD(10)       FUDRIG                         *sblocca ordine
154000120411     C                   MOVEL     'N'           FUDATT
154100120411     C                   MOVEL     FUDRIG        FUD(10)
154200930202     C                   END
154300940105     C*
154400940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DEI MOVIMENTI UNITA'
154500940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
154600940105     C     DSUPRO        IFEQ      'M'
154700940105     C                   MOVEL     FUD(8)        FUDRIG                         *GEST.MOVIMENTI
154800940105     C                   MOVEL     'N'           FUDATT
154900940105     C                   MOVEL     FUDRIG        FUD(8)
155000940105     C                   END
155100930201     C*
155200930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
155300930201     C                   Z-ADD     0             FUNGRU            5 0
155400930201     C*
155500930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
155600930201     C                   EXSR      FUNGES
155700930201     C*
155800930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
155900930201     C                   MOVEL     '0'           F12L00            1
156000930201     C                   MOVEL     '0'           F03L00            1
156100930201     C                   MOVEL     '0'           FINL00            1
156200930201     C                   MOVEL     '0'           ERRL00            1
156300930201     C                   MOVEL     '0'           FLFINE            1
156400930204     C*
156500971010     C* REPERIMENTO DATI UTENTE
156600020709
156700020709     c     *dtaara       define    §azute        azuteds
156800020709     c     *dtaara       define    §datiute      ddatiute
156900020709
157000020709     c                   in(E)     *dtaara
157100020709     c                   if        %Error or rsut = *blanks
157200020709     c                   clear                   tibs34ds
157300020709     c                   call      'TIBS34R'
157400020709     c                   parm                    Tibs34Ds
157500020709     c                   in        *dtaara
157600020709     c                   endif
157700971010     C*
157800971010     C* REPERISCE LA DATA CORRENTE
157900971010     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
158000971010     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
158100971010     C                   Z-ADD     WN8           G08DAT
158200971010     C                   Z-ADD     *ZEROS        G08INV
158300971010     C                   MOVEL     '0'           G08ERR
158400971010     C                   CALL      'XSRDA8'
158500971010     C                   PARM                    WLBDA8
158600971010     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
158700930201     C*
158800930201     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
158900930201     C* 0=NON PROTETTO, 1=PROTETTO
159000930709     C*   90 - DATI FILE
159100070503     C*   91 - CAMPI CHIAVE
159200940504     C*   92 - CAMPI NUM. E DATA DOC.
159300070503     C*   93 - NUMERO MATRICOLA
159400070503      *
159500070503     c                   eval      *in93 = *off
159600070503      *
159700930201     C* --> UTENTE ENTRATO IN GESTIONE
159800070503if  1C     DSUOP0        IFEQ      ' 2'                                         --- 1 -->
159900930201     C*
160000930709     C     DSUOP1        IFEQ      'F06'                                        *INS.
160100070509     c     DSUop1        oreq      'F6N'                                        *DUPLICA
160200971015     C     DSUOP1        OREQ      'O03'                                        *DUPLICA
160300940504     C                   SETOFF                                       909192
160400120521     C                   END
160500930201     C*
160600070503if  2C     DSUOP1        IFEQ      'O02'                                        *MOD.
160700930709     C                   SETON                                        91
160800020709     c                   If        DutLpo = 'S'
160900930709     C                   SETOFF                                       90
161000930201     C                   ELSE                                                   *FIL.
161100930709     C                   SETON                                        90
161200930201     C                   END
161300070503e   2C                   END
161400930201     C*
161500930709     C     DSUOP1        IFEQ      'O05'                                        *VIS+CANC+RIPR
161600930709     C     DSUOP1        OREQ      'O04'
161700930709     C     DSUOP1        OREQ      'O07'
161800940504     C                   SETON                                        909192
161900930201     C                   END
162000930201     C*
162100070503e   1C                   END                                                    <-- 1 ---
162200930201     C* --> UTENTE ENTRATO IN INTERROGAZIONE
162300940105     C     DSUOP0        IFEQ      ' 5'
162400940504     C                   SETON                                        909192
162500930201     C                   END
162600930201     C*
162700930709     C* SE SONO IN MOD. O VIS. O CANC. O RIPR. LEGGO RECORD
162800070503if  1C     DSUOP1        IFEQ      'O02'                                        --- 1 -->
162900971015     C     DSUOP1        OREQ      'O04'
163000930709     C     DSUOP1        OREQ      'O05'
163100930709     C     DSUOP1        OREQ      'O07'
163200930709     C* SE IL REC.NON ESISTE RIMANDO ERRORE AL PGM CHIAMANTE
163300020322     c                   z-add     dsutip        unatip
163400930709     C                   Z-ADD     DSUCOD        UNACOD
163500150604     C* se non siamo in visualizzazione, alloco il rcd
163600150604     C                   IF        DSUOP1 <> 'O05'
163700930709     C     KEYUNA        CHAIN     UNANA01L                           99
163800150604     C* se siamo in visualizzazione, NON alloco il rcd
163900150604     C                   ELSE
164000150604     C     KEYUNA        CHAIN(N)  UNANA01L                           99
164100150604     C                   ENDIF
164200930201     C     *IN99         IFEQ      '1'
164300930201     C                   MOVEL     '1'           ERRL00
164400930709     C                   MOVEL     MSG(16)       DSMSMS
164500930202     C                   ELSE
164600930709     C                   EXSR      RECVID
164700930201     C                   END
164800930208     C*
164900930208     C* SE TUTTO OK, DECODIFICO I CAMPI A VIDEO
165000930208     C                   EXSR      DECODI
165100930201     C*
165200070503e   1C                   END                                                    <-- 1 ---
165300930202     C*
165400930202     C* SE INSERIMENTO AZZERO TUTTI I CAMPI DEL VIDEO
165500930709     C     DSUOP1        IFEQ      'F06'
165600070509     c     DSUop1        oreq      'F6N'
165700930202     C                   EXSR      AZZVID
165800070509if  1c                   if            (DSUop1  = 'F06'
165900070509     c                              or  DSUop1  = 'F6N')
166000070509     c                             and  SDSprm  > 1
166100070509     c                   exsr      RieVid
166200070509e   1c                   endif
166300930202     C                   END
166400971015     C*
166500971015     C* SE DUPLICA REIMPOSTA IL VIDEO
166600971015     C     DSUOP1        IFEQ      'O03'
166700971015     C                   EXSR      DUPVID
166800971015     C                   ENDIF
166900120521
167000120521     C     DSUOP1        IFEQ      'F06'                                        *INS.
167100120521     C     DSUOP1        OREQ      'O03'                                        *DUPLICA
167200120521     C                   SETOn                                        11
167300120521     C                   END
167400930201     C*
167500930201     C* IMPOSTO DESCRIZIONE DELLA FUNZIONE IN CORSO A VIDEO
167600930709     C     DSUOP1        IFEQ      'F06'
167700070509     c     DSUop1        oreq      'F6N'
167800971015     C     DSUOP1        OREQ      'O03'
167900930201     C                   MOVEL     ' INSERIM'    VIDFUN
168000930201     C                   MOVE      'ENTO   '     VIDFUN
168100930201     C                   END
168200930709     C     DSUOP1        IFEQ      'O02'
168300930201     C                   MOVEL     '   MODIF'    VIDFUN
168400930201     C                   MOVE      'ICA    '     VIDFUN
168500930201     C                   END
168600930709     C     DSUOP1        IFEQ      'O04'
168700930201     C                   MOVEL     ' CANCELL'    VIDFUN
168800930201     C                   MOVE      'AZIONE '     VIDFUN
168900930201     C                   END
169000930709     C     DSUOP1        IFEQ      'O05'
169100930201     C                   MOVEL     'VISUALIZ'    VIDFUN
169200930201     C                   MOVE      'ZAZIONE'     VIDFUN
169300930201     C                   END
169400930709     C     DSUOP1        IFEQ      'O07'
169500930201     C                   MOVEL     ' RIPRIST'    VIDFUN
169600930201     C                   MOVE      'INO    '     VIDFUN
169700930201     C                   END
169800070509      *
169900070509      * SE pgm. richiamato da TNSDUKR per l'immissione di un record
170000070509      *  del quale vengono passati i valori per ogni campo,
170100070509      * ALLORA imposto flag per ritorno al pgm. chiamante
170200070509if  1c                   IF             DSUop1  = 'F6N'
170300070509     c                             and  SDSprm  > 1
170400070509     c                   exsr      INSERI
170500070611     c                   eval      DSUerr = ErrL00                              '1'=ERRORE
170600070611     c                   eval      DSUmsg = DSmsms                              MESSAGGIO
170700070509     c                   eval      FLfine = *on
170800070509e   1c                   ENDIF
170900000000     C*
171000000000     C                   ENDSR
171100000000     C*================================================================
171200000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
171300000000     C*================================================================
171400000000     C     FUNGES        BEGSR
171500000000     C*
171600000000     C* PULISCO CAMPI DI LAVORO
171700000000     C                   Z-ADD     0             W                 5 0
171800000000     C*
171900000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
172000000000     C* E CARICO TABELLA FUNZIONI ATTIVE
172100000000     C     1             DO        24            I
172200000000     C                   MOVEL     FUD(I)        FUDRIG
172300000000     C*
172400000000     C* PULIZIA FUNZIONI PRECEDENTI
172500000000     C                   MOVEL     *BLANKS       FUV(I)
172600000000     C*
172700000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
172800000000     C* (SALTA F24)
172900000000     C     FUDATT        IFEQ      'S'
173000000000     C     FUDFUN        ANDNE     '24'
173100000000     C                   ADD       1             W
173200000000     C                   MOVEL     FUDDES        FUV(W)
173300000000     C                   END
173400000000     C                   END                                                    DO WHILE
173500000000     C*
173600000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
173700000000     C* ALTRIMENTI DISATTIVO F24
173800000000     C     1             DO        24            I
173900000000     C                   MOVEL     FUD(I)        FUDRIG
174000000000     C*
174100000000     C     FUDFUN        IFEQ      '24'
174200000000     C     W             IFGT      6
174300000000     C                   MOVEL     'S'           FUDATT
174400000000     C                   ELSE
174500000000     C                   MOVEL     *BLANKS       FUDATT
174600000000     C                   END                                                    W GT 8
174700000000     C                   MOVEL     FUDRIG        FUD(I)
174800000000     C                   END
174900000000     C                   END                                                    DO WHILE
175000000000     C*
175100000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
175200000000     C     W             DIV       6             I
175300000000     C                   MVR                     Z
175400000000     C*
175500000000     C     Z             IFNE      0
175600000000     C     I             ADD       1             Z
175700000000     C                   ELSE
175800000000     C                   Z-ADD     I             Z                 5 0
175900000000     C                   END
176000000000     C*
176100000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
176200000000     C                   ADD       1             FUNGRU
176300000000     C     FUNGRU        IFGT      Z
176400000000     C     FUNGRU        ORLT      1
176500000000     C                   Z-ADD     1             FUNGRU
176600000000     C                   END
176700000000     C*
176800000000     C* PRIMO GRUPPO DI 6 FUNZIONI
176900000000     C     FUNGRU        IFEQ      1
177000000000     C                   MOVEL     FUV(1)        FUN001
177100000000     C                   MOVEL     FUV(2)        FUN002
177200000000     C                   MOVEL     FUV(3)        FUN003
177300000000     C                   MOVEL     FUV(4)        FUN004
177400000000     C                   MOVEL     FUV(5)        FUN005
177500000000     C                   MOVEL     FUV(6)        FUN006
177600000000     C                   END
177700000000     C*
177800000000     C* SECONDO GRUPPO DI 6 FUNZIONI
177900000000     C     FUNGRU        IFEQ      2
178000000000     C                   MOVEL     FUV(7)        FUN001
178100000000     C                   MOVEL     FUV(8)        FUN002
178200000000     C                   MOVEL     FUV(9)        FUN003
178300000000     C                   MOVEL     FUV(10)       FUN004
178400000000     C                   MOVEL     FUV(11)       FUN005
178500000000     C                   MOVEL     FUV(12)       FUN006
178600000000     C                   END
178700000000     C*
178800000000     C* TERZO GRUPPO DI 6 FUNZIONI
178900000000     C     FUNGRU        IFEQ      3
179000000000     C                   MOVEL     FUV(13)       FUN001
179100000000     C                   MOVEL     FUV(14)       FUN002
179200000000     C                   MOVEL     FUV(15)       FUN003
179300000000     C                   MOVEL     FUV(16)       FUN004
179400000000     C                   MOVEL     FUV(17)       FUN005
179500000000     C                   MOVEL     FUV(18)       FUN006
179600000000     C                   END
179700000000     C*
179800000000     C* QUARTO GRUPPO DI 6 FUNZIONI
179900000000     C     FUNGRU        IFEQ      4
180000000000     C                   MOVEL     FUV(19)       FUN001
180100000000     C                   MOVEL     FUV(20)       FUN002
180200000000     C                   MOVEL     FUV(21)       FUN003
180300000000     C                   MOVEL     FUV(22)       FUN004
180400000000     C                   MOVEL     *BLANKS       FUN005
180500000000     C                   MOVEL     *BLANKS       FUN006
180600000000     C                   END
180700000000     C*
180800000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
180900000000     C                   MOVEL     *BLANKS       FUN007
181000000000     C                   MOVEL     *BLANKS       FUN008
181100000000     C*
181200000000     C     1             DO        24            I
181300000000     C                   MOVEL     FUD(I)        FUDRIG
181400000000     C     FUDFUN        IFEQ      '24'
181500000000     C     FUDATT        ANDEQ     'S'
181600000000     C                   MOVEL     FUDDES        FUN008
181700000000     C                   END
181800000000     C                   END                                                    DO
181900000000     C                   ENDSR
182000000000     C*================================================================
182100000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
182200000000     C*================================================================
182300000000     C     FUNCON        BEGSR
182400000000     C*
182500000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
182600000000     C                   EXSR      FUNVAR
182700000000     C*
182800000000     C* PULISCO CAMPI DI LAVORO
182900000000     C                   Z-ADD     0             I                 5 0
183000000000     C                   Z-ADD     0             W                 5 0
183100000000     C*
183200000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
183300000000     C     FUNKEY        IFNE      *BLANKS
183400000000     C     1             DO        24            I
183500000000     C                   MOVEL     FUD(I)        FUDRIG
183600000000     C*
183700000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
183800000000     C     FUNKEY        IFEQ      FUDFUN
183900000000     C     FUDATT        ANDEQ     'S'
184000000000     C                   Z-ADD     I             W
184100000000     C                   END
184200000000     C                   END                                                    DO WHILE
184300000000     C*
184400000000     C* FUNZIONE NON ATTIVA (ERRORE)
184500000000     C     W             IFEQ      0
184600000000     C*
184700000000     C* EMISSIONE MESSAGGIO DI ERRORE
184800930201     C                   MOVEL     MSG(1)        DSMSMS
184900930201     C*
185000930201     C                   END                                                    W=0
185100000000     C                   END                                                    FUNKEY NE *BLANKS
185200000000     C*
185300000000     C* FUNZIONE ATTIVA (PROCEDI ...)
185400000000     C     W             IFNE      0
185500000000     C     FUNKEY        OREQ      *BLANKS
185600000000     C*
185700000000     C     FUNKEY        IFEQ      *BLANKS
185800000000     C                   EXSR      ENTGES
185900000000     C                   END
186000000000     C     FUNKEY        IFEQ      '01'
186100000000     C                   EXSR      F01GES
186200000000     C                   END
186300000000     C     FUNKEY        IFEQ      '03'
186400000000     C                   EXSR      F03GES
186500000000     C                   END
186600930202     C     FUNKEY        IFEQ      '06'
186700930202     C                   EXSR      F06GES
186800930202     C                   END
186900000000     C     FUNKEY        IFEQ      '08'
187000000000     C                   EXSR      F08GES
187100000000     C                   END
187200120411     C     FUNKEY        IFEQ      '10'
187300120411     C                   EXSR      F10GES
187400120411     C                   END
187500000000     C     FUNKEY        IFEQ      '12'
187600000000     C                   EXSR      F12GES
187700000000     C                   END
187800930201     C     FUNKEY        IFEQ      '21'
187900930201     C                   EXSR      F21GES
188000930201     C                   END
188100000000     C     FUNKEY        IFEQ      '22'
188200000000     C                   EXSR      F22GES
188300000000     C                   END
188400000000     C     FUNKEY        IFEQ      '24'
188500000000     C                   EXSR      F24GES
188600000000     C                   END
188700000000     C     FUNKEY        IFEQ      'HP'
188800000000     C                   EXSR      HELGES
188900000000     C                   END
189000000000     C*
189100000000     C                   END
189200000000     C*
189300000000     C                   ENDSR
189400000000     C*================================================================
189500000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
189600000000     C*================================================================
189700000000     C     FUNVAR        BEGSR
189800000000     C*
189900000000     C* INIZIALIZZA VARIABILE -FUNKEY-
190000000000     C*
190100000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
190200000000     C     KEY           IFEQ      ENTER
190300000000     C                   MOVEL     *BLANKS       FUNKEY
190400000000     C                   END
190500000000     C     KEY           IFEQ      F1
190600000000     C                   MOVEL     '01'          FUNKEY
190700000000     C                   END
190800000000     C     KEY           IFEQ      F2
190900000000     C                   MOVEL     '02'          FUNKEY
191000000000     C                   END
191100000000     C     KEY           IFEQ      F3
191200000000     C                   MOVEL     '03'          FUNKEY
191300000000     C                   END
191400000000     C     KEY           IFEQ      F4
191500000000     C                   MOVEL     '04'          FUNKEY
191600000000     C                   END
191700000000     C     KEY           IFEQ      F5
191800000000     C                   MOVEL     '05'          FUNKEY
191900000000     C                   END
192000000000     C     KEY           IFEQ      F6
192100000000     C                   MOVEL     '06'          FUNKEY
192200000000     C                   END
192300000000     C     KEY           IFEQ      F7
192400000000     C                   MOVEL     '07'          FUNKEY
192500000000     C                   END
192600000000     C     KEY           IFEQ      F8
192700000000     C                   MOVEL     '08'          FUNKEY
192800000000     C                   END
192900000000     C     KEY           IFEQ      F9
193000000000     C                   MOVEL     '09'          FUNKEY
193100000000     C                   END
193200000000     C     KEY           IFEQ      F10
193300000000     C                   MOVEL     '10'          FUNKEY
193400000000     C                   END
193500000000     C     KEY           IFEQ      F11
193600000000     C                   MOVEL     '11'          FUNKEY
193700000000     C                   END
193800000000     C     KEY           IFEQ      F12
193900000000     C                   MOVEL     '12'          FUNKEY
194000000000     C                   END
194100000000     C     KEY           IFEQ      F13
194200000000     C                   MOVEL     '13'          FUNKEY
194300000000     C                   END
194400000000     C     KEY           IFEQ      F14
194500000000     C                   MOVEL     '14'          FUNKEY
194600000000     C                   END
194700000000     C     KEY           IFEQ      F15
194800000000     C                   MOVEL     '15'          FUNKEY
194900000000     C                   END
195000000000     C     KEY           IFEQ      F16
195100000000     C                   MOVEL     '16'          FUNKEY
195200000000     C                   END
195300000000     C     KEY           IFEQ      F17
195400000000     C                   MOVEL     '17'          FUNKEY
195500000000     C                   END
195600000000     C     KEY           IFEQ      F18
195700000000     C                   MOVEL     '18'          FUNKEY
195800000000     C                   END
195900000000     C     KEY           IFEQ      F19
196000000000     C                   MOVEL     '19'          FUNKEY
196100000000     C                   END
196200000000     C     KEY           IFEQ      F20
196300000000     C                   MOVEL     '20'          FUNKEY
196400000000     C                   END
196500000000     C     KEY           IFEQ      F21
196600000000     C                   MOVEL     '21'          FUNKEY
196700000000     C                   END
196800000000     C     KEY           IFEQ      F22
196900000000     C                   MOVEL     '22'          FUNKEY
197000000000     C                   END
197100000000     C     KEY           IFEQ      F23
197200000000     C                   MOVEL     '23'          FUNKEY
197300000000     C                   END
197400000000     C     KEY           IFEQ      F24
197500000000     C                   MOVEL     '24'          FUNKEY
197600000000     C                   END
197700000000     C     KEY           IFEQ      ROLLUP
197800000000     C                   MOVEL     'RU'          FUNKEY
197900000000     C                   END
198000000000     C     KEY           IFEQ      HELP
198100000000     C                   MOVEL     'HP'          FUNKEY
198200000000     C                   END
198300000000     C                   ENDSR
198400000000     C*================================================================
198500000000     C* NOTE PER COMPILAZIONE SCHIERE
198600000000     C*================================================================
198700000000     C* -MSG-
198800000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
198900000000     C* (NON TOCCARE MSG,1)
199000000000     C*
199100000000     C* -CMD-
199200000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
199300000000     C*
199400000000     C* -FUD-
199500000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
199600000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
199700000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
199800000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
199900000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
200000000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
200100000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
200200000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
200300000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
200400000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
200500000000     C*
200600000000     C*================================================================
200700000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
200800941115TNSDU2R- Tasto funzionale non gestito                                          1
200900941115TNSDU2R- Codice già esistente.                                                 2
201000941115TNSDU2R- Inserimento effettuato.                                               3
201100941115TNSDU2R- Modifica effettuata.                                                  4
201200941115TNSDU2R- Cancellazione effettuata.                                             5
201300941115TNSDU2R- Ripristino effettuato.                                                6
201400941115TNSDU2R- Tipo unità non accettabile.                                           7
201500941115TNSDU2R- Data documento obbligatoria in inserimento.                           8
201600941115TNSDU2R- Marca unità non accettabile.                                          9
201700941115TNSDU2R- Modello unità non accettabile.                                       10
201800941115TNSDU2R- Matricola unità non accettabile.                                     11
201900941115TNSDU2R- Data non accettabile.                                                12
202000941115TNSDU2R- Ditta non accettabile.                                               13
202100941115TNSDU2R- Ditta manutenzione non accettabile.                                  14
202200941115TNSDU2R- Magazzino ubicazione non accettabile.                                15
202300941115TNSDU2R- Richiesta gestione di record inesistente.                            16
202400941115TNSDU2R- N. documento obbligatorio in inserimento.                            17
202500020320TNSDU2R- Numeratore non trovato.                                              18
202600020320TNSDU2R- Numeratore già impegnato riprovare più tardi.                        19
202700020320TNSDU2R- Oltrepassato numero massimo di codici.                               20
202800020320TNSDU2R- Esiste già una unità con lo stesso modello e la stessa matricola.    21
202900020320TNSDU2R- Secondo livello di raggruppamento non accettabile.                   22
203000020320TNSDU2R- Cliente non accettabile.                                             23
203100031204TNSDU2R- Hai indicato il tipo di PC ???????????????????                       24
203200091113TNSDU2R- LA matricola deve essere univoca per Tipo Unità!                     25
203300110401Se unità in proprietà alla 923 non posso accettare magazzino ubicazione "990" 26
203400120521Numero ordine obbligatorio in inserimento/copia                               27
203500000000** ======== SCHIERA: CMD  (COMANDI)                        ================
203600000000WRKSPLF
203700000000
203800000000
203900000000
204000000000
204100000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
204200930201                   -        (S/N)=N                        funz.01¬
204300000000                   -        (S/N)=N                        funz.02¬
204400000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
204500000000                   -        (S/N)=N                        funz.04¬
204600000000                   -        (S/N)=N                        funz.05¬
204700930202F06=Aggiornamento  -        (S/N)=S                        funz.06¬
204800930201                   -        (S/N)=N                        funz.07¬
204900971013F08=Movimenti      -        (S/N)=S                        funz.08¬
205000000000                   -        (S/N)=N                        funz.09¬
205100120411F10=Sblocca ORDINE -        (S/N)=S                        funz.10¬
205200000000                   -        (S/N)=N                        funz.11¬
205300930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
205400000000                   -        (S/N)=N                        funz.13¬
205500000000                   -        (S/N)=N                        funz.14¬
205600000000                   -        (S/N)=N                        funz.15¬
205700000000                   -        (S/N)=N                        funz.16¬
205800000000                   -        (S/N)=N                        funz.17¬
205900000000                   -        (S/N)=N                        funz.18¬
206000000000                   -        (S/N)=N                        funz.19¬
206100000000                   -        (S/N)=N                        funz.20¬
206200971013F21=Stampa         -        (S/N)=S                        funz.21¬
206300940105F22=Gestione stampe-        (S/N)=N                        funz.22¬
206400000000                   -        (S/N)=N                        funz.23¬
206500000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
