000100020321     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020321      *------------------------------------------------------------------------*
000300020321      * GESTIONE MOVIMENTI UNITA' EDP - SCELTA
000400020321      *------------------------------------------------------------------------*
000500020321
000600020321      *------------------------------------------------------------------------*
000700020321      * DATA BASE
000800020321      *------------------------------------------------------------------------*
000900941115     FTNSDU5D   CF   E             WORKSTN INFDS(DEVDS1)
001000941115     F                                     SFILE(SDU5DVM:MSGULT)
001100931015     FUNMOV01L  UF A E           K DISK
001200931217     FUNANA01L  UF   E           K DISK
001300100907     FUNANA02L  iF   E           K DISK    rename(unana000:unana002) prefix(G_)
001400100803
001500100803     Funmor01L  UF A E           K DISK
001600101027     FUNgio00F  uF   E             DISK    RENAME(UNgio000:UNgioFIS) prefix(F_)
001700100803
001800100804     ffiapd01l  if   e           k disk
001900020321      *------------------------------------------------------------------------*
002000020321      * SCHIERE
002100020321      *------------------------------------------------------------------------*
002200110401     D MSG             S             78    DIM(27) CTDATA PERRCD(1)             *MESSAGGI
002300930802     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002400930806     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002500020321      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600930803     D FFU             S              6    DIM(31)                              *FUNZIONE
002700930803     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002800930803     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002900100921     d
003000100929     D tipi            S              2    DIM(10)                              *FUNZIONE ATTIVA S/N
003100020321      * TABELLA DESCRIZIONI A VIDEO
003200930802     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003300020321      * TABELLA TASTI FUNZIONALI PREMUTI
003400930802     D KEYS            S              1    DIM(40)
003500020321
003600110401     d w2hdfor         s                   LIKE(apdrsc)
003700110401     d paxccc          s                   LIKE(ACOkcc)
003800020321     d paxdmt          s                   LIKE(ACOrag)
003900020321     d PAXdut          s             30
004000020321     d PAXdit          s              3
004100020321     d PAXflr          s             90
004200020321     d PAXsta          s              1  0
004300020709     d codut           s              1  0 Inz(1)
004400100804     D kpdpdr          S                   LIKE(APDpdr)
004500100804     D kpdtip          S                   LIKE(APDtip)
004600100908     D savTMAG         S              1
004700100929     D wMAG            S              3
004800100929     D wdmag           S             30
004900100929     D wtmag           S              1
005000101027     D wflag           S              1
005100101027     D GIONRR          S              9  0
005200101027     D savnrr          S              9  0
005300101028     D savksc          S              7
005400100804     d Dataiso         s               d   datfmt(*iso)
005500100804     d Dataeur         s               d   datfmt(*eur)
005600100804     d datadmy         s               d   datfmt(*dmy)
005700100804     d datasys         s               d   inz(*sys)
005800101027
005900101027     d wrkgetlista     s           4096    varying
006000020321      *------------------------------------------------------------------------*
006100020321      * INPUT
006200020321      *------------------------------------------------------------------------*
006300020321      * RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
006400930723     D DSFUN         E DS
006500930803     D  FUNRIG                 1     40
006600020321      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006700930127     D DSMSG         E DS
006800020321      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
006900930712     D DSUNIT        E DS
007000110404     D Dgio01        E DS
007100100907      * Parametri ricevuti
007200100907     d dsUNGIO       e ds                  extname(UNGIO00F)
007300930802     D DSKEYS        E DS
007400930803     D  KEYRIG                 1     40
007500930806     D                SDS
007600930806     D  USER                 254    263
007700020321      * RIDEFINIZIONE INFDS DEL FILE VIDEO
007800000000     D DEVDS1          DS
007900000000     D  KEY                  369    369
008000930721     D  CURSOR               370    371
008100930720     D  FRTSFL               378    379B 0
008200020321      *------------------
008300020321      * DS "XSRDA8" - CONTROLLA DATA (8)
008400020321      *------------------
008500971010     D WLBDA8          DS
008600971010     D  G08DAT                 1      8  0
008700971010     D  G08INV                 9     16  0
008800971010     D  G08ERR                17     17
008900971010     D  G08TGI                18     22  0
009000020321
009100020321      * ARCHITETTURA
009200000000     D KPJBA         E DS
009300020321
009400020321     d tibs02ds      e ds
009500020327     d tibs69ds      e ds
009600020327     d ds_cnaco      e ds                  extname(cnaco00f)
009700020327     d ds_cnind      e ds                  extname(cnind00f)
009800020327     d ds_cnclp      e ds                  extname(cnclp00f)
009900020327     d ds_fncls      e ds                  extname(fncls00f)
010000020709
010100020709     d ddatiute      e ds
010200020709     d azuteds       e ds                  extname(azute00f)
010300020709     d tibs34ds      e ds                  inz
010400100804     d
010500100804     d fnlv24ds      e ds                  inz
010600100929     d tnsdmagds     e ds                  inz
010700100921     d DUNM          e ds                  inz
010800110105     d DUNT          e ds                  inz
010900020321
011000100803     d kmgc            s                   like(unrmgc)
011100100907
011200020321      *------------------------------------------------------------------------*
011300020321      * MAIN LINE
011400020321      *------------------------------------------------------------------------*
011500020321
011600931015     C* SE VI E' STATO UN ERRORE NELLA ROUTIN INIZIALE ESCO PGM
011700931015     C     $FINE         IFEQ      '1'                                          --- 1 -->
011800931015     C*
011900931015     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
012000100908     c                   if        $err='1'
012100931015     C                   MOVEL     '1'           DSUERR
012200100908     c                   endif
012300931015     C*
012400931015     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
012500931015     C                   ELSE                                                   <-- 1 -->
012600000000     C*
012700000000     C* CICLO FINO A CHE:
012800931015     C     $FINE         DOWEQ     '0'                                          --- 2 -->
012900000000     C*
013000930805     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
013100000000     C*
013200930721     C* CONTROLLA GESTIONE VIDEO
013300930806     C     $GES          IFEQ      'V1'
013400930806     C                   EXSR      GESV1                                        *PARAMETRI
013500930805     C                   ENDIF
013600000000     C*
013700931015     C                   ENDDO                                                  <-- 2 ---
013800931015     C                   ENDIF                                                  <-- 1 ---
013900940105     C*
014000940105     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
014100940105     C     $F03          IFEQ      '1'
014200940105     C                   MOVEL     '1'           DSUF03
014300940105     C                   END
014400000000     C*
014500940105     C                   MOVEL     DSUNIT        KPJBU
014600000000     C                   SETON                                        LR
014700930713     C*================================================================
014800930806     C* CARV1  - CARICA GESTIONE VIDEO 'MOVIMENTO'
014900930713     C*================================================================
015000930806     C     CARV1         BEGSR
015100930713     C*
015200930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015300930713     C*
015400930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015500930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015600930713     C*
015700941115     C                   WRITE     SDU5DVT                                      *TESTATA
015800941115     C                   WRITE     SDU5DVF                                      *FUNZIONI
015900930713     C*
016000930713     C                   ENDSR
016100930804     C*================================================================
016200930806     C* GESV1  - GESTIONE VIDEO 'MOVIMENTO'
016300930804     C*================================================================
016400930806     C     GESV1         BEGSR
016500930804     C*
016600930804     C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
016700930806     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
016800930805     C                   ENDIF
016900930804     C*
017000930804     C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
017100930804     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
017200930805     C                   ENDIF
017300930804     C*
017400941115     C                   EXFMT     SDU5DV1                                      *MOVIMENTO
017500020321
017600020321     c                   eval      *in66 = *off
017700020321
017800930804     C*
017900930804     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
018000930804     C*
018100930805     C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
018200930804     C                   SELECT
018300930804     C     KEY           WHENEQ    ENTER
018400930806     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
018500930804     C     KEY           WHENEQ    F6
018600930805     C                   EXSR      F06GES                                       *GEST.F6 GENERALIZZA
018700940105     C     KEY           WHENEQ    F9
018800940105     C                   EXSR      F09GES                                       *ANAGRAFICA
018900940104     C     KEY           WHENEQ    F21
019000940104     C                   EXSR      F21GES                                       *STAMPA MOVIMENTI
019100930804     C                   OTHER
019200930805     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
019300930804     C                   ENDSL
019400930804     C                   ENDIF
019500930804     C*
019600930804     C                   ENDSR
019700930804     C*================================================================
019800930806     C* ENTV1  - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
019900930804     C*================================================================
020000930806     C     ENTV1         BEGSR
020100930806     C*
020200930806     C* CONTROLLA I CAMPI IMMESSI
020300930806     C                   EXSR      DECODI
020400930806     C*
020500930806     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
020600930806     C     DSUOP1        IFEQ      'O05'
020700100804     c
020800100804     c* Controllo se devo richiamare l'accertamento responsabilità danno
020900100804     c                   if        v1ccau='FISE'
021000100804     c                   if        %subst(v1dtmo:1:1)='E'
021100100804     c                   movel     v1cmgc        kmgc
021200100804     c                   else
021300100804     c                   movel     v1cmag        kmgc
021400100804     c                   endif
021500100804     c                   exsr      CTRRESPO
021600100804     c                   endif
021700100804     c
021800931015     C                   MOVEL     '1'           $FINE
021900930806     C                   END
022000930804     C*
022100930804     C                   ENDSR
022200000000     C*================================================================
022300930806     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA
022400000000     C*================================================================
022500930126     C     F06GES        BEGSR
022600930127     C*
022700930806     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
022800930806     C     DSUOP1        IFEQ      'F06'
022900100908     C     DSUOP1        oreq      'F6N'
023000930806     C                   EXSR      INSERI
023100100803     c* Controllo se devo attivare   l'accertamento responsabilità danno
023200100803     c                   if        v1ccau='FISE'
023300100803     c                   movel     v1cmgc        kmgc
023400100803     c                   exsr      CTRRESPO
023500100803     c                   endif
023600930806     C                   END
023700100803     c
023800930806     C     DSUOP1        IFEQ      'O02'
023900930806     C                   EXSR      MODIFI
024000100803     c* Controllo se devo richiamare l'accertamento responsabilità danno
024100100804     c                   if        v1ccau='FISE'
024200100804     c                   if        %subst(v1dtmo:1:1)='E'
024300100803     c                   movel     v1cmgc        kmgc
024400100803     c                   else
024500100803     c                   movel     v1cmag        kmgc
024600100803     c                   endif
024700100803     c                   exsr      CTRRESPO
024800100803     c                   endif
024900930806     C                   END
025000930806     C     DSUOP1        IFEQ      'O04'
025100930806     C                   EXSR      CANCEL
025200930806     C                   END
025300930806     C     DSUOP1        IFEQ      'O07'
025400930806     C                   EXSR      RIPRIS
025500930806     C                   END
025600930127     C*
025700000000     C                   ENDSR
025800940105     C*================================================================
025900940105     C* F09GES -GEST.TASTO FUNZ. F09 ===> ANAGRAFICA
026000940105     C*================================================================
026100940105     C     F09GES        BEGSR
026200940105     C*
026300940105     C* SALVO OPZIONE CON CUI E' STATO CHIAMATO QUESTO PGM
026400940105     C                   MOVEL     DSUOP1        WOP1              3
026500940105     C*
026600940105     C* LANCIO PGM PER VISUALIZZARE ANAGRAFICA UNITA'
026700940105     C                   MOVEL     'O05'         DSUOP1                         *VISUALIZZAZIONE
026800940105     C                   UNLOCK    UNANA01L                                     *SBLOCCA IL REC.ANAG
026900940105     C*
027000940105     C                   MOVEL     DSUNIT        KPJBU
027100941115     C                   CALL      'TNSDU2R'                                    *PGM GEST. ANAGRAF.
027200940105     C                   PARM                    KPJBA
027300940105     C                   MOVEL     KPJBU         DSUNIT
027400940105     C*
027500940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
027600020405     C**!!!DSUF03        IFEQ      '1'
027700020405     C**!!!              MOVEL     '1'           $FINE
027800020405     C**!!!              ENDIF
027900940105     C*
028000940105     C* REIMPOSTO OPZIONE DI QUESTO PGM
028100940105     C                   MOVEL     WOP1          DSUOP1
028200940105     C*
028300940105     C                   ENDSR
028400930128     C*================================================================
028500930804     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA ELENCO
028600930128     C*================================================================
028700930128     C     F21GES        BEGSR
028800930128     C*
028900940105     C                   MOVEL     DSUNIT        KPJBU
029000941115     C                   CALL      'TNSDU8R'                                    *PGM STAMPA MOVIM.
029100930209     C                   PARM                    KPJBA
029200940105     C                   MOVEL     KPJBU         DSUNIT
029300940105     C*
029400940105     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
029500020405     C**!!!DSUF03        IFEQ      '1'
029600020405     C**!!!              MOVEL     '1'           $FINE
029700020405     C**!!!              ENDIF
029800930128     C*
029900930128     C                   ENDSR
030000000000     C*================================================================
030100930126     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
030200000000     C*================================================================
030300000000     C     F22GES        BEGSR
030400000000     C*
030500000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
030600000000     C                   Z-ADD     80            LENGH            15 5
030700000000     C                   CALL      'QCAEXEC'
030800000000     C                   PARM                    CMD(1)
030900000000     C                   PARM                    LENGH
031000000000     C*
031100000000     C                   ENDSR
031200930806     C*================================================================
031300930806     C* INSERI - FUNZIONE DI INSERIMENTO
031400930806     C*================================================================
031500930806     C     INSERI        BEGSR
031600930806     C*
031700930806     C* CONTROLLA I CAMPI IMMESSI
031800930806     C                   EXSR      DECODI
031900930806     C*
032000930906     C     $ERR          IFEQ      '0'                                          --- 1 -->
032100931015     C*
032200931015     C* RICERCO L'ULTIMO PROGRESSIVO USATO PER QUESTO TIPO/CODICE/DATA
032300020322     C                   MOVE      V1CTIP        UNMTIP
032400110407     C                   MOVE      V1CMAT        UNMMAT
032500020322     C                   MOVE      V1CTIP        W1CTIP            2 0
032600931015     C                   Z-ADD     V1CCOD        UNMCOD
032700971010     C                   Z-ADD     V1CDTM        G08DAT
032800971010     C                   Z-ADD     *ZEROS        G08INV
032900971010     C                   MOVEL     '0'           G08ERR
033000971010     C                   CALL      'XSRDA8'
033100971010     C                   PARM                    WLBDA8
033200971010     C                   Z-ADD     G08INV        UNMDTM
033300971010     C                   Z-ADD     G08INV        W1CDTM            8 0
033400940104     C                   Z-ADD     9999999       UNMPRG                         *PRIMA DEI MOV.VECCH
033500931015     C     KEYUNM        SETGT     UNMOV01L                           99        *FINE-ARCHIV
033600140214     C     KEYUN2        READPE    UNMOV01L                               99
033700931213     C     *IN99         IFEQ      '1'                                          --- 2 -->
033800931015     C                   Z-ADD     *ZEROS        WTM060
033900931213     C                   ELSE                                                   <-- 2 -->
034000020322     C     UNMTIP        IFEQ      W1CTIP                                       --- 3 -->
034100020322     C     UNMCOD        ANDEQ     V1CCOD
034200971010     C     UNMDTM        ANDEQ     W1CDTM
034300931015     C                   Z-ADD     UNMPRG        WTM060            6 0
034400931217     C                   ELSE                                                   <-- 3 -->
034500931015     C                   Z-ADD     *ZEROS        WTM060
034600931217     C                   END                                                    <-- 3 ---
034700931213     C                   END                                                    <-- 2 ---
034800940104     C*
034900940104     C* SE IL PROGRESSIVO TROVATO E' INFERIORE E 20000 (REC.TIPO VECCHIO)
035000940104     C* LO FORZO IO A 20.000
035100940104     C     WTM060        IFLT      20000
035200940104     C                   Z-ADD     19999         WTM060
035300940104     C                   END
035400930806     C*
035500930907     C* IMPOSTO IL RECORD DI UNMOV
035600930806     C                   EXSR      VIDREC
035700930907     C*
035800931217     C* SE E' IN UN MAG., SCRIVO IL RECORD DI USCITA DAL MAGAZZINO
035900931217     C     UNAMAG        IFNE      *ZEROS
036000930907     C                   MOVEL     'U'           UNMTMO
036100931213     C                   MOVE      UNAMAG        UNMMAG                         *MAG UBIC.->MAG.USCI
036200931217     C                   MOVE      V1CMAG        UNMMGC                         *MAG DI DESTINAZ.
036300100907     c* Vedo se unità da rendere, resa
036400100907     c                   if        v1ccau='FISE'and %subst(unalsw:15:1)='S'
036500100908     c                   eval      unmreso='S'
036600100907     c                   else
036700100908     c                   clear                   unmreso
036800100907     c                   endif
036900931213     C     WTM060        ADD       1             UNMPRG
037000971014     C                   MOVEL     *BLANKS       UNMATB
037100971014     C                   Z-ADD     *ZEROS        UNMDTR
037200100908     c* Memorizzo se movimenti automatico
037300100908     c                   if        dsuop1='F6N'
037400100908     c                   eval      unmmaut='S'
037500100908     c                   endif
037600931015     C                   WRITE     UNMOV000
037700931217     C                   END
037800020321
037900930907     C* SCRIVO IL RECORD DI ENTRATA NEL MAGAZZINO DESTINAZIONE
038000930907     C                   MOVEL     'E'           UNMTMO
038100930907     C                   MOVE      V1CMAG        UNMMAG
038200931213     C                   MOVE      UNAMAG        UNMMGC                         *MAG.UBIC.->MAG.CORR
038300101022     c* Per il movimento di entrata cancello il ksc se da 951
038400101022     c                   if        unmcau = 'FISE' and unamag = 951
038500101022     c                   clear                   unmksc
038600101022     c                   endif
038700931217     C     WTM060        ADD       1             UNMPRG
038800971014     C                   MOVEL     *BLANKS       UNMATB
038900971014     C                   Z-ADD     *ZEROS        UNMDTR
039000931015     C                   WRITE     UNMOV000
039100100907     c
039200931217     C* AGGIORNO MAG. UBICAZIONE IN ANAGRAFICA
039300020322     c                   move      v1ctip        unatip
039400931217     C                   Z-ADD     V1CCOD        UNACOD
039500931217     C     KEYUNA        CHAIN     UNANA01L                           99        *ANAGRAFICA UNITA
039600931217     C     *IN99         IFEQ      '0'
039700020321      * se causale 'FISE' e magazzino = 951 devo azzerare cliente
039800101028     c                   clear                   savksc
039900020321     c                   if        unmcau = 'FISE' and unamag = 951
040000101028     c                   movel     unaksc        savksc
040100101028     c                   clear                   unaksc
040200140509     c                   eval      %subst(unalsw:1:14)=*blanks
040300020321     c                   endif
040400100907     c
040500100907     c* Se "FISE" e unità da rientrare, tolgo il reso dalle unità
040600100908     c                   if        unmcau='FISE' and unmreso='S'
040700100907     c                   eval      %subst(unalsw:15:1)=' '
040800101027     c
040900101027     c* Cerco record di UNGIO per aggiornare anche lì flag RESO
041000101027     c                   exsr      AggioUNGIO
041100100907     c                   endif
041200100907     c
041300931217     C                   MOVE      V1CMAG        UNAMAG                         *MAG UBIC.
041400020321      * se causale 'SECL' devo mettere mag 951 e cliente
041500101006     c***                if        unmcau = 'SECL'  or unmcau='FICL'
041600101006     c                   if        §unmksc='S'
041700020321     c                   eval      unamag = 951
041800020321     c                   eval      unaksc = unmksc
041900020321     c                   endif
042000100908     c
042100100908     c* Se inserimento da movimenti gironalieri la descrizione la scrivo in UNADES
042200100908     c                   if        dsuop1='F6N'
042300100908     c                   movel     unmdes        unades
042400100908     c                   endif
042500100908     c
042600931217     C                   UPDATE    UNANA000
042700931217     C                   ENDIF
042800941215     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
042900941215     C                   MOVEL     '1'           $FINE
043000930806     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
043100931210     C                   MOVEL     MSG(17)       DSMSMS
043200930806     C*
043300930907     C                   END                                                    <-- 1 ---
043400930806     C*
043500930806     C                   ENDSR
043600930806     C*================================================================
043700930806     C* MODIFI - FUNZIONE DI MODIFICA
043800930806     C*================================================================
043900930806     C     MODIFI        BEGSR
044000930806     C*
044100930806     C* CONTROLLA I CAMPI IMMESSI
044200930806     C                   EXSR      DECODI
044300930806     C*
044400930906     C     $ERR          IFEQ      '0'                                          --- 1 -->
044500930806     C*
044600931217     C* AGGIORNO IL RECORD DI USCITA (SE C'E')
044700020322     C                   Z-ADD     DSUTIP        UNMTIP
044800931217     C                   Z-ADD     DSUCOD        UNMCOD
044900931217     C                   Z-ADD     DSUDTM        UNMDTM
045000931217     C                   Z-ADD     DSUPRG        UNMPRG
045100931217     C                   MOVEL     'U'           UNMTMO
045200931217     C     KEYUNM        CHAIN     UNMOV01L                           99
045300931217     C     *IN99         IFEQ      '0'
045400931217     C                   MOVEL     V1CDES        UNMDES
045500931217     C                   Z-ADD     V1CDOC        UNMDOC
045600971010     C                   Z-ADD     V1CDDO        G08DAT
045700971010     C                   Z-ADD     *ZEROS        G08INV
045800971010     C                   MOVEL     '0'           G08ERR
045900971010     C                   CALL      'XSRDA8'
046000971010     C                   PARM                    WLBDA8
046100971010     C                   Z-ADD     G08INV        UNMDDO
046200931217     C                   Z-ADD     V1CCNT        UNMCNT
046300971014     C                   MOVEL     *BLANKS       UNMATB
046400020321     c                   eval      unmksc = v1cksc
046500931015     C                   UPDATE    UNMOV000
046600931217     C                   ENDIF
046700931217     C* AGGIORNO IL RECORD DI ENTRATA
046800020322     C                   Z-ADD     DSUTIP        UNMTIP
046900931217     C                   Z-ADD     DSUCOD        UNMCOD
047000931217     C                   Z-ADD     DSUDTM        UNMDTM
047100931217     C                   Z-ADD     DSUPRG        UNMPRG
047200931217     C                   MOVEL     'E'           UNMTMO
047300931217     C     KEYUNM        CHAIN     UNMOV01L                           99
047400931217     C     *IN99         IFEQ      '0'
047500931217     C                   MOVEL     V1CDES        UNMDES
047600931217     C                   Z-ADD     V1CDOC        UNMDOC
047700971010     C                   Z-ADD     V1CDDO        G08DAT
047800971010     C                   Z-ADD     *ZEROS        G08INV
047900971010     C                   MOVEL     '0'           G08ERR
048000971010     C                   CALL      'XSRDA8'
048100971010     C                   PARM                    WLBDA8
048200971010     C                   Z-ADD     G08INV        UNMDDO
048300931217     C                   Z-ADD     V1CCNT        UNMCNT
048400971014     C                   MOVEL     *BLANKS       UNMATB
048500020321     c                   eval      unmksc = v1cksc
048600931217     C                   UPDATE    UNMOV000
048700931217     C                   ENDIF
048800930806     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
048900930906     C                   MOVEL     '1'           $FINE
049000930806     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI MODIF. EFFETTUATA
049100931210     C                   MOVEL     MSG(18)       DSUMSG
049200930806     C*
049300930806     C                   END                                                    <-- 1 ---
049400930806     C*
049500930806     C                   ENDSR
049600930806     C*================================================================
049700930806     C* CANCEL - FUNZIONE DI CANCELLAZIONE
049800930806     C*================================================================
049900930806     C     CANCEL        BEGSR
050000930806     C*
050100931217     C* ANNULLO MOVIMENTO USCITA (SE C'E')
050200020322     C                   Z-ADD     DSUTIP        UNMTIP
050300931217     C                   Z-ADD     DSUCOD        UNMCOD
050400931217     C                   Z-ADD     DSUDTM        UNMDTM
050500931217     C                   Z-ADD     DSUPRG        UNMPRG
050600931217     C                   MOVEL     'U'           UNMTMO
050700931217     C     KEYUNM        CHAIN     UNMOV01L                           99
050800931217     C     *IN99         IFEQ      '0'
050900971014IF  2C     UNMDTR        IFEQ      *ZEROS                                       *MAI TRASMESSO
051000971014     C                   DELETE    UNMOV000
051100971014X   2C                   ELSE                                                   *GIA' TRASMESSO
051200931217     C                   MOVEL     'A'           UNMATB
051300931015     C                   UPDATE    UNMOV000
051400971014E   2C                   ENDIF
051500931217     C                   ENDIF
051600931217     C* ANNULLO MOVIMENTO ENTRATA
051700020322     C                   Z-ADD     DSUTIP        UNMTIP
051800931217     C                   Z-ADD     DSUCOD        UNMCOD
051900931217     C                   Z-ADD     DSUDTM        UNMDTM
052000931217     C                   Z-ADD     DSUPRG        UNMPRG
052100931217     C                   MOVEL     'E'           UNMTMO
052200931217     C     KEYUNM        CHAIN     UNMOV01L                           99
052300931217     C     *IN99         IFEQ      '0'
052400931217     C* MEMORIZZO MAG.CORR. CIOE' IL MAG. IN CUI RITORNA L'UNITA'
052500931217     C                   Z-ADD     UNMMGC        WNMMGC            3 0
052600971014IF  2C     UNMDTR        IFEQ      *ZEROS                                       *MAI TRASMESSO
052700971014     C                   DELETE    UNMOV000
052800971014X   2C                   ELSE                                                   *GIA' TRASMESSO
052900931217     C                   MOVEL     'A'           UNMATB
053000931217     C                   UPDATE    UNMOV000
053100971014E   2C                   ENDIF
053200971014     C                   ENDIF
053300931217     C* AGGIORNO MAG. UBICAZIONE IN ANAGRAFICA
053400020322     c                   z-add     dsutip        unatip
053500931217     C                   Z-ADD     DSUCOD        UNACOD
053600931217     C     KEYUNA        CHAIN     UNANA01L                           99        *ANAGRAFICA UNITA
053700931217     C     *IN99         IFEQ      '0'
053800931217     C                   Z-ADD     WNMMGC        UNAMAG                         *MAG UBIC.
053900100907     c
054000100907     c* se era unità resa, reimposto la "R" in anagrafica
054100100908     c                   if        unmcau='FISE' and unmreso='S'
054200101027     c                   eval      %subst(unalsw:15:1)='S'
054300101027     c
054400101027     c* Ceca UNGIO per togliere il fdlag di RESO
054500101027     c                   exsr      SflaggaUNGIO
054600100907     c                   endif
054700101006     c* Se causale con cliente, tolgo il cliente da UNAKSC
054800101006     c                   if        §unmksc='S'
054900101006     c                   clear                   unaksc
055000140509     c                   eval      %subst(unalsw:1:14)=*blanks
055100101006     c                   endif
055200100907     c
055300931217     C                   UPDATE    UNANA000
055400931217     C                   ENDIF
055500100907
055600930806     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
055700930906     C                   MOVEL     '1'           $FINE
055800930806     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI CANC. EFFETTUATA
055900931210     C                   MOVEL     MSG(19)       DSUMSG
056000930806     C*
056100930806     C                   ENDSR
056200930806     C*================================================================
056300930806     C* RIPRIS - FUNZIONE DI RIPRISTINO
056400930806     C*================================================================
056500930806     C     RIPRIS        BEGSR
056600931217     C*
056700931217     C* RIPRISTINO MOVIMENTO USCITA (SE C'E')
056800020322     C                   Z-ADD     DSUTIP        UNMTIP
056900931217     C                   Z-ADD     DSUCOD        UNMCOD
057000931217     C                   Z-ADD     DSUDTM        UNMDTM
057100931217     C                   Z-ADD     DSUPRG        UNMPRG
057200931217     C                   MOVEL     'U'           UNMTMO
057300931217     C     KEYUNM        CHAIN     UNMOV01L                           99
057400931217     C     *IN99         IFEQ      '0'
057500971014     C                   MOVEL     *BLANKS       UNMATB
057600931015     C                   UPDATE    UNMOV000
057700931217     C                   ENDIF
057800931217     C* RIPRISTINO MOVIMENTO ENTRATA
057900020322     C                   Z-ADD     DSUTIP        UNMTIP
058000931217     C                   Z-ADD     DSUCOD        UNMCOD
058100931217     C                   Z-ADD     DSUDTM        UNMDTM
058200931217     C                   Z-ADD     DSUPRG        UNMPRG
058300931217     C                   MOVEL     'E'           UNMTMO
058400931217     C     KEYUNM        CHAIN     UNMOV01L                           99
058500931217     C     *IN99         IFEQ      '0'
058600931217     C* MEMORIZZO MAG. CIOE' IL MAG. IN CUI RITORNA L'UNITA'
058700931217     C                   Z-ADD     UNMMAG        WNMMAG            3 0
058800971014     C                   MOVEL     *BLANKS       UNMATB
058900931217     C                   UPDATE    UNMOV000
059000931217     C                   ENDIF
059100931217     C* AGGIORNO MAG. UBICAZIONE IN ANAGRAFICA
059200020322     c                   z-add     dsutip        unatip
059300931217     C                   Z-ADD     DSUCOD        UNACOD
059400931217     C     KEYUNA        CHAIN     UNANA01L                           99        *ANAGRAFICA UNITA
059500931217     C     *IN99         IFEQ      '0'
059600931217     C                   Z-ADD     WNMMAG        UNAMAG                         *MAG UBIC.
059700931217     C                   UPDATE    UNANA000
059800931217     C                   ENDIF
059900930806     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
060000930906     C                   MOVEL     '1'           $FINE
060100930806     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI RIPR. EFFETTUATO
060200931210     C                   MOVEL     MSG(20)       DSUMSG
060300930806     C*
060400930806     C                   ENDSR
060500930806     C*================================================================
060600930806     C*  RECVID - PORTA I CAMPI DA UNMOV AL VIDEO
060700930806     C*================================================================
060800930806     C     RECVID        BEGSR
060900930806     C*
061000931015     C                   MOVE      UNMTIP        V1CTIP
061100930906     C                   Z-ADD     UNMCOD        V1CCOD
061200110407     C                   movel     UNMmat        V1Cmat
061300930906     C                   MOVEL     UNMCAU        V1CCAU
061400971010     C                   Z-ADD     UNMDTM        G08INV
061500971010     C                   Z-ADD     *ZEROS        G08DAT
061600971010     C                   MOVEL     '3'           G08ERR
061700971010     C                   CALL      'XSRDA8'
061800971010     C                   PARM                    WLBDA8
061900971010     C                   Z-ADD     G08DAT        V1CDTM
062000931015     C                   MOVE      UNMMAG        V1CMAG
062100931015     C                   MOVE      UNMMGC        V1CMGC
062200930906     C                   MOVEL     UNMDES        V1CDES
062300100907     c* vedo se movimento di unità reda
062400100908     c                   if        unmreso='S'
062500100907     c                   eval      v1hreso='S'
062600100907     c                   eval      v1creso='SI'
062700100907     c                   seton                                        01
062800100907     c                   else
062900100907     c                   clear                   v1hreso
063000100907     c                   eval      v1creso='NO'
063100100907     c                   setoff                                       01
063200100907     c                   endif
063300100910     c* Movimento automatico
063400100910     c                   if        unmmaut='S'
063500100910     c                   seton                                        02
063600100910     c                   else
063700100910     c                   setoff                                       02
063800100910     c                   endif
063900930906     C                   Z-ADD     UNMDOC        V1CDOC
064000971010     C                   Z-ADD     UNMDDO        G08INV
064100971010     C                   Z-ADD     *ZEROS        G08DAT
064200971010     C                   MOVEL     '3'           G08ERR
064300971010     C                   CALL      'XSRDA8'
064400971010     C                   PARM                    WLBDA8
064500971010     C                   Z-ADD     G08DAT        V1CDDO
064600930906     C                   Z-ADD     UNMCNT        V1CCNT
064700930907     C*
064800930907     C     UNMTMO        IFEQ      'E'
064900930907     C                   MOVEL     'ENTRATA'     V1DTMO
065000930907     C                   ELSE
065100930907     C                   MOVEL     'USCITA '     V1DTMO
065200930907     C                   END
065300930806     C*
065400930806     C     UNMATB        IFEQ      'A'
065500930906     C                   MOVEL     'ANNULLAT'    V1DATB
065600930906     C                   MOVE      'O'           V1DATB
065700930906     C                   ELSE
065800930906     C                   MOVEL     *BLANKS       V1DATB
065900930806     C                   END
066000931217     C*
066100931217     C* DECODIFICO MAG. CORRELATO
066200931217     C     UNMMGC        IFEQ      *ZEROS                                       --- 1 -->
066300931217     C                   MOVEL     *BLANKS       V1DMGC
066400931217     C                   ELSE                                                   <-- 1 -->
066500931217     C                   MOVE      UNMMGC        WMAG
066600931217     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
066700931217     C                   MOVEL     WDMAG         V1DMGC
066800931217     C                   ENDIF                                                  <-- 1 ---
066900020321
067000020321     c                   eval      v1cksc = unmksc
067100930906     C*
067200930906     C                   EXSR      DECODI
067300930906     C     $ERR          IFEQ      '1'
067400930906     C                   MOVEL     '0'           $ERR
067500930906     C                   ENDIF
067600930806     C*
067700930806     C                   ENDSR
067800100907     C*================================================================
067900100907     C*  RIEVID - carico video da pgm movimenti giornalieri
068000100907     C*================================================================
068100100907     C     RIEVID        BEGSR
068200100907     C*
068300110412     C                   MOVE      UNgMAT        V1CMAT
068400100907     C                   MOVE      UNgTIP        V1CTIP
068500100907
068600100908     c* Per INVIO imposto causale SEFI o SECL
068700100907     c                   select
068800100908     c                   when      unginvr='I' and ungksc>0
068900100907     C                   MOVEL     'SECL'        V1CCAU
069000100908     c                   when      unginvr='I' and ungksc=0
069100100907     C                   MOVEL     'SEFI'        V1CCAU
069200120524     c* Rientro FORNITORE in sede --> causale RIEN
069300100908     c                   when      ungcuf='FOR' and unginvr='R'
069400100908     C                   MOVEL     'RIEN'        V1CCAU
069500110412     c* se le note sono piene, imposto nella matricola il campo note--> nuova matricola
069600110412     c                   if        ungnot<>*blanks
069700110412     c                   eval      v1cmat=ungnot
069800110412     c                   endif
069900120524     c* Rientro FORNITORE diretto in filiale--> causale FIFI
070000120524     c                   when      ungcuf='FOR' and unginvr='O'
070100120524     C                   MOVEL     'FIFI'        V1CCAU
070200120524     c* se le note sono piene, imposto nella matricola il campo note--> nuova matricola
070300120524     c                   if        ungnot<>*blanks
070400120524     c                   eval      v1cmat=ungnot
070500120524     c                   endif
070600110412     c
070700100908     c                   when      ungcuf='FOR' and unginvr='F' and
070800100908     c                             savtmag=' '
070900100908     C                   MOVEL     'FISE'        V1CCAU
071000101022     c
071100101022     c* Se magazzino corrrelato è 951 devo indicare anche il codice
071200101022     c                   if        ungmgc=951
071300101022     c                   eval      v1cksc=g_unaksc
071400101022     c                   endif
071500100908     c                   when      ungcuf='FOR' and unginvr='F' and
071600100908     c                             savtmag='Y'
071700100908     C                   MOVEL     'RIPA'        V1CCAU
071800100908     c                   endsl
071900110412     c
072000110412     c* con tipo e matricola prendo il codice unità
072100110412     c     kuna2         chain     unana02l
072200110412     c                   if        %found(unana02l)
072300110412     c                   movel     g_unacod      v1ccod
072400110412     c                   else
072500110412     c                   clear                   v1ccod
072600110412     c                   endif
072700100907     C                   MOVE      UNgMAG        V1CMAG
072800100907     C                   MOVE      UNgMGC        V1CMGC
072900100907     C                   MOVEL     UNgDES        V1CDES
073000100907     C                   Z-ADD     UNgDOC        V1CDOC
073100100907     C                   Z-ADD     UNgDDO        G08INV
073200100907     C                   Z-ADD     *ZEROS        G08DAT
073300100907     C                   MOVEL     '3'           G08ERR
073400100907     C                   CALL      'XSRDA8'
073500100907     C                   PARM                    WLBDA8
073600100907     C                   Z-ADD     G08DAT        V1CDDO
073700100907     C*
073800100907     C* DECODIFICO MAG. CORRELATO
073900100907     C     UNgMGC        IFEQ      *ZEROS                                       --- 1 -->
074000100907     C                   MOVEL     *BLANKS       V1DMGC
074100100907     C                   ELSE                                                   <-- 1 -->
074200100908     C                   MOVE      UNgMGC        WMAG
074300100907     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
074400100907     C                   MOVEL     WDMAG         V1DMGC
074500100929     C                   ENDIF
074600100908     c
074700100928     c* Se rientro da Filiale per fornitore, al 1°movimento imposto magz.destinazione 935
074800100908     c                   if        unginvr='F'and v1ccau='FISE'
074900100908     C                   MOVE      v1cMAG        WMAG
075000100908     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
075100100908     c                   if        wtmag='Y'
075200100908     c                   eval      v1cmag='935'
075300100908     c                   eval      savtmag=wtmag
075400100908     c                   endif
075500100908     c                   endif
075600100908     c
075700100908     c* Se rientro da Filiale per fornitore al 2° movimento inmposto 935 nell'ubicaz.
075800100908     c                   if        unginvr='F'and v1ccau='RIPA'
075900100908     c                   eval      v1cmgc='935'
076000100908     c                   endif
076100101013     c
076200101013     c* Se movimento Sede e invio a fornitore RIPA altrimenti SESE
076300101013     c                   if        unginvr='S'
076400101013     C                   MOVE      v1cMAG        WMAG
076500101013     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
076600101013     c                   if        wtmag='Y'
076700101013     C                   MOVEL     'RIPA'        V1CCAU
076800101013     c                   else
076900101013     C                   MOVEL     'SESE'        V1CCAU
077000101013     c                   endif
077100101013     c                   endif
077200100907
077300101022     c                   if         unginvr='I'
077400100907     c                   eval      v1cksc = ungksc
077500101022     c                   endif
077600100907     C*
077700100907     C                   EXSR      DECODI
077800100907     C     $ERR          IFEQ      '1'
077900100907     C                   MOVEL     '0'           $ERR
078000100907     C                   ENDIF
078100100907     C*
078200100907     C                   ENDSR
078300930906     C*================================================================
078400930906     C*  VIDREC - IMPOSTA CAMPI DI UNMOV DA VIDEO
078500930906     C*================================================================
078600930906     C     VIDREC        BEGSR
078700930906     C*
078800931217     C                   MOVE      V1CTIP        UNMTIP
078900931217     C                   Z-ADD     V1CCOD        UNMCOD
079000110407     C                   movel     V1Cmat        UNMmat
079100931217     C                   MOVEL     V1CCAU        UNMCAU
079200971010     C                   Z-ADD     V1CDTM        G08DAT
079300971010     C                   Z-ADD     *ZEROS        G08INV
079400971010     C                   MOVEL     '0'           G08ERR
079500971010     C                   CALL      'XSRDA8'
079600971010     C                   PARM                    WLBDA8
079700971010     C                   Z-ADD     G08INV        UNMDTM
079800930906     C                   MOVEL     V1CDES        UNMDES
079900930906     C                   Z-ADD     V1CDOC        UNMDOC
080000971010     C                   Z-ADD     V1CDDO        G08DAT
080100971010     C                   Z-ADD     *ZEROS        G08INV
080200971010     C                   MOVEL     '0'           G08ERR
080300971010     C                   CALL      'XSRDA8'
080400971010     C                   PARM                    WLBDA8
080500971010     C                   Z-ADD     G08INV        UNMDDO
080600930906     C                   Z-ADD     V1CCNT        UNMCNT
080700020321     c                   eval      unmksc = v1cksc
080800930906     C*
080900930906     C                   ENDSR
081000930806     C*================================================================
081100930806     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
081200930806     C*================================================================
081300930806     C     AZZVID        BEGSR
081400930806     C*
081500110407     C                   MOVEL     *BLANKS       V1cmat
081600110407     C                   MOVEL     *BLANKS       V1DATB
081700930906     C                   MOVEL     *BLANKS       V1CTIP
081800930906     C                   MOVEL     *BLANKS       V1DTIP
081900930906     C                   Z-ADD     *ZEROS        V1CCOD
082000930906     C                   MOVEL     *BLANKS       V1DCOD
082100170424     C**                 Z-ADD     *date         V1CDTM
082200170424     C                   Z-ADD     dataogGMA     V1CDTM
082300930906     C                   MOVEL     *BLANKS       V1CCAU
082400930906     C                   MOVEL     *BLANKS       V1DCAU
082500930906     C                   MOVEL     *BLANKS       V1CMAG
082600930906     C                   MOVEL     *BLANKS       V1DMAG
082700930907     C                   MOVEL     *BLANKS       V1DTMO
082800930906     C                   MOVEL     *BLANKS       V1CDES
082900930906     C                   Z-ADD     *ZEROS        V1CDOC
083000170424     C**                 Z-ADD     *date         V1CDDO
083100170424     C                   Z-ADD     dataogGMA     V1CDDO
083200930906     C                   Z-ADD     *ZEROS        V1CCNT
083300930906     C                   MOVEL     *BLANKS       V1CMGC
083400930906     C                   MOVEL     *BLANKS       V1DMGC
083500020321     c                   clear                   v1cksc
083600100907     c                   clear                   v1dksc
083700100907     c                   clear                   v1hreso
083800100907     c                   clear                   v1creso
083900100910     c                   setoff                                       0102
084000930806     C*
084100930806     C                   ENDSR
084200930205     C*================================================================
084300930906     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
084400930205     C*================================================================
084500930906     C     DECODI        BEGSR
084600930205     C*
084700930906     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI CAMPI
084800930906     C                   SETOFF                                       606162
084900930906     C                   SETOFF                                       636465
085000020321     c                   eval      *in67 = *off
085100930712     C*
085200930907     C* CONTROLLO IL TIPO UNITA'
085300930907     C                   MOVEL     *BLANKS       V1DTIP                         *DESCR.TIPO UNITA'
085400930907     C                   MOVE      V1CTIP        WTIP
085500930805     C                   EXSR      CHKTIP                                       *CONTROLLA TIPO UNIT
085600930907     C     $ERR          IFEQ      '1'                                          --- 1 -->
085700930721     C                   MOVEL     '1'           $ERR
085800930721     C                   MOVEL     MSG(9)        $MSG
085900930721     C                   EXSR      CARMSG
086000930907     C                   SETON                                        60
086100930907     C                   ELSE                                                   <-- 1 -->
086200930907     C                   MOVE      WTIP          V1CTIP                         *COD.
086300930907     C                   MOVEL     WDTIP         V1DTIP                         *DESCR.
086400930907     C                   ENDIF                                                  <-- 1 ---
086500930907     C*
086600930907     C* CONTROLLO IL CODICE UNITA'
086700100921     c                   clear                   wtmag
086800930907     C     $ERR          IFEQ      '0'
086900930907     C                   MOVEL     *BLANKS       V1DCOD                         *DESCR.UNITA'
087000020322     c                   move      v1ctip        unatip
087100930907     C                   Z-ADD     V1CCOD        UNACOD
087200930907     C     KEYUNA        CHAIN     UNANA01L                           99        *ANAGRAFICA UNITA
087300930907     C     *IN99         IFEQ      '1'                                          --- 1 -->
087400930907     C                   MOVEL     '1'           $ERR
087500930907     C                   MOVEL     MSG(11)       $MSG
087600930907     C                   EXSR      CARMSG
087700930907     C                   SETON                                        61
087800930907     C                   ELSE                                                   <-- 1 -->
087900100903     c                   movel     unamod        v1cmod
088000930907     C                   MOVEL     UNADES        V1DCOD                         *DESCR.
088100110401     c*  se ditta di proprietà = 923 non posso accettare mag 990
088200110401     c                   if        unasis='923' and v1cmag='990'
088300110401     C                   MOVEL     '1'           $ERR
088400110401     C                   MOVEL     MSG(27)       $MSG
088500110401     C                   EXSR      CARMSG
088600110401     c                   seton                                        64
088700110401     c                   endif
088800110401     c
088900930907     C                   ENDIF                                                  <-- 1 ---
089000930907     C                   ENDIF
089100100907     c
089200100903     C* SE INSERIMENTO METTO A VIDEO IL MAGAZZINO CORRELATO (MAG.UBIZAZ.ANAGR)
089300100907      * più il codice cliente e se unità da rendere
089400110407      * più la matricola
089500100908    1C     DSUOP1        IFEQ      'F06'
089600100907     c                   clear                   v1creso
089700100907     c                   setoff                                       01
089800100903     C                   MOVE      UNAMAG        V1CMGC
089900100908    2C     UNAMAG        IFEQ      *ZEROS                                       --- 1 -->
090000100903     C                   MOVEL     *BLANKS       V1DMGC
090100100908   x2C                   ELSE                                                   <-- 1 -->
090200100903     C                   MOVE      UNAMAG        WMAG
090300100903     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
090400100903     C                   MOVEL     WDMAG         V1DMGC
090500110407     c
090600100908    3c                   if        unaksc <> *zeros
090700100903     c                   eval      v1cksc = unaksc
090800100903     c                   exsr      chkksc
090900100908    3c                   endif
091000110407     c
091100110407     c                   movel     unamat        v1cmat
091200110407     c
091300100908    2C                   ENDIF                                                  <-- 2 ---
091400100908    1C                   ENDIF                                                  <-- 1 ---
091500100907     c
091600100908     c* anche per conferma moviemnti giornalieri lo faccio
091700100908    1C     DSUOP1        IFEQ      'F06'
091800100908    1C     DSUOP1        oreq      'F6N'
091900100907     c                   if        %subst(unalsw:15:1)='S'  and v1ccau='FISE'
092000100907     c                   eval      v1creso='SI'
092100100907     c                   seton                                        01
092200100907     c                   endif
092300100907     c
092400100903     C                   END
092500930907     C*
092600930907     C* CONTROLLO LA DATA DEL MOVIMENTO
092700930907     C     $ERR          IFEQ      '0'
092800971010     C                   Z-ADD     V1CDTM        WDAT              8 0
092900930907     C                   EXSR      CHKDAT                                       *CONTROLLA DATA
093000930907     C     $ERR          IFEQ      '1'                                          *DATA ERRATA
093100930907     C                   SETON                                        62
093200930907     C                   ELSE                                                   *DATA OK
093300971010     C                   Z-ADD     V1CDTM        G08DAT
093400971010     C                   Z-ADD     *ZEROS        G08INV
093500971010     C                   MOVEL     '0'           G08ERR
093600971010     C                   CALL      'XSRDA8'
093700971010     C                   PARM                    WLBDA8
093800971013     C                   Z-ADD     G08DAT        V1CDTM
093900971013     C                   Z-ADD     G08INV        W1CDTM            8 0
094000930907     C                   ENDIF
094100930907     C                   ENDIF
094200930907     C*
094300930907     C* CONTROLLO LA CAUSALE DEL MOVIMENTO
094400930907     C     $ERR          IFEQ      '0'
094500930907     C                   MOVEL     *BLANKS       V1DCAU                         *DESCR.CAUSALE
094600930907     C                   MOVE      V1CCAU        WCAU
094700930907     C                   EXSR      CHKCAU                                       *CONTROLLA CAUSALE
094800930907     C     $ERR          IFEQ      '1'                                          --- 1 -->
094900930907     C                   MOVEL     '1'           $ERR
095000930907     C                   MOVEL     MSG(16)       $MSG
095100930907     C                   EXSR      CARMSG
095200930907     C                   SETON                                        63
095300930907     C                   ELSE                                                   <-- 1 -->
095400930907     C                   MOVE      WCAU          V1CCAU                         *COD.
095500930907     C                   MOVEL     WDCAU         V1DCAU                         *DESCR.
095600930907     C                   ENDIF                                                  <-- 1 ---
095700930907     C                   ENDIF
095800100921     c
095900100921     c* Controllo il tipo Magazzino Correlato
096000100921    3c                   if        dsuop1='F06' and §unmtmgc<>*blanks
096100100921     c                   movea     §unmtmgc      tipi
096200100929     c* ricerco con FAG + LUE
096300100929     c                   movel     wtmag         RICtipo           2
096400100929     c                   move      §omaglue      RICtipo
096500100929     c     RICtipo       lookup    tipi                                   30
096600100921    4c                   if        not *in30
096700100921     C                   MOVEL     '1'           $ERR
096800100921     C                   MOVEL     MSG(26)       $MSG
096900100921     C                   EXSR      CARMSG
097000100921     C                   SETON                                        63
097100100921    4c                   endif
097200100921    3c                   endif
097300930212     C*
097400930907     C* CONTROLLO IL MAGAZZINO
097500100921    1C     $ERR          IFEQ      '0'
097600930907     C                   MOVE      V1CMAG        WMAG
097700930805     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
097800100921    2C     $ERR          IFEQ      '1'                                          --- 1 -->
097900930721     C                   MOVEL     MSG(8)        $MSG
098000930721     C                   EXSR      CARMSG
098100930907     C                   SETON                                        64
098200100921   x2C                   ELSE                                                   <-- 1 -->
098300930907     C                   MOVE      WMAG          V1CMAG
098400930907     C                   MOVEL     WDMAG         V1DMAG
098500100921     c* Verifico il tipo rispetto alla causale  solo in inserimento
098600100921    3c                   if        dsuop1='F06' and §unmtmag<>*blanks
098700100921     c                   movea     §unmtmag      tipi
098800100929     c* ricerco con FAG + LUE
098900100929     c                   movel     wtmag         RICtipo           2
099000100929     c                   move      §omaglue      RICtipo
099100100929     c     RICtipo       lookup    tipi                                   30
099200100921    4c                   if        not *in30
099300100921     C                   MOVEL     '1'           $ERR
099400100921     C                   MOVEL     MSG(25)       $MSG
099500100921     C                   SETON                                        64
099600100921     C                   EXSR      CARMSG
099700100921    4c                   endif
099800100921    3c                   endif
099900100921    2C                   ENDIF                                                  <-- 1 ---
100000100921    1C                   ENDIF
100100930712     C*
100200930907     C* CONTROLLO LA DATA DEL DOCUMENTO
100300930907     C     $ERR          IFEQ      '0'
100400930907     C     V1CDDO        IFNE      *ZEROS                                       --- 1 -->
100500971010     C                   Z-ADD     V1CDDO        WDAT
100600930805     C                   EXSR      CHKDAT                                       *CONTROLLA DATA
100700930907     C     $ERR          IFEQ      '1'                                          --- 2 -->*DATA ERRAT
100800931015     C                   SETON                                        65
100900930907     C                   ELSE                                                   <-- 2 -->*DATA OK
101000971010     C                   Z-ADD     V1CDDO        G08DAT
101100971010     C                   Z-ADD     *ZEROS        G08INV
101200971010     C                   MOVEL     '0'           G08ERR
101300971010     C                   CALL      'XSRDA8'
101400971010     C                   PARM                    WLBDA8
101500971013     C                   Z-ADD     G08DAT        V1CDDO
101600971013     C                   Z-ADD     G08INV        W1CDDO            8 0
101700930907     C                   ENDIF                                                  <-- 2 ---
101800930907     C                   ENDIF                                                  <-- 1 ---
101900930907     C                   ENDIF
102000020321
102100020321      * se inserimento e causale 'SECL' obbligatorio il cliente
102200100921     c***                          (v1ccau ='FICL' or v1ccau='SECL') and
102300100921     c                   if        (dsuop1 = 'F06' or dsuop1='F6N')   and
102400100921     c                             §unmksc='S' and
102500020403     c                             v1cksc = *zeros and v1dksc = *blanks
102600020321     c                   eval      $err = *on
102700020321     c                   eval      $msg = msg(7)
102800020321     c                   exsr      carmsg
102900020321     c                   eval      *in67 = *on
103000020321     c                   endif
103100020327
103200020327      * controllo il cliente
103300020327     c                   if        $err = *off
103400020327     c                   if        v1cksc <> *zeros or v1dksc <> *blanks
103500020327     c                   exsr      chkksc
103600020327     c                   if        $err = *on
103700020327     c                   eval      $msg = msg(7)
103800020327     c                   exsr      carmsg
103900020327     c                   eval      *in67 = *on
104000020327     c                   endif
104100020327     c                   endif
104200020327     c                   if        v1cksc = *zeros and v1cmag = '951'
104300020327     c                   eval      $err = *on
104400020327     c                   eval      $msg = msg(7)
104500020327     c                   exsr      carmsg
104600020327     c                   eval      *in67 = *on
104700020327     c                   endif
104800020327     c                   endif
104900020321
105000930715     C*
105100930715     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
105200930715     C* CURSORE SUL CAMPO DEL CONTROL
105300930721     C     $ERR          IFEQ      '1'
105400930805     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
105500930715     C                   ELSE
105600930805     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
105700930715     C                   END
105800930205     C*
105900930205     C                   ENDSR
106000020321      *================================================================
106100020321      *  CHKTIP - CONTROLLA IL TIPO DI UNITA
106200020321      *================================================================
106300930712     C     CHKTIP        BEGSR
106400020321
106500020321      * CONTROLLO SE E' STATO INSERITO '?'
106600020321     C     '?'           SCAN      WTIP                                   99
106700020321
106800020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
106900020321     c                   if        *in99                                        --- 1 -->
107000020321     c                   clear                   TIBS02DS
107100020321     c                   movel     'R'           t02mod
107200020321     c                   movel     knsif         t02sif
107300020321     c                   movel     'UNT'         t02cod
107400020321     c                   call      'TIBS02R'
107500020321     c                   parm                    KPJBA
107600020321     c                   parm                    TIBS02DS
107700020321     c                   movel     T02ke1        wtip              2
107800020321     c                   movel     t02uni        wdtip            30
107900020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
108000020321     c                   endif                                                  <-- 1 ---
108100020321
108200020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
108300020321     c                   clear                   TIBS02DS
108400020321     c                   movel     'C'           T02mod
108500020321     c                   movel     knsif         t02sif
108600020321     c                   movel     'UNT'         t02cod
108700020321     c                   movel(p)  wtip          T02ke1
108800020321     c                   call      'TIBS02R'
108900020321     c                   parm                    KPJBA
109000020321     c                   parm                    TIBS02DS
109100020321     c                   if        t02err <> *blanks
109200020321     c                   eval      $err = '1'
109300020321     c                   clear                   wdtip
109400110105     c                   clear                   dunt
109500020321     c                   else
109600020321     c                   movel     t02uni        wdtip
109700110105     c                   movel     t02uni        dunt
109800020321     c                   endif
109900020321
110000930712     C                   ENDSR
110100020321      *================================================================
110200020321      *  CHKCAU - CONTROLLA LA CAUSALE MOVIMENTO
110300020321      *================================================================
110400930712     C     CHKCAU        BEGSR
110500020321
110600020321      * CONTROLLO SE E' STATO INSERITO '?'
110700020321     C     '?'           SCAN      WCAU                                   99
110800020321
110900020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
111000020321     c                   if        *in99                                        --- 1 -->
111100020321     c                   clear                   TIBS02DS
111200020321     c                   movel     'R'           t02mod
111300020321     c                   movel     knsif         t02sif
111400020321     c                   movel     'UNM'         t02cod
111500020321     c                   call      'TIBS02R'
111600020321     c                   parm                    KPJBA
111700020321     c                   parm                    TIBS02DS
111800020321     c                   movel     T02ke1        wcau              4
111900020321     c                   movel     t02uni        wdcau            30
112000100921     c                   movel     t02uni        dunm
112100020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
112200020321     c                   endif                                                  <-- 1 ---
112300020321
112400020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
112500020321     c                   clear                   TIBS02DS
112600020321     c                   movel     'C'           T02mod
112700020321     c                   movel     knsif         t02sif
112800020321     c                   movel     'UNM'         t02cod
112900020321     c                   movel(p)  wcau          T02ke1
113000020321     c                   call      'TIBS02R'
113100020321     c                   parm                    KPJBA
113200020321     c                   parm                    TIBS02DS
113300020321     c                   if        t02err <> *blanks
113400020321     c                   eval      $err = '1'
113500020321     c                   clear                   wdcau
113600100921     c                   clear                   dunm
113700020321     c                   else
113800020321     c                   movel     t02uni        wdcau
113900100921     c                   movel     t02uni        dunm
114000020321     c                   endif
114100020321
114200930712     C                   ENDSR
114300930712     C*================================================================
114400930907     C*  CHKMAG - CONTROLLA IL MAGAZZINO
114500930712     C*================================================================
114600930712     C     CHKMAG        BEGSR
114700100929     c                   clear                   tnsdmagds
114800100929     c                   eval      §magmag=wmag
114900100929     c                   eval      §imagtorg='E  '
115000100929     c                   call      'TNSDMAGR'
115100100929     c                   parm                    kpjba
115200100929     c                   parm                    tnsdmagds
115300100929     c
115400100929     c* Eseguita Ricerca
115500100929    1c                   if        §omagerr='R'
115600100929     c                   eval      wmag=§magmag
115700100929     c                   eval      wdmag=§omagdes
115800100929     c                   eval      wtmag=§omagfag
115900100929     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
116000100929   x1c                   else
116100100929     c* Errore nel magazzino
116200100929    2c                   if        §omagerr='E'
116300100929     C                   MOVEL     '1'           $ERR
116400100929     c                   clear                   wtmag
116500100929     c                   eval      wdmag=*all'?'
116600100929     c* se tutto ok prendo decodifica e flag
116700100929   x2c                   else
116800100929     c                   eval      wdmag=§omagdes
116900100929     c                   eval      wtmag=§omagfag
117000100929    2c                   endif
117100100929    1c                   endif
117200930712     C*
117300930712     C                   ENDSR
117400930712     C*================================================================
117500930712     C* CHKDAT - CONTROLLA LA DATA
117600930712     C*================================================================
117700930712     C     CHKDAT        BEGSR
117800930712     C*
117900971010     C                   Z-ADD     WDAT          G08DAT
118000971010     C                   Z-ADD     *ZEROS        G08INV
118100971010     C                   MOVEL     '0'           G08ERR
118200971010     C                   CALL      'XSRDA8'                                     *CONTROLLA DATA
118300971010     C                   PARM                    WLBDA8
118400971013     C                   Z-ADD     G08DAT        WDAT
118500971010     C     G08ERR        IFNE      '0'
118600930721     C                   MOVEL     '1'           $ERR
118700930721     C                   MOVEL     MSG(10)       $MSG
118800930721     C                   EXSR      CARMSG
118900930805     C                   ENDIF
119000930712     C*
119100930712     C                   ENDSR
119200020321      *================================================================
119300020321      * CHKKSC - CONTROLLA IL CLIENTE
119400020321      *================================================================
119500020321     c     chkksc        begsr
119600020321
119700020321      * se non inserito il codice ma inserita una parte della ragione sociale
119800020321      * richiamo il programma della ricerca alfabetica
119900020321     c                   if        v1cksc = *zeros and v1dksc <> *blanks
120000020321     c                   exsr      ricalf
120100020321     c                   endif
120200020321
120300020321      * se inserito il codice controllo che esista sul piano dei conti
120400020321     c                   if        v1cksc <> *zeros
120500020327     c                   clear                   TIBS69DS
120600020327     c                   z-add     v1cksc        I69kac
120700020327     c                   call      'TIBS69R'
120800020327     c                   parm                    tibs69DS
120900020327     c                   parm                    ds_cnaco
121000020327     c                   parm                    ds_cnind
121100020327     c                   parm                    ds_cnclp
121200020327     c                   parm                    ds_fncls
121300020327
121400020327     c                   if        o69err <> '1'
121500020327     c                   movel     acorag        v1dksc
121600020327     c                   else
121700020321     c                   eval      $err = *on
121800020321     c                   endif
121900020321     c                   endif
122000020321
122100020321     c                   endsr
122200020321      *================================================================
122300020321      *  RICALF - RICERCA ALFABETICA
122400020321      *================================================================
122500020321     c     ricalf        begsr
122600020321
122700020321     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
122800080922     C*******            Z-ADD     9             PAXsta                         9=Escludo Annullati
122900080922     c* visulizzare anche i bloccati
123000080922     C                   Z-ADD     0             PAXsta                         9=Escludo Annullati
123100020709     C                   Z-ADD     DutKci        PAXccc                         Capoconto Clienti
123200020321     c                   movel(P)  v1dksc        PAXdmt
123300020321     C                   z-add     1             PAXnum
123400020321
123500020321     C                   CALL      'XALFA3BR'
123600020321     C                   PARM                    PAXDUT
123700020321     C                   PARM                    CODUT
123800020321     C                   PARM                    PAXDMT
123900020321     C                   PARM                    PAXCCC
124000020321     C                   PARM                    PAXSTA
124100020321     C                   PARM      *blanks       PAXFLR
124200020321     C                   PARM      *blanks       PAXDIT
124300020321     C                   PARM                    PAXNUM            2 0
124400020321     C                   PARM                    PAXKCM           80
124500020321     C                   PARM                    PAXKSM          140
124600020321     C                   PARM                    PAXKDM           60
124700020321
124800020321      * NON TROVATO O PREMUTO CMD12
124900020321    2C     paxsta        IFGT      -1
125000020321     C                   MOVEL     PAXKSM        v1cksc
125100020321    2C                   ENDIF
125200020321
125300020321     c                   endsr
125400100803     C*================================================================
125500100803     C* emetto videata accertamento responsabilità danno
125600100803     C*================================================================
125700100803     C     CTRRESPO      BEGSR
125800100804     c* Solo se la data movimento > di agosto
125900100804     c                   if        unmdtm>20100801
126000100803     c                   clear                   w2ctcm
126100100803     c                   clear                   w2dtcm
126200100803     c                   clear                   w2ciafil
126300100805     c                   clear                   w2diafil
126400100803     c                   clear                   w2ciamag
126500100803     c                   clear                   w2ccapre
126600100803     c                   clear                   w2cfor
126700100804     c                   clear                   w2dfor1
126800100804     c                   clear                   w2dfor2
126900110401     c                   clear                   w2hdfor
127000100804     c                   clear                   w2cauco
127100100803     c                   clear                   w2cimpa
127200100805     c                   clear                   w2cemf
127300100803     c                   clear                   w2cdtv
127400100805     c                   clear                   w2cdes
127500100805     c                   clear                   w2ccms
127600100803     c* cerco record
127700100803     c     kunr          chain     unmor01l
127800100803     c                   if        %found(unmor01l)
127900100803     c                   eval      w2ctcm=unrtcm
128000100805     c                   eval      w2ccms=unrcms
128100100803     c                   eval      w2ciafil=unriafil
128200100803     c                   eval      w2ciamag=unriamag
128300100803     c                   eval      w2ccapre=unrcapre
128400100804     c                   eval      w2cauco =unrtipp
128500100804     c                   if        unrpdr>0
128600100804     c                   movel     unrpdr        w2cfor
128700100804     c                   endif
128800100803     c                   eval      w2cimpa =unrimpa
128900100805     c                   eval      w2cemf  =unremf
129000100805     c                   eval      w2cdes  =unrdes
129100100805     c
129200100804     c                   if        unrdtv>0
129300100804     c                   movel     unrdtv        dataiso
129400100804     c     *eur          movel     dataiso       w2cdtv
129500100804     c                   endif
129600100805
129700100804     c                   exsr      decwin
129800100804     c                   setoff                                       28
129900110610     C                   SETOFF                                       707172
130000110610     C                   SETOFF                                       737475
130100110610     C                   SETOFF                                       76
130200100804     c                   eval      $err='0'
130300100803     c                   endif
130400100803     c
130500100804     c     emfor2        TAG
130600100803     c                   exfmt     sdu5dw2
130700100804     c                   setoff                                       28
130800100804     c                   eval      $err='0'
130900100804     c
131000100803     c* controlli
131100100803     c                   exsr      decWIN
131200100804     c
131300100804     c                   if        $err='1'
131400100804     c                   goto      emfor2
131500100804     c                   endif
131600100804     c* in inserimento valido solo F6
131700100804     c  n91              if        not %found(unmor01l)  and *inkl
131800100804     c                   eval      $err = '1'
131900100804     C                   MOVEL     MSG(15)       w2cmsg
132000100804     c                   seton                                        2870
132100100804     c                   goto      emfor2
132200100804     c                   endif
132300100804     c
132400100804     c* Se non è stato premuto F6 o F12 riemetto videata
132500100804     c* In visualizzazione esco sempre
132600100804     c  n91              if        not *inkl and not  *inkf
132700100804     c                   goto      emfor2
132800100804     c                   endif
132900100804     c* Conferma
133000100804     c                   if        *inkf
133100100804     c                   exsr      AggioMOR
133200100804     c                   endif
133300100804     c
133400100804     c                   endif
133500100803     c
133600100803     c                   endsr
133700100803     C*================================================================
133800100803     C*  DECWIN - CONTROLLA CAMPI della window
133900100803     C*================================================================
134000100803     C     DECwin        BEGSR
134100100803     C*
134200100803     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI CAMPI
134300100803     C                   SETOFF                                       707172
134400100805     C                   SETOFF                                       737475
134500110610     C                   SETOFF                                       76
134600100803     c
134700100805     c* Se immesso TIPO COMUNICAZIONE controllo
134800100803      * CONTROLLO SE E' STATO INSERITO '?'
134900100803     c                   clear                   w2dtcm
135000100803     C     '?'           SCAN      w2ctcm                                 99
135100100803
135200100803      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
135300100803     c                   if        *in99                                        --- 1 -->
135400100803     c                   clear                   TIBS02DS
135500100803     c                   movel     'R'           t02mod
135600100803     c                   movel     knsif         t02sif
135700100803     c                   movel     'UNC'         t02cod
135800100803     c                   call      'TIBS02R'
135900100803     c                   parm                    KPJBA
136000100803     c                   parm                    TIBS02DS
136100100803     c                   movel     T02ke1        w2ctcm
136200100803     c                   movel     t02uni        w2dtcm
136300100804     c                   eval      $err='1'
136400100804     c                   seton                                        70
136500100803     c                   endif                                                  <-- 1 ---
136600100803
136700100803      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
136800100804     c                   if        $err='0' and w2ctcm<>*blanks
136900100803     c                   clear                   TIBS02DS
137000100803     c                   movel     'C'           T02mod
137100100803     c                   movel     knsif         t02sif
137200100803     c                   movel     'UNC'         t02cod
137300100803     c                   movel(p)  w2ctcm        T02ke1
137400100803     c                   call      'TIBS02R'
137500100803     c                   parm                    KPJBA
137600100803     c                   parm                    TIBS02DS
137700100803     c                   if        t02err <> *blanks
137800100803     c                   eval      $err = '1'
137900100803     C                   MOVEL     MSG(13)       w2cmsg
138000100804     c                   seton                                        2870
138100100803     c                   clear                   w2dtcm
138200100803     c                   else
138300100803     c                   movel     t02uni        w2dtcm
138400100803     c                   endif
138500100803     c                   endif
138600100805     c
138700100805     c* Se immesso RESPONSABILE ADDEBITO DA FILIALE controllo
138800100805      * CONTROLLO SE E' STATO INSERITO '?'
138900100805     c                   if        $err='0'
139000100805     c                   clear                   w2diafil
139100100805     C     '?'           SCAN      w2ciafil                               99
139200100805
139300100805      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
139400100805     c                   if        *in99                                        --- 1 -->
139500100805     c                   clear                   TIBS02DS
139600100805     c                   movel     'R'           t02mod
139700100805     c                   movel     knsif         t02sif
139800100805     c                   movel     'UND'         t02cod
139900100805     c                   call      'TIBS02R'
140000100805     c                   parm                    KPJBA
140100100805     c                   parm                    TIBS02DS
140200100805     c                   movel     T02ke1        w2ciafil
140300100805     c                   movel     t02uni        w2diafil
140400100805     c                   eval      $err='1'
140500100805     c                   seton                                        75
140600100805     c                   endif                                                  <-- 1 ---
140700100805
140800100805      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
140900100805     c                   if        $err='0' and w2ciafil>*blanks
141000100805     c                   clear                   TIBS02DS
141100100805     c                   movel     'C'           T02mod
141200100805     c                   movel     knsif         t02sif
141300100805     c                   movel     'UND'         t02cod
141400100805     c                   movel(p)  w2ciafil      T02ke1
141500100805     c                   call      'TIBS02R'
141600100805     c                   parm                    KPJBA
141700100805     c                   parm                    TIBS02DS
141800100805     c                   if        t02err <> *blanks
141900100805     c                   eval      $err = '1'
142000100805     C                   MOVEL     MSG(24)       w2cmsg
142100100805     c                   seton                                        2875
142200100805     c                   clear                   w2diafil
142300100805     c                   else
142400100805     c                   movel     t02uni        w2diafil
142500100805     c                   endif
142600100805     c                   endif
142700100805     c                   endif
142800100803
142900100804     c* Controllo codice fornitore
143000100805     c                   if        $err='0'
143100100804     c                   clear                   w2dfor1
143200100804     c                   clear                   w2dfor2
143300110401     c                   clear                   w2hdfor
143400100804     c                   if        $err='0' and w2cfor<>*blanks and
143500100804     c                             w2cfor<>*zeros and w2cauco=' '
143600100804     c                   eval      $err = '1'
143700100804     C                   MOVEL     MSG(21)       w2cmsg
143800100804     c                   seton                                        2874
143900100804     c                   endif
144000100804     c                   if        $err='0' and w2cauco<>' ' and
144100100804     c                             (w2cfor=*zeros or w2cfor=*blanks)
144200100804     c                   eval      $err = '1'
144300100804     C                   MOVEL     MSG(23)       w2cmsg
144400100804     c                   seton                                        2871
144500100804     c                   endif
144600101105     c* Se w2cauco='L'  --> logistica non faccio alcun controllo sul codice
144700101105     c                   if        w2cauco<>'L'
144800100804     c
144900100804     C     '?'           SCAN      w2cfor                                 99
145000100804     c                   if        $err='0' and *in99
145100100804     C                   clear                   fnlv24ds
145200100804     C                   eval      d24fil   = kmgc
145300100804     C                   eval      d24tip   = w2cauco
145400100804     C                   eval      d24flg   = 'R'
145500100804     C                   movel     fnlv24ds      kpjbu
145600100804     C                   CALL      'FNLV24R'
145700100804     C                   PARM                    KPJBA
145800100804      *
145900100804      * valorizzo i campi a video
146000100804     C                   movel     kpjbu         fnlv24ds
146100100804     C                   if        d24pdr   <> *zeros
146200100804     C                   move      d24pdr        w2cfor
146300100804     C                   movel     d24rsc        w2dfor1
146400110401     C                   movel     d24rsc        w2hdfor
146500100805     c                   eval      w2dfor2=%subst(d24rsc:13:10)
146600100804     C                   ELSE
146700100804     C                   CLEAR                   w2cfor
146800100804     C                   endif
146900100804     c                   eval      $err='1'
147000100804     c                   seton                                        71
147100100804     C                   endif
147200100804     c* controllo
147300100804    1c                   if        $err='0' and w2cfor>*zeros
147400100804     C                   TESTN                   w2cfor               30
147500100804     C                   MOVE      w2cfor        W001A             1
147600100804    2C     *IN30         IFEQ      *OFF
147700100804     C     W001A         ORLT      '0'
147800100804     c                   eval      $err = '1'
147900100804     C                   MOVEL     MSG(22)       w2cmsg
148000100804     c                   seton                                        2871
148100100804    2c                   endif
148200100804    1c                   endif
148300100804     c
148400100804    1c                   if        $err='0' and w2cfor>*zeros
148500100804     C                   move      w2cfor        kpdpdr
148600100804     C                   move      w2cauco       kpdtip
148700100804     C     kfapd         chain     fiapd01l
148800100804    2C                   IF        not  %found(fiapd01l) or apdatb<>*blanks
148900100804     c                   eval      $err = '1'
149000100804     C                   MOVEL     MSG(22)       w2cmsg
149100100804     c                   seton                                        2871
149200100804     c                   else
149300100804     C                   movel     apdrsc        w2dfor1
149400110401     C                   movel     apdrsc        w2hdfor
149500100805     c                   eval      w2dfor2=%subst(apdrsc:13:10)
149600100804    2c                   endif
149700100804    1c                   endif
149800100805    1c                   endif
149900101105     c
150000101105    1c                   endif
150100100804     c
150200100803     C* CONTROLLO LA DATA convalida
150300100804     c                   clear                   invdtv
150400100804     c                   if        $err='0' and w2cdtv>0
150500100803     C                   Z-ADD     w2cdtv        G08DAT
150600100803     C                   Z-ADD     *ZEROS        G08INV
150700100803     C                   MOVEL     '0'           G08ERR
150800100803     C                   CALL      'XSRDA8'
150900100803     C                   PARM                    WLBDA8
151000100803    2C     G08ERR        IFNE      '0'
151100100803     C                   MOVEL     '1'           $ERR
151200100803     C                   MOVEL     MSG(10)       w2cmsg
151300100804     c                   seton                                        2873
151400100803   x2c                   else
151500100803     C                   Z-ADD     G08DAT        w2cdtv
151600100803     C                   Z-ADD     G08INV        invDTv            8 0
151700100803    2C                   ENDIF
151800100803    1c                   ENDIF
151900100803     c
152000100803     c* Se immessa data immettere anche convalida emissione fattura  e viceversa
152100100805     c                   if        $err='0' and w2cdtv>0 and  w2cemf=' '
152200100803     C                   MOVEL     '1'           $ERR
152300100803     C                   MOVEL     MSG(12)       w2cmsg
152400100804     c                   seton                                        2872
152500100803     c                   endif
152600100805     c                   if        $err='0' and w2cdtv=0 and  w2cemf<>' '
152700100803     C                   MOVEL     '1'           $ERR
152800100804     c                   eval      w2cdtv=dataogGMA
152900100803     c                   endif
153000110610     c* Se c'e'
153100110610     c                   if        $err='0' and w2ciafil='A ' and
153200110610     c                             w2cauco='C'  and w2cfor>*zeros
153300110610     c                             and w2ccapre<>'S'
153400110610     c                   eval      w2ccapre='S'
153500110610     c                   seton                                        76
153600110610     C                   MOVEL     '1'           $ERR
153700110610     c                   endif
153800100803     c
153900110105     c* Se immessa "S" maga sede o c'e' la "S" da tabella --> propongo importo da UNT
154000110105     c                   if        w2cimpa=0
154100110105     c                   if        w2ciamag='S' or d§untoimpd='S'
154200110105     c                   eval      w2cimpa=d§untimpd
154300110105     c                   if        w2cimpa>0
154400110105     C                   MOVEL     '1'           $ERR
154500110105     c                   endif
154600110105     c                   endif
154700110105     c                   endif
154800100803     C*
154900100803     c                   endsr
155000100804     C*================================================================
155100100804     C*  AggioMOR - Aggiorno file accertamento responsabilità
155200100804     C*================================================================
155300100804     C     AggioMOR      BEGSR
155400100804     c                   eval      unrtcm=w2ctcm
155500100805     c                   eval      unrcms=w2ccms
155600100805     c                   eval      unrdes=w2cdes
155700100804     c                   eval      unriafil=w2ciafil
155800100804     c                   eval      unriamag=w2ciamag
155900100804     c                   eval      unrcapre=w2ccapre
156000100804     c                   if        w2cfor>*zeros
156100100804     c                   movel     w2cfor        unrpdr
156200110401     c                   movel     w2hdfor       unrrscf
156300101105     c                   eval      unrtipp=w2cauco
156400101105     c
156500101105     c* Logistica--> scrivo anche nel cod fornitore quanto immesso a video NON controllato
156600101105     c                   if        w2cauco='L'
156700101105     c                   movel     w2cfor        unrfor
156800101105     c                   movel     '201'         unrcsf
156900110401     c                   clear                   unrrscf
157000101105     c                   else
157100100804     c                   eval      unrfor  =apdksc
157200100804     c                   eval      unrcsf  =apdcsf
157300101105     c                   endif
157400100804     c                   else
157500100804     c                   clear                   unrpdr
157600100804     c                   clear                   unrfor
157700100804     c                   clear                   unrcsf
157800100804     c                   clear                   unrtipp
157900110401     c                   clear                   unrrscf
158000100804     c                   endif
158100100804     c                   eval      unrimpa =w2cimpa
158200100805     c                   eval      unremf  =w2cemf
158300100804     c                   eval      unrdtv  =invdtv
158400100804     c                   if        %found(unmor01l)
158500100804     c                   update    unmor000
158600100804     c                   else
158700100804     c                   eval      unrtip=unmtip
158800100804     c                   eval      unrcod=unmcod
158900100804     c                   eval      unrdtm=unmdtm
159000100804     c                   eval      unrmgc=kmgc
159100100804     c                   write     unmor000
159200100804     c                   endif
159300100804     c
159400100804     c                   endsr
159500101028     C*=================================================================================
159600101027     C* cerca il movimento di UNGIO con la matricola di reso per aggiornare flag di reso
159700101028     C*=================================================================================
159800101027     C     AggioUNGIO    BEGSR
159900101027     c
160000101027      /free
160100101028         if savksc<= *zeros   ;
160200101027         wrkgetlista='SELECT rrn(UNGIO00F) from  +
160300101027                      UNGIO00F WHERE ungtir=' + %editc(unatip:'X')
160400101027            +' and ungmatr=''' + unamat + ''' and ungmag=' + %editc(unamag:'X')+
160500101027             ' and ungreso='' '''    ;
160600101028         else  ;
160700101028         wrkgetlista='SELECT rrn(UNGIO00F) from  +
160800101028                      UNGIO00F WHERE ungtir=' + %editc(unatip:'X')
160900101028            +' and ungmatr=''' + unamat + ''' and ungksc =' + savksc  +
161000101028             ' and ungreso='' '''    ;
161100101028         endif   ;
161200101028
161300101027         WFLAG='S'  ;
161400101027         exsr PREPA   ;
161500101027
161600101027
161700101027         ENDSR        ;
161800101027
161900101028        // ================================================================
162000101028        //  Prepara SQL
162100101028        //  ================================================================
162200101027         BEGSR  PREPA   ;
162300101027         exec sql   PREPARE s1 FROM :wrkgetlista   ;
162400101027
162500101027         exec sql   DECLARE a1 cursor FOR  s1   ;
162600101027
162700101027         exec sql OPEN a1   ;
162800101027
162900101027         clear sqlcod   ;
163000101027
163100101027
163200101027         clear savnrr  ;
163300101027         exec sql FETCH NEXT  FROM a1 INTO: gionrr  ;
163400101027
163500101027         select ;
163600101027         when sqlcod=100 ;
163700101027
163800101027         when sqlcod<0   ;
163900101027         *inh1=*on   ;
164000101027
164100101027         other   ;
164200101027         // Memorizzo il numero relativo record
164300101027         if  wflag='S'   ;
164400101027         chain gionrr ungiofis  ;
164500101027         F_ungreso='S'   ;
164600101027         update ungiofis  ;
164700101027         else  ;
164800101027         chain gionrr ungiofis  ;
164900101027         F_ungreso=' '   ;
165000101027         update ungiofis  ;
165100101027         endif  ;
165200101027
165300101027         endsl  ;
165400101027
165500101027
165600101027         exec sql CLOSE a1   ;
165700101027         ENDSR   ;
165800101027
165900101027      /end-free
166000101027     C*================================================================
166100101027     C* cerca il movimento di UNGIO con la matricola di reso per toglòeire  flag di reso
166200101027     C*================================================================
166300101027     C     SflaggaUNGIO  BEGSR
166400101027     c
166500101027      /free
166600101027         wrkgetlista='SELECT rrn(UNGIO00F)   from  +
166700101027                      UNGIO00F WHERE ungtir=' + %editc(unatip:'X')
166800101027            +' and ungmatr=''' + unamat + ''' and ungmag=' + %editc(unamag:'X')+
166900101027             ' and ungreso=''S''  order by ungdtm  desc' ;
167000101027
167100101027         WFLAG=' '  ;
167200101027         exsr PREPA   ;
167300101027
167400101027      /end-free
167500101027     c                   ENDSR
167600000000     C*================================================================
167700930713     C*  *INZSR - ROUTINE INIZIALE
167800000000     C*================================================================
167900930713     C     *INZSR        BEGSR
168000000000     C*
168100000000     C* RICEVIMENTO PARAMETRI
168200000000     C     *ENTRY        PLIST
168300940105     C                   PARM                    KPJBA
168400100907     C                   PARM                    dsungio
168500100908     C                   MOVEL     KPJBU         DSUNIT
168600020321
168700020321      * CHIAVE LETTURA UNMOV01L
168800931015     C     KEYUNM        KLIST
168900020322     C                   KFLD                    UNMTIP
169000930712     C                   KFLD                    UNMCOD
169100930712     C                   KFLD                    UNMDTM
169200930712     C                   KFLD                    UNMPRG
169300931217     C                   KFLD                    UNMTMO
169400020321      * CHIAVE LETTURA UNMOV01L
169500941216     C     KEYUN2        KLIST
169600020322     C                   KFLD                    UNMTIP
169700941216     C                   KFLD                    UNMCOD
169800941216     C                   KFLD                    UNMDTM
169900020321
170000020321      * CHIAVE LETTURA UNANA01L
170100931015     C     KEYUNA        KLIST
170200020322     c                   kfld                    unatip
170300931015     C                   KFLD                    UNACOD
170400100907     C     Kuna2         KLIST
170500110412     c                   kfld                    ungtip
170600110412     C                   KFLD                    v1cmat
170700020321
170800100803     C     kUNR          KLIST
170900100803     C                   KFLD                    UNMTIP
171000100803     C                   KFLD                    UNMCOD
171100100803     C                   KFLD                    Kmgc
171200100803     C                   KFLD                    UNMDTM
171300100804     C     Kfapd         klist
171400100804     C                   kfld                    kpdtip
171500100804     C                   kfld                    kpdpdr
171600940105     C*
171700100804     c     *iso          movel     datasys       dataogAMG         8 0
171800100804     c     *eur          movel     datasys       dataogGMA         8 0
171900100804     C*
172000940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DELL'ANAGRAF. UNITA'
172100940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
172200940105     C     DSUPRO        IFEQ      'A'
172300940105     C                   MOVEL     FUN(9)        FUNRIG                         *GEST.MOVIMENTI
172400940105     C                   MOVEL     'N'           FUNASG
172500940105     C                   MOVEL     'N'           FUNASI
172600940105     C                   MOVEL     'N'           FUNAFG
172700940105     C                   MOVEL     'N'           FUNAFI
172800940105     C                   MOVEL     FUNRIG        FUN(9)
172900940105     C                   END
173000930714     C*
173100930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
173200930803     C                   EXSR      INZSTD
173300930714     C*
173400930806     C* IMPOSTO CAMPI TESTATA VIDEO
173500931015     C     DSUOP1        IFEQ      'F06'
173600100907     C     DSUOP1        oreq      'F6N'
173700931015     C                   MOVEL     '  INSERI'    VTDFUN
173800931015     C                   MOVE      'MENTO   '    VTDFUN
173900020321     c                   eval      *in66 = *on
174000931015     C                   END
174100931015     C     DSUOP1        IFEQ      'O02'
174200931015     C                   MOVEL     '    MODI'    VTDFUN
174300931015     C                   MOVE      'FICA    '    VTDFUN
174400020321     c                   eval      *in66 = *off
174500931015     C                   END
174600931015     C     DSUOP1        IFEQ      'O04'
174700931015     C                   MOVEL     ' CANCELL'    VTDFUN
174800931015     C                   MOVE      'AZIONE  '    VTDFUN
174900020321     c                   eval      *in66 = *off
175000931015     C                   END
175100931015     C     DSUOP1        IFEQ      'O05'
175200931015     C                   MOVEL     'VISUALIZ'    VTDFUN
175300931015     C                   MOVE      'ZAZIONE '    VTDFUN
175400020321     c                   eval      *in66 = *off
175500931015     C                   END
175600931015     C     DSUOP1        IFEQ      'O07'
175700931015     C                   MOVEL     '   RIPRI'    VTDFUN
175800931015     C                   MOVE      'STINO   '    VTDFUN
175900020321     c                   eval      *in66 = *off
176000931015     C                   END
176100930713     C*
176200931015     C* SE VISUALIZZAZIONE TOLGO FUNZIONE AGGIORNAMENTO
176300931015     C     DSUOP1        IFEQ      'O05'
176400931015     C                   MOVEL     FUN(6)        FUNRIG                         *AGGIORNAMENTO
176500931015     C                   MOVEL     'N'           FUNASG
176600931015     C                   MOVEL     'N'           FUNASI
176700931015     C                   MOVEL     'N'           FUNAFG
176800931015     C                   MOVEL     'N'           FUNAFG
176900931015     C                   MOVEL     FUNRIG        FUN(6)
177000931015     C                   END
177100931015     C*
177200931015     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
177300931015     C* 0=NON PROTETTO, 1=PROTETTO
177400931015     C*   90 - CAMPI CHIAVE
177500931015     C*   91 - ALTRI CAMPI
177600931015     C* --> UTENTE ENTRATO IN GESTIONE
177700940105     C     DSUOP0        IFEQ      ' 2'                                         --- 1 -->
177800931015     C     DSUOP1        IFEQ      'F06'                                        *INS.
177900100908     C     DSUOP1        oreq      'F6N'                                        *INS.
178000931015     C                   SETOFF                                       9091
178100931015     C                   END
178200931015     C     DSUOP1        IFEQ      'O02'                                        *MOD.
178300931015     C                   SETON                                        90
178400020709     c                   If        DutLpo = 'S'
178500931015     C                   SETOFF                                       91
178600931015     C                   ELSE                                                   *FIL.
178700931015     C                   SETON                                        91
178800931015     C                   END
178900931015     C                   END
179000931015     C     DSUOP1        IFEQ      'O05'                                        *VIS+CANC+RIPR
179100931015     C     DSUOP1        OREQ      'O04'
179200931015     C     DSUOP1        OREQ      'O07'
179300931015     C                   SETON                                        9091
179400931015     C                   END
179500931015     C                   END                                                    <-- 1 ---
179600940105     C     DSUOP0        IFEQ      ' 5'                                         *INTERROGAZ.
179700931015     C                   SETON                                        9091
179800931015     C                   END
179900931015     C*
180000931015     C* SE SONO IN MOD. O VIS. O CANC. O RIPR. LEGGO IL RECORD IN INPUT
180100931015     C     DSUOP1        IFEQ      'O02'                                        --- 1 -->
180200931015     C     DSUOP1        OREQ      'O04'
180300931015     C     DSUOP1        OREQ      'O05'
180400931015     C     DSUOP1        OREQ      'O07'
180500931015     C* SE IL REC NON ESISTE RIMANDO ERRORE AL PGM CHIAMANTE
180600020322     C                   Z-ADD     DSUTIP        UNMTIP
180700931015     C                   Z-ADD     DSUCOD        UNMCOD
180800931015     C                   Z-ADD     DSUDTM        UNMDTM
180900931015     C                   Z-ADD     DSUPRG        UNMPRG
181000931217     C                   MOVEL     DSUTMO        UNMTMO
181100931015     C     KEYUNM        CHAIN     UNMOV01L                           99
181200931015     C     *IN99         IFEQ      '1'
181300931015     C                   MOVEL     '1'           $ERR
181400931015     C                   MOVEL     '1'           $FINE
181500931015     C                   MOVEL     MSG(14)       DSUMSG
181600931015     C                   ELSE
181700931015     C                   EXSR      RECVID                                       *RECORD ===> VIDEO
181800931015     C                   EXSR      DECODI                                       *DECODIFICA CAMPI VI
181900931015     C                   EXSR      CARV1                                        *CARICA TESTATA VIDE
182000931015     C                   END
182100931210     C                   END                                                    <-- 1 ---
182200100908     c
182300931015     C* SE INSERIMENTO AZZERO TUTTI I CAMPI DEL VIDEO
182400100908    1C     DSUOP1        IFEQ      'F06'
182500100908     C     DSUOP1        oreq      'F6N'
182600931015     C                   EXSR      AZZVID
182700940104     C                   MOVE      DSUTIP        V1CTIP
182800940104     C                   Z-ADD     DSUCOD        V1CCOD
182900931210     C                   EXSR      CARV1                                        *CARICA TESTATA VIDE
183000100908     c
183100100907     c* Se richiamato da conferma movimenti giornalieri carico i dati del video
183200100908    2c                   if        dsuop1='F6N' and %parms>1
183300100908     c                   clear                   savtmag
183400100907     c                   exsr      RieVid
183500100908    3c                   if        $err='0'
183600100908     c                   exsr      INSERI
183700110404     c
183800110404     c* se causale FISE e ho guasto fisico--> scrivo già UNMOR
183900110803     c* Scrivo anche se importo obbligatorio da tabella UNT
184000110404     c                   if        v1ccau='FISE'
184100110404     c                   eval      dgio01=ungflo
184200110803     c                   if        §giogsf='S'  or (d§untoimpd='S'
184300110803     c                              and d§untimpd>0)
184400110404     c                   clear                   unmor000
184500110404     c                   eval      unrtip=unmtip
184600110404     c                   eval      unrcod=unmcod
184700110404     c                   eval      unrdtm=unmdtm
184800110404     c                   eval      unrmgc=unmmgc
184900110404     c                   eval      unriamag='S'
185000110404     c                   eval      unrdes=ungnot
185100110610     c                   eval      unrimpa=d§untimpd
185200110404     c
185300110404     c                   write     unmor000
185400110404     c                   endif
185500110404     c                   endif
185600100908     c
185700100908     c* se SAVTMAG ='Y' devo scrivere anche il 2° movimento
185800100908    4c                   if        savtmag='Y'
185900100908     C                   EXSR      AZZVID
186000100908     C                   MOVE      DSUTIP        V1CTIP
186100100908     C                   Z-ADD     DSUCOD        V1CCOD
186200100908     C                   EXSR      CARV1                                        *CARICA TESTATA VIDE
186300100908     c                   exsr      RieVid
186400100908     c                   exsr      INSERI
186500100908e   4c                   endif
186600100908     c
186700100908     c                   eval      $fine='1'
186800100908e   3c                   endif
186900100908e   2c                   endif
187000100908     c
187100100903     c                   exsr      decodi
187200100903     c                   eval      $err='0'
187300100908    1C                   END
187400000000     C*
187500000000     C                   ENDSR
187600971010     C*================================================================
187700930805     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
187800930805     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
187900930805     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
188000930805     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
188100930805     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
188200930805     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
188300930805     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
188400930714     C*================================================================
188500930803     C* INZSTD - ROUTINE INIZIALE STANDARD
188600930714     C*================================================================
188700930803     C     INZSTD        BEGSR
188800930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
188900930802     C                   CALL      'XKEYS'
189000930802     C                   PARM                    DSKEYS
189100930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
189200930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
189300930714     C     XMSGPA        PLIST
189400930714     C                   PARM                    DSMSMS
189500930714     C                   PARM                    DSMSPG
189600930714     C                   PARM                    DSMSKE
189700930714     C                   PARM                    DSMSER
189800930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
189900941115     C                   MOVEL     'TNSDU5R'     DSMSPG                         *NOME PGM CHIAMANTE
190000941115     C                   MOVEL     'TNSDU5R'     VMCQUE                         *CODA MESSAGGI DESTI
190100930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
190200930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
190300930805     C* INIZIALIZZO FLAG CONTROLLO PGM
190400930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
190500930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
190600930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
190700930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
190800930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
190900930721     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
191000930714     C* LANCIO ROUTINE PER VERIFICARE SE
191100930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
191200930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
191300020711
191400020711     c     *dtaara       define    §azute        azuteds
191500020711     c     *dtaara       define    §datiute      ddatiute
191600020711
191700020711     c                   in(E)     *dtaara
191800020711     c                   if        %Error or rsut = *blanks
191900020711     c                   clear                   tibs34ds
192000020711     c                   call      'TIBS34R'
192100020711     c                   parm                    Tibs34Ds
192200020711     c                   in        *dtaara
192300020711     c                   endif
192400020711
192500930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
192600930721     C                   BITOFF    '01234567'    POSMI1            1
192700930721     C                   BITOFF    '01234567'    POSMI2            1
192800930721     C                   BITOFF    '01234567'    POSMA1            1
192900930721     C                   BITOFF    '01234567'    POSMA2            1
193000930721     C                   BITON     '34'          POSMI1            1            *18
193100930721     C                   BITON     '7'           POSMI2            1            *01
193200930721     C                   BITON     '34'          POSMA1            1            *18
193300930721     C                   BITON     '0'           POSMA2            1            *80
193400930721     C                   MOVE      POSMI2        POSMIN            2
193500930721     C                   MOVEL     POSMI1        POSMIN
193600930721     C                   MOVE      POSMA2        POSMAX            2
193700930721     C                   MOVEL     POSMA1        POSMAX
193800930714     C                   ENDSR
193900930804     C*================================================================
194000930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
194100930804     C*================================================================
194200930804     C     FUNSTD        BEGSR
194300930804     C                   SELECT
194400930804     C     KEY           WHENEQ    F3
194500930804     C                   EXSR      F03STD
194600930804     C     KEY           WHENEQ    F12
194700930804     C                   EXSR      F12STD
194800930804     C     KEY           WHENEQ    F24
194900930804     C                   EXSR      F24STD
195000930804     C                   OTHER
195100930804     C                   MOVEL     MSG(2)        $MSG
195200930804     C                   EXSR      CARMSG
195300930804     C                   ENDSL
195400930804     C                   ENDSR
195500930804     C*================================================================
195600930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
195700930804     C*================================================================
195800930804     C     F03STD        BEGSR
195900940105     C* IMPOSTO FLAG DI PREMUTO TASTO F03
196000940105     C                   MOVEL     '1'           $F03
196100930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
196200930804     C                   MOVEL     '1'           $FINE
196300930804     C                   ENDSR
196400930804     C*================================================================
196500930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
196600930804     C*================================================================
196700930804     C     F12STD        BEGSR
196800930805     C* IMPOSTO FLAG DI FIN PGM
196900930804     C                   MOVEL     '1'           $FINE
197000930804     C                   ENDSR
197100930804     C*================================================================
197200930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
197300930804     C*================================================================
197400930804     C     F24STD        BEGSR
197500930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
197600930804     C                   EXSR      OUTFUN
197700930804     C                   ENDSR
197800930721     C*================================================================
197900930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
198000930721     C*          CODA MESSAGGI DEL PGM
198100930721     C*================================================================
198200930721     C     CARMSG        BEGSR
198300930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
198400930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
198500930721     C     MSGULT        IFEQ      0
198600930721     C                   MOVEL     '1'           *IN40
198700941115     C                   WRITE     SDU5DVK
198800930721     C                   MOVEL     '0'           *IN40
198900930805     C                   ENDIF
199000930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
199100930721     C                   MOVEL     $MSG          DSMSMS
199200930721     C                   CALL      'XMSG'        XMSGPA
199300930721     C     DSMSER        IFEQ      0
199400930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
199500930805     C                   ENDIF
199600930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
199700931015     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
199800941115     C                   WRITE     SDU5DVM                                      *SFL MESS
199900930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
200000930721     C                   MOVEL     *BLANKS       $MSG
200100930721     C                   ENDSR
200200930721     C*================================================================
200300930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
200400930721     C*================================================================
200500930721     C     OUTMSG        BEGSR
200600930721     C* VISUALIZZAZIONE MESSAGGIO
200700930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
200800941115     C                   WRITE     SDU5DVK                                      *CONTR MESS
200900930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
201000930721     C                   Z-ADD     *ZEROS        MSGULT
201100930721     C                   ENDSR
201200930803     C*================================================================
201300930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
201400930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
201500930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
201600930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
201700930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
201800930803     C*================================================================
201900930803     C     CARFUN        BEGSR
202000930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
202100931015     C     1             DO        31            I                 5 0          --- 1 -->
202200930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
202300930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
202400930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
202500020709     c                   If        DutLpo = 'S'
202600940105     C     DSUOP0        IFEQ      ' 2'                                         *GESTIONE
202700930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
202800930803     C                   ELSE                                                   *INTERROG.
202900930803     C                   MOVEL     FUNASI        FAT(I)
203000930803     C                   ENDIF
203100930803     C                   ELSE                                                   <-- 2 --> *FILIALE
203200930803     C     DSUOP0        IFEQ      ' 2'                                         *GESTIONE
203300930803     C                   MOVEL     FUNAFG        FAT(I)
203400930803     C                   ELSE                                                   *INTERROG.
203500930803     C                   MOVEL     FUNAFI        FAT(I)
203600930803     C                   ENDIF
203700930803     C                   ENDIF                                                  <-- 2 ---
203800930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
203900930806     C                   MOVEL     USER          WEDP              3            *PROFILO
204000930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
204100930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
204200930806     C                   ELSE
204300930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
204400930806     C                   ENDIF
204500930806     C                   ENDIF
204600930803     C                   ENDDO                                                  <-- 1 ---
204700930803     C                   ENDSR
204800930803     C*================================================================
204900930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
205000930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
205100930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
205200930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
205300930803     C*================================================================
205400930803     C     OUTFUN        BEGSR
205500930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
205600930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
205700930803     C                   ENDDO
205800930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
205900930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
206000930803     C     1             DO        31            I
206100930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
206200930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
206300930803     C                   ADD       1             FUNNAT
206400930803     C                   ENDIF
206500930803     C                   ENDDO
206600930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
206700930803     C     FUNNAT        IFGT      8
206800930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
206900930803     C                   ENDIF
207000930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
207100930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
207200930803     C                   Z-ADD     8             FUNVIS            5 0
207300930803     C     FAT(23)       IFEQ      'S'
207400930803     C                   SUB       1             FUNVIS
207500930803     C                   ENDIF
207600930803     C     FAT(24)       IFEQ      'S'
207700930803     C                   SUB       1             FUNVIS
207800930803     C                   ENDIF
207900930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
208000931015     C                   Z-ADD     *ZEROS        W                 5 0
208100930803     C                   ADD       1             FUNULT
208200930803     C     FUNULT        IFGT      31
208300930803     C                   Z-ADD     1             FUNULT
208400930805     C                   ENDIF
208500930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
208600930803     C     FUNULT        DO        31            I                              --- 1 -->
208700930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
208800930803     C     W             IFLT      FUNVIS                                       --- 2 -->
208900930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
209000930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
209100930803     C     FDE(I)        ANDNE     *BLANKS
209200930805     C     I             IFEQ      23                                           *TOLGO F23
209300930805     C     I             OREQ      24                                           *E F24
209400930803     C                   ELSE
209500930803     C                   ADD       1             W
209600930803     C                   MOVEL     FDE(I)        FUV(W)
209700930803     C                   ENDIF
209800930803     C                   ENDIF                                                  <-- 3 ---
209900930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
210000930803     C                   Z-ADD     I             FUNULT
210100930803     C                   ENDIF                                                  <-- 2 ---
210200930803     C                   ENDDO                                                  <-- 1 ---
210300930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
210400930806     C                   MOVEL     FUV(1)        VFD001
210500930806     C                   MOVEL     FUV(2)        VFD002
210600930806     C                   MOVEL     FUV(3)        VFD003
210700930806     C                   MOVEL     FUV(4)        VFD004
210800930806     C                   MOVEL     FUV(5)        VFD005
210900930806     C                   MOVEL     FUV(6)        VFD006
211000930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
211100930803     C     FAT(24)       ANDEQ     'N'
211200930806     C                   MOVEL     FUV(7)        VFD007
211300930806     C                   MOVEL     FUV(8)        VFD008
211400930803     C                   ELSE                                                   <-- 1 -->
211500930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
211600930803     C     FAT(24)       ANDEQ     'S'
211700930806     C                   MOVEL     FDE(23)       VFD007
211800930806     C                   MOVEL     FDE(24)       VFD008
211900930803     C                   ELSE                                                   <-- 2 -->
212000930806     C                   MOVEL     FUV(7)        VFD007
212100930803     C     FAT(23)       IFEQ      'S'
212200930806     C                   MOVEL     FDE(23)       VFD008
212300930803     C                   ELSE
212400930806     C                   MOVEL     FDE(24)       VFD008
212500930805     C                   ENDIF
212600930805     C                   ENDIF                                                  <-- 2 ---
212700930805     C                   ENDIF                                                  <-- 1 ---
212800930803     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
212900930803     C                   ENDSR
213000930803     C*================================================================
213100930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
213200930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
213300930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
213400930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
213500930803     C*================================================================
213600930803     C     CONFUN        BEGSR
213700930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
213800930803     C     1             DO        31            I                              --- 1 -->
213900930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
214000940105     C* CONTROLLO SE IL TASTO E' ATTIVO
214100930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
214200930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
214300930803     C                   EXSR      CARMSG
214400930803     C                   MOVEL     '1'           $ERR
214500930803     C                   ENDIF                                                  <-- 3 ---
214600930803     C                   ENDIF                                                  <-- 2 ---
214700930803     C                   ENDDO                                                  <-- 1 ---
214800930803     C                   ENDSR
214900930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
215000941115TNSDU5R- Tasto funzionale non valido.                                          1
215100941115TNSDU5R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
215200941115TNSDU5R- Opzione non valida.                                                   3
215300941115TNSDU5R- Opzione abilitata ma non gestita. Avvertire CED.                      4
215400941115TNSDU5R- Inizio elenco.                                                        5
215500941115TNSDU5R- Fine elenco.                                                          6
215600020321TNSDU5R- Cliente non accettabile.                                              7
215700941115TNSDU5R- Magazzino errato.                                                     8
215800941115TNSDU5R- Tipo unità errato.                                                    9
215900941115TNSDU5R- Data errata.                                                         10
216000941115TNSDU5R- Codice unità non accettabile.                                        11
216100100804Se data convalida immettere anche emissioneFattura S/N                        12
216200100804Tipo comunicazione respo.danno errato                                         13
216300971013TNSDU5R- Richiesta funzione per movimento inesistente                         14
216400100804Inserimento obbligatorio: premere F6                                          15
216500941115TNSDU5R- Causale inesistente.                                                 16
216600941115TNSDU5R- Inserimento effettuato.                                              17
216700941115TNSDU5R- Modifica effettuata.                                                 18
216800941115TNSDU5R- Cancellazione effettuata.                                            19
216900941115TNSDU5R- Ripristino effettuato.                                               20
217000101105Immettere tipo fornitore: A=Aut  C=Coop  L=Coop Logistica                     21
217100101105Fornitore inesistente                                                         22
217200100804Immettere cod.Fornitore se inserito  Tipo                                     23
217300100805Responsabile del danno errato                                                 24
217400100921Magazzino Destinazione incongruente con la causale                            25
217500100921Magazzino Correlato incongruente con la causale                               26
217600110401Se unità in proprietà alla 923 non posso accettare MAG 990                    27
217700000000** ======== SCHIERA: CMD  (COMANDI)                        ================
217800000000WRKSPLF
217900971104DSPRCDLCK FILE(*LIBL/UNMOV00F)
218000000000
218100000000
218200000000
218300930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
218400930806F1    -                   -    NN     NN
218500930723F2    -                   -    NN     NN
218600930802F3    -F3=Fine            -    SS     SS
218700930723F4    -                   -    NN     NN
218800930723F5    -                   -    NN     NN
218900931015F6    -F6=Aggiornamento   -    SN     NN
219000930723F7    -                   -    NN     NN
219100930723F8    -                   -    NN     NN
219200971013F9    -F9=Unità           -    SS     SS
219300930723F10   -                   -    NN     NN
219400930723F11   -                   -    NN     NN
219500930802F12   -F12=Ritorno        -    SS     SS
219600930723F13   -                   -    NN     NN
219700930723F14   -                   -    NN     NN
219800930723F15   -                   -    NN     NN
219900930723F16   -                   -    NN     NN
220000930723F17   -                   -    NN     NN
220100930723F18   -                   -    NN     NN
220200930723F19   -                   -    NN     NN
220300930723F20   -                   -    NN     NN
220400971013F21   -F21=Stampa         -    SS     SS
220500930804F22   -                   -    NN     NN
220600930723F23   -                   -    NN     NN
220700930803F24   -F24=Altri tasti    -    NN     NN
220800930802ENTER -                   -    SS     SS
220900930802ROLLUP-                   -    NN     NN
221000930802ROLLDN-                   -    NN     NN
221100930802HELP  -                   -    NN     NN
221200930802PRINT -                   -    NN     NN
221300930802BSPACE-                   -    NN     NN
221400930802CLEAR -                   -    NN     NN
