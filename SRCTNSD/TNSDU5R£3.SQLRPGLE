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
040101140509     c                   eval      %subst(unalsw:1:14)=*blanks
040200020321     c                   endif
040300100907     c
040400100907     c* Se "FISE" e unità da rientrare, tolgo il reso dalle unità
040500100908     c                   if        unmcau='FISE' and unmreso='S'
040600100907     c                   eval      %subst(unalsw:15:1)=' '
040700101027     c
040800101027     c* Cerco record di UNGIO per aggiornare anche lì flag RESO
040900101027     c                   exsr      AggioUNGIO
041000100907     c                   endif
041100100907     c
041200931217     C                   MOVE      V1CMAG        UNAMAG                         *MAG UBIC.
041300020321      * se causale 'SECL' devo mettere mag 951 e cliente
041400101006     c***                if        unmcau = 'SECL'  or unmcau='FICL'
041500101006     c                   if        §unmksc='S'
041600020321     c                   eval      unamag = 951
041700020321     c                   eval      unaksc = unmksc
041800020321     c                   endif
041900100908     c
042000100908     c* Se inserimento da movimenti gironalieri la descrizione la scrivo in UNADES
042100100908     c                   if        dsuop1='F6N'
042200100908     c                   movel     unmdes        unades
042300100908     c                   endif
042400100908     c
042500931217     C                   UPDATE    UNANA000
042600931217     C                   ENDIF
042700941215     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
042800941215     C                   MOVEL     '1'           $FINE
042900930806     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
043000931210     C                   MOVEL     MSG(17)       DSMSMS
043100930806     C*
043200930907     C                   END                                                    <-- 1 ---
043300930806     C*
043400930806     C                   ENDSR
043500930806     C*================================================================
043600930806     C* MODIFI - FUNZIONE DI MODIFICA
043700930806     C*================================================================
043800930806     C     MODIFI        BEGSR
043900930806     C*
044000930806     C* CONTROLLA I CAMPI IMMESSI
044100930806     C                   EXSR      DECODI
044200930806     C*
044300930906     C     $ERR          IFEQ      '0'                                          --- 1 -->
044400930806     C*
044500931217     C* AGGIORNO IL RECORD DI USCITA (SE C'E')
044600020322     C                   Z-ADD     DSUTIP        UNMTIP
044700931217     C                   Z-ADD     DSUCOD        UNMCOD
044800931217     C                   Z-ADD     DSUDTM        UNMDTM
044900931217     C                   Z-ADD     DSUPRG        UNMPRG
045000931217     C                   MOVEL     'U'           UNMTMO
045100931217     C     KEYUNM        CHAIN     UNMOV01L                           99
045200931217     C     *IN99         IFEQ      '0'
045300931217     C                   MOVEL     V1CDES        UNMDES
045400931217     C                   Z-ADD     V1CDOC        UNMDOC
045500971010     C                   Z-ADD     V1CDDO        G08DAT
045600971010     C                   Z-ADD     *ZEROS        G08INV
045700971010     C                   MOVEL     '0'           G08ERR
045800971010     C                   CALL      'XSRDA8'
045900971010     C                   PARM                    WLBDA8
046000971010     C                   Z-ADD     G08INV        UNMDDO
046100931217     C                   Z-ADD     V1CCNT        UNMCNT
046200971014     C                   MOVEL     *BLANKS       UNMATB
046300020321     c                   eval      unmksc = v1cksc
046400931015     C                   UPDATE    UNMOV000
046500931217     C                   ENDIF
046600931217     C* AGGIORNO IL RECORD DI ENTRATA
046700020322     C                   Z-ADD     DSUTIP        UNMTIP
046800931217     C                   Z-ADD     DSUCOD        UNMCOD
046900931217     C                   Z-ADD     DSUDTM        UNMDTM
047000931217     C                   Z-ADD     DSUPRG        UNMPRG
047100931217     C                   MOVEL     'E'           UNMTMO
047200931217     C     KEYUNM        CHAIN     UNMOV01L                           99
047300931217     C     *IN99         IFEQ      '0'
047400931217     C                   MOVEL     V1CDES        UNMDES
047500931217     C                   Z-ADD     V1CDOC        UNMDOC
047600971010     C                   Z-ADD     V1CDDO        G08DAT
047700971010     C                   Z-ADD     *ZEROS        G08INV
047800971010     C                   MOVEL     '0'           G08ERR
047900971010     C                   CALL      'XSRDA8'
048000971010     C                   PARM                    WLBDA8
048100971010     C                   Z-ADD     G08INV        UNMDDO
048200931217     C                   Z-ADD     V1CCNT        UNMCNT
048300971014     C                   MOVEL     *BLANKS       UNMATB
048400020321     c                   eval      unmksc = v1cksc
048500931217     C                   UPDATE    UNMOV000
048600931217     C                   ENDIF
048700930806     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
048800930906     C                   MOVEL     '1'           $FINE
048900930806     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI MODIF. EFFETTUATA
049000931210     C                   MOVEL     MSG(18)       DSUMSG
049100930806     C*
049200930806     C                   END                                                    <-- 1 ---
049300930806     C*
049400930806     C                   ENDSR
049500930806     C*================================================================
049600930806     C* CANCEL - FUNZIONE DI CANCELLAZIONE
049700930806     C*================================================================
049800930806     C     CANCEL        BEGSR
049900930806     C*
050000931217     C* ANNULLO MOVIMENTO USCITA (SE C'E')
050100020322     C                   Z-ADD     DSUTIP        UNMTIP
050200931217     C                   Z-ADD     DSUCOD        UNMCOD
050300931217     C                   Z-ADD     DSUDTM        UNMDTM
050400931217     C                   Z-ADD     DSUPRG        UNMPRG
050500931217     C                   MOVEL     'U'           UNMTMO
050600931217     C     KEYUNM        CHAIN     UNMOV01L                           99
050700931217     C     *IN99         IFEQ      '0'
050800971014IF  2C     UNMDTR        IFEQ      *ZEROS                                       *MAI TRASMESSO
050900971014     C                   DELETE    UNMOV000
051000971014X   2C                   ELSE                                                   *GIA' TRASMESSO
051100931217     C                   MOVEL     'A'           UNMATB
051200931015     C                   UPDATE    UNMOV000
051300971014E   2C                   ENDIF
051400931217     C                   ENDIF
051500931217     C* ANNULLO MOVIMENTO ENTRATA
051600020322     C                   Z-ADD     DSUTIP        UNMTIP
051700931217     C                   Z-ADD     DSUCOD        UNMCOD
051800931217     C                   Z-ADD     DSUDTM        UNMDTM
051900931217     C                   Z-ADD     DSUPRG        UNMPRG
052000931217     C                   MOVEL     'E'           UNMTMO
052100931217     C     KEYUNM        CHAIN     UNMOV01L                           99
052200931217     C     *IN99         IFEQ      '0'
052300931217     C* MEMORIZZO MAG.CORR. CIOE' IL MAG. IN CUI RITORNA L'UNITA'
052400931217     C                   Z-ADD     UNMMGC        WNMMGC            3 0
052500971014IF  2C     UNMDTR        IFEQ      *ZEROS                                       *MAI TRASMESSO
052600971014     C                   DELETE    UNMOV000
052700971014X   2C                   ELSE                                                   *GIA' TRASMESSO
052800931217     C                   MOVEL     'A'           UNMATB
052900931217     C                   UPDATE    UNMOV000
053000971014E   2C                   ENDIF
053100971014     C                   ENDIF
053200931217     C* AGGIORNO MAG. UBICAZIONE IN ANAGRAFICA
053300020322     c                   z-add     dsutip        unatip
053400931217     C                   Z-ADD     DSUCOD        UNACOD
053500931217     C     KEYUNA        CHAIN     UNANA01L                           99        *ANAGRAFICA UNITA
053600931217     C     *IN99         IFEQ      '0'
053700931217     C                   Z-ADD     WNMMGC        UNAMAG                         *MAG UBIC.
053800100907     c
053900100907     c* se era unità resa, reimposto la "R" in anagrafica
054000100908     c                   if        unmcau='FISE' and unmreso='S'
054100101027     c                   eval      %subst(unalsw:15:1)='S'
054200101027     c
054300101027     c* Ceca UNGIO per togliere il fdlag di RESO
054400101027     c                   exsr      SflaggaUNGIO
054500100907     c                   endif
054600101006     c* Se causale con cliente, tolgo il cliente da UNAKSC
054700101006     c                   if        §unmksc='S'
054800101006     c                   clear                   unaksc
054801140509     c                   eval      %subst(unalsw:1:14)=*blanks
054900101006     c                   endif
055000100907     c
055100931217     C                   UPDATE    UNANA000
055200931217     C                   ENDIF
055300100907
055400930806     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
055500930906     C                   MOVEL     '1'           $FINE
055600930806     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI CANC. EFFETTUATA
055700931210     C                   MOVEL     MSG(19)       DSUMSG
055800930806     C*
055900930806     C                   ENDSR
056000930806     C*================================================================
056100930806     C* RIPRIS - FUNZIONE DI RIPRISTINO
056200930806     C*================================================================
056300930806     C     RIPRIS        BEGSR
056400931217     C*
056500931217     C* RIPRISTINO MOVIMENTO USCITA (SE C'E')
056600020322     C                   Z-ADD     DSUTIP        UNMTIP
056700931217     C                   Z-ADD     DSUCOD        UNMCOD
056800931217     C                   Z-ADD     DSUDTM        UNMDTM
056900931217     C                   Z-ADD     DSUPRG        UNMPRG
057000931217     C                   MOVEL     'U'           UNMTMO
057100931217     C     KEYUNM        CHAIN     UNMOV01L                           99
057200931217     C     *IN99         IFEQ      '0'
057300971014     C                   MOVEL     *BLANKS       UNMATB
057400931015     C                   UPDATE    UNMOV000
057500931217     C                   ENDIF
057600931217     C* RIPRISTINO MOVIMENTO ENTRATA
057700020322     C                   Z-ADD     DSUTIP        UNMTIP
057800931217     C                   Z-ADD     DSUCOD        UNMCOD
057900931217     C                   Z-ADD     DSUDTM        UNMDTM
058000931217     C                   Z-ADD     DSUPRG        UNMPRG
058100931217     C                   MOVEL     'E'           UNMTMO
058200931217     C     KEYUNM        CHAIN     UNMOV01L                           99
058300931217     C     *IN99         IFEQ      '0'
058400931217     C* MEMORIZZO MAG. CIOE' IL MAG. IN CUI RITORNA L'UNITA'
058500931217     C                   Z-ADD     UNMMAG        WNMMAG            3 0
058600971014     C                   MOVEL     *BLANKS       UNMATB
058700931217     C                   UPDATE    UNMOV000
058800931217     C                   ENDIF
058900931217     C* AGGIORNO MAG. UBICAZIONE IN ANAGRAFICA
059000020322     c                   z-add     dsutip        unatip
059100931217     C                   Z-ADD     DSUCOD        UNACOD
059200931217     C     KEYUNA        CHAIN     UNANA01L                           99        *ANAGRAFICA UNITA
059300931217     C     *IN99         IFEQ      '0'
059400931217     C                   Z-ADD     WNMMAG        UNAMAG                         *MAG UBIC.
059500931217     C                   UPDATE    UNANA000
059600931217     C                   ENDIF
059700930806     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
059800930906     C                   MOVEL     '1'           $FINE
059900930806     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI RIPR. EFFETTUATO
060000931210     C                   MOVEL     MSG(20)       DSUMSG
060100930806     C*
060200930806     C                   ENDSR
060300930806     C*================================================================
060400930806     C*  RECVID - PORTA I CAMPI DA UNMOV AL VIDEO
060500930806     C*================================================================
060600930806     C     RECVID        BEGSR
060700930806     C*
060800931015     C                   MOVE      UNMTIP        V1CTIP
060900930906     C                   Z-ADD     UNMCOD        V1CCOD
061000110407     C                   movel     UNMmat        V1Cmat
061100930906     C                   MOVEL     UNMCAU        V1CCAU
061200971010     C                   Z-ADD     UNMDTM        G08INV
061300971010     C                   Z-ADD     *ZEROS        G08DAT
061400971010     C                   MOVEL     '3'           G08ERR
061500971010     C                   CALL      'XSRDA8'
061600971010     C                   PARM                    WLBDA8
061700971010     C                   Z-ADD     G08DAT        V1CDTM
061800931015     C                   MOVE      UNMMAG        V1CMAG
061900931015     C                   MOVE      UNMMGC        V1CMGC
062000930906     C                   MOVEL     UNMDES        V1CDES
062100100907     c* vedo se movimento di unità reda
062200100908     c                   if        unmreso='S'
062300100907     c                   eval      v1hreso='S'
062400100907     c                   eval      v1creso='SI'
062500100907     c                   seton                                        01
062600100907     c                   else
062700100907     c                   clear                   v1hreso
062800100907     c                   eval      v1creso='NO'
062900100907     c                   setoff                                       01
063000100907     c                   endif
063100100910     c* Movimento automatico
063200100910     c                   if        unmmaut='S'
063300100910     c                   seton                                        02
063400100910     c                   else
063500100910     c                   setoff                                       02
063600100910     c                   endif
063700930906     C                   Z-ADD     UNMDOC        V1CDOC
063800971010     C                   Z-ADD     UNMDDO        G08INV
063900971010     C                   Z-ADD     *ZEROS        G08DAT
064000971010     C                   MOVEL     '3'           G08ERR
064100971010     C                   CALL      'XSRDA8'
064200971010     C                   PARM                    WLBDA8
064300971010     C                   Z-ADD     G08DAT        V1CDDO
064400930906     C                   Z-ADD     UNMCNT        V1CCNT
064500930907     C*
064600930907     C     UNMTMO        IFEQ      'E'
064700930907     C                   MOVEL     'ENTRATA'     V1DTMO
064800930907     C                   ELSE
064900930907     C                   MOVEL     'USCITA '     V1DTMO
065000930907     C                   END
065100930806     C*
065200930806     C     UNMATB        IFEQ      'A'
065300930906     C                   MOVEL     'ANNULLAT'    V1DATB
065400930906     C                   MOVE      'O'           V1DATB
065500930906     C                   ELSE
065600930906     C                   MOVEL     *BLANKS       V1DATB
065700930806     C                   END
065800931217     C*
065900931217     C* DECODIFICO MAG. CORRELATO
066000931217     C     UNMMGC        IFEQ      *ZEROS                                       --- 1 -->
066100931217     C                   MOVEL     *BLANKS       V1DMGC
066200931217     C                   ELSE                                                   <-- 1 -->
066300931217     C                   MOVE      UNMMGC        WMAG
066400931217     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
066500931217     C                   MOVEL     WDMAG         V1DMGC
066600931217     C                   ENDIF                                                  <-- 1 ---
066700020321
066800020321     c                   eval      v1cksc = unmksc
066900930906     C*
067000930906     C                   EXSR      DECODI
067100930906     C     $ERR          IFEQ      '1'
067200930906     C                   MOVEL     '0'           $ERR
067300930906     C                   ENDIF
067400930806     C*
067500930806     C                   ENDSR
067600100907     C*================================================================
067700100907     C*  RIEVID - carico video da pgm movimenti giornalieri
067800100907     C*================================================================
067900100907     C     RIEVID        BEGSR
068000100907     C*
068100110412     C                   MOVE      UNgMAT        V1CMAT
068200100907     C                   MOVE      UNgTIP        V1CTIP
068300100907
068400100908     c* Per INVIO imposto causale SEFI o SECL
068500100907     c                   select
068600100908     c                   when      unginvr='I' and ungksc>0
068700100907     C                   MOVEL     'SECL'        V1CCAU
068800100908     c                   when      unginvr='I' and ungksc=0
068900100907     C                   MOVEL     'SEFI'        V1CCAU
069000120524     c* Rientro FORNITORE in sede --> causale RIEN
069100100908     c                   when      ungcuf='FOR' and unginvr='R'
069200100908     C                   MOVEL     'RIEN'        V1CCAU
069300110412     c* se le note sono piene, imposto nella matricola il campo note--> nuova matricola
069400110412     c                   if        ungnot<>*blanks
069500110412     c                   eval      v1cmat=ungnot
069600110412     c                   endif
069700120524     c* Rientro FORNITORE diretto in filiale--> causale FIFI
069800120524     c                   when      ungcuf='FOR' and unginvr='O'
069900120524     C                   MOVEL     'FIFI'        V1CCAU
070000120524     c* se le note sono piene, imposto nella matricola il campo note--> nuova matricola
070100120524     c                   if        ungnot<>*blanks
070200120524     c                   eval      v1cmat=ungnot
070300120524     c                   endif
070400110412     c
070500100908     c                   when      ungcuf='FOR' and unginvr='F' and
070600100908     c                             savtmag=' '
070700100908     C                   MOVEL     'FISE'        V1CCAU
070800101022     c
070900101022     c* Se magazzino corrrelato è 951 devo indicare anche il codice
071000101022     c                   if        ungmgc=951
071100101022     c                   eval      v1cksc=g_unaksc
071200101022     c                   endif
071300100908     c                   when      ungcuf='FOR' and unginvr='F' and
071400100908     c                             savtmag='Y'
071500100908     C                   MOVEL     'RIPA'        V1CCAU
071600100908     c                   endsl
071700110412     c
071800110412     c* con tipo e matricola prendo il codice unità
071900110412     c     kuna2         chain     unana02l
072000110412     c                   if        %found(unana02l)
072100110412     c                   movel     g_unacod      v1ccod
072200110412     c                   else
072300110412     c                   clear                   v1ccod
072400110412     c                   endif
072500100907     C                   MOVE      UNgMAG        V1CMAG
072600100907     C                   MOVE      UNgMGC        V1CMGC
072700100907     C                   MOVEL     UNgDES        V1CDES
072800100907     C                   Z-ADD     UNgDOC        V1CDOC
072900100907     C                   Z-ADD     UNgDDO        G08INV
073000100907     C                   Z-ADD     *ZEROS        G08DAT
073100100907     C                   MOVEL     '3'           G08ERR
073200100907     C                   CALL      'XSRDA8'
073300100907     C                   PARM                    WLBDA8
073400100907     C                   Z-ADD     G08DAT        V1CDDO
073500100907     C*
073600100907     C* DECODIFICO MAG. CORRELATO
073700100907     C     UNgMGC        IFEQ      *ZEROS                                       --- 1 -->
073800100907     C                   MOVEL     *BLANKS       V1DMGC
073900100907     C                   ELSE                                                   <-- 1 -->
074000100908     C                   MOVE      UNgMGC        WMAG
074100100907     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
074200100907     C                   MOVEL     WDMAG         V1DMGC
074300100929     C                   ENDIF
074400100908     c
074500100928     c* Se rientro da Filiale per fornitore, al 1°movimento imposto magz.destinazione 935
074600100908     c                   if        unginvr='F'and v1ccau='FISE'
074700100908     C                   MOVE      v1cMAG        WMAG
074800100908     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
074900100908     c                   if        wtmag='Y'
075000100908     c                   eval      v1cmag='935'
075100100908     c                   eval      savtmag=wtmag
075200100908     c                   endif
075300100908     c                   endif
075400100908     c
075500100908     c* Se rientro da Filiale per fornitore al 2° movimento inmposto 935 nell'ubicaz.
075600100908     c                   if        unginvr='F'and v1ccau='RIPA'
075700100908     c                   eval      v1cmgc='935'
075800100908     c                   endif
075900101013     c
076000101013     c* Se movimento Sede e invio a fornitore RIPA altrimenti SESE
076100101013     c                   if        unginvr='S'
076200101013     C                   MOVE      v1cMAG        WMAG
076300101013     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
076400101013     c                   if        wtmag='Y'
076500101013     C                   MOVEL     'RIPA'        V1CCAU
076600101013     c                   else
076700101013     C                   MOVEL     'SESE'        V1CCAU
076800101013     c                   endif
076900101013     c                   endif
077000100907
077100101022     c                   if         unginvr='I'
077200100907     c                   eval      v1cksc = ungksc
077300101022     c                   endif
077400100907     C*
077500100907     C                   EXSR      DECODI
077600100907     C     $ERR          IFEQ      '1'
077700100907     C                   MOVEL     '0'           $ERR
077800100907     C                   ENDIF
077900100907     C*
078000100907     C                   ENDSR
078100930906     C*================================================================
078200930906     C*  VIDREC - IMPOSTA CAMPI DI UNMOV DA VIDEO
078300930906     C*================================================================
078400930906     C     VIDREC        BEGSR
078500930906     C*
078600931217     C                   MOVE      V1CTIP        UNMTIP
078700931217     C                   Z-ADD     V1CCOD        UNMCOD
078800110407     C                   movel     V1Cmat        UNMmat
078900931217     C                   MOVEL     V1CCAU        UNMCAU
079000971010     C                   Z-ADD     V1CDTM        G08DAT
079100971010     C                   Z-ADD     *ZEROS        G08INV
079200971010     C                   MOVEL     '0'           G08ERR
079300971010     C                   CALL      'XSRDA8'
079400971010     C                   PARM                    WLBDA8
079500971010     C                   Z-ADD     G08INV        UNMDTM
079600930906     C                   MOVEL     V1CDES        UNMDES
079700930906     C                   Z-ADD     V1CDOC        UNMDOC
079800971010     C                   Z-ADD     V1CDDO        G08DAT
079900971010     C                   Z-ADD     *ZEROS        G08INV
080000971010     C                   MOVEL     '0'           G08ERR
080100971010     C                   CALL      'XSRDA8'
080200971010     C                   PARM                    WLBDA8
080300971010     C                   Z-ADD     G08INV        UNMDDO
080400930906     C                   Z-ADD     V1CCNT        UNMCNT
080500020321     c                   eval      unmksc = v1cksc
080600930906     C*
080700930906     C                   ENDSR
080800930806     C*================================================================
080900930806     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
081000930806     C*================================================================
081100930806     C     AZZVID        BEGSR
081200930806     C*
081300110407     C                   MOVEL     *BLANKS       V1cmat
081400110407     C                   MOVEL     *BLANKS       V1DATB
081500930906     C                   MOVEL     *BLANKS       V1CTIP
081600930906     C                   MOVEL     *BLANKS       V1DTIP
081700930906     C                   Z-ADD     *ZEROS        V1CCOD
081800930906     C                   MOVEL     *BLANKS       V1DCOD
081900020321     C                   Z-ADD     *date         V1CDTM
082000930906     C                   MOVEL     *BLANKS       V1CCAU
082100930906     C                   MOVEL     *BLANKS       V1DCAU
082200930906     C                   MOVEL     *BLANKS       V1CMAG
082300930906     C                   MOVEL     *BLANKS       V1DMAG
082400930907     C                   MOVEL     *BLANKS       V1DTMO
082500930906     C                   MOVEL     *BLANKS       V1CDES
082600930906     C                   Z-ADD     *ZEROS        V1CDOC
082700020321     C                   Z-ADD     *date         V1CDDO
082800930906     C                   Z-ADD     *ZEROS        V1CCNT
082900930906     C                   MOVEL     *BLANKS       V1CMGC
083000930906     C                   MOVEL     *BLANKS       V1DMGC
083100020321     c                   clear                   v1cksc
083200100907     c                   clear                   v1dksc
083300100907     c                   clear                   v1hreso
083400100907     c                   clear                   v1creso
083500100910     c                   setoff                                       0102
083600930806     C*
083700930806     C                   ENDSR
083800930205     C*================================================================
083900930906     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
084000930205     C*================================================================
084100930906     C     DECODI        BEGSR
084200930205     C*
084300930906     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI CAMPI
084400930906     C                   SETOFF                                       606162
084500930906     C                   SETOFF                                       636465
084600020321     c                   eval      *in67 = *off
084700930712     C*
084800930907     C* CONTROLLO IL TIPO UNITA'
084900930907     C                   MOVEL     *BLANKS       V1DTIP                         *DESCR.TIPO UNITA'
085000930907     C                   MOVE      V1CTIP        WTIP
085100930805     C                   EXSR      CHKTIP                                       *CONTROLLA TIPO UNIT
085200930907     C     $ERR          IFEQ      '1'                                          --- 1 -->
085300930721     C                   MOVEL     '1'           $ERR
085400930721     C                   MOVEL     MSG(9)        $MSG
085500930721     C                   EXSR      CARMSG
085600930907     C                   SETON                                        60
085700930907     C                   ELSE                                                   <-- 1 -->
085800930907     C                   MOVE      WTIP          V1CTIP                         *COD.
085900930907     C                   MOVEL     WDTIP         V1DTIP                         *DESCR.
086000930907     C                   ENDIF                                                  <-- 1 ---
086100930907     C*
086200930907     C* CONTROLLO IL CODICE UNITA'
086300100921     c                   clear                   wtmag
086400930907     C     $ERR          IFEQ      '0'
086500930907     C                   MOVEL     *BLANKS       V1DCOD                         *DESCR.UNITA'
086600020322     c                   move      v1ctip        unatip
086700930907     C                   Z-ADD     V1CCOD        UNACOD
086800930907     C     KEYUNA        CHAIN     UNANA01L                           99        *ANAGRAFICA UNITA
086900930907     C     *IN99         IFEQ      '1'                                          --- 1 -->
087000930907     C                   MOVEL     '1'           $ERR
087100930907     C                   MOVEL     MSG(11)       $MSG
087200930907     C                   EXSR      CARMSG
087300930907     C                   SETON                                        61
087400930907     C                   ELSE                                                   <-- 1 -->
087500100903     c                   movel     unamod        v1cmod
087600930907     C                   MOVEL     UNADES        V1DCOD                         *DESCR.
087700110401     c*  se ditta di proprietà = 923 non posso accettare mag 990
087800110401     c                   if        unasis='923' and v1cmag='990'
087900110401     C                   MOVEL     '1'           $ERR
088000110401     C                   MOVEL     MSG(27)       $MSG
088100110401     C                   EXSR      CARMSG
088200110401     c                   seton                                        64
088300110401     c                   endif
088400110401     c
088500930907     C                   ENDIF                                                  <-- 1 ---
088600930907     C                   ENDIF
088700100907     c
088800100903     C* SE INSERIMENTO METTO A VIDEO IL MAGAZZINO CORRELATO (MAG.UBIZAZ.ANAGR)
088900100907      * più il codice cliente e se unità da rendere
089000110407      * più la matricola
089100100908    1C     DSUOP1        IFEQ      'F06'
089200100907     c                   clear                   v1creso
089300100907     c                   setoff                                       01
089400100903     C                   MOVE      UNAMAG        V1CMGC
089500100908    2C     UNAMAG        IFEQ      *ZEROS                                       --- 1 -->
089600100903     C                   MOVEL     *BLANKS       V1DMGC
089700100908   x2C                   ELSE                                                   <-- 1 -->
089800100903     C                   MOVE      UNAMAG        WMAG
089900100903     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
090000100903     C                   MOVEL     WDMAG         V1DMGC
090100110407     c
090200100908    3c                   if        unaksc <> *zeros
090300100903     c                   eval      v1cksc = unaksc
090400100903     c                   exsr      chkksc
090500100908    3c                   endif
090600110407     c
090700110407     c                   movel     unamat        v1cmat
090800110407     c
090900100908    2C                   ENDIF                                                  <-- 2 ---
091000100908    1C                   ENDIF                                                  <-- 1 ---
091100100907     c
091200100908     c* anche per conferma moviemnti giornalieri lo faccio
091300100908    1C     DSUOP1        IFEQ      'F06'
091400100908    1C     DSUOP1        oreq      'F6N'
091500100907     c                   if        %subst(unalsw:15:1)='S'  and v1ccau='FISE'
091600100907     c                   eval      v1creso='SI'
091700100907     c                   seton                                        01
091800100907     c                   endif
091900100907     c
092000100903     C                   END
092100930907     C*
092200930907     C* CONTROLLO LA DATA DEL MOVIMENTO
092300930907     C     $ERR          IFEQ      '0'
092400971010     C                   Z-ADD     V1CDTM        WDAT              8 0
092500930907     C                   EXSR      CHKDAT                                       *CONTROLLA DATA
092600930907     C     $ERR          IFEQ      '1'                                          *DATA ERRATA
092700930907     C                   SETON                                        62
092800930907     C                   ELSE                                                   *DATA OK
092900971010     C                   Z-ADD     V1CDTM        G08DAT
093000971010     C                   Z-ADD     *ZEROS        G08INV
093100971010     C                   MOVEL     '0'           G08ERR
093200971010     C                   CALL      'XSRDA8'
093300971010     C                   PARM                    WLBDA8
093400971013     C                   Z-ADD     G08DAT        V1CDTM
093500971013     C                   Z-ADD     G08INV        W1CDTM            8 0
093600930907     C                   ENDIF
093700930907     C                   ENDIF
093800930907     C*
093900930907     C* CONTROLLO LA CAUSALE DEL MOVIMENTO
094000930907     C     $ERR          IFEQ      '0'
094100930907     C                   MOVEL     *BLANKS       V1DCAU                         *DESCR.CAUSALE
094200930907     C                   MOVE      V1CCAU        WCAU
094300930907     C                   EXSR      CHKCAU                                       *CONTROLLA CAUSALE
094400930907     C     $ERR          IFEQ      '1'                                          --- 1 -->
094500930907     C                   MOVEL     '1'           $ERR
094600930907     C                   MOVEL     MSG(16)       $MSG
094700930907     C                   EXSR      CARMSG
094800930907     C                   SETON                                        63
094900930907     C                   ELSE                                                   <-- 1 -->
095000930907     C                   MOVE      WCAU          V1CCAU                         *COD.
095100930907     C                   MOVEL     WDCAU         V1DCAU                         *DESCR.
095200930907     C                   ENDIF                                                  <-- 1 ---
095300930907     C                   ENDIF
095400100921     c
095500100921     c* Controllo il tipo Magazzino Correlato
095600100921    3c                   if        dsuop1='F06' and §unmtmgc<>*blanks
095700100921     c                   movea     §unmtmgc      tipi
095800100929     c* ricerco con FAG + LUE
095900100929     c                   movel     wtmag         RICtipo           2
096000100929     c                   move      §omaglue      RICtipo
096100100929     c     RICtipo       lookup    tipi                                   30
096200100921    4c                   if        not *in30
096300100921     C                   MOVEL     '1'           $ERR
096400100921     C                   MOVEL     MSG(26)       $MSG
096500100921     C                   EXSR      CARMSG
096600100921     C                   SETON                                        63
096700100921    4c                   endif
096800100921    3c                   endif
096900930212     C*
097000930907     C* CONTROLLO IL MAGAZZINO
097100100921    1C     $ERR          IFEQ      '0'
097200930907     C                   MOVE      V1CMAG        WMAG
097300930805     C                   EXSR      CHKMAG                                       *CONTROLLA MAGAZZINO
097400100921    2C     $ERR          IFEQ      '1'                                          --- 1 -->
097500930721     C                   MOVEL     MSG(8)        $MSG
097600930721     C                   EXSR      CARMSG
097700930907     C                   SETON                                        64
097800100921   x2C                   ELSE                                                   <-- 1 -->
097900930907     C                   MOVE      WMAG          V1CMAG
098000930907     C                   MOVEL     WDMAG         V1DMAG
098100100921     c* Verifico il tipo rispetto alla causale  solo in inserimento
098200100921    3c                   if        dsuop1='F06' and §unmtmag<>*blanks
098300100921     c                   movea     §unmtmag      tipi
098400100929     c* ricerco con FAG + LUE
098500100929     c                   movel     wtmag         RICtipo           2
098600100929     c                   move      §omaglue      RICtipo
098700100929     c     RICtipo       lookup    tipi                                   30
098800100921    4c                   if        not *in30
098900100921     C                   MOVEL     '1'           $ERR
099000100921     C                   MOVEL     MSG(25)       $MSG
099100100921     C                   SETON                                        64
099200100921     C                   EXSR      CARMSG
099300100921    4c                   endif
099400100921    3c                   endif
099500100921    2C                   ENDIF                                                  <-- 1 ---
099600100921    1C                   ENDIF
099700930712     C*
099800930907     C* CONTROLLO LA DATA DEL DOCUMENTO
099900930907     C     $ERR          IFEQ      '0'
100000930907     C     V1CDDO        IFNE      *ZEROS                                       --- 1 -->
100100971010     C                   Z-ADD     V1CDDO        WDAT
100200930805     C                   EXSR      CHKDAT                                       *CONTROLLA DATA
100300930907     C     $ERR          IFEQ      '1'                                          --- 2 -->*DATA ERRAT
100400931015     C                   SETON                                        65
100500930907     C                   ELSE                                                   <-- 2 -->*DATA OK
100600971010     C                   Z-ADD     V1CDDO        G08DAT
100700971010     C                   Z-ADD     *ZEROS        G08INV
100800971010     C                   MOVEL     '0'           G08ERR
100900971010     C                   CALL      'XSRDA8'
101000971010     C                   PARM                    WLBDA8
101100971013     C                   Z-ADD     G08DAT        V1CDDO
101200971013     C                   Z-ADD     G08INV        W1CDDO            8 0
101300930907     C                   ENDIF                                                  <-- 2 ---
101400930907     C                   ENDIF                                                  <-- 1 ---
101500930907     C                   ENDIF
101600020321
101700020321      * se inserimento e causale 'SECL' obbligatorio il cliente
101800100921     c***                          (v1ccau ='FICL' or v1ccau='SECL') and
101900100921     c                   if        (dsuop1 = 'F06' or dsuop1='F6N')   and
102000100921     c                             §unmksc='S' and
102100020403     c                             v1cksc = *zeros and v1dksc = *blanks
102200020321     c                   eval      $err = *on
102300020321     c                   eval      $msg = msg(7)
102400020321     c                   exsr      carmsg
102500020321     c                   eval      *in67 = *on
102600020321     c                   endif
102700020327
102800020327      * controllo il cliente
102900020327     c                   if        $err = *off
103000020327     c                   if        v1cksc <> *zeros or v1dksc <> *blanks
103100020327     c                   exsr      chkksc
103200020327     c                   if        $err = *on
103300020327     c                   eval      $msg = msg(7)
103400020327     c                   exsr      carmsg
103500020327     c                   eval      *in67 = *on
103600020327     c                   endif
103700020327     c                   endif
103800020327     c                   if        v1cksc = *zeros and v1cmag = '951'
103900020327     c                   eval      $err = *on
104000020327     c                   eval      $msg = msg(7)
104100020327     c                   exsr      carmsg
104200020327     c                   eval      *in67 = *on
104300020327     c                   endif
104400020327     c                   endif
104500020321
104600930715     C*
104700930715     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
104800930715     C* CURSORE SUL CAMPO DEL CONTROL
104900930721     C     $ERR          IFEQ      '1'
105000930805     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
105100930715     C                   ELSE
105200930805     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
105300930715     C                   END
105400930205     C*
105500930205     C                   ENDSR
105600020321      *================================================================
105700020321      *  CHKTIP - CONTROLLA IL TIPO DI UNITA
105800020321      *================================================================
105900930712     C     CHKTIP        BEGSR
106000020321
106100020321      * CONTROLLO SE E' STATO INSERITO '?'
106200020321     C     '?'           SCAN      WTIP                                   99
106300020321
106400020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
106500020321     c                   if        *in99                                        --- 1 -->
106600020321     c                   clear                   TIBS02DS
106700020321     c                   movel     'R'           t02mod
106800020321     c                   movel     knsif         t02sif
106900020321     c                   movel     'UNT'         t02cod
107000020321     c                   call      'TIBS02R'
107100020321     c                   parm                    KPJBA
107200020321     c                   parm                    TIBS02DS
107300020321     c                   movel     T02ke1        wtip              2
107400020321     c                   movel     t02uni        wdtip            30
107500020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
107600020321     c                   endif                                                  <-- 1 ---
107700020321
107800020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
107900020321     c                   clear                   TIBS02DS
108000020321     c                   movel     'C'           T02mod
108100020321     c                   movel     knsif         t02sif
108200020321     c                   movel     'UNT'         t02cod
108300020321     c                   movel(p)  wtip          T02ke1
108400020321     c                   call      'TIBS02R'
108500020321     c                   parm                    KPJBA
108600020321     c                   parm                    TIBS02DS
108700020321     c                   if        t02err <> *blanks
108800020321     c                   eval      $err = '1'
108900020321     c                   clear                   wdtip
109000110105     c                   clear                   dunt
109100020321     c                   else
109200020321     c                   movel     t02uni        wdtip
109300110105     c                   movel     t02uni        dunt
109400020321     c                   endif
109500020321
109600930712     C                   ENDSR
109700020321      *================================================================
109800020321      *  CHKCAU - CONTROLLA LA CAUSALE MOVIMENTO
109900020321      *================================================================
110000930712     C     CHKCAU        BEGSR
110100020321
110200020321      * CONTROLLO SE E' STATO INSERITO '?'
110300020321     C     '?'           SCAN      WCAU                                   99
110400020321
110500020321      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
110600020321     c                   if        *in99                                        --- 1 -->
110700020321     c                   clear                   TIBS02DS
110800020321     c                   movel     'R'           t02mod
110900020321     c                   movel     knsif         t02sif
111000020321     c                   movel     'UNM'         t02cod
111100020321     c                   call      'TIBS02R'
111200020321     c                   parm                    KPJBA
111300020321     c                   parm                    TIBS02DS
111400020321     c                   movel     T02ke1        wcau              4
111500020321     c                   movel     t02uni        wdcau            30
111600100921     c                   movel     t02uni        dunm
111700020321     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
111800020321     c                   endif                                                  <-- 1 ---
111900020321
112000020321      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
112100020321     c                   clear                   TIBS02DS
112200020321     c                   movel     'C'           T02mod
112300020321     c                   movel     knsif         t02sif
112400020321     c                   movel     'UNM'         t02cod
112500020321     c                   movel(p)  wcau          T02ke1
112600020321     c                   call      'TIBS02R'
112700020321     c                   parm                    KPJBA
112800020321     c                   parm                    TIBS02DS
112900020321     c                   if        t02err <> *blanks
113000020321     c                   eval      $err = '1'
113100020321     c                   clear                   wdcau
113200100921     c                   clear                   dunm
113300020321     c                   else
113400020321     c                   movel     t02uni        wdcau
113500100921     c                   movel     t02uni        dunm
113600020321     c                   endif
113700020321
113800930712     C                   ENDSR
113900930712     C*================================================================
114000930907     C*  CHKMAG - CONTROLLA IL MAGAZZINO
114100930712     C*================================================================
114200930712     C     CHKMAG        BEGSR
114300100929     c                   clear                   tnsdmagds
114400100929     c                   eval      §magmag=wmag
114500100929     c                   eval      §imagtorg='E  '
114600100929     c                   call      'TNSDMAGR'
114700100929     c                   parm                    kpjba
114800100929     c                   parm                    tnsdmagds
114900100929     c
115000100929     c* Eseguita Ricerca
115100100929    1c                   if        §omagerr='R'
115200100929     c                   eval      wmag=§magmag
115300100929     c                   eval      wdmag=§omagdes
115400100929     c                   eval      wtmag=§omagfag
115500100929     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
115600100929   x1c                   else
115700100929     c* Errore nel magazzino
115800100929    2c                   if        §omagerr='E'
115900100929     C                   MOVEL     '1'           $ERR
116000100929     c                   clear                   wtmag
116100100929     c                   eval      wdmag=*all'?'
116200100929     c* se tutto ok prendo decodifica e flag
116300100929   x2c                   else
116400100929     c                   eval      wdmag=§omagdes
116500100929     c                   eval      wtmag=§omagfag
116600100929    2c                   endif
116700100929    1c                   endif
116800930712     C*
116900930712     C                   ENDSR
117000930712     C*================================================================
117100930712     C* CHKDAT - CONTROLLA LA DATA
117200930712     C*================================================================
117300930712     C     CHKDAT        BEGSR
117400930712     C*
117500971010     C                   Z-ADD     WDAT          G08DAT
117600971010     C                   Z-ADD     *ZEROS        G08INV
117700971010     C                   MOVEL     '0'           G08ERR
117800971010     C                   CALL      'XSRDA8'                                     *CONTROLLA DATA
117900971010     C                   PARM                    WLBDA8
118000971013     C                   Z-ADD     G08DAT        WDAT
118100971010     C     G08ERR        IFNE      '0'
118200930721     C                   MOVEL     '1'           $ERR
118300930721     C                   MOVEL     MSG(10)       $MSG
118400930721     C                   EXSR      CARMSG
118500930805     C                   ENDIF
118600930712     C*
118700930712     C                   ENDSR
118800020321      *================================================================
118900020321      * CHKKSC - CONTROLLA IL CLIENTE
119000020321      *================================================================
119100020321     c     chkksc        begsr
119200020321
119300020321      * se non inserito il codice ma inserita una parte della ragione sociale
119400020321      * richiamo il programma della ricerca alfabetica
119500020321     c                   if        v1cksc = *zeros and v1dksc <> *blanks
119600020321     c                   exsr      ricalf
119700020321     c                   endif
119800020321
119900020321      * se inserito il codice controllo che esista sul piano dei conti
120000020321     c                   if        v1cksc <> *zeros
120100020327     c                   clear                   TIBS69DS
120200020327     c                   z-add     v1cksc        I69kac
120300020327     c                   call      'TIBS69R'
120400020327     c                   parm                    tibs69DS
120500020327     c                   parm                    ds_cnaco
120600020327     c                   parm                    ds_cnind
120700020327     c                   parm                    ds_cnclp
120800020327     c                   parm                    ds_fncls
120900020327
121000020327     c                   if        o69err <> '1'
121100020327     c                   movel     acorag        v1dksc
121200020327     c                   else
121300020321     c                   eval      $err = *on
121400020321     c                   endif
121500020321     c                   endif
121600020321
121700020321     c                   endsr
121800020321      *================================================================
121900020321      *  RICALF - RICERCA ALFABETICA
122000020321      *================================================================
122100020321     c     ricalf        begsr
122200020321
122300020321     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
122400080922     C*******            Z-ADD     9             PAXsta                         9=Escludo Annullati
122500080922     c* visulizzare anche i bloccati
122600080922     C                   Z-ADD     0             PAXsta                         9=Escludo Annullati
122700020709     C                   Z-ADD     DutKci        PAXccc                         Capoconto Clienti
122800020321     c                   movel(P)  v1dksc        PAXdmt
122900020321     C                   z-add     1             PAXnum
123000020321
123100020321     C                   CALL      'XALFA3BR'
123200020321     C                   PARM                    PAXDUT
123300020321     C                   PARM                    CODUT
123400020321     C                   PARM                    PAXDMT
123500020321     C                   PARM                    PAXCCC
123600020321     C                   PARM                    PAXSTA
123700020321     C                   PARM      *blanks       PAXFLR
123800020321     C                   PARM      *blanks       PAXDIT
123900020321     C                   PARM                    PAXNUM            2 0
124000020321     C                   PARM                    PAXKCM           80
124100020321     C                   PARM                    PAXKSM          140
124200020321     C                   PARM                    PAXKDM           60
124300020321
124400020321      * NON TROVATO O PREMUTO CMD12
124500020321    2C     paxsta        IFGT      -1
124600020321     C                   MOVEL     PAXKSM        v1cksc
124700020321    2C                   ENDIF
124800020321
124900020321     c                   endsr
125000100803     C*================================================================
125100100803     C* emetto videata accertamento responsabilità danno
125200100803     C*================================================================
125300100803     C     CTRRESPO      BEGSR
125400100804     c* Solo se la data movimento > di agosto
125500100804     c                   if        unmdtm>20100801
125600100803     c                   clear                   w2ctcm
125700100803     c                   clear                   w2dtcm
125800100803     c                   clear                   w2ciafil
125900100805     c                   clear                   w2diafil
126000100803     c                   clear                   w2ciamag
126100100803     c                   clear                   w2ccapre
126200100803     c                   clear                   w2cfor
126300100804     c                   clear                   w2dfor1
126400100804     c                   clear                   w2dfor2
126500110401     c                   clear                   w2hdfor
126600100804     c                   clear                   w2cauco
126700100803     c                   clear                   w2cimpa
126800100805     c                   clear                   w2cemf
126900100803     c                   clear                   w2cdtv
127000100805     c                   clear                   w2cdes
127100100805     c                   clear                   w2ccms
127200100803     c* cerco record
127300100803     c     kunr          chain     unmor01l
127400100803     c                   if        %found(unmor01l)
127500100803     c                   eval      w2ctcm=unrtcm
127600100805     c                   eval      w2ccms=unrcms
127700100803     c                   eval      w2ciafil=unriafil
127800100803     c                   eval      w2ciamag=unriamag
127900100803     c                   eval      w2ccapre=unrcapre
128000100804     c                   eval      w2cauco =unrtipp
128100100804     c                   if        unrpdr>0
128200100804     c                   movel     unrpdr        w2cfor
128300100804     c                   endif
128400100803     c                   eval      w2cimpa =unrimpa
128500100805     c                   eval      w2cemf  =unremf
128600100805     c                   eval      w2cdes  =unrdes
128700100805     c
128800100804     c                   if        unrdtv>0
128900100804     c                   movel     unrdtv        dataiso
129000100804     c     *eur          movel     dataiso       w2cdtv
129100100804     c                   endif
129200100805
129300100804     c                   exsr      decwin
129400100804     c                   setoff                                       28
129500110610     C                   SETOFF                                       707172
129600110610     C                   SETOFF                                       737475
129700110610     C                   SETOFF                                       76
129800100804     c                   eval      $err='0'
129900100803     c                   endif
130000100803     c
130100100804     c     emfor2        TAG
130200100803     c                   exfmt     sdu5dw2
130300100804     c                   setoff                                       28
130400100804     c                   eval      $err='0'
130500100804     c
130600100803     c* controlli
130700100803     c                   exsr      decWIN
130800100804     c
130900100804     c                   if        $err='1'
131000100804     c                   goto      emfor2
131100100804     c                   endif
131200100804     c* in inserimento valido solo F6
131300100804     c  n91              if        not %found(unmor01l)  and *inkl
131400100804     c                   eval      $err = '1'
131500100804     C                   MOVEL     MSG(15)       w2cmsg
131600100804     c                   seton                                        2870
131700100804     c                   goto      emfor2
131800100804     c                   endif
131900100804     c
132000100804     c* Se non è stato premuto F6 o F12 riemetto videata
132100100804     c* In visualizzazione esco sempre
132200100804     c  n91              if        not *inkl and not  *inkf
132300100804     c                   goto      emfor2
132400100804     c                   endif
132500100804     c* Conferma
132600100804     c                   if        *inkf
132700100804     c                   exsr      AggioMOR
132800100804     c                   endif
132900100804     c
133000100804     c                   endif
133100100803     c
133200100803     c                   endsr
133300100803     C*================================================================
133400100803     C*  DECWIN - CONTROLLA CAMPI della window
133500100803     C*================================================================
133600100803     C     DECwin        BEGSR
133700100803     C*
133800100803     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI CAMPI
133900100803     C                   SETOFF                                       707172
134000100805     C                   SETOFF                                       737475
134100110610     C                   SETOFF                                       76
134200100803     c
134300100805     c* Se immesso TIPO COMUNICAZIONE controllo
134400100803      * CONTROLLO SE E' STATO INSERITO '?'
134500100803     c                   clear                   w2dtcm
134600100803     C     '?'           SCAN      w2ctcm                                 99
134700100803
134800100803      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
134900100803     c                   if        *in99                                        --- 1 -->
135000100803     c                   clear                   TIBS02DS
135100100803     c                   movel     'R'           t02mod
135200100803     c                   movel     knsif         t02sif
135300100803     c                   movel     'UNC'         t02cod
135400100803     c                   call      'TIBS02R'
135500100803     c                   parm                    KPJBA
135600100803     c                   parm                    TIBS02DS
135700100803     c                   movel     T02ke1        w2ctcm
135800100803     c                   movel     t02uni        w2dtcm
135900100804     c                   eval      $err='1'
136000100804     c                   seton                                        70
136100100803     c                   endif                                                  <-- 1 ---
136200100803
136300100803      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
136400100804     c                   if        $err='0' and w2ctcm<>*blanks
136500100803     c                   clear                   TIBS02DS
136600100803     c                   movel     'C'           T02mod
136700100803     c                   movel     knsif         t02sif
136800100803     c                   movel     'UNC'         t02cod
136900100803     c                   movel(p)  w2ctcm        T02ke1
137000100803     c                   call      'TIBS02R'
137100100803     c                   parm                    KPJBA
137200100803     c                   parm                    TIBS02DS
137300100803     c                   if        t02err <> *blanks
137400100803     c                   eval      $err = '1'
137500100803     C                   MOVEL     MSG(13)       w2cmsg
137600100804     c                   seton                                        2870
137700100803     c                   clear                   w2dtcm
137800100803     c                   else
137900100803     c                   movel     t02uni        w2dtcm
138000100803     c                   endif
138100100803     c                   endif
138200100805     c
138300100805     c* Se immesso RESPONSABILE ADDEBITO DA FILIALE controllo
138400100805      * CONTROLLO SE E' STATO INSERITO '?'
138500100805     c                   if        $err='0'
138600100805     c                   clear                   w2diafil
138700100805     C     '?'           SCAN      w2ciafil                               99
138800100805
138900100805      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
139000100805     c                   if        *in99                                        --- 1 -->
139100100805     c                   clear                   TIBS02DS
139200100805     c                   movel     'R'           t02mod
139300100805     c                   movel     knsif         t02sif
139400100805     c                   movel     'UND'         t02cod
139500100805     c                   call      'TIBS02R'
139600100805     c                   parm                    KPJBA
139700100805     c                   parm                    TIBS02DS
139800100805     c                   movel     T02ke1        w2ciafil
139900100805     c                   movel     t02uni        w2diafil
140000100805     c                   eval      $err='1'
140100100805     c                   seton                                        75
140200100805     c                   endif                                                  <-- 1 ---
140300100805
140400100805      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
140500100805     c                   if        $err='0' and w2ciafil>*blanks
140600100805     c                   clear                   TIBS02DS
140700100805     c                   movel     'C'           T02mod
140800100805     c                   movel     knsif         t02sif
140900100805     c                   movel     'UND'         t02cod
141000100805     c                   movel(p)  w2ciafil      T02ke1
141100100805     c                   call      'TIBS02R'
141200100805     c                   parm                    KPJBA
141300100805     c                   parm                    TIBS02DS
141400100805     c                   if        t02err <> *blanks
141500100805     c                   eval      $err = '1'
141600100805     C                   MOVEL     MSG(24)       w2cmsg
141700100805     c                   seton                                        2875
141800100805     c                   clear                   w2diafil
141900100805     c                   else
142000100805     c                   movel     t02uni        w2diafil
142100100805     c                   endif
142200100805     c                   endif
142300100805     c                   endif
142400100803
142500100804     c* Controllo codice fornitore
142600100805     c                   if        $err='0'
142700100804     c                   clear                   w2dfor1
142800100804     c                   clear                   w2dfor2
142900110401     c                   clear                   w2hdfor
143000100804     c                   if        $err='0' and w2cfor<>*blanks and
143100100804     c                             w2cfor<>*zeros and w2cauco=' '
143200100804     c                   eval      $err = '1'
143300100804     C                   MOVEL     MSG(21)       w2cmsg
143400100804     c                   seton                                        2874
143500100804     c                   endif
143600100804     c                   if        $err='0' and w2cauco<>' ' and
143700100804     c                             (w2cfor=*zeros or w2cfor=*blanks)
143800100804     c                   eval      $err = '1'
143900100804     C                   MOVEL     MSG(23)       w2cmsg
144000100804     c                   seton                                        2871
144100100804     c                   endif
144200101105     c* Se w2cauco='L'  --> logistica non faccio alcun controllo sul codice
144300101105     c                   if        w2cauco<>'L'
144400100804     c
144500100804     C     '?'           SCAN      w2cfor                                 99
144600100804     c                   if        $err='0' and *in99
144700100804     C                   clear                   fnlv24ds
144800100804     C                   eval      d24fil   = kmgc
144900100804     C                   eval      d24tip   = w2cauco
145000100804     C                   eval      d24flg   = 'R'
145100100804     C                   movel     fnlv24ds      kpjbu
145200100804     C                   CALL      'FNLV24R'
145300100804     C                   PARM                    KPJBA
145400100804      *
145500100804      * valorizzo i campi a video
145600100804     C                   movel     kpjbu         fnlv24ds
145700100804     C                   if        d24pdr   <> *zeros
145800100804     C                   move      d24pdr        w2cfor
145900100804     C                   movel     d24rsc        w2dfor1
146000110401     C                   movel     d24rsc        w2hdfor
146100100805     c                   eval      w2dfor2=%subst(d24rsc:13:10)
146200100804     C                   ELSE
146300100804     C                   CLEAR                   w2cfor
146400100804     C                   endif
146500100804     c                   eval      $err='1'
146600100804     c                   seton                                        71
146700100804     C                   endif
146800100804     c* controllo
146900100804    1c                   if        $err='0' and w2cfor>*zeros
147000100804     C                   TESTN                   w2cfor               30
147100100804     C                   MOVE      w2cfor        W001A             1
147200100804    2C     *IN30         IFEQ      *OFF
147300100804     C     W001A         ORLT      '0'
147400100804     c                   eval      $err = '1'
147500100804     C                   MOVEL     MSG(22)       w2cmsg
147600100804     c                   seton                                        2871
147700100804    2c                   endif
147800100804    1c                   endif
147900100804     c
148000100804    1c                   if        $err='0' and w2cfor>*zeros
148100100804     C                   move      w2cfor        kpdpdr
148200100804     C                   move      w2cauco       kpdtip
148300100804     C     kfapd         chain     fiapd01l
148400100804    2C                   IF        not  %found(fiapd01l) or apdatb<>*blanks
148500100804     c                   eval      $err = '1'
148600100804     C                   MOVEL     MSG(22)       w2cmsg
148700100804     c                   seton                                        2871
148800100804     c                   else
148900100804     C                   movel     apdrsc        w2dfor1
149000110401     C                   movel     apdrsc        w2hdfor
149100100805     c                   eval      w2dfor2=%subst(apdrsc:13:10)
149200100804    2c                   endif
149300100804    1c                   endif
149400100805    1c                   endif
149500101105     c
149600101105    1c                   endif
149700100804     c
149800100803     C* CONTROLLO LA DATA convalida
149900100804     c                   clear                   invdtv
150000100804     c                   if        $err='0' and w2cdtv>0
150100100803     C                   Z-ADD     w2cdtv        G08DAT
150200100803     C                   Z-ADD     *ZEROS        G08INV
150300100803     C                   MOVEL     '0'           G08ERR
150400100803     C                   CALL      'XSRDA8'
150500100803     C                   PARM                    WLBDA8
150600100803    2C     G08ERR        IFNE      '0'
150700100803     C                   MOVEL     '1'           $ERR
150800100803     C                   MOVEL     MSG(10)       w2cmsg
150900100804     c                   seton                                        2873
151000100803   x2c                   else
151100100803     C                   Z-ADD     G08DAT        w2cdtv
151200100803     C                   Z-ADD     G08INV        invDTv            8 0
151300100803    2C                   ENDIF
151400100803    1c                   ENDIF
151500100803     c
151600100803     c* Se immessa data immettere anche convalida emissione fattura  e viceversa
151700100805     c                   if        $err='0' and w2cdtv>0 and  w2cemf=' '
151800100803     C                   MOVEL     '1'           $ERR
151900100803     C                   MOVEL     MSG(12)       w2cmsg
152000100804     c                   seton                                        2872
152100100803     c                   endif
152200100805     c                   if        $err='0' and w2cdtv=0 and  w2cemf<>' '
152300100803     C                   MOVEL     '1'           $ERR
152400100804     c                   eval      w2cdtv=dataogGMA
152500100803     c                   endif
152600110610     c* Se c'e'
152700110610     c                   if        $err='0' and w2ciafil='A ' and
152800110610     c                             w2cauco='C'  and w2cfor>*zeros
152900110610     c                             and w2ccapre<>'S'
153000110610     c                   eval      w2ccapre='S'
153100110610     c                   seton                                        76
153200110610     C                   MOVEL     '1'           $ERR
153300110610     c                   endif
153400100803     c
153500110105     c* Se immessa "S" maga sede o c'e' la "S" da tabella --> propongo importo da UNT
153600110105     c                   if        w2cimpa=0
153700110105     c                   if        w2ciamag='S' or d§untoimpd='S'
153800110105     c                   eval      w2cimpa=d§untimpd
153900110105     c                   if        w2cimpa>0
154000110105     C                   MOVEL     '1'           $ERR
154100110105     c                   endif
154200110105     c                   endif
154300110105     c                   endif
154400100803     C*
154500100803     c                   endsr
154600100804     C*================================================================
154700100804     C*  AggioMOR - Aggiorno file accertamento responsabilità
154800100804     C*================================================================
154900100804     C     AggioMOR      BEGSR
155000100804     c                   eval      unrtcm=w2ctcm
155100100805     c                   eval      unrcms=w2ccms
155200100805     c                   eval      unrdes=w2cdes
155300100804     c                   eval      unriafil=w2ciafil
155400100804     c                   eval      unriamag=w2ciamag
155500100804     c                   eval      unrcapre=w2ccapre
155600100804     c                   if        w2cfor>*zeros
155700100804     c                   movel     w2cfor        unrpdr
155800110401     c                   movel     w2hdfor       unrrscf
155900101105     c                   eval      unrtipp=w2cauco
156000101105     c
156100101105     c* Logistica--> scrivo anche nel cod fornitore quanto immesso a video NON controllato
156200101105     c                   if        w2cauco='L'
156300101105     c                   movel     w2cfor        unrfor
156400101105     c                   movel     '201'         unrcsf
156500110401     c                   clear                   unrrscf
156600101105     c                   else
156700100804     c                   eval      unrfor  =apdksc
156800100804     c                   eval      unrcsf  =apdcsf
156900101105     c                   endif
157000100804     c                   else
157100100804     c                   clear                   unrpdr
157200100804     c                   clear                   unrfor
157300100804     c                   clear                   unrcsf
157400100804     c                   clear                   unrtipp
157500110401     c                   clear                   unrrscf
157600100804     c                   endif
157700100804     c                   eval      unrimpa =w2cimpa
157800100805     c                   eval      unremf  =w2cemf
157900100804     c                   eval      unrdtv  =invdtv
158000100804     c                   if        %found(unmor01l)
158100100804     c                   update    unmor000
158200100804     c                   else
158300100804     c                   eval      unrtip=unmtip
158400100804     c                   eval      unrcod=unmcod
158500100804     c                   eval      unrdtm=unmdtm
158600100804     c                   eval      unrmgc=kmgc
158700100804     c                   write     unmor000
158800100804     c                   endif
158900100804     c
159000100804     c                   endsr
159100101028     C*=================================================================================
159200101027     C* cerca il movimento di UNGIO con la matricola di reso per aggiornare flag di reso
159300101028     C*=================================================================================
159400101027     C     AggioUNGIO    BEGSR
159500101027     c
159600101027      /free
159700101028         if savksc<= *zeros   ;
159800101027         wrkgetlista='SELECT rrn(UNGIO00F) from  +
159900101027                      UNGIO00F WHERE ungtir=' + %editc(unatip:'X')
160000101027            +' and ungmatr=''' + unamat + ''' and ungmag=' + %editc(unamag:'X')+
160100101027             ' and ungreso='' '''    ;
160200101028         else  ;
160300101028         wrkgetlista='SELECT rrn(UNGIO00F) from  +
160400101028                      UNGIO00F WHERE ungtir=' + %editc(unatip:'X')
160500101028            +' and ungmatr=''' + unamat + ''' and ungksc =' + savksc  +
160600101028             ' and ungreso='' '''    ;
160700101028         endif   ;
160800101028
160900101027         WFLAG='S'  ;
161000101027         exsr PREPA   ;
161100101027
161200101027
161300101027         ENDSR        ;
161400101027
161500101028        // ================================================================
161600101028        //  Prepara SQL
161700101028        //  ================================================================
161800101027         BEGSR  PREPA   ;
161900101027         exec sql   PREPARE s1 FROM :wrkgetlista   ;
162000101027
162100101027         exec sql   DECLARE a1 cursor FOR  s1   ;
162200101027
162300101027         exec sql OPEN a1   ;
162400101027
162500101027         clear sqlcod   ;
162600101027
162700101027
162800101027         clear savnrr  ;
162900101027         exec sql FETCH NEXT  FROM a1 INTO: gionrr  ;
163000101027
163100101027         select ;
163200101027         when sqlcod=100 ;
163300101027
163400101027         when sqlcod<0   ;
163500101027         *inh1=*on   ;
163600101027
163700101027         other   ;
163800101027         // Memorizzo il numero relativo record
163900101027         if  wflag='S'   ;
164000101027         chain gionrr ungiofis  ;
164100101027         F_ungreso='S'   ;
164200101027         update ungiofis  ;
164300101027         else  ;
164400101027         chain gionrr ungiofis  ;
164500101027         F_ungreso=' '   ;
164600101027         update ungiofis  ;
164700101027         endif  ;
164800101027
164900101027         endsl  ;
165000101027
165100101027
165200101027         exec sql CLOSE a1   ;
165300101027         ENDSR   ;
165400101027
165500101027      /end-free
165600101027     C*================================================================
165700101027     C* cerca il movimento di UNGIO con la matricola di reso per toglòeire  flag di reso
165800101027     C*================================================================
165900101027     C     SflaggaUNGIO  BEGSR
166000101027     c
166100101027      /free
166200101027         wrkgetlista='SELECT rrn(UNGIO00F)   from  +
166300101027                      UNGIO00F WHERE ungtir=' + %editc(unatip:'X')
166400101027            +' and ungmatr=''' + unamat + ''' and ungmag=' + %editc(unamag:'X')+
166500101027             ' and ungreso=''S''  order by ungdtm  desc' ;
166600101027
166700101027         WFLAG=' '  ;
166800101027         exsr PREPA   ;
166900101027
167000101027      /end-free
167100101027     c                   ENDSR
167200000000     C*================================================================
167300930713     C*  *INZSR - ROUTINE INIZIALE
167400000000     C*================================================================
167500930713     C     *INZSR        BEGSR
167600000000     C*
167700000000     C* RICEVIMENTO PARAMETRI
167800000000     C     *ENTRY        PLIST
167900940105     C                   PARM                    KPJBA
168000100907     C                   PARM                    dsungio
168100100908     C                   MOVEL     KPJBU         DSUNIT
168200020321
168300020321      * CHIAVE LETTURA UNMOV01L
168400931015     C     KEYUNM        KLIST
168500020322     C                   KFLD                    UNMTIP
168600930712     C                   KFLD                    UNMCOD
168700930712     C                   KFLD                    UNMDTM
168800930712     C                   KFLD                    UNMPRG
168900931217     C                   KFLD                    UNMTMO
169000020321      * CHIAVE LETTURA UNMOV01L
169100941216     C     KEYUN2        KLIST
169200020322     C                   KFLD                    UNMTIP
169300941216     C                   KFLD                    UNMCOD
169400941216     C                   KFLD                    UNMDTM
169500020321
169600020321      * CHIAVE LETTURA UNANA01L
169700931015     C     KEYUNA        KLIST
169800020322     c                   kfld                    unatip
169900931015     C                   KFLD                    UNACOD
170000100907     C     Kuna2         KLIST
170100110412     c                   kfld                    ungtip
170200110412     C                   KFLD                    v1cmat
170300020321
170400100803     C     kUNR          KLIST
170500100803     C                   KFLD                    UNMTIP
170600100803     C                   KFLD                    UNMCOD
170700100803     C                   KFLD                    Kmgc
170800100803     C                   KFLD                    UNMDTM
170900100804     C     Kfapd         klist
171000100804     C                   kfld                    kpdtip
171100100804     C                   kfld                    kpdpdr
171200940105     C*
171300100804     c     *iso          movel     datasys       dataogAMG         8 0
171400100804     c     *eur          movel     datasys       dataogGMA         8 0
171500100804     C*
171600940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DELL'ANAGRAF. UNITA'
171700940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
171800940105     C     DSUPRO        IFEQ      'A'
171900940105     C                   MOVEL     FUN(9)        FUNRIG                         *GEST.MOVIMENTI
172000940105     C                   MOVEL     'N'           FUNASG
172100940105     C                   MOVEL     'N'           FUNASI
172200940105     C                   MOVEL     'N'           FUNAFG
172300940105     C                   MOVEL     'N'           FUNAFI
172400940105     C                   MOVEL     FUNRIG        FUN(9)
172500940105     C                   END
172600930714     C*
172700930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
172800930803     C                   EXSR      INZSTD
172900930714     C*
173000930806     C* IMPOSTO CAMPI TESTATA VIDEO
173100931015     C     DSUOP1        IFEQ      'F06'
173200100907     C     DSUOP1        oreq      'F6N'
173300931015     C                   MOVEL     '  INSERI'    VTDFUN
173400931015     C                   MOVE      'MENTO   '    VTDFUN
173500020321     c                   eval      *in66 = *on
173600931015     C                   END
173700931015     C     DSUOP1        IFEQ      'O02'
173800931015     C                   MOVEL     '    MODI'    VTDFUN
173900931015     C                   MOVE      'FICA    '    VTDFUN
174000020321     c                   eval      *in66 = *off
174100931015     C                   END
174200931015     C     DSUOP1        IFEQ      'O04'
174300931015     C                   MOVEL     ' CANCELL'    VTDFUN
174400931015     C                   MOVE      'AZIONE  '    VTDFUN
174500020321     c                   eval      *in66 = *off
174600931015     C                   END
174700931015     C     DSUOP1        IFEQ      'O05'
174800931015     C                   MOVEL     'VISUALIZ'    VTDFUN
174900931015     C                   MOVE      'ZAZIONE '    VTDFUN
175000020321     c                   eval      *in66 = *off
175100931015     C                   END
175200931015     C     DSUOP1        IFEQ      'O07'
175300931015     C                   MOVEL     '   RIPRI'    VTDFUN
175400931015     C                   MOVE      'STINO   '    VTDFUN
175500020321     c                   eval      *in66 = *off
175600931015     C                   END
175700930713     C*
175800931015     C* SE VISUALIZZAZIONE TOLGO FUNZIONE AGGIORNAMENTO
175900931015     C     DSUOP1        IFEQ      'O05'
176000931015     C                   MOVEL     FUN(6)        FUNRIG                         *AGGIORNAMENTO
176100931015     C                   MOVEL     'N'           FUNASG
176200931015     C                   MOVEL     'N'           FUNASI
176300931015     C                   MOVEL     'N'           FUNAFG
176400931015     C                   MOVEL     'N'           FUNAFG
176500931015     C                   MOVEL     FUNRIG        FUN(6)
176600931015     C                   END
176700931015     C*
176800931015     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
176900931015     C* 0=NON PROTETTO, 1=PROTETTO
177000931015     C*   90 - CAMPI CHIAVE
177100931015     C*   91 - ALTRI CAMPI
177200931015     C* --> UTENTE ENTRATO IN GESTIONE
177300940105     C     DSUOP0        IFEQ      ' 2'                                         --- 1 -->
177400931015     C     DSUOP1        IFEQ      'F06'                                        *INS.
177500100908     C     DSUOP1        oreq      'F6N'                                        *INS.
177600931015     C                   SETOFF                                       9091
177700931015     C                   END
177800931015     C     DSUOP1        IFEQ      'O02'                                        *MOD.
177900931015     C                   SETON                                        90
178000020709     c                   If        DutLpo = 'S'
178100931015     C                   SETOFF                                       91
178200931015     C                   ELSE                                                   *FIL.
178300931015     C                   SETON                                        91
178400931015     C                   END
178500931015     C                   END
178600931015     C     DSUOP1        IFEQ      'O05'                                        *VIS+CANC+RIPR
178700931015     C     DSUOP1        OREQ      'O04'
178800931015     C     DSUOP1        OREQ      'O07'
178900931015     C                   SETON                                        9091
179000931015     C                   END
179100931015     C                   END                                                    <-- 1 ---
179200940105     C     DSUOP0        IFEQ      ' 5'                                         *INTERROGAZ.
179300931015     C                   SETON                                        9091
179400931015     C                   END
179500931015     C*
179600931015     C* SE SONO IN MOD. O VIS. O CANC. O RIPR. LEGGO IL RECORD IN INPUT
179700931015     C     DSUOP1        IFEQ      'O02'                                        --- 1 -->
179800931015     C     DSUOP1        OREQ      'O04'
179900931015     C     DSUOP1        OREQ      'O05'
180000931015     C     DSUOP1        OREQ      'O07'
180100931015     C* SE IL REC NON ESISTE RIMANDO ERRORE AL PGM CHIAMANTE
180200020322     C                   Z-ADD     DSUTIP        UNMTIP
180300931015     C                   Z-ADD     DSUCOD        UNMCOD
180400931015     C                   Z-ADD     DSUDTM        UNMDTM
180500931015     C                   Z-ADD     DSUPRG        UNMPRG
180600931217     C                   MOVEL     DSUTMO        UNMTMO
180700931015     C     KEYUNM        CHAIN     UNMOV01L                           99
180800931015     C     *IN99         IFEQ      '1'
180900931015     C                   MOVEL     '1'           $ERR
181000931015     C                   MOVEL     '1'           $FINE
181100931015     C                   MOVEL     MSG(14)       DSUMSG
181200931015     C                   ELSE
181300931015     C                   EXSR      RECVID                                       *RECORD ===> VIDEO
181400931015     C                   EXSR      DECODI                                       *DECODIFICA CAMPI VI
181500931015     C                   EXSR      CARV1                                        *CARICA TESTATA VIDE
181600931015     C                   END
181700931210     C                   END                                                    <-- 1 ---
181800100908     c
181900931015     C* SE INSERIMENTO AZZERO TUTTI I CAMPI DEL VIDEO
182000100908    1C     DSUOP1        IFEQ      'F06'
182100100908     C     DSUOP1        oreq      'F6N'
182200931015     C                   EXSR      AZZVID
182300940104     C                   MOVE      DSUTIP        V1CTIP
182400940104     C                   Z-ADD     DSUCOD        V1CCOD
182500931210     C                   EXSR      CARV1                                        *CARICA TESTATA VIDE
182600100908     c
182700100907     c* Se richiamato da conferma movimenti giornalieri carico i dati del video
182800100908    2c                   if        dsuop1='F6N' and %parms>1
182900100908     c                   clear                   savtmag
183000100907     c                   exsr      RieVid
183100100908    3c                   if        $err='0'
183200100908     c                   exsr      INSERI
183300110404     c
183400110404     c* se causale FISE e ho guasto fisico--> scrivo già UNMOR
183500110803     c* Scrivo anche se importo obbligatorio da tabella UNT
183600110404     c                   if        v1ccau='FISE'
183700110404     c                   eval      dgio01=ungflo
183800110803     c                   if        §giogsf='S'  or (d§untoimpd='S'
183900110803     c                              and d§untimpd>0)
184000110404     c                   clear                   unmor000
184100110404     c                   eval      unrtip=unmtip
184200110404     c                   eval      unrcod=unmcod
184300110404     c                   eval      unrdtm=unmdtm
184400110404     c                   eval      unrmgc=unmmgc
184500110404     c                   eval      unriamag='S'
184600110404     c                   eval      unrdes=ungnot
184700110610     c                   eval      unrimpa=d§untimpd
184800110404     c
184900110404     c                   write     unmor000
185000110404     c                   endif
185100110404     c                   endif
185200100908     c
185300100908     c* se SAVTMAG ='Y' devo scrivere anche il 2° movimento
185400100908    4c                   if        savtmag='Y'
185500100908     C                   EXSR      AZZVID
185600100908     C                   MOVE      DSUTIP        V1CTIP
185700100908     C                   Z-ADD     DSUCOD        V1CCOD
185800100908     C                   EXSR      CARV1                                        *CARICA TESTATA VIDE
185900100908     c                   exsr      RieVid
186000100908     c                   exsr      INSERI
186100100908e   4c                   endif
186200100908     c
186300100908     c                   eval      $fine='1'
186400100908e   3c                   endif
186500100908e   2c                   endif
186600100908     c
186700100903     c                   exsr      decodi
186800100903     c                   eval      $err='0'
186900100908    1C                   END
187000000000     C*
187100000000     C                   ENDSR
187200971010     C*================================================================
187300930805     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
187400930805     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
187500930805     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
187600930805     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
187700930805     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
187800930805     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
187900930805     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
188000930714     C*================================================================
188100930803     C* INZSTD - ROUTINE INIZIALE STANDARD
188200930714     C*================================================================
188300930803     C     INZSTD        BEGSR
188400930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
188500930802     C                   CALL      'XKEYS'
188600930802     C                   PARM                    DSKEYS
188700930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
188800930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
188900930714     C     XMSGPA        PLIST
189000930714     C                   PARM                    DSMSMS
189100930714     C                   PARM                    DSMSPG
189200930714     C                   PARM                    DSMSKE
189300930714     C                   PARM                    DSMSER
189400930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
189500941115     C                   MOVEL     'TNSDU5R'     DSMSPG                         *NOME PGM CHIAMANTE
189600941115     C                   MOVEL     'TNSDU5R'     VMCQUE                         *CODA MESSAGGI DESTI
189700930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
189800930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
189900930805     C* INIZIALIZZO FLAG CONTROLLO PGM
190000930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
190100930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
190200930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
190300930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
190400930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
190500930721     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
190600930714     C* LANCIO ROUTINE PER VERIFICARE SE
190700930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
190800930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
190900020711
191000020711     c     *dtaara       define    §azute        azuteds
191100020711     c     *dtaara       define    §datiute      ddatiute
191200020711
191300020711     c                   in(E)     *dtaara
191400020711     c                   if        %Error or rsut = *blanks
191500020711     c                   clear                   tibs34ds
191600020711     c                   call      'TIBS34R'
191700020711     c                   parm                    Tibs34Ds
191800020711     c                   in        *dtaara
191900020711     c                   endif
192000020711
192100930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
192200930721     C                   BITOFF    '01234567'    POSMI1            1
192300930721     C                   BITOFF    '01234567'    POSMI2            1
192400930721     C                   BITOFF    '01234567'    POSMA1            1
192500930721     C                   BITOFF    '01234567'    POSMA2            1
192600930721     C                   BITON     '34'          POSMI1            1            *18
192700930721     C                   BITON     '7'           POSMI2            1            *01
192800930721     C                   BITON     '34'          POSMA1            1            *18
192900930721     C                   BITON     '0'           POSMA2            1            *80
193000930721     C                   MOVE      POSMI2        POSMIN            2
193100930721     C                   MOVEL     POSMI1        POSMIN
193200930721     C                   MOVE      POSMA2        POSMAX            2
193300930721     C                   MOVEL     POSMA1        POSMAX
193400930714     C                   ENDSR
193500930804     C*================================================================
193600930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
193700930804     C*================================================================
193800930804     C     FUNSTD        BEGSR
193900930804     C                   SELECT
194000930804     C     KEY           WHENEQ    F3
194100930804     C                   EXSR      F03STD
194200930804     C     KEY           WHENEQ    F12
194300930804     C                   EXSR      F12STD
194400930804     C     KEY           WHENEQ    F24
194500930804     C                   EXSR      F24STD
194600930804     C                   OTHER
194700930804     C                   MOVEL     MSG(2)        $MSG
194800930804     C                   EXSR      CARMSG
194900930804     C                   ENDSL
195000930804     C                   ENDSR
195100930804     C*================================================================
195200930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
195300930804     C*================================================================
195400930804     C     F03STD        BEGSR
195500940105     C* IMPOSTO FLAG DI PREMUTO TASTO F03
195600940105     C                   MOVEL     '1'           $F03
195700930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
195800930804     C                   MOVEL     '1'           $FINE
195900930804     C                   ENDSR
196000930804     C*================================================================
196100930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
196200930804     C*================================================================
196300930804     C     F12STD        BEGSR
196400930805     C* IMPOSTO FLAG DI FIN PGM
196500930804     C                   MOVEL     '1'           $FINE
196600930804     C                   ENDSR
196700930804     C*================================================================
196800930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
196900930804     C*================================================================
197000930804     C     F24STD        BEGSR
197100930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
197200930804     C                   EXSR      OUTFUN
197300930804     C                   ENDSR
197400930721     C*================================================================
197500930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
197600930721     C*          CODA MESSAGGI DEL PGM
197700930721     C*================================================================
197800930721     C     CARMSG        BEGSR
197900930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
198000930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
198100930721     C     MSGULT        IFEQ      0
198200930721     C                   MOVEL     '1'           *IN40
198300941115     C                   WRITE     SDU5DVK
198400930721     C                   MOVEL     '0'           *IN40
198500930805     C                   ENDIF
198600930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
198700930721     C                   MOVEL     $MSG          DSMSMS
198800930721     C                   CALL      'XMSG'        XMSGPA
198900930721     C     DSMSER        IFEQ      0
199000930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
199100930805     C                   ENDIF
199200930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
199300931015     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
199400941115     C                   WRITE     SDU5DVM                                      *SFL MESS
199500930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
199600930721     C                   MOVEL     *BLANKS       $MSG
199700930721     C                   ENDSR
199800930721     C*================================================================
199900930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
200000930721     C*================================================================
200100930721     C     OUTMSG        BEGSR
200200930721     C* VISUALIZZAZIONE MESSAGGIO
200300930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
200400941115     C                   WRITE     SDU5DVK                                      *CONTR MESS
200500930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
200600930721     C                   Z-ADD     *ZEROS        MSGULT
200700930721     C                   ENDSR
200800930803     C*================================================================
200900930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
201000930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
201100930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
201200930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
201300930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
201400930803     C*================================================================
201500930803     C     CARFUN        BEGSR
201600930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
201700931015     C     1             DO        31            I                 5 0          --- 1 -->
201800930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
201900930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
202000930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
202100020709     c                   If        DutLpo = 'S'
202200940105     C     DSUOP0        IFEQ      ' 2'                                         *GESTIONE
202300930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
202400930803     C                   ELSE                                                   *INTERROG.
202500930803     C                   MOVEL     FUNASI        FAT(I)
202600930803     C                   ENDIF
202700930803     C                   ELSE                                                   <-- 2 --> *FILIALE
202800930803     C     DSUOP0        IFEQ      ' 2'                                         *GESTIONE
202900930803     C                   MOVEL     FUNAFG        FAT(I)
203000930803     C                   ELSE                                                   *INTERROG.
203100930803     C                   MOVEL     FUNAFI        FAT(I)
203200930803     C                   ENDIF
203300930803     C                   ENDIF                                                  <-- 2 ---
203400930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
203500930806     C                   MOVEL     USER          WEDP              3            *PROFILO
203600930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
203700930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
203800930806     C                   ELSE
203900930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
204000930806     C                   ENDIF
204100930806     C                   ENDIF
204200930803     C                   ENDDO                                                  <-- 1 ---
204300930803     C                   ENDSR
204400930803     C*================================================================
204500930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
204600930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
204700930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
204800930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
204900930803     C*================================================================
205000930803     C     OUTFUN        BEGSR
205100930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
205200930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
205300930803     C                   ENDDO
205400930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
205500930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
205600930803     C     1             DO        31            I
205700930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
205800930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
205900930803     C                   ADD       1             FUNNAT
206000930803     C                   ENDIF
206100930803     C                   ENDDO
206200930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
206300930803     C     FUNNAT        IFGT      8
206400930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
206500930803     C                   ENDIF
206600930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
206700930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
206800930803     C                   Z-ADD     8             FUNVIS            5 0
206900930803     C     FAT(23)       IFEQ      'S'
207000930803     C                   SUB       1             FUNVIS
207100930803     C                   ENDIF
207200930803     C     FAT(24)       IFEQ      'S'
207300930803     C                   SUB       1             FUNVIS
207400930803     C                   ENDIF
207500930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
207600931015     C                   Z-ADD     *ZEROS        W                 5 0
207700930803     C                   ADD       1             FUNULT
207800930803     C     FUNULT        IFGT      31
207900930803     C                   Z-ADD     1             FUNULT
208000930805     C                   ENDIF
208100930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
208200930803     C     FUNULT        DO        31            I                              --- 1 -->
208300930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
208400930803     C     W             IFLT      FUNVIS                                       --- 2 -->
208500930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
208600930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
208700930803     C     FDE(I)        ANDNE     *BLANKS
208800930805     C     I             IFEQ      23                                           *TOLGO F23
208900930805     C     I             OREQ      24                                           *E F24
209000930803     C                   ELSE
209100930803     C                   ADD       1             W
209200930803     C                   MOVEL     FDE(I)        FUV(W)
209300930803     C                   ENDIF
209400930803     C                   ENDIF                                                  <-- 3 ---
209500930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
209600930803     C                   Z-ADD     I             FUNULT
209700930803     C                   ENDIF                                                  <-- 2 ---
209800930803     C                   ENDDO                                                  <-- 1 ---
209900930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
210000930806     C                   MOVEL     FUV(1)        VFD001
210100930806     C                   MOVEL     FUV(2)        VFD002
210200930806     C                   MOVEL     FUV(3)        VFD003
210300930806     C                   MOVEL     FUV(4)        VFD004
210400930806     C                   MOVEL     FUV(5)        VFD005
210500930806     C                   MOVEL     FUV(6)        VFD006
210600930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
210700930803     C     FAT(24)       ANDEQ     'N'
210800930806     C                   MOVEL     FUV(7)        VFD007
210900930806     C                   MOVEL     FUV(8)        VFD008
211000930803     C                   ELSE                                                   <-- 1 -->
211100930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
211200930803     C     FAT(24)       ANDEQ     'S'
211300930806     C                   MOVEL     FDE(23)       VFD007
211400930806     C                   MOVEL     FDE(24)       VFD008
211500930803     C                   ELSE                                                   <-- 2 -->
211600930806     C                   MOVEL     FUV(7)        VFD007
211700930803     C     FAT(23)       IFEQ      'S'
211800930806     C                   MOVEL     FDE(23)       VFD008
211900930803     C                   ELSE
212000930806     C                   MOVEL     FDE(24)       VFD008
212100930805     C                   ENDIF
212200930805     C                   ENDIF                                                  <-- 2 ---
212300930805     C                   ENDIF                                                  <-- 1 ---
212400930803     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
212500930803     C                   ENDSR
212600930803     C*================================================================
212700930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
212800930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
212900930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
213000930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
213100930803     C*================================================================
213200930803     C     CONFUN        BEGSR
213300930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
213400930803     C     1             DO        31            I                              --- 1 -->
213500930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
213600940105     C* CONTROLLO SE IL TASTO E' ATTIVO
213700930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
213800930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
213900930803     C                   EXSR      CARMSG
214000930803     C                   MOVEL     '1'           $ERR
214100930803     C                   ENDIF                                                  <-- 3 ---
214200930803     C                   ENDIF                                                  <-- 2 ---
214300930803     C                   ENDDO                                                  <-- 1 ---
214400930803     C                   ENDSR
214500930802** ======== SCHIERA: MSG  (MESSAGGI)                       ================
214600941115TNSDU5R- Tasto funzionale non valido.                                          1
214700941115TNSDU5R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
214800941115TNSDU5R- Opzione non valida.                                                   3
214900941115TNSDU5R- Opzione abilitata ma non gestita. Avvertire CED.                      4
215000941115TNSDU5R- Inizio elenco.                                                        5
215100941115TNSDU5R- Fine elenco.                                                          6
215200020321TNSDU5R- Cliente non accettabile.                                              7
215300941115TNSDU5R- Magazzino errato.                                                     8
215400941115TNSDU5R- Tipo unità errato.                                                    9
215500941115TNSDU5R- Data errata.                                                         10
215600941115TNSDU5R- Codice unità non accettabile.                                        11
215700100804Se data convalida immettere anche emissioneFattura S/N                        12
215800100804Tipo comunicazione respo.danno errato                                         13
215900971013TNSDU5R- Richiesta funzione per movimento inesistente                         14
216000100804Inserimento obbligatorio: premere F6                                          15
216100941115TNSDU5R- Causale inesistente.                                                 16
216200941115TNSDU5R- Inserimento effettuato.                                              17
216300941115TNSDU5R- Modifica effettuata.                                                 18
216400941115TNSDU5R- Cancellazione effettuata.                                            19
216500941115TNSDU5R- Ripristino effettuato.                                               20
216600101105Immettere tipo fornitore: A=Aut  C=Coop  L=Coop Logistica                     21
216700101105Fornitore inesistente                                                         22
216800100804Immettere cod.Fornitore se inserito  Tipo                                     23
216900100805Responsabile del danno errato                                                 24
217000100921Magazzino Destinazione incongruente con la causale                            25
217100100921Magazzino Correlato incongruente con la causale                               26
217200110401Se unità in proprietà alla 923 non posso accettare MAG 990                    27
217300000000** ======== SCHIERA: CMD  (COMANDI)                        ================
217400000000WRKSPLF
217500971104DSPRCDLCK FILE(*LIBL/UNMOV00F)
217600000000
217700000000
217800000000
217900930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
218000930806F1    -                   -    NN     NN
218100930723F2    -                   -    NN     NN
218200930802F3    -F3=Fine            -    SS     SS
218300930723F4    -                   -    NN     NN
218400930723F5    -                   -    NN     NN
218500931015F6    -F6=Aggiornamento   -    SN     NN
218600930723F7    -                   -    NN     NN
218700930723F8    -                   -    NN     NN
218800971013F9    -F9=Unità           -    SS     SS
218900930723F10   -                   -    NN     NN
219000930723F11   -                   -    NN     NN
219100930802F12   -F12=Ritorno        -    SS     SS
219200930723F13   -                   -    NN     NN
219300930723F14   -                   -    NN     NN
219400930723F15   -                   -    NN     NN
219500930723F16   -                   -    NN     NN
219600930723F17   -                   -    NN     NN
219700930723F18   -                   -    NN     NN
219800930723F19   -                   -    NN     NN
219900930723F20   -                   -    NN     NN
220000971013F21   -F21=Stampa         -    SS     SS
220100930804F22   -                   -    NN     NN
220200930723F23   -                   -    NN     NN
220300930803F24   -F24=Altri tasti    -    NN     NN
220400930802ENTER -                   -    SS     SS
220500930802ROLLUP-                   -    NN     NN
220600930802ROLLDN-                   -    NN     NN
220700930802HELP  -                   -    NN     NN
220800930802PRINT -                   -    NN     NN
220900930802BSPACE-                   -    NN     NN
221000930802CLEAR -                   -    NN     NN
