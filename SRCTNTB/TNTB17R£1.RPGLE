000100011108      *--------------------------------------------------------------------------------------------*
000200011108      * Gestione tabella Limiti C/Assegno 'LCA'                                                    *
000300011108      *--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500011108      *--------------------------------------------------------------------------------------------*
000600011108      * Data base                                                                                  *
000700011108      *--------------------------------------------------------------------------------------------*
000800011108     FTNTB17D   CF   E             WORKSTN
000900011108     FTNTBE01L  UF A E           K DISK
001000011108      *--------------------------------------------------------------------------------------------*
001100011108      * Data structure                                                                             *
001200011108      *--------------------------------------------------------------------------------------------*
001300011108      * Schiere a tempo di compilazione
001400011108      *---
001500011108     D Opz             S             15    DIM(3) CTDATA PERRCD(1)              *Messaggi video
001600011108     D Msg             S             78    DIM(4) CTDATA PERRCD(1)              *Messaggi video
001700011108      *---
001800011108      * Schiere memorizzazione dati correnti
001900011108      *---
002000011108     D DLca          E DS                                                       *reperimento dati
002100050301     D  alflcab              103    115
002200011108      *---
002300011106     D Tibs02Ds      E DS
002400011106      *----------
002500011108      * Variabili
002600010925      *----------
002700011108     D $Fine           S              1    INZ('N')                             *fine programma
002800011108     D TipVid          S              1    INZ('1')                             *video in gestione
002900011108     D $CarVd1         S              1    INZ('1')                             *video in gestione
003000011108     D $CarVd2         S              1    INZ('0')                             *video in gestione
003100011108     D Win             S             99A
003200011108      *---
003300011108      * Indici
003400011108      *---
003500011108     D X               S              2  0 INZ                                  *indice
003600011108     D Y               S              2  0 INZ                                  *indice
003700011108      *---
003800011108      * Architettura
003900011108      *---
004000011108     D Kpjba         E DS                                                       *architettura
004100020715
004200020715     d DdatiUte      e ds
004300020715     d AzuteDs       e ds                  ExtName(Azute00f)
004400020715     d Tibs34Ds      e ds                  inz
004500011108      *--------------------------------------------------------------------------------------------*
004600011108      * Main lines
004700011108      *--------------------------------------------------------------------------------------------*
004800011108      *
004900011108      * Operazioni iniziali
005000011106     C                   EXSR      RutInz
005100011108      *
005200011108      * Gestione video
005300011106     C     $Fine         DOUEQ     'S'
005400011106     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile vuoto
005500011106     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile vuoto
005600011105     C                   ENDCS
005700011105     C                   ENDDO
005800011108      *
005900000000     C                   SETON                                        LR
006000011108      *--------------------------------------------------------------------------------------------*
006100011108      * GesVd1 - Gestione videata selezione codice tabella -
006200011108      *--------------------------------------------------------------------------------------------*
006300011106     C     GesVd1        BEGSR
006400010925      *
006500011108      * Inizializzazione videata
006600011106     C     $CarVd1       ifeq      *on
006700011106     C                   EXSR      CarVd1
006800011106     C                   move      *off          $CarVd1
006900010925     C                   endif
007000011106      * Scrivo la testata
007100011108     C                   WRITE     Tb17T01
007200011106      *
007300011108      * Se esistono errori sulla videata
007400011106      * emetto la write del formato a indicatori spenti per vedere
007500011106      * le eventuali decodifiche
007600011106     C     *IN25         ifeq      *on
007700011106     C                   movea     *In           Win
007800011106     C                   move      *ALL'0'       in3040           11
007900011106     C                   movea     IN3040        *in(40)
008000011108     C                   WRITE     Tb17V01
008100011106     C                   movea     Win           *In
008200010925     C                   endif
008300011106      *
008400011108     C                   EXFMT     Tb17V01
008500011002      *
008600011106     c     '?'           scan      V1Cca                                  50
008700011105      * Ricerca
008800011105     c                   select
008900011105      * F3=Fine
009000011106     C     *inKc         wheneq    *on
009100010925     C                   EXSR      F03Ges
009200011105      *
009300011106     C     *in50         wheneq    *on
009400011106     C                   EXSR      Search
009500011105     C                   other
009600010925      *
009700010925      * Controlli e decodifiche
009800010925     C                   EXSR      CtrD1
009900011105      *
010000011106     C     *IN25         ifeq      *off
010100010925      *
010200011108      * Gestisco il video di dettaglio
010300011105     C                   move      *on           $carvd2
010400011106     C                   move      '2'           TipVid
010500011105     C                   endif
010600011105      *
010700011105     C                   endsl
010800011105      *
010900011105      *
011000010925     C                   endsr
011100011108      *--------------------------------------------------------------------------------------------*
011200011108      * GesVd2 - Gestione videata dettaglio dati
011300011108      *--------------------------------------------------------------------------------------------*
011400011106     C     GesVd2        BEGSR
011500011105      *
011600011108      * Inizializzazione videata
011700011106     C     $CarVd2       ifeq      *on
011800011106     C                   EXSR      Carvd2
011900011106     C                   move      *off          $CarVd2
012000011105     C                   endif
012100011108      * Scrivo la testata
012200011108     C                   WRITE     Tb17T01
012300011105      *
012400011106      * Se esistono errori sulla videata
012500011105      * emetto la write del formato a indicatori spenti per vedere
012600011105      * le eventuali decodifiche
012700011106     C     *IN25         ifeq      *on
012800011106     C                   movea     *In           Win
012900011106     C                   move      *ALL'0'       in3040           11
013000011106     C                   movea     IN3040        *in(40)
013100011108     C                   WRITE     Tb17V02
013200011106     C                   movea     Win           *In
013300011106     C                   endif
013400011105      *
013500011106     C     *in04         ifeq      *on
013600011108     C                   WRITE     Tb17V02
013700011106     C                   EXFMT     PROTECT
013800011106      *
013900011106     C                   else
014000011108     C                   EXFMT     Tb17V02
014100011106     C                   endif
014200011108      *
014300011108     C                   Select
014400011108      * F3=Fine
014500011106     C     *inKc         wheneq    *on
014600011105     C                   EXSR      F03Ges
014700011108      * F12=ritorno
014800011106     C     *inKl         wheneq    *on
014900011105     C                   EXSR      F12Ges
015000011108      *
015100011108     C                   other
015200011105      *
015300011105      * Controlli e decodifiche
015400011105     C                   EXSR      CtrD2
015500011106      *
015600011106      * Non ci sono errori e ho chiesto conferma/annullamentoripristino
015700011106     c     *in25         ifeq      *off
015800011106     c     *inKf         ifeq      *on
015900011106     c     *inKe         oreq      *on
016000011106     c     *inKq         oreq      *on
016100011105      *
016200011106      * Eseguo aggiornamento
016300011105     C                   EXSR      AggTnt
016400011105     C                   endif
016500011106     C                   endif
016600011108      *
016700011105     C                   endsl
016800011105     C                   endsr
016900011108      *--------------------------------------------------------------------------------------------*
017000011108      * Search - Ricerca su campi video
017100011108      *--------------------------------------------------------------------------------------------*
017200010925     C     Search        BEGSR
017300011002      *
017400011106     C                   setoff                                       2540
017500010928      *
017600011106     C                   move      *blanks       V1CcaD
017700011106     C                   move      *blanks       Tibs02Ds
017800011106     c                   move      'LCA'         T02Cod
017900011106     c                   move      'R'           T02Mod
018000011106     c                   movel     KnSif         T02Sif
018100011105     C                   CALL      'TIBS02R'
018200011106     C                   PARM                    kpjba
018300011106     C                   PARM                    Tibs02Ds
018400011106     C                   if        T02Err =  *blanks
018500011106     C                   movel     T02ke1        V1Cca
018600011106     C                   movel     T02uni        v1CcaD
018700011106     c                   endif
018800010928      *
018900010925     C                   ENDSR
019000011108      *--------------------------------------------------------------------------------------------*
019100011108      * F03Ges - Tasto funzionale F03 -> Fine programma
019200011108      *--------------------------------------------------------------------------------------------*
019300011106     C     F03Ges        BEGSR
019400011108      *
019500011106     C                   movel     'S'           $Fine                          *fine pgm
019600011108      *
019700010925     C                   ENDSR
019800011108      *--------------------------------------------------------------------------------------------*
019900011108      * F12Ges - Tasto funzionale f12 -> Ritorno
020000011108      *--------------------------------------------------------------------------------------------*
020100011106     C     F12Ges        BEGSR
020200011108      *
020300011106     C                   eval      TipVid = '1'
020400011108      *
020500010925     C                   ENDSR
020600011108      *--------------------------------------------------------------------------------------------*
020700011108      * CarVd1 - Carica primo video
020800011108      *--------------------------------------------------------------------------------------------*
020900011106     C     CarVd1        BEGSR
021000011108      *
021100011106     c                   setoff                                       25
021200011106     c                   setoff                                       010204
021300011108     C                   clear                   Tb17V01
021400011106     C                   move      '  ?'         V1Cca
021500011106      *
021600000000     C                   ENDSR
021700011108      *--------------------------------------------------------------------------------------------*
021800011108      * CarVd2 - Carica secondo video
021900011108      *--------------------------------------------------------------------------------------------*
022000011106     C     CarVd2        BEGSR
022100011106     C                   setoff                                       010204
022200050301     C                   setoff                                       3233
022300011106      *
022400011106      * Aggancio la tabella, se trovo il codice sono in modifica
022500011108      * o ripristino (se record annullato), altrimenti in immissione
022600011106     c                   movel     V1Cca         CodTab            3
022700011106     c                   EXSR      ChnTnt
022800011106      *
022900011106      * MODIFICA/RIPRISTINO
023000011106     c  n21              do
023100011106     c                   seton                                        02
023200011106     c                   movel     Tbeuni        DLca
023300011106     c                   eval      v2Cca =V1Cca
023400011106     c                   eval      v2CcaD=§lcads
023500011106     c                   z-add     §LcaMn        V2LMn
023600011106     c                   z-add     §LcaX1        V2LX1
023700011106     c                   z-add     §LcaX2        V2LX2
023800011106     c                   z-add     §LcaCo        V2LCo
023900050301     c                   if        alflcab>=*zeros
024000050301     c                   z-add     §Lcab         V2Lb
024100050301     c                   else
024200050301     c                   movel     *hival        v2lb
024300050301     c                   endif
024400011106      *
024500011106     C     TbeAtb        ifeq      *blanks
024600011106     C                   setoff                                       04
024700011106     C                   eval      V2PzD = Opz(2)
024800011106     C                   else
024900011106     C                   seton                                        04
025000011106     C                   eval      V2PzD = Opz(3)
025100011106     C                   endif
025200011106     c                   enddo
025300011106      *
025400011106      * IMMISSIONE
025500011106     c   21              do
025600011106     c                   movel     *blanks       v1ccad
025700011106     c                   clear                   Tb17V02
025800011106     c                   movel     V1Cca         V2Cca
025900050301     c                   movel     *hival        V2lb
026000011106     c                   seton                                        01
026100011106     C                   eval      V2PzD = Opz(1)
026200011106     c                   enddo
026300011106     c
026400011105     C                   ENDSR
026500011108      ************************************************************
026600011108      * Controllo e decodifica prima videata
026700011108      ************************************************************
026800010925     C     CtrD1         BEGSR
026900011106      *
027000011106     C                   setoff                                       2540
027100011106     c                   clear                   V1Msg
027200011106      *
027300011106      * Codice tabella obbligatorio
027400011106     c     V1Cca         ifeq      *blanks
027500011106     c                   seton                                        2540
027600011106     c                   eval      V1Msg = Msg(1)
027700011106      *
027800011106      * Se esiste decodifico
027900011106     c                   else
028000011106     C                   clear                   tibs02ds
028100011106     C                   movel     'C'           t02mod
028200011106     C                   movel     Sif           t02sif
028300011106     C                   movel     'LCA'         t02cod
028400011106     C                   movel     V1Cca         t02ke1
028500011106     C                   call      'TIBS02R'
028600011106     C                   parm                    KPJBA
028700011106     C                   parm                    TIBS02DS
028800011106     C                   if        t02err =  *blanks
028900011106     C                   movel     t02uni        V1CcaD
029000011106     c                   endif
029100011106     c                   endif
029200010925      *
029300010925     C                   ENDSR
029400011108      ************************************************************
029500011108      * Controllo e decodifica seconda videata
029600011108      ************************************************************
029700011105     C     CtrD2         BEGSR
029800011108     C                   setoff                                       2531
029900011106     C                   setoff                                       3233
030000011106     C                   clear                   V2Msg
030100011106     C
030200011106      *
030300011106      * Controlli da effettuarsi se record non annullato
0304000111061a   C     *In04         ifeq      *off
030500011106      *
030600011106      * Controllo che l'importo max 1 non sia maggiore del max2
0307000111062a   C     V2LX1         ifgt      V2LX2
030800011106     C                   seton                                        2533
030900011108     C                   eval      V2Msg = Msg(4)
0310000111062-   C                   endif
031100011106      *
031200011108      * Se ho digitato uno dei due importi massimi
031300011108      * controllo che l'importo minimo non sia maggiore dei due massimi
0314000111082a   C     V2LX1         ifgt      0
031500011108     C     V2LX2         orgt      0
031600011108      *
0317000111083a   C     V2LMn         ifgt      V2LX1
0318000111083a   C     V2LX1         andgt     0
031900011106     C     V2LMn         orgt      V2LX2
0320000111083a   C     V2LX2         andgt     0
032100011106     C                   seton                                        2532
032200011108     C                   eval      V2Msg = Msg(3)
0323000111083-   C                   endif
0324000111082-   C                   endif
032500011106      *
032600011106      * Descrizione non inserita
0327000111062a   C     V2CcaD        ifeq      *blanks
032800011106     C                   seton                                        2531
032900011108     C                   eval      V2Msg = Msg(2)
0330000111062-   C                   endif
033100011106      *
0332000111061-   C                   endif
033300011105      *
033400011105     C                   ENDSR
033500011108      ************************************************************
033600011108      * Aggiornamento tabella LCA
033700011108      ************************************************************
033800010925     C     AggTnt        BEGSR
033900010926      *
034000011105     C                   SELECT
034100011106      * F06=Conferma
034200011106     C     *InKf         wheneq    *on
034300011106      *
034400011105      * Immissione
034500011108     C     *in01         ifeq      *on
034600011105     C                   EXSR      RieTnt
034700011106     C                   move      *zeros        TbeFlt
034800011107     C                   move      'S'           TbeFtt
034900011108     c                   move      *zeros        Tbedtr
035000011105     C                   WRITE     tntbe000                             22
035100011106     C                   endif
035200011105      * Modifica
035300011106     C     *in02         Ifeq      *on
035400011106     C                   EXSR      RieTnt
035500011106     C                   UPDATE    tntbe000
035600011106     C                   endif
035700011105      *
035800011106      * F16=Annullamento
035900011105     C     *inkq         wheneq    *on
036000011106     C     *in04         andeq     *off
036100011105     C                   move      'A'           tbeatb
036200011107     C                   move      *blanks       tbeftr
036300011106     C                   UPDATE    tntbe000
036400011105      *
036500011106      * F5=Ripristino
036600011105     C     *inke         wheneq    *on
036700011106     C     *in04         andeq     *on
036800011105     C                   move      ' '           tbeatb
036900011107     C                   move      *blanks       tbeftr
037000011106     C                   UPDATE    tntbe000
037100011105      *
037200011105     C                   ENDSL
037300011108      * Torno alla prima videata che carico come da inizio pgm
037400011106     C                   move      '1'           TipVid
037500011106     C                   move      *on           $CarVd1
037600011106     C
037700010925     C                   ENDSR
037800011108      ************************************************************
037900011108      * Riempimento file tabella
038000011108      ************************************************************
038100010925     C     RieTnt        BEGSR
038200011106      *
038300011106      * Codice tabella
038400011106     c                   movel     *blanks       TbeAtb
038500011106     c                   move      Sif           TbeSif
038600011106     c                   move      'GE'          TbeApl
038700011106     c                   movel     'LCA'         TbeCod
038800011106     c                   movel     V2cca         TbeKe1
038900011106     c                   movel     *blanks       TbeKe2
039000011105      *
039100011106     c                   clear                   DLca
039200011108     c                   movel     V2CcaD        §LCADS
039300011108     c                   z-add     V2Lmn         §LCAMN
039400011108     c                   z-add     V2Lx1         §LCAX1
039500011108     c                   z-add     V2Lx2         §LCAX2
039600011108     c                   z-add     V2Lco         §LCACO
039700050301     c                   z-add     V2Lb          §LCAb
039800011108     c                   movel     DLca          TbeUni
039900011106      *
040000011106     c                   move      *blanks       TbeFtr
040100010925     C
040200010925     C                   ENDSR
040300011108      *--------------------------------------------------------------------------------------------*
040400011108      * Aggancio tabella
040500011108      *--------------------------------------------------------------------------------------------*
040600011106     c     ChnTnt        BEGSR
040700011106     c                   move      'LCA'         TbeCod
040800011106     c                   clear                   TbeKe1
040900011106     c                   movel     CodTab        TbeKe1
041000011106     c                   move      *blanks       TbeKe2
041100011106     c                   move      *blanks       TbeLin
041200011106     c                   movel     Knsif         TbeSif
041300011106     c     k01tbe5       chain     tntbe01l                           21
041400011106      * Se non ho trovato il record con il sistema informativo
041500011106      * che ho in linea, lo abblenco
041600011106     c   21              do
041700011106     c                   eval      TbeSif= *blanks
041800011106     c                   eval      Sif= *blanks
041900011106     c     k01tbe5       chain     tntbe01l                           21
042000011106     c  N21              eval      Sif= *blanks
042100011106     c                   enddo
042200011108      *
042300011106     c                   ENDSR
042400011108      *--------------------------------------------------------------------------------------------*
042500011108      * RutInz - operazioni iniziali
042600011108      *--------------------------------------------------------------------------------------------*
042700011108     C     RutInz        BEGSR
042800011108      *
042900011108      * Ricevimento parametri
043000000000     C     *ENTRY        PLIST
043100000000     C                   PARM                    kpjba
043200011108      *
043300011108      * Chiavi di lettura
043400010925     c     K01tbe5       KLIST                                                  *tntbe01l
043500010925     c                   KFLD                    tbecod                          -tabella
043600010925     c                   KFLD                    tbeke1                          -chiave uno
043700010925     c                   KFLD                    tbeke2                          -chiave due
043800010925     c                   KFLD                    tbelin                          -lingua
043900010925     c                   KFLD                    tbesif                          -s.informativo
044000010925      *
044100011108      * Reperisce dati utente
044200020715
044300020715     c     *dtaara       define    §azute        azuteds
044400020715     c     *dtaara       define    §datiute      ddatiute
044500020715
044600020715     c                   in(E)     *dtaara
044700020715     c                   if        %Error or rsut = *blanks
044800020715     c                   clear                   tibs34ds
044900020715     c                   call      'TIBS34R'
045000020715     c                   parm                    Tibs34Ds
045100020715     c                   in        *dtaara
045200020715     c                   endif
045300011106      *
045400011106      * Sistema informativo
045500011106     C                   movel     knsif         Sif              10
045600011106     C
045700000000     C                   ENDSR
045800000000     O*--------------------------------------------------------------------------------------------*
045900010926** -OPZ-
046000010926  Inserimento
046100010926   Modifica
046200010926  Annullato
046300000000** -MSG-
046400011105TNTB17R- Immettere il codice tabella
046500011105TNTB17R- Descrizione obbligatoria
046600011108TNTB17R- L'importo minimo non puo' essere maggiore di un importo massimo
046700011108TNTB17R- L'importo massimo 1 non puo' essere maggiore del massimo 2
