000100011108      *--------------------------------------------------------------------------------------------*
000200031112      * Gestione tabella Vdl Pacchi 'MSC'                                                          *
000300011108      *--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500011108      *--------------------------------------------------------------------------------------------*
000600011108      * Data base                                                                                  *
000700011108      *--------------------------------------------------------------------------------------------*
000800031112     FTNTB33D   CF   E             WORKSTN
000900011108     FTNTBE01L  UF A E           K DISK
001000031112     fazorg01l  if   e           k disk
001100011108      *--------------------------------------------------------------------------------------------*
001200011108      * Data structure                                                                             *
001300011108      *--------------------------------------------------------------------------------------------*
001400011108      * Schiere a tempo di compilazione
001500011108      *---
001600031112     D Opz             S             15    DIM(03) CTDATA PERRCD(1)             *Messaggi video
001700031112     D Msg             S             78    DIM(10) CTDATA PERRCD(1)             *Messaggi video
001800011108      *---
001900011108      * Schiere memorizzazione dati correnti
002000011108      *---
002100031112     D DMsc          E DS                                                       *reperimento dati
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
003200031112     d kazorg          s                   like(orgfil)
003300011108      *---
003400011108      * Indici
003500011108      *---
003600011108     D X               S              2  0 INZ                                  *indice
003700011108     D Y               S              2  0 INZ                                  *indice
003800011108      *---
003900011108      * Architettura
004000011108      *---
004100011108     D Kpjba         E DS                                                       *architettura
004200020715
004300020715     d DdatiUte      e ds
004400020715     d AzuteDs       e ds                  ExtName(Azute00f)
004500020715     d Tibs34Ds      e ds                  Inz
004600011108      *--------------------------------------------------------------------------------------------*
004700011108      * Main lines
004800011108      *--------------------------------------------------------------------------------------------*
004900011108      *
005000011108      * Operazioni iniziali
005100011106     C                   EXSR      RutInz
005200011108      *
005300011108      * Gestione video
005400011106     C     $Fine         DOUEQ     'S'
005500011106     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile vuoto
005600011106     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile vuoto
005700011105     C                   ENDCS
005800011105     C                   ENDDO
005900011108      *
006000000000     C                   SETON                                        LR
006100011108      *--------------------------------------------------------------------------------------------*
006200011108      * GesVd1 - Gestione videata selezione codice tabella -
006300011108      *--------------------------------------------------------------------------------------------*
006400011106     C     GesVd1        BEGSR
006500010925      *
006600011108      * Inizializzazione videata
006700011106     C     $CarVd1       ifeq      *on
006800011106     C                   EXSR      CarVd1
006900011106     C                   move      *off          $CarVd1
007000010925     C                   endif
007100011106      * Scrivo la testata
007200031112     C                   WRITE     Tb33T01
007300011106      *
007400011108      * Se esistono errori sulla videata
007500011106      * emetto la write del formato a indicatori spenti per vedere
007600011106      * le eventuali decodifiche
007700031112     C     *IN28         ifeq      *on
007800011106     C                   movea     *In           Win
007900011106     C                   move      *ALL'0'       in3040           11
008000011106     C                   movea     IN3040        *in(40)
008100031112     C                   WRITE     Tb33V01
008200011106     C                   movea     Win           *In
008300010925     C                   endif
008400011106      *
008500031112     C                   EXFMT     Tb33V01
008600011002      *
008700031112     c     '?'           scan      V1fgs                                  50
008800011105      * Ricerca
008900011105     c                   select
009000011105      * F3=Fine
009100011106     C     *inKc         wheneq    *on
009200010925     C                   EXSR      F03Ges
009300011105      *
009400011106     C     *in50         wheneq    *on
009500011106     C                   EXSR      Search
009600011105     C                   other
009700010925      *
009800010925      * Controlli e decodifiche
009900010925     C                   EXSR      CtrD1
010000011105      *
010100031112     C     *IN28         ifeq      *off
010200010925      *
010300011108      * Gestisco il video di dettaglio
010400011105     C                   move      *on           $carvd2
010500011106     C                   move      '2'           TipVid
010600011105     C                   endif
010700011105      *
010800011105     C                   endsl
010900011105      *
011000011105      *
011100010925     C                   endsr
011200011108      *--------------------------------------------------------------------------------------------*
011300011108      * GesVd2 - Gestione videata dettaglio dati
011400011108      *--------------------------------------------------------------------------------------------*
011500011106     C     GesVd2        BEGSR
011600011105      *
011700011108      * Inizializzazione videata
011800011106     C     $CarVd2       ifeq      *on
011900011106     C                   EXSR      Carvd2
012000011106     C                   move      *off          $CarVd2
012100011105     C                   endif
012200011108      * Scrivo la testata
012300031112     C                   WRITE     Tb33T01
012400011105      *
012500011106      * Se esistono errori sulla videata
012600011105      * emetto la write del formato a indicatori spenti per vedere
012700011105      * le eventuali decodifiche
012800031112     C     *IN28         ifeq      *on
012900011106     C                   movea     *In           Win
013000011106     C                   move      *ALL'0'       in3040           11
013100011106     C                   movea     IN3040        *in(40)
013200031112     C                   WRITE     Tb33V02
013300011106     C                   movea     Win           *In
013400011106     C                   endif
013500011105      *
013600011106     C     *in04         ifeq      *on
013700031112     C                   WRITE     Tb33V02
013800011106     C                   EXFMT     PROTECT
013900011106      *
014000011106     C                   else
014100031112     C                   EXFMT     Tb33V02
014200011106     C                   endif
014300011108      *
014400011108     C                   Select
014500011108      * F3=Fine
014600011106     C     *inKc         wheneq    *on
014700011105     C                   EXSR      F03Ges
014800011108      * F12=ritorno
014900011106     C     *inKl         wheneq    *on
015000011105     C                   EXSR      F12Ges
015100011108      *
015200011108     C                   other
015300011105      *
015400011105      * Controlli e decodifiche
015500011105     C                   EXSR      CtrD2
015600011106      *
015700011106      * Non ci sono errori e ho chiesto conferma/annullamentoripristino
015800031112     c     *in28         ifeq      *off
015900011106     c     *inKf         ifeq      *on
016000011106     c     *inKe         oreq      *on
016100011106     c     *inKq         oreq      *on
016200011105      *
016300011106      * Eseguo aggiornamento
016400011105     C                   EXSR      AggTnt
016500011105     C                   endif
016600011106     C                   endif
016700011108      *
016800011105     C                   endsl
016900011105     C                   endsr
017000011108      *--------------------------------------------------------------------------------------------*
017100011108      * Search - Ricerca su campi video
017200011108      *--------------------------------------------------------------------------------------------*
017300010925     C     Search        BEGSR
017400011002      *
017500031112     C                   setoff                                       2840
017600010928      *
017700011106     C                   move      *blanks       Tibs02Ds
017800031112     c                   move      'MSC'         T02Cod
017900011106     c                   move      'R'           T02Mod
018000011106     c                   movel     KnSif         T02Sif
018100011105     C                   CALL      'TIBS02R'
018200011106     C                   PARM                    kpjba
018300011106     C                   PARM                    Tibs02Ds
018400011106     C                   if        T02Err =  *blanks
018500031112     C                   movel     T02ke1        V1Fgs
018600031112     C                   movel     T02ke2        V1Prg
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
021100031112     c                   setoff                                       28
021200011106     c                   setoff                                       010204
021300031112     C                   clear                   Tb33V01
021400031112     C                   move      '  ?'         V1FGS
021500031112     c                   Clear                   V1dfgs
021600031112     c                   Clear                   V1prg
021700011106      *
021800000000     C                   ENDSR
021900011108      *--------------------------------------------------------------------------------------------*
022000011108      * CarVd2 - Carica secondo video
022100011108      *--------------------------------------------------------------------------------------------*
022200011106     C     CarVd2        BEGSR
022300011106     C                   setoff                                       010204
022400011106      *
022500011106      * Aggancio la tabella, se trovo il codice sono in modifica
022600011108      * o ripristino (se record annullato), altrimenti in immissione
022700011106     c                   EXSR      ChnTnt
022800011106      *
022900011106      * MODIFICA/RIPRISTINO
023000011106     c  n21              do
023100011106     c                   seton                                        02
023200031112     c                   movel     Tbeuni        Dmsc
023300031112     c                   eval      V2fgs = V1fgs
023400031112     c                   eval      v2dfgs = v1dfgs
023500031112     c                   eval      V2prg = v1prg
023600031112     c                   eval      v2cdes = §mscdes
023700031112     c                   eval      v2clnp = §msclnp
023800031112     c                   eval      v2clna = §msclna
023900031112     c                   eval      v2cnrs = §mscnrs
024000031112     c                   eval      v2cnsc = §mscnsc
024100031112     c                   eval      v2cznc = §mscznc
024200031112     c                   eval      v2cpes = §mscpes
024300031112     c                   eval      v2cvol = §mscvol
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
025600031112     c                   clear                   Tb33V02
025700031112     c                   movel     V1fgs         V2fgs
025800031112     c                   movel     V2dfgs        V2dfgs
025900031112     c                   movel     V1prg         V2prg
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
027000031112     C                   setoff                                       2840
027100011106     c                   clear                   V1Msg
027200011106      *
027300031112      * Codntrollo punto operativo
027400031112if  1c     V1fgs         ifeq      *blanks
027500031112     c                   seton                                        2840
027600011106     c                   eval      V1Msg = Msg(1)
027700031112   x1c                   else
027800031112     c                   Movel     v1fgs         kAzorg
027900031112     c                   Exsr      Ctrpo
028000031112if  2c                   if        *in28
028100031112     c                   eval      v1msg = msg(2)
028200031112     c                   seton                                          40
028300031112   x2c                   else
028400031112     c                   Movel     orgdes        v1dfgs
028500031112    2c                   endif
028600031112    1c                   endif
028700010925      *
028800010925     C                   ENDSR
028900011108      ************************************************************
029000011108      * Controllo e decodifica seconda videata
029100011108      ************************************************************
029200011105     C     CtrD2         BEGSR
029300031112     C                   setoff                                       2831
029400011106     C                   setoff                                       3233
029500011106     C                   clear                   V2Msg
029600031112     c                   clear                   v2dlnp
029700031112     c                   clear                   v2dlna
029800011106      *
029900011106      * Controlli da effettuarsi se record non annullato
0300000111061a   C     *In04         ifeq      *off
030100031112      *
030200031112      * Descrizione
0303000311122a   C     V2cdes        ifeq      *blanks
030400031112     C                   seton                                        2831
030500031112     C                   eval      V2Msg = Msg(3)
0306000311122-   C                   endif
030700011106      *
030800031112      * Linea di partenza
0309000311122a   C     V2clnp        ifeq      *zeros
031000031112     C                   seton                                        2832
031100011108     C                   eval      V2Msg = Msg(4)
031200031112x2a  c                   else
031300031112     c                   Movel     v2clnp        kAzorg
031400031112     c                   Exsr      Ctrpo
031500031112if  3c                   if        *in28
031600031112     c                   eval      v2msg = msg(5)
031700031112     c                   seton                                          32
031800031112   x3c                   else
031900031112     c                   Movel     orgdes        v1dlnp
032000031112    3c                   endif
0321000111062-   C                   endif
032200031112      *
032300031112      * Linea di arrivo
0324000311122a   C     V2clna        ifeq      *zeros
032500031112     C                   seton                                        2833
032600031112     C                   eval      V2Msg = Msg(6)
032700031112x2a  c                   else
032800031112     c                   Movel     v2clna        kAzorg
032900031112     c                   Exsr      Ctrpo
033000031112if  3c                   if        *in28
033100031112     c                   eval      v2msg = msg(7)
033200031112     c                   seton                                          33
033300031112   x3c                   else
033400031112     c                   Movel     orgdes        v1dlna
033500031112    3c                   endif
0336000311122-   C                   endif
033700011106      *
033800031112      * se record annullato decodifico le linee
033900031112   x1c                   else
0340000311122a   C     V2clnp        ifne      *zeros
034100031112     c                   Movel     v2clnp        kAzorg
034200031112     c                   Exsr      Ctrpo
034300031112     c                   Movel     orgdes        v1dlnp
0344000311122-   C                   endif
0345000311122a   C     V2clna        ifne      *zeros
034600031112     c                   Movel     v2clna        kAzorg
034700031112     c                   Exsr      Ctrpo
034800031112     c                   Movel     orgdes        v1dlna
0349000311122-   C                   endif
035000011106      *
0351000111061-   C                   endif
035200011105      *
035300011105     C                   ENDSR
035400031112      ************************************************************
035500031112      * Controllo e decodifica p.o.
035600031112      ************************************************************
035700031112     C     Ctrpo         BEGSR
035800031112
035900031112     c     kAzorg        chain     AZORG01L
036000031112if  1c                   if        not %found(AZORG01L)
036100031112     c                             or  ORGfva =  '*'
036200031112     c                   seton                                        28
036300031112e   1c                   endif
036400031112
036500031112     C                   ENDSR
036600011108      ************************************************************
036700011108      * Aggiornamento tabella LVM
036800011108      ************************************************************
036900010925     C     AggTnt        BEGSR
037000010926      *
037100011105     C                   SELECT
037200011106      * F06=Conferma
037300011106     C     *InKf         wheneq    *on
037400011106      *
037500011105      * Immissione
037600011108     C     *in01         ifeq      *on
037700011105     C                   EXSR      RieTnt
037800011106     C                   move      *zeros        TbeFlt
037900011107     C                   move      'S'           TbeFtt
038000011108     c                   move      *zeros        Tbedtr
038100011105     C                   WRITE     tntbe000                             22
038200011106     C                   endif
038300011105      * Modifica
038400011106     C     *in02         Ifeq      *on
038500011106     C                   EXSR      RieTnt
038600011106     C                   UPDATE    tntbe000
038700011106     C                   endif
038800011105      *
038900011106      * F16=Annullamento
039000011105     C     *inkq         wheneq    *on
039100011106     C     *in04         andeq     *off
039200011105     C                   move      'A'           tbeatb
039300011107     C                   move      *blanks       tbeftr
039400011106     C                   UPDATE    tntbe000
039500011105      *
039600011106      * F5=Ripristino
039700011105     C     *inke         wheneq    *on
039800011106     C     *in04         andeq     *on
039900011105     C                   move      ' '           tbeatb
040000011107     C                   move      *blanks       tbeftr
040100011106     C                   UPDATE    tntbe000
040200011105      *
040300011105     C                   ENDSL
040400011108      * Torno alla prima videata che carico come da inizio pgm
040500011106     C                   move      '1'           TipVid
040600011106     C                   move      *on           $CarVd1
040700011106     C
040800010925     C                   ENDSR
040900011108      ************************************************************
041000011108      * Riempimento file tabella
041100011108      ************************************************************
041200010925     C     RieTnt        BEGSR
041300011106      *
041400011106      * Codice tabella
041500011106     c                   movel     *blanks       TbeAtb
041600011106     c                   move      Sif           TbeSif
041700011106     c                   move      'GE'          TbeApl
041800011108     c                   movel     'LVM'         TbeCod
041900011108     c                   movel     V2Lvm         TbeKe1
042000011106     c                   movel     *blanks       TbeKe2
042100011105      *
042200011108     c                   clear                   DLvm
042300011108     c                   movel     V2LvmD        §LVMDS
042400011108     c                   z-add     V2Lmn         §LVMMN
042500011108     c                   z-add     V2Lx1         §LVMX1
042600011108     c                   z-add     V2Lx2         §LVMX2
042700011108     c                   movel     DLvm          TbeUni
042800011106      *
042900011106     c                   move      *blanks       TbeFtr
043000010925     C
043100010925     C                   ENDSR
043200011108      *--------------------------------------------------------------------------------------------*
043300011108      * Aggancio tabella
043400011108      *--------------------------------------------------------------------------------------------*
043500011106     c     ChnTnt        BEGSR
043600031112     c                   move      'MSC'         TbeCod
043700011106     c                   clear                   TbeKe1
043800031112     c                   movel(p)  V1fgs         TbeKe1
043900031112     c                   movel(p)  v1prg         TbeKe2
044000011106     c                   move      *blanks       TbeLin
044100011106     c                   movel     Knsif         TbeSif
044200011106     c     k01tbe5       chain     tntbe01l                           21
044300011106      * Se non ho trovato il record con il sistema informativo
044400011106      * che ho in linea, lo abblenco
044500011106     c   21              do
044600011106     c                   eval      TbeSif= *blanks
044700011106     c                   eval      Sif= *blanks
044800011106     c     k01tbe5       chain     tntbe01l                           21
044900011106     c  N21              eval      Sif= *blanks
045000011106     c                   enddo
045100011108      *
045200011106     c                   ENDSR
045300011108      *--------------------------------------------------------------------------------------------*
045400011108      * RutInz - operazioni iniziali
045500011108      *--------------------------------------------------------------------------------------------*
045600011108     C     RutInz        BEGSR
045700011108      *
045800011108      * Ricevimento parametri
045900000000     C     *ENTRY        PLIST
046000000000     C                   PARM                    kpjba
046100011108      *
046200011108      * Chiavi di lettura
046300010925     c     K01tbe5       KLIST                                                  *tntbe01l
046400010925     c                   KFLD                    tbecod                          -tabella
046500010925     c                   KFLD                    tbeke1                          -chiave uno
046600010925     c                   KFLD                    tbeke2                          -chiave due
046700010925     c                   KFLD                    tbelin                          -lingua
046800010925     c                   KFLD                    tbesif                          -s.informativo
046900010925      *
047000011108      * Reperisce dati utente
047100020715
047200020715     c     *dtaara       define    §azute        azuteds
047300020715     c     *dtaara       define    §datiute      ddatiute
047400020715
047500020715     c                   in(E)     *dtaara
047600020715     c                   if        %Error or rsut = *blanks
047700020715     c                   clear                   tibs34ds
047800020715     c                   call      'TIBS34R'
047900020715     c                   parm                    Tibs34Ds
048000020715     c                   in        *dtaara
048100020715     c                   endif
048200011106      *
048300011106      * Sistema informativo
048400011106     C                   movel     knsif         Sif              10
048500011106     C
048600000000     C                   ENDSR
048700000000     O*--------------------------------------------------------------------------------------------*
048800010926** -OPZ-
048900010926  Inserimento
049000010926   Modifica
049100010926  Annullato
049200000000** -MSG-
049300031112TNTB33R- Immettere il punto operativo
049400031112TNTB33R- Punto Operativo annullato o inesistente
049500031112TNTB33R- Immettere la descrizione
049600031112TNTB33R- Immettere la linea di partenza
049700031112TNTB33R- Linea di partenza annullata o inesistente
049800031112TNTB33R- Immettere la linea di arrivo
049900031112TNTB33R- Linea di arrivo annullata o inesistente
