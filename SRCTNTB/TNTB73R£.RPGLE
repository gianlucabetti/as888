000100011108      *--------------------------------------------------------------------------------------------*
000200090519      * Gestione tabella Limiti per tipo servizio  'LTS'                                           *
000300011108      *--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500011108      *--------------------------------------------------------------------------------------------*
000600011108      * Data base                                                                                  *
000700011108      *--------------------------------------------------------------------------------------------*
000800090519     FTNTB73D   CF   E             WORKSTN
000900011108     FTNTBE01L  UF A E           K DISK
000901090519     FTABEL00F  IF   E           K DISK
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
002000090519     D Dlts          E DS                                                       *reperimento dati
002001090519     D Ds5e          E DS                                                       *reperimento dati
002300011106      *----------
002400011108      * Variabili
002500010925      *----------
002600011108     D $Fine           S              1    INZ('N')                             *fine programma
002700011108     D TipVid          S              1    INZ('1')                             *video in gestione
002800011108     D $CarVd1         S              1    INZ('1')                             *video in gestione
002900011108     D $CarVd2         S              1    INZ('0')                             *video in gestione
003000011108     D Win             S             99A
003001090520     D codut           S              1  0 INZ(1)
003002090519     d cod             s                   like(tblcod)
003003090519     d §key            s                   Like(tblkey)
003100011108      *---
003200011108      * Indici
003300011108      *---
003400011108     D X               S              2  0 INZ                                  *indice
003500011108     D Y               S              2  0 INZ                                  *indice
003600011108      *---
003700011108      * Architettura
003800011108      *---
003900011108     D Kpjba         E DS                                                       *architettura
004000020715
004001090608     d tibs02ds      e ds
004100020715     d DdatiUte      e ds
004200020715     d AzuteDs       e ds                  ExtName(Azute00f)
004300020715     d Tibs34Ds      e ds                  Inz
004400011108      *--------------------------------------------------------------------------------------------*
004500011108      * Main lines
004600011108      *--------------------------------------------------------------------------------------------*
004700011108      *
004800011108      * Operazioni iniziali
004900011106     C                   EXSR      RutInz
005000011108      *
005100011108      * Gestione video
005200011106     C     $Fine         DOUEQ     'S'
005300011106     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile vuoto
005400011106     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile vuoto
005500011105     C                   ENDCS
005600011105     C                   ENDDO
005700011108      *
005800000000     C                   SETON                                        LR
005900011108      *--------------------------------------------------------------------------------------------*
006000090519      * GesVd1 - Gestione videata selezione tipo servizio  -
006100011108      *--------------------------------------------------------------------------------------------*
006200011106     C     GesVd1        BEGSR
006300010925      *
006400011108      * Inizializzazione videata
006500011106     C     $CarVd1       ifeq      *on
006600011106     C                   EXSR      CarVd1
006700011106     C                   move      *off          $CarVd1
006800010925     C                   endif
006900011106      * Scrivo la testata
007000090519     C                   WRITE     Tb73T01
007100011106      *
007200011108      * Se esistono errori sulla videata
007300011106      * emetto la write del formato a indicatori spenti per vedere
007400011106      * le eventuali decodifiche
007500011106     C     *IN25         ifeq      *on
007600011106     C                   movea     *In           Win
007700011106     C                   move      *ALL'0'       in3040           11
007800011106     C                   movea     IN3040        *in(40)
007900090519     C                   WRITE     Tb73V01
008000011106     C                   movea     Win           *In
008100010925     C                   endif
008200011106      *
008300090519     C                   EXFMT     Tb73V01
008400011002      *
008500090519     c     '?'           scan      V1tsp                                  50
008600011105      * Ricerca
008700011105     c                   select
008800011105      * F3=Fine
008900011106     C     *inKc         wheneq    *on
009000010925     C                   EXSR      F03Ges
009100011105      *
009200011106     C     *in50         wheneq    *on
009300011106     C                   EXSR      Search
009400011105     C                   other
009500010925      *
009600010925      * Controlli e decodifiche
009700010925     C                   EXSR      CtrD1
009800011105      *
009900011106     C     *IN25         ifeq      *off
010000010925      *
010100011108      * Gestisco il video di dettaglio
010200011105     C                   move      *on           $carvd2
010300011106     C                   move      '2'           TipVid
010400011105     C                   endif
010500011105      *
010600011105     C                   endsl
010700011105      *
010800011105      *
010900010925     C                   endsr
011000011108      *--------------------------------------------------------------------------------------------*
011100011108      * GesVd2 - Gestione videata dettaglio dati
011200011108      *--------------------------------------------------------------------------------------------*
011300011106     C     GesVd2        BEGSR
011400011105      *
011500011108      * Inizializzazione videata
011600011106     C     $CarVd2       ifeq      *on
011700011106     C                   EXSR      Carvd2
011800011106     C                   move      *off          $CarVd2
011900011105     C                   endif
012000011108      * Scrivo la testata
012100090519     C                   WRITE     Tb73T01
012200011105      *
012300011106      * Se esistono errori sulla videata
012400011105      * emetto la write del formato a indicatori spenti per vedere
012500011105      * le eventuali decodifiche
012600011106     C     *IN25         ifeq      *on
012700011106     C                   movea     *In           Win
012800011106     C                   move      *ALL'0'       in3040           11
012900011106     C                   movea     IN3040        *in(40)
013000090519     C                   WRITE     Tb73V02
013100011106     C                   movea     Win           *In
013200011106     C                   endif
013300011105      *
013400011106     C     *in04         ifeq      *on
013500090519     C                   WRITE     Tb73V02
013600011106     C                   EXFMT     PROTECT
013700011106      *
013800011106     C                   else
013900090519     C                   EXFMT     Tb73V02
014000011106     C                   endif
014100011108      *
014200011108     C                   Select
014300011108      * F3=Fine
014400011106     C     *inKc         wheneq    *on
014500011105     C                   EXSR      F03Ges
014600011108      * F12=ritorno
014700011106     C     *inKl         wheneq    *on
014800011105     C                   EXSR      F12Ges
014900011108      *
015000011108     C                   other
015100011105      *
015200011105      * Controlli e decodifiche
015300011105     C                   EXSR      CtrD2
015400011106      *
015500011106      * Non ci sono errori e ho chiesto conferma/annullamentoripristino
015600011106     c     *in25         ifeq      *off
015700011106     c     *inKf         ifeq      *on
015800011106     c     *inKe         oreq      *on
015900011106     c     *inKq         oreq      *on
016000011105      *
016100011106      * Eseguo aggiornamento
016200011105     C                   EXSR      AggTnt
016300011105     C                   endif
016400011106     C                   endif
016500011108      *
016600011105     C                   endsl
016700011105     C                   endsr
016800011108      *--------------------------------------------------------------------------------------------*
016900011108      * Search - Ricerca su campi video
017000011108      *--------------------------------------------------------------------------------------------*
017100010925     C     Search        BEGSR
017200011002      *
017300011106     C                   setoff                                       2540
017400010928      *
017401090608     c                   Clear                   tibs02ds
017403090608     c                   Eval      T02Mod = 'R'
017404090608     c                   Eval      T02Sif = Knsif
017405090608     c                   Eval      T02Cod = 'LTS'
017406090608     c                   Call      'TIBS02R'
017407090608     c                   Parm                    Kpjba
017408090608     c                   Parm                    tibs02ds
017409090608     c                   Movel     T02Ke1        v1tsp
017410090608      *
017500090519     C                   move      *blanks       V1tspD
017700090519     c                   move      '5E'          cod
017701090608     c                   movel(p)  v1tsp         §key
017702090608     c     ktab          chain     tabel00f
017703090608     c                   if        %found(tabel00f) and tblflg=' '
017704090608     C                   movel     tbluni        ds5e
017705090608     c                   movel     §5edes        v1tspd
017706090608     c                   endif
017707090608     c**                 exsr      cx§tab
018400090608     C**                 movel     §key          V1tsp
018500090608     C**                 movel     §des          V1tspD
018511090608     c
018700010928      *
018800010925     C                   ENDSR
018900011108      *--------------------------------------------------------------------------------------------*
019000011108      * F03Ges - Tasto funzionale F03 -> Fine programma
019100011108      *--------------------------------------------------------------------------------------------*
019200011106     C     F03Ges        BEGSR
019300011108      *
019400011106     C                   movel     'S'           $Fine                          *fine pgm
019500011108      *
019600010925     C                   ENDSR
019700011108      *--------------------------------------------------------------------------------------------*
019800011108      * F12Ges - Tasto funzionale f12 -> Ritorno
019900011108      *--------------------------------------------------------------------------------------------*
020000011106     C     F12Ges        BEGSR
020100011108      *
020200011106     C                   eval      TipVid = '1'
020300011108      *
020400010925     C                   ENDSR
020500011108      *--------------------------------------------------------------------------------------------*
020600011108      * CarVd1 - Carica primo video
020700011108      *--------------------------------------------------------------------------------------------*
020800011106     C     CarVd1        BEGSR
020900011108      *
021000011106     c                   setoff                                       25
021100011106     c                   setoff                                       010204
021200090519     C                   clear                   Tb73V01
021300090519     C                   move      '  ?'         V1tsp
021400011106      *
021500000000     C                   ENDSR
021600011108      *--------------------------------------------------------------------------------------------*
021700011108      * CarVd2 - Carica secondo video
021800011108      *--------------------------------------------------------------------------------------------*
021900011106     C     CarVd2        BEGSR
022000011106     C                   setoff                                       010204
022100011106      *
022200011106      * Aggancio la tabella, se trovo il codice sono in modifica
022300011108      * o ripristino (se record annullato), altrimenti in immissione
022400090519     c                   movel     V1tsp         CodTab            3
022500011106     c                   EXSR      ChnTnt
022600011106      *
022700011106      * MODIFICA/RIPRISTINO
022800011106     c  n21              do
022900011106     c                   seton                                        02
023000090519     c                   movel     Tbeuni        Dlts
023100090519     c                   eval      V2tsp =V1tsp
023200090519     c                   eval      V2tspD=v1tspd
023501090519     c                   z-add     §LTSLCLM      v2lclm
023502090519     c                   z-add     §LTSLCLF      v2lclf
023503090519     c                   z-add     §LTSLPKM      v2lpkm
023504090519     c                   z-add     §LTSLPKF      v2lpkf
023505090519     c                   z-add     §LTSLMCM      v2lmcm
023506090519     c                   z-add     §LTSLMCF      v2lmcf
023600011106      *
023700011106     C     TbeAtb        ifeq      *blanks
023800011106     C                   setoff                                       04
023900011106     C                   eval      V2PzD = Opz(2)
024000011106     C                   else
024100011106     C                   seton                                        04
024200011106     C                   eval      V2PzD = Opz(3)
024300011106     C                   endif
024400011106     c                   enddo
024500011106      *
024600011106      * IMMISSIONE
024700011106     c   21              do
024900090519     c                   clear                   Tb73V02
025000090519     c                   movel     V1tsp         V2tsp
025001090519     c                   movel     V1tspd        V2tspd
025100011106     c                   seton                                        01
025200011106     C                   eval      V2PzD = Opz(1)
025300011106     c                   enddo
025400011106     c
025500011105     C                   ENDSR
025600011108      ************************************************************
025700011108      * Controllo e decodifica prima videata
025800011108      ************************************************************
025900010925     C     CtrD1         BEGSR
026000011106      *
026100011106     C                   setoff                                       2540
026200011106     c                   clear                   V1Msg
026300011106      *
026400011106      * Codice tabella obbligatorio
026500090519     c     V1tsp         ifeq      *blanks
026600090519     c                   seton                                        2540
026700011106     c                   eval      V1Msg = Msg(1)
026800011106      *
026900011106      * Se esiste decodifico
027000011106     c                   else
027001090519     c                   move      '5E'          cod
027002090519     c                   movel(p)  v1tsp         §key
027003090519     c     ktab          chain     tabel00f
027004090519     c                   if        %found(tabel00f) and tblflg=' '
028000090519     C                   movel     tbluni        ds5e
028001090519     c                   movel     §5edes        v1tspd
028002090519     c                   else
028003090519     c                   seton                                        2540
028004090519     c                   eval      V1Msg = Msg(1)
028200011106     c                   endif
028201090519     c                   endif
028300010925      *
028400010925     C                   ENDSR
028500011108      ************************************************************
028600011108      * Controllo e decodifica seconda videata
028700011108      ************************************************************
028800011105     C     CtrD2         BEGSR
028900090519     C                   setoff                                       2532
029000090519     C                   setoff                                       3334
029001090519     C                   setoff                                       3531
029100011106     C                   clear                   V2Msg
029200011106     C
029300011106      *
029400011106      * Controlli da effettuarsi se record non annullato
0295000111061a   C     *In04         ifeq      *off
029600011106      *
029601090519      * COLLI
029700090610      * Controllo che il numero colli  forz. non sia maggiore del max
0298000905192a   C     V2lclf        ifgt      V2lclm
029900090519     C                   seton                                        2531
030000011108     C                   eval      V2Msg = Msg(4)
0301000111062-   C                   endif
030101090519      * PESO KG
030102090519     c* controllo
030103090610      * Controllo che il peso  forz. non sia maggiore del max
0301040905192a   C     V2lpkf        ifgt      V2lpkm
030105090519     C                   seton                                        2532
030106090519     C                   eval      V2Msg = Msg(4)
0301070905192-   C                   endif
030108090519      * VOLUME Mc
030109090610      * Controllo che il volume forz. non sia maggiore del max
0301100905192a   C     V2lmcf        ifgt      V2lmcm
030111090519     C                   seton                                        2534
030112090519     C                   eval      V2Msg = Msg(4)
0301130905192-   C                   endif
032200011106      *
0323000111061-   C                   endif
032400011105      *
032500011105     C                   ENDSR
032600011108      ************************************************************
032700090519      * Aggiornamento tabella LTS
032800011108      ************************************************************
032900010925     C     AggTnt        BEGSR
033000010926      *
033100011105     C                   SELECT
033200011106      * F06=Conferma
033300011106     C     *InKf         wheneq    *on
033400011106      *
033500011105      * Immissione
033600011108     C     *in01         ifeq      *on
033700011105     C                   EXSR      RieTnt
033800011106     C                   move      *zeros        TbeFlt
033900011107     C                   move      'S'           TbeFtt
034000011108     c                   move      *zeros        Tbedtr
034100011105     C                   WRITE     tntbe000                             22
034200011106     C                   endif
034300011105      * Modifica
034400011106     C     *in02         Ifeq      *on
034500011106     C                   EXSR      RieTnt
034600011106     C                   UPDATE    tntbe000
034700011106     C                   endif
034800011105      *
034900011106      * F16=Annullamento
035000011105     C     *inkq         wheneq    *on
035100011106     C     *in04         andeq     *off
035200011105     C                   move      'A'           tbeatb
035300011107     C                   move      *blanks       tbeftr
035400011106     C                   UPDATE    tntbe000
035500011105      *
035600011106      * F5=Ripristino
035700011105     C     *inke         wheneq    *on
035800011106     C     *in04         andeq     *on
035900011105     C                   move      ' '           tbeatb
036000011107     C                   move      *blanks       tbeftr
036100011106     C                   UPDATE    tntbe000
036200011105      *
036300011105     C                   ENDSL
036400011108      * Torno alla prima videata che carico come da inizio pgm
036500011106     C                   move      '1'           TipVid
036600011106     C                   move      *on           $CarVd1
036700011106     C
036800010925     C                   ENDSR
036900011108      ************************************************************
037000011108      * Riempimento file tabella
037100011108      ************************************************************
037200010925     C     RieTnt        BEGSR
037300011106      *
037400011106      * Codice tabella
037500011106     c                   movel     *blanks       TbeAtb
037600011106     c                   move      Sif           TbeSif
037700011106     c                   move      'GE'          TbeApl
037800090519     c                   movel     'LTS'         TbeCod
037900090519     c                   movel     V2tsp         TbeKe1
038000011106     c                   movel     *blanks       TbeKe2
038100011105      *
038200090519     c                   clear                   Dlts
038400090519     c                   z-add     V2Lclm        §LTSLCLM
038500090519     c                   z-add     V2Lclf        §LTSLCLF
038600090519     c                   z-add     V2Lpkm        §LTSLPKM
038601090519     c                   z-add     V2Lpkf        §LTSLPKF
038602090519     c                   z-add     V2Lmcm        §LTSLMCM
038603090519     c                   z-add     V2Lmcf        §LTSLMCF
038700090519     c                   movel     Dlts          TbeUni
038800011106      *
038900011106     c                   move      *blanks       TbeFtr
039000010925     C
039100010925     C                   ENDSR
039200011108      *--------------------------------------------------------------------------------------------*
039300011108      * Aggancio tabella
039400011108      *--------------------------------------------------------------------------------------------*
039500011106     c     ChnTnt        BEGSR
039600090519     c                   move      'LTS'         TbeCod
039700011106     c                   clear                   TbeKe1
039800011106     c                   movel     CodTab        TbeKe1
039900011106     c                   move      *blanks       TbeKe2
040000011106     c                   move      *blanks       TbeLin
040100011106     c                   movel     Knsif         TbeSif
040200011106     c     k01tbe5       chain     tntbe01l                           21
040300011106      * Se non ho trovato il record con il sistema informativo
040400011106      * che ho in linea, lo abblenco
040500011106     c   21              do
040600011106     c                   eval      TbeSif= *blanks
040700011106     c                   eval      Sif= *blanks
040800011106     c     k01tbe5       chain     tntbe01l                           21
040900011106     c  N21              eval      Sif= *blanks
041000011106     c                   enddo
041100011108      *
041200011106     c                   ENDSR
041201090519     c
041202090519     C* CALL AL PGM DI RICERCA TABELLE X§TABER -----------------------*
041203090519     C     CX§TAB        BEGSR
041204090519     C                   CLEAR                   §KEY
041205090519     C                   CALL      'X§TABER'
041206090520     C                   PARM                    CODUT
041207090519     C                   PARM                    COD
041208090519     C                   PARM                    §KEY
041209090519     C                   PARM                    §DES             25
041210090519     C                   ENDSR
041300011108      *--------------------------------------------------------------------------------------------*
041400011108      * RutInz - operazioni iniziali
041500011108      *--------------------------------------------------------------------------------------------*
041600011108     C     RutInz        BEGSR
041700011108      *
041800011108      * Ricevimento parametri
041900000000     C     *ENTRY        PLIST
042000000000     C                   PARM                    kpjba
042100011108      *
042200011108      * Chiavi di lettura
042300010925     c     K01tbe5       KLIST                                                  *tntbe01l
042400010925     c                   KFLD                    tbecod                          -tabella
042500010925     c                   KFLD                    tbeke1                          -chiave uno
042600010925     c                   KFLD                    tbeke2                          -chiave due
042700010925     c                   KFLD                    tbelin                          -lingua
042800010925     c                   KFLD                    tbesif                          -s.informativo
042801090519     C     Ktab          klist
042802090519     C                   kfld                    codut
042803090519     C                   kfld                    cod
042804090519     C                   kfld                    §key
042900010925      *
043000011108      * Reperisce dati utente
043100020715
043200020715     c     *dtaara       define    §azute        azuteds
043300020715     c     *dtaara       define    §datiute      ddatiute
043400020715
043500020715     c                   in(E)     *dtaara
043600020715     c                   if        %Error or rsut = *blanks
043700020715     c                   clear                   tibs34ds
043800020715     c                   call      'TIBS34R'
043900020715     c                   parm                    Tibs34Ds
044000020715     c                   in        *dtaara
044100020715     c                   endif
044200011106      *
044300011106      * Sistema informativo
044400011106     C                   movel     knsif         Sif              10
044500011106     C
044600000000     C                   ENDSR
044700000000     O*--------------------------------------------------------------------------------------------*
044800010926** -OPZ-
044900010926  Inserimento
045000010926   Modifica
045100010926  Annullato
045200000000** -MSG-
045300090519TNTB73R- Tipo Servizo mancante o errato
045400090519TNTB73R-
045500090519TNTB73R-
045600090610TNTB73R- Il valore massimo non può essere inferiore del valore forzabile
