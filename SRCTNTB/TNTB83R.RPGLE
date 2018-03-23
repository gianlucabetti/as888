000100011108      *--------------------------------------------------------------------------------------------*
000200110105      * Gestione tabella tipi unità EDP   "UNT"
000300011108      *--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500011108      *--------------------------------------------------------------------------------------------*
000600011108      * Data base                                                                                  *
000700011108      *--------------------------------------------------------------------------------------------*
000800110105     FTNTB83D   CF   E             WORKSTN
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
002000110105     D DUNT          E DS                                                       *reperimento dati
002100011108      *---
002200011106     D Tibs02Ds      E DS
002300011106      *----------
002400011108      * Variabili
002500010925      *----------
002600011108     D $Fine           S              1    INZ('N')                             *fine programma
002700011108     D TipVid          S              1    INZ('1')                             *video in gestione
002800011108     D $CarVd1         S              1    INZ('1')                             *video in gestione
002900011108     D $CarVd2         S              1    INZ('0')                             *video in gestione
003000011108     D Win             S             99A
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
006000011108      * GesVd1 - Gestione videata selezione codice tabella -
006100011108      *--------------------------------------------------------------------------------------------*
006200011106     C     GesVd1        BEGSR
006300010925      *
006400011108      * Inizializzazione videata
006500011106     C     $CarVd1       ifeq      *on
006600011106     C                   EXSR      CarVd1
006700011106     C                   move      *off          $CarVd1
006800010925     C                   endif
006900011106      * Scrivo la testata
007000110105     C                   WRITE     Tb83T01
008200011106      *
008300110105     C                   EXFMT     Tb83V01
008301110105     c                   setoff                                       2890
008400011002      *
008500110105     c     '?'           scan      V1cunt                                 50
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
009900110105     C     *IN90         ifeq      *off
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
012100110105     C                   WRITE     Tb83T01
013300011105      *
013400011106     C     *in04         ifeq      *on
013500110105     C                   WRITE     Tb83V02
013600011106     C                   EXFMT     PROTECT
013700011106      *
013800011106     C                   else
013900110105     C                   EXFMT     Tb83V02
014000011106     C                   endif
014001110105
014002110105     c                   setoff                                       289032
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
015600110105     c     *in90         ifeq      *off
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
017300110105     C                   setoff                                       9028
017400010928      *
017500110105     C                   move      *blanks       V1dunt
017600011106     C                   move      *blanks       Tibs02Ds
017700110105     c                   move      'UNT'         T02Cod
017800011106     c                   move      'R'           T02Mod
017900011106     c                   movel     KnSif         T02Sif
018000011105     C                   CALL      'TIBS02R'
018100011106     C                   PARM                    kpjba
018200011106     C                   PARM                    Tibs02Ds
018300011106     C                   if        T02Err =  *blanks
018400110105     C                   movel     T02ke1        V1cunt
018500110105     C                   movel     T02uni        V1dunt
018600011106     c                   endif
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
021000110105     c                   setoff                                       2890
021100011106     c                   setoff                                       010204
021200110105     C                   clear                   v2pzd
021201110105     C                   clear                   Tb83V01
021300110105     C                   move      '  ?'         V1cunt
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
022400110105     c                   movel     V1cunt        CodTab            3
022500011106     c                   EXSR      ChnTnt
022600011106      *
022700011106      * MODIFICA/RIPRISTINO
022800011106     c  n21              do
022900011106     c                   seton                                        02
022901110105     c                   movel     V1cunt        V2cunt
023000110105     c                   movel     Tbeuni        DUNT
023300110105     c                   movel     d§untdes      V1dunt
023400110105     c                   movel     d§untfl1      V2cfl1
023401110105     c                   movel     d§untoimpd    V2coimpd
023402110105     c                   z-add     d§untimpd     V2cimpd
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
024800110105     c                   movel     *blanks       v1dunt
024900110105     c                   clear                   Tb83V02
025000110105     c                   movel     V1cunt        V2cunt
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
026100110105     C                   setoff                                       2890
026200110105     c                   clear                   V1cMsg
026300011106      *
026400110105      * Codice tipo unità obbligatorio
026500110105     c                   if        v1cunt=*blanks or v1cunt=*zeros
026600110105     c                   seton                                        2890
026700110105     c                   eval      V1cMsg = Msg(1)
026800011106      *
026900011106      * Se esiste decodifico
027000011106     c                   else
027100011106     C                   clear                   tibs02ds
027200011106     C                   movel     'C'           t02mod
027300011106     C                   movel     Sif           t02sif
027400110105     C                   movel     'UNT'         t02cod
027500110105     C                   movel     V1cunt        t02ke1
027600011106     C                   call      'TIBS02R'
027700011106     C                   parm                    KPJBA
027800011106     C                   parm                    TIBS02DS
027900011106     C                   if        t02err =  *blanks
028000110105     C                   movel     t02uni        V1dunt
028100011106     c                   endif
028200011106     c                   endif
028300010925      *
028400010925     C                   ENDSR
028500011108      ************************************************************
028600011108      * Controllo e decodifica seconda videata
028700011108      ************************************************************
028800011105     C     CtrD2         BEGSR
028900110105     C                   setoff                                       2890
029000110105     C                   setoff                                       32
029100110105     C                   clear                   V1cMsg
029200011106     C
029300011106      *
029400011106      * Controlli da effettuarsi se record non annullato
0295000111061a   C     *In04         ifeq      *off
031600011106      *
031700011106      * Descrizione non inserita
0318001101052a   C     V1dunt        ifeq      *blanks
031900110105     C                   seton                                        902832
032000110105     C                   eval      V1cmsg = Msg(2)
0321000111062-   C                   endif
032200011106      *
0323000111061-   C                   endif
032400011105      *
032500011105     C                   ENDSR
032600011108      ************************************************************
032700011108      * Aggiornamento tabella LIA
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
037700110105     c                   move      'UN'          TbeApl
037800110105     c                   movel     'UNT'         TbeCod
037900110105     c                   movel     V2cunt        TbeKe1
038000011106     c                   movel     *blanks       TbeKe2
038100011105      *
038200110105     c                   clear                   DUNT
038300110105     c                   movel     V1dunt        d§untdes
038400110105     c                   movel     V2cfl1        d§untfl1
038500110105     c                   movel     V2coimpd      d§untoimpd
038600110105     c                   z-add     V2cimpd       d§untimpd
038700110105     c                   movel     Dunt          TbeUni
038800011106      *
038900011106     c                   move      *blanks       TbeFtr
039000010925     C
039100010925     C                   ENDSR
039200011108      *--------------------------------------------------------------------------------------------*
039300011108      * Aggancio tabella
039400011108      *--------------------------------------------------------------------------------------------*
039500011106     c     ChnTnt        BEGSR
039600110105     c                   move      'UNT'         TbeCod
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
045300110105Immettere il tipo unità
045400110105Descrizione obbligatoria
045500110105
045600110105
