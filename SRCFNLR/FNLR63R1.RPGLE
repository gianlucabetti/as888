000100000810     H DECEDIT('0,') DATEDIT(*DMY.)
000200950313      *---------*----------------------------------------------------*
000300000728      * FNLR63R1*                                                    *
000400950313      *---------*                                                    *
000500950313      *         - GESTIONE CHIUSURA DISTINTA ESTERO -                *
000600950315      *           -- DATI DISTINTA + CARICAMENTO --                  *
000700950313      *--------------------------------------------------------------*
000800950315     FFNLR63D   CF   E             WORKSTN
000900950317     F                                     SFILE(LR63S05:NRR5)
001000950315     FFNLR53D   O    E             WORKSTN
001100950315     F                                     SFILE(LR53S02:NRR2)
001200950315     F                                     SFILE(LR53S03:NRR3)
001300020902     FFNCDE01L  UF   E           K DISK    USROPN
001400020902     F                                     COMMIT
001500950315     FFNARB01L  UF   E           K DISK    USROPN
001600950313     F                                     COMMIT
001700990519     FFNANM01L  IF   E           K DISK
001800990519     F                                     INFDS(FNANM1)
001900990923     FFIARI01L  IF   E           K DISK
002000990923     FFIAR601L  IF   E           K DISK
002100051115     FFIAR901L  IF   E           K DISK    USROPN
002200950313     F                                     COMMIT
002300950313     FFNART01L  IF   E           K DISK    USROPN
002400950313     F                                     COMMIT
002500950320     FFNFVV01L  UF A E           K DISK    USROPN
002600950313     F                                     COMMIT
002700070515     FFidst01L  UF A E           K DISK    USROPN
002800070515     F                                     COMMIT
002900021204     FFIAPD01L  IF   E           K DISK
003000050406     Ftigcp21L  IF   E           K DISK
003100950606     FFNLBL02L  IF   E           K DISK
003200961114     FFNLBL01L  IF   E           K DISK
003300961114     F                                     RENAME(FNLBL000:FNLBL001)
003400950315     FTABEL00F  IF   E           K DISK
003500980323      *
003600950315     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003700980323      *
003800980323     FFNCDS03L  UF   E           K DISK    USROPN
003900980323     F                                     COMMIT
004000980323     F                                     RENAME(FNCDS000:FNCDS003)
004100991012     FTNTBE01L  IF   E           K DISK
004200110418     FFiar501l  IF   E           K DISK
004300950313      *----------------------------------------------------------------*
004400950313      *    s c h i e r e                                               *
004500950313      *----------------------------------------------------------------*
004600950314     D G15             S              8  0 DIM(5)                               DATA DISTINTA - 15 G
004700950314     D D3A             S             71    DIM(100)                             SKI TAB. 3A
004800950314     D C3A             S              2    DIM(100)                             SKI CHIAVI 3A
004900990923     D D1A             S             61    DIM(100)                             SKI TAB. 1A
005000990923     D C1A             S              2    DIM(100)                             SKI CHIAVI 1A
005100991012     D D7R             S             48    DIM(100)                             SKI TAB. 7R
005200991012     D C7R             S              2    DIM(100)                             SKI CHIAVI 7R
005300001019     D ERR             S             70    DIM(26) CTDATA PERRCD(1)             ERRORI
005400950315      *----------------------------------------------------------------
005500950315      *  DS
005600950315      *----------------------------------------------------------------
005700950313     D FNLR63        E DS                  EXTNAME(FNLR63DS)
005800950313      *
005900950313     D FNLR53        E DS                  EXTNAME(FNLR53DS)
006000950314     D  NDC                    1     35  0
006100950314     D                                     DIM(5)                               NUM.DISTINTA
006200950314     D  DTD                   36     75  0
006300950314     D                                     DIM(5)                               DATA DISTINTA
006400950314     D  DDC                   76    115  0
006500950314     D                                     DIM(5)                               DATA CHIUSURA
006600950314     D  HRC                  116    135  0
006700950314     D                                     DIM(5)                               ORA CHIUSURA
006800950314     D  FPP                  136    140
006900950314     D                                     DIM(5)                               TIPO USCITA
007000950314     D  PEP                  141    145
007100950314     D                                     DIM(5)                               PICKING S/N
007200950314     D  SET                  146    180  0
007300950314     D                                     DIM(5)                               ORA CHIUSURA
007400030204     D  CAR                  247    251
007500030204     D                                     DIM(5)                               Carico S/N
007600950314      *
007700000222     D Wdpd            s              1
007800020902     D dataiso         s               d
007900000222      *
008000950313     D KPJBA         E DS
008100950313     D  LIBSYS                92    100
008200950313     D  MSGERR               453    502
008300030718     D trul90ds      E DS                  inz
008400950313      *
008500110418     D dar5gen       E DS
008600110418     d TRULINTds     e ds                  inz
008700110418     d   iINTtip     e                     inz('I')
008800020121     D OG143         E DS
008900070515     D OG146         E DS
009000950315     D DS7R          E DS
009100950315     D DS3A          E DS
009200990923     D DS1A          E DS
009300991012     D DSCV          E DS
009400950313     D DS4G          E DS
009500991012     D DGED          E DS
009600051215     D fnlv80ds      e ds
009700021204     D fnlv24ds      e ds
009800021204     D fnlv55ds      e ds
009900020830     D ddatiute      e ds
010000020828     D azuteds       e ds                  extname(AZUTE00F)
010100020828     D tibs34ds      E DS                  inz
010200961114     D DSLR33        E DS                  EXTNAME(FNLR33DS)
010300991012     D YEURDS        E DS                  EXTNAME(YEURCODS)
010400990519     D FNANM1          DS
010500961114     D  ANMNR1               397    400B 0
010600950315      *
010700950315      *  DS per calcolo data da nr. giorni
010800950315     D WLBGIO          DS
010900950315     D  GIODAT                 1      8  0
011000950315     D  GIOINV                 9     16  0
011100950315     D  GIOTGI                17     21  0
011200950313      *  DS per inverisone data
011300950313     D WLBDA8          DS
011400950313     D  G02DAT                 1      8  0
011500950313     D  G02INV                 9     16  0
011600950313     D  G02ERR                17     17
011700950313     D  G02TGI                18     22  0
011800950314     D                 DS
011900950314     D  V2CHCO                 1      2  0
012000950314     D  V2CMCO                 3      4  0
012100950314     D  V2CHMC                 1      4  0
012200950314     D                 DS
012300950315     D  V2CFPD                 1      3  0
012400950315     D  V2CCPC                 4      7
012500950315     D  WCDCPC                 1      7
012600950315      * X  CHKOBJ FNFTDS0T
012700950315     D                 DS
012800950315     D  WFILC                 43     45  0
012900950315     D  WCMDC                  1     60
013000950315      * X  ALCOBJ FNFTDS0T
013100950315     D                 DS
013200950315     D  WFILA                 37     39  0
013300950315     D  WCMDA                  1     60
013400950315     D                SDS
013500950315     D  NOMPGM                 1     10
013600030318      *----------------------------------------------------------------
013700030318     D  Msg_alert1     c                   Const('Ricevuti più di 999 POD da el-
013800030318     D                                     aborare. <Proseguire -> ')
013900030318     D  Msg_alert2     c                   Const('ed in seguito effettuare una -
014000030318     D                                     ulteriore chiusura.>    ')
014100030318     D  Riesegui       c                   Const('Ci sono altre spediz.da chiud-
014200030318     D                                     ere: rieseguire il pgm')
014300950313      *----------------------------------------------------------------
014400950315      *  Definzione interna record AZORG
014500950315      *----------------------------------------------------------------
014600950315     IAZORG01L  AA
014700950315     I                             P    4    5 0ORGFIL
014800950315     I                                  6    6  ORGFVA
014900950315     I                                  7    7  ORGFAG
015000950315     I                                 14   33  ORGDES
015100000222     I                               3776 3800  ORGDE3
015200070515     i                               3851 3875  ORGDe6
015300950313      *---------------------------------------------------------------*
015400950313      * FLUSSO PRINCIPALE                                             *
015500950313      *---------------------------------------------------------------*
015600950313      *  Operazioni iniziali
015700950313     C     *ENTRY        PLIST
015800950313     C                   PARM                    KPJBA
015900950313     C                   MOVEL     KPJBU         FNLR63
016000020830     c* controllo se il po che mi arriva è gestito da qualcun'altro
016100020830     c                   exsr      srfgs
016200020830     c*
016300020830     C                   Z-ADD     D55tfa        KFGS
016400950322     C                   MOVEL     ' '           WFINE
016500950313      *  Se pgm richiamato per l'ultima volta chiudo in LR
016600950313     C     D63TRI        IFNE      'U'
016700950313      *  Impostazione campi distinta
016800950313     C                   EXSR      INZD02
016900950313      *  Loop gestione videate
017000950313     C     WFINE         DOUEQ     'S'
017100950313     C     WTPVID        CASEQ     '2'           GESD02
017200950313     C                   END
017300950313     C                   END
017400950313     C*
017500950313     C     FINE          TAG
017600950317     C                   MOVEL     FNLR63        KPJBU
017700950313     C                   SETON                                        RT
017800950313     C*
017900950313     C                   ELSE
018000950320     C     WCHIUS        IFEQ      'S'
018100950315     C                   EXSR      CALL53
018200950320     C                   END
018300950320     C     WSBLOC        IFEQ      'S'
018400950320     C                   MOVEL     FNLR63        KPJBU
018500950320     C                   CALL      'FNLR63R3'
018600950320     C                   PARM                    KPJBA
018700950320     C                   END
018800950426     C     WPGMER        IFEQ      'S'
018900950320     C                   MOVEL     FNLR63        KPJBU
019000950320     C                   CALL      'FNLR63R2'
019100950320     C                   PARM                    KPJBA
019200950320     C                   END
019300950320     C*
019400950320     C                   COMMIT
019500950313     C                   SETON                                        LR
019600950313     C                   END
019700950313      *---------------------------------------------------------*
019800950313      *  Inizializzo campi della prima videata                  *
019900950313      *---------------------------------------------------------*
020000950313     C     INZD02        BEGSR
020100950313      *
020200950313     C                   SETOFF                                       404142
020300950313     C                   SETOFF                                       2801
020400020904     C                   Z-ADD     D55tfa        V2CLNA
020500950313     C                   MOVEL     D63PDR        V2CFPD
020600950315     C                   MOVE      D63PDR        WCDCPC
020700950313     C                   Z-ADD     WOGGI         V2CDDC
020800950313     C                   Z-ADD     D63HMC        V2CHMC
020900950313     C                   MOVEL     D63FPP        V2CFPP
021000950313     C                   MOVEL     D63PEP        V2CPEP
021100950315     C                   MOVEL     D63NDT        V2CNDC
021200000426     C*--------------------------------------------------------------*
021300950313     C*  Controllo se ci sono record di FNCDE con il numero distinta
021400020830     C*  sia per il po richiesto che per il po di gestione:
021500000426     C*  in tal caso vuole dire che c'è stata una precedente elaboraz.
021600000426     C*  di chiusura distinte che si è interrotta quindi per poter
021700000426     C*  proseguire devo eseguire lo sblocco. (la chiusura distinta
021800020830     C*  estero prima imposta su FNCDE il nr. distinta poi una
021900000426     C*  volta confermati i dati e aggiornato FNARB cancella i record
022000000426     C*  di FNCDE aventi il nr. distinta = a quello del documento
022100020830     C*  appena confermato. Se viene richiesto di uscire
022200000426     C*  dal giro prima di aver completato la chiusura un apposito
022300000426     C*  pgm provvede a togliere da FNCDE i dati della distinta.)
022400000426     C*--------------------------------------------------------------*
022500020830     C                   Z-ADD     0             KNDC
022600020830     C                   Z-ADD     D63LNA        KLNA
022700950315     C     KCDE          SETGT     FNCDE01L
022800020830     C     klna          READE     FNCDE01L                               31
022900000426      * Se ce ne sono ...
023000950313     C     *IN31         IFEQ      '0'
023100000426      * ... libero CDE e
023200950320     C                   EXCEPT    NOAGG
023300000426      * ... accendo indicatore msg errore che avvisa di eseguire lo sblocco
023400950313     C                   SETON                                            01
023500950313     C                   END
023600950313     C*
023700950313     C                   ENDSR
023800950313      *---------------------------------------------------------*
023900950313      *  Gestione della seconda videata                         *
024000950313      *---------------------------------------------------------*
024100950313     C     GESD02        BEGSR
024200950313      *
024300001019     C*  Controllo numero righe fncde se 9999 mando messaggio
024400001019     C     d63nrr        IFEQ      9999
024500001019     C                   SETON                                        28
024600001019     C                   MOVEL     ERR(26)       $MSG
024700001019     C                   END
024800001019      *
024900950313     C                   EXFMT     LR63D02
025000950320     C                   SETOFF                                       404142
025100950320     C                   SETOFF                                       28
025200950313     C*  Fine Lavoro
025300950313     C     *INKC         IFEQ      '1'
025400950313     C                   MOVEL     '1'           D63F03
025500950313     C                   MOVEL     'S'           WFINE
025600950313     C                   GOTO      FINVD2
025700950313     C                   END
025800950313     C*  Ritorno
025900950313     C     *INKL         IFEQ      '1'
026000950320     C                   MOVEL     'S'           WFINE
026100950320     C                   GOTO      FINVD2
026200950313     C                   END
026300950313     C*  Sblocco
026400950313     C     *INKK         IFEQ      '1'
026500000426      * se richiesto richiamo pgm che va a togliere il nr. distinta
026600000426      * da FNCDE. Prima di richiedere lo sblocco bisogna appurare
026700000426      * che non ci sia un altro utente che sta chiudendo distinte
026800000426      * per la stessa lna
026900950317     C                   MOVEL     FNLR63        KPJBU
027000950313     C                   CALL      'FNLR63R3'
027100950313     C                   PARM                    KPJBA
027200950320     C                   MOVEL     KPJBU         FNLR63
027300950320     C                   MOVEL     'S'           WSBLOC
027400950317     C                   SETOFF                                       01
027500950313     C                   GOTO      FINVD2
027600950313     C                   END
027700950313     C*  Controlli
027800950313     C                   EXSR      CTR02
027900950313     C*
028000950313     C  N01*IN28         IFEQ      '0'
028100950314     C*  Esamino record di FNCDE per impostare nr.distinta
028200950314     C                   EXSR      UPDCDE
028300950314     C*
028400950313     C     WERROR        IFEQ      'S'
028500950313     C*  Se ho trovato degli errori chiamo pgm x la loro gestione
028600950313     C                   EXSR      GESERR
028700950313     C                   END
028800950314     C*  Carico dati per ditinta di consegna
028900950315     C     WFINE         IFNE      'S'
029000950320     C     D63NRR        IFEQ      0
029100950320     C                   MOVEL     'S'           WFINE
029200950320     C                   ELSE
029300950314     C                   EXSR      CARDIS
029400950313     C*  Richiamo pgm FNLR53R
029500961230     C     NRR2          IFNE      0
029600950313     C                   EXSR      CALL53
029700030319      *-
029800030319      * se aveva più di 999 P.O.D. deve uscire e rientrare per chiudere
029900030319      * una nuova distinta con i records residui.
030000030319      * Altrimenti tornerebbe sulla videata della distinta appena elaborata.
030100030319     c                   if        conta_cde = 999
030200030319     C                   movel     'S'           WFINE
030300030319     C                   goto      FINVD2
030400030319     c                   endiF
030500030319      *-
030600961230     C                   ELSE
030700961230     C                   SETON                                        28
030800961230     C                   MOVEL     ERR(23)       $MSG
030900961230     C                   END
031000950320     C                   END
031100950313     C                   END
031200950315     C*
031300950315     C                   END
031400950313     C*
031500950315     C     FINVD2        ENDSR
031600950314     C*-----------------------------------------------------*
031700950314     C*  Controlli su seconda videata
031800950314     C*-----------------------------------------------------*
031900950314     C     CTR02         BEGSR
032000000222      *
032100000222      * Verifico se LNA è DPD
032200021203     C     d63lna        CHAIN     azorg01l
032300020121      *
032400020121      * imposta i campi della DS
032500020121     c                   If        %found
032600020121     C                   movel     ORGDE3        OG143
032700070515     C                   movel     ORGDE6        OG146
032800020121     c                   endif
032900020121      *
033000020717      *  sostituito utilizzando il campo del network
033100020717     c                   EVAl      WDPD = *blank
033200020717     c                   if        §ogNTW = 'DPD'
033300020717     c                   EVAl      WDPD = 'S'
033400020717     c                   end
033500000222      *
033600000222      *   Controllo se richiesta ricerca codice padroncino
033700950314     C                   SETOFF                                           32
033800950315     C     '?'           SCAN      V2CCPC                                 32
033900950314     C   32              DO
034000021204     C                   CLEAR                   FNLV24DS
034100021204     C                   MOVEL     'R'           D24FLG
034200021204     C                   MOVEL     'A'           D24TIP
034300021204     C                   MOVEL     V2Cfpd        D24FIL
034400950315     C                   MOVE      *BLANKS       V2CCPC
034500021204     C                   MOVEL(P)  FNLV24DS      KPJBU
034600950314     C                   CALL      'FNLV24R'
034700950314     C                   PARM                    KPJBA
034800021204     C                   MOVEL     KPJBU         FNLV24DS
034900021204     C     D24PDR        IFNE      *ZEROS
035000021204     C                   MOVEL     D24PDR        WCDCPC
035100950314     C                   END
035200950315     C                   END
035300950314     C* Controllo se esiste nell'anagrafica padroncini
035400950314     C                   TESTN                   WCDCPC               32
035500950317     C     *IN32         IFEQ      '0'
035600950314     C                   SETON                                        2841
035700950314     C                   MOVEL     ERR(1)        $MSG
035800950314     C                   GOTO      FINCT2
035900950314     C                   END
036000950314     C*
036100950314     C                   MOVE      WCDCPC        APDPDR
036200021204     C     KAPD          CHAIN     FIAPD01L                           31
036300950314     C     *IN31         IFEQ      '1'
036400950314     C                   SETON                                        2841
036500950314     C                   MOVEL     ERR(2)        $MSG
036600950314     C                   GOTO      FINCT2
036700950314     C                   END
036800950316     C*
036900950316     C                   MOVE      'N'           D53WRT
037000971104     C     APDPDD        IFNE      'S'
037100971104     C*   e padroncino non è dipedente controllo se in filiale
037200971104     C*  di appartenza è attivata la gestione conteggi
037300971104     C                   MOVEL     APDPDR        ORGFIL
037400971104     C     ORGFIL        CHAIN     AZORG01L                           31
037500971104     C     *IN31         IFEQ      '0'
037600971104     C                   MOVE      'S'           D53WRT
037700971104     C                   END
037800971104     C                   END
037900971104     C*----------------------------------------------------*
038000971104     C*  Data distinta
038100971104     C     V2CDDC        IFEQ      0
038200971104     C                   SETON                                        2840
038300950314     C                   MOVEL     ERR(3)        $MSG
038400950314     C                   GOTO      FINCT2
038500950314     C                   END
038600950314     C*
038700950314     C                   CLEAR                   WLBDA8
038800950314     C                   MOVE      V2CDDC        G02DAT
038900950314     C                   CALL      'XSRDA8'
039000950314     C                   PARM                    WLBDA8
039100950314     C     G02ERR        IFEQ      '1'
039200950314     C                   SETON                                        2840
039300950314     C                   MOVEL     ERR(4)        $MSG
039400950314     C                   GOTO      FINCT2
039500950510     C                   ELSE
039600961114     C                   MOVEL     G02INV        §DDC
039700950510     C                   MOVE      G02DAT        V2CDDC
039800950314     C                   END
039900950314     C*
040000950314     C*  Controllo ora di consegna
040100950314     C     V2CHMC        IFEQ      0
040200950314     C                   SETON                                        4228
040300950314     C                   MOVEL     ERR(5)        $MSG
040400950314     C                   GOTO      FINCT2
040500950314     C                   END
040600950314     C*
040700950314     C     V2CHCO        IFGT      21
040800950314     C     V2CHCO        ORLT      7
040900950314     C     V2CHCO        ANDGT     0
041000950314     C     V2CMCO        ORNE      0
041100950314     C     V2CMCO        ANDGT     59
041200950314     C                   SETON                                        4228
041300950314     C                   MOVEL     ERR(6)        $MSG
041400950314     C                   END
041500950314     C*
041600950314     C     FINCT2        ENDSR
041700961114     C*--------------------------------------------------------------*
041800961114     C*  Sblocco spedizione se possibile
041900961114     C*--------------------------------------------------------------*
042000961114     C     SBLOCC        BEGSR
042100961114     C*
042200961114     C     ARBFBC        IFEQ      'A'
042300961114     C     ARBFBC        OREQ      'B'
042400961114     C                   MOVEL     *BLANKS       ARBFBC
042500961114     C                   MOVEL     *BLANKS       ARBCMC
042600961114     C                   ELSE
042700961114     C     ARBFBC        IFEQ      'G'
042800961114     C*  Controllo se la bolla ha una mamma
042900961114     C     KARB          CHAIN     FNLBL01L                           33
043000961114     C     *IN33         IFEQ      '0'
043100970521     C     LBLLAP        ANDEQ     LBLLAN
043200961114     C                   Z-ADD     LBLAAP        KAAS1
043300961114     C                   Z-ADD     LBLLPP        KLNP1
043400961114     C                   Z-ADD     LBLNRP        KNRS1
043500961114     C                   Z-ADD     LBLNSP        KNSP1
043600961114     C                   MOVEL     'S'           WFIGL
043700961114     C                   ELSE
043800961114     C                   Z-ADD     CDEAAS        KAAS1
043900961114     C                   Z-ADD     CDELNP        KLNP1
044000961114     C                   Z-ADD     CDENRS        KNRS1
044100961114     C                   Z-ADD     CDENSP        KNSP1
044200961114     C                   MOVEL     'N'           WFIGL             1
044300961114     C                   END
044400961114     C*  Verifico se posso sbloccare la spedizione
044500961114     C     WFIGL         IFEQ      'S'
044600140623     C     KGCA          CHAIN     tigcp21L
044700140623      ** tolto l'indicatore (35) xché condizionava campo a video in chiusura distinta
044800140623     C**** *IN35         IFEQ      '0'
044900140623     C**** GCPFAS        ANDNE     40
045000140623     C                   IF        %Found(tigcp21L) and gcpFAS <> 40
045100961114     C                   GOTO      FINSBL
045200961114     C                   END
045300961114     C                   ELSE
045400140623     C     KGCA          CHAIN     tigcp21L
045500140623     C**** *IN35         IFEQ      '0'
045600140623     C**** GCPFAS        ANDLT     35
045700060403      * adesso anche x fase 36
045800060403      *  ossia x disposizioni immesse in Partenza
045900140623     C**** *IN35         oreq      '0'
046000140623     C**** GCPFAS        ANDeq     36
046100140623      *
046200140623     C                   IF        %Found(tigcp21L) and gcpFAS < 35
046300140623     c                               or
046400140623     C                             %Found(tigcp21L) and gcpFAS = 36
046500961114     C                   GOTO      FINSBL
046600961114     C                   END
046700961114     C                   END
046800961114     C                   MOVEL     *BLANKS       ARBFBC
046900961114     C                   MOVEL     *BLANKS       ARBCMC
047000961114     C                   END
047100961114     C*
047200961114     C                   END
047300961114     C*
047400961114     C*  Se ho trovato i segnacolli vado a chiudere le anomalie
047500961114     C                   CLEAR                   DSLR33
047600961114     C                   MOVE      '4'           D33FSC
047700961114     C                   Z-ADD     §DDC          D33DCH
047800961114     C                   MOVEL     'DI'          D33CCH
047900961114     C*
048000961114     C                   Z-ADD     140           KCAA
048100990519     C     KANM          CHAIN     FNANM01L                           37
048200961114     C     *IN37         DOWEQ     '0'
048300961114     C                   Z-ADD     ANMNR1        D33NRR
048400961114     C                   CALL      'FNLR33R'
048500961114     C                   PARM                    DSLR33
048600990519     C     KANM          READE     FNANM01L                               37
048700961114     C                   END
048800961114     C*
048900961114     C     FINSBL        ENDSR
049000950314     C*-----------------------------------------------------*
049100950314     C*  Aggiorno archivio FNCDE con il numero distinta
049200950314     C*-----------------------------------------------------*
049300950314     C     UPDCDE        BEGSR
049400980323      *
049500950320     C                   Z-ADD     0             D63NRR
049600030318     C                   Z-ADD     0             conta_cde         5 0
049700950322     C                   Z-ADD     0             KNDC
049800980324     C                   Z-ADD     *ZEROS        WDSE              1 0
049900980324     C                   MOVEL     'N'           WERROR            1
050000950317     C     KCDE          CHAIN     FNCDE01L                           31
050100030318      *
050200030318      * Al massimo il programma può gestire 999 records (STOP) per distinta
050300030318      * quindi occorre eseguire più volte il pgm creando più distinte
050400030318      * quando sono presenti più di 999 spedizioni da chiudere.
050500950317     C     *IN31         DOWEQ     '0'
050600030318     c     conta_cde     andlt     999
050700140620     C                   Z-ADD     0             WDSE                           azzera
050800030318      *
050900030318      * Il SFL può contenere al massimo 999 STOP,quindi,occorre fare
051000030318      *  più volte la distinta estero bloccando i records a gruppi
051100030318      *   di 999 per volta.
051200030318     c                   add       1             conta_cde
051300030318      *
051400961220     C                   MOVEL     'S'           WERRBL
051500961122     C                   Z-ADD     0             WNRERR            5 0
051600950315     C                   Z-ADD     CDEAAS        KAAS
051700950315     C                   Z-ADD     CDELNP        KLNP
051800950315     C                   Z-ADD     CDENRS        KNRS
051900950315     C                   Z-ADD     CDENSP        KNSP
052000980323      *  Bolla non trovata su FNARB
052100950314     C     KARB          CHAIN     FNARB01L                           32
052200950314     C     *IN32         IFEQ      '1'
052300980323     C                   Z-ADD     7             I
052400950314     C                   EXSR      WRTERR
052500950314     C                   GOTO      AGGREC
052600950314     C                   END
052700980323      *  Linea di arrivo archivio P.C. diversa da bolle arrivi
052800950315     C     ARBLNA        IFNE      CDELNA
052900980323     C                   Z-ADD     8             I
053000950314     C                   EXSR      WRTERR
053100950314     C                   GOTO      AGGREC
053200950314     C                   END
053300000222      *  Rif.numerico mittente diverso da bolle arrivi  NO x DPD
053400000222     C                   if        ARBRMN <> CDERMN and Wdpd <> 'S'
053500980323     C                   Z-ADD     9             I
053600950314     C                   EXSR      WRTERR
053700950314     C                   GOTO      AGGREC
053800950314     C                   END
053900980323      *  Spedizione già consegnata
054000950314     C     ARBDCM        IFNE      0
054100980323     C                   Z-ADD     11            I
054200950314     C                   EXSR      WRTERR
054300950314     C                   GOTO      AGGREC
054400950314     C                   END
054500980323      *  Spedizione in consegna in altra distinta
054600950314     C     ARBNDC        IFNE      0
054700020829     C     ARBNDC        ifne      CDENDC
054800980323     C                   Z-ADD     10            I
054900950314     C                   EXSR      WRTERR
055000950314     C                   GOTO      AGGREC
055100950314     C                   END
055200020829     C                   END
055300980323      *  Spedizione CON FIGLIA
055400950606     C     KARB          CHAIN     FNLBL02L                           33
055500950606     C     *IN33         IFEQ      '0'
055600980323     C                   Z-ADD     18            I
055700950606     C                   EXSR      WRTERR
055800950606     C                   GOTO      AGGREC
055900950606     C                   END
056000980323      *  Spedizione bloccata/giacente/lasc.avviso
056100950322     C     ARBFBC        IFNE      *BLANKS
056200000426      *  Verifico se è possibile sbloccare in automatico la sped.
056300961114     C                   EXSR      SBLOCC
056400000426      *  ... se ci riesco la sblocco, altrimenti se dopo l'esecuzione
056500000426      *  della subroutine la spedizione è ancora bloccata do errore
056600961114     C     ARBFBC        IFNE      *BLANKS
056700980323     C                   Z-ADD     16            I
056800950322     C                   EXSR      WRTERR
056900950322     C                   GOTO      AGGREC
057000950322     C                   END
057100961114     C                   END
057200980323      *  Spedizione non abilitata all'incasso del C/Ass.
057300950322     C                   Z-ADD     1             X
057400950322     C                   CLEAR                   DS3A
057500950322     C     ARBCBO        LOOKUP    C3A(X)                                 32
057600950322     C     *IN32         IFEQ      '1'
057700950322     C                   MOVEL     D3A(X)        DS3A
057800950322     C                   END
057900950321     C     ARBACC        IFNE      'S'
058000950322     C     §3AFCA        ANDEQ     0
058100950322     C     ARBICC        ANDEQ     ' '
058200980323     C                   Z-ADD     14            I
058300950321     C                   EXSR      WRTERR
058400950321     C                   GOTO      AGGREC
058500950321     C                   END
058600980323      *  Spedizione non abilitata all'incasso del Assegnato
058700950322     C                   MOVEL     §3ATB1        WPORTO            1
058800950322     C     ARBACA        IFNE      'S'
058900950322     C     WPORTO        IFEQ      'A'
059000950322     C     §3ATB2        ORNE      *BLANKS
059100980323     C                   Z-ADD     15            I
059200950321     C                   EXSR      WRTERR
059300961118     C                   GOTO      AGGREC
059400950321     C                   END
059500950322     C                   END
059600980323      *  Errore bloccante
059700961129     C     CDEFL1        IFEQ      'B'
059800980323     C                   Z-ADD     22            I
059900961129     C                   EXSR      WRTERR
060000961129     C                   GOTO      AGGREC
060100961129     C                   END
060200980323      *  Se c'è la data di consegna e quella di giacenza
060300961127     C     CDEDCM        IFNE      0
060400961127     C     CDEDAG        ANDNE     0
060500050406     C     KARB          CHAIN     tigcp21L                           33
060600961127     C                   MOVEL     'N'           WERRBL
060700980323      *  Se esiste giacenza con fase 35 prevale consegna
060800961127     C     *IN33         IFEQ      '0'
060900961127     C     GCPFAS        ANDEQ     35
061000961127     C                   Z-ADD     0             CDEDAG
061100961127     C                   ELSE
061200980323     C                   Z-ADD     19            I
061300961127     C                   EXSR      WRTERR
061400961127     C                   END
061500961127     C                   END
061600980323      *  Se c'è la data di consegna e (giacenza o lasciato avviso)
061700980323      *  oppure lasc.avviso e giacenza decide l'utente
061800961127     C     CDEDCM        IFNE      0
061900961127     C     CDEDLA        ANDNE     0
062000961127     C     CDEDLA        ORNE      0
062100961127     C     CDEDAG        ANDNE     0
062200970107     C                   MOVEL     'N'           WERRBL
062300980323     C                   Z-ADD     19            I
062400961127     C                   EXSR      WRTERR
062500961127     C                   END
062600980323      *  Motivo di consegna non gestibile
062700961118     C     CDEERR        IFEQ      'S'
062800980323     C                   Z-ADD     20            I
062900961118     C                   MOVEL     'N'           WERRBL
063000961118     C                   EXSR      WRTERR
063100961118     C                   END
063200000925      *contrassegno non annullato (distinta automatica) opzione 2 poste
063300000925     C     CDEERR        IFEQ      'C'
063400000925     C                   Z-ADD     25            I
063500000925     C                   MOVEL     'N'           WERRBL
063600000925     C                   EXSR      WRTERR
063700000925     C                   END
063800980324      *---------------------------------------
063900980324      *  * Se date a zero e non esiste dettaglio segnacolli
064000980402      *     - scrivo messaggio di impostare almeno una data.
064100980324      *  * Se date a zero ed esiste dettaglio segnacolli
064200980402      *     - scrivo messaggio di analizzare i segnacolli  V5CDSE = 2
064300980402      *-------------------------------------------------------------------------
064400980402      *  - scrivo msg di analizzare i segnacolli se eventi <>  V5CDSE = 2
064500980402      *  - non scrivo msg se segnacolli di eventi = (codice,data,ora) V5CDSE = 1
064600980402      *     SE RIPRISTINATA IMPLEMENTARE SCRIVENDO DATE E MSG DI FNCDS
064700980402      *-------------------------------------------------------------------------
064800980402      *
064900961120     C     CDEDCM        IFEQ      0
065000961120     C     CDEDLA        ANDEQ     0
065100961120     C     CDEDAG        ANDEQ     0
065200961120     C     CDEDRC        ANDEQ     0
065300980323      *
065400980323     C                   Z-ADD     CDEAAS        KAAS
065500980323     C                   Z-ADD     CDELNP        KLNP
065600980323     C                   Z-ADD     CDENRS        KNRS
065700980323     C                   Z-ADD     CDENSP        KNSP
065800980324     C     KARB          CHAIN     FNCDS003                           33
065900980324      *
066000980324     C     *IN33         IFEQ      '1'
066100980324     C                   Z-ADD     12            I
066200980324     C                   ELSE
066300980402      *
066400980402     C                   Z-ADD     24            I                              se ripristini
066500980402     C                   Z-ADD     2             WDSE                           specifiche *?*
066600980402     C                   END                                                    elimina queste
066700980324      *
066800970107     C                   MOVEL     'N'           WERRBL
066900961120     C                   EXSR      WRTERR
067000961120     C                   END
067100980324      *---------------------------------------
067200000210      *  Impossibile modificare codice cons.particolare ** FORZABILE **
067300000210      *   scrivo la consegna particolare nel msg e la levo dal sbf
067400000210     C                   SELECT
067500000210     C                   WHEN      CDEtc1 = *blanks
067600000210     C                   WHEN      ARBtc1 = CDEtc1  or  ARBtc2 = CDEtc1
067700000210     C                   WHEN      ARBtc1 = *blanks  or  ARBtc2 = *blanks
067800000210     C                   OTHER
067900000210     C                   movel     'N'           WERRBL
068000000210     C                   z-add     17            I
068100000210     C                   exsr      WRTERR
068200000210     C                   ENDSL
068300980406      *
068400980406      *  Se 2° evento mancanza scrivo messaggio
068500961120     C     CDEFL1        IFEQ      'S'
068600980323     C                   Z-ADD     22            I                 4 0
068700961120     C                   MOVEL     'N'           WERRBL            1
068800961120     C                   EXSR      WRTERR
068900961120     C                   END
069000980323      *
069100980323      *  Aggiorno record su FNARB
069200950320     C                   ADD       1             D63NRR
069300020830     C                   z-add     d55tfa        arbifp
069400020829     C                   Z-ADD     V2CNDC        ARBNDC
069500950314     C                   EXCEPT    AGGARB
069600980323      *
069700961120     C     AGGREC        TAG
069800980323      *
069900980323      *  Aggiorno FNCDE
070000950315     C                   Z-ADD     V2CNDC        CDENDC
070100950315     C                   EXCEPT    AGGCDE
070200980323      *
070300980323      *  Aggiorno FNCDS se esiste
070400980323     C                   Z-ADD     CDEAAS        KAAS
070500980323     C                   Z-ADD     CDELNP        KLNP
070600980323     C                   Z-ADD     CDENRS        KNRS
070700980323     C                   Z-ADD     CDENSP        KNSP
070800980323     C     KARB          SETLL     FNCDS003                               33
070900980323     C                   DO        *HIVAL
071000980323     C     KARB          READE     FNCDS003                               33
071100980323     C  N33              Z-ADD     V2CNDC        CDSNDC
071200980323     C  N33              EXCEPT    AGGCDS
071300980323     C  N33              END
071400980323      *
071500980323      *  Leggo prossimo record di FNCDE con numero distinta a 0
071600950317     C     KCDE          READE     FNCDE01L                               31
071700950315     C                   END
071800950314     C*
071900950317     C                   COMMIT
072000950317     C*
072100030318     C*  Se presenti più di 999 spedizioni manda un msg di Avviso
072200030318     C*  che non ha potuto elaborare tutto sotto la stessa distinta
072300030318     C*  e quindi occorre rieseguire il programma.
072400030318     c                   if        conta_cde = 999
072500030318     C*    INVIO MSG
072600030318     C                   movel     msg_alert1    WINMSG
072700030318     c                   exfmt     LR63W08
072800030318     C                   clear                   WINMSG
072900030318     C                   movel     msg_alert2    WINMSG
073000030318     c                   exfmt     LR63W08
073100030318     C                   clear                   WINMSG
073200030318     c                   endiF
073300030318     C*
073400950314     C                   ENDSR
073500950314     C*-----------------------------------------------------*
073600000426     C*  SCRITTURA ERRORI: la rountine gestisce la scrittura
073700000426     C*  di un subfile nascosto. Il programma di gestione
073800000426     C*  errori carica il sfl per la loro visualizzazione
073900000426     C*  da quest'ulitmo. L'utente esegue le operazioni per
074000000426     C*  eliminarli. Queste rimangono sospese fino a che non
074100000426     C*  viene data la conferma. Il programma procederà a
074200000426     C*  rileggere il sfl precedentemente aggiornato in   .
074300000426     C*  base alle scelte effettuate, a modificare altri
074400000426     C*  eventuali file.
074500000426     C*  Si caricherà poi gli eventuali errori rimasti a
074600000426     C*  video.
074700950314     C*-----------------------------------------------------*
074800950314     C     WRTERR        BEGSR
074900950314     C*
075000950314     C*  Se è la prima volta azzero subfile degli errori
075100950314     C     WERROR        IFEQ      'N'
075200980323     C                   Z-ADD     0             NRR5              4 0
075300950314     C                   MOVEL     'S'           WERROR
075400950315     C                   SETOFF                                       2021
075500950317     C                   WRITE     LR63C05
075600950314     C                   SETON                                        2021
075700950314     C                   END
075800961122     C*
075900961122     C*  Aggiorno numeratore errori
076000961122     C                   ADD       1             WNRERR
076100950314     C*  Se la data di consegna non c'è ma esiste la data
076200950314     C*  di riserva valorizzo la prima con la seconda
076300950315     C     CDEDRC        IFNE      0
076400950315     C     CDEDCM        ANDEQ     0
076500950315     C                   Z-ADD     CDEDRC        CDEDCM
076600950314     C                   END
076700950314     C*
076800950317     C                   ADD       1             NRR5
076900961122     C     WNRERR        IFGT      1
077000961122     C                   MOVEL     '1'           V5CIN3
077100961122     C                   ELSE
077200961122     C                   MOVEL     '0'           V5CIN3
077300961122     C                   END
077400961122     C                   MOVEL     *BLANKS       V5CSCE
077500950606     C                   MOVEL     *BLANKS       V5CTPD
077600980324     C                   MOVEL     WDSE          V5CDSE
077700000426     C                   MOVEL     WERRBL        V5CBLO                         errore bloc.
077800950317     C                   Z-ADD     CDEAAS        V5CAAS
077900950317     C                   Z-ADD     CDELNP        V5CLNP
078000950317     C                   Z-ADD     CDENRS        V5CNRS
078100950317     C                   Z-ADD     CDENSP        V5CNSP
078200950317     C                   Z-ADD     CDENSP        V5CNSP
078300950320     C                   Z-ADD     CDERMN        V5CRMN
078400961118     C                   MOVEL     CDECCA        V5CCCA
078500000210     C                   MOVEL     CDETC1        V5CTC1
078600961118     C                   MOVEL     CDESTS        V5CSTS
078700961120     C                   MOVEL     CDECOD        V5CCOD
078800961120     C                   MOVEL     CDEMOT        V5DCOD
078900961120     C                   MOVEL     CDEFTX        V5CFTX
079000961120     C                   MOVEL     CDEFL1        V5CFL1
079100950320     C                   CLEAR                   WLBDA8
079200950320     C                   Z-ADD     CDEDCM        G02INV
079300950320     C                   MOVEL     '3'           G02ERR
079400950320     C                   CALL      'XSRDA8'
079500950320     C                   PARM                    WLBDA8
079600950320     C                   Z-ADD     G02DAT        V5CDCM
079700950320     C                   Z-ADD     CDEDRC        G02INV
079800950320     C                   MOVEL     '3'           G02ERR
079900950320     C                   CALL      'XSRDA8'
080000950320     C                   PARM                    WLBDA8
080100950320     C                   Z-ADD     G02DAT        V5CDRC
080200950320     C                   Z-ADD     CDEDLA        G02INV
080300950320     C                   MOVEL     '3'           G02ERR
080400950320     C                   CALL      'XSRDA8'
080500950320     C                   PARM                    WLBDA8
080600950320     C                   Z-ADD     G02DAT        V5CDLA
080700950320     C                   Z-ADD     CDEDAG        G02INV
080800950320     C                   MOVEL     '3'           G02ERR
080900950320     C                   CALL      'XSRDA8'
081000950320     C                   PARM                    WLBDA8
081100950320     C                   Z-ADD     G02DAT        V5CGIA
081200000210      * MESSAGGIO DI ERRORE
081300000210     C                   SELECT
081400000210     C                   WHEN      I = 17
081500000210     C                   movel     ERR(I)        V5CERR
081600000210     c                   eval      %subst(V5Cerr:38:1) = CDEtc1
081700000210     C                   clear                   V5Ctc1
081800000210     C                   WHEN      I = 20
081900000210     C                   movel(P)  CDEMOT        V5CERR
082000000210     C                   OTHER
082100000210     C                   movel     ERR(I)        V5CERR
082200000210     C                   ENDSL
082300000426      * write riga sfl errori
082400950317     C                   WRITE     LR63S05
082500000210      *
082600950314     C                   ENDSR
082700950315     C*-----------------------------------------------------*
082800950315     C*  Gestione degli errori
082900950315     C*-----------------------------------------------------*
083000950315     C     GESERR        BEGSR
083100950315     C*
083200950315     C*  Richiamo pgm subfile gestione errori
083300950315     C                   MOVEL     FNLR63        KPJBU
083400950315     C                   CALL      'FNLR63R2'
083500950315     C                   PARM                    KPJBA
083600950320     C                   MOVEL     KPJBU         FNLR63
083700950426     C                   MOVEL     'S'           WPGMER
083800980323      *  Se non ho corretto tutti gli errori e sono uscita con
083900980323      *  F3 richiamo il programma per sbloccare le distinte
084000950315     C     D63F03        IFEQ      '1'
084100950315     C                   MOVEL     FNLR63        KPJBU
084200950315     C                   CALL      'FNLR63R3'
084300950315     C                   PARM                    KPJBA
084400950320     C                   MOVEL     KPJBU         FNLR63
084500950315     C                   MOVEL     'S'           WFINE
084600950320     C                   MOVEL     'S'           WSBLOC
084700950315     C                   END
084800980323      *
084900950315     C                   ENDSR
085000950314      *---------------------------------------------------------*
085100950314      *  Caricamento singole distinte                           *
085200950314      *---------------------------------------------------------*
085300950314     C     CARDIS        BEGSR
085400950314     C*
085500950315     C*  Impostazione dati in schiere
085600950315     C                   EXSR      INZSCH
085700950314     C*
085800950317     C                   Z-ADD     D63NDT        KNDC
085900950315     C     KCDE          CHAIN     FNCDE01L                           31
086000950320     C*  Aggiorno FNARB + archivio dei fogli viaggio
086100950320     C     *IN31         IFEQ      '0'
086200950320     C                   EXSR      UPDDIS
086300950320     C*
086400950314     C     *IN31         DOWEQ     '0'
086500950315     C*  Scrivo record di consegna e incassi
086600950315     C                   EXSR      WRTSBF
086700950314     C*
086800950320     C     KCDE          READE     FNCDE01L                               31
086900950314     C                   END
087000950320     C                   END
087100950317     C*
087200950317     C                   COMMIT
087300950315     C*
087400950315     C                   ENDSR
087500950315      *---------------------------------------------------------*
087600950315      *  Richiamo pgm. FNLR53R                                  *
087700950315      *---------------------------------------------------------*
087800950315     C     CALL53        BEGSR
087900950315     C*
088000950315     C                   MOVEL     D63TRI        D53TRI
088100950315     C                   Z-ADD     D63LNA        D53LNA
088200950315     C                   MOVEL     'E'           D53ITA
088300950315     C                   MOVEL     APDRSC        D53DSF
088400050516     C                   move      APDpdr        D53dsf
088500950530     C                   MOVEL     APDFCM        D53CMG
088600950315     C                   MOVEL     ' '           D53F03
088700950315     C                   MOVEL     'N'           D53WRT
088800950315     C                   MOVEL     FNLR53        KPJBU
088900140923      *
089000140923      * Il richiamo al CL serve per gestire una chiusura anomala del pgm di chiusura
089100140923      *  distinta. Viene intercettato l'errore e gestirlo come se si fosse premuto
089200140923      *  F3 dalla ch.distinta stessa.
089300140923      *  In questo modo, NON si eliminano i records che devono comunque essere ancora
089400140923      *  ri-processati causa rottura del pgm FNLR53R.
089500140923     C********           CALL      'FNLR53R'
089600140924     C                   CALL      'FNLR63CM'
089700950315     C                   PARM                    KPJBA
089800030718     C                   PARM                    trul90ds
089900140923      *
090000950315     C                   MOVEL     KPJBU         FNLR53
090100950320     C                   MOVEL     'S'           WCHIUS
090200950315     C*
090300020830     C*  Se non ho confermato la distinta richiamo pgm x sblocco
090400950323     C     D53F03        IFNE      ' '
090500950321     C                   MOVEL     FNLR63        KPJBU
090600950315     C                   CALL      'FNLR63R3'
090700950315     C                   PARM                    KPJBA
090800950320     C                   MOVEL     'S'           WSBLOC
090900950323     C     D53F03        IFEQ      '2'
091000950323     C                   MOVEL     'S'           WFINE
091100950323     C                   END
091200950322     C*  Ho confermato la distinta
091300950315     C                   ELSE
091400950322     C     D63TRI        IFNE      'U'
091500950322     C                   EXSR      CONFER
091600030318     C*  se confermato e ci sono altre bolle ancora da chiudere
091700030318     C*  viene lanciato un msg di avvertimento.
091800030318     c                   if        conta_cde = 999
091900030318     C*    INVIO MSG
092000030318     C                   movel     Riesegui      WINMSG
092100030318     c                   exfmt     LR63W08
092200030318     C                   clear                   WINMSG
092300030318     c                   endiF
092400950315     C                   END
092500950322     C*
092600950322     C                   END
092700950321     C                   CLEAR                   FNLR53
092800950317     C*
092900950317     C                   COMMIT
093000950315     C*
093100950315     C                   ENDSR
093200950315     C*-----------------------------------------------------*
093300950315     C*  Inizializzazione delle variabili nelle schiere
093400950315     C*-----------------------------------------------------*
093500950315     C     INZSCH        BEGSR
093600950315     C*
093700950315     C                   Z-ADD     0             NDC
093800950315     C                   Z-ADD     0             DTD
093900950315     C                   Z-ADD     0             DDC
094000950315     C                   Z-ADD     0             HRC
094100950315     C                   Z-ADD     0             SET
094200950315     C                   MOVEA     *BLANKS       FPP
094300950315     C                   MOVEA     *BLANKS       PEP
094400030204     C                   MOVEA     *BLANKS       CAR
094500950315     C*
094600950315     C*  Preimposto i dati nelle schiere in base al video
094700950315     C                   Z-ADD     V2CNDC        NDC(1)
094800950315     C                   Z-ADD     V2CDDC        DDC(1)
094900950315     C                   Z-ADD     V2CDDC        DTD(1)
095000950315     C                   Z-ADD     V2CHMC        HRC(1)
095100950315     C                   MOVEL     V2CFPP        FPP(1)
095200950315     C                   MOVEL     V2CPEP        PEP(1)
095300950315     C*  Converto la data della distinta nel formato AAAAMMGG
095400950315     C                   CLEAR                   WLBDA8
095500950315     C                   MOVE      V2CDDC        G02DAT
095600950315     C                   CALL      'XSRDA8'
095700950315     C                   PARM                    WLBDA8
095800950315     C                   Z-ADD     G02INV        §DDC
095900950315     C     G02TGI        SUB       15            GIOTGI
096000950315     C                   MOVE      *ZEROS        GIODAT
096100950315     C                   MOVE      *ZEROS        GIOINV
096200950315     C                   CALL      'XSRGI8'
096300950315     C                   PARM                    WLBGIO
096400950315     C                   MOVE      GIOINV        §G15              8 0
096500950315     C                   Z-ADD     §G15          G15(1)
096600950315     C*
096700950315     C                   SETON                                        62
096800950315     C                   WRITE     LR53C02
096900950315     C                   WRITE     LR53C03
097000950315     C                   SETOFF                                       6228
097100950315     C                   Z-ADD     0             SET
097200950315     C*
097300950315     C                   Z-ADD     0             NRR2              4 0
097400950315     C                   Z-ADD     0             NRR3              4 0
097500020611     C*
097600020611     C* inzializza il numeratore dello Stop se non è presente su ARB
097700020626     C                   Z-ADD     1             num_stop          5 0
097800950315     C*
097900950315     C                   ENDSR
098000950315      *---------------------------------------------------------*
098100950315      *  Aggiorno FNARB + FNFVV                                 *
098200950315      *---------------------------------------------------------*
098300950315     C     UPDDIS        BEGSR
098400950320     C*
098500950321     C                   Z-ADD     V2CNDC        KNFV
098600950321     C     KFVV          CHAIN     FNFVV01L                           96
098700950315     C* Scrivo il foglio viaggio bloccato
098800950315     C                   CLEAR                   FNFVV000
098900950317     C                   MOVEL     APDRSC        FVVDSF
099000020830     C                   MOVEL     d55tfa        FVVFGS
099100020916     C                   MOVEL     simFEL        FVVFLE
099200950315     C                   Z-ADD     §DDC          FVVDFV
099300950317     C                   Z-ADD     V2CNDC        FVVNFV
099400950315     C                   Z-ADD     4             FVVNPG
099500950315     C                   MOVEL     'S'           FVVFSC
099600950315     C                   MOVEL     V2CFPP        FVVFPP
099700950317     C                   MOVEL     V2CPEP        FVVPEP
099800950315     C                   MOVEL     'F'           FVVFCF
099900950321     C  N96              UPDATE    FNFVV000
100000950321     C   96              WRITE     FNFVV000
100100070515      *scrive nuovo file distinte FIDST se partita nuova gestione automatica
100200070515     c                   if        §ogdda <> *blank
100300070515     C                   Z-ADD     V2CNDC        KNFV6
100400070515     C     Kdst          CHAIN     Fidst01L                           96
100500070515     C* Scrivo il foglio viaggio bloccato
100600070515     c                   if        not %found(fidst01l)
100700070515     C                   clear                   Fidst000
100800070515     C                   MOVEL     APDRSC        dstnot
100900070515     C                   z-add     APDpdr        dstpdr
101000070515     C                   MOVEL     FVVFGS        dstfgs
101100070515     C                   Z-ADD     FVVDFV        dstdfv
101200070515     C                   Z-ADD     FVVNFV        dstnfv
101300070515     C                   Z-ADD     FVVNPG        dstnpg
101400070515     C                   MOVEL     FVVFSC        dstfca
101500070515     C                   MOVEL     FVVFPP        dstfpp
101600070515     C                   MOVEL     FVVPEP        dstpep
101700070515     C                   MOVE      'A'           dsttipa
101800070515     C                   MOVE      'N'           dstpda
101900070515     C                   WRITE     Fidst000
102000070515     c                   endif
102100070515     c                   endif
102200950315     C*
102300950315     C                   ENDSR
102400950315      *---------------------------------------------------------*
102500950315      *  Scrivo record subfile                                  *
102600950315      *---------------------------------------------------------*
102700950315     C     WRTSBF        BEGSR
102800950320     C*
102900950320     C* Eseguo chain su archivio bolle arrivo e lo fleggo
103000950320     C                   Z-ADD     CDEAAS        KAAS
103100950320     C                   Z-ADD     CDELNP        KLNP
103200950320     C                   Z-ADD     CDENRS        KNRS
103300950320     C                   Z-ADD     CDENSP        KNSP
103400950320     C     KARB          CHAIN     FNARB01L                           32
103500950320     C     *IN32         IFEQ      '0'
103600950320     C                   Z-ADD     §DDC          ARBDDC
103700950320     C                   Z-ADD     0             ARBNMI
103800950320     C                   MOVE      WCDCPC        ARBPDC
103900950321     C* Se merce disabilitata la abilito
104000950321     C     ARBAMA        IFEQ      0
104100950321     C                   MOVEL     3             ARBAMA
104200950321     C                   END
104300950320     C                   EXCEPT    AGGDIS
104400950320     C                   END
104500950315     C* Pulisco campi subfile
104600991012     C                   SETOFF                                       06
104700950315     C                   SETOFF                                       404142
104800950315     C                   SETOFF                                       434445
104900950315     C                   SETOFF                                       464748
105000991012     C                   SETOFF                                       080522
105100950315     C                   SETOFF                                       790703
105200950315     C                   CLEAR                   LR53S02
105300950315     C                   CLEAR                   LR53S03
105400950315     C                   Z-ADD     0             WIMA
105500970416     C                   Z-ADD     0             WCAS
105600970416     C                   MOVEL     *BLANKS       WVCA
105700990923     C                   MOVEL     *BLANKS       WDIV
105800970416     C                   MOVEL     *BLANKS       WTIC
105900950323     C                   Z-ADD     0             AR9CAS
106000950323     C                   MOVEL     *BLANKS       AR9VCA
106100950315     C* Incremento totale spedizioni x distinta
106200950315     C                   ADD       1             SET(1)
106300950315     C                   Z-ADD     V2CNDC        VNMDOC
106400950315     C                   Z-ADD     G15(1)        VH2G15
106500950315     C                   Z-ADD     §DDC          VH2DDC
106600950315     C                   Z-ADD     §DDC          VH2DDT
106700950315     C                   MOVEL     'NO'          V2CCON
106800950315     C*
106900110418     C                   clear                   V3CMIT
107000110418     C*                  MOVEL     ARBRSM        V3CMIT
107100110418     C*    ARBRMO        IFNE      *BLANKS
107200110418     C*                  MOVEL     ARBRMO        V3CMIT
107300110418     C*                  END
107400950315     C                   MOVEL     ARBRSD        V02DES
107500950315     C                   Z-ADD     ARBNTC        V2CNTC
107600950315     C                   Z-ADD     ARBAAS        V2CAAS
107700950315     C                   Z-ADD     ARBLNP        V2CLNP
107800950315     C                   Z-ADD     ARBNRS        V2CNRS
107900950315     C                   Z-ADD     ARBNSP        V2CNSP
108000950315     C                   MOVEL     ARBCBO        V2CCBO
108100950315     C                   MOVEL     ARBGMA        V2CGMA
108200950315     C     V2CGMA        COMP      *BLANKS                                08
108300950315     C                   MOVEL     ARBMGS        MM                2 0
108400950315     C                   MOVE      ARBMGS        GG                2 0
108500950315     C                   MOVEL     GG            V2CGMS
108600950315     C                   MOVE      MM            V2CGMS
108700950315     C                   MOVE      ARBSTP        V02STP
108800020611      *
108900020611      * Non è possibile avere lo stop a (0) quindi imposta il numero
109000020611      *  di stop tramite un numeratore
109100030318      *   ma attenzione si possono gestire al massimo 999 stop e non di più
109200030318      *   in tal caso si riinizia da (1).
109300020611     c                   if        v02stp = 0
109400020611     c                   eval      v02stp = num_stop
109500030318     c                   if        num_stop <> 999
109600020611     c                   eval      num_stop = num_stop + 1
109700030318     c                   else
109800030318     c                   eval      num_stop = 1
109900030318     c                   end
110000020611     c                   end
110100020611      *
110200970416     C* Controllo se esiste bolla mamma
110300970417     C     KARB          CHAIN     FNLBL01L                           76
110400970416     C     *IN76         IFEQ      '0'
110500970416     C     LBLLAN        ANDEQ     LBLLAP
110600980430     C                   Z-ADD     LBLAAP        KAAS
110700980430     C                   Z-ADD     LBLLPP        KLNP
110800980430     C                   Z-ADD     LBLNRP        KNRS
110900980430     C                   Z-ADD     LBLNSP        KNSP
111000980430     C     KARB          CHAIN     FNARB01L                           76
111100000426     C* Se bolla trovata e non chiusa con causale di reso ....
111200980430     C     *IN76         IFEQ      '0'
111300980430     C     ARBCCA        ANDNE     '2'
111400980430     C     ARBCCA        ANDNE     '6'
111500970416     C* Memorizzo il legame esistente per i successivi accessi su FNARB
111600970416     C                   Z-ADD     LBLAAP        VH2AAP
111700970416     C                   Z-ADD     LBLLPP        VH2LPP
111800970416     C                   Z-ADD     LBLNRP        VH2NRP
111900970416     C                   Z-ADD     LBLNSP        VH2NSP
112000970416     C                   END
112100000426     C* Riaggancio la figlia
112200980430     C                   Z-ADD     LBLAAN        KAAS
112300980430     C                   Z-ADD     LBLLPN        KLNP
112400980430     C                   Z-ADD     LBLNRN        KNRS
112500980430     C                   Z-ADD     LBLNSN        KNSP
112600980430     C     KARB          CHAIN     FNARB01L                           76
112700980430     C                   END
112800950315     C*  controllo se c'è il contrassegno
112900950315     C                   Z-ADD     1             X
113000950315     C                   CLEAR                   DS3A
113100950315     C     ARBCBO        LOOKUP    C3A(X)                                 32
113200950315     C     *IN32         IFEQ      '1'
113300950315     C                   MOVEL     D3A(X)        DS3A
113400950315     C                   END
113500950315     C*  Controllo se c'è assegnato
113600950315     C     ARBICA        IFEQ      ' '
113700990923     C     ARBICA        OREQ      'R'
113800990923     C     ARBFIP        OREQ      'I'
113900950315     C                   MOVEL     §3ATB1        WPORTO            1
114000950315     C     WPORTO        IFEQ      'A'
114100990923     C                   MOVEL     '1'           KTRC
114200990923     C                   ELSE
114300990923     C                   MOVEL     '2'           KTRC
114400990923     C                   END
114500001128     C     KAR6          CHAIN     FIAR601L                           32
114600990923     C     *IN32         IFEQ      '0'
114700001128     C     AR6DFT        ANDNE     *ZEROS
114800990923     C                   Z-ADD     AR6IFT        WIMA
114900990923     C                   MOVEL     AR6DIV        WDIV
115000990923     C* Loop su file incassi parziali x determinare importo da inc.
115100990923     C                   MOVEL     'A'           KTIP
115200990923     C     KARI          CHAIN     FIARI01L                           95
115300990923     C     *IN95         DOWEQ     '0'
115400991012     C     AR6DIV        IFEQ      ARIDIV
115500990923     C                   SUB       ARIIPP        WIMA
115600991012     C                   ELSE
115700991012     C* Se la divisa di incasso è diversa converto importo già
115800991012     C* incassato nella divisa della bolla
115900991012     C                   CLEAR                   YEURDS
116000991012     C                   MOVEL     ARIDIV        YECDVI
116100991012     C                   Z-ADD     ARIIPP        YECIMI
116200991012     C                   MOVEL     AR6DIV        YECDVO
116300991012     C                   MOVEL     'H'           YECTAR
116400991012     C                   CALL      'YEURCO'
116500991012     C                   PARM                    YEURDS
116600991012     C                   SUB       YECIMO        WIMA
116700991012     C                   END
116800990923     C     KARI          READE     FIARI01L                               95
116900990923     C                   END
117000990923     C                   Z-ADD     AR6DFT        V3CDFT
117100970416     C                   END
117200950315     C*
117300950315     C                   ELSE
117400950315     C                   MOVE      'S'           V2CICA
117500950315     C                   END
117600950315     C* Metto la 'V' in v02sel se per la bolla è prevista la gestione
117700950315     C* della firma del destinatario
117800950315     C     ARBGMA        IFNE      *BLANKS
117900950315     C                   Z-ADD     1             X
118000950315     C                   CLEAR                   DS7R
118100950315     C     ARBGMA        LOOKUP    C7R(X)                                 32
118200950315     C     *IN32         IFEQ      '0'
118300950315     C                   SETON                                        08
118400950315     C                   MOVEL     TBLUNI        DS7R
118500950315     C                   END
118600950315     C     §7R2FI        IFEQ      'S'
118700970107     C                   MOVEL     'V'           V02SEL
118800950315     C                   END
118900950315     C                   END
119000961121     C* Metto E se prevista visualizzazione note EDI
119100961121     C     CDEFTX        IFEQ      'S'
119200961211     C                   MOVEL     '*'           V02SEL
119300961125     C                   ELSE
119400961125     C     CDENO1        IFNE      *BLANKS
119500961125     C     CDENO2        ORNE      *BLANKS
119600970107     C                   MOVEL     '*'           V02SEL
119700961121     C                   END
119800961125     C                   END
119900950315     C*
120000950315     C                   MOVE      ARBTCR        V2CTCR
120100950315     C                   MOVE      ARBDCR        V2CDCR
120200950315     C                   MOVE      ARBHCR        V2CHCR
120300051215      * richiamo pgm per informazione relativamente alla data cons. rich.
120400051215      * se TASSATIVA
120500051215     c                   clear                   v2ctat
120600051215     c                   clear                   fnlv80ds
120700051215     c                   z-add     arbAAS        ILV80AAS
120800051215     c                   z-add     arbLNP        ILV80LNP
120900051215     c                   z-add     arbNRS        ILV80NRS
121000051215     c                   z-add     arbNSP        ILV80NSP
121100051215     c                   z-add     arbMGS        ILV80MGS
121200051215     c                   move      arbTCR        ILV80TCR
121300051215     c                   z-add     arbDCR        ILV80DCR
121400051215     c                   z-add     arbHCR        ILV80HCR
121500051215     c                   move      arbTC1        ILV80TC1
121600051215     c                   move      arbTC2        ILV80TC2
121700051215     c                   call      'FNLV80R'
121800051215     c                   parm                    kpjba
121900051215     c                   parm                    fnlv80ds
122000051215     c                   move      olv80tat      v2ctat
122100051215      *
122200950315     C*
122300950321     C     §3AFCA        IFGT      0
122400950921     C     ARBICC        IFEQ      'R'
122500950921     C     ARBICC        OREQ      ' '
122600950921     C* Controllo se C/Assegno sulla BOLLA
122700051115     C     KARB          CHAIN     FIAR901L                           32
122800970417     C     *IN32         IFEQ      '1'
122900970416     C* Controllo se C/Assegno sulla mamma
123000970416     C     VH2AAP        IFNE      0
123100970416     C                   Z-ADD     LBLAAP        KAAP
123200970416     C                   Z-ADD     LBLLPP        KLPP
123300970416     C                   Z-ADD     LBLNRP        KNRP
123400970417     C                   Z-ADD     LBLNSP        KNSPP
123500970416     C     KARB1         CHAIN     FNARB01L                           32
123600051115     C  N32KARB1         CHAIN     FIAR901L                           32
123700970416     C                   END
123800970416     C                   END                                                    --- 02 ---
123900970416     C     *IN32         IFEQ      '0'                                          --- 02 ---
124000970416     C                   Z-ADD     AR9CAS        WCAS
124100970416     C                   MOVEL     AR9VCA        WVCA
124200970416     C                   MOVEL     AR9TIC        WTIC
124300970416     C                   END                                                    --- 02 ---
124400950315     C* Carico cliente x controllo tabella "TM"
124500950315     C     ARBCCM        IFGT      *ZEROS
124600950315     C                   MOVE      ARBCCM        §CLITM
124700950315     C                   ELSE
124800950315     C                   MOVE      ARBKSC        §CLITM
124900950315     C                   END
125000950315     C                   ELSE
125100950315     C                   MOVE      'S'           V2CICC
125200950315     C                   END                                                    --- 01 ---
125300950315     C                   END                                                    --- 01 ---
125400950315     C* Conto i colli ancora da consegnare
125500970416     C*  Colli su figlia
125600950320     C     KARB          CHAIN     FNART01L                           32
125700950320     C     *IN32         IFEQ      '0'
125800950320     C     *IN32         DOWEQ     '0'
125900950315     C     ARTDCM        IFEQ      0
126000950315     C                   ADD       1             V02NCL
126100950315     C                   END
126200950320     C     KARB          READE     FNART01L                               32
126300950315     C                   END
126400970416     C*
126500970416     C                   ELSE
126600970416     C*  Colli su mamma
126700970416     C     VH2AAP        IFNE      0
126800970416     C                   Z-ADD     LBLAAP        KAAP
126900970416     C                   Z-ADD     LBLLPP        KLPP
127000970416     C                   Z-ADD     LBLNRP        KNRP
127100970417     C                   Z-ADD     LBLNSP        KNSPP
127200970416     C     KARB1         CHAIN     FNART01L                           95
127300970416     C     *IN95         DOWEQ     '0'
127400970416     C     ARTDCM        IFEQ      0
127500970416     C                   ADD       1             V02NCL
127600970416     C                   END
127700970416     C     KARB1         READE     FNART01L                               95
127800970416     C                   END
127900970416     C                   END
128000970416     C*
128100950315     C                   END
128200950315     C*
128300950315     C     V02NCL        IFNE      0
128400950315     C                   Z-ADD     ARTAAS        HARAAS
128500950315     C                   Z-ADD     ARTLNP        HARLNP
128600950315     C                   Z-ADD     ARTNRS        HARNRS
128700950315     C                   Z-ADD     ARTNSP        HARNSP
128800950315     C                   Z-ADD     ARTFLS        V2CFLS
128900950315     C                   END
129000950315     C                   ADD       1             NRR2
129100950315     C*  Routine per determinazione volume da utilizzare
129200950315     C                   EXSR      DETVOL
129300950315     C* Determino peso e volume ancora da consegnare
129400950315     C     ARBPKF        MULT      V02NCL        APPOG            18 3
129500950315     C     APPOG         DIV(H)    ARBNCL        V02PKG
129600950315     C     WVOL          MULT      V02NCL        APPOG            18 3
129700950315     C     APPOG         DIV(H)    ARBNCL        V02VOL
129800950315     C*  Scrivo record importi
129900950315     C     WIMA          IFNE      0
130000970416     C     WCAS          ORNE      0
130100950315     C                   Z-ADD     V2CAAS        V3CAAS
130200950315     C                   Z-ADD     V2CLNP        V3CLNP
130300950315     C                   Z-ADD     V2CNRS        V3CNRS
130400950315     C                   Z-ADD     V2CNSP        V3CNSP
130500950315     C                   MOVEL     V2CCBO        V3CCBO
130600950315     C                   MOVEL     V2CCON        V3CCON
130700950315     C                   MOVEL     V02DES        V3CDES
130800950315     C     NRR3          ADD       1             V2CNRD
130900950315     C                   MOVEL     *BLANKS       V3CTOT
131000950315     C                   Z-ADD     NRR2          V3CNRI
131100950315     C*  Controllo se esiste C/Assegno
131200970416     C     WCAS          IFGT      0
131300950315     C                   ADD       1             NRR3
131400950315     C                   MOVEL     'N'           V02SN1
131500950315     C                   MOVEL     'C/A'         V3DTPI
131600970416     C                   Z-ADD     WCAS          V3CIMP
131700970416     C                   MOVEL     WVCA          V3CVCA
131800990924     C                   MOVEL     WVCA          VH3DIV
131900110418     c* se incassi imposto la ragione sociale intestatario assegno.
132000110418     c     kar5          chain     fiar501l
132100110418     c                   if        %found(fiar501l)
132200110418     c                   movel     ar5uni        dar5gen
132300110418     c                   reset                   trulintds
132400110418     c                   eval      iINTtic  = wtic
132500110418     c                   eval      iINTrscM = arbrsm
132600110418     c                   eval      iINTrscO = arbrmo
132700110418     c                   eval      iINTcdi  = §AR5cdi
132800110418     c                   call      'TRULINTR'
132900110418     c                   parm                    TRULINTDS
133000110418     c                   if        oINTerr = *blank
133100110418     c                   eval      v3cmit  = oINTrsci
133200110418     c                   end
133300110418     c                   end
133400991012     C* Gestisco riga di totale solo se esiste anche assegnato
133500950315     C     WIMA          IFGT      0
133600991012     C* .. & la divisa di incasso assegnato è quella corrente...
133700011009     C     WDIV          IFEQ      §GEDCr
133800991012     C* .. o quella alternativa ..
133900991012     C     WDIV          OREQ      DIVEUR
134000991012     C* .. & la divisa di incasso C/Assegno è quella corrente...
134100011009     C     WVCA          IFEQ      §GEDCr
134200991012     C* .. o quella alternativa ..
134300991012     C     WVCA          OREQ      DIVEUR
134400950315     C                   MOVEL     'NO'          V3CTOT
134500950315     C                   END
134600991012     C                   END
134700991012     C                   END
134800970416     C                   MOVEL     WTIC          V3CTIC
134900990923     C                   Z-ADD     1             X
135000990923     C     WTIC          LOOKUP    C1A(X)                                 95
135100990923     C                   MOVEL     D1A(X)        DS1A
135200991123     C     §1AFMB        COMP      'M'                                    06
135300950315     C                   WRITE     LR53S03
135400950315     C                   SETON                                        03
135500950315     C                   END
135600950315     C*  Controllo se esiste Assegnato
135700950315     C     WIMA          IFGT      0
135800950315     C                   ADD       1             NRR3
135900950315     C                   MOVEL     'N'           V02SN1
136000950315     C                   MOVEL     'Ass'         V3DTPI
136100950315     C                   Z-ADD     WIMA          V3CIMP
136200990924     C                   MOVEL     WDIV          VH3DIV
136300950315     C                   MOVEL     ARBMIF        V3CTIC
136400950315     C                   WRITE     LR53S03
136500950315     C                   MOVEL     *BLANKS       V3CDES
136600950315     C                   END
136700950315     C*  Se esitono entrambi  Controllo se esiste C/Assegno
136800970416     C     WCAS          IFGT      0
136900990923     C     WVCA          ANDEQ     WDIV
137000950315     C     WIMA          ANDGT     0
137100950315     C                   SETON                                        05
137200950315     C                   ADD       1             NRR3
137300950315     C                   MOVEL     'N'           V02SN1
137400950315     C                   MOVEL     'Tot'         V3DTPI
137500970416     C     WCAS          ADD       WIMA          V3CIMP
137600990923     C                   MOVEL     WDIV          V3CVCA
137700990924     C                   MOVEL     WDIV          VH3DIV
137800950315     C                   MOVEL     *BLANKS       V3CTIC
137900950315     C                   WRITE     LR53S03
138000950315     C                   END
138100950315     C*
138200991012     C*  Se esitono entrambi verifico se scrivere riga totale:
138300991012     C     WCAS          IFGT      0
138400991012     C     WIMA          ANDGT     0
138500991012     C* .. Divisa di incasso assegnato è quella corrente...
138600011009     C     WDIV          IFEQ      §GEDCr
138700991012     C* .. o quella alternativa ..
138800991012     C     WDIV          OREQ      DIVEUR
138900991012     C* .. & la divisa di incasso C/Assegno è quella corrente...
139000011009     C     WVCA          IFEQ      §GEDCr
139100991012     C* .. o quella alternativa ..
139200991012     C     WVCA          OREQ      DIVEUR
139300991012     C                   SETON                                        05
139400991012     C                   ADD       1             NRR3
139500011009     C                   MOVEL     §GEDCr        V3DTPI
139600991012     C* Se l'assegnato non è in moneta di conto converto e sommo
139700991012     C* il risultato nel totale
139800011009     C     WDIV          IFNE      §GEDCr
139900991012     C                   CLEAR                   YEURDS
140000991012     C                   MOVEL     WDIV          YECDVI
140100991012     C                   Z-ADD     WIMA          YECIMI
140200011009     C                   MOVEL     §GEDCr        YECDVO
140300991012     C                   MOVEL     'H'           YECTAR
140400991012     C                   CALL      'YEURCO'
140500991012     C                   PARM                    YEURDS
140600991012     C                   Z-ADD     YECIMO        V3CIMP
140700991012     C                   ELSE
140800991012     C                   Z-ADD     WIMA          V3CIMP
140900991012     C                   END
141000991012     C* Se il C/Assegno non è in moneta di conto converto e sommo
141100991012     C* il risultato nel totale
141200011009     C     WVCA          IFNE      §GEDCr
141300991012     C                   CLEAR                   YEURDS
141400991012     C                   MOVEL     WVCA          YECDVI
141500991012     C                   Z-ADD     WCAS          YECIMI
141600011009     C                   MOVEL     §GEDCr        YECDVO
141700991012     C                   MOVEL     'H'           YECTAR
141800991012     C                   CALL      'YEURCO'
141900991012     C                   PARM                    YEURDS
142000991012     C                   ADD       YECIMO        V3CIMP
142100991012     C                   ELSE
142200991012     C                   ADD       WCAS          V3CIMP
142300991012     C                   END
142400011009     C                   MOVEL     §GEDCr        V3CVCA
142500011009     C                   MOVEL     §GEDCr        VH3DIV
142600991012     C                   MOVEL     *BLANKS       V3CTIC
142700991012     C                   WRITE     LR53S03
142800991012     C                   END
142900991012     C                   END
143000991012     C                   END
143100991012     C*
143200950315     C                   Z-ADD     NRR3          V2CNRA
143300950315     C                   END
143400950315     C*
143500950315     C*  Imposto i dati da archivio  P.C.
143600950315     C                   MOVEL     CDENO2        V2CFIR
143700950315     C                   MOVEL     CDENO1        V2CNT1
143800970107     C     CDEHMC        IFNE      9999
143900950321     C                   MOVEL     CDEHMC        V02HCO
144000950321     C                   MOVE      CDEHMC        V02MCO
144100970107     C                   END
144200950321     C                   MOVEL     ARBGC1        V021GC
144300950321     C                   MOVEL     ARBGC2        V022GC
144400950321     C*
144500950321     C     ARBGC1        IFNE      CDEGC1
144600950321     C     ARBGC2        ANDNE     CDEGC2
144700950322     C     ARBGC1        OREQ      CDEGC1
144800950322     C     ARBGC1        ANDEQ     *BLANKS
144900950322     C     ARBGC2        OREQ      CDEGC2
145000950322     C     ARBGC1        ANDEQ     *BLANKS
145100950321     C                   MOVEL     CDEGC1        V2CGC1
145200950321     C                   MOVEL     CDEGC2        V2CGC2
145300950321     C                   END
145400961118     C*  Gestisco consegne anomala
145500961118     C                   MOVEL     CDECCA        V02CCA
145600950321     C*  Gestisco consegne particolari
145700950321     C                   MOVEL     ARBTC1        V2CCP1
145800950321     C                   MOVEL     ARBTC1        V2HCP1
145900950321     C                   MOVEL     ARBTC2        V2CCP2
146000950321     C                   MOVEL     ARBTC2        V2HCP2
146100000210      *
146200000210     C                   SELECT
146300000210     C                   WHEN      CDEtc1 = *blanks
146400000210     C                   WHEN      ARBtc1 <> *blanks  and  ARBtc2 <> *blanks
146500000210     C                   WHEN      ARBtc1 = CDEtc1  or  ARBtc2 = CDEtc1
146600000210     C                   WHEN      ARBtc1 = *blanks
146700000210     C                   movel     CDETC1        V2CCP1
146800000210     C                   WHEN      ARBtc2 = *blanks
146900000210     C                   movel     CDETC1        V2CCP2
147000000210     C                   ENDSL
147100000210      *
147200950321     C                   CLEAR                   WLBDA8
147300950315     C     CDEDRC        IFNE      0
147400950315     C     CDEDCM        IFEQ      0
147500950315     C                   Z-ADD     CDEDRC        CDEDCM
147600950315     C                   END
147700950315     C                   MOVEL     'R  '         V02RIS
147800950315     C                   END
147900950315     C*  Se c'è la data di consegna o la data di riserva non considero
148000961128     C                   MOVEL     CDECMC        V02CCN
148100950315     C     CDEDCM        IFNE      0
148200950601     C                   Z-ADD     CDEDCM        G02INV
148300950315     C                   ELSE
148400950315     C     CDEDLA        IFGT      CDEDAG
148500990716     C     CDECMC        IFNE      'AV9'
148600950315     C                   MOVEL     'AVV'         V02CCN
148700990716     C                   ELSE
148800990716     C                   MOVEL     'AV9'         V02CCN
148900990716     C                   END
149000950315     C                   Z-ADD     CDEDLA        G02INV
149100950315     C                   ELSE
149200950315     C     CDECMC        IFNE      *BLANKS
149300950315     C                   MOVEL     CDECMC        V02CCN
149400950315     C                   ELSE
149500950315     C                   MOVEL     'G   '        V02CCN
149600950315     C                   END
149700950315     C                   Z-ADD     CDEDAG        G02INV
149800950315     C                   END
149900950315     C                   END
150000950315     C*
150100950315     C                   MOVEL     '3'           G02ERR
150200950315     C                   CALL      'XSRDA8'
150300950315     C                   PARM                    WLBDA8
150400950315     C                   MOVEL     G02DAT        V02DTC
150500950315     C*
150600950315     C                   WRITE     LR53S02
150700950315     C*
150800950315     C                   ENDSR
150900950315     C*-----------------------------------------------------*
151000950315     C     DETVOL        BEGSR
151100950315     C*-----------------------------------------------------*
151200950315     C* VOLUME : - SE ARBFVR   ("T"), VOLUME ARBVOL
151300950315     C*            ALTRIMENTI
151400950315     C*               - SE TOTALE   ("T"), VOLUME ARBVLS
151500950315     C*               - SE PARZIALE ("Z"), VOLUME ARBVLS (SE MAGGIORE)
151600950315     C*                               ALTRIMENTI SOMMO ARBVOL
151700950315     C     ARBFVF        IFEQ      'T'
151800950315     C                   Z-ADD     ARBVLF        WVOL
151900950315     C                   ELSE
152000050208     C     ARBNCR        ifeq      ARBNCL
152100950315     C                   Z-ADD     ARBVLC        WVOL
152200950315     C*
152300950315     C                   ELSE
152400950315     C     ARBVLC        IFGE      ARBVLF
152500950315     C                   Z-ADD     ARBVLC        WVOL
152600950315     C                   ELSE
152700950315     C                   Z-ADD     ARBVLF        WVOL
152800950315     C                   ENDIF
152900950315     C                   ENDIF
153000950315     C                   ENDIF
153100950315     C*
153200950315     C                   ENDSR
153300950322     C*-----------------------------------------------------*
153400950322     C* Ho confermato la chiusura distinta
153500950322     C*-----------------------------------------------------*
153600950322     C     CONFER        BEGSR
153700950322     C*
153800950322     C                   Z-ADD     V2CNDC        KNFV
153900950322     C     KFVV          CHAIN     FNFVV01L                           96
154000950322     C* Controllo se il flag chiusura è uguale a 'F' lo pulisco
154100950322     C  N96FVVFCF        IFEQ      'F'
154200950322     C                   MOVE      ' '           FVVFCF
154300950322     C                   EXCEPT    AGGFVV
154400950322     C                   END
154500950322     C                   MOVEL     'S'           WFINE
154600980323      *
154700980323      *  Cancello record da FNCDE e/o FNCDS.
154800980323      *
154900950322     C                   Z-ADD     D63NDT        KNDC
155000961127     C     KCDE          CHAIN     FNCDE01L                           31
155100020902      *
155200980324     C     *IN31         DOWEQ     '0'                                               -- 1
155300980323      *
155400980323     C                   Z-ADD     CDEAAS        KAAS
155500980323     C                   Z-ADD     CDELNP        KLNP
155600980323     C                   Z-ADD     CDENRS        KNRS
155700980323     C                   Z-ADD     CDENSP        KNSP
155800980323     C     KARB          SETLL     FNCDS003                               33
155900980323      *
156000980324      *  Cancello record da FNCDS se il numero distinta è impostato.
156100980324     C                   DO        *HIVAL                                            -- 2
156200980323     C     KARB          READE     FNCDS003                               33
156300980324     C  N33CDSNDC        IFEQ      CDENDC                                            -- 3
156400980323     C                   DELETE    FNCDS003
156500980324     C                   END                                                         3 --
156600980324     C  N33              END                                                         2 --
156700980324      * Controllo se restano RCD di FNCDS
156800980324     C     KARB          CHAIN     FNCDS003                           33
156900980324      *  Cancello FNCDE se non esistono record di FNCDS e se non è il secondo
157000980324      *    evento 20 - 117 con bolla senza il primo già confermato.
157100980324     C     CDEFL2        IFEQ      'S'                                               -- 2
157200961127     C                   MOVEL     *BLANKS       CDEFL2
157300961127     C                   MOVEL     *BLANKS       CDEERR
157400970225     C                   Z-ADD     0             CDENDC
157500980401     C                   MOVEL     *BLANKS       CDEFL3
157600961127     C                   EXCEPT    AGGFL2
157700980324     C                   ELSE                                                        -- 2 --
157800980324      *
157900980324     C     *IN33         IFEQ      '1'                                               -- 3
158000980324     C                   DELETE    FNCDE01L
158100980324     C                   ELSE                                                        -- 3 --
158200980324      *  Se resta solo un rcd di FNCDS a totale aggiorno FNCDE e cancello FNCDS
158300980324     C     CDSSEE        IFEQ      *BLANKS                                           -- 4
158400980324     C     KARB          READE     FNCDS003                               33
158500980324     C     *IN33         IFEQ      '0'                                               -- 5
158600980324     C                   EXSR      MUOVI
158700980324     C                   UPDATE    FNCDE000
158800980324     C                   DELETE    FNCDS003
158900980324     C                   END                                                         5 --
159000980401      *
159100980401      *  Se restano FNCDS a dettaglio ed CDEFL3 = 1 normalizzo FNCDE
159200980401     C                   ELSE                                                        -- 4 --
159300980401     C     CDEFL3        IFEQ      '1'                                               -- 5
159400980401     C                   EXSR      MUOVI0
159500980401     C                   UPDATE    FNCDE000
159600980401     C                   END                                                         5 --
159700980324     C                   END                                                         4 --
159800980324      *
159900980324     C                   END                                                         3 --
160000980324     C                   END                                                         2 --
160100980323      *
160200961127     C     KCDE          READE     FNCDE01L                               31
160300980324     C                   END                                                         1 --
160400980323      *
160500950322     C                   ENDSR
160600980324      *-----------------------------------------------------*
160700980324      *  Aggiono FNCDE con valori di FNCDS
160800980324      *-----------------------------------------------------*
160900980324     C     MUOVI         BEGSR
161000980324      *
161100980324     C                   Z-ADD     CDSDCM        CDEDCM
161200980324     C                   Z-ADD     CDSHMC        CDEHMC
161300980324     C                   Z-ADD     CDSDLA        CDEDLA
161400980324     C                   Z-ADD     CDSDAG        CDEDAG
161500980324     C                   Z-ADD     CDSDRC        CDEDRC
161600980324     C                   MOVEL     CDSNO1        CDENO1
161700980324     C                   MOVEL     CDSNO2        CDENO2
161800980324     C                   MOVEL     CDSCMC        CDECMC
161900980324     C                   MOVEL     CDSCCA        CDECCA
162000000210     C                   MOVEL     CDSTC1        CDETC1
162100980324     C                   MOVEL     CDSERR        CDEERR
162200980324     C                   MOVEL     CDSSTS        CDESTS
162300980324     C                   MOVEL     CDSCOD        CDECOD
162400980324     C                   MOVEL     CDSMOT        CDEMOT
162500980324     C                   MOVEL     CDSFL1        CDEFL1
162600980401     C                   MOVEL     *BLANKS       CDEFL3
162700980324      *
162800980324     C                   ENDSR
162900980401      *-----------------------------------------------------*
163000980401      *  Azzero FNCDE
163100980401      *-----------------------------------------------------*
163200980401     C     MUOVI0        BEGSR
163300980401      *
163400980401     C                   Z-ADD     *ZEROS        CDEDCM
163500980401     C                   Z-ADD     *ZEROS        CDEHMC
163600980401     C                   Z-ADD     *ZEROS        CDEDLA
163700980401     C                   Z-ADD     *ZEROS        CDEDAG
163800980401     C                   Z-ADD     *ZEROS        CDEDRC
163900980401     C                   MOVEL     *BLANKS       CDENO1
164000980401     C                   MOVEL     *BLANKS       CDENO2
164100980401     C                   MOVEL     *BLANKS       CDECMC
164200980401     C                   MOVEL     *BLANKS       CDECCA
164300000210     C                   MOVEL     *BLANKS       CDETC1
164400980401     C                   MOVEL     *BLANKS       CDEERR
164500980401     C                   MOVEL     *BLANKS       CDESTS
164600980401     C                   MOVEL     *BLANKS       CDECOD
164700980401     C                   MOVEL     *BLANKS       CDEMOT
164800980401     C                   MOVEL     *BLANKS       CDEFL1
164900980401     C                   MOVEL     *BLANKS       CDEFL2
165000980401     C                   MOVEL     *BLANKS       CDEFL3
165100980401      *
165200980401     C                   ENDSR
165300980324     C*-----------------------------------------------------*
165400980324     C*  Operazioni iniziali
165500980324     C*-----------------------------------------------------*
165600980324     C     *INZSR        BEGSR
165700980324      *
165800980324      * Apertura controllata dei files
165900950315     C                   OPEN      FNCDE01L                             16
166000950320     C  N16              OPEN      FNFVV01L                             16
166100070515     C  N16              OPEN      Fidst01L                             16
166200950313     C  N16              OPEN      FNARB01L                             16
166300051115     C  N16              OPEN      FIAR901L                             16
166400950313     C  N16              OPEN      FNART01L                             16
166500980324     C  N16              OPEN      FNCDS03L                             16
166600950313     C   16              GOTO      FINE                                         OPEN vado a fine
166700980324      *
166800950313     C*  Definizione chiavi
166900950315     C     KARB          KLIST
167000950313     C                   KFLD                    KAAS
167100950313     C                   KFLD                    KLNP
167200950313     C                   KFLD                    KNRS
167300950313     C                   KFLD                    KNSP
167400990923     C     KAR6          KLIST
167500990923     C                   KFLD                    KAAS
167600990923     C                   KFLD                    KLNP
167700990923     C                   KFLD                    KNRS
167800990923     C                   KFLD                    KNSP
167900990923     C                   KFLD                    KTRC
168000990923     C     KARI          KLIST
168100990923     C                   KFLD                    KAAS
168200990923     C                   KFLD                    KLNP
168300990923     C                   KFLD                    KNRS
168400990923     C                   KFLD                    KNSP
168500990923     C                   KFLD                    KTIP
168600961114     C*
168700961114     C     KGCA          KLIST
168800961114     C                   KFLD                    KAAS1
168900961114     C                   KFLD                    KLNP1
169000961114     C                   KFLD                    KNRS1
169100961114     C                   KFLD                    KNSP1
169200961114     C     KANM          KLIST
169300990519     C                   KFLD                    KAAS1
169400961114     C                   KFLD                    KLNP1
169500961114     C                   KFLD                    KNRS1
169600961114     C                   KFLD                    KNSP1
169700961114     C                   KFLD                    KCAA
169800950313     C*
169900950313     C     KFVV          KLIST
170000950313     C                   KFLD                    KNPG
170100950313     C                   KFLD                    KNFV
170200950313     C                   KFLD                    KFGS
170300070515     C*
170400070515     C     Kdst          KLIST
170500070515     C                   KFLD                    KNPG
170600070515     C                   KFLD                    KNFV6             6 0
170700070515     C                   KFLD                    KFGS
170800950313     C*
170900950315     C     KTAB          KLIST
171000950313     C                   KFLD                    KKUT
171100950313     C                   KFLD                    KCOD
171200991012     C     KTBE          KLIST
171300991012     C                   KFLD                    KCODE
171400991012     C                   KFLD                    KKEYE
171500950315     C*
171600950315     C     KCDE          KLIST
171700950315     C                   KFLD                    KLNA
171800950315     C                   KFLD                    KNDC
171900970417     C*
172000970417     C     KARB1         KLIST
172100970417     C                   KFLD                    KAAP
172200970417     C                   KFLD                    KLPP
172300970417     C                   KFLD                    KNRP
172400970417     C                   KFLD                    KNSPP
172500021204     C*
172600021204     C     KAPD          KLIST
172700021204     C                   KFLD                    APDTIP
172800021204     C                   KFLD                    APDPDR
172900021204     C                   MOVEL     'A'           APDTIP
173000110418     C     Kar5          KLIST
173100110418     C                   KFLD                    v2cAAS
173200110418     C                   KFLD                    v2cLNP
173300110418     C                   KFLD                    v2cNRS
173400110418     C                   KFLD                    v2cNSP
173500110418     c                   kfld                    t5trd
173600110418     C                   move      'GEN'         T5trd             3
173700950313     C*----------------------------------------------------*
173800950313     C*  Definizione delle variabili
173900950313     C     *LIKE         DEFINE    ARBAAS        KAAS
174000950313     C     *LIKE         DEFINE    ARBLNP        KLNP
174100950313     C     *LIKE         DEFINE    ARBNRS        KNRS
174200950313     C     *LIKE         DEFINE    ARBNSP        KNSP
174300990923     C     *LIKE         DEFINE    ARITIP        KTIP
174400950313     C     *LIKE         DEFINE    FVVNPG        KNPG
174500950313     C     *LIKE         DEFINE    FVVNFV        KNFV
174600950313     C     *LIKE         DEFINE    FVVFGS        KFGS
174700950313     C     *LIKE         DEFINE    TBLKUT        KKUT
174800950313     C     *LIKE         DEFINE    TBLCOD        KCOD
174900950315     C     *LIKE         DEFINE    ARBDDC        §DDC
175000950315     C     *LIKE         DEFINE    CDELNA        KLNA
175100950315     C     *LIKE         DEFINE    CDENDC        KNDC
175200950315     C     *LIKE         DEFINE    AR6IFT        WIMA
175300950315     C     *LIKE         DEFINE    ARBVLF        WVOL
175400970417     C     *LIKE         DEFINE    AR9CAS        WCAS
175500970417     C     *LIKE         DEFINE    AR9VCA        WVCA
175600990923     C     *LIKE         DEFINE    AR6DIV        WDIV
175700970417     C     *LIKE         DEFINE    AR9TIC        WTIC
175800990923     C     *LIKE         DEFINE    AR6TRC        KTRC
175900961114     C     *LIKE         DEFINE    LBLAAP        KAAS1
176000961114     C     *LIKE         DEFINE    ANMLNP        KLNP1
176100961114     C     *LIKE         DEFINE    ANMNRS        KNRS1
176200961114     C     *LIKE         DEFINE    ANMNSP        KNSP1
176300961114     C     *LIKE         DEFINE    ANMCAA        KCAA
176400970417     C     *LIKE         DEFINE    LBLAAP        KAAP
176500970417     C     *LIKE         DEFINE    LBLLPP        KLPP
176600970417     C     *LIKE         DEFINE    LBLNRP        KNRP
176700970417     C     *LIKE         DEFINE    LBLNSP        KNSPP
176800991012     C     *LIKE         DEFINE    TBECOD        KCODE
176900991012     C     *LIKE         DEFINE    TBEKE1        KKEYE
177000950313     C*----------------------------------------------------*
177100950313     C* Giro data del giorno
177200950313     C                   TIME                    WHHDAT           14 0
177300950317     C                   MOVE      WHHDAT        WOGGI             8 0
177400020828     C* Decodifica Elaboratore
177500020828     c     *dtaara       define    §azute        azuteds
177600020828     c     *dtaara       define    §datiute      ddatiute
177700020828     C                   in(E)     *dtaara
177800020828     C                   IF        %Error  or  RSUT = *blanks
177900020828     C                   call      'TIBS34R'
178000020828     C                   parm                    Tibs34Ds
178100020828     C                   in        *dtaara
178200020828     c                   ENDIF
178300950315     C*----------------------------------------------------*
178400950315     C*  Caricamento Tabella 7R
178500950315     C                   Z-ADD     0             X                 3 0
178600950317     C                   Z-ADD     1             KKUT
178700950315     C                   MOVEL     '7R'          KCOD
178800950315     C     KTAB          CHAIN     TABEL00F                           96
178900950315     C     *IN96         DOWEQ     '0'
179000950315     C     X             ANDLT     100
179100950315     C     TBLFLG        IFEQ      ' '
179200950315     C                   ADD       1             X
179300950315     C                   MOVEL     TBLKEY        C7R(X)
179400950315     C                   MOVEL     TBLUNI        D7R(X)
179500950315     C                   END
179600950315     C     KTAB          READE     TABEL00F                               96
179700950315     C                   END
179800950315     C*  Caricamento Tabella 3A
179900950315     C                   Z-ADD     0             X                 3 0
180000950315     C                   MOVEL     '3A'          KCOD
180100950315     C     KTAB          CHAIN     TABEL00F                           96
180200950315     C     *IN96         DOWEQ     '0'
180300950315     C     X             ANDLT     100
180400950315     C     TBLFLG        IFEQ      ' '
180500950315     C                   ADD       1             X
180600950315     C                   MOVEL     TBLKEY        C3A(X)
180700950315     C                   MOVEL     TBLUNI        D3A(X)
180800950315     C                   END
180900950315     C     KTAB          READE     TABEL00F                               96
181000950315     C                   END
181100990923     C* Caricamento tipo incasso
181200990923     C                   Z-ADD     0             X                 3 0
181300990923     C                   MOVEL     '1A'          KCOD
181400990923     C     KTAB          CHAIN     TABEL00F                           96
181500990923     C     *IN96         DOWEQ     '0'
181600990923     C     X             ANDLT     100
181700990923     C     TBLFLG        IFEQ      ' '
181800990923     C                   ADD       1             X
181900990923     C                   MOVEL     TBLKEY        C1A(X)
182000990923     C                   MOVEL     TBLUNI        D1A(X)
182100990923     C                   END
182200990923     C     KTAB          READE     TABEL00F                               96
182300990923     C                   END
182400991012     C*  Aggancio tabella 'GED' x reperire la divisa corrente
182500991012     C                   MOVEL     'GED'         KCODE
182600991012     C                   MOVEL     *BLANKS       KKEYE
182700991012     C                   MOVEL     '1'           KKEYE
182800991012     C     KTBE          CHAIN     TNTBE01L                           13
182900991012     C     *IN13         IFEQ      '0'
183000991012     C                   MOVEL     TBEUNI        DGED
183100991012     C                   END
183200991012     C*
183300991012     C*  CARICO DATI TABELLA CV
183400991012     C                   MOVEL     *BLANKS       DIVEUR
183500991012     C                   MOVEL     'CV'          KCOD
183600991012     C     KTAB          CHAIN     TABEL00F                           13
183700991012     C     *IN13         DOWEQ     '0'
183800991012     C                   MOVEL     TBLKEY        DIVCV             3
183900991012     C                   MOVEL     TBLUNI        DSCV
184000991012     C* Se divisa diversa dalla moneta di conto reperisco data
184100991012     C* validità x trovare altra divisa da utilizzare x incassi
184200011009     C     §GEDCR        IFNE      DIVCV
184300991012     C     DIVEUR        ANDEQ     *BLANKS
184400991012     C     §CVDVA        ANDGE     'S'
184500991012     C                   MOVEL     DIVCV         DIVEUR            3
184600991012     C                   END
184700991012     C     KTAB          READE     TABEL00F                               13
184800991012     C                   END
184900950313     C*----------------------------------------------------*
185000950315     C                   CLEAR                   FNLR53
185100950315     C                   Z-ADD     4             KNPG
185200950313     C                   MOVEL     ' '           WFINE             1
185300950313     C                   MOVEL     '2'           WTPVID            1
185400950320     C                   MOVEL     'N'           WCHIUS            1
185500950426     C                   MOVEL     'N'           WPGMER            1
185600950320     C                   MOVEL     'N'           WSBLOC            1
185700950313     C*----------------------------------------------------*
185800950313     C                   ENDSR
185900020830      *---------------------------------------------------------*
186000020830      *  reperisce il po di gestione                            *
186100020830      *---------------------------------------------------------*
186200020830     C     srfgs         BEGSR
186300020830     C*
186400020830     c                   clear                   fnlv55ds
186500020830     c                   eval      d55tpt = '6'
186600020830     c                   movel     d63lna        d55lin
186700020830     c                   movel     *date         dataiso
186800020830     c                   movel     dataiso       d55drf
186900020830     c                   call      'FNLV55R'
187000020902     c                   parm                    fnlv55ds
187100020830     C*
187200020830     C                   ENDSR
187300950315     C*----------------------------------------------------*
187400950315     OFNCDE000  E            AGGCDE
187500950315     O                       CDENDC
187600950606     O                       CDEDAG
187700950606     O                       CDEDCM
187800961127     OFNCDE000  E            AGGFL2
187900961127     O                       CDEFL2
188000961127     O                       CDEERR
188100970225     O                       CDENDC
188200950320     OFNCDE000  E            NOAGG
188300950315     OFNARB000  E            AGGARB
188400020829     O                       ARBifp
188500020829     O                       ARBNDC
188600961114     O                       ARBFBC
188700961114     O                       ARBCMC
188800950315     OFNARB000  E            AGGDIS
188900950315     O                       ARBPDC
189000950315     O                       ARBDDC
189100950315     O                       ARBNMI
189200950323     O                       ARBAMA
189300950315     OFNFVV000  E            AGGFVV
189400950315     O                       FVVFCF
189500980323      *
189600980323     OFNCDS003  E            AGGCDS
189700980323     O                       CDSNDC
189800980323     O                       CDSDAG
189900980323     O                       CDSDCM
190000980323      *----------------------------------------------------*
190100980323**
190200950314Codice padroncino non valido                                           01
190300950314Codice padroncino inesistente                                          02
190400950314Immettere data di consegna                                             03
190500950314Data di consegna errata                                                04
190600950314Immettere ora di consegna                                              05
190700950314Ora di consegna errata                                                 06
190800950314Numero di spedizione inesitente                                        07
190900950314Linea di arrivo diversa dalla bolla                                    08
191000950314Riferimento mittente numerico diverso                                  09
191100950314Bolla con numero distinta già impostato                                10
191200950314Bolla già consegnata                                                   11
191300950314Deve essere impostata almeno una data                                  12
191400950315Non chiudere dist. di questo padr.x conflitti! Riprovare fra 2 min.    13
191500950321Sped. non abilitata all'incasso del C/A                                14
191600950321Spedizione da tassare                                                  15
191700961119Spediz.con giacenza in fase non congrua                                16
191800000210Impossibile aggiungere Cons.Partic. - -                                17
191900950606Bolla con figlia !!!                                                   18
192000961121Immetti G=sped.giac./C=Conse./A=L.Avv.                                 19
192100961120Evento da analizzare !!                                                20
192200961118Immetti E se vuoi visualizzare note EDI                                21
192300961120Ricevuto secondo evento colli mancanti                                 22
192400961230Non ci sono spedizioni valide da chiudere in distinta                  23
192500980323Analizzare dettaglio segnacolli !!!                                    24
192600000925C/A ancora presente da annullare !!                                    25
192700001019Esistono altre bolle da estrarre dopo la chiusura ripetere l'operazione26
