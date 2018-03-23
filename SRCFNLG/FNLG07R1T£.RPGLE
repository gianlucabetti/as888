000100080423     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200140206     H*--------------------------------------------------------------*
000300140206     H*
000400170113     H* crea file spia per invio sms/mail ai destinatari in seguito a apertura
000500080423     H*
000600000000     H*--------------------------------------------------------------*
000700170113     FFnarb01l  if   E           k DISK
000800170203     Ftigcp01l  if   E           k DISK
000900170117     Fazorg01l  if   E           k DISK
001000170117     Ftntbe01l  if   E           k DISK
001100170113     Ftabel00f  if   E           k DISK
001200170113     FFIsia00F  o    E             DISK
001300170206     d kpjba         e ds
001400140206     d fnlv81ds      e ds
001500140212     d fnlv82ds      e ds
001600170116     d fnlg071ds     e ds
001700170206     d tibs02ds      e ds
001800170206     d dvpolg07      e ds
001900170206     D  fil                    1     60  0   dim(20)
002000170113     d dsiagia       e ds
002100170203     d ds3a          e ds
002200170116     d ds2a          e ds
002300170119     d ds2d          e ds
002400170206     d ds2z          e ds
002500170117     d dntw          e ds
002600170117     d og143         e ds
002700140212     d tipa            s              3
002800140213     d dataiso         s               d   datfmt(*iso)
002900140213     D dateu           S              8  0
003000080423     d
003100170113      *----------------------------------------------------------------
003200170117     c     *entry        plist
003300170206     c                   parm                    kpjba
003400170117     c                   parm                    fnlg071ds
003500170206      *?   verifica se filiale abilitata
003600170206      * Aggancio tabella "VPO" "FNLg07R1" per recupero filiali attive
003700170206     C                   CLEAR                   tibs02ds
003800170206     c                   clear                   dvpolg07
003900170206     C                   MOVEL     'C'           T02MOD
004000170206     C                   MOVEL     KNSIF         T02SIF
004100170206     C                   MOVEL     'VPO'         T02COD
004200170206     c                   movel(P)  'LG07'        t02ke1
004300170206     C                   CALL      'TIBS02R'
004400170206     C                   PARM                    KPJBA
004500170206     C                   PARM                    tibs02ds
004600170206    2C                   IF        T02ERR = *BLANKS
004700170206     C                   MOVEL     T02uni        dvpolg07
004800170206    2C                   ENDIF
004900170208     c     999           lookup    fil                                    90
005000170208     c  n90i071lna       lookup    fil                                    90
005100170206     c  n90              return
005200170206     c     karb          chain     tigcp01l
005300170203     c                   if        %found(tigcp01l)
005400170209     c                   movel     gcpaas        o071dap
005500170210     c                   move      gcpmgc        o071dap
005600170209     c                   eval      O071fgc =gcpfgc
005700170209     c                   eval      O071Ngc =gcpngc
005800170203     c                   if        gcpfas <>  20
005900170203     c                   move      'E'           o071esito
006000170203     c                   eval      o071msg = 'Giacenza in fase non congrua'
006100170203     c                   return
006200170203     c                   endif
006300170203     c                   endif
006400170203     c                   if        i071chiA = *BLANK and
006500170203     c                             i071chi <> 'A'
006600170203     c                   move      'E'           o071esito
006700170203     c                   eval      o071msg = 'Modalità scrittura ammessa -
006800170203     c                             solo in arrivo'
006900170203     c                   return
007000170203     c                   endif
007100170117     c
007200170206     c     i071lna       chain     azorg01l
007300170117     c                   if        %found(azorg01l)
007400170117     c                   movel     orgde3        og143
007500170117      * reperimento tabella NTW per verificare ntw estero
007600170117     c                   clear                   dntw
007700170117     c                   move      'NTW'         tbecod
007800170117     c                   movel(p)  §ogntw        tbeke1
007900170117     c                   move      *blank        tbeke2
008000170117     c     ktbe          chain     tntbe01l
008100170117     c                   if        %found(tntbe01l)
008200170117     c                   movel     tbeuni        dntw
008300170117     c                   endif
008400170117     c                   endif
008500170117      * se diverso da ITALIA(blank) esce
008600170117     c                   if        §NTWFIE <> *blank
008700170203     c                   move      'E'           o071esito
008800170203     c                   eval      o071msg = 'Spedizione con linea arrivo -
008900170203     c                             estera'
009000170117     c                   return
009100170117     c                   endif
009200170117     c*
009300140206     c* il pgm può essere chiamato in inserimento o in cancellazione
009400170206      *?   SE RICHIAMATO PER SCRITTURA / oppure solo controllo
009500170206     c                   if        i071chiA = *BLANK
009600140206     c                   exsr      scrivo
009700170206     c                   else
009800170206     c                   exsr      controllo
009900170206     c                   endif
010000140206     c                   seton                                        lr
010100140206     c**********************************************************************
010200140206     c     scrivo        begsr
010300140206     c**********************************************************************
010400170207     c                   clear                   FISIA000
010500170207     c                   clear                   dsiagia
010600140206     C* verifico se ho l'indirizzo mail o il telefono del destinatario
010700170203      * ds note
010800170203     c     karb          chain     fnarb01l
010900170203     c                   if        %found(fnarb01l)
011000170206      *verifica se assegnato
011100170206     c                   clear                   assegnato         1
011200170206     C                   MOVEL     '3A'          tblcod
011300170206     c                   movel(p)  arbcbo        tblkey
011400170206     c     ktab          chain     tabel00f
011500170206     c                   if        %found(tabel00f)
011600170206     c                   eval      ds3a  = tbluni
011700170206     c                   movel     §3atb1        assegnato
011800170206     c                   endif
011900170203      * verifico se per codice cliente è ammessa la gestione in arrivo
012000170203      * tabella 2D
012100170203     c                   exsr      contr2D
012200170203      * se non è ammessa gestione per la bolla di nessuna tipologia
012300170203      * di disposizione non mando avvisi al destinatario
012400170203     c                   if        noarrivo = 3
012500170203     c                   move      'E'           o071esito
012600170203     c                   eval      o071msg = 'Non sono ammesse disposizioni -
012700170206     c                             per la spedizione'
012800170203     c                   return
012900170206     c                   endif
013000170203      *
013100170203     c                   if        arbffd = 'S'
013200170203     c                   move      'E'           o071esito
013300170203     c                   eval      o071msg = 'Spedizione in fermo deposito'
013400170203     c                   return
013500170203     c                   endif
013600170210     c                   if        gcpdur <> o071dap
013700170203     c                   move      'E'           o071esito
013800170203     c                   eval      o071msg = 'Giacenza in riapertura '
013900170203     c                   return
014000170203     c                   endif
014100170209     c                   eval      §SIADAP = O071dap
014200170209     c                   eval      §SIAFGC = O071fgc
014300170209     c                   eval      §SIANGC = O071Ngc
014400170203     c                   eval      §SIACMC = i071cmc
014500170203      *
014600170203     C                   MOVEL     '2A'          tblcod
014700170206     c                   movel     i071cmc       tblkey
014800170203     c     ktab          chain     tabel00f
014900170203     c                   if        %found(tabel00f)
015000170203     c                   eval      ds2a  = tbluni
015100170203     c                   eval      §SIACMCD =§2ades
015200170203     c                   endif
015300170203     C                   MOVEL     '2Z'          tblcod
015400170206     c                   movel     i071cmc       tblkey
015500170203     c     ktab          chain     tabel00f
015600170203     c                   if        %found(tabel00f)
015700170203     c                   eval      ds2z  = tbluni
015800170210     c                   if        §2ZINVAPE <> 'S'
015900170203     c                   move      'E'           o071esito
016000170203     c                   eval      o071msg = 'Codice mancata consegna non -
016100170203     c                             abilitato'
016200170203     c                   return
016300170203     c                   endif
016400170203     c                   endif
016500170203     c                   eval      SIAnote= Dsiagia
016600140206     c                   clear                   fnlv81ds
016700170203     c                   eval      ILV81AAS = i071AAS
016800170203     c                   eval      ILV81lnp = i071LNP
016900170203     c                   eval      ILV81nrs = i071NRS
017000170203     c                   eval      ILV81nsp = i071NSP
017100170203     c                   eval      ILV81trc = 'G'
017200140206     c                   call      'FNLV81R'
017300140206     c                   parm                    fnlv81ds
017400140206     c                   if        OLV81EML = ' ' and OLV81tel = ' '
017500170203     c                   move      'E'           o071esito
017600170203     c                   eval      o071msg = 'Non presente mail e telefono-
017700170203     c                              destinatario'
017800140206     c                   return
017900170206     c                   endif
018000140206     c* controllo se è già presente la spia
018100140212     c                   clear                   fnlv82ds
018200170203     c                   eval      ILV82AAS = i071AAS
018300170203     c                   eval      ILV82lnp = i071LNP
018400170203     c                   eval      ILV82nrs = i071NRS
018500170203     c                   eval      ILV82nsp = i071NSP
018600170203     c                   eval      ILV82tipa= 'G'
018700140212     c                   call      'FNLV82R'
018800140212     c                   parm                    fnlv82ds
018900140212     c                   if        OLV82STS <>'9'
019000170203     c                   move      'E'           o071esito
019100170203     c                   eval      o071msg = 'Spia già presente non -
019200170203     c                             inserita'
019300140206     c                   return
019400170206     c                   endif
019500140206     c* scrivo la spia
019600140508      * salvo flag se esiste rekord
019700140508      *
019800140206     c                   time                    SIADAORI
019900170206     c                   eval      SIAAAS = i071AAS
020000170206     c                   eval      SIALNP = i071LNP
020100170206     c                   eval      SIANRS = i071NRS
020200170206     c                   eval      SIANSP = i071NSP
020300170206     c                   eval      SIATIPA= 'AGD'
020400140206     c                   eval      SIASTS ='0'
020500170113      *
020600170113     c                   write     FISIA000                             01
020700140206     c*
020800170206     C                   endif
020900140206     C                   endsr
021000170119     c**********************************************************************
021100170119     c     contr2D       begsr
021200170119     c**********************************************************************
021300170119     C                   z-add     0             noarrivo          1 0
021400170119     C                   MOVE      ARBKSC        §8899             4 0
021500170119     c* consegna
021600170119     C                   MOVEL     '2D'          TBLCOD
021700170119     C                   MOVEL     *BLANK        TBLKEY
021800170119     C                   MOVEL     'C'           TBLKEY
021900170119     c     1             do        3
022000170119     C     KTAB          CHAIN     TABEL00f
022100170119     c                   if        %found(tabel00f)
022200170119     C                   MOVEL     TBLUNI        DS2D
022300170119     c* mai
022400170119     C     §2DUTA        IFEQ      'N'
022500170119     c* no 88 e 99
022600170119     C     §2DUTA        OREQ      'V'
022700170119     c* e cli.8888
022800170119     C     §8899         ANDEQ     8888
022900170119     c* no 88 e 99
023000170119     C     §2DUTA        OREQ      'V'
023100170119     c* e cli.9999
023200170119     C     §8899         ANDEQ     9999
023300170119     c* no 88
023400170119     C     §2DUTA        OREQ      '8'
023500170119     c* e cli.8888
023600170119     C     §8899         ANDEQ     8888
023700170119     c* no 99
023800170119     C     §2DUTA        OREQ      '9'
023900170119     c* e cli.9999
024000170119     C     §8899         ANDEQ     9999
024100170206      * assegnato e dirottamento
024200170206     C     tblkey        OReq      'D'
024300170206     c     assegnato     andeq     'A'
024400170119     C                   add       1             noarrivo
024500170119     C                   END
024600170119     C                   endif
024700170119     c                   select
024800170119     c                   when      tblkey = 'C' and noarrivo = 0
024900170203     c                   leavesr
025000170119     c                   when      tblkey = 'C'
025100170119     c                   movel     'D'           tblkey
025200170119     c                   iter
025300170119     c                   when      tblkey = 'D'
025400170119     c                   movel     'R'           tblkey
025500170119     c                   iter
025600170119     c                   endsl
025700170119     c                   enddo
025800170119     C                   endsr
025900170206     c**********************************************************************
026000170206     c     controllo     begsr
026100170206     c**********************************************************************
026200170206     C* verifico se ho l'indirizzo mail o il telefono del destinatario
026300170206      * ds note
026400170206     c     karb          chain     fnarb01l
026500170206     c                   if        %found(fnarb01l)
026600170206      *verifica se assegnato
026700170206     c                   clear                   assegnato         1
026800170206     C                   MOVEL     '3A'          tblcod
026900170206     c                   movel(p)  arbcbo        tblkey
027000170206     c     ktab          chain     tabel00f
027100170206     c                   if        %found(tabel00f)
027200170206     c                   eval      ds3a  = tbluni
027300170206     c                   movel     §3atb1        assegnato
027400170206     c                   endif
027500170206      * verifico se per codice cliente è ammessa la gestione
027600170206      * tabella 2D
027700170206     c                   move      'S'           o071disC
027800170206     c                   move      'S'           o071disD
027900170206     c                   move      'S'           o071disR
028000170206     c                   exsr      contr2Ddet
028100170206      *se spedizione in assegnato e arrivo forzo comunque NO al dirottamento
028200170206     c                   if        o071disD = 'S' and
028300170206     c                             i071chi = 'A' and
028400170206     c                             assegnato = 'A'
028500170206     c                   move      'N'           o071disD
028600170206     c                   endif
028700170206     c                   endif
028800170206     c                   endsr
028900170206     c**********************************************************************
029000170206     c     contr2Ddet    begsr
029100170206     c**********************************************************************
029200170206     C                   MOVE      ARBKSC        §8899             4 0
029300170206     c* consegna
029400170206     C                   MOVEL     '2D'          TBLCOD
029500170206     C                   MOVEL     *BLANK        TBLKEY
029600170206     C                   MOVEL     'C'           TBLKEY
029700170206     c     1             do        3
029800170206     C     KTAB          CHAIN     TABEL00f
029900170206     c                   if        %found(tabel00f)
030000170206     C                   MOVEL     TBLUNI        DS2D
030100170206      *?  Arrivo
030200170206     c                   if        i071chi = 'A'
030300170206     c* mai
030400170206     C     §2DUTA        IFEQ      'N'
030500170206     c* no 88 e 99
030600170206     C     §2DUTA        OREQ      'V'
030700170206     c* e cli.8888
030800170206     C     §8899         ANDEQ     8888
030900170206     c* no 88 e 99
031000170206     C     §2DUTA        OREQ      'V'
031100170206     c* e cli.9999
031200170206     C     §8899         ANDEQ     9999
031300170206     c* no 88
031400170206     C     §2DUTA        OREQ      '8'
031500170206     c* e cli.8888
031600170206     C     §8899         ANDEQ     8888
031700170206     c* no 99
031800170206     C     §2DUTA        OREQ      '9'
031900170206     c* e cli.9999
032000170206     C     §8899         ANDEQ     9999
032100170208      *?  valore non ammesso Nego abilitazione e passo al successivo
032200170206     c                   select
032300170206     c                   when      tblkey = 'C'
032400170206     c                   move      'N'           o071disC
032500170206     c                   movel     'D'           tblkey
032600170206     c                   when      tblkey = 'D'
032700170206     c                   move      'N'           o071disD
032800170206     c                   movel     'R'           tblkey
032900170206     c                   when      tblkey = 'R'
033000170206     c                   move      'N'           o071disR
033100170206     c                   leavesr
033200170206     c                   endsl
033300170208     c                   else
033400170208      *?  valore ammesso passo al successivo
033500170208     c                   select
033600170208     c                   when      tblkey = 'C'
033700170208     c                   movel     'D'           tblkey
033800170208     c                   when      tblkey = 'D'
033900170208     c                   movel     'R'           tblkey
034000170208     c                   when      tblkey = 'R'
034100170208     c                   leavesr
034200170208     c                   endsl
034300170208     C                   ENDif
034400170206     c                   else
034500170206      *?    Partenza
034600170206     C     §2DUTp        IFEQ      'N'
034700170206     c* no 88 e 99
034800170206     C     §2DUTp        OREQ      'V'
034900170206     c* e cli.8888
035000170206     C     §8899         ANDEQ     8888
035100170206     c* no 88 e 99
035200170206     C     §2DUTp        OREQ      'V'
035300170206     c* e cli.9999
035400170206     C     §8899         ANDEQ     9999
035500170206     c* no 88
035600170206     C     §2DUTp        OREQ      '8'
035700170206     c* e cli.8888
035800170206     C     §8899         ANDEQ     8888
035900170206     c* no 99
036000170206     C     §2DUTp        OREQ      '9'
036100170206     c* e cli.9999
036200170206     C     §8899         ANDEQ     9999
036300170208      *?  valore non ammesso Nego abilitazione e passo al successivo
036400170206     c                   select
036500170206     c                   when      tblkey = 'C'
036600170206     c                   move      'N'           o071disC
036700170206     c                   movel     'D'           tblkey
036800170206     c                   when      tblkey = 'D'
036900170206     c                   move      'N'           o071disD
037000170206     c                   movel     'R'           tblkey
037100170206     c                   when      tblkey = 'R'
037200170206     c                   move      'N'           o071disR
037300170206     c                   leavesr
037400170206     c                   endsl
037500170208     c                   else
037600170208      *?  valore ammesso passo al successivo
037700170208     c                   select
037800170208     c                   when      tblkey = 'C'
037900170208     c                   movel     'D'           tblkey
038000170208     c                   when      tblkey = 'D'
038100170208     c                   movel     'R'           tblkey
038200170208     c                   when      tblkey = 'R'
038300170208     c                   leavesr
038400170208     c                   endsl
038500170206     C                   END
038600170206     C                   endif
038700170206     C                   endif
038800170206     c                   enddo
038900170206     C                   endsr
039000140206     c**********************************************************************
039100140206     c     *inzsr        begsr
039200140206     c**********************************************************************
039300170113     c     karb          klist
039400170206     c                   kfld                    i071AAS
039500170206     c                   kfld                    i071LNP
039600170206     c                   kfld                    i071NRS
039700170206     c                   kfld                    i071NSP
039800170113     c     ktab          klist
039900170113     c                   kfld                    TBLKUT
040000170113     c                   kfld                    TBLCOD
040100170113     c                   kfld                    TBLKEY
040200170113     c                   z-add     1             TBLKUT
040300170117     c     ktbe          klist
040400170117     c                   kfld                    tbecod
040500170117     c                   kfld                    tbeke1
040600170117     c                   kfld                    tbeke2
040700140206     c                   endsr
