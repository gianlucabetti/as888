000100080423     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200140206     H*--------------------------------------------------------------*
000300140206     H*
000400170113     H* crea file spia per invio sms/mail ai destinatari in seguito a apertura
000500080423     H*
000600000000     H*--------------------------------------------------------------*
000700170113     FFnarb01l  if   E           k DISK
000800170227     FFnlbl01l  if   E           k DISK
000900170227     Ftigcp51l  if   E           k DISK
001000170117     Fazorg01l  if   E           k DISK
001100170117     Ftntbe01l  if   E           k DISK
001200170113     Ftabel00f  if   E           k DISK
001300170113     FFIsia00F  o    E             DISK
001400170206     d kpjba         e ds
001500140206     d fnlv81ds      e ds
001600140212     d fnlv82ds      e ds
001700170116     d fnlg071ds     e ds
001800170206     d tibs02ds      e ds
001900170206     d dvpolg07      e ds
002000170206     D  fil                    1     60  0   dim(20)
002100170113     d dsiagia       e ds
002200170203     d ds3a          e ds
002300170116     d ds2a          e ds
002400170119     d ds2d          e ds
002500170206     d ds2z          e ds
002600170117     d dntw          e ds
002700170117     d og143         e ds
002800170227     d wAAS            s                   like(gcpaas)
002900170227     d wLNP            s                   like(gcplnp)
003000170227     d wNRS            s                   like(gcpnrs)
003100170227     d wNSP            s                   like(gcpnsp)
003200170227     d tipa            s              3
003300140213     d dataiso         s               d   datfmt(*iso)
003400140213     D dateu           S              8  0
003500080423     d
003600170113      *----------------------------------------------------------------
003700170117     c     *entry        plist
003800170206     c                   parm                    kpjba
003900170117     c                   parm                    fnlg071ds
004000170224      *?   verifica se filiale abilitata SE RICHIAMATO PER SCRITTURA
004100170206      * Aggancio tabella "VPO" "FNLg07R1" per recupero filiali attive
004200170224     c                   if        i071chia = *blank
004300170206     C                   CLEAR                   tibs02ds
004400170206     c                   clear                   dvpolg07
004500170206     C                   MOVEL     'C'           T02MOD
004600170206     C                   MOVEL     KNSIF         T02SIF
004700170206     C                   MOVEL     'VPO'         T02COD
004800170206     c                   movel(P)  'LG07'        t02ke1
004900170206     C                   CALL      'TIBS02R'
005000170206     C                   PARM                    KPJBA
005100170206     C                   PARM                    tibs02ds
005200170206    2C                   IF        T02ERR = *BLANKS
005300170206     C                   MOVEL     T02uni        dvpolg07
005400170206    2C                   ENDIF
005500170208     c     999           lookup    fil                                    90
005600170208     c  n90i071lna       lookup    fil                                    90
005700170206     c  n90              return
005800170224     c                   endif
005900170227     c     karb          chain     fnlbl01l
006000170227     c                   if        %found(fnlbl01l)
006100170227     c                   z-add     LBLAAO        wAAS
006200170227     c                   z-add     LBLLPO        wLNP
006300170227     c                   z-add     LBLNRO        wNRS
006400170227     c                   z-add     LBLNSO        wNSP
006500170227     c                   else
006600170227     c                   z-add     i071AAS       wAAS
006700170227     c                   z-add     i071LNP       wLNP
006800170227     c                   z-add     i071NRS       wNRS
006900170227     c                   z-add     i071NSP       wNSP
007000170227     c                   endif
007100170227     c     kgcp          chain     tigcp51l
007200170227     c                   if        %found(tigcp51l)
007300170214     c                   movel     gcpagc        o071dap
007400170210     c                   move      gcpmgc        o071dap
007500170209     c                   eval      O071fgc =gcpfgc
007600170209     c                   eval      O071Ngc =gcpngc
007700170222     c                   if        gcpfas <>  20
007800170203     c                   move      'E'           o071esito
007900170203     c                   eval      o071msg = 'Giacenza in fase non congrua'
008000170203     c                   return
008100170203     c                   endif
008101171129     c                   else
008102171129     c                   return
010300170203     c                   endif
010400170203     c                   if        i071chiA = *BLANK and
010500170203     c                             i071chi <> 'A'
010600170203     c                   move      'E'           o071esito
010700170203     c                   eval      o071msg = 'Modalità scrittura ammessa -
010800170203     c                             solo in arrivo'
010900170203     c                   return
011000170203     c                   endif
011100170117     c
011200170405      *eseguyo controllo del network se chiamato da arrivo
011300170405     c                   if        i071chi = 'A'
011400170206     c     i071lna       chain     azorg01l
011500170117     c                   if        %found(azorg01l)
011600170117     c                   movel     orgde3        og143
011700170117      * reperimento tabella NTW per verificare ntw estero
011800170117     c                   clear                   dntw
011900170117     c                   move      'NTW'         tbecod
012000170117     c                   movel(p)  §ogntw        tbeke1
012100170117     c                   move      *blank        tbeke2
012200170117     c     ktbe          chain     tntbe01l
012300170117     c                   if        %found(tntbe01l)
012400170117     c                   movel     tbeuni        dntw
012500170117     c                   endif
012600170117     c                   endif
012700170117      * se diverso da ITALIA(blank) esce
012800170117     c                   if        §NTWFIE <> *blank
012900170203     c                   move      'E'           o071esito
013000170203     c                   eval      o071msg = 'Spedizione con linea arrivo -
013100170203     c                             estera'
013200170117     c                   return
013300170117     c                   endif
013400170405     c                   endif
013500170117     c*
013600140206     c* il pgm può essere chiamato in inserimento o in cancellazione
013700170206      *?   SE RICHIAMATO PER SCRITTURA / oppure solo controllo
013800170206     c                   if        i071chiA = *BLANK
013900140206     c                   exsr      scrivo
014000170206     c                   else
014100170206     c                   exsr      controllo
014200170206     c                   endif
014300140206     c                   seton                                        lr
014400140206     c**********************************************************************
014500140206     c     scrivo        begsr
014600140206     c**********************************************************************
014700170207     c                   clear                   FISIA000
014800170207     c                   clear                   dsiagia
014900140206     C* verifico se ho l'indirizzo mail o il telefono del destinatario
015000170203      * ds note
015100170203     c     karb          chain     fnarb01l
015200170203     c                   if        %found(fnarb01l)
015300170206      *verifica se assegnato
015400170206     c                   clear                   assegnato         1
015500170206     C                   MOVEL     '3A'          tblcod
015600170206     c                   movel(p)  arbcbo        tblkey
015700170206     c     ktab          chain     tabel00f
015800170206     c                   if        %found(tabel00f)
015900170206     c                   eval      ds3a  = tbluni
016000170206     c                   movel     §3atb1        assegnato
016100170206     c                   endif
016200170203      * verifico se per codice cliente è ammessa la gestione in arrivo
016300170203      * tabella 2D
016400170203     c                   exsr      contr2D
016500170203      * se non è ammessa gestione per la bolla di nessuna tipologia
016600170203      * di disposizione non mando avvisi al destinatario
016700170203     c                   if        noarrivo = 3
016800170203     c                   move      'E'           o071esito
016900170203     c                   eval      o071msg = 'Non sono ammesse disposizioni -
017000170206     c                             per la spedizione'
017100170203     c                   return
017200170206     c                   endif
017300170203      *
017400170203     c                   if        arbffd = 'S'
017500170203     c                   move      'E'           o071esito
017600170203     c                   eval      o071msg = 'Spedizione in fermo deposito'
017700170203     c                   return
017800170203     c                   endif
017900170210     c                   if        gcpdur <> o071dap
018000170203     c                   move      'E'           o071esito
018100170203     c                   eval      o071msg = 'Giacenza in riapertura '
018200170203     c                   return
018300170203     c                   endif
018400170330      *?  CHIODO temporaneo per allinearsi al 778 che non fa gestire
018500170330      * la giacenza se non c'è comunicazione al mittente ne stampa ne vag
018600170330     c                   if        gcptcm = 'Z'
018700170330     c                   move      'E'           o071esito
018800170330     c                   eval      o071msg = 'No invio comunicazione '
018900170330     c                   return
019000170330     c                   endif
019100170209     c                   eval      §SIADAP = O071dap
019200170209     c                   eval      §SIAFGC = O071fgc
019300170209     c                   eval      §SIANGC = O071Ngc
019400170203     c                   eval      §SIACMC = i071cmc
019500170203      *
019600170203     C                   MOVEL     '2A'          tblcod
019700170206     c                   movel     i071cmc       tblkey
019800170203     c     ktab          chain     tabel00f
019900170203     c                   if        %found(tabel00f)
020000170203     c                   eval      ds2a  = tbluni
020100170203     c                   eval      §SIACMCD =§2ades
020200170203     c                   endif
020300170203     C                   MOVEL     '2Z'          tblcod
020400170206     c                   movel     i071cmc       tblkey
020500170203     c     ktab          chain     tabel00f
020600170203     c                   if        %found(tabel00f)
020700170203     c                   eval      ds2z  = tbluni
020800170210     c                   if        §2ZINVAPE <> 'S'
020900170203     c                   move      'E'           o071esito
021000170203     c                   eval      o071msg = 'Codice mancata consegna non -
021100170203     c                             abilitato'
021200170203     c                   return
021300170203     c                   endif
021400170203     c                   endif
021500170203     c                   eval      SIAnote= Dsiagia
021600140206     c                   clear                   fnlv81ds
021700170221     c                   eval      ILV81AAS = gcpAASa
021800170221     c                   eval      ILV81lnp = gcpLNPa
021900170221     c                   eval      ILV81nrs = gcpnrsa
022000170221     c                   eval      ILV81nsp = gcpNSPa
022100170210     c                   eval      ILV81trc = 'A'
022200140206     c                   call      'FNLV81R'
022300140206     c                   parm                    fnlv81ds
022400140206     c                   if        OLV81EML = ' ' and OLV81tel = ' '
022500170203     c                   move      'E'           o071esito
022600170203     c                   eval      o071msg = 'Non presente mail e telefono-
022700170203     c                              destinatario'
022800140206     c                   return
022900170206     c                   endif
023000140206     c* controllo se è già presente la spia
023100140212     c                   clear                   fnlv82ds
023200170317     c                   eval      ILV82AAS = gcpAASa
023300170317     c                   eval      ILV82lnp = gcpLNPa
023400170317     c                   eval      ILV82nrs = gcpnrsa
023500170317     c                   eval      ILV82nsp = gcpNSPa
023600170210     c                   eval      ILV82tipa= 'AGD'
023700140212     c                   call      'FNLV82R'
023800140212     c                   parm                    fnlv82ds
023900140212     c                   if        OLV82STS <>'9'
024000170203     c                   move      'E'           o071esito
024100170203     c                   eval      o071msg = 'Spia già presente non -
024200170203     c                             inserita'
024300140206     c                   return
024400170206     c                   endif
024500140206     c* scrivo la spia
024600140508      * salvo flag se esiste rekord
024700140508      *
024800140206     c                   time                    SIADAORI
024900170317     c                   eval      SIAAAS = gcpAASa
025000170317     c                   eval      SIALNP = gcpLNPa
025100170317     c                   eval      SIANRS = gcpnrsa
025200170317     c                   eval      SIANSP = gcpNSPa
025300170206     c                   eval      SIATIPA= 'AGD'
025400140206     c                   eval      SIASTS ='0'
025500170113      *
025600170113     c                   write     FISIA000                             01
025700140206     c*
025800170206     C                   endif
025900140206     C                   endsr
026000170119     c**********************************************************************
026100170119     c     contr2D       begsr
026200170119     c**********************************************************************
026300170119     C                   z-add     0             noarrivo          1 0
026400170119     C                   MOVE      ARBKSC        §8899             4 0
026500170119     c* consegna
026600170119     C                   MOVEL     '2D'          TBLCOD
026700170119     C                   MOVEL     *BLANK        TBLKEY
026800170119     C                   MOVEL     'C'           TBLKEY
026900170119     c     1             do        3
027000170119     C     KTAB          CHAIN     TABEL00f
027100170119     c                   if        %found(tabel00f)
027200170119     C                   MOVEL     TBLUNI        DS2D
027300170119     c* mai
027400170119     C     §2DUTA        IFEQ      'N'
027500170119     c* no 88 e 99
027600170119     C     §2DUTA        OREQ      'V'
027700170119     c* e cli.8888
027800170119     C     §8899         ANDEQ     8888
027900170119     c* no 88 e 99
028000170119     C     §2DUTA        OREQ      'V'
028100170119     c* e cli.9999
028200170119     C     §8899         ANDEQ     9999
028300170119     c* no 88
028400170119     C     §2DUTA        OREQ      '8'
028500170119     c* e cli.8888
028600170119     C     §8899         ANDEQ     8888
028700170119     c* no 99
028800170119     C     §2DUTA        OREQ      '9'
028900170119     c* e cli.9999
029000170119     C     §8899         ANDEQ     9999
029100170206      * assegnato e dirottamento
029200170206     C     tblkey        OReq      'D'
029300170206     c     assegnato     andeq     'A'
029400170119     C                   add       1             noarrivo
029500170119     C                   END
029600170119     C                   endif
029700170119     c                   select
029800170119     c                   when      tblkey = 'C' and noarrivo = 0
029900170203     c                   leavesr
030000170119     c                   when      tblkey = 'C'
030100170119     c                   movel     'D'           tblkey
030200170119     c                   iter
030300170119     c                   when      tblkey = 'D'
030400170119     c                   movel     'R'           tblkey
030500170119     c                   iter
030600170119     c                   endsl
030700170119     c                   enddo
030800170119     C                   endsr
030900170206     c**********************************************************************
031000170206     c     controllo     begsr
031100170206     c**********************************************************************
031200170206     C* verifico se ho l'indirizzo mail o il telefono del destinatario
031300170206      * ds note
031400170206     c     karb          chain     fnarb01l
031500170206     c                   if        %found(fnarb01l)
031600170206      *verifica se assegnato
031700170206     c                   clear                   assegnato         1
031800170206     C                   MOVEL     '3A'          tblcod
031900170206     c                   movel(p)  arbcbo        tblkey
032000170206     c     ktab          chain     tabel00f
032100170206     c                   if        %found(tabel00f)
032200170206     c                   eval      ds3a  = tbluni
032300170206     c                   movel     §3atb1        assegnato
032400170206     c                   endif
032500170206      * verifico se per codice cliente è ammessa la gestione
032600170206      * tabella 2D
032700170206     c                   move      'S'           o071disC
032800170206     c                   move      'S'           o071disD
032900170206     c                   move      'S'           o071disR
033000170206     c                   exsr      contr2Ddet
033100170206      *se spedizione in assegnato e arrivo forzo comunque NO al dirottamento
033200170206     c                   if        o071disD = 'S' and
033300170206     c                             i071chi = 'A' and
033400170206     c                             assegnato = 'A'
033500170206     c                   move      'N'           o071disD
033600170206     c                   endif
033700170206     c                   endif
033800170206     c                   endsr
033900170206     c**********************************************************************
034000170206     c     contr2Ddet    begsr
034100170206     c**********************************************************************
034200170206     C                   MOVE      ARBKSC        §8899             4 0
034300170206     c* consegna
034400170206     C                   MOVEL     '2D'          TBLCOD
034500170206     C                   MOVEL     *BLANK        TBLKEY
034600170206     C                   MOVEL     'C'           TBLKEY
034700170206     c     1             do        3
034800170206     C     KTAB          CHAIN     TABEL00f
034900170206     c                   if        %found(tabel00f)
035000170206     C                   MOVEL     TBLUNI        DS2D
035100170206      *?  Arrivo
035200170206     c                   if        i071chi = 'A'
035300170206     c* mai
035400170206     C     §2DUTA        IFEQ      'N'
035500170206     c* no 88 e 99
035600170206     C     §2DUTA        OREQ      'V'
035700170206     c* e cli.8888
035800170206     C     §8899         ANDEQ     8888
035900170206     c* no 88 e 99
036000170206     C     §2DUTA        OREQ      'V'
036100170206     c* e cli.9999
036200170206     C     §8899         ANDEQ     9999
036300170206     c* no 88
036400170206     C     §2DUTA        OREQ      '8'
036500170206     c* e cli.8888
036600170206     C     §8899         ANDEQ     8888
036700170206     c* no 99
036800170206     C     §2DUTA        OREQ      '9'
036900170206     c* e cli.9999
037000170206     C     §8899         ANDEQ     9999
037100170208      *?  valore non ammesso Nego abilitazione e passo al successivo
037200170206     c                   select
037300170206     c                   when      tblkey = 'C'
037400170206     c                   move      'N'           o071disC
037500170206     c                   movel     'D'           tblkey
037600170206     c                   when      tblkey = 'D'
037700170206     c                   move      'N'           o071disD
037800170206     c                   movel     'R'           tblkey
037900170206     c                   when      tblkey = 'R'
038000170206     c                   move      'N'           o071disR
038100170206     c                   leavesr
038200170206     c                   endsl
038300170208     c                   else
038400170208      *?  valore ammesso passo al successivo
038500170208     c                   select
038600170208     c                   when      tblkey = 'C'
038700170208     c                   movel     'D'           tblkey
038800170208     c                   when      tblkey = 'D'
038900170208     c                   movel     'R'           tblkey
039000170208     c                   when      tblkey = 'R'
039100170208     c                   leavesr
039200170208     c                   endsl
039300170208     C                   ENDif
039400170206     c                   else
039500170206      *?    Partenza
039600170206     C     §2DUTp        IFEQ      'N'
039700170206     c* no 88 e 99
039800170206     C     §2DUTp        OREQ      'V'
039900170206     c* e cli.8888
040000170206     C     §8899         ANDEQ     8888
040100170206     c* no 88 e 99
040200170206     C     §2DUTp        OREQ      'V'
040300170206     c* e cli.9999
040400170206     C     §8899         ANDEQ     9999
040500170206     c* no 88
040600170206     C     §2DUTp        OREQ      '8'
040700170206     c* e cli.8888
040800170206     C     §8899         ANDEQ     8888
040900170206     c* no 99
041000170206     C     §2DUTp        OREQ      '9'
041100170206     c* e cli.9999
041200170206     C     §8899         ANDEQ     9999
041300170208      *?  valore non ammesso Nego abilitazione e passo al successivo
041400170206     c                   select
041500170206     c                   when      tblkey = 'C'
041600170206     c                   move      'N'           o071disC
041700170206     c                   movel     'D'           tblkey
041800170206     c                   when      tblkey = 'D'
041900170206     c                   move      'N'           o071disD
042000170206     c                   movel     'R'           tblkey
042100170206     c                   when      tblkey = 'R'
042200170206     c                   move      'N'           o071disR
042300170206     c                   leavesr
042400170206     c                   endsl
042500170208     c                   else
042600170208      *?  valore ammesso passo al successivo
042700170208     c                   select
042800170208     c                   when      tblkey = 'C'
042900170208     c                   movel     'D'           tblkey
043000170208     c                   when      tblkey = 'D'
043100170208     c                   movel     'R'           tblkey
043200170208     c                   when      tblkey = 'R'
043300170208     c                   leavesr
043400170208     c                   endsl
043500170206     C                   END
043600170206     C                   endif
043700170206     C                   endif
043800170206     c                   enddo
043900170206     C                   endsr
044000140206     c**********************************************************************
044100140206     c     *inzsr        begsr
044200140206     c**********************************************************************
044300170113     c     karb          klist
044400170206     c                   kfld                    i071AAS
044500170206     c                   kfld                    i071LNP
044600170206     c                   kfld                    i071NRS
044700170206     c                   kfld                    i071NSP
044800170227     c     kgcp          klist
044900170227     c                   kfld                    wAAS
045000170227     c                   kfld                    wLNP
045100170227     c                   kfld                    wNRS
045200170227     c                   kfld                    wNSP
045300170113     c     ktab          klist
045400170113     c                   kfld                    TBLKUT
045500170113     c                   kfld                    TBLCOD
045600170113     c                   kfld                    TBLKEY
045700170113     c                   z-add     1             TBLKUT
045800170117     c     ktbe          klist
045900170117     c                   kfld                    tbecod
046000170117     c                   kfld                    tbeke1
046100170117     c                   kfld                    tbeke2
046200140206     c                   endsr
