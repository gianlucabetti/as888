000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300170614      * Il pgm scrive i record per tipo disposizione e provenienza
000400060125      *
000500060125      *****************************************************************
000801170731     Ftabel00f  IF   E           k DISK
000802170731     Ftita430c  IF   E           k DISK
000900170731     Fwfb33400f uf a E           k DISK
000901170801     FQsysprt   O    F  132        printer oflind(*inof)
001000030429     F*-------------------------------------------------------------------------
001100000607     D kpjba         e ds
001101170614     dparam            ds
001102170731     d lnap                           3s 0
001103170731     d datap                          8s 0
001104170731     d datap2                         8s 0
001500170801     D titas         E DS                  extname(titas00f)
001501170731     D tntbe         E DS                  extname(tntbe00f)
001504170615     D tisi95ds      e ds
001505170731     D dta4A         e ds
001506170731     D ds3a          e ds
002000030429      *
002001170614     d dataiso         s               d   datfmt(*iso)
002002170614     d dataeur         s               d   datfmt(*eur)
002003170615     d trovato         s              1
002004170731     d x               s              2  0
002006170801     d anno            s              4  0
002007170801     d mmgg1           s              4  0
002008170801     d mmgg2           s              4  0
002010170614      *
002012170614     D WLBDA8          DS
002013170614     D  G02DAT                 1      8  0
002014170614     D  G02INV                 9     16  0
002015170614     D  G02ERR                17     17
002016170614     D  G02TGI                18     22  0
002017170614     d Wdata8          DS
002018170614     d  dadata                 1      8  0
002019170614     d  adata                  9     16  0
002020170614     d  GioLav                17     21  0
002021170731     D C3a             S              2    DIM(40)
002022170731     D filp            S              3    DIM(85)
002900020205     c**********************************************************************
007900170523      * ?effettua una delete preventiva
008000170420     C/EXEC SQL
008100170731     C+ delete from wfb33400f
008200170420     C/END-EXEC
008300170420      *
008303170523     c
008304170802      * primo giro con titas00f
008305170523     c
008400161026     C/EXEC SQL
008500170731     C+ DECLARE B1 CURSOR FOR SELECT * from titas00f where
008700170731     C+ taslna = :lnap and tasnrs = 0 and tasaas = :anno
008701170731     C+ and tasmgs between :mmgg1 and :mmgg2
008900161026     C/END-EXEC
009000161026     C/EXEC SQL
009100161026     C+ OPEN B1
009200161026     C/END-EXEC
009300161026     C                   DOU       SqlCod <> 0
009400161026     C/EXEC SQL
009500170731     C+ FETCH NEXT FROM B1 INTO :titas
009600161026     C/END-EXEC
009700161026     C                   SELECT
009800161026     c*
009900161026     C                   WHEN      SqlCod = 0
010001170614      * ricerca note disposizioni
010400170731     c                   exsr      verifiche
011300161026      * ? EoF
011400161026     C                   WHEN      SqlCod = 100
011500161026     c                   leave
011600161026      **
011700161026      * ? Errori
011800161026     C                   WHEN      SqlCod <> 0
011900161026     c                   leave
012000161026      **
012100161026     C                   ENDSL
012200161026      **
012300161026     C                   ENDDO
012400161026     C/EXEC SQL
012500161026     C+ CLOSE B1
012600161026     C/END-EXEC
012800030429      *
012801170803      * secondo giro con titas10f
012802170803      *
012803170803     C/EXEC SQL
012804170803     C+ DECLARE B2 CURSOR FOR SELECT * from titas10f where
012805170803     C+ taslna = :lnap and tasnrs = 0 and tasaas = :anno
012806170803     C+ and tasmgs between :mmgg1 and :mmgg2
012807170803     C/END-EXEC
012808170803     C/EXEC SQL
012809170803     C+ OPEN B2
012810170803     C/END-EXEC
012811170803     C                   DOU       SqlCod <> 0
012812170803     C/EXEC SQL
012813170803     C+ FETCH NEXT FROM B2 INTO :titas
012814170803     C/END-EXEC
012815170803     C                   SELECT
012816170803     c*
012817170803     C                   WHEN      SqlCod = 0
012818170803      * ricerca note disposizioni
012819170803     c                   exsr      verifiche
012820170803      * ? EoF
012821170803     C                   WHEN      SqlCod = 100
012822170803     c                   leave
012823170803      **
012824170803      * ? Errori
012825170803     C                   WHEN      SqlCod <> 0
012826170803     c                   leave
012827170803      **
012828170803     C                   ENDSL
012829170803      **
012830170803     C                   ENDDO
012831170803     C/EXEC SQL
012832170803     C+ CLOSE B2
012833170803     C/END-EXEC
012900000607     c                   seton                                        lr
013000161025      * ?___________________________________________________________________
013100170731     c     verifiche     begsr
013200161025      * ?___________________________________________________________________
013201170616     c                   clear                   trovato
013202170731      * verifica se linea di partenza è fra le tabellate per la forzatura
013203170731     c                   move      taslnp        taslnpa           3
013204170731     c     taslnpa       lookup    filp                                   90
013205170731     c  n90              leavesr
013206170731      * verifica che il codice bolla non indichi una bolla di recupero
013207170731     c                   z-add     1             x
013208170731     c     tascbo        lookup    c3a(x)                                 90
013209170731     c  n90              leavesr
013213170731      *
013214170731     c                   clear                   DTA4A
013215170801     c     ktita4        chain     tita430c                           89
013216170731     c                   if        not *in89
013217170801     c                   move      ta4not        DTA4A
013218170731     c                   endif
013219170731     c                   exsr      vertisi
013220170801     c                   select
013221170801     c                   when      o95lna = 334
013222170801     c                   leavesr
013226170801     c                   when      o95lna = 333
013227170731     c                   exsr      scrivi
013228170801     c                   other
013229170801     C   of              except    testa
013231170801     C                   except    detta
013232170801     c                   endsl
017300161025     c                   endsr
017442170615      * ?___________________________________________________________________
017443170615     c     VerTisi       begsr
017444170615      * ?___________________________________________________________________
017460170615     C                   CLEAR                   TISI95ds
017461170731     C                   MOVEL     '3'           I95TCN
017462170615     C                   MOVEL     *BLANK        I95TLA
017463170731     C                   MOVEL     tasind        I95IND
017464170731     C                   MOVEL     tascad        I95CAP
017465170801     C                   MOVEL     taslod        I95LOC
017466170731     C                   MOVEL     tasprd        I95PRV
017467170731     c                   movel     tasnzd        i95nar
017468170615     C                   MOVEL     taspkb        I95LKG
017469170615     C                   MOVEL     tasVLb        I95LMC
017470170615     C                   CALL      'TISI95R'
017471170615     C                   PARM                    TISI95ds
017472170615     c                   if        o95err <> *blank
017473170801     c                   movel     taslna        o95lna
017474170615     c                   endif
017476170615     c                   endsr
024700161025     C*_____________________________________________________________________
024800170615     C** scrittura  record raggruppamento giacenze per disposizione
024900161025     C*_____________________________________________________________________
025000170523     C     scrivi        BEGSR
025002170801     c                   eval      WTASASS = TASAaS
025003170731     c                   eval      WTASLNP = TASLNP
025004170731     c                   eval      WTASNRS = TASNRS
025005170731     c                   eval      WTASNSP = TASNSP
025006170731     c                   eval      WTASMGS = TASMGS
025007170731     c                   eval      WTASRMA = §TA4ARMA
025008170731     c                   eval      WTASRMN = TASRMN
025009170731     c                   eval      WTASRSD = TASRSD
025010170731     c                   eval      WTASIND = TASIND
025011170731     c                   eval      WTASCAD = TASCAD
025012170731     c                   eval      WTASLOD = TASLOD
025013170731     c                   eval      WTASPRD = TASPRD
025014170731     c                   eval      WTASNZD = TASNZD
025015170731     c                   eval      WTASNCL = TASNCL
025016170731     c                   eval      WTASPKB = TASPKB
025017170731     c                   eval      WTASVLB = TASVLB
025021170731     c                   write     wfb3340
026702170614      *
026800160125     C                   endsr
030301170731      *____________________________________________________
030302170731     c     reptab        begsr
030304170731      *____________________________________________________
030400170731     C/EXEC SQL
030500170731     C+ DECLARE a1 CURSOR FOR SELECT * from edpes/tntbe_fla  where
030600170731     C+ tbeke1 = '333' and tbeke2 = '334' and tbecod = 'FLA'
030700170731     C/END-EXEC
030800170731     C/EXEC SQL
030900170731     C+ OPEN a1
031000170731     C/END-EXEC
031100170731     C                   DOU       SqlCod <> 0
031200170731     C/EXEC SQL
031300170731     C+ FETCH NEXT FROM a1 INTO :tntbe
031400170731     C/END-EXEC
031500170731     C                   SELECT
031600170731     c*
031700170731     C                   WHEN      SqlCod = 0
031800170731      * ricerca note disposizioni
031900170731     c                   movea     tbeuni        filp
032000170731      * ? EoF
032100170731     C                   WHEN      SqlCod = 100
032200170731     c                   leave
032300170731      **
032400170731      * ? Errori
032500170731     C                   WHEN      SqlCod <> 0
032600170731     c                   leave
032700170731      **
032800170731     C                   ENDSL
032900170731      **
033000170731     C                   ENDDO
033100170731     C/EXEC SQL
033200170801     C+ CLOSE a1
033300170731     C/END-EXEC
033400170731     c                   endsr
033401170801     C**-------------------------------------------------------------**
033402170801     c     *inzsr        begsr
033403170801     C**-------------------------------------------------------------**
033404170801     c     *entry        plist
033405170801     c                   parm                    kpjba
033406170801      *
033407170801     C                   time                    w0120            14 0
033408170801     C                   move      w0120         wdat              8 0
033409170801     c                   move      wdat          dataeur
033410170801     c                   move      dataeur       dataiso
033411170801     c                   move      dataiso       wdat
033412170801     c                   if        kpjbu <> *blank
033413170801     c                   movel     kpjbu         param
033414170801     c                   else
033415170801     c                   move      wdat          datap
033416170801     c                   move      wdat          datap2
033417170801     c                   endif
033418170801     c                   move      datap         mmgg1
033419170801     c                   movel     datap         anno
033420170801     c                   move      datap2        mmgg2
033421170801     c                   seton                                        of
033422170801      *
033423170801     C     KTAB          KLIST
033424170801     C                   KFLD                    tblkut
033425170801     C                   KFLD                    TBLCOD
033426170801     C                   z-add     1             tblkut
033427170801     C     Ktita4        KLIST
033428170801     C                   KFLD                    TAsAAS
033429170801     C                   KFLD                    TAsLNP
033430170801     C                   KFLD                    TAsNRS
033431170801     C                   KFLD                    TAsNSP
033432170801     C                   KFLD                    TA4TRC
033433170801     C                   move      'A'           ta4trc
033434170801     C* CARICO I TIPI BOLLA
033435170801     C                   clear                   x
033436170801     C                   MOVEL     '3A'          TBLCOD
033437170801     C                   MOVEL     *BLANK        TBLKEY
033438170801     C     KTAB          SETLL     TABEL
033439170801     C     *IN21         DOUEQ     '1'
033440170801     C     KTAB          READE     TABEL                                  21
033441170801     C  N21TBLFLG        IFEQ      *BLANK
033442170801     C                   MOVEL     TBLUNI        DS3A
033443170801     C     §3arbl        IFeq      'R'
033444170801     c                   iter
033445170801     c                   endif
033446170801     C                   ADD       1             x
033447170801     C                   MOVEL     TBLKEY        C3a(x)
033448170801     C                   END
033449170801     C                   END
033450170801     c                   exsr      reptab
033451170801     c                   endsr
033500170801     o*-------------------------------------------------------------*
033600170801     OQsysPrt   E            Testa            01
033700170801     o                                           66 'Elenco Spedizioni con line'
033800170801     o                                              'a arrivo da verificare '
033900170801     O          E            Testa          1 03
034000170801     o                                           14 'Spedizione'
034001170801     o                                           23 'Cbo'
034100170801     o                                           27 'Lna'
034500170801     O          E            detta          1
034600170801     O                       tasaas               5
034700170801     O                       taslnp            +  1
034800170801     O                       tasnrs            +  1
034900170801     O                       tasnsp            +  1
034901170801     O                       tascbo            +  1
035000170801     O                       taslna            +  1
