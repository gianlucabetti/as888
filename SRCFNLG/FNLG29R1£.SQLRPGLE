000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300170614      * Il pgm scrive i record per tipo disposizione e provenienza
000400060125      *
000500060125      *****************************************************************
000700170614     Ftitas30c  IF   E           k DISK
000800170614     Ftignp01l  IF   E           k DISK
000801170718     Ftigcp02l  IF   E           k DISK
000900170619     Fwfgiaex0f uf a E           k DISK
001000030429     F*-------------------------------------------------------------------------
001100000607     D kpjba         e ds
001101170614     dparam            ds
001102170718     d datap                          8
001103170718     d datap2                         8
001500170718     D tignp         E DS                  extname(tignp00f)  prefix(x)
001502170622     D DGNPDMC915    e ds
001503170615     D DGNPDMC924    e ds
001504170615     D tisi95ds      e ds
002000030429      *
002001170614     d dataiso         s               d   datfmt(*iso)
002002170614     d dataeur         s               d   datfmt(*eur)
002003170615     d trovato         s              1
002004170615     d windir          s             35
002005170615     d wnumgia         s              9  0
002006170614     d wNUMFD          s              9  0
002007170614     d wDCRMIN         s              9  0
002008170614     d wDCRMAX         s              9  0
002009170614     d wALTROIN        s              9  0
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
002900020205     c**********************************************************************
007900170523      * ?effettua una delete preventiva
008000170420     C/EXEC SQL
008100170619     C+ delete from wfgiaex0f
008200170420     C/END-EXEC
008300170420      *
008303170523     c
008304170523     c
008400161026     C/EXEC SQL
008500170718     C+ DECLARE B1 CURSOR FOR SELECT * from tignp00f where
008700170718     C+ substr(gnpdmc, 1, 11) = 'DATA DISPOS' and
008701170718     C+ substr(gnpdmc, 21, 8) between :datap and :datap2
008800170718     C+  order by GnPAGC, gnpfgc, gnpngc, gnpfrg
008900161026     C/END-EXEC
009000161026     C/EXEC SQL
009100161026     C+ OPEN B1
009200161026     C/END-EXEC
009300161026     C                   DOU       SqlCod <> 0
009400161026     C/EXEC SQL
009500170718     C+ FETCH NEXT FROM B1 INTO :tignp
009600161026     C/END-EXEC
009700161026     C                   SELECT
009800161026     c*
009900161026     C                   WHEN      SqlCod = 0
010001170614      * ricerca note disposizioni
010400170614     c                   exsr      note
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
012900000607     c                   seton                                        lr
013000161025      * ?___________________________________________________________________
013100170614     c     note          begsr
013200161025      * ?___________________________________________________________________
013201170616     c                   clear                   trovato
013202170718     c     kgia          chain     tigcp02l
013203170718     c                   if        not%found(tigcp02l)
013204170718     c                   leavesr
013205170718     c                   endif
014100170614     c     knote         setll     tignp01l
014102170523     c                   do        *hival
014103170614     c     knote         reade     tignp01l
014200170614     c                   if        %eof(tignp01l)
014300170523     c                   leave
014301170523     c                   endif
014302170616     c                   if        gnpfas <> 20
014303170616     c                   iter
014304170616     c                   endif
014305170614      *verifica da note il tipo disposizione
014501170614      *pulisce campi di work e file
014503170614     c                   clear                   giacex
014504170614     c                   clear                   wnumgia
014505170614     c                   clear                   wNUMFD
014506170614     c                   clear                   wDCRMIN
014507170614     c                   clear                   wDCRMAX
014508170614     c                   clear                   wALTROIN
014509170614     c                   if        %subst(gnpdmc:1:11) = 'DATA DISPOS'
014510170614     c                   exsr      altrenote
014511170616     c                   eval      trovato = 'S'
014512170614     c                   leave
014513170614     c                   else
014514170614     c                   move      gcpdis        dispo
014515170614     c                   z-add     0             coddis
014516170615     c                   clear                   desdis
014517170614     c                   move      'F'           webfile
014518170614     c                   z-add     1             wnumgia
014519170616     c                   eval      trovato = 'S'
014520170614     c                   leave
014521170614     c                   endif
014900170523     c                   enddo
014901170523     c                   if        trovato = 'S'
014902170523     c                   exsr      scrivi
014903170523     c                   endif
017200161025      *
017300161025     c                   endsr
017301170614      * ?___________________________________________________________________
017302170614     c     Altrenote     begsr
017303170614      * ?___________________________________________________________________
017305170614     c                   do        *hival
017306170614     c     knote         reade     tignp01l
017307170614     c                   if        %eof(tignp01l)
017308170614     c                   leave
017309170614     c                   endif
017313170615     c                   if        gnpdif < 900 or gnpdif > 924
017314170614     c                   move      'F'           webfile
017315170615     c                   z-add     1             wnumgia
017316170615     c                   move      gcpdis        dispo
017317170614     c                   leave
017318170614     c                   else
017319170614     c                   move      'W'           webfile
017320170615     c                   move      gcpdis        dispo
017321170614     c                   select
017322170614     c                   when      gnpdif = 900
017323170615     c                   z-add     1             wnumgia
017324170615     c                   z-add     gnpdif        coddis
017325170615     c                   eval      desdis = 'RENDERE'
017326170614     c                   leave
017327170614     c                   when      gnpdif = 910
017328170615     c                   z-add     1             wnumgia
017329170615     c                   z-add     gnpdif        coddis
017330170615     c                   eval      desdis = 'RICONSEGNA STESSO INDIRIZZO'
017331170614     c                   leave
017332170615     c                   when      gnpdif = 911
017333170615     c                   z-add     1             wnumgia
017334170615     c                   z-add     gnpdif        coddis
017335170615     c                   eval      desdis = 'RICONSEGNA STESSO INDIRIZZO -
017336170615     c                             + APPUNTAMENTO'
017337170622     c*                  exsr      VerDcr
017338170614     c                   leave
017339170615     c                   when      gnpdif = 914
017340170615     c                   z-add     1             wnumgia
017341170615     c                   z-add     gnpdif        coddis
017342170615     c                   eval      desdis = 'RICONSEGNA STESSO INDIRIZZO -
017343170615     c                             IN DATA'
017344170622     c                   exsr      Ver915
017345170615     c                   leave
017346170614     c                   when      gnpdif = 916
017347170615     c                   z-add     1             wnumgia
017348170615     c                   eval      desdis = 'FERMO DEPOSITO'
017349170615     c                   z-add     gnpdif        coddis
017350170614     c                   z-add     1             wnumfd
017351170614     c                   leave
017352170614     c                   when      gnpdif = 920
017353170615     c                   z-add     1             wnumgia
017354170615     c                   z-add     gnpdif        coddis
017355170615     c                   eval      desdis = 'RICONSEGNA ALTRO INDIRIZZO'
017356170615     C                   clear                   DGNPDMC924
017357170615     c                   exsr      VerTisi
017358170615     c                   if        gcplnaa <> o95lna
017359170615     c                   if        gcplnaa = o95lla or gcplnaa = o95llo or
017360170615     c                             gcplnaa = o95lls
017361170615     c                   else
017362170614     c                   z-add     1             waltroin
017363170614     c                   endif
017364170615     c                   endif
017365170614     c                   leave
017368170614     c                   endsl
017369170614      *
017370170614     c                   endif
017371170614     c                   enddo
017377170614     c                   endsr
017378170614      * ?___________________________________________________________________
017379170614     c     VerDcr        begsr
017380170614      * ?___________________________________________________________________
017381170614     c     ktas          chain     titas30c
017382170614     c                   if        %found(titas30c)
017383170614     c                   if        tasdcr > 0  and
017384170614     c                             tasdcr >= gcpddm
017385170614     c                   move      gcpddm        dadata
017386170614     c                   move      tasdcr        adata
017387170614     c                   CALL      'XSRLAV8'
017388170614     c                   PARM                    Wdata8
017389170614     c                   if        Giolav > 3
017390170614     c                   z-add     1             wdcrmax
017391170614     c                   else
017392170614     c                   z-add     1             wdcrmin
017393170614     c                   endif
017394170614     c                   endif
017395170614     c                   endif
017396170614     c                   endsr
017397170622      * ?___________________________________________________________________
017398170622     c     Ver915        begsr
017399170622      * ?___________________________________________________________________
017400170622     c                   do        *hival
017401170622     c     knote         reade     tignp01l
017402170622     c                   if        %eof(tignp01l)
017403170622     c                   leave
017404170622     c                   endif
017405170622     c                   select
017408170718     c                   when      gnpdif = 915 or gnpdif = 9152
017410170622     C                   movel     gnpdmc        DGNPDMC915
017411170622     c                   move      ndcr          dataeur
017412170622     c                   move      dataeur       dataiso
017413170622      *
017414170622     c                   move      dataiso       adata
017415170622     c                   move      gcpddm        dadata
017416170622     c                   CALL      'XSRLAV8'
017417170622     c                   PARM                    Wdata8
017418170718     c                   if        gnpdif = 9152
017420170718     c                   add       1             giolav
017421170718     c                   endif
017422170622     c                   if        Giolav > 3
017423170622     c                   z-add     1             wdcrmax
017424170622     c                   else
017425170622     c                   z-add     1             wdcrmin
017426170622     c                   endif
017427170622     c                   leave
017439170622     c                   endsl
017440170622     c                   enddo
017441170622     c                   endsr
017442170615      * ?___________________________________________________________________
017443170615     c     VerTisi       begsr
017444170615      * ?___________________________________________________________________
017445170615     c                   do        *hival
017446170615     c     knote         reade     tignp01l
017447170615     c                   if        %eof(tignp01l)
017448170615     c                   leave
017449170615     c                   endif
017450170615     c                   select
017451170615     c                   when      gnpdif = 923
017452170615     c                   movel     gnpdmc        windir
017453170615     c                   when      gnpdif = 924
017454170615     C                   movel     gnpdmc        DGNPDMC924
017455170615     c                   leave
017456170615     c                   endsl
017457170615     c                   enddo
017458170615     c     ktas          chain     titas30c
017459170615     c                   if        %found(titas30c)
017460170615     C                   CLEAR                   TISI95ds
017461170619     C                   MOVEL     '7'           I95TCN
017462170615     C                   MOVEL     *BLANK        I95TLA
017463170615     C                   MOVEL     windir        I95IND
017464170615     C                   MOVEL     CAP           I95CAP
017465170615     C                   MOVEL     LOC           I95LOC
017466170615     C                   MOVEL     PRO           I95PRV
017467170615     c                   movel     NAZ           i95nar
017468170615     C                   MOVEL     taspkb        I95LKG
017469170615     C                   MOVEL     tasVLb        I95LMC
017470170615     C                   CALL      'TISI95R'
017471170615     C                   PARM                    TISI95ds
017472170615     c                   if        o95err <> *blank
017473170615     c                   movel     gcplnaa       o95lna
017474170615     c                   endif
017475170615     c                   endif
017476170615     c                   endsr
024700161025     C*_____________________________________________________________________
024800170615     C** scrittura  record raggruppamento giacenze per disposizione
024900161025     C*_____________________________________________________________________
025000170523     C     scrivi        BEGSR
025001170619     c     kwf           chain     wfgiaex0f                          99
025002170718     c                   move      datap         datada
025003170718     c                   move      datap2        dataa
025004170614     c                   add       wnumgia       numgia
025005170614     c                   add       wNUMFD        NUMFD
025006170614     c                   add       wDCRMIN       DCRMIN
025007170614     c                   add       wDCRMAX       DCRMAX
025008170614     c                   add       wALTROIN      ALTROIN
025009170614     c  n99              update    giacex
025010170614     c   99              write     giacex
026702170614      *
026800160125     C                   endsr
026900020205     C**-------------------------------------------------------------**
027000000607     c     *inzsr        begsr
027100060125     C**-------------------------------------------------------------**
027200000607     c     *entry        plist
027300020205     c                   parm                    kpjba
027301170614      *
027302170614     C                   time                    w0120            14 0
027303170614     C                   move      w0120         wdat              8 0
027304170614     c                   move      wdat          dataeur
027305170614     c                   move      dataeur       dataiso
027306170614     c                   move      dataiso       wdat
027309170614     c                   if        kpjbu <> *blank
027310170614     c                   movel     kpjbu         param
027312170614     c                   else
027313170614     c                   move      wdat          datap
027314170614     c                   move      wdat          datap2
027315170614     c                   endif
027316170718     c                   move      datap         dadata
027318170718     c                   move      datap         adata
027319170718     c                   move      dadata        dataiso
027320170615     c                   move      dataiso       dataeur
027321170615     c                   move      dataeur       dadata            8 0
027322170718     c                   move      adata         dataiso
027326170615     c                   move      dataiso       dataeur
027327170615     c                   move      dataeur       adata             8 0
028900060125      *
029000160125     C* Definisco chiavi di accesso
029100170614     C     Ktas          KLIST
029200170614     C                   KFLD                    gcpAASa
029300170614     C                   KFLD                    gcpLNPa
029301170614     C                   KFLD                    gcpNRSa
029302170614     C                   KFLD                    gcpNSPa
029303170718      *
029304170718     C     Kgia          KLIST
029305170718     C                   KFLD                    xGnPAGC
029306170718     C                   KFLD                    xGnPFGC
029307170718     C                   KFLD                    xGnPNGC
029308170718     C                   KFLD                    xGnPFRG
029310170718      *
029400161025      *
029401170614     C     Knote         KLIST
029402170614     C                   KFLD                    GcPAGC
029403170614     C                   KFLD                    GcPFGC
029404170614     C                   KFLD                    GcPNGC
029405170614     C                   KFLD                    GcPFRG
029800161027      *
029801170614     C     Kwf           KLIST
029802170614     C                   KFLD                    DISPO
029803170614     C                   KFLD                    WEBFILE
029804170614     C                   KFLD                    CODDIS
030300020402     c                   endsr
