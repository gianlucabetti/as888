000100000725     H DECEDIT('0,') DATEDIT(*DMY.)
000200060929     h dftactgrp(*no) actgrp(*caller)
000300061213      * Fidg02r *----------------------------------------------------*
000400070320      *      GESTIONE estensione ANAGRAFICA AUTOTRASP./distinte consegna
000500911204      *--------------------------------------------------------------*
000600061213     FFidg02D   CF   E             WORKSTN
000700061213     F                                     SFILE(dg02S01:NRR)
000800060927     ftabel00f  if   e           k disk
000900060927     ffiapd01l  if   e           k disk
001000060927     fazorg01l  if   e           k disk
001100070320     Ffiapd22L  UF A E           K DISK    usropn commit(flgcmt)
001200070320     Ffiapd31L  UF a E           K DISK    usropn commit(flgcmt)
001201070717     Ffiapd51L  UF a E           K DISK    usropn commit(flgcmt)
001300070410     FFIDST09L  UF A E           K DISK    usropn commit(flgcmt)
001400070320     Ffidst31L  UF A E           K DISK    usropn commit(flgcmt)
001401070717     Ffidst51L  UF A E           K DISK    usropn commit(flgcmt)
001500061229     Ffidst04L  if   E           K DISK    rename(fidst000:fidst004)
001600940809     D*
001700061002     ddtaiso           s               d
001800061002     ddtaeur           s               d   datfmt(*eur)
001900061219     ddtadde           s               d
002002070718     ddtascad          s               d
002100061220     dcomdde           s               d
002200070119     dcomscad          s               d
002201070718     dcomhdde          s               d
002202070718     dcomhscad         s               d
002203070719     dsavdde           s               d
002204070719     dsavscad          s               d
002300060927     D xx              S              2  0 inz
002400060927     D yy              S              2  0 inz
002500060927     D zz              S              2  0 inz
002600071126     D MSG             S             78    DIM(28) CTDATA PERRCD(1)
002700070726     D L1              S              3  0 DIM(30) ASCEND
002800070726     D L6              S              3  0 DIM(30) ASCEND
002900070502     D cmd             S             90    DIM(1) CTDATA PERRCD(1)              *comandi CLP
003000070502     d                 DS
003100070502     d  DJob                  79     88                                          -Jobq
003200070502     d DSComando               1     90                                         *comando
003300070502      * Informazioni lavoro
003400070502     d                SDS
003500070502     d  JOB                  244    253                                         *lavoro
003600070502     d $lenght         S             15  5 INZ(90)                              *comando -lunghezza-
003700070502     d $command        S             90                                         *comando -contenuto-
003800940809     D*
003900060927     d* Parametri x Controllo profilo utenti
004000070119     d TIBS34ds      e ds
004100060203     d AZUTEds       e ds                  extname(AZUTE00F)
004200061222     c* numeratori
004300061222     d trul33ds      e ds
004400060927     d* Ds per dati organigramma
004500060203     d DDatiUte      e ds
004600060927     d*
004700060927     D KPJBA         E DS
004800060927     D fiapd2ds      E DS                  extname(fiapd20f)
004900060929     D fidstds       E DS                  extname(fidst00f)
005000060926     D fnlv24ds      E DS
005100061213     D fidg02ds      E DS
005200070112     D fidg07ds      E DS
005300070124     D fidg09ds      E DS
005400070119     d dapdflr       e ds
005500070320     d dgrgflo       e ds
005600060927     D Ds1p          E DS
005700060927     D Ds5e          E DS
005800060927     d ds03          e ds
005900070718     D OG147         E DS                  INZ
005901070718     D OG148         E DS                  INZ
006000061229     d comdta          s               d
006100060926     D Ptc1            S               *
006200060926     D                                     INZ(%ADDR(apd2tc1))
006300060926     D tc1             S                   LIKE(apd2tc1)
006400060926     D                                     DIM(5)
006500060926     D                                     BASED(Ptc1)
006600060926     D Pts1            S               *
006700060926     D                                     INZ(%ADDR(apd2ts1))
006800060926     D ts1             S                   LIKE(apd2ts1)
006900060926     D                                     DIM(2)
007000060926     D                                     BASED(Pts1)
007100060929     d* schiere file fidst00f
007200060929     D Ptc1d           S               *
007300060929     D                                     INZ(%ADDR(dsttc1))
007400060929     D tc1d            S                   LIKE(dsttc1)
007500060929     D                                     DIM(5)
007600060929     D                                     BASED(Ptc1d)
007700060929     D Pts1d           S               *
007800060929     D                                     INZ(%ADDR(dstts1))
007900060929     D ts1d            S                   LIKE(dstts1)
008000060929     D                                     DIM(2)
008100060929     D                                     BASED(Pts1d)
008200060926     D tc              S                   LIKE(apd2tc1)
008300060926     D                                     DIM(5)
008400060926     D ts              S                   LIKE(apd2ts1)
008500060926     D                                     DIM(2)
008600060927     D dtc             S             15
008700060926     D                                     DIM(5)
008800060927     D dts             S             15
008900060926     D                                     DIM(2)
009000061219     c* schiere video
009100061219     D prsq            S              4  0
009200090113     D                                     DIM(9999)
009300070129     D cgi             S             13
009400090113     D                                     DIM(9999)
009500940909     D* DS PER TRUL06R - CARICAMENTO £X
009600940909     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009700060926     D  LIN                    1     90  0
009800950314     D                                     DIM(30)
009900060927     d*
010000070202     d flagcopia       s              1
010100070202     d savnras         s                   like(apd2nras)
010200070119     d comnras         s                   like(apd2nras)
010201071126     d wtol            s                   like(vtol)
010202071126     d wpeso           s             14  1
010203071126     d wvolume         s             10  3
010204071126     d wnftl           s             10  0
010300060926     D                 DS
010400061213     D  Vpdr                   1      7
010500061213     D  Vfgs                   1      3  0
010600061213     D  Vcpdr                  4      7
010700061213     i*
010800070125     iDG02d03
010900061213     i              vtc1                        tc(1)
011000061213     i              vtc2                        tc(2)
011100061213     i              vtc3                        tc(3)
011200061213     i              vtc4                        tc(4)
011300061213     i              vtc5                        tc(5)
011400061213     i              vtc1d                       dtc(1)
011500061213     i              vtc2d                       dtc(2)
011600061213     i              vtc3d                       dtc(3)
011700061213     i              vtc4d                       dtc(4)
011800061213     i              vtc5d                       dtc(5)
011900061213     i              vts1                        ts(1)
012000061213     i              vts2                        ts(2)
012100061213     i              vts1d                       dts(1)
012200061213     i              vts2d                       dts(2)
012300060926     C**********************************************************************
012400060926     c*
012500060926     c                   do        *hival
012600070115     c                   WRITE     dg02T01
012700070115     c* 42 on inserimento anagrafico
012800070115     c                   setoff                                       42
012900060926     C* richiamato da menù
013000070112     c                   if        d02file <> 'FIDST00F'
013100060926     c                   exsr      srd01
013200060926     c                   end
013300060926     c   kc              leave
013301070718     c* consegne o ritiri
013302070718     c     vctp          comp      'C'                                    48
013400060926     C*
013500060926     C                   EXSR      srd02
013600061220     c                   clear                   vmsg
013700061220     c                   setoff                                       28
013800070112     c* se sono stato chiamato da distinta esco
013900070124     c                   if        d02file = 'FIDST00F' or *inkc
014000060928     c                   leave
014100060928     c                   end
014200060927     c                   enddo
014300061010     c* imposto il comando eseguito
014400061010     c                   select
014500061010     c                   when      *inkc
014600061213     c                   eval      d02cmde = 'KC'
014700070320     c                   when      *inkf or *inkq or *inke
014800061213     c                   eval      d02cmde = 'KF'
014900061010     c                   when      *inkl
015000061213     c                   eval      d02cmde = 'KL'
015100061010     c                   endsl
015200060926     C*
015300061213     C                   MOVEL     fidg02ds      KPJBU
015400060926      *
015500060926     C                   SETON                                        LR
015600060926     C**********************************************************************
015700060926     c     srd01         begsr
015800060927     C**********************************************************************
015900060926     c*
016000061222     c                   setoff                                       2829
016100061222     c                   clear                   vmsg
016101070717     c                   eval      vctp='C'
016200060926     c                   do        *hival
016300060926     c*
016400070125     c                   WRITE     dg02T01
016500061213     C                   EXFMT     dg02D01
016600920918     C* CMD3 - FINE LAVORO
016700060926     C   KC              leave
016701070718     c                   if        *inkd
016800070718     C                   exsr      srric
016801070720     c                   if        oksce = ' '
016802070718     c                   iter
016804070719     c                   end
016805070720     c                   end
016900060927     c                   exsr      ctr01
017000061220     c* inserimento nuova anagrafica
017100061220     c                   if        not *in28 and *inkj
017200070115     c                   seton                                        42
017300061220     C                   leave
017400061220     c                   end
017500061220     c* errore
017600061220     c                   if        *in28 or (*in29 and not *inkj)
017700061220     c                   iter
017800061220     c                   end
017900060927     c                   leave
018000060927     c                   enddo
018100060927     c*
018200060927     c                   endsr
018300060927     C**********************************************************************
018400070124     c     srriccgi      begsr
018500060927     C**********************************************************************
018600070112     c*
018700070627     c                   setoff                                       081314
018800070124     c                   clear                   fidg09ds
018900070124     c                   eval      D09IOP0 = 'P01'
019000070129     c                   eval      d09ifgs = vscfgsg
019002070718     c                   eval      D09ITUG = vctp
019100070126     c                   if        d02file = 'FIDST00F'
019200070126     c                   move      vdfv          dtaiso
019300070126     c                   move      dtaiso        d09idat
019400070126     c                   else
019500070126     c                   move      vdde          dtaiso
019600070126     c                   move      dtaiso        d09idat
019700070126     c                   end
019800070130     c*                  eval      d09ides = vsdcgi
019900070124     c                   eval      kpjbu = fidg09ds
020000070124     c                   call      'FIDG09R'
020100070124     c                   parm                    kpjba
020200070124     c                   eval      fidg09ds = kpjbu
020300070124     c                   if        d09oerr = '1'
020400070129     c                   seton                                        280813
020500070124     c                   movel     Msg(2)        Vmsg
020600070124     c                   else
020700070320     c                   eval      dgrgflo = D09OFLO
020800070320     c                   if        §grgdst = 'N'
020900070320     c                   seton                                        280813
021000070320     c                   movel     Msg(25)       Vmsg
021100070320     c                   else
021200070130     c                   if        d09ocgi <> ' '
021300070129     c                   eval      vscfgsg= d09ofgs
021400070129     c                   eval      vsccgi = d09ocgi
021500070124     c                   eval      vsdcgi = d09odes
021501070627     c     udate_8       comp      D09ODSC                            14
021600070320     c                   end
021700070124     c                   end
021800070130     c                   end
021900070124     c*
022000070124     c                   endsr
022100070124     C**********************************************************************
022200070124     c     srric         begsr
022300070124     C**********************************************************************
022400070124     c*
022600070720     c                   clear                   oksce             1
022601070720     c                   clear                   fidg07ds
022700070119     c                   movel     'A'           d07tipa
022800070112     c                   movel     vfgs          d07fgs
022900070112     c                   if        vcpdr <> ' '
023000070726     C     '?'           SCAN      Vcpdr                                  31
023100070112     c                   if        not *in31
023200070112     c                   movel     vfgs          d07pdr
023300070112     c                   move      vcpdr         d07pdr
023400070112     c                   end
023500070112     c                   end
023600070112     c                   movel     fidg07ds      kpjbu
023700070116     c                   movel     vdta          dtaiso
023800070112     c                   call      'FIDG07R'
023900070112     c                   parm                    kpjba
024000070112     c                   movel     kpjbu         fidg07ds
024100070119     c                   if        d07pdro <> 0
024200070119     c                   movel     d07pdro       vfgs
024300070119     c                   move      d07pdro       vcpdr
024400070112     c                   end
024500070119     c                   if        d07fppo <> ' '
024600070119     c                   movel     d07fppo       vfpp
024700070112     c                   end
024800070119     c                   if        d07ddeo <> 0
024900070119     c                   move      d07ddeo       dtaiso
025000070112     c                   move      dtaiso        vdta
025100070112     c                   end
025200070112     c*
025300070112     c                   endsr
025400070112     C**********************************************************************
025500070112     c     ctr01         begsr
025600070112     C**********************************************************************
025700070726     C                   SETOFF                                       285729
025701070718     c                   setoff                                       49
025800061220     c                   clear                   vmsg
025900060927     c                   do
026000070726     C* CONTROLLO FILIALE AUTOtrasp.
026100070726     C     Vfgs          IFEQ      *ZEROS
026200070726     C     '?'           SCAN      Vcpdr                                  31
026300070726     C     *IN31         IFEQ      *OFF
026400070726     C                   MOVEL     MSG(14)       VMSG
026500070726     C                   SETON                                        2857
026600060927     C                   leave
026700060926     c                   end
026800060927     c                   end
026900060926     C** ricerca
027000070726     C     '?'           SCAN      Vcpdr                                  31
027100060926     C                   if        *in31
027200060926     C                   clear                   fnlv24ds
027300060926     C                   MOVEL     tipo          d24tip
027400061213     C                   MOVEL     vfgs          d24fil
027500060926     C                   MOVEL     'R'           d24FLG
027600060926     C                   MOVEL(p)  fnlv24ds      KPJBU
027700060926     C                   CALL      'FNLV24R'
027800060926     C                   PARM                    KPJBA
027900060926     C                   MOVEL     KPJBU         fnlv24ds
027901070906     c                   if        d24pdr <> 0
028000061213     c                   move      d24pdr        vpdr
028001070906     c                   else
028002070906     c                   clear                   vcpdr
028003070906     c                   end
028100061213     c                   movel     d24rsc        vdpdr
028200060926     c                   end
028300070726     C* Filiale diversa da 0:
028400070726     C     Vfgs          IFne      *ZEROS
028500030129     c                   select
028600060203     c                   when      DUTlpo = '2' or DUTlpo = *blanks
028700070726     C* deve essere in £6 se secondo livello
028800070726     C     Vfgs          LOOKUP    L6                                     45
028900070726     C                   when      DUTlpo = '1'
029000070726     C* deve essere in £1 se primo livello
029100070726     C     Vfgs          LOOKUP    L1                                     45
029200070726     C                   when      DUTlpo = 'S'
029300070726     C* deve essere in azorg01l
029400070726     C     Vfgs          chain     azorg01l
029500030129     c                   if        %found(azorg01l)
029600030203     c                   if        orgfag <> 'A' and orgfag <> 'F'
029700030203     c                   setoff                                       45
029800030203     c                   else
029900030129     c                   seton                                        45
030000030203     c                   end
030100030129     c                   else
030200030129     c                   setoff                                       45
030300030129     c                   end
030400070726     C                   ENDsl
030500070726     C* Filiale non presente in tabella
030600070726     C     *IN45         IFEQ      *OFF
030700070726     C                   MOVEL     MSG(12)       VMSG
030800070726     C                   SETON                                        2857
030900070726     C                   leave
031000070726     C                   END
031100070726     C                   END
031101070928     C                   MOVEL     ORGde8        og148
031102070928     C                   MOVEL     ORGde7        og147
031103070928     c     §OGCGIO       comp      ' '                                    51
031200060926     C* CODICE SEMPRE > 0
031300070726     C     Vcpdr         IFEQ      *BLANKS
031400070726     C     Vcpdr         OREQ      *ZERO
031500061213     C                   MOVEL     MSG(1)        VMSG
031600060927     C                   SETON                                        28
031700060927     C                   leave
031800920918     C                   END
031900060926     C* CONTROLLO VALIDITA' CODICE
032000061213     C                   TESTN                   Vpdr                 50
032100070726     C                   MOVE      Vpdr          W001A             1
032200070726     C     *IN50         IFEQ      *OFF
032300070726     C     W001A         ORLT      '0'
032400061213     C                   MOVEL     MSG(2)        VMSG
032500060926     C                   SETON                                        57  28
032600060927     C                   leave
032700070726     C                   END
032800061213     c                   move      vpdr          kpdr
032900060926     c     kapd          chain     fiapd01l
033000060926     c                   if        %found(fiapd01l)
033100061213     c                   movel     apdrsc        vdpdr
033200070119     c                   movel     apdflr        dapdflr
033300061220     c                   else
033400061220     C                   MOVEL     MSG(2)        VMSG
033500061220     C                   SETON                                        57  28
033600061220     c                   leave
033700070119     c                   end
033701070723     c* se sono in inserimento o in riattivaz. controllo che l'anagrafico
033702070723     c* sia attiva
033703070723     c                   if        apdatb <> ' '
033704070723     C                   MOVEL     MSG(22)       VMSG
033705070723     C                   SETON                                        57  28
033706070723     c                   leave
033707070723     c                   end
033800061213     c* controllo che esista un estensione in vigore alla data richiesta
033900061213     c                   move      vdta          dtaiso
034000061218     c                   move      dtaiso        comiso            8 0
034100061218     c     kapd21        setgt     fiapd22l
034200070320     c                   if        flgcmt = *on
034300061218     c     kapd2         readpe    fiapd22l
034400070320     c                   else
034500070320     c     kapd2         readpe(n) fiapd22l
034600070320     c                   end
035000071108     c                   if        (not %eof(fiapd22l) and (
035100070726     c                             comiso<apd2dde or comiso>apd2dscad)) or
035200061220     c                             %eof(fiapd22l)
035300070208     C                   MOVEL     MSG(21)       VMSG
035400070208     C                   SETON                                        29
035500070208     C                   leave
035700070208     c                   end
035701070718     c* verifico che tipo pianificazione è abilitata
035702070718     c                   if        (§OGCGIO = ' ' and vctp = 'R') or
035703070718     c                             (§OGTLLS = ' ' and vctp = 'C')
035704070718     C                   MOVEL     MSG(26)       VMSG
035705070718     C                   SETON                                        49  28
035706070718     c                   leave
035707070718     c                   end
035800060927     c*
035900060927     c                   enddo
036000060927     c*
036100060926     c                   endsr
036200060926     C**************************************************************************
036300060926     C     SRAZZ2        BEGSR
036400060926     C**************************************************************************
036500071126     C                   SETOFF                                       404115
036600071126     C                   SETOFF                                       444716
036601071126     c                   setoff                                       282425
036602071126     c                   setoff                                       261112
036603071126     c                   clear                   vmsg
036604071126     c                   movea     '000000000000'*in(1)
036700061222     c                   move      vpdr          kpdr
036800061222     c     kapd          chain     fiapd01l
036900061222     c                   if        %found(fiapd01l)
037000061222     c                   movel     apdrsc        vdpdr
037100070119     c                   movel     apdflr        dapdflr
037200070119     c                   else
037300070119     c                   eval      d02err = '1'
037400070119     c                   movel     fidg02ds      kpjbu
037500070119     c                   seton                                        lr
037600070119     c                   return
037700070119     c                   end
037701070717     c                   movel     vdta          vdde
037702070717     c                   movel     '2039-12-31'  vscad
037703070717     c                   clear                   vatb
037704070717     c                   clear                   savnras
037705070717     c                   clear                   hdde
037706070717     c                   clear                   hscad
037800060927     c                   clear                   tc
037900060927     c                   clear                   dtc
037901070831     c                   eval      vnftl=999
037902070831     c                   if        vctp = 'C'
037904070718     c                   eval      vpkl = apdpkl
037905070718     c                   eval      vvlu = apdvlu
037906070831     c                   else
037908070831     c                   clear                   vpkl
037909070831     c                   clear                   vvlu
037910070831     c                   end
037911070718     c* d03
038000061218     c                   eval      vftc = 'E'
038100060927     c                   clear                   ts
038200060927     c                   clear                   dts
038300061214     c                   eval      vfts = 'E'
038700060927     c                   clear                   vtol
038701070718     c* d04
038702070718     c                   eval      vspi = 'N'
038704070718     c                   clear                   vtpa
038705070718     c                   clear                   vrtol
039500070115     c                   seton                                        88
039600061221     c* se non sono in inserimento controllo se esiste il record
039700070913     c                   if        not *inkj
039800070913     c                             or (d02file='FIDST00F' and d02cmd='KJ')
039801070913     c                             or (d02file='FIDST00F' and d02cmd='KF'
039802070913     c                             and d02ana = '1')
039900061218     c                   move      vdta          dtaiso
040000061218     c                   move      dtaiso        comiso
040100061218     c     kapd21        setgt     fiapd22l
040200070320     c                   if        d02file='FIDST00F' or
040300070320     c                             flgcmt = *off
040400070202     c     kapd2         readpe(n) fiapd22l                               88
040500070202     c                   else
040600061218     c     kapd2         readpe    fiapd22l                               88
040700070202     c                   end
040701070724     c* per le distinte l'anagrafico deve essere attivo mentre per
040702070724     c* l'anagrafico no perchè si può riattivare
040800061222     c                   if        not *in88 and comiso >= apd2dde and
040900070724     c                             comiso <= apd2dscad and ((
040901070724     c                             d02file='FIDST00F' and apd2atb = ' ') or
040902070724     c                             d02file<>'FIDST00F')
041000061222     C                   EXSR      SRCAMPI
041100061222     c                   else
041200061222     c                   seton                                        88
041300061222     c                   end
041400061222     c                   end
041500061213     c                   if        d02file = 'FIDST00F'
041600070320     c                   if        flgcmt = *off
041700070410     c     kdst          chain(n)  FIDST09l                           88
041800070320     c                   else
041900070410     c     kdst          chain     FIDST09l                           88
042000070320     c                   end
042100060929     C  n88              EXSR      SRCAMPI1
042200061011     c                   end
042300061011     c                   if        vinc = ' '
042400061011     c                   eval      vinc = 'S'
042500061011     c                   end
042501070717     c                   if        vrinc= ' '
042502070718     c                   eval      vrinc= 'S'
042503070612     c                   end
042504070718     c                   if        vspi= ' '
042505070718     c                   eval      vspi = 'N'
042506070718     c                   end
042600061221     c* trovato il record
042700060929     c                   if        not *in88
042800070319     c                   select
042900070320     c* estensione annullata, entro in riattivazione
043000070319     c                   when      vatb = 'A'
043100060927     c                   eval      vcmd ='RIATTIVAZIONE'
043200060928     C                   SETON                                        41
043300070320     c* richiesto annullamento oppure anagrafico aut senza giri, entro in
043400070320     c* annullamento
043500070718     c                   when      d02cmd = 'KQ'
043800060927     c                   seton                                        40
043900060927     C                   EVAL      VCMD='ANNULLAMENTO'
044000070413     c* aggiornamento
044100070413     c                   when      d02cmd = 'KF' or d02cmd = ' '
044101070913     c                   if        d02ana = '1'
044201070913     C                   EVAL      VCMD='INSERIMENTO'
044202070913     c                   else
044203070913     C                   EVAL      VCMD='AGGIORNAMENTO'
044204070913     c                   end
044300070719     c                   seton                                        4440
044400070319     c                   other
044500070320     c* visualizzazione
044600070319     C                   EVAL      VCMD='VISUALIZZA'
044700070319     c                   seton                                        47
044800070319     c                   endsl
044900060928     C                   ELSE
045000070115     c* se sono richiamato, non trovo il record e non sono in inserimento
045100070115     c* esco
045200070115     c                   if        d02cmd <>'KJ' and d02cmd <> ' ' and
045300070115     c                             not *inkj
045400070119     c                   eval      d02err = '1'
045500070119     c                   movel     fidg02ds      kpjbu
045600060928     c                   seton                                        lr
045700060928     c                   return
045800060928     c                   end
045900070320     c* inserimento
046000061218     c                   clear                   fiapd2ds
046100060927     C                   EVAL      VCMD='INSERIMENTO'
046200060928     c                   seton                                        42
046300060927     C                   END
046400061222     c* decodifico mattina/pomeriggio/giornata
046500061222     c                   select
046600061222     c                   when      vfpp = 'M'
046700061222     c                   eval      vdfpp = 'Mattina'
046800070125     c                   eval      vdfpp1= 'Mattina'
046900061222     c                   when      vfpp = 'P'
047000061222     c                   eval      vdfpp = 'Pomeriggio'
047100070125     c                   eval      vdfpp1= 'Pomeriggio'
047200061222     c                   endsl
047300060927     c                   endsr
047400060926     C**************************************************************************
047500060926     C     SRcampi       BEGSR
047600060926     C**************************************************************************
047700060927     c                   movea     tc1           tc
047800061213     c                   movea     ts1           ts
047900060929     c                   exsr      srdec
048001070719     c* devo testare se giri annulati
048002070719     c                   if        vctp = 'C'
048003070719     c     apd2nras      chain(n)  fiapd31l
048004070719     c                   if        %found(fiapd31l)
048005070719     c                   eval      vatb = apd3atb
048006070719     c                   else
048007070719     c                   clear                   vatb
048008070719     c                   end
048009070719     c                   else
048010070719     c     apd2nras      chain(n)  fiapd51l
048011070719     c                   if        %found(fiapd51l)
048012070719     c                   eval      vatb = apd5atb
048013070719     c                   else
048014070719     c                   clear                   vatb
048015070719     c                   end
048016070719     c                   end
048100061213     c                   eval      vftc = apd2ftc
048200061213     c                   eval      vfts = apd2fts
048300060926     c                   eval      vinc = apd2inc
048303070718     c                   eval      vtol = apd2tol
048900061222     c                   movel     apd2dde       hdde
049000070119     c                   movel     apd2dscad     hscad
049100061213     c                   movel     apd2dde       dtaiso
049200061213     c                   movel     dtaiso        dtaeur
049300061213     c                   movel     dtaeur        vdde
049400070119     c                   movel     apd2dscad     dtaiso
049500061213     c                   movel     dtaiso        dtaeur
049600070119     c                   movel     dtaeur        vscad
049700070119     c                   eval      savnras= apd2nras
049701070718     c                   eval      vrinc = apd2rinc
049702070718     c                   eval      vrtol = apd2rtol
049703070718     c                   eval      vspi = apd2rspi
049704070718     c                   eval      vtpa = apd2rtpa
049705070718     c* consegne
049706070718     c                   if        vctp = 'C'
049707070719     c                   if        apd2nftl <> 0
049708070718     c                   eval      vnftl= apd2nftl
049709070719     c                   end
049710070719     c                   if        apd2pkl <> 0
049711070718     c                   eval      vpkl = apd2pkl
049712070719     c                   end
049713070719     c                   if        apd2vlu <> 0
049714070718     c                   eval      vvlu = apd2vlu
049715070719     c                   end
049716070718     c                   else
049717070718     c* ritiri
049718070719     c                   if        apd2rnftl <> 0
049719070718     c                   eval      vnftl= apd2rnftl
049720070719     c                   end
049721070719     c                   if        apd2rpkl <> 0
049722070718     c                   eval      vpkl = apd2rpkl
049723070719     c                   end
049724070719     c                   if        apd2rvlu <> 0
049725070718     c                   eval      vvlu = apd2rvlu
049726070719     c                   end
049727070718     c                   end
049800940411     C*
049900910122     C                   ENDSR
050000060929     C**************************************************************************
050100060929     C     SRcampi1      BEGSR
050200060929     C**************************************************************************
050300060929     c                   movea     tc1d          tc
050400060929     c                   movea     ts1d          ts
050500060929     c                   exsr      srdec
050501070719     c* devo testare se giri annulati
050502070719     c                   if        vctp = 'C'
050503070719     c     kdst1         chain(n)  fidst31l
050504070719     c                   if        %found(fidst31l)
050505070719     c                   eval      vatb = dst3atb
050506070719     c                   else
050507070719     c                   clear                   vatb
050508070719     c                   end
050509070719     c                   else
050510070719     c     kdst1         chain(n)  fidst51l
050511070719     c                   if        %found(fidst51l)
050512070719     c                   eval      vatb = dst5atb
050513070719     c                   else
050514070719     c                   clear                   vatb
050515070719     c                   end
050516070719     c                   end
050700061213     c                   eval      vftc = dstftc
050800061213     c                   eval      vfts = dstfts
050900061213     c                   eval      vinc = dstinc
051000060929     c                   eval      vtol = dsttol
051501070718     c                   eval      vrinc = dstrinc
051502070718     c                   eval      vrtol = dstrtol
051503070718     c                   eval      vtpa = dstrtpa
051504070718     c                   eval      vspi = dstrspi
051505070718     c* consegne
051506070718     c                   if        vctp = 'C'
051507070719     c                   if        dstnftl <> 0
051508070718     c                   eval      vnftl= dstnftl
051509070719     c                   end
051510070719     c                   if        dstpkl <> 0
051511070718     c                   eval      vpkl = dstpkl
051512070719     c                   end
051513070719     c                   if        dstvlu <> 0
051514070718     c                   eval      vvlu = dstvlu
051515070719     c                   end
051516070718     c                   else
051517070718     c* ritiri
051518070719     c                   if        dstrnftl <> 0
051519070718     c                   eval      vnftl= dstrnftl
051520070719     c                   end
051521070719     c                   if        dstrpkl  <> 0
051522070718     c                   eval      vpkl = dstrpkl
051523070719     c                   end
051524070719     c                   if        dstrvlu <> 0
051525070718     c                   eval      vvlu = dstrvlu
051526070719     c                   end
051527070718     c                   end
051600060929     C*
051700060929     C                   ENDSR
051800060929     C**************************************************************************
051900060929     C     SRdec         BEGSR
052000060929     C**************************************************************************
052100060929     c* decodifica consegna particolare
052200060929     c                   eval      kcodt= '1P'
052300060929     c                   do        5             xx
052400060929     c                   if        tc(xx) <> ' '
052500060929     c                   movel     tc(xx)        kkeyt
052600060929     c     ktabel        chain     tabel00f
052700060929     c                   if        %found(tabel00f)
052800060929     c                   movel     tbluni        ds1p
052900060929     c                   movel     §1pdes        dtc(xx)
053000060929     c                   end
053100070125     c                   else
053200070125     c                   clear                   dtc(xx)
053300060929     c                   end
053400060929     c                   enddo
053500060929     c* decodifica tipo servizio
053600060929     c                   eval      kcodt= '5E'
053700060929     c                   do        2             xx
053800060929     c                   if        ts(xx) <> ' '
053900060929     c                   movel     ts(xx)        kkeyt
054000060929     c     ktabel        chain     tabel00f
054100060929     c                   if        %found(tabel00f)
054200060929     c                   movel     tbluni        ds5e
054300060929     c                   movel     §5edes        dts(xx)
054400060929     c                   end
054500070125     c                   else
054600070125     c                   clear                   dts(xx)
054700060929     c                   end
054800060929     c                   enddo
054900060929     C*
055000060929     C                   ENDSR
055100940809     C**************************************************************************
055200060926     C     srd02         BEGSR
055300940809     C**************************************************************************
055400060926     C                   EXSR      SRAZZ2
055500061221     C                   EXSR      gess01
055600940809     C*
055700060926     c                   do        *hival
055800061213     C                   WRITE     dg02T01
055900061218     C                   WRITE     dg02d02
056000070320     c* proteggo il video se record annullato o in visualizzazione
056100070319     c                   select
056200070719     c                   when      *in41 or *in47
056300061213     C                   write     dg02c01
056400060928     C                   EXFMT     protect
056500070319     c                   other
056600061213     C                   EXFMT     dg02c01
056700070319     c                   endsl
056800060926     c*
056900060926     c                   if        *inkc or *inkl
057000060926     C                   leave
057100060926     c                   end
057600061229     c* duplica da altro autotrasportatore
057700061229     c                   if        *inkg
057800061229     c                   exsr      srw01
057900061229     c                   end
058000070125     c* altri dati
058100070125     c                   if        *inkb
058200070320     c                   seton                                        46
058201070718     c* consegne
058202070718     c                   if        vctp = 'C'
058300070125     c                   exsr      srd03
058301070718     c                   else
058302070718     c* ritiri
058303070718     c                   exsr      srd04
058304070718     c                   end
058400070320     c                   setoff                                       46
058500070125     c                   if        *inkc
058600070125     C                   leave
058700070125     c                   end
058800070125     c                   end
058900061229     C* CONTROLLI
059000070330     C  n47              EXSR      Ctr02
059100061229     C   28              iter
059200061221     C* CMD16 - ANNULLAMENTO solo per anagrafico (non abilitato x 43)
059300060926     C                   if        *inkq
059301070718     c* consegne
059302070718     c                   if        vctp = 'C'
059303070718     c                   exsr      srannc
059304070718     c                   else
059305070718     c* ritiri
059306070718     c                   exsr      srannr
059307070718     c                   end
062200061221     c                   leave
062300060927     c                   ENDIF
062400920921     C*
062500060927     c                   if        *inke or *inkf
062600061213     c                   if        d02file <> 'FIDST00F'
062700920921     C                   EXSR      SCRIVI
062800061011     c                   else
062900060929     C                   EXSR      SCRIVI1
063000061011     c                   end
063100061220     c                   if        not *in28
063200060927     c                   leave
063300061220     c                   end
063400060927     C                   ENDIF
063500931130     C* 98 ON - WRITE UPDATE O CHAIN NON ANDATE A BUON FINE
063600060926     C   98              iter
063700060927     c                   enddo
063800061221     c                   if        not *in28  and
063900061221     c                             (d02com = ' ' or d02com = 'S')
064000061221     c                   commit
064100061221     c                   end
064200920921     C*
064300060926     C                   ENDSR
064400060926     C**************************************************************************
064500070718     C     srannc        BEGSR
064600060926     C**************************************************************************
064700060927     C*
064702070718     c     apd2nras      setll     fiapd31l
064703070718     c                   do        *hival
064704070718     c     apd2nras      reade     fiapd31l
064705070718     C                   IF        %eof(fiapd31l)
064706070718     c                   leave
064707070718     c                   end
064708070718     C                   MOVE      'A'           APD3ATB
064709070718     C                   UPDATE    fiapd300                             98
064710070718     C     *IN98         IFEQ      *ON
064711070718     C                   MOVEL     MSG(4)        VMSG
064712070718     C                   SETON                                        28
064713070718     C                   iter
064714070718     c                   ENDIF
064715070718     c                   enddo
064716070718     C*
064717070718     C                   ENDSR
064718070718     C**************************************************************************
064719070718     C     srannr        BEGSR
064720070718     C**************************************************************************
064721070718     C*
064722070718     c     apd2nras      setll     fiapd51l
064723070718     c                   do        *hival
064724070718     c     apd2nras      reade     fiapd51l
064725070718     C                   IF        %eof(fiapd51l)
064726070718     c                   leave
064727070718     c                   end
064728070718     C                   MOVE      'A'           APD5ATB
064729070718     C                   UPDATE    fiapd500                             98
064730070718     C     *IN98         IFEQ      *ON
064731070718     C                   MOVEL     MSG(4)        VMSG
064732070718     C                   SETON                                        28
064733070718     C                   iter
064734070718     c                   ENDIF
064735070718     c                   enddo
064736070718     C*
064737070718     C                   ENDSR
064738070718     C**************************************************************************
064739070718     C     ctr02         BEGSR
064740070718     C**************************************************************************
064741070718     C*
064800070123     c                   setoff                                       282425
064900070718     c                   setoff                                       261112
064901071126     c                   setoff                                       15
065000061220     c                   clear                   vmsg
065100060927     c                   movea     '000000000000'*in(1)
065200061219     C* controllo data deccorenza scadenza
065300061219     c                   move      vdde          dtadde
065400070119     c                   move      vscad         dtascad
065500061229     c* se richiesto annullamento controllo che non ci siano distinte
065600061229     c* aperte
065700061229     c                   if        *inkq
065800061229     c                   if        d02file <> 'FIDST00F'
065900061229     c                   exsr      srdst
066000061229     c                   if        dstape = '1'
066100061229     C                   MOVEL     MSG(17)       VMSG
066200061229     c                   seton                                        28
066300061229     c                   end
066400061229     c                   end
066500061229     c                   else
066600061222     c                   do
066700070723     c* se sono in inserimento o in riattivaz. controllo che l'anagrafico
066701070723     c* sia attiva
066800070723     c                   if        apdatb <> ' ' and (*in42 or *in41)
066900070115     C                   MOVEL     MSG(22)       VMSG
067000070115     C                   SETON                                            28
067100070115     c                   leave
067200070115     c                   end
067300061219     c* decorrenza < scadenza
067400070119     c                   if        dtadde > dtascad
067500061220     C                   MOVEL     MSG(11)       VMSG
067600061219     c                   seton                                        281112
067700061222     c                   leave
067800061219     c                   end
067900061219     c* verifico che non ci siano anagrafiche in vigore nello stesso periodo
068000061221     c* solo x anagrafico
068100061221     c                   if        d02file <> 'FIDST00F'
068200061219     c                   move      vdde          comiso
068300061219     c     kapd2         setgt     fiapd22l
068400061221     c                   do        *hival
068500070320     c                   if        flgcmt = *on
068600061219     c     kapd2         readpe    fiapd22l
068700070320     c                   else
068800070320     c     kapd2         readpe(n) fiapd22l
068900070320     c                   end
069000061221     c                   if        %eof(fiapd22l)
069100061221     c                   leave
069200061221     c                   end
069300070119     c                   if        apd2nras= savnras and not *in42
069400061221     c                   iter
069500061221     c                   end
069600070119     c                   move      apd2dscad     comscad
069700061219     c                   move      apd2dde       comdde
069800070119     c                   if        dtadde >= comdde and dtadde <= comscad
069900061220     C                   MOVEL     MSG(15)       VMSG
070000061219     c                   seton                                        281112
070100061222     c                   leave
070200061219     c                   end
070300070119     c                   if        dtascad>= comdde and dtascad<= comscad
070400061220     C                   MOVEL     MSG(15)       VMSG
070500061219     c                   seton                                        281112
070600061222     c                   leave
070700061219     c                   end
070800070119     c                   if        dtadde <=comdde and dtascad>=comscad
070900061220     C                   MOVEL     MSG(15)       VMSG
071000061219     c                   seton                                        281112
071100061222     c                   leave
071200061219     c                   end
071300061221     c                   enddo
071400061222     c   28              leave
071501070718     c                   end
071600070119     c* controllo max fermate
071700070119     c                   if        vnftl= 0
071800070123     c                   movel     Msg(23)       Vmsg
071900070119     c                   seton                                        2824
072000070119     c                   leave
072100070119     c                   endif
072200070123     c* controllo peso
072300070123     c                   if        vpkl = 0
072400070123     c                   movel     Msg(3)        Vmsg
072500070123     c                   seton                                        2825
072600070123     c                   leave
072700070123     c                   endif
072800070124     c* controllo volume
072900070123     c                   if        vvlu = 0
073000070123     c                   movel     Msg(3)        Vmsg
073100070123     c                   seton                                        2826
073200070123     c                   leave
073300070123     c                   endif
073301071126     c* controllo che la tolleranza non superi la capienza del campo
073302071126     c* peso, volume e fermate
073304071126     c                   if        vctp = 'C'
073305071126     c                   eval      wtol = vtol
073306071126     c                   else
073307071126     c                   eval      wtol = vRtol
073308071126     c                   end
073309071126     c                   exsr      srtol
073310071126     c                   if        *in28
073311071126     c                   seton                                        15
073312071126     c                   seton                                        242526
073313071126     C                   MOVEL     MSG(28)       VMSG
073314071126     c                   leave
073315071126     c                   endif
073400061218     c* controllo il sfl
073500061218     c  n28              exsr      ctrs01
073600061222     c                   enddo
073601070718     c* se cambia date avviso che verranno variate anche per consegne/ritiri
073602071009     c* solo la prima volta e se partiti con ritiri
073603071009     c                   if        §OGCGIO <> ' '
073604070718     c                   if        d02file <> 'FIDST00F' and not *in28
073605070723     c                             and not *in42
073606070719     c                             and (dtadde<>savdde or dtascad<>savscad)
073607070719     c                   move      dtadde        savdde
073608070719     c                   move      dtascad       savscad
073609070719     c                   move      hdde          comhdde
073614070719     c                   move      hscad         comhscad
073618070718     c                   if        dtadde <> comhdde or dtascad <> comhscad
073619070718     C                   MOVEL     MSG(27)       VMSG
073620070719     c                   seton                                        28
073621070718     c                   end
073622071009     c                   end
073623070718     c                   end
073700061229     c                   end
073800060926     C*
073900060926     C                   ENDSR
074000070125     C**************************************************************************
074100070718     C* gestione altri dati consegne
074200070125     C**************************************************************************
074300070125     C     Srd03         BEGSR
074400070125     C*
074500070125     c                   setoff                                       28
074600070125     c                   clear                   $msg
074700070125     c                   do        *hival
074800070719     c* proteggo il video se sono in visualizzazione o se record annullato
074900070320     c                   select
075000070719     c                   when      *in47 or *in41
075100070319     c                   WRITE     dg02T01
075200070319     C                   WRITE     dg02d02
075300070319     C                   write     dg02d03
075400070320     C                   EXFMT     protect
075500070320     c                   other
075600070125     c                   WRITE     dg02T01
075700070125     C                   WRITE     dg02d02
075800070125     C                   EXFMT     dg02d03
075900070320     c                   endsl
076000070125     C* CMD3 - FINE LAVORO
076100070125     c                   if        *inkl or *inkc
076200070125     C                   leave
076300070125     c                   end
076400070125     c* duplica da altro autotrasportatore
076500070125     c                   if        *inkg
076600070125     c                   exsr      srw01
076700070125     c                   end
076800070125     C* CONTROLLI
076900070330     c  n47              exsr      ctr03
077000070125     c   28              iter
077100070125     c*
077200070125     c                   if        *inkf or *inkq or *inke
077300070125     C                   leave
077400070125     c                   end
077500070125     c                   enddo
077600070125     C*
077700070125     C                   ENDSR
077800070125     C**************************************************************************
077900070125     C     ctr03         BEGSR
078000070125     C**************************************************************************
078100070125     C*
078200071126     c                   setoff                                       2815
078300070125     c                   clear                   vmsg
078400070125     c                   movea     '0000000'     *in(1)
078500070125     C* consegne particolari
078600070125     c                   clear                   dtc
078700070125     C                   MOVEL     '1P'          kcodt
078800070125     c                   do        5             xx
078900070125     C* RICERCA
079000070125    2C     tc(xx)        IFEQ      '?'
079100070125     C                   clear                   KKEYT
079200070125     C                   EXSR      CX§TAB
079300070125     C                   MOVEL     kkeyt         tc(xx)
079400070125     C                   MOVEL     kdest         dtc(xx)
079500070726     C                   END
079600070726     C                   ENDdo
079700070125     C* CONTROLLO
079800070125     c                   do        5             xx
079900070125     c                   if        tc(xx) <> ' '
080000070125     C                   MOVEL(P)  tc(xx)        kKEYt
080100070125     C     KTABEL        CHAIN     TABEL00F                           31
080200070125     C     *IN31         IFEQ      *ON
080300070125     C                   MOVEL     MSG(5)        VMSG
080400070125     c                   eval      *in(xx) = '1'
080500070125     C                   SETON                                        28
080600070125     C                   leavesr
080700070125     C                   ENDIF
080800070125     C     TBLFLG        IFNE      ' '
080900070125     c                   eval      *in(xx) = '1'
081000070125     C                   MOVEL     MSG(5)        VMSG
081100070125     C                   SETON                                        28
081200070125     C                   leavesr
081300070125     C                   ENDIF
081400070125     C                   MOVEL     TBLUNI        DS1P
081500070125     c                   movel     §1pdes        dtc(xx)
081600070125     C                   IF        §1puar <> 'S' and §1pupr <> 'S'
081700070125     c                   eval      *in(xx) = '1'
081800070125     C                   MOVEL     MSG(19)       VMSG
081900070125     C                   SETON                                        28
082000070125     C                   leavesr
082100070125     C                   ENDIF
082200070125     c                   end
082300070125     c                   enddo
082400070125     c* controllo che non ci siano c.p. doppie
082500070125     c                   do        5             xx
082600070125     c                   do        5             yy
082700070125     c                   if        tc(xx) <> ' ' and tc(yy) <> ' ' and
082800070125     c                             xx <> yy and tc(xx) = tc(yy)
082900070125     c                   movel     Msg(13)       Vmsg
083000070125     c                   seton                                        28
083100070125     c                   eval      *in(yy)='1'
083200070125     c                   leavesr
083300070125     c                   endif
083400070125     c                   enddo
083500070125     c                   enddo
083600070125     C* tipo servizio
083700070125     c                   clear                   dts
083800070125     c                   MOVEL     '5E'          kcodt
083900070125     C* RICERCA
084000070125     c                   do        2             xx
084100070125     c                   if        ts(xx) = '?'
084200070125     C                   clear                   KKEYT
084300070125     C                   EXSR      CX§TAB
084400070125     C                   MOVEL     kkeyt         ts(xx)
084500070125     C                   MOVEL     kdest         dts(xx)
084600070125     c                   end
084700070125     c                   enddo
084800070125     C* CONTROLLO
084900070125     c                   do        2             xx
085000070125     c                   if        ts(xx) <> ' '
085100070125     C                   MOVEL(P)  ts(xx)        kKEYt
085200070125     C     KTABEL        CHAIN     TABEL00F                           31
085300070125     C     *IN31         IFEQ      *ON
085400070125     C                   MOVEL     MSG(6)        VMSG
085500070125     c                   eval      yy = xx +5
085600070125     c                   eval      *in(yy) = '1'
085700070125     C                   SETON                                        28
085800070125     C                   leavesr
085900070125     C                   ENDIF
086000070125     C     TBLFLG        IFNE      ' '
086100070125     C                   MOVEL     MSG(6)        VMSG
086200070125     c                   eval      yy = xx +5
086300070125     c                   eval      *in(yy) = '1'
086400070125     C                   SETON                                        28
086500070125     C                   leavesr
086600070125     C                   ENDIF
086700070125     C                   MOVEL     TBLUNI        DS5e
086800070125     c                   movel     §5edes        dts(xx)
086900070125     c                   end
087000070125     c                   enddo
087100070125     c* controllo che non ci siano t.s. doppi
087200070125     c                   if        ts(1) <> ' ' and ts(2) <> ' '
087300070125     c                             and ts(1) = ts(2)
087400070125     c                   movel     Msg(18)       Vmsg
087500070125     c                   seton                                        280607
087600070125     c                   leavesr
087700070125     c                   endif
087701071126     c* controllo che la tolleranza non superi la capienza del campo
087702071126     c* peso, volume e fermate
087703071126     c                   eval      wtol = vtol
087704071126     c                   exsr      srtol
087705071126     c                   if        *in28
087706071126     c                   seton                                        15
087707071126     c                   seton                                        242526
087708071126     C                   MOVEL     MSG(28)       VMSG
087709071126     c                   leavesr
087710071126     c                   endif
087800070125     C*
087900070125     C                   ENDSR
087901071126     C**************************************************************************
087902071126     C     ctr04         BEGSR
087903071126     C**************************************************************************
087904071126     C*
087905071126     c                   setoff                                       2816
087906071126     c                   clear                   vmsg
088001071126     c* controllo che la tolleranza non superi la capienza del campo
088002071126     c* peso, volume e fermate
088003071126     c                   eval      wtol = vRtol
088004071126     c                   exsr      srtol
088005071126     c                   if        *in28
088006071126     c                   seton                                        16
088007071126     c                   seton                                        242526
088008071126     C                   MOVEL     MSG(28)       VMSG
088009071126     c                   leavesr
088010071126     c                   endif
088011071126     C*
088012071126     C                   ENDSR
088013070718     C**************************************************************************
088014071126     c* controllo che la tolleranza non superi la capienza del campo
088015071126     c* peso, volume e fermate
088016070718     C**************************************************************************
088017071126     C     srtol         BEGSR
088018070718     C*
088019071126     c                   eval      wpeso =((vpkl * wtol)/100) + vpkl
088020071126     c                   eval      wvolume =((vvlu * wtol)/100) + vvlu
088021071126     c                   eval      wnftl =((vnftl* wtol)/100) + vnftl
088022071126     c                   if        wpeso > 999999,9 or
088023071126     c                             wvolume > 99,999 or
088024071126     c                             wnftl > 99999
088025071126     c                   seton                                        28
088026071126     c                   end
088027071126     C*
088028071126     C                   ENDSR
088029071126     C**************************************************************************
088030071126     C* gestione altri dati ritiri
088031071126     C**************************************************************************
088032071126     C     Srd04         BEGSR
088033071126     C*
088034071126     c                   setoff                                       2816
088035070718     c                   clear                   $msg
088036070718     c                   do        *hival
088037070719     c* proteggo il video se sono in visualizzazione o se record annullato
088038070718     c                   select
088039070719     c                   when      *in47 or *in41
088040070718     c                   WRITE     dg02T01
088041070718     C                   WRITE     dg02d02
088042070718     C                   write     dg02d04
088043070718     C                   EXFMT     protect
088044070718     c                   other
088045070718     c                   WRITE     dg02T01
088046070718     C                   WRITE     dg02d02
088047070718     C                   EXFMT     dg02d04
088048070718     c                   endsl
088049070718     C* CMD3 - FINE LAVORO
088050070718     c                   if        *inkl or *inkc
088051070718     C                   leave
088052070718     c                   end
088053071126     C* CONTROLLI
088054071126     c  n47              exsr      ctr04
088055071126     c   28              iter
088066070718     c* duplica da altro autotrasportatore
088067070718     c                   if        *inkg
088068070718     c                   exsr      srw01
088069070718     c                   end
088070070718     c*
088071070718     c                   if        *inkf or *inkq or *inke
088072070718     C                   leave
088073070718     c                   end
088074070718     c                   enddo
088075070718     C*
088076070718     C                   ENDSR
088077061219     C**************************************************************************
088100061219     C* GESTIONE SUBFILE
088200061219     C**************************************************************************
088300061229     C     srdst         BEGSR
088400061229     c                   clear                   dstape            1
088500061229     c                   clear                   dstfcf
088600061219     C*
088700061229     c     kdst4         setll     fidst04l
088800061229     c                   do        *hival
088900061229     c     kdst4         reade     fidst04l
089000061229     c                   if        %eof(fidst04l)
089100061229     c                   leave
089200061229     c                   end
089300070419     c                   if        dstatb <> ' '
089400070419     c                   iter
089500070419     c                   end
089600061229     c                   move      dstdfv        comdta
089700070119     c                   if        comdta>=dtadde and comdta<=dtascad
089701070627     c     kdst31        setll     fidst31l                               33
089702070627     c                   if        *in33
089800061229     c                   eval      dstape = '1'
089900061229     c                   leave
089901070627     c                   end
089902070720     c     kdst31        setll     fidst51l                               33
089903070720     c                   if        *in33
089904070720     c                   eval      dstape = '1'
089905070720     c                   leave
089906070720     c                   end
090000061229     c                   end
090100061229     c                   enddo
090200061229     C*
090300061229     C                   ENDSR
090400061229     C**************************************************************************
090500061229     C* GESTIONE SUBFILE
090600061229     C**************************************************************************
090700061229     C     GESS01        BEGSR
090900061229     C*
091000061219     C* PULIZIA SUBFILE
091100061219     C                   SETON                                        21
091200061219     C                   WRITE     dg02C01
091300061219     C                   SETOFF                                       2122
091400061219     C                   Z-ADD     0             NRR               4 0
091500061219     C*
091600061219     C* CARICAMENTO SUBFILE
091700070913     c                   if        d02file = 'FIAPD20F' or
091800070913     c                             d02file = *blanks or
091900070913     c                             (d02file = 'FIDST00F' and d02cmd = 'KJ'
092000070913     c                             and *in42) or
092001070913     c                             (d02file = 'FIDST00F' and d02cmd = 'KF'
092002070913     c                             and d02ana='1')
092100070202     c                   eval      flagcopia='0'
092101070718     c* consegne
092102070718     c                   if        vctp = 'C'
092200070718     c                   exsr      carapdc
092201070718     c                   else
092202070718     c* ritiri
092203070718     c                   exsr      carapdr
092204070913     c                   end
092300061222     c                   end
092400061221     c                   if        d02file = 'FIDST00F'
092401070718     c* consegne
092402070718     c                   if        vctp = 'C'
092500070718     c                   exsr      cardstc
092501070718     c                   else
092502070718     c* ritiri
092503070720     c                   exsr      cardstr
092504070718     c                   end
092600061221     c                   end
092700070125     C* carico 100 righe vuote
092800070125     c                   do        100
092900070124     c                   setoff                                       080910
093000070627     c                   setoff                                       1314
093100070129     c                   eval      vscfgsg = vfgs
093200070129     c                   clear                   vsccgi
093300070124     c                   clear                   vsdcgi
093400070119     c                   clear                   vscprgg
093500061219     c                   clear                   vscsql
093501090113     c                   if        nrr < 9999
093600061219     C                   ADD       1             NRR
093700061219     C                   WRITE     dg02s01
093701090113     c                   else
093702090113     c                   leave
093703090113     c                   end
093800061219     c                   enddo
093900061219     c                   seton                                        22
094000061219     C                   Z-ADD     1             REC
094100061219     C                   ENDSR
094200061218     C**************************************************************************
094300070718     C* Carica sfl anagrafico consegne
094400061218     C**************************************************************************
094500070718     C     Carapdc       BEGSR
094600061218     C*
094700070119     C     savnras       SETLL     fiapd31l
094800061221     c                   do        *hival
094900070202     c                   if        flagcopia = '1' or d02file='FIDST00F'
095000070320     c                             or flgcmt = *off
095100070202     C     savnras       READe(n)  fiapd31l                               32
095200070202     c                   else
095300070119     C     savnras       READe     fiapd31l                               32
095400070202     c                   end
095500061221     c                   if        %eof(fiapd31l)
095600061221     c                   leave
095700061221     c                   end
095701071108     c* se il record è annulato lo ignoro
095702071108     c                   if        d02file = 'FIDST00F'
095703071108     c                             and apd3atb <> ' '
095704071108     c                   iter
095705071108     c                   end
095800061221     C*
095801070719     c                   eval      vatb = apd3atb
095900070129     c                   eval      vscfgsg= apd3fgsg
096000070129     c                   eval      vsccgi = apd3cgi
096100070129     c                   clear                   vsdcgi
096200070124     c                   exsr      srcgi
096300070119     c                   eval      vscprgg= apd3prgg
096400061221     c                   eval      vscsql = apd3sql
096500061221     C                   ADD       1             NRR
096600061221     C                   WRITE     dg02s01
096700070726     C                   ENDDO
096800061221     C*
096900061221     C                   ENDSR
096901070718     C**************************************************************************
096902070718     C* Carica sfl anagrafico ritiri
096903070718     C**************************************************************************
096904070718     C     Carapdr       BEGSR
096905070718     C*
096906070718     C     savnras       SETLL     fiapd51l
096907070718     c                   do        *hival
096908070718     c                   if        flagcopia = '1' or d02file='FIDST00F'
096909070718     c                             or flgcmt = *off
096910070718     C     savnras       READe(n)  fiapd51l                               32
096911070718     c                   else
096912070718     C     savnras       READe     fiapd51l                               32
096913070718     c                   end
096914070718     c                   if        %eof(fiapd51l)
096915070718     c                   leave
096916070718     c                   end
096917071108     c* se il record è annulato lo ignoro
096918071108     c                   if        d02file = 'FIDST00F'
096919071108     c                             and apd5atb <> ' '
096920071108     c                   iter
096921071108     c                   end
096922070718     C*
096923070719     c                   eval      vatb = apd5atb
096924070718     c                   eval      vscfgsg= apd5fgsg
096925070718     c                   eval      vsccgi = apd5cgi
096926070718     c                   clear                   vsdcgi
096927070718     c                   exsr      srcgi
096928070718     c                   eval      vscprgg= apd5prgg
096929070718     c                   eval      vscsql = apd5sql
096930070718     C                   ADD       1             NRR
096931070718     C                   WRITE     dg02s01
096932070726     C                   ENDDO
096933070718     C*
096934070718     C                   ENDSR
097000061221     C**************************************************************************
097100070718     C* Carica sfl distinta consegne
097200061221     C**************************************************************************
097300070718     C     Cardstc       BEGSR
097400061221     C*
097500061228     C     kdst1         SETLL     fidst31l
097600061221     c                   do        *hival
098101070627     c                   if        flagcopia = '1'
098102070627     c                             or flgcmt = *off
098103070627     C     kdst1         READe(n)  fidst31l                               32
098104070627     c                   else
098105070627     C     kdst1         READe     fidst31l                               32
098106070627     c                   end
098200061222     c                   if        %eof(fidst31l)
098300061221     c                   leave
098400061221     c                   end
098500061221     C*
098600070129     c                   eval      vscfgsg= dst3fgsg
098700070129     c                   eval      vsccgi = dst3cgi
098800070129     c                   clear                   vsdcgi
098900070124     c                   exsr      srcgi
099000070119     c                   eval      vscprgg= dst3prgg
099100061221     c                   eval      vscsql = dst3sql
099200061221     C                   ADD       1             NRR
099300061221     C                   WRITE     dg02s01
099400070726     C                   ENDDO
099500061221     C*
099600061221     C                   ENDSR
099601070718     C**************************************************************************
099602070718     C* Carica sfl distinta ritiri
099603070718     C**************************************************************************
099604070718     C     Cardstr       BEGSR
099605070718     C*
099606070718     C     kdst1         SETLL     fidst51l
099607070718     c                   do        *hival
099608070718     c                   if        flagcopia = '1'
099609070718     c                             or flgcmt = *off
099610070718     C     kdst1         READe(n)  fidst51l                               32
099611070718     c                   else
099612070718     C     kdst1         READe     fidst51l                               32
099613070718     c                   end
099614070718     c                   if        %eof(fidst51l)
099615070718     c                   leave
099616070718     c                   end
099617070718     C*
099618070718     c                   eval      vscfgsg= dst5fgsg
099619070718     c                   eval      vsccgi = dst5cgi
099620070718     c                   clear                   vsdcgi
099621070718     c                   exsr      srcgi
099622070718     c                   eval      vscprgg= dst5prgg
099623070718     c                   eval      vscsql = dst5sql
099624070718     C                   ADD       1             NRR
099625070718     C                   WRITE     dg02s01
099626070726     C                   ENDDO
099627070718     C*
099628070718     C                   ENDSR
099700061221     C**************************************************************************
099800061221     C* CONTROLLO DATI SUBFILE
099900061221     C**************************************************************************
100000061221     C     CTRS01        BEGSR
100100061221     C*
100200090113     c                   z-add     0             y                 4 0
100300090113     c                   z-add     0             z                 4 0
100400070123     c                   clear                   cgi
100500061219     c                   clear                   prsq
100600061220     c                   setoff                                       28
100700061220     c                   clear                   vmsg
100800070726     C                   do        *hival        nn
100900061220     C     nn            chain     dg02S01                            31
101000070726     C   31              leave
101100061220     c                   setoff                                       080910
101200070627     c                   setoff                                       1314
101300061218     C                   Z-ADD     NRR           REC
101400070124     c                   if        vsccgi = ' '
101500070124     c                   clear                   vsdcgi
101600070124     c                   end
101700061218     C* CONTROLLI
101800070208     C* p.o. giro
101900070208     C     Vscfgsg       IFEQ      *ZEROS
102000070208     C                   MOVEL     MSG(14)       VMSG
102100070208     C                   SETON                                        2813
102200070129     c                   else
102300070129     c                   select
102400070129     c                   when      DUTlpo = '2' or DUTlpo = *blanks
102500070208     C* deve essere in £6 se secondo livello
102600070208     C     Vscfgsg       LOOKUP    L6                                     45
102700070208     C                   when      DUTlpo = '1'
102800070208     C* deve essere in £1 se primo livello
102900070208     C     Vscfgsg       LOOKUP    L1                                     45
103000070208     C                   when      DUTlpo = 'S'
103100070208     C* deve essere in azorg01l
103200070208     C     Vscfgsg       chain     azorg01l
103300070129     c                   if        %found(azorg01l)
103400070129     c                   if        orgfag <> 'A' and orgfag <> 'F'
103500070129     c                   setoff                                       45
103600070129     c                   else
103700070129     c                   seton                                        45
103800070129     c                   end
103900070129     c                   else
104000070129     c                   setoff                                       45
104100070129     c                   end
104200070208     C                   ENDsl
104300070208     C* Filiale non presente in tabella
104400070208     C                   IF        not *in45 and not *in28
104500070208     C                   MOVEL     MSG(12)       VMSG
104600070208     C                   SETON                                        2813
104700070208     C                   END
104800070129     c                   end
104900070129     c* giro
105000070124     c                   if        vsccgi <> ' ' and not *in28
105100070726     C     '?'           SCAN      Vsccgi                                 31
105200070124     c                   if        *in31
105300070124     C                   EXSR      srriccgi
105400070124     c                   end
105500070124     C                   EXSR      srcgi
105600070124     c                   if        not *in28
105700090113     c*                  z-add     1             x                 3 0
105800070129     c                   movel     vscfgsg       com13            13
105900070129     c                   move      vsccgi        com13
106000070129     c     com13         lookup    cgi                                    08
106100061219     c                   if        *in08
106200070129     c                   seton                                        2813
106300061220     c                   movel     Msg(10)       Vmsg
106400061219     c                   else
106500061220     c                   add       1             y
106600070129     c                   movel     vscfgsg       cgi(y)
106700070129     c                   move      vsccgi        cgi(y)
106800061219     c                   end
106900070124     c                   end
107000061220     c*
107100070119     c     vscprgg       comp      0                                      09
107200061220     c                   if        *in09 and not *in28
107300061219     c                   seton                                        28
107400061220     c                   movel     Msg(7)        Vmsg
107500061219     c                   end
107600070124     c*
107700061219     c     vscsql        comp      0                                      10
107800061220     c                   if        *in10 and not *in28
107900061219     c                   seton                                        28
108000061220     c                   movel     Msg(16)       Vmsg
108100061219     c                   end
108200061220     c                   end
108300070208     c*
108400070208     c                   if        not *in28
108500070208     c                   if        (vsccgi = ' ' and
108600070208     c                             (vscprgg<> 0 or vscsql <> 0)) or
108700070208     c                             (vsccgi<> ' ' and
108800070208     c                             (vscprgg= 0 or vscsql = 0))
108900070208     c                   seton                                        280809
109000070208     c                   seton                                        1013
109100070208     c                   movel     Msg(3)        Vmsg
109200070208     c                   end
109300070208     c                   end
109400061220     c* stessa priorità sequenza
109500061220     c                   if        not *in28 and
109600070119     c                             (vscprgg<> 0 or vscsql <> 0)
109700090113     c*                  z-add     1             x                 3 0
109800070119     c                   movel     vscprgg       comps             4 0
109900061219     c                   move      vscsql        comps
110000061219     c     comps         lookup    prsq                                   28
110100061220     c                   if        *in28
110200061219     c                   seton                                        0910
110300061220     c                   movel     Msg(8)        Vmsg
110400061219     c                   else
110500061220     c                   add       1             z
110600061220     c                   move      comps         prsq(z)
110700061219     c                   end
110800061220     c                   end
110900061218     C*
111000061219     C                   UPDATE    dg02S01
111100061218     C   28              leave
111200061218     C*
111300070726     C                   ENDDO
111400061219     c* almeno un giro
111500070720     c*                  if        y = 0 and not *in28
111600070720     c*    1             chain     dg02s01
111700070720     c*                  seton                                        280813
111800070720     c*                  update    dg02s01
111900070720     c*                  movel     Msg(9)        Vmsg
112000070720     c*                  end
112100061220     c                   z-add     1             rec
112200061218     C*
112300061218     C                   ENDSR
112400940809     C**************************************************************************
112500070124     C* controllo e decodifico giro
112600940809     C**************************************************************************
112700070124     C     Srcgi         BEGSR
112800940809     C*
112900070627     c                   setoff                                       081314
113000070129     c                   clear                   fidg09ds
113100070129     c                   eval      D09IOP0 = '001'
113200070129     c                   eval      d09ifgs = vscfgsg
113300070129     c                   eval      d09icgi = vsccgi
113301070718     c                   eval      D09ITUG = vctp
113700070129     c                   if        d02file = 'FIDST00F'
113800070129     c                   move      vdfv          dtaiso
113900070129     c                   move      dtaiso        d09idat
114000070129     c                   else
114100070129     c                   move      vdde          dtaiso
114200070129     c                   move      dtaiso        d09idat
114300070129     c                   end
114400070129     c                   eval      kpjbu = fidg09ds
114500070129     c                   call      'FIDG09R'
114600070129     c                   parm                    kpjba
114700070129     c                   eval      fidg09ds = kpjbu
114800070129     c                   if        d09oerr = '1'
114900070129     c                   seton                                        280813
115000070129     c                   movel     Msg(2)        Vmsg
115100070129     c                   else
115200070320     c                   eval      dgrgflo = D09OFLO
115300070320     c                   if        §grgdst = 'N'
115400070320     c                   seton                                        280813
115500070320     c                   movel     Msg(25)       Vmsg
115600070320     c                   else
115700070130     c                   if        d09ocgi <> ' '
115800070129     c                   eval      vscfgsg= d09ofgs
115900070129     c                   eval      vsccgi = d09ocgi
116000070129     c                   eval      vsdcgi = d09odes
116001070627     c     udate_8       comp      D09ODSC                            14
116100070320     c                   end
116200070129     c                   end
116300070130     c                   end
116400070124     C*
116500070124     C                   ENDSR
116600070124     C**************************************************************************
116700070124     C* gestione duplica da altro aut.
116800070124     C**************************************************************************
116900070124     C     Srw01         BEGSR
117000070124     C*
117100061229     c                   setoff                                       2829
117200061229     c                   clear                   $msg
117300061229     c                   clear                   w01pdr
117400070111     c                   eval      w01fpp = vfpp
117500061229     c                   movel     vdta          w01dta
117600070123     c                   movel     'S'           w01cgi
117601070627     c                   movel     'N'           w01acc
117602071008     c                   movel     vctp          w01tp
117700061229     c                   do        *hival
117800061229     c*
117900061229     C                   EXFMT     dg02w01
118000061229     C* CMD3 - FINE LAVORO
118100061229     C   Kl              leave
118200061229     c                   exsr      ctrw01
118300061229     c* errore
118400061229     c   90              iter
118500070719     c                   exsr      srcopia
118600061229     c                   leave
118700061229     c                   enddo
118800061229     C*
118900061229     C                   ENDSR
119000061229     C**********************************************************************
119100061229     c     ctrw01        begsr
119200061229     C**********************************************************************
119300070726     C                   SETOFF                                       9058
119400061229     c                   clear                   $msg
119500061229     c                   do
119600061229     C** ricerca
119700070726     C     '?'           SCAN      w01pdr                                 31
119800061229     C                   if        *in31
119900061229     C                   clear                   fnlv24ds
120000061229     C                   MOVEL     tipo          d24tip
120100061229     C                   MOVEL     vpdr          d24fil
120200061229     C                   MOVEL     'R'           d24FLG
120300061229     C                   MOVEL(p)  fnlv24ds      KPJBU
120400061229     C                   CALL      'FNLV24R'
120500061229     C                   PARM                    KPJBA
120600061229     C                   MOVEL     KPJBU         fnlv24ds
120700061229     c                   move      d24pdr        w01pdr
120701070703     c                   seton                                        90
120800061229     c                   end
120900061229     c                   movel     w01pdr        com03             3 0
121000070726     C* Filiale diversa da 0:
121100070726     C     com03         IFne      *ZEROS
121200070726     c                   select
121300061229     c                   when      DUTlpo = '2' or DUTlpo = *blanks
121400070726     C* deve essere in £6 se secondo livello
121500070726     C     com03         LOOKUP    L6                                     45
121600070726     C                   when      DUTlpo = '1'
121700070726     C* deve essere in £1 se primo livello
121800070726     C     com03         LOOKUP    L1                                     45
121900070726     C                   when      DUTlpo = 'S'
122000070726     C* deve essere in azorg01l
122100070726     C     com03         chain     azorg01l
122200061229     c                   if        %found(azorg01l)
122300061229     c                   if        orgfag <> 'A' and orgfag <> 'F'
122400061229     c                   setoff                                       45
122500061229     c                   else
122600061229     c                   seton                                        45
122700061229     c                   end
122800061229     c                   else
122900061229     c                   setoff                                       45
123000061229     c                   end
123100070726     C                   ENDsl
123200070726     C* Filiale non presente in tabella
123300070726     C     *IN45         IFEQ      *OFF
123400070726     C                   MOVEL     MSG(12)       $MSG
123500070726     C                   SETON                                        5890
123600070726     C                   leave
123700070726     C                   END
123800070726     C                   END
123900061229     C* CODICE SEMPRE > 0
124000070726     C     w01pdr        IFEQ      *BLANKS
124100070726     C     w01pdr        OREQ      *ZERO
124200061229     C                   MOVEL     MSG(1)        $MSG
124300061229     C                   SETON                                        90
124400061229     C                   leave
124500061229     C                   END
124600061229     C* CONTROLLO VALIDITA' CODICE
124700061229     C                   TESTN                   w01pdr               50
124800070726     C                   MOVE      w01pdr        W001A             1
124900070726     C     *IN50         IFEQ      *OFF
125000070726     C     W001A         ORLT      '0'
125100061229     C                   MOVEL     MSG(2)        $MSG
125200061229     C                   SETON                                        58  90
125300061229     C                   leave
125400070726     C                   END
125500061229     c* controllo che esista un estensione in vigore alla data richiesta
125600061229     c                   move      w01dta        dtaiso
125700061229     c                   move      dtaiso        comiso            8 0
125800061229     c                   move      w01pdr        w01pdrc
125900061229     c     kapdw21       setgt     fiapd22l
126000070202     c     kapdw2        readpe(n) fiapd22l
126100070726     c                   if        (not %eof(fiapd22l) and (apd2atb<>' ' or
126200070726     c                             comiso<apd2dde or comiso>apd2dscad)) or
126300061229     c                             %eof(fiapd22l)
126400061229     C                   MOVEL     MSG(21)       $MSG
126500061229     C                   SETON                                        90
126600061229     C                   leave
126700061229     c                   end
126800061229     c*
126900061229     c                   enddo
127000061229     c*
127100061229     c                   endsr
127200061229     C**************************************************************************
127300070719     C     SRcopia       BEGSR
127400061229     C**************************************************************************
127401070627     C* PULIZIA SUBFILE
127402070627     C                   SETON                                        21
127403070627     C                   WRITE     dg02C01
127404070627     C                   SETOFF                                       2122
127405070627     C                   Z-ADD     0             NRR               4 0
127406070627     c* se scelto di accodare carico prima il vecchio e poi il nuovo
127407070627     C* CARICAMENTO SUBFILE
127408070627     c                   if        w01acc = 'S'
127409070627     c                   eval      flagcopia='1'
127410070627     c                   if        d02file = 'FIDST00F'
127411070718     c* consegne
127412071008     c                   if        vctp = 'C'
127413070718     c                   exsr      cardstc
127414070718     c                   else
127415070718     c* ritiri
127416070718     c                   exsr      cardstr
127417070718     c                   end
127418070627     c                   else
127419070718     c* consegne
127420071008     c                   if        vctp = 'C'
127421070718     c                   exsr      carapdc
127422070718     c                   else
127423070718     c* ritiri
127424070718     c                   exsr      carapdr
127425070627     c                   end
127426070718     c                   end
127427070627     c                   end
127500070726     c                   move      w01dta        dtaiso
127600061229     c                   move      dtaiso        comiso
127700061229     c                   move      w01pdr        w01pdrc
127800070119     c                   eval      comnras= savnras
127900061229     c     kapdw21       setgt     fiapd22l
128000070202     c     kapdw2        readpe(n) fiapd22l                               88
128100061229     c                   if        not *in88 and comiso >= apd2dde and
128200070719     c                             comiso <= apd2dscad and apd2atb = ' '
128300070119     c* se scelto di copiare tutto solo x anagrafico, per distinta obbliga
128400070119     c* solo giri
128500070123     c                   if        w01cgi = 'N'
128600061229     c                   movea     tc1           tc
128700061229     c                   movea     ts1           ts
128800061229     c                   exsr      srdec
128900061229     c                   eval      vftc = apd2ftc
129000061229     c                   eval      vfts = apd2fts
129100061229     c                   eval      vinc = apd2inc
129202070718     c                   eval      vtol = apd2tol
129501070718     c                   eval      vrtol = apd2rtol
129502070718     c                   eval      vrinc = apd2rinc
129503070718     c                   eval      vtpa  = apd2rtpa
129504070718     c                   eval      vspi  = apd2rspi
129505070718     c* consegne
129506071008     c                   if        w01tp = 'C'
129507070718     c                   eval      vnftl= apd2nftl
129508070718     c                   eval      vpkl = apd2pkl
129509070718     c                   eval      vvlu = apd2vlu
129510070718     c                   else
129511070718     c* ritiri
129512070718     c                   eval      vnftl= apd2rnftl
129513070718     c                   eval      vpkl = apd2rpkl
129514070718     c                   eval      vvlu = apd2rvlu
129515070718     c                   end
129700070119     c                   end
129800070119     c                   eval      savnras= apd2nras
130400061229     C* CARICAMENTO SUBFILE
130500070202     c                   eval      flagcopia='1'
130501070718     c* consegne
130502071008     c                   if        w01tp = 'C'
130600070718     c                   exsr      carapdc
130601070718     c                   else
130602070718     c* ritiri
130603070718     c                   exsr      carapdr
130604070718     c                   end
130605070726     c                   end
130700070125     C* carico 100 righe vuote
130800070125     c                   do        100
130801070627     c                   setoff                                       080910
130802070627     c                   setoff                                       1314
130900070129     c                   movel     vfgs          vscfgsg
131000070129     c                   clear                   vsccgi
131100070125     c                   clear                   vsdcgi
131200070119     c                   clear                   vscprgg
131300061229     c                   clear                   vscsql
131301090113     c                   if        nrr < 9999
131400061229     C                   ADD       1             NRR
131500061229     C                   WRITE     dg02s01
131501090113     c                   else
131502090113     c                   leave
131503090113     c                   end
131600061229     c                   enddo
131700061229     c                   seton                                        22
131800061229     C                   Z-ADD     1             REC
132000070119     c                   eval      savnras= comnras
132100061229     C                   ENDSR
132200061229     C**************************************************************************
132300061229     C*    IMMISSIONE/AGGIORNAMENTO/RIATTIVAZIONE
132400061229     C**************************************************************************
132500061229     C     SCRIVI        BEGSR
132600061229     C*
132700061218     c                   move      vpdr          kpdr
132800061222     c                   move      hdde          apd2dde
132900070119     c                   move      hscad         apd2dscad
133000061219     c                   setoff                                       3298
133100061219     c* manutenzione, annullamento, riattivazione
133200070718     c*                  if        not *in42
133300061218     C     KAPD3         CHAIN     fiapd22L                           3298
133400070718     c*                  else
133401070718     c                   if        not %found(fiapd22l)
133500061219     c* inserimento
133600061222     c                   seton                                        32
133700070831     c*                  clear                   apd2nras
133701070831     c                   clear                   fiapd2ds
133800061222     c* prende il numero della distinta del po in gestione
133900061222     C                   CLEAR                   TRUL33DS
134000061222     c                   eval      i33tla = 'L'
134100061222     C                   Z-ADD     200           I33CNU
134200061222     C                   Z-ADD     1             I33NUM
134300061222     c                   movel(p)  trul33ds      kpjbu
134400061222     c                   call      'TRUL33R'
134500061222     c                   parm                    kpjba
134600061222     c                   movel     kpjbu         trul33ds
134700061222     C                   IF        O33ERR = 0
134800070119     C                   Z-ADD     O33Nrf        apd2nras
134900061222     c                   else
135000061222     c                   seton                                        28
135100061222     c                   eval      vmsg = o33msg
135200061222     C                   END
135300061219     c                   end
135400061219     c*
135500061222     C                   IF        not *in98 and not *in28
135600060926     C                   MOVE      ' '           APD2ATB
135700070119     C                   MOVEL     'A'           APD2tipa
135800061218     C                   MOVEL     VPDR          APD2PDR
135900060926     c* consegne particolari
136000060926     c                   movea     tc            tc1
136100060926     c* tipo servizio
136200060926     c                   movea     ts            ts1
136300060926     c*
136500060926     c                   eval      apd2tol = vtol
136600060926     c                   eval      apd2inc = vinc
136800061214     c                   eval      apd2ftc = vftc
136900061214     c                   eval      apd2fts = vfts
137000061214     c                   eval      apd2fpp = vfpp
137100061214     c                   movel     vdde          dtaiso
137200061214     c                   movel     dtaiso        apd2dde
137300070119     c                   movel     vscad         dtaiso
137400070119     c                   movel     dtaiso        apd2dscad
137401070718     c* consegne
137402070718     c                   if        vctp = 'C'
137600061214     c                   eval      apd2pkl= vpkl
137700061214     c                   eval      apd2vlu = vvlu
137701070718     c                   eval      apd2nftl= vnftl
137702070718     c                   else
137703070718     c* ritiri
137704070718     c                   eval      apd2rpkl= vpkl
137705070718     c                   eval      apd2rvlu = vvlu
137706070718     c                   eval      apd2rnftl= vnftl
137707070718     c                   end
137708070718     c                   eval      apd2rtol = vrtol
137709070718     c                   eval      apd2rinc = vrinc
137710070718     c                   eval      apd2rtpa = vtpa
137711070718     c                   eval      apd2rspi = vspi
137800021129     C*
137900060926     C   32              WRITE     fiapd200                             98
138000060926     C  N32              UPDATE    fiapd200                             98
138100061219     c                   if        not *in98
138101070718     c* consegne
138102070718     c                   if        vctp = 'C'
138200070718     c                   exsr      wtrgiric
138201070718     c                   else
138202070718     c* ritiri
138203070718     c                   exsr      wtrgirir
138300061219     C                   End
138301070718     C                   End
138400061219     C                   End
138500061219     C* record allocato
138600061219     c                   if        *in98
138700061218     C                   MOVEL     MSG(4)        VMSG
138800060927     C                   SETON                                        28
138900940809     C                   ENDIF
139000920921     C*
139100920921     C                   ENDSR
139200060929     C**************************************************************************
139300060929     C*    IMMISSIONE/AGGIORNAMENTO/RIATTIVAZIONE
139400060929     C**************************************************************************
139500060929     C     SCRIVI1       BEGSR
139600060929     c                   clear                   fidst000
139700060929     C*
139800070410     C     Kdst          CHAIN     FIDST09L                           3298
139900060929     C     *IN98         IFEQ      *OFF
140000060929     C                   MOVE      ' '           dstATB
140100061218     C                   MOVEL     VPDR          dstPDR
140200070119     c                   eval      dsttipa= d02tipa
140300060929     c* consegne particolari
140400060929     c                   movea     tc            tc1d
140500060929     c* tipo servizio
140600060929     c                   movea     ts            ts1d
140700060929     c*
140900060929     c                   eval      dsttol = vtol
141000060929     c                   eval      dstinc = vinc
141200061219     c                   eval      dstftc = vftc
141300061219     c                   eval      dstfts = vfts
141400060929     c* se non esiste la distinta prendo gli altri dati dal file fiapd00f
141500060929     c                   if        *in32
141600061228     c                   clear                   dstfcf
141700070119     c                   clear                   dstfasd
141800061228     c                   clear                   dstnot
141901080306     c                   eval      dstnpg = d02npg
142000061222     c                   eval      dstfgs = d02fil
142100061213     c                   eval      dstnfv = d02nfv
142200061213     c                   eval      dstdfv = d02dfv
142300061213     c                   eval      dstfpp = d02fpp
142400061213     c                   eval      dsttpm = d02tpm
142500060929     c* piking mattina pomeriggio
142600060929     c                   if        dstfpp = 'M'
142700060929     c                   eval      dstpep = apdpkm
142800060929     c                   else
142900060929     c                   eval      dstpep = apdpkp
143000060929     c                   end
143100060929     c* carico mattina pomeriggio
143200060929     c                   if        dstfpp = 'M'
143300060929     c                   eval      dstfca = apdcam
143400060929     c                   else
143500060929     c                   eval      dstpep = apdcap
143600060929     c                   end
143700060929     c                   end
143701070718     c                   eval      dstrtol = vrtol
143702070718     c                   eval      dstrinc = vrinc
143703070718     c                   eval      dstrtpa = vtpa
143704070718     c                   eval      dstrspi = vspi
143705070718     c* consegne
143706070718     c                   if        vctp = 'C'
143707070718     c                   eval      dstnftl= vnftl
143900061214     c                   eval      dstpkl = vpkl
144000061214     c                   eval      dstvlu = vvlu
144001070718     c                   else
144002070718     c* ritiri
144003070718     c                   eval      dstrnftl= vnftl
144004070718     c                   eval      dstrpkl = vpkl
144005070718     c                   eval      dstrvlu = vvlu
144006070718     c                   end
144100060929     C*
144200060929     C   32              WRITE     fidst000                             98
144300060929     C  N32              UPDATE    fidst000                             98
144400061219     c                   if        not *in98
144401070718     c* consegne
144402070718     c                   if        vctp = 'C'
144500070718     c                   exsr      wtrgiridc
144501070718     c                   else
144502070718     c* ritiri
144503070718     c                   exsr      wtrgiridr
144600061219     C                   End
144601070718     C                   End
144700060929     C                   ENDIF
144800060929     C*
144900060929     C     *IN98         IFEQ      *ON
145000061218     C                   MOVEL     MSG(4)        VMSG
145100060929     C                   SETON                                        28
145200060929     C                   ENDIF
145300060929     C*
145400060929     C                   ENDSR
145500061219     C**************************************************************************
145600070718     C     wtrgiric      BEGSR
145700061219     C**************************************************************************
145800061219     C*
145900061219     c                   if        not *in32
146000061219     C* manutenzione: cancello i giri e li rinserisco
146100070119     c     apd2nras      setll     fiapd31l
146200061219     c                   do        *hival
146300070119     c     apd2nras      reade     fiapd31l
146400061219     c                   if        %eof(fiapd31l)
146500061219     c                   leave
146600061219     c                   end
146700061219     c                   delete    fiapd300
146800061219     c                   enddo
146900061219     c                   end
147000061219     C*
147100061219     c                   clear                   apd3atb
147200070119     c                   eval      apd3nras= apd2nras
147300070119     c                   eval      apd3tipa= apd2tipa
147400061219     c                   eval      apd3pdr = apd2pdr
147500061219     c                   eval      apd3dde = apd2dde
147600070119     c                   eval      apd3dscad= apd2dscad
147700061219     c                   eval      apd3fpp = apd2fpp
147800061219     c*
147900070726     C                   do        *hival        nn                5 0
148000061220     C     nn            chain     dg02S01                            31
148100070726     C   31              leave
148200070123     c                   if        vsccgi<>' '
148300070129     c                   movel     vscfgsg       apd3fgsg
148400070129     c                   movel     vsccgi        apd3cgi
148500070119     c                   eval      apd3prgg= vscprgg
148600061219     c                   eval      apd3sql = vscsql
148700061219     c                   write     fiapd300
148800061219     c                   end
148900070726     C                   ENDDO
148901070723     C* se sono in manutenzione ed
149000070723     C* ho variato le date devo variare anche i giri dei ritiri
149001070723     c                   if        not *in32
149003070723     c     apd2nras      setll     fiapd51l
149004070723     c                   do        *hival
149005070723     c     apd2nras      reade     fiapd51l
149006070723     c                   if        %eof(fiapd51l)
149007070723     c                   leave
149008070723     c                   end
149009070723     c                   eval      apd5dde = apd2dde
149010070723     c                   eval      apd5dscad= apd2dscad
149011070723     c                   update    fiapd500
149012070723     c                   enddo
149013070723     c                   end
149100061219     C                   ENDSR
149101070718     C**************************************************************************
149102070718     C     wtrgirir      BEGSR
149103070718     C**************************************************************************
149104070718     C*
149105070718     c                   if        not *in32
149106070718     C* manutenzione: cancello i giri e li rinserisco
149107070718     c     apd2nras      setll     fiapd51l
149108070718     c                   do        *hival
149109070718     c     apd2nras      reade     fiapd51l
149110070718     c                   if        %eof(fiapd51l)
149111070718     c                   leave
149112070718     c                   end
149113070718     c                   delete    fiapd500
149114070718     c                   enddo
149115070718     c                   end
149116070718     C*
149117070718     c                   clear                   apd5atb
149118070718     c                   eval      apd5nras= apd2nras
149119070718     c                   eval      apd5tipa= apd2tipa
149120070718     c                   eval      apd5pdr = apd2pdr
149121070718     c                   eval      apd5dde = apd2dde
149122070718     c                   eval      apd5dscad= apd2dscad
149123070718     c                   eval      apd5fpp = apd2fpp
149124070718     c*
149125070726     C                   do        *hival        nn                5 0
149126070718     C     nn            chain     dg02S01                            31
149127070726     C   31              leave
149128070718     c                   if        vsccgi<>' '
149129070718     c                   movel     vscfgsg       apd5fgsg
149130070718     c                   movel     vsccgi        apd5cgi
149131070718     c                   eval      apd5prgg= vscprgg
149132070718     c                   eval      apd5sql = vscsql
149133070718     c                   write     fiapd500
149134070718     c                   end
149135070726     C                   ENDDO
149137070723     C* se sono in manutenzione ed
149138070723     C* ho variato le date devo variare anche i giri delle consegne
149139070723     c                   if        not *in32
149140070723     c     apd2nras      setll     fiapd31l
149141070723     c                   do        *hival
149142070723     c     apd2nras      reade     fiapd31l
149143070723     c                   if        %eof(fiapd31l)
149144070723     c                   leave
149145070723     c                   end
149146070723     c                   eval      apd3dde = apd2dde
149147070723     c                   eval      apd3dscad= apd2dscad
149148070723     c                   update    fiapd300
149149070723     c                   enddo
149150070723     c                   end
149151070718     C                   ENDSR
149200061219     C**************************************************************************
149300070718     C     wtrgiridc     BEGSR
149400061219     C**************************************************************************
149500061219     C*
149600061219     c                   if        not *in32
149700061219     C* manutenzione: cancello i giri e li rinserisco
149800061228     c     kdst1         setll     fidst31l
149900061219     c                   do        *hival
150000061228     c     kdst1         reade     fidst31l
150100061219     c                   if        %eof(fidst31l)
150200061219     c                   leave
150300061219     c                   end
150400061219     c                   delete    fidst300
150500061219     c                   enddo
150600061219     c                   end
150700061219     C*
150800061219     c                   clear                   dst3atb
150900061219     c                   eval      dst3fgs = dstfgs
151000061219     c                   eval      dst3npg = dstnpg
151100061219     c                   eval      dst3dfv = dstdfv
151200061219     c                   eval      dst3nfv = dstnfv
151300061219     c*
151400070726     C                   do        *hival        nn                5 0
151500061220     C     nn            chain     dg02S01                            31
151600070726     C   31              leave
151700070123     c                   if        vsccgi<>' '
151800070129     c                   movel     vscfgsg       dst3fgsg
151900070129     c                   movel     vsccgi        dst3cgi
152000070119     c                   eval      dst3prgg= vscprgg
152100061219     c                   eval      dst3sql = vscsql
152200061219     c                   write     fidst300
152300061219     c                   end
152400070726     C                   ENDDO
152500061219     C*
152600061219     C                   ENDSR
152601070718     C**************************************************************************
152602070718     C     wtrgiridr     BEGSR
152603070718     C**************************************************************************
152604070718     C*
152605070718     c                   if        not *in32
152606070718     C* manutenzione: cancello i giri e li rinserisco
152607070718     c     kdst1         setll     fidst51l
152608070718     c                   do        *hival
152609070718     c     kdst1         reade     fidst51l
152610070718     c                   if        %eof(fidst51l)
152611070718     c                   leave
152612070718     c                   end
152613070718     c                   delete    fidst500
152614070718     c                   enddo
152615070718     c                   end
152616070718     C*
152617070718     c                   clear                   dst5atb
152618070718     c                   eval      dst5fgs = dstfgs
152619070718     c                   eval      dst5npg = dstnpg
152620070718     c                   eval      dst5dfv = dstdfv
152621070718     c                   eval      dst5nfv = dstnfv
152622070718     c*
152623070726     C                   do        *hival        nn                5 0
152624070718     C     nn            chain     dg02S01                            31
152625070726     C   31              leave
152626070718     c                   if        vsccgi<>' '
152627070718     c                   movel     vscfgsg       dst5fgsg
152628070718     c                   movel     vsccgi        dst5cgi
152629070718     c                   eval      dst5prgg= vscprgg
152630070718     c                   eval      dst5sql = vscsql
152631070718     c                   write     fidst500
152632070718     c                   end
152633070726     C                   ENDDO
152634070718     C*
152635070718     C                   ENDSR
152700061219     C**************************************************************************
152800061219     C     CX§TAB        BEGSR
152900061219     C**************************************************************************
153000061219     C*
153100061219     c                   MOVE      *BLANKS       KDEST            30
153200061219     C                   CALL      'X§TABER'
153300061219     C                   PARM                    KUTET
153400061219     C                   PARM                    KCODT
153500061219     C                   PARM                    KKEYT
153600061219     C                   PARM                    KDEST
153700061219     C*
153800061219     C                   ENDSR
153900060926      *---------------------------------------------------------------*
154000060926      * Reperimento Dati del job (Utente/Operativi)                   *
154100060926      *---------------------------------------------------------------*
154200060926     c     DatiJob       BEGSR
154300060926      *
154400060926     c     *dtaara       define    §azute        azuteds
154500060926     c     *dtaara       define    §datiute      ddatiute
154600060926      *
154700060926     c                   in(E)     *dtaara
154800060926     c                   IF        %ERROR or RSUT = *blanks
154900060926     c                   clear                   Tibs34Ds
155000060926     c                   call      'TIBS34R'
155100060926     c                   parm                    Tibs34Ds
155200060926     c                   in        *dtaara
155300060926     c                   ENDIF
155400060926      *
155500060926     c                   ENDSR
155600940809     C**************************************************************************
155700940809     C*     R O U T I N E     I N I Z I A L E
155800940809     C**************************************************************************
155900940809     C     *INZSR        BEGSR
156000060926     c*
156100060926     C     *ENTRY        PLIST
156200060926     C                   PARM                    KPJBA
156300070510      *
156400070510     C                   TIME                    W0140            14 0
156500070510     C                   MOVE      W0140         UDATE8            8 0
156600060926      * Reperisco dati job
156700060926     c                   exsr      DatiJob
157100070510     c                   move      udate8        dateu8            8 0
157200070510     c                   move      udate8        dtaeur
157300070510     c                   move      dtaeur        dtaiso
157400070510     c                   move      dtaiso        udate_8           8 0
157500060926      *
157600061213     C                   MOVEL     RSUT          VDSA
157700070112     c                   setoff                                       55
157800070312     c* richiamato
157900060927     c                   if        kpjbu <> ' '
158000061213     C                   MOVEL     KPJBU         fidg02ds
158100070119     c                   clear                   d02cmde
158200070119     c                   clear                   d02err
158300070320     c* d02opncmt = S se devo aprire i file sotto commit oppure no
158400070320     c                   if        d02opncmt = 'N'
158500070320     c                   eval      flgcmt = *off
158600070320     c                   else
158700070320     c                   eval      flgcmt = *on
158800070320     c                   end
158900070320     c* visualizzazione
159000070319     c     d02cmd        comp      '05'                                   47
159100070112     c* DISTINTE
159200061213     c                   if        d02file = 'FIDST00F'
159300060929     c                   seton                                        43
159400070718     c                   eval      vctp=d02tp
159401070718     c                   eval      vfgs=d02fil
159500070115     c                   movel     d02pdr        vpdr
159600070115     c                   eval      vnfv=d02nfv
159700061213     c                   eval      vfpp=d02fpp
159800061222     c                   movel     d02dfv        vdfv
159900061222     c                   movel     d02dfv        vdta
160000070410     c                   open      FIDST09l
160200070112     c                   end
160300070112     c*  ANAGRAFICO
160400070112     c                   if        d02file = 'FIAPD20F'
160500070112     c                   seton                                        55
160600070112     c                   movel     d02pdr        vfgs
160700070112     c                   move      d02pdr        vcpdr
160800070112     C                   MOVEL     'M'           Vfpp                           *MATTINO
160801070718     C                   MOVEL     'C'           Vctp                           *MATTINO
160900070112     c                   movel     d02dfv        vdta
161000061011     c                   end
161100060927     c                   else
161200070312     c* da menù
161300070320     c                   eval      flgcmt = *on
161400061213     C                   clear                   fidg02ds
161500070726     C                   MOVEL(p)  '?'           VCPdr
161600070726     C                   MOVE      dutpou        Vfgs
161700070112     C                   MOVEL     'M'           Vfpp                           *MATTINO
161701070718     C                   MOVEL     'C'           Vctp                           *MATTINO
161900070510     c                   movel     udate_8       vdta
162000060927     c                   end
162100070320     c                   open      fiapd22l
162200070320     c                   open      fiapd31l
162201070627     c                   open      fidst31l
162202070718     c                   open      fiapd51l
162203070718     c                   open      fidst51l
162300070312     c* organigramma
162400070312     c     vfgs          chain     azorg01l
162500070312     c                   if        %found(azorg01l)
162600070312     c                   movel     orgdes        vdfgs
162700070718     C                   MOVEL     ORGde8        og148
162701070718     C                   MOVEL     ORGde7        og147
162800070312     c                   end
162801070723     c* abilito l'utilizzo del pgm solo se flag geo consegne o ritiro <> ' '
162803070723     c                   if        (§OGTLLS =' ' and §OGCGIO = ' ') or
162804070723     c                             (§OGTLLS =' ' and vctp = 'C') or
162805070723     c                             (§OGcgio =' ' and vctp = 'R')
162807070718     C                   EVAL      $command = cmd(1)
162808070718     C                   MOVEA     cmd(1)        DSComando
162809070718     C                   MOVEL     Job           DJob
162810070718     C                   EVAL      $command = DsComando
162811070718     C                   CALL      'QCMDEXC'
162812070718     C                   PARM                    $command
162813070718     C                   PARM                    $lenght
162814070718     c                   seton                                        lr
162815070718     c                   return
162816070718     c                   end
162818070928     c     §OGCGIO       comp      ' '                                    51
164200060928     C* CARICO LA TABELLA '£1'
164300060928     C                   MOVE      '£1'          D06COD
164400060928     C                   MOVEL     SIMFEL        D06KEY
164500060928     C                   MOVEL     DSUL06        KPJBU
164600060928     C                   CALL      'TRUL06R'
164700060928     C                   PARM                    KPJBA
164800060928     C                   MOVEL     KPJBU         DSUL06
164900060928     C                   MOVEA     LIN           L1
165000070726     C* CARICO LA TABELLA '£6' SE FILIALE DI SECONDO LIVELLO
165100060928     c                   if            DUTlpo = '2'
165200060928     c                             or  DUTlpo = *blanks
165300070726     C                   MOVE      '£6'          D06COD
165400070726     c                   movel     DUTpou        D06KEY
165500070726     C                   MOVEL     DSUL06        KPJBU
165600070726     C                   CALL      'TRUL06R'
165700070726     C                   PARM                    KPJBA
165800070726     C                   MOVEL     KPJBU         DSUL06
165900070726     C                   MOVEA     LIN           L6
166000070726     C                   END
166100940809     C*
166200940809     C     *LIKE         DEFINE    APDPDR        KPDR
166300940909     C*
166400021129     C     Kapd          KLIST
166500060927     C                   KFLD                    tipo              1
166600060927     C                   KFLD                    kpdr              7 0
166700060927     c                   eval      tipo = 'A'
166800061218     C     Kapd21        KLIST
166900061218     C                   KFLD                    tipo              1
167000061218     C                   KFLD                    kpdr              7 0
167100061218     C                   KFLD                    vfpp
167200061218     C                   KFLD                    comiso
167300061229     C     Kapdw21       KLIST
167400061229     C                   KFLD                    tipo              1
167500061229     C                   KFLD                    w01pdrc           7 0
167600061229     C                   KFLD                    w01fpp
167700061229     C                   KFLD                    comiso
167800061218     C     Kapd3         KLIST
167900061218     C                   KFLD                    tipo              1
168000061218     C                   KFLD                    kpdr              7 0
168100061218     C                   KFLD                    vfpp
168200061218     C                   KFLD                    apd2dde
168300070119     C                   KFLD                    apd2dscad
168400061218     C     Kapd2         KLIST
168500061218     C                   KFLD                    tipo              1
168600061218     C                   KFLD                    kpdr              7 0
168700061218     C                   KFLD                    vfpp
168800061229     C     Kapdw2        KLIST
168900061229     C                   KFLD                    tipo              1
169000061229     C                   KFLD                    w01pdrc
169100061229     C                   KFLD                    w01fpp
169200060929     C*
169300061228     C     Kdst          KLIST
169400070410     c                   KFLD                    d02fil
169500070319     C                   KFLD                    d02npg
169600061228     C                   KFLD                    d02nfv
169700061229     C*
169800061229     C     Kdst4         KLIST
169900061229     C                   KFLD                    tipo
170000061229     c                   KFLD                    kpdr
170100061229     c                   KFLD                    dstfcf
170200061228     C*
170300061228     C     Kdst1         KLIST
170400061228     C                   KFLD                    d02fil
170500061228     C                   KFLD                    d02nfv
170501070627     C     Kdst31        KLIST
170502070627     C                   KFLD                    dstfgs
170503070627     C                   KFLD                    dstnfv
170600010207     C*
170700010207     C     KTABEL        KLIST
170800010207     C                   KFLD                    KUTET
170900061218     C                   KFLD                    KCODT             2
171000061218     C                   KFLD                    KKEYT             8
171100010207     C*
171200010207     C                   Z-ADD     1             KUTET             1 0
171300000808     C*
171400940809     C                   ENDSR
171500940809     C**************************************************************************
171600940809**
171700021129Campo obbligatorio: immettere un codice o '?' per avere l'elenco               1
171800021129Immettere un codice valido                                                     2
171900061220Inserire tutti i campi obbligatori                                             3
172000000726Errori verificatisi nell'aggiornamento: RIPROVARE O AVVISARE IL CED (SEDE)     4
172100060927Consegna particolare errata                                                    5
172200060927Tipo servizio errato                                                           6
172300060927Priorità obbligatoria                                                          7
172400061220Priorità + sequenza già inserite                                               8
172500061220E' neccessario inserire almeno un giro                                         9
172600060927Giro già inserito                                                             10
172700061220Data decorrenza < di data scadenza                                            11
172800940909Codice filiale errata o non gestita                                           12
172900060927Consegna particolare già inserita                                             13
173000021129Immettere filiale valida o '?' nel codice l'elenco                            14
173100061222Esiste già un'anagrafica nel periodo scelto.                                  15
173200061220Sequenza obbligatoria                                                         16
173300070419Impossibile annullare l'anagrafica. Esistono distinte aperte.                 17
173400060927Tipo servizio già inserito                                                    18
173500060928Consegna particolare errata o non utilizzabile                                19
173600060928% sfrido obbligatoria                                                         20
173700061220Non esistono anagrafiche in vigore con questi riferimenti F10 x inserirla     21
173800070320Anagrafico autotrasportatore annullato. Occorre prima riattivarlo             22
173900070119Max fermate TELLUS obbligatorio                                               23
174000070119L'autotrasportatore non gestisce i giri                                       24
174100070320Codice giro non inseribile                                                    25
174101070718Tipo pianificazione non abilitato                                             26
174103070718ATTENZIONE: si sta variando la validità della pianificazione consegne E ritiri27
174104071126La % di tolleranza fa eccedere la capienza del campo pesi/volumi/fermate      28
174200070502**   CMD - Comandi
174300070926SNDBRKMSG MSG('Funzione non utilizzabile                            ') TOMSGQ(XXXXXXXXXX)
