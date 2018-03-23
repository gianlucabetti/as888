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
001300070717     Ffiapd51L  UF a E           K DISK    usropn commit(flgcmt)
001400070410     FFIDST09L  UF A E           K DISK    usropn commit(flgcmt)
001500070320     Ffidst31L  UF A E           K DISK    usropn commit(flgcmt)
001600070717     Ffidst51L  UF A E           K DISK    usropn commit(flgcmt)
001700061229     Ffidst04L  if   E           K DISK    rename(fidst000:fidst004)
001800940809     D*
001900061002     ddtaiso           s               d
002000061002     ddtaeur           s               d   datfmt(*eur)
002100061219     ddtadde           s               d
002200070718     ddtascad          s               d
002300061220     dcomdde           s               d
002400070119     dcomscad          s               d
002500070718     dcomhdde          s               d
002600070718     dcomhscad         s               d
002700070719     dsavdde           s               d
002800070719     dsavscad          s               d
002900060927     D xx              S              2  0 inz
003000060927     D yy              S              2  0 inz
003100060927     D zz              S              2  0 inz
003200071126     D MSG             S             78    DIM(28) CTDATA PERRCD(1)
003300070726     D L1              S              3  0 DIM(30) ASCEND
003400070726     D L6              S              3  0 DIM(30) ASCEND
003500070502     D cmd             S             90    DIM(1) CTDATA PERRCD(1)              *comandi CLP
003600070502     d                 DS
003700070502     d  DJob                  79     88                                          -Jobq
003800070502     d DSComando               1     90                                         *comando
003900070502      * Informazioni lavoro
004000070502     d                SDS
004100070502     d  JOB                  244    253                                         *lavoro
004200070502     d $lenght         S             15  5 INZ(90)                              *comando -lunghezza-
004300070502     d $command        S             90                                         *comando -contenuto-
004400940809     D*
004500060927     d* Parametri x Controllo profilo utenti
004600070119     d TIBS34ds      e ds
004700060203     d AZUTEds       e ds                  extname(AZUTE00F)
004800061222     c* numeratori
004900061222     d trul33ds      e ds
005000060927     d* Ds per dati organigramma
005100060203     d DDatiUte      e ds
005200060927     d*
005300060927     D KPJBA         E DS
005400060927     D fiapd2ds      E DS                  extname(fiapd20f)
005500060929     D fidstds       E DS                  extname(fidst00f)
005600060926     D fnlv24ds      E DS
005700061213     D fidg02ds      E DS
005800070112     D fidg07ds      E DS
005900070124     D fidg09ds      E DS
006000070119     d dapdflr       e ds
006100070320     d dgrgflo       e ds
006200060927     D Ds1p          E DS
006300060927     D Ds5e          E DS
006400060927     d ds03          e ds
006500070718     D OG147         E DS                  INZ
006600070718     D OG148         E DS                  INZ
006700170612
006800170612      // - Reperimento filiali gestite dall'utente
006900170612     d TRUL31DS      e ds
007000170612     d  POG                   10    759    dim(250)
007100170615
007200170615     d sav_kpjbu       s                   like(kpjbu)
007300170612
007400061229     d comdta          s               d
007500060926     D Ptc1            S               *
007600060926     D                                     INZ(%ADDR(apd2tc1))
007700060926     D tc1             S                   LIKE(apd2tc1)
007800060926     D                                     DIM(5)
007900060926     D                                     BASED(Ptc1)
008000060926     D Pts1            S               *
008100060926     D                                     INZ(%ADDR(apd2ts1))
008200060926     D ts1             S                   LIKE(apd2ts1)
008300060926     D                                     DIM(2)
008400060926     D                                     BASED(Pts1)
008500060929     d* schiere file fidst00f
008600060929     D Ptc1d           S               *
008700060929     D                                     INZ(%ADDR(dsttc1))
008800060929     D tc1d            S                   LIKE(dsttc1)
008900060929     D                                     DIM(5)
009000060929     D                                     BASED(Ptc1d)
009100060929     D Pts1d           S               *
009200060929     D                                     INZ(%ADDR(dstts1))
009300060929     D ts1d            S                   LIKE(dstts1)
009400060929     D                                     DIM(2)
009500060929     D                                     BASED(Pts1d)
009600060926     D tc              S                   LIKE(apd2tc1)
009700060926     D                                     DIM(5)
009800060926     D ts              S                   LIKE(apd2ts1)
009900060926     D                                     DIM(2)
010000060927     D dtc             S             15
010100060926     D                                     DIM(5)
010200060927     D dts             S             15
010300060926     D                                     DIM(2)
010400061219     c* schiere video
010500061219     D prsq            S              4  0
010600090113     D                                     DIM(9999)
010700070129     D cgi             S             13
010800090113     D                                     DIM(9999)
010900940909     D* DS PER TRUL06R - CARICAMENTO £X
011000940909     D DSUL06        E DS                  EXTNAME(TRUL06DS)
011100060926     D  LIN                    1     90  0
011200950314     D                                     DIM(30)
011300060927     d*
011400070202     d flagcopia       s              1
011500070202     d savnras         s                   like(apd2nras)
011600070119     d comnras         s                   like(apd2nras)
011700071126     d wtol            s                   like(vtol)
011800071126     d wpeso           s             14  1
011900071126     d wvolume         s             10  3
012000071126     d wnftl           s             10  0
012100060926     D                 DS
012200061213     D  Vpdr                   1      7
012300061213     D  Vfgs                   1      3  0
012400061213     D  Vcpdr                  4      7
012500170612
012600170612      //---------------------------------------------------------------
012700170612      //?Definizione Procedure usate.
012800170612      //---------------------------------------------------------------
012900170612       // - Caricamento Filiali in gestione
013000170612     d TRUL31R         pr                  extpgm('TRUL31R')
013100170612     d  kpjba                              likeds(kpjba)
013200170612     d  trul31ds                           likeds(trul31ds)
013300170612
013400061213     i*
013500070125     iDG02d03
013600061213     i              vtc1                        tc(1)
013700061213     i              vtc2                        tc(2)
013800061213     i              vtc3                        tc(3)
013900061213     i              vtc4                        tc(4)
014000061213     i              vtc5                        tc(5)
014100061213     i              vtc1d                       dtc(1)
014200061213     i              vtc2d                       dtc(2)
014300061213     i              vtc3d                       dtc(3)
014400061213     i              vtc4d                       dtc(4)
014500061213     i              vtc5d                       dtc(5)
014600061213     i              vts1                        ts(1)
014700061213     i              vts2                        ts(2)
014800061213     i              vts1d                       dts(1)
014900061213     i              vts2d                       dts(2)
015000060926     C**********************************************************************
015100060926     c*
015200060926     c                   do        *hival
015300070115     c                   WRITE     dg02T01
015400070115     c* 42 on inserimento anagrafico
015500070115     c                   setoff                                       42
015600060926     C* richiamato da menù
015700070112     c                   if        d02file <> 'FIDST00F'
015800060926     c                   exsr      srd01
015900060926     c                   end
016000060926     c   kc              leave
016100070718     c* consegne o ritiri
016200070718     c     vctp          comp      'C'                                    48
016300060926     C*
016400060926     C                   EXSR      srd02
016500061220     c                   clear                   vmsg
016600061220     c                   setoff                                       28
016700070112     c* se sono stato chiamato da distinta esco
016800070124     c                   if        d02file = 'FIDST00F' or *inkc
016900060928     c                   leave
017000060928     c                   end
017100060927     c                   enddo
017200061010     c* imposto il comando eseguito
017300061010     c                   select
017400061010     c                   when      *inkc
017500061213     c                   eval      d02cmde = 'KC'
017600070320     c                   when      *inkf or *inkq or *inke
017700061213     c                   eval      d02cmde = 'KF'
017800061010     c                   when      *inkl
017900061213     c                   eval      d02cmde = 'KL'
018000061010     c                   endsl
018100060926     C*
018200061213     C                   MOVEL     fidg02ds      KPJBU
018300060926      *
018400060926     C                   SETON                                        LR
018500060926     C**********************************************************************
018600060926     c     srd01         begsr
018700060927     C**********************************************************************
018800060926     c*
018900061222     c                   setoff                                       2829
019000061222     c                   clear                   vmsg
019100070717     c                   eval      vctp='C'
019200060926     c                   do        *hival
019300060926     c*
019400070125     c                   WRITE     dg02T01
019500061213     C                   EXFMT     dg02D01
019600920918     C* CMD3 - FINE LAVORO
019700060926     C   KC              leave
019800070718     c                   if        *inkd
019900070718     C                   exsr      srric
020000070720     c                   if        oksce = ' '
020100070718     c                   iter
020200070719     c                   end
020300070720     c                   end
020400060927     c                   exsr      ctr01
020500061220     c* inserimento nuova anagrafica
020600061220     c                   if        not *in28 and *inkj
020700070115     c                   seton                                        42
020800061220     C                   leave
020900061220     c                   end
021000061220     c* errore
021100061220     c                   if        *in28 or (*in29 and not *inkj)
021200061220     c                   iter
021300061220     c                   end
021400060927     c                   leave
021500060927     c                   enddo
021600060927     c*
021700060927     c                   endsr
021800060927     C**********************************************************************
021900070124     c     srriccgi      begsr
022000060927     C**********************************************************************
022100070112     c*
022200070627     c                   setoff                                       081314
022300070124     c                   clear                   fidg09ds
022400070124     c                   eval      D09IOP0 = 'P01'
022500070129     c                   eval      d09ifgs = vscfgsg
022600070718     c                   eval      D09ITUG = vctp
022700070126     c                   if        d02file = 'FIDST00F'
022800070126     c                   move      vdfv          dtaiso
022900070126     c                   move      dtaiso        d09idat
023000070126     c                   else
023100070126     c                   move      vdde          dtaiso
023200070126     c                   move      dtaiso        d09idat
023300070126     c                   end
023400070130     c*                  eval      d09ides = vsdcgi
023500070124     c                   eval      kpjbu = fidg09ds
023600070124     c                   call      'FIDG09R'
023700070124     c                   parm                    kpjba
023800070124     c                   eval      fidg09ds = kpjbu
023900070124     c                   if        d09oerr = '1'
024000070129     c                   seton                                        280813
024100070124     c                   movel     Msg(2)        Vmsg
024200070124     c                   else
024300070320     c                   eval      dgrgflo = D09OFLO
024400070320     c                   if        §grgdst = 'N'
024500070320     c                   seton                                        280813
024600070320     c                   movel     Msg(25)       Vmsg
024700070320     c                   else
024800070130     c                   if        d09ocgi <> ' '
024900070129     c                   eval      vscfgsg= d09ofgs
025000070129     c                   eval      vsccgi = d09ocgi
025100070124     c                   eval      vsdcgi = d09odes
025200070627     c     udate_8       comp      D09ODSC                            14
025300070320     c                   end
025400070124     c                   end
025500070130     c                   end
025600070124     c*
025700070124     c                   endsr
025800070124     C**********************************************************************
025900070124     c     srric         begsr
026000070124     C**********************************************************************
026100070124     c*
026200070720     c                   clear                   oksce             1
026300070720     c                   clear                   fidg07ds
026400070119     c                   movel     'A'           d07tipa
026500070112     c                   movel     vfgs          d07fgs
026600070112     c                   if        vcpdr <> ' '
026700070726     C     '?'           SCAN      Vcpdr                                  31
026800070112     c                   if        not *in31
026900070112     c                   movel     vfgs          d07pdr
027000070112     c                   move      vcpdr         d07pdr
027100070112     c                   end
027200070112     c                   end
027300070112     c                   movel     fidg07ds      kpjbu
027400070116     c                   movel     vdta          dtaiso
027500070112     c                   call      'FIDG07R'
027600070112     c                   parm                    kpjba
027700070112     c                   movel     kpjbu         fidg07ds
027800070119     c                   if        d07pdro <> 0
027900070119     c                   movel     d07pdro       vfgs
028000070119     c                   move      d07pdro       vcpdr
028100070112     c                   end
028200070119     c                   if        d07fppo <> ' '
028300070119     c                   movel     d07fppo       vfpp
028400070112     c                   end
028500070119     c                   if        d07ddeo <> 0
028600070119     c                   move      d07ddeo       dtaiso
028700070112     c                   move      dtaiso        vdta
028800070112     c                   end
028900070112     c*
029000070112     c                   endsr
029100070112     C**********************************************************************
029200070112     c     ctr01         begsr
029300070112     C**********************************************************************
029400070726     C                   SETOFF                                       285729
029500070718     c                   setoff                                       49
029600061220     c                   clear                   vmsg
029700060927     c                   do
029800070726     C* CONTROLLO FILIALE AUTOtrasp.
029900070726     C     Vfgs          IFEQ      *ZEROS
030000070726     C     '?'           SCAN      Vcpdr                                  31
030100070726     C     *IN31         IFEQ      *OFF
030200070726     C                   MOVEL     MSG(14)       VMSG
030300070726     C                   SETON                                        2857
030400060927     C                   leave
030500060926     c                   end
030600060927     c                   end
030700060926     C** ricerca
030800070726     C     '?'           SCAN      Vcpdr                                  31
030900060926     C                   if        *in31
031000060926     C                   clear                   fnlv24ds
031100060926     C                   MOVEL     tipo          d24tip
031200061213     C                   MOVEL     vfgs          d24fil
031300060926     C                   MOVEL     'R'           d24FLG
031400060926     C                   MOVEL(p)  fnlv24ds      KPJBU
031500060926     C                   CALL      'FNLV24R'
031600060926     C                   PARM                    KPJBA
031700060926     C                   MOVEL     KPJBU         fnlv24ds
031800070906     c                   if        d24pdr <> 0
031900061213     c                   move      d24pdr        vpdr
032000070906     c                   else
032100070906     c                   clear                   vcpdr
032200070906     c                   end
032300061213     c                   movel     d24rsc        vdpdr
032400060926     c                   end
032500070726     C* Filiale diversa da 0:
032600070726     C     Vfgs          IFne      *ZEROS
032700170612     c*****              select
032800170612     c*****              when      DUTlpo = '2' or DUTlpo = *blanks
032900070726     C* deve essere in £6 se secondo livello
033000170612     C*****Vfgs          LOOKUP    L6                                     45
033100170612     C*****              when      DUTlpo = '1'
033200070726     C* deve essere in £1 se primo livello
033300170612     C*****Vfgs          LOOKUP    L1                                     45
033400170612     C*****              when      DUTlpo = 'S'
033500070726     C* deve essere in azorg01l
033600070726     C     Vfgs          chain     azorg01l
033700030129     c                   if        %found(azorg01l)
033800030203     c                   if        orgfag <> 'A' and orgfag <> 'F'
033900030203     c                   setoff                                       45
034000030203     c                   else
034100030129     c                   seton                                        45
034200030203     c                   end
034300030129     c                   else
034400030129     c                   setoff                                       45
034500030129     c                   end
034600170612     C*******            ENDsl
034700170612
034800170612       //?Filiale in gestione all'utente
034900170612         IF  %lookup(%editc(vfgs:'X'):pog) = 0;
035000170612           *in45 = *off;
035100170612         ENDIF;
035200170612
035300070726     C* Filiale non presente in tabella
035400070726     C     *IN45         IFEQ      *OFF
035500070726     C                   MOVEL     MSG(12)       VMSG
035600070726     C                   SETON                                        2857
035700070726     C                   leave
035800070726     C                   END
035900070726     C                   END
036000070928     C                   MOVEL     ORGde8        og148
036100070928     C                   MOVEL     ORGde7        og147
036200070928     c     §OGCGIO       comp      ' '                                    51
036300060926     C* CODICE SEMPRE > 0
036400070726     C     Vcpdr         IFEQ      *BLANKS
036500070726     C     Vcpdr         OREQ      *ZERO
036600061213     C                   MOVEL     MSG(1)        VMSG
036700060927     C                   SETON                                        28
036800060927     C                   leave
036900920918     C                   END
037000060926     C* CONTROLLO VALIDITA' CODICE
037100061213     C                   TESTN                   Vpdr                 50
037200070726     C                   MOVE      Vpdr          W001A             1
037300070726     C     *IN50         IFEQ      *OFF
037400070726     C     W001A         ORLT      '0'
037500061213     C                   MOVEL     MSG(2)        VMSG
037600060926     C                   SETON                                        57  28
037700060927     C                   leave
037800070726     C                   END
037900061213     c                   move      vpdr          kpdr
038000060926     c     kapd          chain     fiapd01l
038100060926     c                   if        %found(fiapd01l)
038200061213     c                   movel     apdrsc        vdpdr
038300070119     c                   movel     apdflr        dapdflr
038400061220     c                   else
038500061220     C                   MOVEL     MSG(2)        VMSG
038600061220     C                   SETON                                        57  28
038700061220     c                   leave
038800070119     c                   end
038900070723     c* se sono in inserimento o in riattivaz. controllo che l'anagrafico
039000070723     c* sia attiva
039100070723     c                   if        apdatb <> ' '
039200070723     C                   MOVEL     MSG(22)       VMSG
039300070723     C                   SETON                                        57  28
039400070723     c                   leave
039500070723     c                   end
039600061213     c* controllo che esista un estensione in vigore alla data richiesta
039700061213     c                   move      vdta          dtaiso
039800061218     c                   move      dtaiso        comiso            8 0
039900061218     c     kapd21        setgt     fiapd22l
040000070320     c                   if        flgcmt = *on
040100061218     c     kapd2         readpe    fiapd22l
040200070320     c                   else
040300070320     c     kapd2         readpe(n) fiapd22l
040400070320     c                   end
040500071108     c                   if        (not %eof(fiapd22l) and (
040600070726     c                             comiso<apd2dde or comiso>apd2dscad)) or
040700061220     c                             %eof(fiapd22l)
040800070208     C                   MOVEL     MSG(21)       VMSG
040900070208     C                   SETON                                        29
041000070208     C                   leave
041100070208     c                   end
041200070718     c* verifico che tipo pianificazione è abilitata
041300070718     c                   if        (§OGCGIO = ' ' and vctp = 'R') or
041400070718     c                             (§OGTLLS = ' ' and vctp = 'C')
041500070718     C                   MOVEL     MSG(26)       VMSG
041600070718     C                   SETON                                        49  28
041700070718     c                   leave
041800070718     c                   end
041900060927     c*
042000060927     c                   enddo
042100060927     c*
042200060926     c                   endsr
042300060926     C**************************************************************************
042400060926     C     SRAZZ2        BEGSR
042500060926     C**************************************************************************
042600071126     C                   SETOFF                                       404115
042700071126     C                   SETOFF                                       444716
042800071126     c                   setoff                                       282425
042900071126     c                   setoff                                       261112
043000071126     c                   clear                   vmsg
043100071126     c                   movea     '000000000000'*in(1)
043200061222     c                   move      vpdr          kpdr
043300061222     c     kapd          chain     fiapd01l
043400061222     c                   if        %found(fiapd01l)
043500061222     c                   movel     apdrsc        vdpdr
043600070119     c                   movel     apdflr        dapdflr
043700070119     c                   else
043800070119     c                   eval      d02err = '1'
043900070119     c                   movel     fidg02ds      kpjbu
044000070119     c                   seton                                        lr
044100070119     c                   return
044200070119     c                   end
044300070717     c                   movel     vdta          vdde
044400070717     c                   movel     '2039-12-31'  vscad
044500070717     c                   clear                   vatb
044600070717     c                   clear                   savnras
044700070717     c                   clear                   hdde
044800070717     c                   clear                   hscad
044900060927     c                   clear                   tc
045000060927     c                   clear                   dtc
045100070831     c                   eval      vnftl=999
045200070831     c                   if        vctp = 'C'
045300070718     c                   eval      vpkl = apdpkl
045400070718     c                   eval      vvlu = apdvlu
045500070831     c                   else
045600070831     c                   clear                   vpkl
045700070831     c                   clear                   vvlu
045800070831     c                   end
045900070718     c* d03
046000061218     c                   eval      vftc = 'E'
046100060927     c                   clear                   ts
046200060927     c                   clear                   dts
046300061214     c                   eval      vfts = 'E'
046400060927     c                   clear                   vtol
046500070718     c* d04
046600070718     c                   eval      vspi = 'N'
046700070718     c                   clear                   vtpa
046800070718     c                   clear                   vrtol
046900070115     c                   seton                                        88
047000061221     c* se non sono in inserimento controllo se esiste il record
047100070913     c                   if        not *inkj
047200070913     c                             or (d02file='FIDST00F' and d02cmd='KJ')
047300070913     c                             or (d02file='FIDST00F' and d02cmd='KF'
047400070913     c                             and d02ana = '1')
047500061218     c                   move      vdta          dtaiso
047600061218     c                   move      dtaiso        comiso
047700061218     c     kapd21        setgt     fiapd22l
047800070320     c                   if        d02file='FIDST00F' or
047900070320     c                             flgcmt = *off
048000070202     c     kapd2         readpe(n) fiapd22l                               88
048100070202     c                   else
048200061218     c     kapd2         readpe    fiapd22l                               88
048300070202     c                   end
048400070724     c* per le distinte l'anagrafico deve essere attivo mentre per
048500070724     c* l'anagrafico no perchè si può riattivare
048600061222     c                   if        not *in88 and comiso >= apd2dde and
048700070724     c                             comiso <= apd2dscad and ((
048800070724     c                             d02file='FIDST00F' and apd2atb = ' ') or
048900070724     c                             d02file<>'FIDST00F')
049000061222     C                   EXSR      SRCAMPI
049100061222     c                   else
049200061222     c                   seton                                        88
049300061222     c                   end
049400061222     c                   end
049500061213     c                   if        d02file = 'FIDST00F'
049600070320     c                   if        flgcmt = *off
049700070410     c     kdst          chain(n)  FIDST09l                           88
049800070320     c                   else
049900070410     c     kdst          chain     FIDST09l                           88
050000070320     c                   end
050100060929     C  n88              EXSR      SRCAMPI1
050200061011     c                   end
050300061011     c                   if        vinc = ' '
050400061011     c                   eval      vinc = 'S'
050500061011     c                   end
050600070717     c                   if        vrinc= ' '
050700070718     c                   eval      vrinc= 'S'
050800070612     c                   end
050900070718     c                   if        vspi= ' '
051000070718     c                   eval      vspi = 'N'
051100070718     c                   end
051200061221     c* trovato il record
051300060929     c                   if        not *in88
051400070319     c                   select
051500070320     c* estensione annullata, entro in riattivazione
051600070319     c                   when      vatb = 'A'
051700060927     c                   eval      vcmd ='RIATTIVAZIONE'
051800060928     C                   SETON                                        41
051900070320     c* richiesto annullamento oppure anagrafico aut senza giri, entro in
052000070320     c* annullamento
052100070718     c                   when      d02cmd = 'KQ'
052200060927     c                   seton                                        40
052300060927     C                   EVAL      VCMD='ANNULLAMENTO'
052400070413     c* aggiornamento
052500070413     c                   when      d02cmd = 'KF' or d02cmd = ' '
052600070913     c                   if        d02ana = '1'
052700070913     C                   EVAL      VCMD='INSERIMENTO'
052800070913     c                   else
052900070913     C                   EVAL      VCMD='AGGIORNAMENTO'
053000070913     c                   end
053100070719     c                   seton                                        4440
053200070319     c                   other
053300070320     c* visualizzazione
053400070319     C                   EVAL      VCMD='VISUALIZZA'
053500070319     c                   seton                                        47
053600070319     c                   endsl
053700060928     C                   ELSE
053800070115     c* se sono richiamato, non trovo il record e non sono in inserimento
053900070115     c* esco
054000070115     c                   if        d02cmd <>'KJ' and d02cmd <> ' ' and
054100070115     c                             not *inkj
054200070119     c                   eval      d02err = '1'
054300070119     c                   movel     fidg02ds      kpjbu
054400060928     c                   seton                                        lr
054500060928     c                   return
054600060928     c                   end
054700070320     c* inserimento
054800061218     c                   clear                   fiapd2ds
054900060927     C                   EVAL      VCMD='INSERIMENTO'
055000060928     c                   seton                                        42
055100060927     C                   END
055200061222     c* decodifico mattina/pomeriggio/giornata
055300061222     c                   select
055400061222     c                   when      vfpp = 'M'
055500061222     c                   eval      vdfpp = 'Mattina'
055600070125     c                   eval      vdfpp1= 'Mattina'
055700061222     c                   when      vfpp = 'P'
055800061222     c                   eval      vdfpp = 'Pomeriggio'
055900070125     c                   eval      vdfpp1= 'Pomeriggio'
056000061222     c                   endsl
056100060927     c                   endsr
056200060926     C**************************************************************************
056300060926     C     SRcampi       BEGSR
056400060926     C**************************************************************************
056500060927     c                   movea     tc1           tc
056600061213     c                   movea     ts1           ts
056700060929     c                   exsr      srdec
056800070719     c* devo testare se giri annulati
056900070719     c                   if        vctp = 'C'
057000070719     c     apd2nras      chain(n)  fiapd31l
057100070719     c                   if        %found(fiapd31l)
057200070719     c                   eval      vatb = apd3atb
057300070719     c                   else
057400070719     c                   clear                   vatb
057500070719     c                   end
057600070719     c                   else
057700070719     c     apd2nras      chain(n)  fiapd51l
057800070719     c                   if        %found(fiapd51l)
057900070719     c                   eval      vatb = apd5atb
058000070719     c                   else
058100070719     c                   clear                   vatb
058200070719     c                   end
058300070719     c                   end
058400061213     c                   eval      vftc = apd2ftc
058500061213     c                   eval      vfts = apd2fts
058600060926     c                   eval      vinc = apd2inc
058700070718     c                   eval      vtol = apd2tol
058800061222     c                   movel     apd2dde       hdde
058900070119     c                   movel     apd2dscad     hscad
059000061213     c                   movel     apd2dde       dtaiso
059100061213     c                   movel     dtaiso        dtaeur
059200061213     c                   movel     dtaeur        vdde
059300070119     c                   movel     apd2dscad     dtaiso
059400061213     c                   movel     dtaiso        dtaeur
059500070119     c                   movel     dtaeur        vscad
059600070119     c                   eval      savnras= apd2nras
059700070718     c                   eval      vrinc = apd2rinc
059800070718     c                   eval      vrtol = apd2rtol
059900070718     c                   eval      vspi = apd2rspi
060000070718     c                   eval      vtpa = apd2rtpa
060100070718     c* consegne
060200070718     c                   if        vctp = 'C'
060300070719     c                   if        apd2nftl <> 0
060400070718     c                   eval      vnftl= apd2nftl
060500070719     c                   end
060600070719     c                   if        apd2pkl <> 0
060700070718     c                   eval      vpkl = apd2pkl
060800070719     c                   end
060900070719     c                   if        apd2vlu <> 0
061000070718     c                   eval      vvlu = apd2vlu
061100070719     c                   end
061200070718     c                   else
061300070718     c* ritiri
061400070719     c                   if        apd2rnftl <> 0
061500070718     c                   eval      vnftl= apd2rnftl
061600070719     c                   end
061700070719     c                   if        apd2rpkl <> 0
061800070718     c                   eval      vpkl = apd2rpkl
061900070719     c                   end
062000070719     c                   if        apd2rvlu <> 0
062100070718     c                   eval      vvlu = apd2rvlu
062200070719     c                   end
062300070718     c                   end
062400940411     C*
062500910122     C                   ENDSR
062600060929     C**************************************************************************
062700060929     C     SRcampi1      BEGSR
062800060929     C**************************************************************************
062900060929     c                   movea     tc1d          tc
063000060929     c                   movea     ts1d          ts
063100060929     c                   exsr      srdec
063200070719     c* devo testare se giri annulati
063300070719     c                   if        vctp = 'C'
063400070719     c     kdst1         chain(n)  fidst31l
063500070719     c                   if        %found(fidst31l)
063600070719     c                   eval      vatb = dst3atb
063700070719     c                   else
063800070719     c                   clear                   vatb
063900070719     c                   end
064000070719     c                   else
064100070719     c     kdst1         chain(n)  fidst51l
064200070719     c                   if        %found(fidst51l)
064300070719     c                   eval      vatb = dst5atb
064400070719     c                   else
064500070719     c                   clear                   vatb
064600070719     c                   end
064700070719     c                   end
064800061213     c                   eval      vftc = dstftc
064900061213     c                   eval      vfts = dstfts
065000061213     c                   eval      vinc = dstinc
065100060929     c                   eval      vtol = dsttol
065200070718     c                   eval      vrinc = dstrinc
065300070718     c                   eval      vrtol = dstrtol
065400070718     c                   eval      vtpa = dstrtpa
065500070718     c                   eval      vspi = dstrspi
065600070718     c* consegne
065700070718     c                   if        vctp = 'C'
065800070719     c                   if        dstnftl <> 0
065900070718     c                   eval      vnftl= dstnftl
066000070719     c                   end
066100070719     c                   if        dstpkl <> 0
066200070718     c                   eval      vpkl = dstpkl
066300070719     c                   end
066400070719     c                   if        dstvlu <> 0
066500070718     c                   eval      vvlu = dstvlu
066600070719     c                   end
066700070718     c                   else
066800070718     c* ritiri
066900070719     c                   if        dstrnftl <> 0
067000070718     c                   eval      vnftl= dstrnftl
067100070719     c                   end
067200070719     c                   if        dstrpkl  <> 0
067300070718     c                   eval      vpkl = dstrpkl
067400070719     c                   end
067500070719     c                   if        dstrvlu <> 0
067600070718     c                   eval      vvlu = dstrvlu
067700070719     c                   end
067800070718     c                   end
067900060929     C*
068000060929     C                   ENDSR
068100060929     C**************************************************************************
068200060929     C     SRdec         BEGSR
068300060929     C**************************************************************************
068400060929     c* decodifica consegna particolare
068500060929     c                   eval      kcodt= '1P'
068600060929     c                   do        5             xx
068700060929     c                   if        tc(xx) <> ' '
068800060929     c                   movel     tc(xx)        kkeyt
068900060929     c     ktabel        chain     tabel00f
069000060929     c                   if        %found(tabel00f)
069100060929     c                   movel     tbluni        ds1p
069200060929     c                   movel     §1pdes        dtc(xx)
069300060929     c                   end
069400070125     c                   else
069500070125     c                   clear                   dtc(xx)
069600060929     c                   end
069700060929     c                   enddo
069800060929     c* decodifica tipo servizio
069900060929     c                   eval      kcodt= '5E'
070000060929     c                   do        2             xx
070100060929     c                   if        ts(xx) <> ' '
070200060929     c                   movel     ts(xx)        kkeyt
070300060929     c     ktabel        chain     tabel00f
070400060929     c                   if        %found(tabel00f)
070500060929     c                   movel     tbluni        ds5e
070600060929     c                   movel     §5edes        dts(xx)
070700060929     c                   end
070800070125     c                   else
070900070125     c                   clear                   dts(xx)
071000060929     c                   end
071100060929     c                   enddo
071200060929     C*
071300060929     C                   ENDSR
071400940809     C**************************************************************************
071500060926     C     srd02         BEGSR
071600940809     C**************************************************************************
071700060926     C                   EXSR      SRAZZ2
071800061221     C                   EXSR      gess01
071900940809     C*
072000060926     c                   do        *hival
072100061213     C                   WRITE     dg02T01
072200061218     C                   WRITE     dg02d02
072300070320     c* proteggo il video se record annullato o in visualizzazione
072400070319     c                   select
072500070719     c                   when      *in41 or *in47
072600061213     C                   write     dg02c01
072700060928     C                   EXFMT     protect
072800070319     c                   other
072900061213     C                   EXFMT     dg02c01
073000070319     c                   endsl
073100060926     c*
073200060926     c                   if        *inkc or *inkl
073300060926     C                   leave
073400060926     c                   end
073500061229     c* duplica da altro autotrasportatore
073600061229     c                   if        *inkg
073700061229     c                   exsr      srw01
073800061229     c                   end
073900070125     c* altri dati
074000070125     c                   if        *inkb
074100070320     c                   seton                                        46
074200070718     c* consegne
074300070718     c                   if        vctp = 'C'
074400070125     c                   exsr      srd03
074500070718     c                   else
074600070718     c* ritiri
074700070718     c                   exsr      srd04
074800070718     c                   end
074900070320     c                   setoff                                       46
075000070125     c                   if        *inkc
075100070125     C                   leave
075200070125     c                   end
075300070125     c                   end
075400061229     C* CONTROLLI
075500070330     C  n47              EXSR      Ctr02
075600061229     C   28              iter
075700061221     C* CMD16 - ANNULLAMENTO solo per anagrafico (non abilitato x 43)
075800060926     C                   if        *inkq
075900070718     c* consegne
076000070718     c                   if        vctp = 'C'
076100070718     c                   exsr      srannc
076200070718     c                   else
076300070718     c* ritiri
076400070718     c                   exsr      srannr
076500070718     c                   end
076600061221     c                   leave
076700060927     c                   ENDIF
076800920921     C*
076900060927     c                   if        *inke or *inkf
077000061213     c                   if        d02file <> 'FIDST00F'
077100920921     C                   EXSR      SCRIVI
077200061011     c                   else
077300060929     C                   EXSR      SCRIVI1
077400061011     c                   end
077500061220     c                   if        not *in28
077600060927     c                   leave
077700061220     c                   end
077800060927     C                   ENDIF
077900931130     C* 98 ON - WRITE UPDATE O CHAIN NON ANDATE A BUON FINE
078000060926     C   98              iter
078100060927     c                   enddo
078200061221     c                   if        not *in28  and
078300061221     c                             (d02com = ' ' or d02com = 'S')
078400061221     c                   commit
078500061221     c                   end
078600920921     C*
078700060926     C                   ENDSR
078800060926     C**************************************************************************
078900070718     C     srannc        BEGSR
079000060926     C**************************************************************************
079100060927     C*
079200070718     c     apd2nras      setll     fiapd31l
079300070718     c                   do        *hival
079400070718     c     apd2nras      reade     fiapd31l
079500070718     C                   IF        %eof(fiapd31l)
079600070718     c                   leave
079700070718     c                   end
079800070718     C                   MOVE      'A'           APD3ATB
079900070718     C                   UPDATE    fiapd300                             98
080000070718     C     *IN98         IFEQ      *ON
080100070718     C                   MOVEL     MSG(4)        VMSG
080200070718     C                   SETON                                        28
080300070718     C                   iter
080400070718     c                   ENDIF
080500070718     c                   enddo
080600070718     C*
080700070718     C                   ENDSR
080800070718     C**************************************************************************
080900070718     C     srannr        BEGSR
081000070718     C**************************************************************************
081100070718     C*
081200070718     c     apd2nras      setll     fiapd51l
081300070718     c                   do        *hival
081400070718     c     apd2nras      reade     fiapd51l
081500070718     C                   IF        %eof(fiapd51l)
081600070718     c                   leave
081700070718     c                   end
081800070718     C                   MOVE      'A'           APD5ATB
081900070718     C                   UPDATE    fiapd500                             98
082000070718     C     *IN98         IFEQ      *ON
082100070718     C                   MOVEL     MSG(4)        VMSG
082200070718     C                   SETON                                        28
082300070718     C                   iter
082400070718     c                   ENDIF
082500070718     c                   enddo
082600070718     C*
082700070718     C                   ENDSR
082800070718     C**************************************************************************
082900070718     C     ctr02         BEGSR
083000070718     C**************************************************************************
083100070718     C*
083200070123     c                   setoff                                       282425
083300070718     c                   setoff                                       261112
083400071126     c                   setoff                                       15
083500061220     c                   clear                   vmsg
083600060927     c                   movea     '000000000000'*in(1)
083700061219     C* controllo data deccorenza scadenza
083800061219     c                   move      vdde          dtadde
083900070119     c                   move      vscad         dtascad
084000061229     c* se richiesto annullamento controllo che non ci siano distinte
084100061229     c* aperte
084200061229     c                   if        *inkq
084300061229     c                   if        d02file <> 'FIDST00F'
084400061229     c                   exsr      srdst
084500061229     c                   if        dstape = '1'
084600061229     C                   MOVEL     MSG(17)       VMSG
084700061229     c                   seton                                        28
084800061229     c                   end
084900061229     c                   end
085000061229     c                   else
085100061222     c                   do
085200070723     c* se sono in inserimento o in riattivaz. controllo che l'anagrafico
085300070723     c* sia attiva
085400070723     c                   if        apdatb <> ' ' and (*in42 or *in41)
085500070115     C                   MOVEL     MSG(22)       VMSG
085600070115     C                   SETON                                            28
085700070115     c                   leave
085800070115     c                   end
085900061219     c* decorrenza < scadenza
086000070119     c                   if        dtadde > dtascad
086100061220     C                   MOVEL     MSG(11)       VMSG
086200061219     c                   seton                                        281112
086300061222     c                   leave
086400061219     c                   end
086500061219     c* verifico che non ci siano anagrafiche in vigore nello stesso periodo
086600061221     c* solo x anagrafico
086700061221     c                   if        d02file <> 'FIDST00F'
086800061219     c                   move      vdde          comiso
086900061219     c     kapd2         setgt     fiapd22l
087000061221     c                   do        *hival
087100070320     c                   if        flgcmt = *on
087200061219     c     kapd2         readpe    fiapd22l
087300070320     c                   else
087400070320     c     kapd2         readpe(n) fiapd22l
087500070320     c                   end
087600061221     c                   if        %eof(fiapd22l)
087700061221     c                   leave
087800061221     c                   end
087900070119     c                   if        apd2nras= savnras and not *in42
088000061221     c                   iter
088100061221     c                   end
088200070119     c                   move      apd2dscad     comscad
088300061219     c                   move      apd2dde       comdde
088400070119     c                   if        dtadde >= comdde and dtadde <= comscad
088500061220     C                   MOVEL     MSG(15)       VMSG
088600061219     c                   seton                                        281112
088700061222     c                   leave
088800061219     c                   end
088900070119     c                   if        dtascad>= comdde and dtascad<= comscad
089000061220     C                   MOVEL     MSG(15)       VMSG
089100061219     c                   seton                                        281112
089200061222     c                   leave
089300061219     c                   end
089400070119     c                   if        dtadde <=comdde and dtascad>=comscad
089500061220     C                   MOVEL     MSG(15)       VMSG
089600061219     c                   seton                                        281112
089700061222     c                   leave
089800061219     c                   end
089900061221     c                   enddo
090000061222     c   28              leave
090100070718     c                   end
090200070119     c* controllo max fermate
090300070119     c                   if        vnftl= 0
090400070123     c                   movel     Msg(23)       Vmsg
090500070119     c                   seton                                        2824
090600070119     c                   leave
090700070119     c                   endif
090800070123     c* controllo peso
090900070123     c                   if        vpkl = 0
091000070123     c                   movel     Msg(3)        Vmsg
091100070123     c                   seton                                        2825
091200070123     c                   leave
091300070123     c                   endif
091400070124     c* controllo volume
091500070123     c                   if        vvlu = 0
091600070123     c                   movel     Msg(3)        Vmsg
091700070123     c                   seton                                        2826
091800070123     c                   leave
091900070123     c                   endif
092000071126     c* controllo che la tolleranza non superi la capienza del campo
092100071126     c* peso, volume e fermate
092200071126     c                   if        vctp = 'C'
092300071126     c                   eval      wtol = vtol
092400071126     c                   else
092500071126     c                   eval      wtol = vRtol
092600071126     c                   end
092700071126     c                   exsr      srtol
092800071126     c                   if        *in28
092900071126     c                   seton                                        15
093000071126     c                   seton                                        242526
093100071126     C                   MOVEL     MSG(28)       VMSG
093200071126     c                   leave
093300071126     c                   endif
093400061218     c* controllo il sfl
093500061218     c  n28              exsr      ctrs01
093600061222     c                   enddo
093700070718     c* se cambia date avviso che verranno variate anche per consegne/ritiri
093800071009     c* solo la prima volta e se partiti con ritiri
093900071009     c                   if        §OGCGIO <> ' '
094000070718     c                   if        d02file <> 'FIDST00F' and not *in28
094100070723     c                             and not *in42
094200070719     c                             and (dtadde<>savdde or dtascad<>savscad)
094300070719     c                   move      dtadde        savdde
094400070719     c                   move      dtascad       savscad
094500070719     c                   move      hdde          comhdde
094600070719     c                   move      hscad         comhscad
094700070718     c                   if        dtadde <> comhdde or dtascad <> comhscad
094800070718     C                   MOVEL     MSG(27)       VMSG
094900070719     c                   seton                                        28
095000070718     c                   end
095100071009     c                   end
095200070718     c                   end
095300061229     c                   end
095400060926     C*
095500060926     C                   ENDSR
095600070125     C**************************************************************************
095700070718     C* gestione altri dati consegne
095800070125     C**************************************************************************
095900070125     C     Srd03         BEGSR
096000070125     C*
096100070125     c                   setoff                                       28
096200070125     c                   clear                   $msg
096300070125     c                   do        *hival
096400070719     c* proteggo il video se sono in visualizzazione o se record annullato
096500070320     c                   select
096600070719     c                   when      *in47 or *in41
096700070319     c                   WRITE     dg02T01
096800070319     C                   WRITE     dg02d02
096900070319     C                   write     dg02d03
097000070320     C                   EXFMT     protect
097100070320     c                   other
097200070125     c                   WRITE     dg02T01
097300070125     C                   WRITE     dg02d02
097400070125     C                   EXFMT     dg02d03
097500070320     c                   endsl
097600070125     C* CMD3 - FINE LAVORO
097700070125     c                   if        *inkl or *inkc
097800070125     C                   leave
097900070125     c                   end
098000070125     c* duplica da altro autotrasportatore
098100070125     c                   if        *inkg
098200070125     c                   exsr      srw01
098300070125     c                   end
098400070125     C* CONTROLLI
098500070330     c  n47              exsr      ctr03
098600070125     c   28              iter
098700070125     c*
098800070125     c                   if        *inkf or *inkq or *inke
098900070125     C                   leave
099000070125     c                   end
099100070125     c                   enddo
099200070125     C*
099300070125     C                   ENDSR
099400070125     C**************************************************************************
099500070125     C     ctr03         BEGSR
099600070125     C**************************************************************************
099700070125     C*
099800071126     c                   setoff                                       2815
099900070125     c                   clear                   vmsg
100000070125     c                   movea     '0000000'     *in(1)
100100070125     C* consegne particolari
100200070125     c                   clear                   dtc
100300070125     C                   MOVEL     '1P'          kcodt
100400070125     c                   do        5             xx
100500070125     C* RICERCA
100600070125    2C     tc(xx)        IFEQ      '?'
100700070125     C                   clear                   KKEYT
100800070125     C                   EXSR      CX§TAB
100900070125     C                   MOVEL     kkeyt         tc(xx)
101000070125     C                   MOVEL     kdest         dtc(xx)
101100070726     C                   END
101200070726     C                   ENDdo
101300070125     C* CONTROLLO
101400070125     c                   do        5             xx
101500070125     c                   if        tc(xx) <> ' '
101600070125     C                   MOVEL(P)  tc(xx)        kKEYt
101700070125     C     KTABEL        CHAIN     TABEL00F                           31
101800070125     C     *IN31         IFEQ      *ON
101900070125     C                   MOVEL     MSG(5)        VMSG
102000070125     c                   eval      *in(xx) = '1'
102100070125     C                   SETON                                        28
102200070125     C                   leavesr
102300070125     C                   ENDIF
102400070125     C     TBLFLG        IFNE      ' '
102500070125     c                   eval      *in(xx) = '1'
102600070125     C                   MOVEL     MSG(5)        VMSG
102700070125     C                   SETON                                        28
102800070125     C                   leavesr
102900070125     C                   ENDIF
103000070125     C                   MOVEL     TBLUNI        DS1P
103100070125     c                   movel     §1pdes        dtc(xx)
103200070125     C                   IF        §1puar <> 'S' and §1pupr <> 'S'
103300070125     c                   eval      *in(xx) = '1'
103400070125     C                   MOVEL     MSG(19)       VMSG
103500070125     C                   SETON                                        28
103600070125     C                   leavesr
103700070125     C                   ENDIF
103800070125     c                   end
103900070125     c                   enddo
104000070125     c* controllo che non ci siano c.p. doppie
104100070125     c                   do        5             xx
104200070125     c                   do        5             yy
104300070125     c                   if        tc(xx) <> ' ' and tc(yy) <> ' ' and
104400070125     c                             xx <> yy and tc(xx) = tc(yy)
104500070125     c                   movel     Msg(13)       Vmsg
104600070125     c                   seton                                        28
104700070125     c                   eval      *in(yy)='1'
104800070125     c                   leavesr
104900070125     c                   endif
105000070125     c                   enddo
105100070125     c                   enddo
105200070125     C* tipo servizio
105300070125     c                   clear                   dts
105400070125     c                   MOVEL     '5E'          kcodt
105500070125     C* RICERCA
105600070125     c                   do        2             xx
105700070125     c                   if        ts(xx) = '?'
105800070125     C                   clear                   KKEYT
105900070125     C                   EXSR      CX§TAB
106000070125     C                   MOVEL     kkeyt         ts(xx)
106100070125     C                   MOVEL     kdest         dts(xx)
106200070125     c                   end
106300070125     c                   enddo
106400070125     C* CONTROLLO
106500070125     c                   do        2             xx
106600070125     c                   if        ts(xx) <> ' '
106700070125     C                   MOVEL(P)  ts(xx)        kKEYt
106800070125     C     KTABEL        CHAIN     TABEL00F                           31
106900070125     C     *IN31         IFEQ      *ON
107000070125     C                   MOVEL     MSG(6)        VMSG
107100070125     c                   eval      yy = xx +5
107200070125     c                   eval      *in(yy) = '1'
107300070125     C                   SETON                                        28
107400070125     C                   leavesr
107500070125     C                   ENDIF
107600070125     C     TBLFLG        IFNE      ' '
107700070125     C                   MOVEL     MSG(6)        VMSG
107800070125     c                   eval      yy = xx +5
107900070125     c                   eval      *in(yy) = '1'
108000070125     C                   SETON                                        28
108100070125     C                   leavesr
108200070125     C                   ENDIF
108300070125     C                   MOVEL     TBLUNI        DS5e
108400070125     c                   movel     §5edes        dts(xx)
108500070125     c                   end
108600070125     c                   enddo
108700070125     c* controllo che non ci siano t.s. doppi
108800070125     c                   if        ts(1) <> ' ' and ts(2) <> ' '
108900070125     c                             and ts(1) = ts(2)
109000070125     c                   movel     Msg(18)       Vmsg
109100070125     c                   seton                                        280607
109200070125     c                   leavesr
109300070125     c                   endif
109400071126     c* controllo che la tolleranza non superi la capienza del campo
109500071126     c* peso, volume e fermate
109600071126     c                   eval      wtol = vtol
109700071126     c                   exsr      srtol
109800071126     c                   if        *in28
109900071126     c                   seton                                        15
110000071126     c                   seton                                        242526
110100071126     C                   MOVEL     MSG(28)       VMSG
110200071126     c                   leavesr
110300071126     c                   endif
110400070125     C*
110500070125     C                   ENDSR
110600071126     C**************************************************************************
110700071126     C     ctr04         BEGSR
110800071126     C**************************************************************************
110900071126     C*
111000071126     c                   setoff                                       2816
111100071126     c                   clear                   vmsg
111200071126     c* controllo che la tolleranza non superi la capienza del campo
111300071126     c* peso, volume e fermate
111400071126     c                   eval      wtol = vRtol
111500071126     c                   exsr      srtol
111600071126     c                   if        *in28
111700071126     c                   seton                                        16
111800071126     c                   seton                                        242526
111900071126     C                   MOVEL     MSG(28)       VMSG
112000071126     c                   leavesr
112100071126     c                   endif
112200071126     C*
112300071126     C                   ENDSR
112400070718     C**************************************************************************
112500071126     c* controllo che la tolleranza non superi la capienza del campo
112600071126     c* peso, volume e fermate
112700070718     C**************************************************************************
112800071126     C     srtol         BEGSR
112900070718     C*
113000071126     c                   eval      wpeso =((vpkl * wtol)/100) + vpkl
113100071126     c                   eval      wvolume =((vvlu * wtol)/100) + vvlu
113200071126     c                   eval      wnftl =((vnftl* wtol)/100) + vnftl
113300071126     c                   if        wpeso > 999999,9 or
113400071126     c                             wvolume > 99,999 or
113500071126     c                             wnftl > 99999
113600071126     c                   seton                                        28
113700071126     c                   end
113800071126     C*
113900071126     C                   ENDSR
114000071126     C**************************************************************************
114100071126     C* gestione altri dati ritiri
114200071126     C**************************************************************************
114300071126     C     Srd04         BEGSR
114400071126     C*
114500071126     c                   setoff                                       2816
114600070718     c                   clear                   $msg
114700070718     c                   do        *hival
114800070719     c* proteggo il video se sono in visualizzazione o se record annullato
114900070718     c                   select
115000070719     c                   when      *in47 or *in41
115100070718     c                   WRITE     dg02T01
115200070718     C                   WRITE     dg02d02
115300070718     C                   write     dg02d04
115400070718     C                   EXFMT     protect
115500070718     c                   other
115600070718     c                   WRITE     dg02T01
115700070718     C                   WRITE     dg02d02
115800070718     C                   EXFMT     dg02d04
115900070718     c                   endsl
116000070718     C* CMD3 - FINE LAVORO
116100070718     c                   if        *inkl or *inkc
116200070718     C                   leave
116300070718     c                   end
116400071126     C* CONTROLLI
116500071126     c  n47              exsr      ctr04
116600071126     c   28              iter
116700070718     c* duplica da altro autotrasportatore
116800070718     c                   if        *inkg
116900070718     c                   exsr      srw01
117000070718     c                   end
117100070718     c*
117200070718     c                   if        *inkf or *inkq or *inke
117300070718     C                   leave
117400070718     c                   end
117500070718     c                   enddo
117600070718     C*
117700070718     C                   ENDSR
117800061219     C**************************************************************************
117900061219     C* GESTIONE SUBFILE
118000061219     C**************************************************************************
118100061229     C     srdst         BEGSR
118200061229     c                   clear                   dstape            1
118300061229     c                   clear                   dstfcf
118400061219     C*
118500061229     c     kdst4         setll     fidst04l
118600061229     c                   do        *hival
118700061229     c     kdst4         reade     fidst04l
118800061229     c                   if        %eof(fidst04l)
118900061229     c                   leave
119000061229     c                   end
119100070419     c                   if        dstatb <> ' '
119200070419     c                   iter
119300070419     c                   end
119400061229     c                   move      dstdfv        comdta
119500070119     c                   if        comdta>=dtadde and comdta<=dtascad
119600070627     c     kdst31        setll     fidst31l                               33
119700070627     c                   if        *in33
119800061229     c                   eval      dstape = '1'
119900061229     c                   leave
120000070627     c                   end
120100070720     c     kdst31        setll     fidst51l                               33
120200070720     c                   if        *in33
120300070720     c                   eval      dstape = '1'
120400070720     c                   leave
120500070720     c                   end
120600061229     c                   end
120700061229     c                   enddo
120800061229     C*
120900061229     C                   ENDSR
121000061229     C**************************************************************************
121100061229     C* GESTIONE SUBFILE
121200061229     C**************************************************************************
121300061229     C     GESS01        BEGSR
121400061229     C*
121500061219     C* PULIZIA SUBFILE
121600061219     C                   SETON                                        21
121700061219     C                   WRITE     dg02C01
121800061219     C                   SETOFF                                       2122
121900061219     C                   Z-ADD     0             NRR               4 0
122000061219     C*
122100061219     C* CARICAMENTO SUBFILE
122200070913     c                   if        d02file = 'FIAPD20F' or
122300070913     c                             d02file = *blanks or
122400070913     c                             (d02file = 'FIDST00F' and d02cmd = 'KJ'
122500070913     c                             and *in42) or
122600070913     c                             (d02file = 'FIDST00F' and d02cmd = 'KF'
122700070913     c                             and d02ana='1')
122800070202     c                   eval      flagcopia='0'
122900070718     c* consegne
123000070718     c                   if        vctp = 'C'
123100070718     c                   exsr      carapdc
123200070718     c                   else
123300070718     c* ritiri
123400070718     c                   exsr      carapdr
123500070913     c                   end
123600061222     c                   end
123700061221     c                   if        d02file = 'FIDST00F'
123800070718     c* consegne
123900070718     c                   if        vctp = 'C'
124000070718     c                   exsr      cardstc
124100070718     c                   else
124200070718     c* ritiri
124300070720     c                   exsr      cardstr
124400070718     c                   end
124500061221     c                   end
124600070125     C* carico 100 righe vuote
124700070125     c                   do        100
124800070124     c                   setoff                                       080910
124900070627     c                   setoff                                       1314
125000070129     c                   eval      vscfgsg = vfgs
125100070129     c                   clear                   vsccgi
125200070124     c                   clear                   vsdcgi
125300070119     c                   clear                   vscprgg
125400061219     c                   clear                   vscsql
125500090113     c                   if        nrr < 9999
125600061219     C                   ADD       1             NRR
125700061219     C                   WRITE     dg02s01
125800090113     c                   else
125900090113     c                   leave
126000090113     c                   end
126100061219     c                   enddo
126200061219     c                   seton                                        22
126300061219     C                   Z-ADD     1             REC
126400061219     C                   ENDSR
126500061218     C**************************************************************************
126600070718     C* Carica sfl anagrafico consegne
126700061218     C**************************************************************************
126800070718     C     Carapdc       BEGSR
126900061218     C*
127000070119     C     savnras       SETLL     fiapd31l
127100061221     c                   do        *hival
127200070202     c                   if        flagcopia = '1' or d02file='FIDST00F'
127300070320     c                             or flgcmt = *off
127400070202     C     savnras       READe(n)  fiapd31l                               32
127500070202     c                   else
127600070119     C     savnras       READe     fiapd31l                               32
127700070202     c                   end
127800061221     c                   if        %eof(fiapd31l)
127900061221     c                   leave
128000061221     c                   end
128100071108     c* se il record è annulato lo ignoro
128200071108     c                   if        d02file = 'FIDST00F'
128300071108     c                             and apd3atb <> ' '
128400071108     c                   iter
128500071108     c                   end
128600061221     C*
128700070719     c                   eval      vatb = apd3atb
128800070129     c                   eval      vscfgsg= apd3fgsg
128900070129     c                   eval      vsccgi = apd3cgi
129000070129     c                   clear                   vsdcgi
129100070124     c                   exsr      srcgi
129200070119     c                   eval      vscprgg= apd3prgg
129300061221     c                   eval      vscsql = apd3sql
129400061221     C                   ADD       1             NRR
129500061221     C                   WRITE     dg02s01
129600070726     C                   ENDDO
129700061221     C*
129800061221     C                   ENDSR
129900070718     C**************************************************************************
130000070718     C* Carica sfl anagrafico ritiri
130100070718     C**************************************************************************
130200070718     C     Carapdr       BEGSR
130300070718     C*
130400070718     C     savnras       SETLL     fiapd51l
130500070718     c                   do        *hival
130600070718     c                   if        flagcopia = '1' or d02file='FIDST00F'
130700070718     c                             or flgcmt = *off
130800070718     C     savnras       READe(n)  fiapd51l                               32
130900070718     c                   else
131000070718     C     savnras       READe     fiapd51l                               32
131100070718     c                   end
131200070718     c                   if        %eof(fiapd51l)
131300070718     c                   leave
131400070718     c                   end
131500071108     c* se il record è annulato lo ignoro
131600071108     c                   if        d02file = 'FIDST00F'
131700071108     c                             and apd5atb <> ' '
131800071108     c                   iter
131900071108     c                   end
132000070718     C*
132100070719     c                   eval      vatb = apd5atb
132200070718     c                   eval      vscfgsg= apd5fgsg
132300070718     c                   eval      vsccgi = apd5cgi
132400070718     c                   clear                   vsdcgi
132500070718     c                   exsr      srcgi
132600070718     c                   eval      vscprgg= apd5prgg
132700070718     c                   eval      vscsql = apd5sql
132800070718     C                   ADD       1             NRR
132900070718     C                   WRITE     dg02s01
133000070726     C                   ENDDO
133100070718     C*
133200070718     C                   ENDSR
133300061221     C**************************************************************************
133400070718     C* Carica sfl distinta consegne
133500061221     C**************************************************************************
133600070718     C     Cardstc       BEGSR
133700061221     C*
133800061228     C     kdst1         SETLL     fidst31l
133900061221     c                   do        *hival
134000070627     c                   if        flagcopia = '1'
134100070627     c                             or flgcmt = *off
134200070627     C     kdst1         READe(n)  fidst31l                               32
134300070627     c                   else
134400070627     C     kdst1         READe     fidst31l                               32
134500070627     c                   end
134600061222     c                   if        %eof(fidst31l)
134700061221     c                   leave
134800061221     c                   end
134900061221     C*
135000070129     c                   eval      vscfgsg= dst3fgsg
135100070129     c                   eval      vsccgi = dst3cgi
135200070129     c                   clear                   vsdcgi
135300070124     c                   exsr      srcgi
135400070119     c                   eval      vscprgg= dst3prgg
135500061221     c                   eval      vscsql = dst3sql
135600061221     C                   ADD       1             NRR
135700061221     C                   WRITE     dg02s01
135800070726     C                   ENDDO
135900061221     C*
136000061221     C                   ENDSR
136100070718     C**************************************************************************
136200070718     C* Carica sfl distinta ritiri
136300070718     C**************************************************************************
136400070718     C     Cardstr       BEGSR
136500070718     C*
136600070718     C     kdst1         SETLL     fidst51l
136700070718     c                   do        *hival
136800070718     c                   if        flagcopia = '1'
136900070718     c                             or flgcmt = *off
137000070718     C     kdst1         READe(n)  fidst51l                               32
137100070718     c                   else
137200070718     C     kdst1         READe     fidst51l                               32
137300070718     c                   end
137400070718     c                   if        %eof(fidst51l)
137500070718     c                   leave
137600070718     c                   end
137700070718     C*
137800070718     c                   eval      vscfgsg= dst5fgsg
137900070718     c                   eval      vsccgi = dst5cgi
138000070718     c                   clear                   vsdcgi
138100070718     c                   exsr      srcgi
138200070718     c                   eval      vscprgg= dst5prgg
138300070718     c                   eval      vscsql = dst5sql
138400070718     C                   ADD       1             NRR
138500070718     C                   WRITE     dg02s01
138600070726     C                   ENDDO
138700070718     C*
138800070718     C                   ENDSR
138900061221     C**************************************************************************
139000061221     C* CONTROLLO DATI SUBFILE
139100061221     C**************************************************************************
139200061221     C     CTRS01        BEGSR
139300061221     C*
139400090113     c                   z-add     0             y                 4 0
139500090113     c                   z-add     0             z                 4 0
139600070123     c                   clear                   cgi
139700061219     c                   clear                   prsq
139800061220     c                   setoff                                       28
139900061220     c                   clear                   vmsg
140000070726     C                   do        *hival        nn
140100061220     C     nn            chain     dg02S01                            31
140200070726     C   31              leave
140300061220     c                   setoff                                       080910
140400070627     c                   setoff                                       1314
140500061218     C                   Z-ADD     NRR           REC
140600070124     c                   if        vsccgi = ' '
140700070124     c                   clear                   vsdcgi
140800070124     c                   end
140900061218     C* CONTROLLI
141000070208     C* p.o. giro
141100070208     C     Vscfgsg       IFEQ      *ZEROS
141200070208     C                   MOVEL     MSG(14)       VMSG
141300070208     C                   SETON                                        2813
141400070129     c                   else
141500170615     c******             select
141600170615     c******             when      DUTlpo = '2' or DUTlpo = *blanks
141700070208     C* deve essere in £6 se secondo livello
141800170615     C*****Vscfgsg       LOOKUP    L6                                     45
141900170615     C*****              when      DUTlpo = '1'
142000070208     C* deve essere in £1 se primo livello
142100170615     C*****Vscfgsg       LOOKUP    L1                                     45
142200170615     C*****              when      DUTlpo = 'S'
142300070208     C* deve essere in azorg01l
142400070208     C     Vscfgsg       chain     azorg01l
142500070129     c                   if        %found(azorg01l)
142600070129     c                   if        orgfag <> 'A' and orgfag <> 'F'
142700070129     c                   setoff                                       45
142800070129     c                   else
142900070129     c                   seton                                        45
143000070129     c                   end
143100070129     c                   else
143200070129     c                   setoff                                       45
143300070129     c                   end
143400170615     C*****              ENDsl
143500170615
143600170615       //?Filiale in gestione all'utente
143700170615         IF  %lookup(%editc(vscfgsg:'X'):pog) = 0;
143800170615           *in45 = *off;
143900170615         ENDIF;
144000170615
144100070208     C* Filiale non presente in tabella
144200070208     C                   IF        not *in45 and not *in28
144300070208     C                   MOVEL     MSG(12)       VMSG
144400070208     C                   SETON                                        2813
144500070208     C                   END
144600070129     c                   end
144700070129     c* giro
144800070124     c                   if        vsccgi <> ' ' and not *in28
144900070726     C     '?'           SCAN      Vsccgi                                 31
145000070124     c                   if        *in31
145100070124     C                   EXSR      srriccgi
145200070124     c                   end
145300070124     C                   EXSR      srcgi
145400070124     c                   if        not *in28
145500090113     c*                  z-add     1             x                 3 0
145600070129     c                   movel     vscfgsg       com13            13
145700070129     c                   move      vsccgi        com13
145800070129     c     com13         lookup    cgi                                    08
145900061219     c                   if        *in08
146000070129     c                   seton                                        2813
146100061220     c                   movel     Msg(10)       Vmsg
146200061219     c                   else
146300061220     c                   add       1             y
146400070129     c                   movel     vscfgsg       cgi(y)
146500070129     c                   move      vsccgi        cgi(y)
146600061219     c                   end
146700070124     c                   end
146800061220     c*
146900070119     c     vscprgg       comp      0                                      09
147000061220     c                   if        *in09 and not *in28
147100061219     c                   seton                                        28
147200061220     c                   movel     Msg(7)        Vmsg
147300061219     c                   end
147400070124     c*
147500061219     c     vscsql        comp      0                                      10
147600061220     c                   if        *in10 and not *in28
147700061219     c                   seton                                        28
147800061220     c                   movel     Msg(16)       Vmsg
147900061219     c                   end
148000061220     c                   end
148100070208     c*
148200070208     c                   if        not *in28
148300070208     c                   if        (vsccgi = ' ' and
148400070208     c                             (vscprgg<> 0 or vscsql <> 0)) or
148500070208     c                             (vsccgi<> ' ' and
148600070208     c                             (vscprgg= 0 or vscsql = 0))
148700070208     c                   seton                                        280809
148800070208     c                   seton                                        1013
148900070208     c                   movel     Msg(3)        Vmsg
149000070208     c                   end
149100070208     c                   end
149200061220     c* stessa priorità sequenza
149300061220     c                   if        not *in28 and
149400070119     c                             (vscprgg<> 0 or vscsql <> 0)
149500090113     c*                  z-add     1             x                 3 0
149600070119     c                   movel     vscprgg       comps             4 0
149700061219     c                   move      vscsql        comps
149800061219     c     comps         lookup    prsq                                   28
149900061220     c                   if        *in28
150000061219     c                   seton                                        0910
150100061220     c                   movel     Msg(8)        Vmsg
150200061219     c                   else
150300061220     c                   add       1             z
150400061220     c                   move      comps         prsq(z)
150500061219     c                   end
150600061220     c                   end
150700061218     C*
150800061219     C                   UPDATE    dg02S01
150900061218     C   28              leave
151000061218     C*
151100070726     C                   ENDDO
151200061219     c* almeno un giro
151300070720     c*                  if        y = 0 and not *in28
151400070720     c*    1             chain     dg02s01
151500070720     c*                  seton                                        280813
151600070720     c*                  update    dg02s01
151700070720     c*                  movel     Msg(9)        Vmsg
151800070720     c*                  end
151900061220     c                   z-add     1             rec
152000061218     C*
152100061218     C                   ENDSR
152200940809     C**************************************************************************
152300070124     C* controllo e decodifico giro
152400940809     C**************************************************************************
152500070124     C     Srcgi         BEGSR
152600940809     C*
152700070627     c                   setoff                                       081314
152800070129     c                   clear                   fidg09ds
152900070129     c                   eval      D09IOP0 = '001'
153000070129     c                   eval      d09ifgs = vscfgsg
153100070129     c                   eval      d09icgi = vsccgi
153200070718     c                   eval      D09ITUG = vctp
153300070129     c                   if        d02file = 'FIDST00F'
153400070129     c                   move      vdfv          dtaiso
153500070129     c                   move      dtaiso        d09idat
153600070129     c                   else
153700070129     c                   move      vdde          dtaiso
153800070129     c                   move      dtaiso        d09idat
153900070129     c                   end
154000070129     c                   eval      kpjbu = fidg09ds
154100070129     c                   call      'FIDG09R'
154200070129     c                   parm                    kpjba
154300070129     c                   eval      fidg09ds = kpjbu
154400070129     c                   if        d09oerr = '1'
154500070129     c                   seton                                        280813
154600070129     c                   movel     Msg(2)        Vmsg
154700070129     c                   else
154800070320     c                   eval      dgrgflo = D09OFLO
154900070320     c                   if        §grgdst = 'N'
155000070320     c                   seton                                        280813
155100070320     c                   movel     Msg(25)       Vmsg
155200070320     c                   else
155300070130     c                   if        d09ocgi <> ' '
155400070129     c                   eval      vscfgsg= d09ofgs
155500070129     c                   eval      vsccgi = d09ocgi
155600070129     c                   eval      vsdcgi = d09odes
155700070627     c     udate_8       comp      D09ODSC                            14
155800070320     c                   end
155900070129     c                   end
156000070130     c                   end
156100070124     C*
156200070124     C                   ENDSR
156300070124     C**************************************************************************
156400070124     C* gestione duplica da altro aut.
156500070124     C**************************************************************************
156600070124     C     Srw01         BEGSR
156700070124     C*
156800061229     c                   setoff                                       2829
156900061229     c                   clear                   $msg
157000061229     c                   clear                   w01pdr
157100070111     c                   eval      w01fpp = vfpp
157200061229     c                   movel     vdta          w01dta
157300070123     c                   movel     'S'           w01cgi
157400070627     c                   movel     'N'           w01acc
157500071008     c                   movel     vctp          w01tp
157600061229     c                   do        *hival
157700061229     c*
157800061229     C                   EXFMT     dg02w01
157900061229     C* CMD3 - FINE LAVORO
158000061229     C   Kl              leave
158100061229     c                   exsr      ctrw01
158200061229     c* errore
158300061229     c   90              iter
158400070719     c                   exsr      srcopia
158500061229     c                   leave
158600061229     c                   enddo
158700061229     C*
158800061229     C                   ENDSR
158900061229     C**********************************************************************
159000061229     c     ctrw01        begsr
159100061229     C**********************************************************************
159200070726     C                   SETOFF                                       9058
159300061229     c                   clear                   $msg
159400061229     c                   do
159500061229     C** ricerca
159600070726     C     '?'           SCAN      w01pdr                                 31
159700061229     C                   if        *in31
159800061229     C                   clear                   fnlv24ds
159900061229     C                   MOVEL     tipo          d24tip
160000061229     C                   MOVEL     vpdr          d24fil
160100061229     C                   MOVEL     'R'           d24FLG
160200061229     C                   MOVEL(p)  fnlv24ds      KPJBU
160300061229     C                   CALL      'FNLV24R'
160400061229     C                   PARM                    KPJBA
160500061229     C                   MOVEL     KPJBU         fnlv24ds
160600061229     c                   move      d24pdr        w01pdr
160700070703     c                   seton                                        90
160800061229     c                   end
160900061229     c                   movel     w01pdr        com03             3 0
161000070726     C* Filiale diversa da 0:
161100070726     C     com03         IFne      *ZEROS
161200170615     c*****              select
161300170615     c*****              when      DUTlpo = '2' or DUTlpo = *blanks
161400070726     C* deve essere in £6 se secondo livello
161500170615     C*****com03         LOOKUP    L6                                     45
161600170615     C*****              when      DUTlpo = '1'
161700070726     C* deve essere in £1 se primo livello
161800170615     C*****com03         LOOKUP    L1                                     45
161900170615     C*****              when      DUTlpo = 'S'
162000070726     C* deve essere in azorg01l
162100070726     C     com03         chain     azorg01l
162200061229     c                   if        %found(azorg01l)
162300061229     c                   if        orgfag <> 'A' and orgfag <> 'F'
162400061229     c                   setoff                                       45
162500061229     c                   else
162600061229     c                   seton                                        45
162700061229     c                   end
162800061229     c                   else
162900061229     c                   setoff                                       45
163000061229     c                   end
163100170615     C*****              ENDsl
163200170615
163300170615       //?Filiale in gestione all'utente
163400170615         IF  %lookup(%editc(com03:'X'):pog) = 0;
163500170615           *in45 = *off;
163600170615         ENDIF;
163700170615
163800070726     C* Filiale non presente in tabella
163900070726     C     *IN45         IFEQ      *OFF
164000070726     C                   MOVEL     MSG(12)       $MSG
164100070726     C                   SETON                                        5890
164200070726     C                   leave
164300070726     C                   END
164400070726     C                   END
164500061229     C* CODICE SEMPRE > 0
164600070726     C     w01pdr        IFEQ      *BLANKS
164700070726     C     w01pdr        OREQ      *ZERO
164800061229     C                   MOVEL     MSG(1)        $MSG
164900061229     C                   SETON                                        90
165000061229     C                   leave
165100061229     C                   END
165200061229     C* CONTROLLO VALIDITA' CODICE
165300061229     C                   TESTN                   w01pdr               50
165400070726     C                   MOVE      w01pdr        W001A             1
165500070726     C     *IN50         IFEQ      *OFF
165600070726     C     W001A         ORLT      '0'
165700061229     C                   MOVEL     MSG(2)        $MSG
165800061229     C                   SETON                                        58  90
165900061229     C                   leave
166000070726     C                   END
166100061229     c* controllo che esista un estensione in vigore alla data richiesta
166200061229     c                   move      w01dta        dtaiso
166300061229     c                   move      dtaiso        comiso            8 0
166400061229     c                   move      w01pdr        w01pdrc
166500061229     c     kapdw21       setgt     fiapd22l
166600070202     c     kapdw2        readpe(n) fiapd22l
166700070726     c                   if        (not %eof(fiapd22l) and (apd2atb<>' ' or
166800070726     c                             comiso<apd2dde or comiso>apd2dscad)) or
166900061229     c                             %eof(fiapd22l)
167000061229     C                   MOVEL     MSG(21)       $MSG
167100061229     C                   SETON                                        90
167200061229     C                   leave
167300061229     c                   end
167400061229     c*
167500061229     c                   enddo
167600061229     c*
167700061229     c                   endsr
167800061229     C**************************************************************************
167900070719     C     SRcopia       BEGSR
168000061229     C**************************************************************************
168100070627     C* PULIZIA SUBFILE
168200070627     C                   SETON                                        21
168300070627     C                   WRITE     dg02C01
168400070627     C                   SETOFF                                       2122
168500070627     C                   Z-ADD     0             NRR               4 0
168600070627     c* se scelto di accodare carico prima il vecchio e poi il nuovo
168700070627     C* CARICAMENTO SUBFILE
168800070627     c                   if        w01acc = 'S'
168900070627     c                   eval      flagcopia='1'
169000070627     c                   if        d02file = 'FIDST00F'
169100070718     c* consegne
169200071008     c                   if        vctp = 'C'
169300070718     c                   exsr      cardstc
169400070718     c                   else
169500070718     c* ritiri
169600070718     c                   exsr      cardstr
169700070718     c                   end
169800070627     c                   else
169900070718     c* consegne
170000071008     c                   if        vctp = 'C'
170100070718     c                   exsr      carapdc
170200070718     c                   else
170300070718     c* ritiri
170400070718     c                   exsr      carapdr
170500070627     c                   end
170600070718     c                   end
170700070627     c                   end
170800070726     c                   move      w01dta        dtaiso
170900061229     c                   move      dtaiso        comiso
171000061229     c                   move      w01pdr        w01pdrc
171100070119     c                   eval      comnras= savnras
171200061229     c     kapdw21       setgt     fiapd22l
171300070202     c     kapdw2        readpe(n) fiapd22l                               88
171400061229     c                   if        not *in88 and comiso >= apd2dde and
171500070719     c                             comiso <= apd2dscad and apd2atb = ' '
171600070119     c* se scelto di copiare tutto solo x anagrafico, per distinta obbliga
171700070119     c* solo giri
171800070123     c                   if        w01cgi = 'N'
171900061229     c                   movea     tc1           tc
172000061229     c                   movea     ts1           ts
172100061229     c                   exsr      srdec
172200061229     c                   eval      vftc = apd2ftc
172300061229     c                   eval      vfts = apd2fts
172400061229     c                   eval      vinc = apd2inc
172500070718     c                   eval      vtol = apd2tol
172600070718     c                   eval      vrtol = apd2rtol
172700070718     c                   eval      vrinc = apd2rinc
172800070718     c                   eval      vtpa  = apd2rtpa
172900070718     c                   eval      vspi  = apd2rspi
173000070718     c* consegne
173100071008     c                   if        w01tp = 'C'
173200070718     c                   eval      vnftl= apd2nftl
173300070718     c                   eval      vpkl = apd2pkl
173400070718     c                   eval      vvlu = apd2vlu
173500070718     c                   else
173600070718     c* ritiri
173700070718     c                   eval      vnftl= apd2rnftl
173800070718     c                   eval      vpkl = apd2rpkl
173900070718     c                   eval      vvlu = apd2rvlu
174000070718     c                   end
174100070119     c                   end
174200070119     c                   eval      savnras= apd2nras
174300061229     C* CARICAMENTO SUBFILE
174400070202     c                   eval      flagcopia='1'
174500070718     c* consegne
174600071008     c                   if        w01tp = 'C'
174700070718     c                   exsr      carapdc
174800070718     c                   else
174900070718     c* ritiri
175000070718     c                   exsr      carapdr
175100070718     c                   end
175200070726     c                   end
175300070125     C* carico 100 righe vuote
175400070125     c                   do        100
175500070627     c                   setoff                                       080910
175600070627     c                   setoff                                       1314
175700070129     c                   movel     vfgs          vscfgsg
175800070129     c                   clear                   vsccgi
175900070125     c                   clear                   vsdcgi
176000070119     c                   clear                   vscprgg
176100061229     c                   clear                   vscsql
176200090113     c                   if        nrr < 9999
176300061229     C                   ADD       1             NRR
176400061229     C                   WRITE     dg02s01
176500090113     c                   else
176600090113     c                   leave
176700090113     c                   end
176800061229     c                   enddo
176900061229     c                   seton                                        22
177000061229     C                   Z-ADD     1             REC
177100070119     c                   eval      savnras= comnras
177200061229     C                   ENDSR
177300061229     C**************************************************************************
177400061229     C*    IMMISSIONE/AGGIORNAMENTO/RIATTIVAZIONE
177500061229     C**************************************************************************
177600061229     C     SCRIVI        BEGSR
177700061229     C*
177800061218     c                   move      vpdr          kpdr
177900061222     c                   move      hdde          apd2dde
178000070119     c                   move      hscad         apd2dscad
178100061219     c                   setoff                                       3298
178200061219     c* manutenzione, annullamento, riattivazione
178300070718     c*                  if        not *in42
178400061218     C     KAPD3         CHAIN     fiapd22L                           3298
178500070718     c*                  else
178600070718     c                   if        not %found(fiapd22l)
178700061219     c* inserimento
178800061222     c                   seton                                        32
178900070831     c*                  clear                   apd2nras
179000070831     c                   clear                   fiapd2ds
179100061222     c* prende il numero della distinta del po in gestione
179200061222     C                   CLEAR                   TRUL33DS
179300061222     c                   eval      i33tla = 'L'
179400061222     C                   Z-ADD     200           I33CNU
179500061222     C                   Z-ADD     1             I33NUM
179600061222     c                   movel(p)  trul33ds      kpjbu
179700061222     c                   call      'TRUL33R'
179800061222     c                   parm                    kpjba
179900061222     c                   movel     kpjbu         trul33ds
180000061222     C                   IF        O33ERR = 0
180100070119     C                   Z-ADD     O33Nrf        apd2nras
180200061222     c                   else
180300061222     c                   seton                                        28
180400061222     c                   eval      vmsg = o33msg
180500061222     C                   END
180600061219     c                   end
180700061219     c*
180800061222     C                   IF        not *in98 and not *in28
180900060926     C                   MOVE      ' '           APD2ATB
181000070119     C                   MOVEL     'A'           APD2tipa
181100061218     C                   MOVEL     VPDR          APD2PDR
181200060926     c* consegne particolari
181300060926     c                   movea     tc            tc1
181400060926     c* tipo servizio
181500060926     c                   movea     ts            ts1
181600060926     c*
181700060926     c                   eval      apd2tol = vtol
181800060926     c                   eval      apd2inc = vinc
181900061214     c                   eval      apd2ftc = vftc
182000061214     c                   eval      apd2fts = vfts
182100061214     c                   eval      apd2fpp = vfpp
182200061214     c                   movel     vdde          dtaiso
182300061214     c                   movel     dtaiso        apd2dde
182400070119     c                   movel     vscad         dtaiso
182500070119     c                   movel     dtaiso        apd2dscad
182600070718     c* consegne
182700070718     c                   if        vctp = 'C'
182800061214     c                   eval      apd2pkl= vpkl
182900061214     c                   eval      apd2vlu = vvlu
183000070718     c                   eval      apd2nftl= vnftl
183100070718     c                   else
183200070718     c* ritiri
183300070718     c                   eval      apd2rpkl= vpkl
183400070718     c                   eval      apd2rvlu = vvlu
183500070718     c                   eval      apd2rnftl= vnftl
183600070718     c                   end
183700070718     c                   eval      apd2rtol = vrtol
183800070718     c                   eval      apd2rinc = vrinc
183900070718     c                   eval      apd2rtpa = vtpa
184000070718     c                   eval      apd2rspi = vspi
184100021129     C*
184200060926     C   32              WRITE     fiapd200                             98
184300060926     C  N32              UPDATE    fiapd200                             98
184400061219     c                   if        not *in98
184500070718     c* consegne
184600070718     c                   if        vctp = 'C'
184700070718     c                   exsr      wtrgiric
184800070718     c                   else
184900070718     c* ritiri
185000070718     c                   exsr      wtrgirir
185100061219     C                   End
185200070718     C                   End
185300061219     C                   End
185400061219     C* record allocato
185500061219     c                   if        *in98
185600061218     C                   MOVEL     MSG(4)        VMSG
185700060927     C                   SETON                                        28
185800940809     C                   ENDIF
185900920921     C*
186000920921     C                   ENDSR
186100060929     C**************************************************************************
186200060929     C*    IMMISSIONE/AGGIORNAMENTO/RIATTIVAZIONE
186300060929     C**************************************************************************
186400060929     C     SCRIVI1       BEGSR
186500060929     c                   clear                   fidst000
186600060929     C*
186700070410     C     Kdst          CHAIN     FIDST09L                           3298
186800060929     C     *IN98         IFEQ      *OFF
186900060929     C                   MOVE      ' '           dstATB
187000061218     C                   MOVEL     VPDR          dstPDR
187100070119     c                   eval      dsttipa= d02tipa
187200060929     c* consegne particolari
187300060929     c                   movea     tc            tc1d
187400060929     c* tipo servizio
187500060929     c                   movea     ts            ts1d
187600060929     c*
187700060929     c                   eval      dsttol = vtol
187800060929     c                   eval      dstinc = vinc
187900061219     c                   eval      dstftc = vftc
188000061219     c                   eval      dstfts = vfts
188100060929     c* se non esiste la distinta prendo gli altri dati dal file fiapd00f
188200060929     c                   if        *in32
188300061228     c                   clear                   dstfcf
188400070119     c                   clear                   dstfasd
188500061228     c                   clear                   dstnot
188600080306     c                   eval      dstnpg = d02npg
188700061222     c                   eval      dstfgs = d02fil
188800061213     c                   eval      dstnfv = d02nfv
188900061213     c                   eval      dstdfv = d02dfv
189000061213     c                   eval      dstfpp = d02fpp
189100061213     c                   eval      dsttpm = d02tpm
189200060929     c* piking mattina pomeriggio
189300060929     c                   if        dstfpp = 'M'
189400060929     c                   eval      dstpep = apdpkm
189500060929     c                   else
189600060929     c                   eval      dstpep = apdpkp
189700060929     c                   end
189800060929     c* carico mattina pomeriggio
189900060929     c                   if        dstfpp = 'M'
190000060929     c                   eval      dstfca = apdcam
190100060929     c                   else
190200060929     c                   eval      dstpep = apdcap
190300060929     c                   end
190400060929     c                   end
190500070718     c                   eval      dstrtol = vrtol
190600070718     c                   eval      dstrinc = vrinc
190700070718     c                   eval      dstrtpa = vtpa
190800070718     c                   eval      dstrspi = vspi
190900070718     c* consegne
191000070718     c                   if        vctp = 'C'
191100070718     c                   eval      dstnftl= vnftl
191200061214     c                   eval      dstpkl = vpkl
191300061214     c                   eval      dstvlu = vvlu
191400070718     c                   else
191500070718     c* ritiri
191600070718     c                   eval      dstrnftl= vnftl
191700070718     c                   eval      dstrpkl = vpkl
191800070718     c                   eval      dstrvlu = vvlu
191900070718     c                   end
192000060929     C*
192100060929     C   32              WRITE     fidst000                             98
192200060929     C  N32              UPDATE    fidst000                             98
192300061219     c                   if        not *in98
192400070718     c* consegne
192500070718     c                   if        vctp = 'C'
192600070718     c                   exsr      wtrgiridc
192700070718     c                   else
192800070718     c* ritiri
192900070718     c                   exsr      wtrgiridr
193000061219     C                   End
193100070718     C                   End
193200060929     C                   ENDIF
193300060929     C*
193400060929     C     *IN98         IFEQ      *ON
193500061218     C                   MOVEL     MSG(4)        VMSG
193600060929     C                   SETON                                        28
193700060929     C                   ENDIF
193800060929     C*
193900060929     C                   ENDSR
194000061219     C**************************************************************************
194100070718     C     wtrgiric      BEGSR
194200061219     C**************************************************************************
194300061219     C*
194400061219     c                   if        not *in32
194500061219     C* manutenzione: cancello i giri e li rinserisco
194600070119     c     apd2nras      setll     fiapd31l
194700061219     c                   do        *hival
194800070119     c     apd2nras      reade     fiapd31l
194900061219     c                   if        %eof(fiapd31l)
195000061219     c                   leave
195100061219     c                   end
195200061219     c                   delete    fiapd300
195300061219     c                   enddo
195400061219     c                   end
195500061219     C*
195600061219     c                   clear                   apd3atb
195700070119     c                   eval      apd3nras= apd2nras
195800070119     c                   eval      apd3tipa= apd2tipa
195900061219     c                   eval      apd3pdr = apd2pdr
196000061219     c                   eval      apd3dde = apd2dde
196100070119     c                   eval      apd3dscad= apd2dscad
196200061219     c                   eval      apd3fpp = apd2fpp
196300061219     c*
196400070726     C                   do        *hival        nn                5 0
196500061220     C     nn            chain     dg02S01                            31
196600070726     C   31              leave
196700070123     c                   if        vsccgi<>' '
196800070129     c                   movel     vscfgsg       apd3fgsg
196900070129     c                   movel     vsccgi        apd3cgi
197000070119     c                   eval      apd3prgg= vscprgg
197100061219     c                   eval      apd3sql = vscsql
197200061219     c                   write     fiapd300
197300061219     c                   end
197400070726     C                   ENDDO
197500070723     C* se sono in manutenzione ed
197600070723     C* ho variato le date devo variare anche i giri dei ritiri
197700070723     c                   if        not *in32
197800070723     c     apd2nras      setll     fiapd51l
197900070723     c                   do        *hival
198000070723     c     apd2nras      reade     fiapd51l
198100070723     c                   if        %eof(fiapd51l)
198200070723     c                   leave
198300070723     c                   end
198400070723     c                   eval      apd5dde = apd2dde
198500070723     c                   eval      apd5dscad= apd2dscad
198600070723     c                   update    fiapd500
198700070723     c                   enddo
198800070723     c                   end
198900061219     C                   ENDSR
199000070718     C**************************************************************************
199100070718     C     wtrgirir      BEGSR
199200070718     C**************************************************************************
199300070718     C*
199400070718     c                   if        not *in32
199500070718     C* manutenzione: cancello i giri e li rinserisco
199600070718     c     apd2nras      setll     fiapd51l
199700070718     c                   do        *hival
199800070718     c     apd2nras      reade     fiapd51l
199900070718     c                   if        %eof(fiapd51l)
200000070718     c                   leave
200100070718     c                   end
200200070718     c                   delete    fiapd500
200300070718     c                   enddo
200400070718     c                   end
200500070718     C*
200600070718     c                   clear                   apd5atb
200700070718     c                   eval      apd5nras= apd2nras
200800070718     c                   eval      apd5tipa= apd2tipa
200900070718     c                   eval      apd5pdr = apd2pdr
201000070718     c                   eval      apd5dde = apd2dde
201100070718     c                   eval      apd5dscad= apd2dscad
201200070718     c                   eval      apd5fpp = apd2fpp
201300070718     c*
201400070726     C                   do        *hival        nn                5 0
201500070718     C     nn            chain     dg02S01                            31
201600070726     C   31              leave
201700070718     c                   if        vsccgi<>' '
201800070718     c                   movel     vscfgsg       apd5fgsg
201900070718     c                   movel     vsccgi        apd5cgi
202000070718     c                   eval      apd5prgg= vscprgg
202100070718     c                   eval      apd5sql = vscsql
202200070718     c                   write     fiapd500
202300070718     c                   end
202400070726     C                   ENDDO
202500070723     C* se sono in manutenzione ed
202600070723     C* ho variato le date devo variare anche i giri delle consegne
202700070723     c                   if        not *in32
202800070723     c     apd2nras      setll     fiapd31l
202900070723     c                   do        *hival
203000070723     c     apd2nras      reade     fiapd31l
203100070723     c                   if        %eof(fiapd31l)
203200070723     c                   leave
203300070723     c                   end
203400070723     c                   eval      apd3dde = apd2dde
203500070723     c                   eval      apd3dscad= apd2dscad
203600070723     c                   update    fiapd300
203700070723     c                   enddo
203800070723     c                   end
203900070718     C                   ENDSR
204000061219     C**************************************************************************
204100070718     C     wtrgiridc     BEGSR
204200061219     C**************************************************************************
204300061219     C*
204400061219     c                   if        not *in32
204500061219     C* manutenzione: cancello i giri e li rinserisco
204600061228     c     kdst1         setll     fidst31l
204700061219     c                   do        *hival
204800061228     c     kdst1         reade     fidst31l
204900061219     c                   if        %eof(fidst31l)
205000061219     c                   leave
205100061219     c                   end
205200061219     c                   delete    fidst300
205300061219     c                   enddo
205400061219     c                   end
205500061219     C*
205600061219     c                   clear                   dst3atb
205700061219     c                   eval      dst3fgs = dstfgs
205800061219     c                   eval      dst3npg = dstnpg
205900061219     c                   eval      dst3dfv = dstdfv
206000061219     c                   eval      dst3nfv = dstnfv
206100061219     c*
206200070726     C                   do        *hival        nn                5 0
206300061220     C     nn            chain     dg02S01                            31
206400070726     C   31              leave
206500070123     c                   if        vsccgi<>' '
206600070129     c                   movel     vscfgsg       dst3fgsg
206700070129     c                   movel     vsccgi        dst3cgi
206800070119     c                   eval      dst3prgg= vscprgg
206900061219     c                   eval      dst3sql = vscsql
207000061219     c                   write     fidst300
207100061219     c                   end
207200070726     C                   ENDDO
207300061219     C*
207400061219     C                   ENDSR
207500070718     C**************************************************************************
207600070718     C     wtrgiridr     BEGSR
207700070718     C**************************************************************************
207800070718     C*
207900070718     c                   if        not *in32
208000070718     C* manutenzione: cancello i giri e li rinserisco
208100070718     c     kdst1         setll     fidst51l
208200070718     c                   do        *hival
208300070718     c     kdst1         reade     fidst51l
208400070718     c                   if        %eof(fidst51l)
208500070718     c                   leave
208600070718     c                   end
208700070718     c                   delete    fidst500
208800070718     c                   enddo
208900070718     c                   end
209000070718     C*
209100070718     c                   clear                   dst5atb
209200070718     c                   eval      dst5fgs = dstfgs
209300070718     c                   eval      dst5npg = dstnpg
209400070718     c                   eval      dst5dfv = dstdfv
209500070718     c                   eval      dst5nfv = dstnfv
209600070718     c*
209700070726     C                   do        *hival        nn                5 0
209800070718     C     nn            chain     dg02S01                            31
209900070726     C   31              leave
210000070718     c                   if        vsccgi<>' '
210100070718     c                   movel     vscfgsg       dst5fgsg
210200070718     c                   movel     vsccgi        dst5cgi
210300070718     c                   eval      dst5prgg= vscprgg
210400070718     c                   eval      dst5sql = vscsql
210500070718     c                   write     fidst500
210600070718     c                   end
210700070726     C                   ENDDO
210800070718     C*
210900070718     C                   ENDSR
211000061219     C**************************************************************************
211100061219     C     CX§TAB        BEGSR
211200061219     C**************************************************************************
211300061219     C*
211400061219     c                   MOVE      *BLANKS       KDEST            30
211500061219     C                   CALL      'X§TABER'
211600061219     C                   PARM                    KUTET
211700061219     C                   PARM                    KCODT
211800061219     C                   PARM                    KKEYT
211900061219     C                   PARM                    KDEST
212000061219     C*
212100061219     C                   ENDSR
212200060926      *---------------------------------------------------------------*
212300060926      * Reperimento Dati del job (Utente/Operativi)                   *
212400060926      *---------------------------------------------------------------*
212500060926     c     DatiJob       BEGSR
212600060926      *
212700060926     c     *dtaara       define    §azute        azuteds
212800060926     c     *dtaara       define    §datiute      ddatiute
212900060926      *
213000060926     c                   in(E)     *dtaara
213100060926     c                   IF        %ERROR or RSUT = *blanks
213200060926     c                   clear                   Tibs34Ds
213300060926     c                   call      'TIBS34R'
213400060926     c                   parm                    Tibs34Ds
213500060926     c                   in        *dtaara
213600060926     c                   ENDIF
213700170612
213800170612      // Reperimento delle filiali gestibili dall'utente
213900170615        sav_kpjbu = kpjbu;
214000170615        clear kpjbu;
214100170612        clear TRUL31DS;
214200170612        IF  DUTlpo = '2';
214300170612          I31abi = 'PO';
214400170612        ELSE;
214500170612          I31abi = 'RA';
214600170612        ENDIF;
214700170612        I31cdi = DUTdis;
214800170612        I31car = DUTare;
214900170612        I31cpo = DUTpou;
215000170612        trul31r (kpjba:trul31ds);
215100170612        IF  O31pog <= *zeros;
215200170612          clear POG;
215300170612        ENDIF;
215400170615        kpjbu = sav_kpjbu;
215500060926      *
215600060926     c                   ENDSR
215700940809     C**************************************************************************
215800940809     C*     R O U T I N E     I N I Z I A L E
215900940809     C**************************************************************************
216000940809     C     *INZSR        BEGSR
216100060926     c*
216200060926     C     *ENTRY        PLIST
216300060926     C                   PARM                    KPJBA
216400070510      *
216500070510     C                   TIME                    W0140            14 0
216600070510     C                   MOVE      W0140         UDATE8            8 0
216700060926      * Reperisco dati job
216800060926     c                   exsr      DatiJob
216900070510     c                   move      udate8        dateu8            8 0
217000070510     c                   move      udate8        dtaeur
217100070510     c                   move      dtaeur        dtaiso
217200070510     c                   move      dtaiso        udate_8           8 0
217300060926      *
217400061213     C                   MOVEL     RSUT          VDSA
217500070112     c                   setoff                                       55
217600070312     c* richiamato
217700060927     c                   if        kpjbu <> ' '
217800061213     C                   MOVEL     KPJBU         fidg02ds
217900070119     c                   clear                   d02cmde
218000070119     c                   clear                   d02err
218100070320     c* d02opncmt = S se devo aprire i file sotto commit oppure no
218200070320     c                   if        d02opncmt = 'N'
218300070320     c                   eval      flgcmt = *off
218400070320     c                   else
218500070320     c                   eval      flgcmt = *on
218600070320     c                   end
218700070320     c* visualizzazione
218800070319     c     d02cmd        comp      '05'                                   47
218900070112     c* DISTINTE
219000061213     c                   if        d02file = 'FIDST00F'
219100060929     c                   seton                                        43
219200070718     c                   eval      vctp=d02tp
219300070718     c                   eval      vfgs=d02fil
219400070115     c                   movel     d02pdr        vpdr
219500070115     c                   eval      vnfv=d02nfv
219600061213     c                   eval      vfpp=d02fpp
219700061222     c                   movel     d02dfv        vdfv
219800061222     c                   movel     d02dfv        vdta
219900070410     c                   open      FIDST09l
220000070112     c                   end
220100070112     c*  ANAGRAFICO
220200070112     c                   if        d02file = 'FIAPD20F'
220300070112     c                   seton                                        55
220400070112     c                   movel     d02pdr        vfgs
220500070112     c                   move      d02pdr        vcpdr
220600070112     C                   MOVEL     'M'           Vfpp                           *MATTINO
220700070718     C                   MOVEL     'C'           Vctp                           *MATTINO
220800070112     c                   movel     d02dfv        vdta
220900061011     c                   end
221000060927     c                   else
221100070312     c* da menù
221200070320     c                   eval      flgcmt = *on
221300061213     C                   clear                   fidg02ds
221400070726     C                   MOVEL(p)  '?'           VCPdr
221500070726     C                   MOVE      dutpou        Vfgs
221600070112     C                   MOVEL     'M'           Vfpp                           *MATTINO
221700070718     C                   MOVEL     'C'           Vctp                           *MATTINO
221800070510     c                   movel     udate_8       vdta
221900060927     c                   end
222000070320     c                   open      fiapd22l
222100070320     c                   open      fiapd31l
222200070627     c                   open      fidst31l
222300070718     c                   open      fiapd51l
222400070718     c                   open      fidst51l
222500070312     c* organigramma
222600070312     c     vfgs          chain     azorg01l
222700070312     c                   if        %found(azorg01l)
222800070312     c                   movel     orgdes        vdfgs
222900070718     C                   MOVEL     ORGde8        og148
223000070718     C                   MOVEL     ORGde7        og147
223100070312     c                   end
223200070723     c* abilito l'utilizzo del pgm solo se flag geo consegne o ritiro <> ' '
223300070723     c                   if        (§OGTLLS =' ' and §OGCGIO = ' ') or
223400070723     c                             (§OGTLLS =' ' and vctp = 'C') or
223500070723     c                             (§OGcgio =' ' and vctp = 'R')
223600070718     C                   EVAL      $command = cmd(1)
223700070718     C                   MOVEA     cmd(1)        DSComando
223800070718     C                   MOVEL     Job           DJob
223900070718     C                   EVAL      $command = DsComando
224000070718     C                   CALL      'QCMDEXC'
224100070718     C                   PARM                    $command
224200070718     C                   PARM                    $lenght
224300070718     c                   seton                                        lr
224400070718     c                   return
224500070718     c                   end
224600070928     c     §OGCGIO       comp      ' '                                    51
224700060928     C* CARICO LA TABELLA '£1'
224800060928     C                   MOVE      '£1'          D06COD
224900060928     C                   MOVEL     SIMFEL        D06KEY
225000060928     C                   MOVEL     DSUL06        KPJBU
225100060928     C                   CALL      'TRUL06R'
225200060928     C                   PARM                    KPJBA
225300060928     C                   MOVEL     KPJBU         DSUL06
225400060928     C                   MOVEA     LIN           L1
225500070726     C* CARICO LA TABELLA '£6' SE FILIALE DI SECONDO LIVELLO
225600060928     c                   if            DUTlpo = '2'
225700060928     c                             or  DUTlpo = *blanks
225800070726     C                   MOVE      '£6'          D06COD
225900070726     c                   movel     DUTpou        D06KEY
226000070726     C                   MOVEL     DSUL06        KPJBU
226100070726     C                   CALL      'TRUL06R'
226200070726     C                   PARM                    KPJBA
226300070726     C                   MOVEL     KPJBU         DSUL06
226400070726     C                   MOVEA     LIN           L6
226500070726     C                   END
226600940809     C*
226700940809     C     *LIKE         DEFINE    APDPDR        KPDR
226800940909     C*
226900021129     C     Kapd          KLIST
227000060927     C                   KFLD                    tipo              1
227100060927     C                   KFLD                    kpdr              7 0
227200060927     c                   eval      tipo = 'A'
227300061218     C     Kapd21        KLIST
227400061218     C                   KFLD                    tipo              1
227500061218     C                   KFLD                    kpdr              7 0
227600061218     C                   KFLD                    vfpp
227700061218     C                   KFLD                    comiso
227800061229     C     Kapdw21       KLIST
227900061229     C                   KFLD                    tipo              1
228000061229     C                   KFLD                    w01pdrc           7 0
228100061229     C                   KFLD                    w01fpp
228200061229     C                   KFLD                    comiso
228300061218     C     Kapd3         KLIST
228400061218     C                   KFLD                    tipo              1
228500061218     C                   KFLD                    kpdr              7 0
228600061218     C                   KFLD                    vfpp
228700061218     C                   KFLD                    apd2dde
228800070119     C                   KFLD                    apd2dscad
228900061218     C     Kapd2         KLIST
229000061218     C                   KFLD                    tipo              1
229100061218     C                   KFLD                    kpdr              7 0
229200061218     C                   KFLD                    vfpp
229300061229     C     Kapdw2        KLIST
229400061229     C                   KFLD                    tipo              1
229500061229     C                   KFLD                    w01pdrc
229600061229     C                   KFLD                    w01fpp
229700060929     C*
229800061228     C     Kdst          KLIST
229900070410     c                   KFLD                    d02fil
230000070319     C                   KFLD                    d02npg
230100061228     C                   KFLD                    d02nfv
230200061229     C*
230300061229     C     Kdst4         KLIST
230400061229     C                   KFLD                    tipo
230500061229     c                   KFLD                    kpdr
230600061229     c                   KFLD                    dstfcf
230700061228     C*
230800061228     C     Kdst1         KLIST
230900061228     C                   KFLD                    d02fil
231000061228     C                   KFLD                    d02nfv
231100070627     C     Kdst31        KLIST
231200070627     C                   KFLD                    dstfgs
231300070627     C                   KFLD                    dstnfv
231400010207     C*
231500010207     C     KTABEL        KLIST
231600010207     C                   KFLD                    KUTET
231700061218     C                   KFLD                    KCODT             2
231800061218     C                   KFLD                    KKEYT             8
231900010207     C*
232000010207     C                   Z-ADD     1             KUTET             1 0
232100000808     C*
232200940809     C                   ENDSR
232300940809     C**************************************************************************
232400940809**
232500021129Campo obbligatorio: immettere un codice o '?' per avere l'elenco               1
232600021129Immettere un codice valido                                                     2
232700061220Inserire tutti i campi obbligatori                                             3
232800000726Errori verificatisi nell'aggiornamento: RIPROVARE O AVVISARE IL CED (SEDE)     4
232900060927Consegna particolare errata                                                    5
233000060927Tipo servizio errato                                                           6
233100060927Priorità obbligatoria                                                          7
233200061220Priorità + sequenza già inserite                                               8
233300061220E' neccessario inserire almeno un giro                                         9
233400060927Giro già inserito                                                             10
233500061220Data decorrenza < di data scadenza                                            11
233600940909Codice filiale errata o non gestita                                           12
233700060927Consegna particolare già inserita                                             13
233800021129Immettere filiale valida o '?' nel codice l'elenco                            14
233900061222Esiste già un'anagrafica nel periodo scelto.                                  15
234000061220Sequenza obbligatoria                                                         16
234100070419Impossibile annullare l'anagrafica. Esistono distinte aperte.                 17
234200060927Tipo servizio già inserito                                                    18
234300060928Consegna particolare errata o non utilizzabile                                19
234400060928% sfrido obbligatoria                                                         20
234500061220Non esistono anagrafiche in vigore con questi riferimenti F10 x inserirla     21
234600070320Anagrafico autotrasportatore annullato. Occorre prima riattivarlo             22
234700070119Max fermate TELLUS obbligatorio                                               23
234800070119L'autotrasportatore non gestisce i giri                                       24
234900070320Codice giro non inseribile                                                    25
235000070718Tipo pianificazione non abilitato                                             26
235100070718ATTENZIONE: si sta variando la validità della pianificazione consegne E ritiri27
235200071126La % di tolleranza fa eccedere la capienza del campo pesi/volumi/fermate      28
235300070502**   CMD - Comandi
235400070926SNDBRKMSG MSG('Funzione non utilizzabile                            ') TOMSGQ(XXXXXXXXXX)
