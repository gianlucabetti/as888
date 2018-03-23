000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400990113     Fanclp01l  if   e           k disk
000500990113     Ftmclp00f  o    e             disk    prefix(a)
000600990113     Fanamp01l  if   e           k disk
000700990113     Ftmamp00f  o    e             disk    prefix(a)
000800990208     Ftmamp01l  if   e           k disk    rename(tmamp000:tm01) prefix(§)
000900990113     Fanfop01l  if   e           k disk
001000990113     Ftmfop00f  o    e             disk    prefix(a)
001100990113     Fofcnf01l  if   e           k disk
001200990113     Ftmcnf00f  o    e             disk    prefix(a)
001300990113     Fanlmp01l  if   e           k disk
001400990113     Ftmlmp00f  o    e             disk    prefix(a)
001500990113     Fmpmmp01l  if   e           k disk
001600990113     Ftmmmp00f  o    e             disk    prefix(a)
001700990114     focpil00f  if   e           k disk    prefix(c:1)
001800990113     Ftmpilocf  o    e             disk    prefix(c)
001900990114     fofpil00f  if   e           k disk    prefix(f:1)
002000990113     Ftmpiloff  o    e             disk    prefix(f)
002100990114     fmppil00f  if   e           k disk    prefix(m:1)
002200990113     Ftmpil00f  o    e             disk    prefix(m)
002300990114     Focscm01l  if   e           k disk
002400990114     Ftmscm00f  o    e             disk    prefix(a)
002500990114     Fofsfc01l  if   e           k disk
002600990114     Ftmsfc00f  o    e             disk    prefix(a)
002700990114     Fmpsmp01l  if   e           k disk
002800990114     Ftmsmp00f  o    e             disk    prefix(a)
002900990114     Focmoc01l  if   e           k disk
003000990114     Ftmmoc00f  o    e             disk    prefix(a)
003100990114     Focsoc01l  if   e           k disk    rename(ocmoc000:ocms) prefix(b:1)
003200990114     Ftmsoc00f  o    e             disk    prefix(b)
003300990114     Fofmof01l  if   e           k disk
003400990114     Ftmmof00f  o    e             disk    prefix(a)
003500990114     Fofsof01l  if   e           k disk    rename(ofmof000:ofmf) prefix(b:1)
003600990114     Ftmsof00f  o    e             disk    prefix(b)
003700990113     Ddataiso          s               d   datfmt(*iso)
003800990113     Ddataeur          s               d   datfmt(*eur)
003900000000     C*---------------------------------------------------------------*
004000990114      *conversioni nei tracciati nuovi
004100990113     c                   exsr      tmclp
004200990114     c                   exsr      tmfop
004300990113     c                   exsr      tmamp
004400990113     c                   exsr      tmcnf
004500990113     c                   exsr      tmlmp
004600990113     c                   exsr      tmmmp
004700990113     c                   exsr      tmpilc
004800990113     c                   exsr      tmpilf
004900990113     c                   exsr      tmpilm
005000990114     c                   exsr      tmscm
005100990114     c                   exsr      tmsfc
005200990114     c                   exsr      tmsmp
005300990114     c                   exsr      tmmoc
005400990114     c                   exsr      tmsoc
005500990114     c                   exsr      tmmof
005600990114     c                   exsr      tmsof
005700990113     c                   seton                                        lr
005800990113     c*-------------------------------------------------
005900990113     c     tmclp         begsr
006000990113     c*-------------------------------------------------
006100990113     c                   setoff                                           99
006200990113     c     *loval        setll     anclp01l
006300981029     c                   do        *hival
006400990113     c                   read      anclp01l                               99
006500981029     c   99              leave
006600990113     c                   move      clpatt        aclpatt
006700990113     c                   move      clpute        aclpute
006800990113     c                   move      '000151'      aclpkcc
006900990113     c                   move      clpksc        aclpksc
007000990113     c                   movel     0             aclpksc
007100990113     c                   move      clprsi        aclprsi
007200990113     c                   move      clpgpm        aclpgpm
007300990113     c                   move      clpstm        aclpstm
007400990414     c                   movel     clpcdf        aclpcdf
007500990113     c                   move      clpsc1        aclpsc1
007600990113     c                   move      clpsc2        aclpsc2
007700990113     c                   move      clplis        aclplis
007800990311     c                   move      'ITL '        aclpval
007900990113     c                   move      clpinf        aclpinf
008000990113     c                   move      clpres        aclpres
008100990113     c                   move      clpspe        aclpspe
008200990113     c                   move      clpimb        aclpimb
008300990113     c                   move      clppcl        aclppcl
008400990113     c                   move      clpqcl        aclpqcl
008500990113     c                   write     tmclp000
008600981029     c                   enddo
008700990113     c                   endsr
008800990113     c*-------------------------------------------------
008900990114     c     tmfop         begsr
009000990113     c*-------------------------------------------------
009100990113     c                   setoff                                           99
009200990113     c     *loval        setll     anfop01l
009300990113     c                   do        *hival
009400990113     c                   read      anfop01l                               99
009500990113     c   99              leave
009600990113     c                   move      fopatt        afopatt
009700990113     c                   move      fopute        afopute
009800990114     c                   move      '000400'      afopcfo
009900990113     c                   move      fopfor        afopfor
010000990114     c                   movel     '0'           afopfor
010100990113     c                   move      foprsi        afoprsi
010200990113     c                   move      fopgpm        afopgpm
010300990113     c                   move      fopstm        afopstm
010400990113     c                   move      fopsc1        afopsc1
010500990113     c                   move      fopsc2        afopsc2
010600990113     c                   move      foplis        afoplis
010700990113     c                   move      fopapp        afopapp
010800990113     c                   move      fopdcc        afopdcc
010900990113     c                   move      fopqfo        afopqfo
011000990311     c                   move      'ITL '        afopval
011100990113     c                   move      fopinf        afopinf
011200990113     c                   write     tmfop000
011300990113     c                   enddo
011400990113     c                   endsr
011500990113     c*-------------------------------------------------
011600990113     c     tmamp         begsr
011700990113     c*-------------------------------------------------
011800990113     c                   setoff                                           99
011900990113     c     *loval        setll     anamp01l
012000990113     c                   do        *hival
012100990113     c                   read      anamp01l                               99
012200990113     c   99              leave
012300990208     c                   if        ampscm = '79   '
012400990208     c                   iter
012500990208     c                   end
012600990113     c                   move      ampatt        aampatt
012700990113     c                   move      ampute        aampute
012800990113     c                   z-add     ampdig        aampdig
012900990113     c                   if        ampdig > 0
013000990113     c     *ymd          move      ampdig        dataiso
013100990113     c                   move      dataiso       aampdig
013200990113     c                   endif
013300990113     c                   z-add     ampdfg        aampdfg
013400990113     c                   if        ampdfg > 0
013500990113     c     *ymd          move      ampdfg        dataiso
013600990113     c                   move      dataiso       aampdfg
013700990113     c                   endif
013800990113     c                   move      ampclm        aampclm
013900990113     c                   move      ampuma        aampuma
014000990113     c                   move      ampmat        aampmat
014100990113     c                   move      ampdmt        aampdmt
014200990113     c                   move      ampscm        aampscm
014300990113     c                   move      ampamt        aampamt
014400990113     c                   move      ampgap        aampgap
014500990113     c                   move      ampumq        aampumq
014600990113     c                   move      ampums        aampums
014700990113     c                   move      ampumv        aampumv
014800990113     c                   z-add     ampfcq        aampfcq
014900990113     c                   z-add     ampfcs        aampfcs
015000990113     c                   z-add     ampfcv        aampfcv
015100990113     c                   move      ampctf        aampctf
015200990113     c                   z-add     ampaiv        aampaiv
015300990113     c                   move      *blank        aampesv
015400990113     c                   move      amplis        aamplis
015500990113     c                   z-add     ampaia        aampaia
015600990113     c                   move      *blank        aampesa
015700990113     c                   z-add     amplro        aamplro
015800990113     c                   z-add     amplvm        aamplvm
015900990113     c                   z-add     amplec        aamplec
016000990113     c                   z-add     amppdc        aamppdc
016100990113     c                   z-add     amptrp        aamptrp
016200990113     c                   z-add     ampcca        aampcca
016300990113     c                   z-add     amppnt        aamppnt
016400990113     c                   move      ampimb        aampimb
016500990113     c                   move      ampmag        aampmag
016600990113     c                   move      ampubi        aampubi
016700990113     c                   move      *blank        aampfil
016800990113     c                   z-add     amptar        ww                6 0
016900990113     c                   if        ww  > 0
017000990113     c     *dmy          move      ww            dataeur
017100990113     c                   move      dataeur       dataiso
017200990113     c                   move      dataiso       aamptar
017300990113     c                   endif
017400990113     c                   write     tmamp000
017500990113     c                   enddo
017600990113     c                   endsr
017700990113     c*-------------------------------------------------
017800990113     c     tmcnf         begsr
017900990113     c*-------------------------------------------------
018000990113     c                   setoff                                           99
018100990113     c     *loval        setll     ofcnf01l
018200990113     c                   do        *hival
018300990113     c                   read      ofcnf01l                               99
018400990113     c   99              leave
018500990113     c                   move      cnfute        acnfute
018600990113     c                   z-add     cnfnor        acnfnor
018700990113     c                   z-add     cnfc01        acnfc01
018800990113     c                   z-add     cnfc02        acnfc02
018900990113     c                   z-add     cnfc03        acnfc03
019000990113     c                   z-add     cnfc04        acnfc04
019100990113     c                   z-add     cnfc05        acnfc05
019200990113     c                   z-add     cnfdor        acnfdor
019300990114     c                   setoff                                       6667
019400990113     c                   if        cnfdor > 0
019500990114     c     *ymd          test(d)                 cnfdor                 66
019600990114     c  n66*ymd          move      cnfdor        dataiso
019700990114     c  n66              move      dataiso       acnfdor
019800990114     c                   if        *in66
019900990114     c                   setoff                                       66
020000990114     c     *dmy          test(d)                 cnfdor                 67
020100990114     c  n67*dmy          move      cnfdor        dataeur
020200990114     c  n67              move      dataeur       dataiso
020300990114     c  n67              move      dataiso       acnfdor
020400990114     c                   endif
020500990113     c                   end
020600990113     c                   write     tmcnf000
020700990113     c                   enddo
020800990113     c                   endsr
020900990113     c*-------------------------------------------------
021000990113     c     tmlmp         begsr
021100990113     c*-------------------------------------------------
021200990113     c                   setoff                                           99
021300990113     c     *loval        setll     anlmp01l
021400990113     c                   do        *hival
021500990113     c                   read      anlmp01l                               99
021600990113     c   99              leave
021700990208     c     lmpmat        chain     tmamp01l                           01
021800990208     c   01              iter
021900990113     c                   move      lmpute        almpute
022000990113     c                   move      lmpatt        almpatt
022100990113     c                   move      lmplis        almplis
022200990113     c                   move      lmpmat        almpmat
022300990113     c                   move      lmpflg        almpflg
022400990311     c                   move      'ITL '        almpval
022500990113     c                   z-add     lmpvp1        almpvp1
022600990113     c                   z-add     lmpvp2        almpvp2
022700990113     c                   z-add     lmpvp3        almpvp3
022800990113     c                   z-add     lmpvp4        almpvp4
022900990113     c                   z-add     lmpvp5        almpvp5
023000990113     c                   z-add     lmpdig        almpdig
023100990113     c                   if        lmpdig > 0
023200990113     c     *ymd          move      lmpdig        dataiso
023300990113     c                   move      dataiso       almpdig
023400990113     c                   end
023500990113     c                   z-add     lmpdfg        almpdfg
023600990113     c                   if        lmpdfg > 0
023700990113     c     *ymd          move      lmpdfg        dataiso
023800990113     c                   move      dataiso       almpdfg
023900990113     c                   end
024000990113     c                   write     tmlmp000
024100990113     c                   enddo
024200990113     c                   endsr
024300990113     c*-------------------------------------------------
024400990113     c     tmmmp         begsr
024500990113     c*-------------------------------------------------
024600990113     c                   setoff                                           99
024700990407     c     98            setll     mpmmp01l
024800990113     c                   do        *hival
024900990113     c                   read      mpmmp01l                               99
025000990113     c   99              leave
025100990208     c     mmpmat        chain     tmamp01l                           01
025200990208     c   01              iter
025300990113     c                   move      mmpute        ammpute
025400990113     c                   move      mmpatt        ammpatt
025500990113     c                   move      mmpmag        ammpmag
025600990113     c                   move      mmpcau        ammpcau
025700990113     c                   move      mmpopc        ammpopc
025800990407     c                   movel     '500'         ammpcdc
025900990113     c                   move      mmpmat        ammpmat
026000990113     c                   move      mmpuma        ammpuma
026100990113     c                   move      mmpmcp        ammpmcp
026200990113     c                   move      mmpfgq        ammpfgq
026300990113     c                   move      mmpfgv        ammpfgv
026400990113     c                   move      mmpfgt        ammpfgt
026500990113     c                   move      mmpflg        ammpflg
026600990311     c                   move      'ITL '        ammpval
026700990311     c                   z-add     1             ammpcmb
026800990113     c                   z-add     mmpaac        ammpaac
026900990113     c                   add       1900          ammpaac
027000990113     c                   z-add     mmpcrg        ammpcrg
027100990113     c                   z-add     mmpprz        ammpprz
027200990113     c                   z-add     mmpqta        ammpqta
027300990113     c                   z-add     mmpndc        ammpndc
027400990113     c                   z-add     mmpnmv        ammpnmv
027500990113     c                   z-add     mmpdmn        ammpdmn
027600990113     c                   if        mmpdmn > 0
027700990113     c     *ymd          move      mmpdmn        dataiso
027800990113     c                   move      dataiso       ammpdmn
027900990113     c                   end
028000990113     c                   z-add     mmpdmv        ammpdmv
028100990113     c                   if        mmpdmv > 0
028200990113     c     *ymd          move      mmpdmv        dataiso
028300990113     c                   move      dataiso       ammpdmv
028400990113     c                   end
028500990113     c                   z-add     mmpddc        ammpddc
028600990113     c                   if        mmpddc > 0
028700990113     c     *ymd          move      mmpddc        dataiso
028800990113     c                   move      dataiso       ammpddc
028900990113     c                   end
029000990113     c                   write     tmmmp000
029100990113     c                   enddo
029200990113     c                   endsr
029300990113     c*-------------------------------------------------
029400990113     c     tmpilc        begsr
029500990113     c*-------------------------------------------------
029600990113     c                   setoff                                           99
029700990113     c     *loval        setll     ocpil00f
029800990113     c                   do        *hival
029900990113     c                   read      ocpil00f                               99
030000990113     c   99              leave
030100990113     c                   move      cilk01        cpilk01
030200990113     c                   move      cilk02        cpilk02
030300990113     c                   move      cilk03        cpilk03
030400990113     c                   move      cilk04        cpilk04
030500990113     c                   move      cilk05        cpilk05
030600990113     c                   move      cilk06        cpilk06
030700990113     c                   move      cilfil        cpilfil
030800990113     c                   write     tmpilc00
030900990113     c                   enddo
031000990113     c                   endsr
031100990113     c*-------------------------------------------------
031200990113     c     tmpilf        begsr
031300990113     c*-------------------------------------------------
031400990113     c                   setoff                                           99
031500990113     c     *loval        setll     ofpil00f
031600990113     c                   do        *hival
031700990113     c                   read      ofpil00f                               99
031800990113     c   99              leave
031900990113     c                   move      filk01        fpilk01
032000990113     c                   move      filk02        fpilk02
032100990113     c                   move      filk03        fpilk03
032200990113     c                   move      filk04        fpilk04
032300990113     c                   move      filk05        fpilk05
032400990113     c                   move      filk06        fpilk06
032500990113     c                   move      filfil        fpilfil
032600990113     c                   write     tmpilf00
032700990113     c                   enddo
032800990113     c                   endsr
032900990113     c*-------------------------------------------------
033000990113     c     tmpilm        begsr
033100990113     c*-------------------------------------------------
033200990113     c                   setoff                                           99
033300990113     c     *loval        setll     mppil00f
033400990113     c                   do        *hival
033500990113     c                   read      mppil00f                               99
033600990113     c   99              leave
033700990113     c                   move      milk01        mpilk01
033800990113     c                   move      milk02        mpilk02
033900990113     c                   move      milk03        mpilk03
034000990113     c                   move      milk04        mpilk04
034100990113     c                   move      milfil        mpilfil
034200990113     c                   write     tmpil000
034300990113     c                   enddo
034400990113     c                   endsr
034500990114     c*-------------------------------------------------
034600990114     c     tmscm         begsr
034700990114     c*-------------------------------------------------
034800990114     c                   setoff                                           99
034900990114     c     *loval        setll     ocscm01l
035000990114     c                   do        *hival
035100990114     c                   read      ocscm01l                               99
035200990114     c   99              leave
035300990114     c                   move      sccatt        asccatt
035400990114     c                   move      sccute        asccute
035500990114     c                   move      scccli        ascccli
035600990114     c                   movel     '0'           ascccli
035700990114     c                   move      sccclm        asccclm
035800990114     c                   move      sccsc1        asccsc1
035900990114     c                   move      sccsc2        asccsc2
036000990114     c                   write     tmscm000
036100990114     c                   enddo
036200990114     c                   endsr
036300990114     c*-------------------------------------------------
036400990114     c     tmsfc         begsr
036500990114     c*-------------------------------------------------
036600990114     c                   setoff                                           99
036700990114     c     *loval        setll     ofsfc01l
036800990114     c                   do        *hival
036900990114     c                   read      ofsfc01l                               99
037000990114     c   99              leave
037100990114     c                   move      sfcatt        asfcatt
037200990114     c                   move      sfcute        asfcute
037300990114     c                   move      sfcfor        asfcfor
037400990114     c                   movel     '0'           asfcfor
037500990114     c                   move      sfcclm        asfcclm
037600990114     c                   move      sfcsc1        asfcsc1
037700990114     c                   move      sfcsc2        asfcsc2
037800990114     c                   write     tmsfc000
037900990114     c                   enddo
038000990114     c                   endsr
038100990114     c*-------------------------------------------------
038200990114     c     tmsmp         begsr
038300990114     c*-------------------------------------------------
038400990114     c                   setoff                                           99
038500990407     c     98            setll     mpsmp01l
038600990114     c                   do        *hival
038700990114     c                   read      mpsmp01l                               99
038800990114     c   99              leave
038900990208     c     smpmat        chain     tmamp01l                           01
039000990208     c   01              iter
039100990114     c                   move      smpute        asmpute
039200990114     c                   move      smpatt        asmpatt
039300990114     c                   move      smpmag        asmpmag
039400990114     c                   move      smpmat        asmpmat
039500990114     c                   move      smpuma        asmpuma
039600990114     c                   move      smpflg        asmpflg
039700990311     c                   move      'ITL '        asmpval
039800990311     c                   z-add     1             asmpcmb
039900990114     c                   z-add     smpaac        asmpaac
040000990114     c                   add       1900          asmpaac
040100990114     c                   z-add     smpplf        asmpplf
040200990114     c                   z-add     smpplv        asmpplv
040300990114     c                   z-add     smpqgi        asmpqgi
040400990114     c                   z-add     smpqor        asmpqor
040500990114     c                   z-add     smpqpr        asmpqpr
040600990114     c                   z-add     smpqim        asmpqim
040700990114     c                   z-add     smpqip        asmpqip
040800990114     c                   z-add     smpqgf        asmpqgf
040900990114     c                   z-add     smpqe1        asmpqe1
041000990114     c                   z-add     smpqe2        asmpqe2
041100990114     c                   z-add     smpqe3        asmpqe3
041200990114     c                   z-add     smpqe4        asmpqe4
041300990114     c                   z-add     smpqe5        asmpqe5
041400990114     c                   z-add     smpqu1        asmpqu1
041500990114     c                   z-add     smpqu2        asmpqu2
041600990114     c                   z-add     smpqu3        asmpqu3
041700990114     c                   z-add     smpqu4        asmpqu4
041800990114     c                   z-add     smpqu5        asmpqu5
041900990114     c                   z-add     smpnms        asmpnms
042000990114     c                   z-add     smppmc        asmppmc
042100990114     c                   z-add     smpdmn        asmpdmn
042200990114     c                   if        smpdmn > 0
042300990114     c     *ymd          move      smpdmn        dataiso
042400990114     c                   move      dataiso       asmpdmn
042500990114     c                   end
042600990114     c                   z-add     smpdms        asmpdms
042700990114     c                   if        smpdms > 0
042800990114     c     *ymd          move      smpdms        dataiso
042900990114     c                   move      dataiso       asmpdms
043000990114     c                   end
043100990114     c                   write     tmsmp000
043200990114     c                   enddo
043300990114     c                   endsr
043400990114     c*-------------------------------------------------
043500990114     c     tmmoc         begsr
043600990114     c*-------------------------------------------------
043700990114     c                   setoff                                           99
043800990114     c     *loval        setll     ocmoc01l
043900990114     c                   do        *hival
044000990114     c                   read      ocmoc01l                               99
044100990114     c   99              leave
044200990208     c     mocmat        chain     tmamp01l                           01
044300990208     c   01              iter
044400990414     c     mocdor        comp      980101                               01
044500990414     c   01              iter
044600990114     c                   move      mocute        amocute
044700990114     c                   move      mocatt        amocatt
044800990114     c                   move      moccli        amoccli
044900990126     c                   movel     '0'           amoccli
045000990114     c                   move      moctor        amoctor
045100990114     c                   move      moctro        amoctro
045200990114     c                   move      moclis        amoclis
045300990311     c                   move      'ITL '        amocval
045400990114     c                   move      mocspe        amocspe
045500990114     c                   move      mocluo        amocluo
045600990114     c                   move      mocres        amocres
045700990114     c                   move      mocimb        amocimb
045800990414     c                   movel     moccpg        amoccpg
045900990114     c                   move      *blank        amocesv
046000990114     c                   move      mocfes        amocfes
046100990114     c                   move      moctrg        amoctrg
046200990114     c                   move      mocfas        amocfas
046300990114     c                   move      moctsr        amoctsr
046400990114     c                   move      mocmat        amocmat
046500990114     c                   move      mocdmt        amocdmt
046600990114     c                   move      mocumq        amocumq
046700990114     c                   move      moccom        amoccom
046800990114     c                   move      mocmag        amocmag
046900990407     c                   movel     '500'         amoccdc
047000990114     c                   move      mocpcl        amocpcl
047100990114     c                   move      mocsos        amocsos
047200990114     c                   move      mocgpm        amocgpm
047300990114     c                   move      mocstm        amocstm
047400990114     c                   move      mocfls        amocfls
047500990114     c                   move      mocflg        amocflg
047600990114     c                   z-add     mocnor        amocnor
047700990311     c                   z-add     1             amoccmb
047800990114     c                   z-add     mocaiv        amocaiv
047900990114     c                   z-add     mocqta        amocqta
048000990114     c                   z-add     mocprz        amocprz
048100990114     c                   z-add     mocsc1        amocsc1
048200990114     c                   z-add     mocsc2        amocsc2
048300990114     c                   z-add     mocsc3        amocsc3
048400990114     c                   z-add     mocnbl        amocnbl
048500990114     c                   z-add     mocnft        amocnft
048600990114     c                   z-add     moccrg        amoccrg
048700990114     c                   z-add     mocnrf        amocnrf
048800990114     c                   z-add     mocnpc        amocnpc
048900990114     c                   z-add     mocnpn        amocnpn
049000990114     c                   z-add     mocass        amocass
049100990114     c                   z-add     mocclp        amocclp
049200990114     c                   z-add     mocdmn        amocdmn
049300990114     c                   if        mocdmn > 0
049400990114     c     *ymd          move      mocdmn        dataiso
049500990114     c                   move      dataiso       amocdmn
049600990114     c                   end
049700990114     c                   z-add     mocdor        amocdor
049800990114     c                   if        mocdor > 0
049900990114     c     *ymd          move      mocdor        dataiso
050000990114     c                   move      dataiso       amocdor
050100990114     c                   end
050200990114     c                   z-add     mocdcr        amocdcr
050300990114     c                   if        mocdcr > 0
050400990114     c     *ymd          move      mocdcr        dataiso
050500990114     c                   move      dataiso       amocdcr
050600990114     c                   end
050700990114     c                   z-add     mocdcp        amocdcp
050800990114     c                   if        mocdcp > 0
050900990114     c     *ymd          move      mocdcp        dataiso
051000990114     c                   move      dataiso       amocdcp
051100990114     c                   end
051200990114     c                   z-add     mocdbl        amocdbl
051300990114     c                   if        mocdbl > 0
051400990114     c     *ymd          move      mocdbl        dataiso
051500990114     c                   move      dataiso       amocdbl
051600990114     c                   end
051700990114     c                   z-add     mocdft        amocdft
051800990114     c                   if        mocdft > 0
051900990114     c     *ymd          move      mocdft        dataiso
052000990114     c                   move      dataiso       amocdft
052100990114     c                   end
052200990114     c                   z-add     mocdrf        amocdrf
052300990114     c                   if        mocdrf > 0
052400990114     c     *ymd          move      mocdrf        dataiso
052500990114     c                   move      dataiso       amocdrf
052600990114     c                   end
052700990114     c                   z-add     mocdpc        amocdpc
052800990114     c                   if        mocdpc > 0
052900990114     c     *ymd          move      mocdpc        dataiso
053000990114     c                   move      dataiso       amocdpc
053100990114     c                   end
053200990114     c                   write     tmmoc000
053300990114     c                   enddo
053400990114     c                   endsr
053500990114     c*-------------------------------------------------
053600990114     c     tmsoc         begsr
053700990114     c*-------------------------------------------------
053800990114     c                   setoff                                           99
053900990114     c     *loval        setll     ocsoc01l
054000990114     c                   do        *hival
054100990114     c                   read      ocsoc01l                               99
054200990114     c   99              leave
054300990208     c     bocmat        chain     tmamp01l                           01
054400990208     c   01              iter
054500990414     c     bocdor        comp      980101                               01
054600990903     c  n01bocdor        comp      990101                             01  01
054700990414     c   01              iter
054800990114     c                   move      bocute        bmocute
054900990114     c                   move      bocatt        bmocatt
055000990114     c                   move      boccli        bmoccli
055100990202     c                   movel     '0'           bmoccli
055200990114     c                   move      boctor        bmoctor
055300990114     c                   move      boctro        bmoctro
055400990114     c                   move      boclis        bmoclis
055500990311     c                   move      'ITL '        bmocval
055600990114     c                   move      bocspe        bmocspe
055700990114     c                   move      bocluo        bmocluo
055800990114     c                   move      bocres        bmocres
055900990114     c                   move      bocimb        bmocimb
056000990414     c                   movel     boccpg        bmoccpg
056100990114     c                   move      *blank        bmocesv
056200990114     c                   move      bocfes        bmocfes
056300990114     c                   move      boctrg        bmoctrg
056400990114     c                   move      bocfas        bmocfas
056500990114     c                   move      boctsr        bmoctsr
056600990114     c                   move      bocmat        bmocmat
056700990114     c                   move      bocdmt        bmocdmt
056800990114     c                   move      bocumq        bmocumq
056900990114     c                   move      boccom        bmoccom
057000990114     c                   move      bocmag        bmocmag
057100990407     c                   movel     '500'         bmoccdc
057200990114     c                   move      bocpcl        bmocpcl
057300990114     c                   move      bocsos        bmocsos
057400990114     c                   move      bocgpm        bmocgpm
057500990114     c                   move      bocstm        bmocstm
057600990114     c                   move      bocfls        bmocfls
057700990114     c                   move      bocflg        bmocflg
057800990114     c                   z-add     bocnor        bmocnor
057900990311     c                   z-add     1             bmoccmb
058000990114     c                   z-add     bocaiv        bmocaiv
058100990114     c                   z-add     bocqta        bmocqta
058200990114     c                   z-add     bocprz        bmocprz
058300990114     c                   z-add     bocsc1        bmocsc1
058400990114     c                   z-add     bocsc2        bmocsc2
058500990114     c                   z-add     bocsc3        bmocsc3
058600990114     c                   z-add     bocnbl        bmocnbl
058700990114     c                   z-add     bocnft        bmocnft
058800990114     c                   z-add     boccrg        bmoccrg
058900990114     c                   z-add     bocnrf        bmocnrf
059000990114     c                   z-add     bocnpc        bmocnpc
059100990114     c                   z-add     bocnpn        bmocnpn
059200990114     c                   z-add     bocass        bmocass
059300990114     c                   z-add     bocclp        bmocclp
059400990114     c                   z-add     bocdmn        bmocdmn
059500990114     c                   if        bocdmn > 0
059600990114     c     *ymd          move      bocdmn        dataiso
059700990114     c                   move      dataiso       bmocdmn
059800990114     c                   end
059900990114     c                   z-add     bocdor        bmocdor
060000990114     c                   if        bocdor > 0
060100990114     c     *ymd          move      bocdor        dataiso
060200990114     c                   move      dataiso       bmocdor
060300990114     c                   end
060400990114     c                   z-add     bocdcr        bmocdcr
060500990114     c                   if        bocdcr > 0
060600990114     c     *ymd          move      bocdcr        dataiso
060700990114     c                   move      dataiso       bmocdcr
060800990114     c                   end
060900990114     c                   z-add     bocdcp        bmocdcp
061000990114     c                   if        bocdcp > 0
061100990114     c     *ymd          move      bocdcp        dataiso
061200990114     c                   move      dataiso       bmocdcp
061300990114     c                   end
061400990114     c                   z-add     bocdbl        bmocdbl
061500990114     c                   if        bocdbl > 0
061600990114     c     *ymd          move      bocdbl        dataiso
061700990114     c                   move      dataiso       bmocdbl
061800990114     c                   end
061900990114     c                   z-add     bocdft        bmocdft
062000990114     c                   if        bocdft > 0
062100990114     c     *ymd          move      bocdft        dataiso
062200990114     c                   move      dataiso       bmocdft
062300990114     c                   end
062400990114     c                   z-add     bocdrf        bmocdrf
062500990114     c                   if        bocdrf > 0
062600990114     c     *ymd          move      bocdrf        dataiso
062700990114     c                   move      dataiso       bmocdrf
062800990114     c                   end
062900990114     c                   z-add     bocdpc        bmocdpc
063000990114     c                   if        bocdpc > 0
063100990114     c     *ymd          move      bocdpc        dataiso
063200990114     c                   move      dataiso       bmocdpc
063300990114     c                   end
063400990114     c                   write     tmsoc000
063500990114     c                   enddo
063600990114     c                   endsr
063700990114     c*-------------------------------------------------
063800990114     c     tmmof         begsr
063900990114     c*-------------------------------------------------
064000990114     c                   setoff                                           99
064100990114     c     *loval        setll     ofmof01l
064200990114     c                   do        *hival
064300990114     c                   read      ofmof01l                               99
064400990114     c   99              leave
064500990208     c     mofmat        chain     tmamp01l                           01
064600990208     c   01              iter
064700990414     c     mofdor        comp      980101                               01
064800990414     c   01              iter
064900990114     c                   move      mofute        amofute
065000990114     c                   move      mofatt        amofatt
065100990114     c                   move      moffor        amoffor
065200990114     c                   movel     '0'           amoffor
065300990114     c                   move      moftor        amoftor
065400990114     c                   move      mofpty        amofpty
065500990114     c                   move      moflis        amoflis
065600990114     c                   move      mofcco        amofcco
065700990414     c                   movel     mofcpg        amofcpg
065800990114     c                   move      *blank        amofesa
065900990311     c                   move      'ITL '        amofval
066000990114     c                   move      moffes        amoffes
066100990114     c                   move      moftrg        amoftrg
066200990114     c                   move      moffas        amoffas
066300990114     c                   move      moftsr        amoftsr
066400990114     c                   move      mofmat        amofmat
066500990114     c                   move      mofdmt        amofdmt
066600990114     c                   move      mofumq        amofumq
066700990114     c                   move      mofcom        amofcom
066800990114     c                   move      mofimb        amofimb
066900990114     c                   move      mofmag        amofmag
067000990407     c                   movel     '500'         amofcdc
067100990114     c                   move      mofdem        amofdem
067200990114     c                   move      mofgpm        amofgpm
067300990114     c                   move      mofstm        amofstm
067400990114     c                   move      mofflg        amofflg
067500990114     c                   z-add     mofnor        amofnor
067600990114     c                   z-add     mofaia        amofaia
067700990311     c                   z-add     1             amofcmb
067800990114     c                   z-add     mofqta        amofqta
067900990114     c                   z-add     mofprz        amofprz
068000990114     c                   z-add     mofsc1        amofsc1
068100990114     c                   z-add     mofsc2        amofsc2
068200990114     c                   z-add     mofsc3        amofsc3
068300990114     c                   z-add     mofnbl        amofnbl
068400990114     c                   z-add     mofnft        amofnft
068500990114     c                   z-add     mofnpt        amofnpt
068600990114     c                   z-add     mofcrg        amofcrg
068700990114     c                   z-add     mofclp        amofclp
068800990114     c                   z-add     mofdmn        amofdmn
068900990114     c                   if        mofdmn > 0
069000990114     c     *ymd          move      mofdmn        dataiso
069100990114     c                   move      dataiso       amofdmn
069200990114     c                   end
069300990114     c                   z-add     mofdor        amofdor
069400990114     c                   if        mofdor > 0
069500990114     c     *ymd          move      mofdor        dataiso
069600990114     c                   move      dataiso       amofdor
069700990114     c                   end
069800990114     c                   z-add     mofdcn        amofdcn
069900990114     c                   if        mofdcn > 0
070000990114     c     *ymd          move      mofdcn        dataiso
070100990114     c                   move      dataiso       amofdcn
070200990114     c                   end
070300990114     c                   z-add     mofdbl        amofdbl
070400990114     c                   if        mofdbl > 0
070500990114     c     *ymd          move      mofdbl        dataiso
070600990114     c                   move      dataiso       amofdbl
070700990114     c                   end
070800990114     c                   z-add     mofdft        amofdft
070900990114     c                   if        mofdft > 0
071000990114     c     *ymd          move      mofdft        dataiso
071100990114     c                   move      dataiso       amofdft
071200990114     c                   end
071300990114     c                   z-add     mofdpt        amofdpt
071400990114     c                   if        mofdpt > 0
071500990114     c     *ymd          move      mofdpt        dataiso
071600990114     c                   move      dataiso       amofdpt
071700990114     c                   end
071800990114     c                   write     tmmof000
071900990114     c                   enddo
072000990114     c                   endsr
072100990114     c*-------------------------------------------------
072200990114     c     tmsof         begsr
072300990114     c*-------------------------------------------------
072400990114     c                   setoff                                           99
072500990114     c     *loval        setll     ofsof01l
072600990114     c                   do        *hival
072700990114     c                   read      ofsof01l                               99
072800990114     c   99              leave
072900990208     c     bofmat        chain     tmamp01l                           01
073000990208     c   01              iter
073100990414     c     bofdor        comp      980101                               01
073200990903     c  n01bofdor        comp      990101                             01  01
073300990414     c   01              iter
073400990114     c                   move      bofute        bmofute
073500990114     c                   move      bofatt        bmofatt
073600990114     c                   move      boffor        bmoffor
073700990114     c                   movel     '0'           bmoffor
073800990114     c                   move      boftor        bmoftor
073900990114     c                   move      bofpty        bmofpty
074000990114     c                   move      boflis        bmoflis
074100990114     c                   move      bofcco        bmofcco
074200990414     c                   movel     bofcpg        bmofcpg
074300990114     c                   move      *blank        bmofesa
074400990311     c                   move      'ITL '        bmofval
074500990114     c                   move      boffes        bmoffes
074600990114     c                   move      boftrg        bmoftrg
074700990114     c                   move      boffas        bmoffas
074800990114     c                   move      boftsr        bmoftsr
074900990114     c                   move      bofmat        bmofmat
075000990114     c                   move      bofdmt        bmofdmt
075100990114     c                   move      bofumq        bmofumq
075200990114     c                   move      bofcom        bmofcom
075300990114     c                   move      bofimb        bmofimb
075400990114     c                   move      bofmag        bmofmag
075500990407     c                   movel     '500'         bmofcdc
075600990114     c                   move      bofdem        bmofdem
075700990114     c                   move      bofgpm        bmofgpm
075800990114     c                   move      bofstm        bmofstm
075900990114     c                   move      bofflg        bmofflg
076000990114     c                   z-add     bofnor        bmofnor
076100990114     c                   z-add     bofaia        bmofaia
076200990311     c                   z-add     1             bmofcmb
076300990114     c                   z-add     bofqta        bmofqta
076400990114     c                   z-add     bofprz        bmofprz
076500990114     c                   z-add     bofsc1        bmofsc1
076600990114     c                   z-add     bofsc2        bmofsc2
076700990114     c                   z-add     bofsc3        bmofsc3
076800990114     c                   z-add     bofnbl        bmofnbl
076900990114     c                   z-add     bofnft        bmofnft
077000990114     c                   z-add     bofnpt        bmofnpt
077100990114     c                   z-add     bofcrg        bmofcrg
077200990114     c                   z-add     bofclp        bmofclp
077300990114     c                   z-add     bofdmn        bmofdmn
077400990114     c                   if        bofdmn > 0
077500990114     c     *ymd          move      bofdmn        dataiso
077600990114     c                   move      dataiso       bmofdmn
077700990114     c                   end
077800990114     c                   z-add     bofdor        bmofdor
077900990114     c                   if        bofdor > 0
078000990114     c     *ymd          move      bofdor        dataiso
078100990114     c                   move      dataiso       bmofdor
078200990114     c                   end
078300990114     c                   z-add     bofdcn        bmofdcn
078400990114     c                   if        bofdcn > 0
078500990114     c     *ymd          move      bofdcn        dataiso
078600990114     c                   move      dataiso       bmofdcn
078700990114     c                   end
078800990114     c                   z-add     bofdbl        bmofdbl
078900990114     c                   if        bofdbl > 0
079000990114     c     *ymd          move      bofdbl        dataiso
079100990114     c                   move      dataiso       bmofdbl
079200990114     c                   end
079300990114     c                   z-add     bofdft        bmofdft
079400990114     c                   if        bofdft > 0
079500990114     c     *ymd          move      bofdft        dataiso
079600990114     c                   move      dataiso       bmofdft
079700990114     c                   end
079800990114     c                   z-add     bofdpt        bmofdpt
079900990114     c                   if        bofdpt > 0
080000990114     c     *ymd          move      bofdpt        dataiso
080100990114     c                   move      dataiso       bmofdpt
080200990114     c                   end
080300990114     c                   write     tmsof000
080400990114     c                   enddo
080500990114     c                   endsr
080600981029      *
