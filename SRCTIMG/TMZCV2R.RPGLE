000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400990113     Fanamp01l  if   e           k disk
000500990113     Ftmamp00f  o    e             disk    prefix(a)
000600990113     Fanlmp01l  if   e           k disk
000700990113     Ftmlmp00f  o    e             disk    prefix(a)
000800990902     Fmpmmp02l  if   e           k disk
000900990113     Ftmmmp00f  o    e             disk    prefix(a)
001000990114     Fmpsmp01l  if   e           k disk
001100990114     Ftmsmp00f  o    e             disk    prefix(a)
001200990902     Focmoc02l  if   e           k disk
001300990114     Ftmmoc00f  o    e             disk    prefix(a)
001400990902     Focsoc02l  if   e           k disk    rename(ocmoc000:ocms) prefix(b:1)
001500990114     Ftmsoc00f  o    e             disk    prefix(b)
001600990902     Fofmof02l  if   e           k disk
001700990114     Ftmmof00f  o    e             disk    prefix(a)
001800990902     Fofsof02l  if   e           k disk    rename(ofmof000:ofmf) prefix(b:1)
001900990114     Ftmsof00f  o    e             disk    prefix(b)
002000990113     Ddataiso          s               d   datfmt(*iso)
002100990113     Ddataeur          s               d   datfmt(*eur)
002200990903     d codici          s             12    dim(3) ctdata perrcd(1)
002300000000     C*---------------------------------------------------------------*
002400990114      *conversioni nei tracciati nuovi
002500990903     c     1             do        3             i                 2 0
002600990902     c                   clear                   codice           12
002700990902     c                   movel     codici(i)     codice
002800990113     c                   exsr      tmamp
002900990113     c                   exsr      tmlmp
003000990113     c                   exsr      tmmmp
003100990114     c                   exsr      tmsmp
003200990114     c                   exsr      tmmoc
003300990114     c                   exsr      tmsoc
003400990114     c                   exsr      tmmof
003500990114     c                   exsr      tmsof
003600990902     c                   enddo
003700990113     c                   seton                                        lr
003800990113     c*-------------------------------------------------
003900990113     c     tmamp         begsr
004000990113     c*-------------------------------------------------
004100990113     c                   setoff                                           99
004200990902     c     codice        setll     anamp01l
004300990113     c                   do        *hival
004400990902     c     codice        reade     anamp01l                               99
004500990113     c   99              leave
004600990113     c                   move      ampatt        aampatt
004700990113     c                   move      ampute        aampute
004800990113     c                   z-add     ampdig        aampdig
004900990113     c                   if        ampdig > 0
005000990113     c     *ymd          move      ampdig        dataiso
005100990113     c                   move      dataiso       aampdig
005200990113     c                   endif
005300990113     c                   z-add     ampdfg        aampdfg
005400990113     c                   if        ampdfg > 0
005500990113     c     *ymd          move      ampdfg        dataiso
005600990113     c                   move      dataiso       aampdfg
005700990113     c                   endif
005800990113     c                   move      ampclm        aampclm
005900990113     c                   move      ampuma        aampuma
006000990113     c                   move      ampmat        aampmat
006100990113     c                   move      ampdmt        aampdmt
006200990113     c                   move      ampscm        aampscm
006300990113     c                   move      ampamt        aampamt
006400990113     c                   move      ampgap        aampgap
006500990113     c                   move      ampumq        aampumq
006600990113     c                   move      ampums        aampums
006700990113     c                   move      ampumv        aampumv
006800990113     c                   z-add     ampfcq        aampfcq
006900990113     c                   z-add     ampfcs        aampfcs
007000990113     c                   z-add     ampfcv        aampfcv
007100990113     c                   move      ampctf        aampctf
007200990113     c                   z-add     ampaiv        aampaiv
007300990113     c                   move      *blank        aampesv
007400990113     c                   move      amplis        aamplis
007500990113     c                   z-add     ampaia        aampaia
007600990113     c                   move      *blank        aampesa
007700990113     c                   z-add     amplro        aamplro
007800990113     c                   z-add     amplvm        aamplvm
007900990113     c                   z-add     amplec        aamplec
008000990113     c                   z-add     amppdc        aamppdc
008100990113     c                   z-add     amptrp        aamptrp
008200990113     c                   z-add     ampcca        aampcca
008300990113     c                   z-add     amppnt        aamppnt
008400990113     c                   move      ampimb        aampimb
008500990113     c                   move      ampmag        aampmag
008600990113     c                   move      ampubi        aampubi
008700990113     c                   move      *blank        aampfil
008800990113     c                   z-add     amptar        ww                6 0
008900990113     c                   if        ww  > 0
009000990113     c     *dmy          move      ww            dataeur
009100990113     c                   move      dataeur       dataiso
009200990113     c                   move      dataiso       aamptar
009300990113     c                   endif
009400990113     c                   write     tmamp000
009500990113     c                   enddo
009600990113     c                   endsr
009700990113     c*-------------------------------------------------
009800990113     c     tmlmp         begsr
009900990113     c*-------------------------------------------------
010000990113     c                   setoff                                           99
010100990902     c     klmp          klist
010200990902     c                   kfld                    lis               3
010300990902     c                   kfld                    codice
010400990902     c                   move      '1  '         lis
010500990902     c     klmp          setll     anlmp01l
010600990113     c                   do        *hival
010700990902     c     klmp          reade     anlmp01l                               99
010800990113     c   99              leave
010900990113     c                   move      lmpute        almpute
011000990113     c                   move      lmpatt        almpatt
011100990113     c                   move      lmplis        almplis
011200990113     c                   move      lmpmat        almpmat
011300990113     c                   move      lmpflg        almpflg
011400990311     c                   move      'ITL '        almpval
011500990113     c                   z-add     lmpvp1        almpvp1
011600990113     c                   z-add     lmpvp2        almpvp2
011700990113     c                   z-add     lmpvp3        almpvp3
011800990113     c                   z-add     lmpvp4        almpvp4
011900990113     c                   z-add     lmpvp5        almpvp5
012000990113     c                   z-add     lmpdig        almpdig
012100990113     c                   if        lmpdig > 0
012200990113     c     *ymd          move      lmpdig        dataiso
012300990113     c                   move      dataiso       almpdig
012400990113     c                   end
012500990113     c                   z-add     lmpdfg        almpdfg
012600990113     c                   if        lmpdfg > 0
012700990113     c     *ymd          move      lmpdfg        dataiso
012800990113     c                   move      dataiso       almpdfg
012900990113     c                   end
013000990113     c                   write     tmlmp000
013100990113     c                   enddo
013200990113     c                   endsr
013300990113     c*-------------------------------------------------
013400990113     c     tmmmp         begsr
013500990113     c*-------------------------------------------------
013600990113     c                   setoff                                           99
013700990902     c     codice        setll     mpmmp02l
013800990113     c                   do        *hival
013900990902     c     codice        reade     mpmmp02l                               99
014000990113     c   99              leave
014100990902     c     mmpdmv        comp      980101                               01
014200990208     c   01              iter
014300990113     c                   move      mmpute        ammpute
014400990113     c                   move      mmpatt        ammpatt
014500990113     c                   move      mmpmag        ammpmag
014600990113     c                   move      mmpcau        ammpcau
014700990113     c                   move      mmpopc        ammpopc
014800990407     c                   movel     '500'         ammpcdc
014900990113     c                   move      mmpmat        ammpmat
015000990113     c                   move      mmpuma        ammpuma
015100990113     c                   move      mmpmcp        ammpmcp
015200990113     c                   move      mmpfgq        ammpfgq
015300990113     c                   move      mmpfgv        ammpfgv
015400990113     c                   move      mmpfgt        ammpfgt
015500990113     c                   move      mmpflg        ammpflg
015600990311     c                   move      'ITL '        ammpval
015700990311     c                   z-add     1             ammpcmb
015800990113     c                   z-add     mmpaac        ammpaac
015900990113     c                   add       1900          ammpaac
016000990113     c                   z-add     mmpcrg        ammpcrg
016100990113     c                   z-add     mmpprz        ammpprz
016200990113     c                   z-add     mmpqta        ammpqta
016300990113     c                   z-add     mmpndc        ammpndc
016400990113     c                   z-add     mmpnmv        ammpnmv
016500990113     c                   z-add     mmpdmn        ammpdmn
016600990113     c                   if        mmpdmn > 0
016700990113     c     *ymd          move      mmpdmn        dataiso
016800990113     c                   move      dataiso       ammpdmn
016900990113     c                   end
017000990113     c                   z-add     mmpdmv        ammpdmv
017100990113     c                   if        mmpdmv > 0
017200990113     c     *ymd          move      mmpdmv        dataiso
017300990113     c                   move      dataiso       ammpdmv
017400990113     c                   end
017500990113     c                   z-add     mmpddc        ammpddc
017600990113     c                   if        mmpddc > 0
017700990113     c     *ymd          move      mmpddc        dataiso
017800990113     c                   move      dataiso       ammpddc
017900990113     c                   end
018000990113     c                   write     tmmmp000
018100990113     c                   enddo
018200990113     c                   endsr
018300990114     c*-------------------------------------------------
018400990114     c     tmsmp         begsr
018500990114     c*-------------------------------------------------
018600990114     c                   setoff                                           99
018700990407     c     98            setll     mpsmp01l
018800990114     c                   do        *hival
018900990114     c                   read      mpsmp01l                               99
019000990114     c   99              leave
019100990902     c     smpmat        comp      codice                             0101
019200990208     c   01              iter
019300990114     c                   move      smpute        asmpute
019400990114     c                   move      smpatt        asmpatt
019500990114     c                   move      smpmag        asmpmag
019600990114     c                   move      smpmat        asmpmat
019700990114     c                   move      smpuma        asmpuma
019800990114     c                   move      smpflg        asmpflg
019900990311     c                   move      'ITL '        asmpval
020000990311     c                   z-add     1             asmpcmb
020100990114     c                   z-add     smpaac        asmpaac
020200990114     c                   add       1900          asmpaac
020300990114     c                   z-add     smpplf        asmpplf
020400990114     c                   z-add     smpplv        asmpplv
020500990114     c                   z-add     smpqgi        asmpqgi
020600990114     c                   z-add     smpqor        asmpqor
020700990114     c                   z-add     smpqpr        asmpqpr
020800990114     c                   z-add     smpqim        asmpqim
020900990114     c                   z-add     smpqip        asmpqip
021000990114     c                   z-add     smpqgf        asmpqgf
021100990114     c                   z-add     smpqe1        asmpqe1
021200990114     c                   z-add     smpqe2        asmpqe2
021300990114     c                   z-add     smpqe3        asmpqe3
021400990114     c                   z-add     smpqe4        asmpqe4
021500990114     c                   z-add     smpqe5        asmpqe5
021600990114     c                   z-add     smpqu1        asmpqu1
021700990114     c                   z-add     smpqu2        asmpqu2
021800990114     c                   z-add     smpqu3        asmpqu3
021900990114     c                   z-add     smpqu4        asmpqu4
022000990114     c                   z-add     smpqu5        asmpqu5
022100990114     c                   z-add     smpnms        asmpnms
022200990114     c                   z-add     smppmc        asmppmc
022300990114     c                   z-add     smpdmn        asmpdmn
022400990114     c                   if        smpdmn > 0
022500990114     c     *ymd          move      smpdmn        dataiso
022600990114     c                   move      dataiso       asmpdmn
022700990114     c                   end
022800990114     c                   z-add     smpdms        asmpdms
022900990114     c                   if        smpdms > 0
023000990114     c     *ymd          move      smpdms        dataiso
023100990114     c                   move      dataiso       asmpdms
023200990114     c                   end
023300990114     c                   write     tmsmp000
023400990114     c                   enddo
023500990114     c                   endsr
023600990114     c*-------------------------------------------------
023700990114     c     tmmoc         begsr
023800990114     c*-------------------------------------------------
023900990114     c                   setoff                                           99
024000990902     c     codice        setll     ocmoc02l
024100990114     c                   do        *hival
024200990902     c     codice        reade     ocmoc02l                               99
024300990114     c   99              leave
024400990414     c     mocdor        comp      980101                               01
024500990414     c   01              iter
024600990114     c                   move      mocute        amocute
024700990114     c                   move      mocatt        amocatt
024800990114     c                   move      moccli        amoccli
024900990126     c                   movel     '0'           amoccli
025000990114     c                   move      moctor        amoctor
025100990114     c                   move      moctro        amoctro
025200990114     c                   move      moclis        amoclis
025300990311     c                   move      'ITL '        amocval
025400990114     c                   move      mocspe        amocspe
025500990114     c                   move      mocluo        amocluo
025600990114     c                   move      mocres        amocres
025700990114     c                   move      mocimb        amocimb
025800990414     c                   movel     moccpg        amoccpg
025900990114     c                   move      *blank        amocesv
026000990114     c                   move      mocfes        amocfes
026100990114     c                   move      moctrg        amoctrg
026200990114     c                   move      mocfas        amocfas
026300990114     c                   move      moctsr        amoctsr
026400990114     c                   move      mocmat        amocmat
026500990114     c                   move      mocdmt        amocdmt
026600990114     c                   move      mocumq        amocumq
026700990114     c                   move      moccom        amoccom
026800990114     c                   move      mocmag        amocmag
026900990407     c                   movel     '500'         amoccdc
027000990114     c                   move      mocpcl        amocpcl
027100990114     c                   move      mocsos        amocsos
027200990114     c                   move      mocgpm        amocgpm
027300990114     c                   move      mocstm        amocstm
027400990114     c                   move      mocfls        amocfls
027500990114     c                   move      mocflg        amocflg
027600990114     c                   z-add     mocnor        amocnor
027700990311     c                   z-add     1             amoccmb
027800990114     c                   z-add     mocaiv        amocaiv
027900990114     c                   z-add     mocqta        amocqta
028000990114     c                   z-add     mocprz        amocprz
028100990114     c                   z-add     mocsc1        amocsc1
028200990114     c                   z-add     mocsc2        amocsc2
028300990114     c                   z-add     mocsc3        amocsc3
028400990114     c                   z-add     mocnbl        amocnbl
028500990114     c                   z-add     mocnft        amocnft
028600990114     c                   z-add     moccrg        amoccrg
028700990114     c                   z-add     mocnrf        amocnrf
028800990114     c                   z-add     mocnpc        amocnpc
028900990114     c                   z-add     mocnpn        amocnpn
029000990114     c                   z-add     mocass        amocass
029100990114     c                   z-add     mocclp        amocclp
029200990114     c                   z-add     mocdmn        amocdmn
029300990114     c                   if        mocdmn > 0
029400990114     c     *ymd          move      mocdmn        dataiso
029500990114     c                   move      dataiso       amocdmn
029600990114     c                   end
029700990114     c                   z-add     mocdor        amocdor
029800990114     c                   if        mocdor > 0
029900990114     c     *ymd          move      mocdor        dataiso
030000990114     c                   move      dataiso       amocdor
030100990114     c                   end
030200990114     c                   z-add     mocdcr        amocdcr
030300990114     c                   if        mocdcr > 0
030400990114     c     *ymd          move      mocdcr        dataiso
030500990114     c                   move      dataiso       amocdcr
030600990114     c                   end
030700990114     c                   z-add     mocdcp        amocdcp
030800990114     c                   if        mocdcp > 0
030900990114     c     *ymd          move      mocdcp        dataiso
031000990114     c                   move      dataiso       amocdcp
031100990114     c                   end
031200990114     c                   z-add     mocdbl        amocdbl
031300990114     c                   if        mocdbl > 0
031400990114     c     *ymd          move      mocdbl        dataiso
031500990114     c                   move      dataiso       amocdbl
031600990114     c                   end
031700990114     c                   z-add     mocdft        amocdft
031800990114     c                   if        mocdft > 0
031900990114     c     *ymd          move      mocdft        dataiso
032000990114     c                   move      dataiso       amocdft
032100990114     c                   end
032200990114     c                   z-add     mocdrf        amocdrf
032300990114     c                   if        mocdrf > 0
032400990114     c     *ymd          move      mocdrf        dataiso
032500990114     c                   move      dataiso       amocdrf
032600990114     c                   end
032700990114     c                   z-add     mocdpc        amocdpc
032800990114     c                   if        mocdpc > 0
032900990114     c     *ymd          move      mocdpc        dataiso
033000990114     c                   move      dataiso       amocdpc
033100990114     c                   end
033200990114     c                   write     tmmoc000
033300990114     c                   enddo
033400990114     c                   endsr
033500990114     c*-------------------------------------------------
033600990114     c     tmsoc         begsr
033700990114     c*-------------------------------------------------
033800990114     c                   setoff                                           99
033900990902     c     codice        setll     ocsoc02l
034000990114     c                   do        *hival
034100990902     c     codice        reade     ocsoc02l                               99
034200990114     c   99              leave
034300990414     c     bocdor        comp      980101                               01
034400990903     c  n01bocdor        comp      990101                             01  01
034500990414     c   01              iter
034600990114     c                   move      bocute        bmocute
034700990114     c                   move      bocatt        bmocatt
034800990114     c                   move      boccli        bmoccli
034900990202     c                   movel     '0'           bmoccli
035000990114     c                   move      boctor        bmoctor
035100990114     c                   move      boctro        bmoctro
035200990114     c                   move      boclis        bmoclis
035300990311     c                   move      'ITL '        bmocval
035400990114     c                   move      bocspe        bmocspe
035500990114     c                   move      bocluo        bmocluo
035600990114     c                   move      bocres        bmocres
035700990114     c                   move      bocimb        bmocimb
035800990414     c                   movel     boccpg        bmoccpg
035900990114     c                   move      *blank        bmocesv
036000990114     c                   move      bocfes        bmocfes
036100990114     c                   move      boctrg        bmoctrg
036200990114     c                   move      bocfas        bmocfas
036300990114     c                   move      boctsr        bmoctsr
036400990114     c                   move      bocmat        bmocmat
036500990114     c                   move      bocdmt        bmocdmt
036600990114     c                   move      bocumq        bmocumq
036700990114     c                   move      boccom        bmoccom
036800990114     c                   move      bocmag        bmocmag
036900990407     c                   movel     '500'         bmoccdc
037000990114     c                   move      bocpcl        bmocpcl
037100990114     c                   move      bocsos        bmocsos
037200990114     c                   move      bocgpm        bmocgpm
037300990114     c                   move      bocstm        bmocstm
037400990114     c                   move      bocfls        bmocfls
037500990114     c                   move      bocflg        bmocflg
037600990114     c                   z-add     bocnor        bmocnor
037700990311     c                   z-add     1             bmoccmb
037800990114     c                   z-add     bocaiv        bmocaiv
037900990114     c                   z-add     bocqta        bmocqta
038000990114     c                   z-add     bocprz        bmocprz
038100990114     c                   z-add     bocsc1        bmocsc1
038200990114     c                   z-add     bocsc2        bmocsc2
038300990114     c                   z-add     bocsc3        bmocsc3
038400990114     c                   z-add     bocnbl        bmocnbl
038500990114     c                   z-add     bocnft        bmocnft
038600990114     c                   z-add     boccrg        bmoccrg
038700990114     c                   z-add     bocnrf        bmocnrf
038800990114     c                   z-add     bocnpc        bmocnpc
038900990114     c                   z-add     bocnpn        bmocnpn
039000990114     c                   z-add     bocass        bmocass
039100990114     c                   z-add     bocclp        bmocclp
039200990114     c                   z-add     bocdmn        bmocdmn
039300990114     c                   if        bocdmn > 0
039400990114     c     *ymd          move      bocdmn        dataiso
039500990114     c                   move      dataiso       bmocdmn
039600990114     c                   end
039700990114     c                   z-add     bocdor        bmocdor
039800990114     c                   if        bocdor > 0
039900990114     c     *ymd          move      bocdor        dataiso
040000990114     c                   move      dataiso       bmocdor
040100990114     c                   end
040200990114     c                   z-add     bocdcr        bmocdcr
040300990114     c                   if        bocdcr > 0
040400990114     c     *ymd          move      bocdcr        dataiso
040500990114     c                   move      dataiso       bmocdcr
040600990114     c                   end
040700990114     c                   z-add     bocdcp        bmocdcp
040800990114     c                   if        bocdcp > 0
040900990114     c     *ymd          move      bocdcp        dataiso
041000990114     c                   move      dataiso       bmocdcp
041100990114     c                   end
041200990114     c                   z-add     bocdbl        bmocdbl
041300990114     c                   if        bocdbl > 0
041400990114     c     *ymd          move      bocdbl        dataiso
041500990114     c                   move      dataiso       bmocdbl
041600990114     c                   end
041700990114     c                   z-add     bocdft        bmocdft
041800990114     c                   if        bocdft > 0
041900990114     c     *ymd          move      bocdft        dataiso
042000990114     c                   move      dataiso       bmocdft
042100990114     c                   end
042200990114     c                   z-add     bocdrf        bmocdrf
042300990114     c                   if        bocdrf > 0
042400990114     c     *ymd          move      bocdrf        dataiso
042500990114     c                   move      dataiso       bmocdrf
042600990114     c                   end
042700990114     c                   z-add     bocdpc        bmocdpc
042800990114     c                   if        bocdpc > 0
042900990114     c     *ymd          move      bocdpc        dataiso
043000990114     c                   move      dataiso       bmocdpc
043100990114     c                   end
043200990114     c                   write     tmsoc000
043300990114     c                   enddo
043400990114     c                   endsr
043500990114     c*-------------------------------------------------
043600990114     c     tmmof         begsr
043700990114     c*-------------------------------------------------
043800990114     c                   setoff                                           99
043900990902     c     codice        setll     ofmof02l
044000990114     c                   do        *hival
044100990902     c     codice        reade     ofmof02l                               99
044200990114     c   99              leave
044300990414     c     mofdor        comp      980101                               01
044400990414     c   01              iter
044500990114     c                   move      mofute        amofute
044600990114     c                   move      mofatt        amofatt
044700990114     c                   move      moffor        amoffor
044800990114     c                   movel     '0'           amoffor
044900990114     c                   move      moftor        amoftor
045000990114     c                   move      mofpty        amofpty
045100990114     c                   move      moflis        amoflis
045200990114     c                   move      mofcco        amofcco
045300990414     c                   movel     mofcpg        amofcpg
045400990114     c                   move      *blank        amofesa
045500990311     c                   move      'ITL '        amofval
045600990114     c                   move      moffes        amoffes
045700990114     c                   move      moftrg        amoftrg
045800990114     c                   move      moffas        amoffas
045900990114     c                   move      moftsr        amoftsr
046000990114     c                   move      mofmat        amofmat
046100990114     c                   move      mofdmt        amofdmt
046200990114     c                   move      mofumq        amofumq
046300990114     c                   move      mofcom        amofcom
046400990114     c                   move      mofimb        amofimb
046500990114     c                   move      mofmag        amofmag
046600990407     c                   movel     '500'         amofcdc
046700990114     c                   move      mofdem        amofdem
046800990114     c                   move      mofgpm        amofgpm
046900990114     c                   move      mofstm        amofstm
047000990114     c                   move      mofflg        amofflg
047100990114     c                   z-add     mofnor        amofnor
047200990114     c                   z-add     mofaia        amofaia
047300990311     c                   z-add     1             amofcmb
047400990114     c                   z-add     mofqta        amofqta
047500990114     c                   z-add     mofprz        amofprz
047600990114     c                   z-add     mofsc1        amofsc1
047700990114     c                   z-add     mofsc2        amofsc2
047800990114     c                   z-add     mofsc3        amofsc3
047900990114     c                   z-add     mofnbl        amofnbl
048000990114     c                   z-add     mofnft        amofnft
048100990114     c                   z-add     mofnpt        amofnpt
048200990114     c                   z-add     mofcrg        amofcrg
048300990114     c                   z-add     mofclp        amofclp
048400990114     c                   z-add     mofdmn        amofdmn
048500990114     c                   if        mofdmn > 0
048600990114     c     *ymd          move      mofdmn        dataiso
048700990114     c                   move      dataiso       amofdmn
048800990114     c                   end
048900990114     c                   z-add     mofdor        amofdor
049000990114     c                   if        mofdor > 0
049100990114     c     *ymd          move      mofdor        dataiso
049200990114     c                   move      dataiso       amofdor
049300990114     c                   end
049400990114     c                   z-add     mofdcn        amofdcn
049500990114     c                   if        mofdcn > 0
049600990114     c     *ymd          move      mofdcn        dataiso
049700990114     c                   move      dataiso       amofdcn
049800990114     c                   end
049900990114     c                   z-add     mofdbl        amofdbl
050000990114     c                   if        mofdbl > 0
050100990114     c     *ymd          move      mofdbl        dataiso
050200990114     c                   move      dataiso       amofdbl
050300990114     c                   end
050400990114     c                   z-add     mofdft        amofdft
050500990114     c                   if        mofdft > 0
050600990114     c     *ymd          move      mofdft        dataiso
050700990114     c                   move      dataiso       amofdft
050800990114     c                   end
050900990114     c                   z-add     mofdpt        amofdpt
051000990114     c                   if        mofdpt > 0
051100990114     c     *ymd          move      mofdpt        dataiso
051200990114     c                   move      dataiso       amofdpt
051300990114     c                   end
051400990114     c                   write     tmmof000
051500990114     c                   enddo
051600990114     c                   endsr
051700990114     c*-------------------------------------------------
051800990114     c     tmsof         begsr
051900990114     c*-------------------------------------------------
052000990114     c                   setoff                                           99
052100990902     c     codice        setll     ofsof02l
052200990114     c                   do        *hival
052300990902     c     codice        reade     ofsof02l                               99
052400990114     c   99              leave
052500990414     c     bofdor        comp      980101                               01
052600990903     c  n01bofdor        comp      990101                             01  01
052700990414     c   01              iter
052800990114     c                   move      bofute        bmofute
052900990114     c                   move      bofatt        bmofatt
053000990114     c                   move      boffor        bmoffor
053100990114     c                   movel     '0'           bmoffor
053200990114     c                   move      boftor        bmoftor
053300990114     c                   move      bofpty        bmofpty
053400990114     c                   move      boflis        bmoflis
053500990114     c                   move      bofcco        bmofcco
053600990414     c                   movel     bofcpg        bmofcpg
053700990114     c                   move      *blank        bmofesa
053800990311     c                   move      'ITL '        bmofval
053900990114     c                   move      boffes        bmoffes
054000990114     c                   move      boftrg        bmoftrg
054100990114     c                   move      boffas        bmoffas
054200990114     c                   move      boftsr        bmoftsr
054300990114     c                   move      bofmat        bmofmat
054400990114     c                   move      bofdmt        bmofdmt
054500990114     c                   move      bofumq        bmofumq
054600990114     c                   move      bofcom        bmofcom
054700990114     c                   move      bofimb        bmofimb
054800990114     c                   move      bofmag        bmofmag
054900990407     c                   movel     '500'         bmofcdc
055000990114     c                   move      bofdem        bmofdem
055100990114     c                   move      bofgpm        bmofgpm
055200990114     c                   move      bofstm        bmofstm
055300990114     c                   move      bofflg        bmofflg
055400990114     c                   z-add     bofnor        bmofnor
055500990114     c                   z-add     bofaia        bmofaia
055600990311     c                   z-add     1             bmofcmb
055700990114     c                   z-add     bofqta        bmofqta
055800990114     c                   z-add     bofprz        bmofprz
055900990114     c                   z-add     bofsc1        bmofsc1
056000990114     c                   z-add     bofsc2        bmofsc2
056100990114     c                   z-add     bofsc3        bmofsc3
056200990114     c                   z-add     bofnbl        bmofnbl
056300990114     c                   z-add     bofnft        bmofnft
056400990114     c                   z-add     bofnpt        bmofnpt
056500990114     c                   z-add     bofcrg        bmofcrg
056600990114     c                   z-add     bofclp        bmofclp
056700990114     c                   z-add     bofdmn        bmofdmn
056800990114     c                   if        bofdmn > 0
056900990114     c     *ymd          move      bofdmn        dataiso
057000990114     c                   move      dataiso       bmofdmn
057100990114     c                   end
057200990114     c                   z-add     bofdor        bmofdor
057300990114     c                   if        bofdor > 0
057400990114     c     *ymd          move      bofdor        dataiso
057500990114     c                   move      dataiso       bmofdor
057600990114     c                   end
057700990114     c                   z-add     bofdcn        bmofdcn
057800990114     c                   if        bofdcn > 0
057900990114     c     *ymd          move      bofdcn        dataiso
058000990114     c                   move      dataiso       bmofdcn
058100990114     c                   end
058200990114     c                   z-add     bofdbl        bmofdbl
058300990114     c                   if        bofdbl > 0
058400990114     c     *ymd          move      bofdbl        dataiso
058500990114     c                   move      dataiso       bmofdbl
058600990114     c                   end
058700990114     c                   z-add     bofdft        bmofdft
058800990114     c                   if        bofdft > 0
058900990114     c     *ymd          move      bofdft        dataiso
059000990114     c                   move      dataiso       bmofdft
059100990114     c                   end
059200990114     c                   z-add     bofdpt        bmofdpt
059300990114     c                   if        bofdpt > 0
059400990114     c     *ymd          move      bofdpt        dataiso
059500990114     c                   move      dataiso       bmofdpt
059600990114     c                   end
059700990114     c                   write     tmsof000
059800990114     c                   enddo
059900990114     c                   endsr
060000981029      *
060100990902**   codici
06020099090390033                                                                       9897
06030099090390009
06040099090380758
