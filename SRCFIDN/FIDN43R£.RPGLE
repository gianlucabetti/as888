000100990212     H DECEDIT('0,') DATEDIT(*DMY.)
000200981203      *----------------------------------------------------*
000300990212      *  ELENCO C.A.
000400990212      *----------------------------------------------------*
000500990212     FFIDN43D   CF   E             WORKSTN
000600981203     FAZORG01L  IF   E           K DISK
000700990215     FTABEL00F  IF   E           K DISK
000800990215     FAZORG02L  IF   E           K DISK    rename(azorg:azorg002)
000900981203
001000050609     d wabi            s                   like(UteAut)
001100990315     D paxccc          s                   like(acokcc)
001200990315     D WRS             s                   like(v1crsb)
001300990315     D WDKS            s                   like(v1dksb)
001400990315     D paxdmt          s                   like(acorag)
001500990315     D WAMGDCD         s                   like(V1cdcd)
001600990212     D WAMGDCA         s                   like(V1cdcd)
001700990218     D WAMGDHD         s                   like(V1cdcd)
001800990218     D WAMGDHA         s                   like(V1cdcd)
001900990212     D WAMGDSD         s                   like(V1cdcd)
002000990212     D WAMGDSA         s                   like(V1cdcd)
002100990215     D §KUT            s                   like(codut)
002200990215     D §COD            s                   like(TBLCOD)
002300990215     D §KEY            s                   like(TBLkey)
002400990215     D COD             s                   like(TBLCOD)
002500990215     D KEY             s                   like(TBLkey)
002600990215     D WFIL            s                   like(V1cFIL)
002700990215     D WARE            s                   like(ORGCAR)
002800040129     d wZeros          s             11    inz(*all'0')
002900040129     d wErr            s              2  0 inz
003000110209     d Indx            s              2  0 inz
003100981203
003200110210     D MSG             S             78    DIM(37) CTDATA PERRCD(1)             MSG VIDEO
003300050610     D $pog            s              3    dim(250)
003400050610     D $DIG            s              1    dim(20)
003500050610     D $ARG            s              3    dim(50)
003600110209     D $ksb            s              7  0 dim(5)
003700981203
003800981203     D WLBDAT          DS                  INZ
003900981203     D  G02DAT                 1      8  0
004000981203     D  G02INV                 9     16  0
004100981203     D  G02ERR                17     17
004200981203     D  G02TGI                18     22  0
004300981203
004400050609     D*** CNCR80        E DS
004500990212     D FIDN43DS      E DS
004600050609     D TIBS34DS      E DS
004700050609     D TRUL31DS      E DS
004800050609     D TRUL31DS2     E DS
004900990315     D FNLV55DS      E DS
005000990212     D TIBS02DS      E DS
005100990212     D DTAD          E DS
005200990218     D DCCH          E DS
005300071204     D DSTD          E DS
005400050609     d Azuteds       e ds                  extname(Azute00f)
005500050609     d dDatiute      e ds
005600050609     d dLat          e ds
005700050609     d dute01        e ds
005800990315     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005900981203
006000981203     D KPJBA         E DS
006100990315     D
006200990315     D W0140           S             14  0
006300990315     D Wdtgio          S              8  0
006400990315     D dateu           S              8  0
006500990315     D                 DS
006600990315     D  wkl                    1      3  0
006700990315     D  wks                    4      7  0
006800990315     D  WCODICE                1      7  0
006900990315     D
007000990315     d TIBS69DS      E DS
007100990315     d DS_cnaco      E DS                  extname(CNACO00F)
007200990315     d DS_cnind      E DS                  extname(CNIND00F)
007300990315     d DS_cnclp      E DS                  extname(CNCLP00F)
007400990315     d DS_fncls      E DS                  extname(FNCLS00F)
007500981203
007600981203      *------------------------------------------------------------------------*
007700990215     c* INDICATORI
007800990215      *------------------------------------------------------------------------*
007900990215     C*  01   - on - sono in sede    off - sono in filiale
008000990215     C*  06   - 2 livello
008100990215     C*  28   - emetto campo messaggio
008200990215     C*  30/31- comodo
008300050906      *  35   - visualizza richiesta output in wf WFDDN00F
008400040129     C*  40/60- errori posizionamento cursore sui campi
008500990215     C*  90   - riemissione videata per errore o ricerca
008600990215      *------------------------------------------------------------------------*
008700990315     c     iniziofil     tag
008800990212     c                   clear                   v1cprs
008900990309     c                   clear                   v1cter
009000990309     c                   clear                   v1dter
009100990212     c                   clear                   v1cfil
009200990212     c                   clear                   v1caac
009300990212     c                   clear                   v1cpsl
009400990212     c                   clear                   v1dprs
009500990212     c                   clear                   v1dfil
009600990212     c                   clear                   v1dpsl
009700990212     c                   clear                   v1ctad
009800990212     c                   clear                   v1crgr
009900990916     c                   clear                   v1crg2
010000990916     c                   clear                   v1crg3
010100990212     c                   clear                   v1ctsl
010200990212     c                   clear                   v1dtad
010300990212     c                   clear                   v1cdcd
010400990212     c                   clear                   v1cdca
010500990218     c                   clear                   v1cdhd
010600990218     c                   clear                   v1cdha
010700990218     c                   clear                   v1ccch
010800990212     c                   clear                   v1ctpl
010900990212     c                   clear                   v1ctca
011000990212     c                   clear                   v1clnp
011100990212     c                   clear                   v1clna
011200990212     c                   clear                   v1cdsd
011300990212     c                   clear                   v1cdsa
011400050601     c                   clear                   v2ddis
011500050601     c                   clear                   v1cdis
011600050601     c                   clear                   v2dare
011700050601     c                   clear                   v1care
011800060206     c                   clear                   v1ctr1
011900060206     c                   clear                   v1ctr2
012000060206     c                   clear                   v1ctr3
012100990315     c     INIZIOsede    TAG
012200990311     c                   clear                   v1ddis
012300990311     c                   clear                   v1dare
012400990315     c                   clear                   v1cntw
012500990315     c                   clear                   v1cklb
012600990315     c                   clear                   v1cksb
012700990315     c                   clear                   v1dksb
012800990315     c                   clear                   v1crsb
012900990315     c                   clear                   v1cklm
013000990315     c                   clear                   v1cksm
013100990315     c                   clear                   v1dksm
013200990315     c                   clear                   v1crsm
013300990315     c                   clear                   v1ckld
013400990315     c                   clear                   v1cksd
013500990315     c                   clear                   v1dksd
013600990315     c                   clear                   v1crsd
013700990512     c                   clear                   v1ctsn
013800000215     c                   clear                   v1csdl
013900040726     c                   clear                   v1csdm
014000060120     c                   clear                   v1cval
014100060125     c                   clear                   v1cpkd
014200110209     c                   clear                   v1cklb2
014300110209     c                   clear                   v1cksb2
014400110209     c                   clear                   v1dksb2
014500110209     c                   clear                   v1cklb3
014600110209     c                   clear                   v1cksb3
014700110209     c                   clear                   v1dksb3
014800110209     c                   clear                   v1cklb4
014900110209     c                   clear                   v1cksb4
015000110209     c                   clear                   v1dksb4
015100110209     c                   clear                   v1cklb5
015200110209     c                   clear                   v1cksb5
015300110209     c                   clear                   v1dksb5
015400990311     c                   eval      v1cind='N'
015500990311     c                   eval      v1cina='N'
015600990311     c                   eval      v1cint='N'
015700990311     c                   eval      v1cord='2'
015800990406     c                   eval      v1ctod='N'
015900990406     c                   eval      v1ctoa='N'
016000990406     c                   eval      v1cton='N'
016100990406     c                   eval      v1ctod='N'
016200101118     c****               eval      v1cusi='N'
016300090401     c                   eval      v1cusn='S'
016400090325     c
016500990406     c                   eval      v1ctoi='N'
016600101118     c****               eval      v1ccrp='N'
016700090325     c                   eval      v1ccrn='N'
016800040129     c                   eval      v1ccfd='N'
016900990319     c
017000990319      * IMPOSTO IL P.O. IN GESTIONE
017100050906     c                   eval      *in35 = *off
017200990319     C                   IF        SIMFEL > 0
017300050613     c                   eval      v2ddis='T=Tutti'
017400050609     C                   IF        dutlpo = '2' or dutlpo = *blanks
017500050609     c                   movel     dutpou        v1cpsl
017600990319     C                   eval      *in06 = *on
017700990319    2C                   ENDIF
017800990319   X2C                   ELSE
017900990319     C                   eval      *in01 = *on
018000050906     c                   if        %subst(KNMUS:1:3) = 'EDP'
018100080306     c                             or %subst(KNMUS:1:3) = 'QUA'
018200150604     c                             or %subst(KNMUS:1:3) = 'RIC'
018300050906     c                   eval      *in35 = *on
018400050906     c                   endif
018500990319    2C                   ENDIF
018600101111     c                   if        KNMUS = 'GEN002'
018700101111     c                   eval      *in36 = *on
018800101111     c                   endif
018900101111     c
019000071204      * se non acceso il 35 verifico l'utente con utente responsabile ufficio danni
019100130806     c***********        clear                   tibs02ds
019200130806     C***********        MOVEL     'C'           t02mod
019300130806     C***********        MOVEL     knsif         t02sif
019400130806     C***********        MOVEL     'STD'         t02cod
019500130806     c***********        movel     '1'           t02ke1
019600130806     c***********        movel     'EUR'         t02ke2
019700071204      *
019800130806     C***********        CALL      'TIBS02R'
019900130806     C***********        PARM                    KPJBA
020000130806     C***********        PARM                    TIBS02DS
020100071204      *
020200130806     C***********        IF        T02err = *BLANKS
020300130806     C***********        movel     T02uni        DSTD
020400130806
020500150210     c                   if        KNMUS = 'ASS020'
020600150210      * non deve vedere il lancio del file WFRDN50F come GEN002
020700150210     c                   eval      *in36 = *on
020800071204     c                   endif
020900130806
021000130806     c***********        endif
021100071204     c
021200981203     c*
021300990215     c     FOR00         tag
021400990215     c                   if        simfel=0
021500990215     c                   exfmt     dn43d00
021600040129     c                   setoff                                       2890
021700040129     c                   movea     wZeros        *in(40)
021800990215     c* F3 = FINE
021900990215     c   KC              goto      fine
022000990215     c**
022100990215     c                   exsr      ctrd00
022200990215     c   90              goto      for00
022300990215     c                   endif
022400990215     c**
022500990315     c                   seton                                        54
022600990215     c
022700990212     c     FOR01         tag
022800990212     c                   exfmt     dn43d01
022900990212     c**
023000990212     c                   setoff                                       289040
023100990212     c                   setoff                                       414243
023200990212     c                   setoff                                       444546
023300990212     c                   setoff                                       474849
023400990218     c                   setoff                                       505152
023500990315     c                   setoff                                       535455
023600050610     c                   setoff                                       565758
023700981203     c                   clear                   v1cmsg
023800981203     c* F3 = FINE
023900990212     c   KC              goto      fine
024000990215     c* F12 = ritorno
024100990215     c   KL              goto      for00
024200990212     c
024300110209     c
024400990212     c* F8 - selezioni sulle bolle
024500990212     c   KH              goto      for02
024600990212     c*
024700990212      * Controlli
024800990212     c                   exsr      CTRD01
024900990212     c   90              goto      for01
025000990212     c                   exsr      CTRD02
025100990212     c   90              movel     msg(8)        v1cmsg
025200990212     c   90              goto      for01
025300110210     c
025400110210     c* F1 - selezioni Beneficiari aggiuntivi
025500110210     c                   if        *inka
025600110210     c                   exsr      Gesd03
025700110210     c                   goto      for01
025800110210     c                   endif
025900990212     c*
026000990212     c
026100990212     c                   if        not *inkh  AND not *inkf
026200990512     c                   seton                                        54
026300990212     c                   goto      for01
026400990212     c                   endif
026500990212     c*
026600990212     c* Nessuna selezione
026700990212     c
026800990212     c   KF              exsr      conferma
026900990315     c                   if        *inkf AND simfel=0
027000990315     c                   goto      iniziosede
027100990315     c                   endif
027200990315     c                   if        *inkf AND simfel>0
027300990315     c                   goto      iniziofil
027400990315     c                   endif
027500990212     c**
027600990212     c     FOR02         tag
027700990212     c**
027800990212     c                   exfmt     dn43d02
027900990212     c
028000990212     c                   setoff                                       289040
028100990212     c                   setoff                                       414243
028200990212     c                   setoff                                       444546
028300990212     c                   setoff                                       474849
028400990315     c                   setoff                                       505152
028500990212     c                   clear                   v1cmsg
028600990212     c* F3 - FINE
028700990212     c   KC              goto      fine
028800990212     c*
028900990212     c* F8 - selezioni sulle C.A.
029000990512     c   KH              seton                                        54
029100990212     c   KH              goto      for01
029200990212     c**
029300990215      * Controlli
029400990212     c                   exsr      CTRD02
029500990212     c   90              goto      for02
029600990215     c
029700990215     c                   exsr      CTRD01
029800990215     c   90              movel     msg(7)        v1cmsg
029900990215     c   90              goto      for02
030000990212     c
030100990212     c                   if        not *inkh  AND not *inkf
030200990212     c                   goto      for02
030300990212     c                   endif
030400990212     c
030500990212     c*
030600981203      * F6 = Conferma Stampa
030700990212     c   KF              EXSR      conferma
030800990323     c                   if        *inkf AND simfel=0
030900990323     c                   goto      iniziosede
031000990323     c                   endif
031100990323     c                   if        *inkf AND simfel>0
031200990323     c                   goto      iniziofil
031300990323     c                   endif
031400990212     c
031500981203      *
031600990212     c     fine          tag
031700990212     c                   clear                   tibs02ds
031800990212     C                   MOVEL     'C'           t02tla
031900990212      *
032000990212     C                   CALL      'TIBS02R'
032100990212     C                   PARM                    KPJBA
032200990212     C                   PARM                    TIBS02DS
032300990315     C                   clear                   TIBS69DS
032400990315     c                   eval      i69tla='C'
032500990315      *
032600990315     C                   CALL      'TIBS69R'
032700990315     C                   PARM                    tibs69DS
032800990315     C                   PARM                    DS_cnaco
032900990315     C                   PARM                    DS_cnind
033000990315     C                   PARM                    DS_cnclp
033100990315     C                   PARM                    DS_fncls
033200981203     c                   eval      *inlr = *on
033300110209      *--------------------------------------------------------------------*
033400110209      * Gestione video 3  - beneficiari aggiuntivi
033500110209      *--------------------------------------------------------------------*
033600110209     C     Gesd03        BEGSR
033700110209     c**
033800110209     c     FOR03         tag
033900110209     c**
034000110209     c                   exfmt     dn43d03
034100110209     c
034200110209     c                   setoff                                       289040
034300110209     c                   setoff                                       414243
034400110209     c                   setoff                                       444546
034500110209     c                   setoff                                       474849
034600110209     c                   clear                   v1cmsg
034700110209     c**
034800110209      * Controlli
034900110209     c                   exsr      CTRD03
035000110209     c   90              goto      for03
035100110211     c
035200110211     c* Esco solo se premuto F8
035300110211     c                   if        not *inkh
035400110211     c                   goto      for03
035500110211     c                   endif
035600110209     c
035700110210     c* se inserito almeno un codice metto F1 in reverse immage
035800110210     c                   if        v1cksb2>0 or v1cksb3>0 or v1cksb4>0 or
035900110210     c                             v1cksb5>0
036000110210     c                   seton                                        10
036100110210     c                   endif
036200110210     c
036300110209     c                   ENDSR
036400110209      *--------------------------------------------------------------------*
036500110209      * CONTROLLI VIDEO 3
036600110209      *--------------------------------------------------------------------*
036700110209     C     Ctrd03        BEGSR
036800110209     c                   clear                   $ksb(2)
036900110209     c                   clear                   $ksb(3)
037000110209     c                   clear                   $ksb(4)
037100110209     c                   clear                   $ksb(5)
037200110210     c                   setoff                                       10
037300110209     c
037400110209     c*  Beneficiario 2
037500110209     c                   eval      wkl=v1cklb2
037600110209     c                   eval      wks=v1cksb2
037700110209     c                   eval      wdks=v1dksb2
037800110210     c                   eval      wrs=v1crsb
037900110209     c                   EXSR      codice
038000110209     c                   eval      v1cklb2=wkl
038100110209     c                   eval      v1cksb2=wks
038200110209     c                   eval      v1dksb2=wdks
038300110210     c   90              seton                                        40
038400110210     c   90              leavesr
038500110209     c                   if        wcodice>0
038600110209     c                   eval      Indx=%lookup(wcodice:$ksb)
038700110209     c                   if        Indx>0
038800110209     c                   movel     msg(36)       v1cmsg
038900110209     c                   seton                                        289040
039000110209     c                   leavesr
039100110209     c                   else
039200110209     c                   eval      $ksb(2)=wcodice
039300110209     c                   endif
039400110209     c                   endif
039500110209     c
039600110209     c*  Beneficiario 3
039700110209     c                   eval      wkl=v1cklb3
039800110209     c                   eval      wks=v1cksb3
039900110209     c                   eval      wdks=v1dksb3
040000110210     c                   eval      wrs=v1crsb
040100110209     c                   EXSR      codice
040200110209     c                   eval      v1cklb3=wkl
040300110209     c                   eval      v1cksb3=wks
040400110209     c                   eval      v1dksb3=wdks
040500110210     c   90              seton                                        41
040600110210     c   90              leavesr
040700110209     c                   if        wcodice>0
040800110209     c                   eval      Indx=%lookup(wcodice:$ksb)
040900110209     c                   if        Indx>0
041000110209     c                   movel     msg(36)       v1cmsg
041100110209     c                   seton                                        289041
041200110209     c                   leavesr
041300110209     c                   else
041400110209     c                   eval      $ksb(3)=wcodice
041500110209     c                   endif
041600110209     c                   endif
041700110209     c*  Beneficiario 4
041800110209     c                   eval      wkl=v1cklb4
041900110209     c                   eval      wks=v1cksb4
042000110209     c                   eval      wdks=v1dksb4
042100110210     c                   eval      wrs=v1crsb
042200110209     c                   EXSR      codice
042300110209     c                   eval      v1cklb4=wkl
042400110209     c                   eval      v1cksb4=wks
042500110209     c                   eval      v1dksb4=wdks
042600110210     c   90              seton                                        42
042700110210     c   90              leavesr
042800110209     c                   if        wcodice>0
042900110209     c                   eval      Indx=%lookup(wcodice:$ksb)
043000110209     c                   if        Indx>0
043100110209     c                   movel     msg(36)       v1cmsg
043200110209     c                   seton                                        289042
043300110209     c                   leavesr
043400110209     c                   else
043500110209     c                   eval      $ksb(3)=wcodice
043600110209     c                   endif
043700110209     c                   endif
043800110209     c*  Beneficiario 5
043900110209     c                   eval      wkl=v1cklb5
044000110209     c                   eval      wks=v1cksb5
044100110209     c                   eval      wdks=v1dksb5
044200110210     c                   eval      wrs=v1crsb
044300110209     c                   EXSR      codice
044400110209     c                   eval      v1cklb5=wkl
044500110209     c                   eval      v1cksb5=wks
044600110209     c                   eval      v1dksb5=wdks
044700110210     c   90              seton                                        43
044800110210     c   90              leavesr
044900110209     c                   if        wcodice>0
045000110209     c                   eval      Indx=%lookup(wcodice:$ksb)
045100110209     c                   if        Indx>0
045200110209     c                   movel     msg(36)       v1cmsg
045300110209     c                   seton                                        289043
045400110209     c                   leavesr
045500110209     c                   else
045600110209     c                   eval      $ksb(5)=wcodice
045700110209     c                   endif
045800110209     c                   endif
045900110210     c
046000110209     c
046100110209     c                   ENDSR
046200990215      *--------------------------------------------------------------------*
046300990215      * CONTROLLI VIDEO 0
046400990215      *--------------------------------------------------------------------*
046500990215     C     Ctrd00        BEGSR
046600040129      *
046700040129      * Se richiesta "Creazione file dettaglio":
046800040129      * - deve essere specificato "Stampa uso interno: 'S'"
046900040129      * - gli altri flag della videata devono essere impostati 'N'
047000040129if  1c                   IF        V1Ccfd = 'S'
047100050906     c                             or V1Ccfd = 'P'
047200040129      *
047300040129     c                   clear                   wErr
047400040129sel 2c                   select
047500101118w   2c*****              when      V1Cusi = 'N'   and v1cusn='N'
047600101118w   2c                   when      v1cusn='N'
047700040129     c                   eval      wErr   = -43
047800040129w   2c                   when      V1Ctoi = 'S'
047900040129     c                   eval      wErr   = 44
048000040129w   2c                   when      V1Cind = 'S'
048100040129     c                   eval      wErr   = 45
048200040129w   2c                   when      V1Ctod = 'S'
048300040129     c                   eval      wErr   = 46
048400040129w   2c                   when      V1Cina = 'S'
048500040129     c                   eval      wErr   = 47
048600040129w   2c                   when      V1Ctoa = 'S'
048700040129     c                   eval      wErr   = 48
048800040129w   2c                   when      V1Cint = 'S'
048900040129     c                   eval      wErr   = 49
049000040129w   2c                   when      V1Cton = 'S'
049100040129     c                   eval      wErr   = 50
049200101118w   2c****               when      V1Ccrp = 'S'
049300101118     c****               eval      wErr   = 51
049400090320w   2c                   when      V1Ccrn = 'S'
049500090320     c                   eval      wErr   = 52
049600040129e   2c                   endsl
049700040129      *
049800040129if  2c                   if        wErr     <> *zeros
049900040129sel 3c                   select
050000040129w   3c                   when      wErr      < *zeros
050100040129     c                   eval      V1Cmsg    = Msg(25)
050200040129     c                   eval      wErr      = wErr * (-1)
050300040129w   3c                   when      wErr      > *zeros
050400040129     c                   eval      V1Cmsg    = Msg(26)
050500040129e   3c                   endsl
050600040129     c                   eval      *in(wErr) = *on
050700040129     c                   seton                                        28  90
050800040129     c                   goto      EndCT0
050900040129e   2c                   endif
051000040129e   1c                   ENDIF
051100040129      *
051200050610     c* controllo cod DISTRETTO
051300050610     c                   exsr      CtrDISTR
051400050610     c   90              goto      EndCT0
051500990215     c
051600990215     c* Cod Area      se elezionato
051700050610     c                   exsr      CtrAREA
051800050610     c   90              goto      EndCT0
051900090326     c
052000101118     c***                if        (v1cusi='S' or v1ccrp='S') and
052100101118     c***                          (v1cusn='S' or v1ccrn='S')
052200101118     c***                eval      V1Cmsg    = Msg(20)
052300101118     c***                seton                                        28  90
052400101118     c***                endif
052500990312     c
052600990215     c     endct0        ENDSR
052700050610      *--------------------------------------------------------------------*
052800050610     c* Cod Distretto se elezionato
052900050610      *--------------------------------------------------------------------*
053000050610     c     CtrDISTR      BEGSR
053100050610     c                   clear                   v1ddis
053200050610     c                   clear                   v2ddis
053300050613     c  n01              eval      v2ddis='T=Tutti'
053400050610     c     '?'           scan      v1cdis                                 90
053500050610     c                   if        *in90
053600050610     c                   movel     codut         §kut
053700050610     c                   movel     '17'          §cod
053800050610     c                   clear                   §key
053900050610     c                   call      'X§TABER'
054000050610     c                   parm                    §kut
054100050610     c                   parm                    §cod
054200050610     c                   parm                    §key
054300050610     c                   parm                    §des             25
054400050610     c                   movel     §key          v1cdis
054500050610     c                   movel     §des          v1ddis
054600050610     c                   movel     §des          v2ddis
054700050610     c                   seton                                        40
054800050610     c                   goto      endctrDISTR
054900050610     c                   endif
055000050610     c
055100050614    1c     v1cdis        ifne      *blanks
055200050614    2c                   if        (v1cdis<>*blanks and *in01) or
055300050614     c                             (v1cdis<>*blanks and not *in01 and
055400050614     c                              v1cdis<>'T')
055500050613     c
055600050610     c                   movel     '17'          cod
055700050610     c                   movel(p)  v1cdis        key
055800050610     c     ktab          chain     tabel                              30
055900050614    2c                   if        *in30 OR tblflg<>' '
056000050610     c                   movel     msg(9)        v1cmsg
056100050610     c                   seton                                        289040
056200050610     c                   goto      endctrDISTR
056300050614    2c                   endif
056400050610     c                   movel     tbluni        v1ddis
056500050610     c                   movel     tbluni        v2ddis
056600050610     C* Non è possibile richiedere il distretto '9' - SEDE
056700050614    2C                   if        v1cdis='9'
056800050610     c                   movel     msg(11)       v1cmsg
056900050610     c                   seton                                        289040
057000050610     c                   goto      endctrDISTR
057100050614    2c                   endif
057200050614     c
057300050614     c
057400050610     c* controllo se esiste in organigramma
057500050610     c     v1cdis        chain     azorg02l                           30
057600050610     c                   dow       not *in30 and orgfva<>' '
057700050610     c     v1cdis        reade     azorg02l                               30
057800050610     c                   enddo
057900050610     c                   if        *in30
058000050610     c                   movel     msg(12)       v1cmsg
058100050610     c                   seton                                        289040
058200050610     c                   goto      endctrDISTR
058300050610     c                   endif
058400050613     c                   endif
058500050610     c* Controllo se abilitato al distretto, solo se non impostata area
058600050610     c                   if        v1care<=*zeros
058700050610     c     v1cdis        lookup    $dig                                   30
058800050613     c                   if        not *in30 and not *in01  and v1cdis='T'
058900050613     c* Se messo "T" controllo  se può
059000050613     c                   if        o31gtd='T'
059100050613     c                   seton                                        30
059200050613     c                   endif
059300050613     c                   endif
059400050613     c
059500050613     c                   if        not *in30
059600050610     c                   movel     msg(28)       v1cmsg
059700050610     c                   seton                                        289040
059800050610     c                   goto      endctrDISTR
059900050610     c                   endif
060000050610     c                   endif
060100050610     c                   endif
060200050610     c     EndctrDISTR   ENDSR
060300050610      *--------------------------------------------------------------------*
060400050610     c* controllo cod AREA
060500050610      *--------------------------------------------------------------------*
060600050610     c     CtrAREA       BEGSR
060700050610     c                   clear                   v1dare
060800050610     c                   clear                   v2dare
060900050610     c     '?'           scan      v1care                                 90
061000070110    1c                   if        *in90
061100050610     c                   movel     codut         §kut
061200050610     c                   movel     '05'          §cod
061300050610     c                   clear                   §key
061400050610     c                   call      'X§TABER'
061500050610     c                   parm                    §kut
061600050610     c                   parm                    §cod
061700050610     c                   parm                    §key
061800050610     c                   parm                    §des
061900050610     c                   movel     §key          v1care
062000050610     c                   movel     §des          v1dare
062100050610     c                   movel     §des          v2dare
062200050610     c                   seton                                        41
062300050610     c                   goto      endctrAREA
062400070110    1c                   endif
062500070111    1c     v1care        ifeq      *zeros
062600050610     c                   clear                   v1care
062700070111    1c                   endif
062800070111    1c     v1care        ifne      *blanks
062900050610     c     v1care        andne     *zeros
063000050610     c                   movel     '05'          cod
063100050610     c                   movel(p)  v1care        key
063200050610     c     ktab          chain     tabel                              30
063300070111    2c                   if        *in30 OR tblflg<>' '
063400050610     c                   movel     msg(10)       v1cmsg
063500050610     c                   seton                                        289041
063600050610     c                   goto      endctrAREA
063700070111    2c                   endif
063800050610     c                   movel     tbluni        v1dare
063900050610     c                   movel     tbluni        v2dare
064000050610     c
064100070111    2c     v1care        ifle      *zeros
064200050610     c                   clear                   ware
064300050610     c                   else
064400050610     c                   movel     v1care        ware
064500070111    2c                   endif
064600050610     c
064700070111    2c                   if        v1cdis<>' '
064800050610     c     KORG          chain     azorg02l                           30
064900070111    3c                   dow       not *in30 and orgfva<>' '
065000050610     c     KORG          reade     azorg02l                               30
065100070111    3c                   enddo
065200070111    3c                   if        *in30
065300050610     c                   movel     msg(13)       v1cmsg
065400050610     c                   seton                                        289041
065500050610     c                   goto      endctrAREA
065600070111    3c                   endif
065700070111    2c                   endif
065800070110     c
065900050610     c* Controllo se utente abilitato
066000050610     c     v1care        lookup    $arg                                   30
066100070111    2c                   if        not *in30
066200050610     c                   movel     msg(29)       v1cmsg
066300050610     c                   seton                                        289041
066400050610     c                   goto      endctrAREA
066500070111    2c                   endif
066600050610     c
066700050610     c
066800070111    2c                   if        v1cdis=' '
066900050610     c     *loval        setll     azorg02l
067000050610     c                   read      azorg02l                               30
067100070110    3c                   dow       not *in30  and v1cdis=*blanks
067200070110    4c                   if        orgcar<>ware
067300050610     c     korg1         setgt     azorg02l
067400050610     c                   else
067500070110    5c     orgfva        ifeq      *blanks
067600050610     c                   movel     orgfl3        v1cdis
067700070110    5c                   endif
067800070110    4c                   endif
067900050610     c                   read      azorg02l                               30
068000070110    3c                   enddo
068100050610     c
068200050610     C* Non è possibile richiedere il distretto '9' - SEDE
068300070110    3C                   if        v1cdis='9'
068400050610     c                   clear                   v1cdis
068500050610     c                   movel     msg(14)       v1cmsg
068600050610     c                   seton                                        289041
068700050610     c                   goto      endctrAREA
068800070110    3c                   endif
068900050610     c* non trovata area sull'organigramma
069000070110    3C                   if        v1cdis=' '
069100050610     c                   clear                   v1cdis
069200050610     c                   movel     msg(15)       v1cmsg
069300050610     c                   seton                                        289041
069400050610     c                   goto      endctrAREA
069500070110    3c                   endif
069600050610     c
069700050610     c
069800050610     c* Decodifico codice distretto
069900050610     c                   movel     '17'          cod
070000050610     c                   movel(p)  v1cdis        key
070100050610     c     ktab          chain     tabel                              30
070200050610     c  N30              movel     tbluni        v1ddis
070300050610     c  N30              movel     tbluni        v2ddis
070400050610     c                   seton                                        90
070500070111    2c                   endif
070600050610     c
070700070111    1c                   endif
070800050610     c     endctrAREA    ENDSR
070900990215      *--------------------------------------------------------------------*
071000990215      * CONTROLLO SE EFFETTUATA ALMENO UNA SELEZIONE
071100990215      *--------------------------------------------------------------------*
071200990215     c     CTRSEL        begsr
071300990215     c* Effettuare almeno una selezione
071400990215     c     v1cprs        ifeq      0
071500990309     c     v1cter        andeq     0
071600990309     c     v1cfil        andeq     0
071700990215     c     v1cpsl        andeq     0
071800990215     c     v1ctad        andeq     *blanks
071900990215     c     v1cdcd        andeq     0
072000990215     c     v1ctpl        andeq     ' '
072100990215     c     v1ctca        andeq     ' '
072200990215     c     v1crgr        andeq     ' '
072300990916     c     v1crg2        andeq     ' '
072400990916     c     v1crg3        andeq     ' '
072500990215     c     v1clnp        andeq     0
072600990215     c     v1clna        andeq     0
072700990215     c     v1cdcd        andeq     0
072800990219     c     v1cdhd        andeq     0
072900990219     c     v1ccch        andeq     *blanks
073000990315     c     v1cklb        andeq     0
073100990315     c     v1cksb        andeq     0
073200990315     c     v1dksb        andeq     *blanks
073300990315     c     v1crsb        andeq     *blanks
073400990315     c     v1cklm        andeq     0
073500990315     c     v1cksm        andeq     0
073600990315     c     v1dksm        andeq     *blanks
073700990315     c     v1crsm        andeq     *blanks
073800990315     c     v1ckld        andeq     0
073900990315     c     v1cksd        andeq     0
074000990315     c     v1dksd        andeq     *blanks
074100990315     c     v1crsd        andeq     *blanks
074200060124     c     v1cval        andeq     *blanks
074300060125     c     v1cpkd        andeq     0
074400990215     c                   eval      v1cmsg=msg(1)
074500990215     c                   seton                                        2890
074600990215     c                   endif
074700990215     c                   endsr
074800981203      *--------------------------------------------------------------------*
074900990212      * CONTROLLI VIDEO 1
075000981203      *--------------------------------------------------------------------*
075100981203     C     Ctrd01        BEGSR
075200050610     c** controllo i campi  distretto a area
075300050610     c                   exsr      CtrDISTR
075400050610     c   90              goto      endct1
075500050610     c
075600050610     c                   exsr      CtrAREA
075700050610     c   90              goto      endct1
075800050610     c
075900050708     c* controllo se effettuata almeno una selezione
076000990215     c                   exsr      CTRSEL
076100990315     c   90              seton                                        54
076200990215     c   90              goto      endct1
076300990212     c
076400990212     c
076500990212     c* P.O. Responsabile
076600990212     c                   clear                   v1dprs
076700990212     c                   if        v1cprs>0
076800990212     c                   eval      wfil=v1cprs
076900990212     c                   exsr      CTRPO
077000050610     c   90              seton                                        57
077100990212     c   90              goto      endct1
077200990212     c                   eval      v1dprs=orgdes
077300050708     c
077400050708     c* Se immesso anche terminal responsabile, errore
077500050708     c                   if        v1cter>0
077600050708     c                   seton                                        579028
077700050708     c                   movel     MSG(33)       v1cmsg
077800050708     c                   goto      endct1
077900050708     c                   endif
078000050610     c
078100990212     c
078200050610     c* Se selezionato distretto o area controllo se
078300050610     c* congruente, sia per sede che filiale
078400050610     c                   if        v1cord='2'
078500050610     c* controllo se utente abilitato
078600050708     c**                 movel     v1cprs        w003a             3
078700050708     c**   w003a         lookup    $pog                                   30
078800050708     c**                 if        not *in30
078900050708     c**                 seton                                        579028
079000050708     c**                 movel     MSG(30)       v1cmsg
079100050708     c**                 goto      endct1
079200050708     c**                 endif
079300050610     c
079400990219     c                   if        orgfl3<>v1cdis  and v1cdis<>*blanks
079500050610     c                   seton                                        579028
079600990219     c                   movel     MSG(18)       v1cmsg
079700990219     c                   goto      endct1
079800990219     c                   endif
079900990219     c                   movel     orgcar        w003a             3
080000990219     c                   if        w003a<>v1care  and v1care<>*blanks
080100050610     c                   seton                                        579028
080200990219     c                   movel     MSG(19)       v1cmsg
080300990219     c                   goto      endct1
080400990219     c                   endif
080500990219     c                   endif
080600990212     c                   endif
080700990309     c
080800990309     c* terminal Responsabile
080900990309     c                   clear                   v1dter
081000990309     c                   if        v1cter>0
081100050610     c
081200990309     c                   eval      wfil=v1cter
081300990309     c                   exsr      CTRPO
081400990309     c   90              seton                                        54
081500990309     c   90              goto      endct1
081600990309     c                   eval      v1dter=orgdes
081700990315     c
081800990315     c* Deve essere un terminal di partenza
081900990315     c                   clear                   fnlv55ds
082000990315     c                   movel     'L'           d55tla
082100990315     c                   movel     'P'           d55tpt
082200990315     c                   z-add     v1cter        d55lin
082300990315     c                   z-add     dateu         d55drf
082400990315     c                   call      'FNLV55R'
082500990315     c                   parm                    fnlv55ds
082600990315     c                   if        d55tfp<>v1cter
082700990315     c                   seton                                        549028
082800990315     c                   movel     MSG(21)       v1cmsg
082900990315     c                   goto      endct1
083000990315     c                   endif
083100050610     c*
083200050610     c* Solo per ordinamento "2"
083300050610     c                   if        v1cord='2'
083400050610     c* se immesso terminal non immettere l'area
083500050610     c                   if        v1care<>*blanks and v1care<>*zeros
083600050610     c                   seton                                        549028
083700050610     c                   movel     MSG(31)       v1cmsg
083800050610     c                   goto      endct1
083900050610     c                   endif
084000050610     c
084100050610     c* controllo se utente abilitato al lancio del p.o.
084200050708     c**                 movel     v1cter        w003a             3
084300050708     c**   w003a         lookup    $pog                                   30
084400050708     c**                 if        not *in30
084500050708     c**                 seton                                        549028
084600050708     c**                 movel     MSG(30)       v1cmsg
084700050708     c**                 goto      endct1
084800050708     c**                 endif
084900050610     c                   if        orgfl3<>v1cdis  and v1cdis<>*blanks
085000050610     c                   seton                                        549028
085100050610     c                   movel     MSG(32)       v1cmsg
085200050610     c                   goto      endct1
085300050610     c                   endif
085400050610     c                   endif
085500050610     c                   endif
085600060208     c* Il tipo responsabile è richiedibile soltanto se immesso il teminal
085700060208     c*  o il p.o.
085800060208     c                   if        v1cprs=0 and v1cter=0
085900060208     c                   if        v1ctr1<>' ' or v1ctr2<>' ' or v1ctr3<>' '
086000060208     c                   movel     MSG(34)       v1cmsg
086100060208     c                   seton                                        549028
086200060208     c                   goto      endct1
086300060208     c                   endif
086400060208     c                   endif
086500981203
086600990212     c* P.O. Apertura C.A.
086700990212     c                   clear                   v1dfil
086800990212     c                   if        v1cfil>0
086900990212     c                   eval      wfil=v1cfil
087000990212     c                   exsr      CTRPO
087100050610     c   90              seton                                        58
087200990212     c   90              goto      endct1
087300990212     c                   eval      v1dfil=orgdes
087400990219     c
087500050610     c                   if        v1cord='1'
087600050610     c
087700050610     c* Se selezionato distretto o area controllo se
087800050610     c* congruente
087900990219     c                   if        orgfl3<>v1cdis  and v1cdis<>*blanks
088000050610     c                   seton                                        589028
088100990219     c                   movel     MSG(18)       v1cmsg
088200990219     c                   goto      endct1
088300990212     c                   endif
088400990219     c                   movel     orgcar        w003a             3
088500990219     c                   if        w003a<>v1care  and v1care<>*blanks
088600050610     c                   seton                                        589028
088700990219     c                   movel     MSG(19)       v1cmsg
088800990219     c                   goto      endct1
088900990219     c                   endif
089000990219     c                   endif
089100990219     c                   endif
089200990212     c
089300990212     c*  Anno apertura C.A.
089400990212     c                   if        v1caac<100 and v1caac>90
089500990212     c                   movel     19            v1caac
089600990212     c                   endif
089700990212     c                   if        v1caac<100 and v1caac<90 and v1caac>0
089800990212     c                   movel     20            v1caac
089900990212     c                   endif
090000990212
090100990212     c* P.O. per selezione
090200990212     c                   clear                   v1dpsl
090300990212     c                   if        v1cpsl>0
090400990212     c                   eval      wfil=v1cpsl
090500990212     c                   exsr      CTRPO
090600990212     c   90              seton                                        43
090700990212     c   90              goto      endct1
090800990212     c                   eval      v1dpsl=orgdes
090900050610     c* Controllo se utente abilitato
091000050708     c**                 movel     v1cpsl        w003a             3
091100050708     c**   w003a         lookup    $pog                                   30
091200050708     c**                 if        not *in30
091300050708     c**                 seton                                        439028
091400050708     c**                 movel     MSG(30)       v1cmsg
091500050708     c**                 goto      endct1
091600050708     c**                 endif
091700050610     c
091800060208     c                   else
091900060208     c                   if        v1ctsl<>' '
092000060208     c                   seton                                        439028
092100060208     c                   movel     MSG(35)       v1cmsg
092200060208     c                   goto      endct1
092300990212     c                   endif
092400060208     c                   endif
092500990212     c
092600990212     c* Tipo Anomalia
092700990212     c*  ricerca
092800990212     c                   clear                   v1dtad
092900990212     c     '?'           scan      v1ctad                                 90
093000990212     c                   clear                   tibs02ds
093100990212     c                   if        *in90
093200990212     C                   MOVEL     'R'           t02mod
093300990212     C                   MOVEL     knsif         t02sif
093400990212     C                   MOVEL     'TAD'         t02cod
093500990212      *
093600990212     C                   CALL      'TIBS02R'
093700990212     C                   PARM                    KPJBA
093800990212     C                   PARM                    TIBS02DS
093900990212      *
094000990212     C                   IF        T02err = *BLANKS
094100990212     C                   movel     T02uni        DTAD
094200990212     C                   movel     T02ke1        v1ctad
094300990212     C                   movel     §TADdesc      v1dtad
094400990212     c                   else
094500990212     c                   clear                   v1dtad
094600990212     c                   endif
094700990212     c
094800990212     c                   seton                                        44
094900990212     c                   goto      endct1
095000990212     c                   endif
095100990212     c
095200990212     c*  controllo
095300990212     c                   if        v1ctad<>'  '
095400990212     c                   clear                   tibs02ds
095500990212     C                   MOVEL     'C'           t02mod
095600990212     C                   MOVEL     knsif         t02sif
095700990212     C                   MOVEL     'TAD'         t02cod
095800990212     c                   movel     v1ctad        t02ke1
095900990212      *
096000990212     C                   CALL      'TIBS02R'
096100990212     C                   PARM                    KPJBA
096200990212     C                   PARM                    TIBS02DS
096300990212      *
096400990212     C                   IF        T02err = *BLANKS
096500990212     C                   movel     T02uni        DTAD
096600990212     c                   movel     §taddesc      v1dtad
096700990212     c
096800990212      * Se Anomalia per Eventi non è valida
096900060601      * si accetta solo un evento se ha il calcolo della responsabilità
097000060601     C                   IF        §TADragr = 'E'  and §tadcare<>'S'
097100990212     C                   seton                                        442890
097200990212     c                   movel     MSG(3)        v1cmsg
097300990218     c                   goto      endct1
097400990212     C                   ENDIF
097500990212     c                   else
097600990212     C                   seton                                        442890
097700990212     c                   movel     MSG(4)        v1cmsg
097800990218     c                   goto      endct1
097900990212     C                   ENDIF
098000990212     C                   ENDIF
098100990218     c* Causale Chiusura
098200990218     c*  ricerca
098300990218     c                   clear                   v1dcch
098400990218     c     '?'           scan      v1ccch                                 90
098500990218     c                   clear                   tibs02ds
098600990218     c                   if        *in90
098700990218     C                   MOVEL     'R'           t02mod
098800990218     C                   MOVEL     knsif         t02sif
098900990218     C                   MOVEL     'CCH'         t02cod
099000990218      *
099100990218     C                   CALL      'TIBS02R'
099200990218     C                   PARM                    KPJBA
099300990218     C                   PARM                    TIBS02DS
099400990218      *
099500990218     C                   IF        T02err = *BLANKS
099600990218     C                   movel     T02uni        DCCH
099700990218     C                   movel     T02ke1        v1ccch
099800990218     C                   movel     §CCHdesc      v1dcch
099900990218     c                   endif
100000990218     c
100100990218     c                   seton                                        53
100200990218     c                   goto      endct1
100300990218     c                   endif
100400990218     c
100500990218     c*  controllo
100600990218     c                   if        v1ccch<>'  '
100700990218     c                   clear                   tibs02ds
100800990218     C                   MOVEL     'C'           t02mod
100900990218     C                   MOVEL     knsif         t02sif
101000990218     C                   MOVEL     'CCH'         t02cod
101100990218     c                   movel     v1ccch        t02ke1
101200990218      *
101300990218     C                   CALL      'TIBS02R'
101400990218     C                   PARM                    KPJBA
101500990218     C                   PARM                    TIBS02DS
101600990218      *
101700990218     C                   IF        T02err = *BLANKS
101800990218     C                   movel     T02uni        DCCH
101900990218     c                   movel     §cchdesc      v1dcch
102000990218     c                   else
102100990218     C                   seton                                        532890
102200990218     c                   movel     MSG(16)       v1cmsg
102300990218     c                   goto      endct1
102400990218     c                   endif
102500990218     c                   endif
102600990212     c
102700990212     c* Data apertura C.A.
102800990212     c     v1cdcd        ifeq      0
102900990212     c     v1cdca        andgt     0
103000990212     C                   seton                                        452890
103100990212     c                   movel     MSG(5)        v1cmsg
103200990218     c                   goto      endct1
103300990212     C                   ENDIF
103400990212     c
103500990212     c                   clear                   wamgdcd
103600990212     c                   clear                   wamgdca
103700990212     c     v1cdcd        ifgt      0
103800990212     C                   move      V1cdcd        G02DAT
103900990212     C                   eval      g02err = *blanks
104000990212     C                   call      'XSRDA8'
104100990212     C                   parm                    WLBDAT
104200990212      *
104300990212     C                   if        g02err = '1'
104400990212     C                   seton                                        284590
104500990212     C                   movel     msg(5)        v1cmsg
104600990218     c                   goto      endct1
104700990212     c                   else
104800990212     c                   eval      wamgdcd=g02inv
104900990217     c                   eval      v1cdcd =g02dat
105000990212     c                   endif
105100990212     c                   endif
105200990212     c
105300990212     c     v1cdca        ifgt      0
105400990212     C                   move      V1cdca        G02DAT
105500990212     C                   eval      g02err = *blanks
105600990212     C                   call      'XSRDA8'
105700990212     C                   parm                    WLBDAT
105800990212      *  Errore
105900990212     C                   if        g02err = '1'
106000990212     C                   seton                                        284690
106100990212     C                   movel     msg(5)        v1cmsg
106200990212     c                   goto      endct1
106300990212     c                   else
106400990212     c                   eval      wamgdca=g02inv
106500990217     c                   eval      v1cdca =g02dat
106600990212     c
106700990212     c     wamgdca       iflt      wamgdcd
106800990212     C                   seton                                        284690
106900990212     C                   movel     msg(6)        v1cmsg
107000990212     c                   goto      endct1
107100990212     c                   endif
107200990212     c                   endif
107300990212     c                   endif
107400990218     c* Data chiusura C.A.
107500990218     c     v1cdhd        ifeq      0
107600990218     c     v1cdha        andgt     0
107700990218     C                   seton                                        512890
107800990218     c                   movel     MSG(5)        v1cmsg
107900990218     c                   goto      endct1
108000990218     C                   ENDIF
108100990218     c
108200990218     c                   clear                   wamgdhd
108300990218     c                   clear                   wamgdha
108400990218     c     v1cdhd        ifgt      0
108500990218     C                   move      V1cdhd        G02DAT
108600990218     C                   eval      g02err = *blanks
108700990218     C                   call      'XSRDA8'
108800990218     C                   parm                    WLBDAT
108900990218      *
109000990218     C                   if        g02err = '1'
109100990218     C                   seton                                        285190
109200990218     C                   movel     msg(5)        v1cmsg
109300990218     c                   goto      endct1
109400990218     c                   else
109500990218     c                   eval      wamgdhd=g02inv
109600990218     c                   eval      v1cdhd =g02dat
109700990218     c                   endif
109800990218     c                   endif
109900990218     c
110000990218     c     v1cdha        ifgt      0
110100990218     C                   move      V1cdha        G02DAT
110200990218     C                   eval      g02err = *blanks
110300990218     C                   call      'XSRDA8'
110400990218     C                   parm                    WLBDAT
110500990218      *  Errore
110600990218     C                   if        g02err = '1'
110700990218     C                   seton                                        285290
110800990218     C                   movel     msg(5)        v1cmsg
110900990218     c                   goto      endct1
111000990218     c                   else
111100990218     c                   eval      wamgdha=g02inv
111200990218     c                   eval      v1cdha =g02dat
111300990218     c
111400990218     c     wamgdha       iflt      wamgdhd
111500990218     C                   seton                                        285290
111600990218     C                   movel     msg(6)        v1cmsg
111700990218     c                   goto      endct1
111800990218     c                   endif
111900990218     c                   endif
112000990218     c                   endif
112100990218     c
112200990218     c
112300990218     c* Se richieste solo le aperte non posso chiedere causale chiusura
112400990218     c*  o date chiusura
112500990218     c                   if        v1ctca='A'
112600990218     c                   if        v1cdhd>0 or v1ccch<>*blanks
112700990218     C                   seton                                        285190
112800990218     C                   movel     msg(17)       v1cmsg
112900990218     c                   goto      endct1
113000990218     c                   endif
113100990218     c                   endif
113200990315     c
113300990315     c* Controllo Beneficiario se immesso
113400110209     c                   clear                   $ksb(1)
113500990315     c                   eval      wkl=v1cklb
113600990315     c                   eval      wks=v1cksb
113700990315     c                   eval      wdks=v1dksb
113800990315     c                   eval      wrs=v1crsb
113900990315     c                   EXSR      codice
114000990315     c                   eval      v1cklb=wkl
114100990315     c                   eval      v1cksb=wks
114200990315     c                   eval      v1dksb=wdks
114300990315     c   90              seton                                        55
114400990315     c   90              goto      endct1
114500110209     c
114600110209     c                   if        wcodice>0
114700110209     c                   eval      Indx=%lookup(wcodice:$ksb)
114800110209     c                   if        Indx>0
114900110209     c                   movel     msg(36)       v1cmsg
115000110209     c                   seton                                        289055
115100110209     c                   leavesr
115200110209     c                   else
115300110209     c                   eval      $ksb(1)=wcodice
115400110209     c                   endif
115500110209     c                   endif
115600000215     c* STampo dati di liquidazione solo se ordinamento =1
115700000215     c     v1csdl        ifeq      'S'
115800000215     c     v1cord        andeq     '2'
115900000215     C                   seton                                        285690
116000000215     C                   movel     msg(24)       v1cmsg
116100000215     c                   goto      endct1
116200000215     c                   endif
116300990212     c
116400990212     c     ENDCT1        ENDSR
116500990212      *--------------------------------------------------------------------*
116600990212      * CONTROLLI VIDEO
116700990212      *--------------------------------------------------------------------*
116800990212     C     CTRD02        BEGSR
116900110210     c                   setoff                                       11
117000990215     c                   exsr      CTRSEL
117100990215     c   90              seton                                        47
117200990215     c   90              goto      endct2
117300990212     c
117400990212     c* LInea partenza bolla
117500990212     c                   clear                   v1dlnp
117600990212     c                   if        v1clnp>0
117700990212     c                   eval      wfil=v1clnp
117800990212     c                   exsr      CTRPO
117900990212     c   90              seton                                        47
118000990212     c   90              goto      endct2
118100990212     c                   eval      v1dlnp=orgdes
118200110210     c                   seton                                        11
118300990212     c                   endif
118400990212     c
118500990212     c
118600990212     c* Linea arrivo   bolla
118700990212     c                   clear                   v1dlna
118800990212     c                   if        v1clna>0
118900990212     c                   eval      wfil=v1clna
119000990212     c                   exsr      CTRPO
119100990212     c   90              seton                                        48
119200990212     c   90              goto      endct2
119300990212     c                   eval      v1dlna=orgdes
119400110210     c                   seton                                        11
119500990212     c                   endif
119600990212     c
119700990212     c* Data spedizione
119800990212     c     v1cdsd        ifeq      0
119900990212     c     v1cdsa        andgt     0
120000990212     C                   seton                                        492890
120100990212     c                   movel     MSG(5)        v1cmsg
120200990212     C                   ENDIF
120300990212     c
120400990212     c                   clear                   wamgdsd
120500990212     c                   clear                   wamgdsa
120600990212     c     v1cdsd        ifgt      0
120700990212     C                   move      V1cdsd        G02DAT
120800990212     C                   eval      g02err = *blanks
120900990212     C                   call      'XSRDA8'
121000990212     C                   parm                    WLBDAT
121100990212      *
121200990212     C                   if        g02err = '1'
121300990212     C                   seton                                        285090
121400990212     C                   movel     msg(5)        v1cmsg
121500990212     c                   else
121600990212     c                   eval      wamgdsd=g02inv
121700990930     c                   eval      v1cdsd=g02dat
121800110210     c                   seton                                        11
121900990212     c                   endif
122000990212     c                   endif
122100990212     c
122200990212     c     v1cdsa        ifgt      0
122300990212     C                   move      V1cdsa        G02DAT
122400990212     C                   eval      g02err = *blanks
122500990212     C                   call      'XSRDA8'
122600990212     C                   parm                    WLBDAT
122700990212      *  Errore
122800990212     C                   if        g02err = '1'
122900990212     C                   seton                                        285090
123000990212     C                   movel     msg(5)        v1cmsg
123100990212     c                   goto      endct2
123200990212     c                   else
123300990212     c                   eval      wamgdsa=g02inv
123400990930     c                   eval      v1cdsa=g02dat
123500110210     c                   seton                                        11
123600990212     c
123700990212     c     wamgdsa       iflt      wamgdsd
123800990212     C                   seton                                        285090
123900990212     C                   movel     msg(6)        v1cmsg
124000990212     c                   goto      endct2
124100990212     c                   endif
124200990212     c                   endif
124300990212     c                   endif
124400990315     c* Controllo Mittente se immesso
124500990315     c                   eval      wkl=v1cklm
124600990315     c                   eval      wks=v1cksm
124700990315     c                   eval      wdks=v1dksm
124800990315     c                   eval      wrs=v1crsm
124900990315     c                   EXSR      codice
125000990315     c                   eval      v1cklm=wkl
125100990315     c                   eval      v1cksm=wks
125200990315     c                   eval      v1dksm=wdks
125300990315     c   90              seton                                        51
125400990315     c   90              goto      endct2
125500990315     c* Controllo Destinatario
125600990315     c                   eval      wkl=v1ckld
125700990315     c                   eval      wks=v1cksd
125800990315     c                   eval      wdks=v1dksd
125900990315     c                   eval      wrs=v1crsd
126000990315     c                   EXSR      codice
126100990315     c                   eval      v1ckld=wkl
126200990315     c                   eval      v1cksd=wks
126300990315     c                   eval      v1dksd=wdks
126400990315     c   90              seton                                        52
126500990315     c   90              goto      endct2
126600110210     c
126700110210     c                   if        v1cksd>0 or v1crsd<>*blanks or v1cksm>0 or
126800110210     c                             v1crsm<>*blanks
126900110210     c                   seton                                        11
127000110210     c                   endif
127100990212
127200990212     c     ENDCT2        ENDSR
127300990212      *------------------------------------------------------------------------*
127400000907      * CPNTROLLO P.O.
127500990212      *------------------------------------------------------------------------*
127600990212     C     CTRPO         BEGSR
127700990212     c     wfil          chain     azorg01l                           30
127800990212     c     *in30         ifeq      *on
127900990212     c     orgfva        orne      ' '
128000990212     c     orgfag        orne      'A'
128100990212     c     orgfag        andne     'F'
128200990212     c                   eval      v1cmsg=msg(2)
128300990212     c                   seton                                        2890
128400990212     c                   endif
128500990212     c                   ENDSR
128600990315      *--------------------------------------------------------------------*
128700990315      * CONTROLLI CODICE O RAGIONE SOCIALE
128800990315      *--------------------------------------------------------------------*
128900990315     C     CODICE        BEGSR
129000990315     c
129100110209     c* Se immesso codice non immettere ragione sociale e viceversa
129200990315     c                   if        wkl>0  OR wks>0 OR wdks<>*blanks
129300990315     c                   if        wrs<>*blanks
129400990315     c                   eval      v1cmsg=msg(22)
129500990315     c                   seton                                        2890
129600990315     c                   goto      endcodice
129700990315     c                   endif
129800990315     c                   endif
129900110210     c
130000110210     c* se ho immesso la ragione sociale senza, codice avviso per premere F7
130100110210     c                   if        wdks<>*blanks and wks=0 and not *inkg
130200110210     c                   eval      v1cmsg=msg(37)
130300110210     c                   seton                                        2890
130400110210     c                   goto      endcodice
130500110210     c                   endif
130600990315     c
130700990315     c* F7 - ricerca alfabetica
130800110210     c*  la attivo se codice vuoto e ragione sociale piena
130900110210     c                   if        *inkg and wdks<>*blanks and wks=0
131000990315     C* DESCRIZIONE AZIENDA
131100990315     C                   MOVEL     rsut          PAXDUT           30
131200990315     C* PAXSTA=9 ESCLUDO ANNULLATI
131300990315     C                   Z-ADD     9             PAXSTA
131400990315     C* CAPOCONTO CLIENTI
131500050609     C                   Z-ADD     dutkci        PAXCCC
131600990315     C* CODICE DA RICERCARE: SE IMMESSA LNP CERCO SOLO AL SUO INTERNO
131700990315     C                   MOVEL(P)  Wkl           ACOKSC
131800000907     C                   MOVEL     Wkl           PAXFLR
131900990315     C                   MOVEL(P)  Wdks          paxdmt
132000000907     C                   z-add     1             PAXNUM
132100990315     C*
132200000907     C                   CALL      'XALFA3BR'
132300990315     c                   PARM                    PAXDUT
132400990315     C                   PARM                    CODUT
132500990315     C                   PARM                    PAXDMT
132600990315     C                   PARM                    PAXCCC
132700990315     C                   PARM                    PAXSTA            1 0
132800990315     C                   PARM                    PAXFLR           90
132900990315     C                   PARM                    PAXDIT            3
133000000907     C                   PARM                    PAXNUM            2 0
133100000907     C                   PARM                    PAXKCM           80
133200000907     C                   PARM                    PAXKSM          140
133300000907     C                   PARM                    PAXKDM           60
133400990315     C*
133500990315     C* NON TROVATO O PREMUTO CMD12
133600990315    2C     PAXSTA        IFGT      -1
133700000907     c                   movel     paxksm        acoksc
133800000907     c                   movel     acoksc        wkl
133900990315     c                   move      acoksc        wks
134000990315     c                   movel     paxdmt        wdks
134100990315     c                   endif
134200000907     c                   seton                                          90
134300000907     c                   goto      endcodice
134400990315     c                   endif
134500990315     c
134600990315     c* Controllo codice
134700990315     c                   if        wkl=0 and wks>0
134800990315     c                   eval      v1cmsg=msg(23)
134900990315     c                   seton                                        2890
135000990315     c                   endif
135100990315     c
135200990315     c                   if        wkl>0 and wks>0
135300990315     c                   clear                   tibs69ds
135400990315     c                   movel     wcodice       I69kac
135500990315      *
135600990315     C                   CALL      'TIBS69R'
135700990315     C                   PARM                    tibs69DS
135800990315     C                   PARM                    DS_cnaco
135900990315     C                   PARM                    DS_cnind
136000990315     C                   PARM                    DS_cnclp
136100990315     C                   PARM                    DS_fncls
136200990315    2c                   if        o69err=' '
136300990315     c                   movel     acorag        wdks
136400990315     c                   else
136500990315     c                   eval      v1cmsg=msg(23)
136600990315     c                   seton                                        2890
136700990315     c                   endif
136800990315     c                   endif
136900990315     c
137000990315     c     ENDCODICE     ENDSR
137100981203      *------------------------------------------------------------------------*
137200981203      * ROUTINE INIZIALE
137300981203      *------------------------------------------------------------------------*
137400981203     C     *INZSR        BEGSR
137500981203      *
137600981203     C     *ENTRY        PLIST
137700981203     C                   PARM                    KPJBA
137800981203      *
137900050609     c     *dtaara       define    §azute        azuteds
138000050609     c     *dtaara       define    §datiute      ddatiute
138100050609     c                   in(E)     *dtaara
138200050609    1c                   If        %error  or RSUT = *blanks
138300050609     c                   Clear                   Tibs34ds
138400050609     c                   Call      'TIBS34R'
138500050609     c                   Parm                    Tibs34ds
138600050609     c                   In        *dtaara
138700050609    1c                   EndIf
138800050609
138900050609     c                   Clear                   wabi
139000050609     c                   Clear                   v1cmsg
139100050609     c                   Clear                   dLat
139200050609
139300050609      * Verifica errori e autorità profilo
139400050609s   1c                   Select
139500050609      * se ho errori nei dati utente esco dal pgm
139600050609w   1c                   When      DutErr = 'E'
139700050609     c                   GoTo      Fine
139800050609      * se non c'è l'abilitazione
139900050609      * --> se 1° livello, abilitazioni al terminal
140000050609      *     se 2° livello, abilitazioni al punto operativo
140100050609      *     se sede è impossibile ma se capita mando a fine il pgm
140200050609w   1c                   When      UteAut = *Blanks
140300050609if  2c                   If        DutLpo = '1'
140400050609     c                   Eval      wabi   = 'TP'
140500050609e   2c                   EndIf
140600050609if  2c                   If        DutLpo = '2'
140700050609     c                   Eval      wabi   = 'PO'
140800050609e   2c                   EndIf
140900050609if  2c                   If        DutLpo = 'S'
141000050609     c                   GoTo      Fine
141100050609e   2c                   EndIf
141200050609      * carica le abilitazioni del profilo
141300050609x   1c                   Other
141400050609     c                   Movel     UteFaf        Dute01
141500050609     c                   If        §UteEca <> *Blanks
141600050609     c                   Eval      wabi = §UteEca
141700050609     c                   Else
141800050609     c                   Eval      wabi = UteAut
141900050609     c                   EndIf
142000050609e   1c                   EndSl
142100050609
142200050609      * controllo se ok l'abilitazione dell'utente
142300050609     c                   Clear                   Tibs02ds
142400050609     c                   Eval      T02Mod = 'C'
142500050609     c                   Eval      T02Sif = knsif
142600050609     c                   Eval      T02Cod = 'LAT'
142700050609     c                   Movel(p)  wabi          T02Ke1
142800050609     c                   Call      'TIBS02R'
142900050609     c                   Parm                    kpjba
143000050609     c                   Parm                    Tibs02ds
143100050609if  1c                   If        T02Err = *Blanks
143200050609     c                   Eval      dLat = T02Uni
143300050609e   1c                   EndIf
143400050609      * errore o non abilitato
143500050609if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
143600050609     c                   Seton                                        28
143700050609     C                   movel     msg(27)       v1cmsg
143800050609   x1c                   Else
143900050609      * Reperimento dei P.O. gestibili dall'utente
144000050609     c                   clear                   TRUL31DS
144100050609     c                   clear                   TRUL31DS2
144200050609     c                   eval      I31abi = wabi
144300050609     c                   eval      I31cdi = DUTdis
144400050609     c                   eval      I31car = DUTare
144500050609     c                   eval      I31cpo = DUTpou
144600050609     c                   call      'TRUL31R'
144700050609     c                   parm                    KPJBA
144800050609     c                   parm                    TRUL31DS
144900050609     c                   parm                    TRUL31DS2
145000050609if  2c                   if        O31pog > *zeros
145100050610     c                   movea     O31pog        $pog
145200050610     c                   movea     O31arg        $arg
145300050610     c                   movea     O31dig        $dig
145400050609x   2c                   else
145500050609     c                   Seton                                        28
145600050609     c                   movel     MSG(27)       v1cmsg
145700050609e   2c                   endif
145800050609    1c                   endif
145900050609     c
146000050609     C                   Z-ADD     1             CODUT
146100050609     C**                 CALL      'X§PARUT'
146200050609     C**                 PARM                    UT§DSE
146300050609     C**                 MOVEL     RAGUT         RSUT
146400050609     C**                 MOVEL     REC80         CNCR80
146500981203
146600990315      * reperisco data e ora
146700990315     C                   TIME                    W0140
146800990315      * UDATE IN GGMMAAAA
146900990315     C                   MOVE      W0140         WDTGIO
147000990315      * UDATE IN AAAAMMGG
147100990315     C                   Z-ADD     WDTGIO        G02DAT
147200990315     C                   MOVEL     *BLANK        G02ERR
147300990315     C                   CALL      'XSRDA8'
147400990315     C                   PARM                    WLBDAT
147500990315     C                   MOVEL     G02INV        DATEU
147600981203      *
147700990215     c     ktab          klist
147800990215     c                   kfld                    codut
147900990215     c                   kfld                    cod
148000990215     c                   kfld                    key
148100990215     c     kORG          klist
148200990215     c                   kfld                    v1cdis
148300990215     c                   kfld                    ware
148400990215     c     KORG1         klist
148500990215     c                   kfld                    orgfl3
148600990215     c                   kfld                    orgcar
148700981203     C                   ENDSR
148800990215      *------------------------------------------------------------------------*
148900990215      * SOTTOMENTTO ELABORAZIONE
149000990215      *------------------------------------------------------------------------*
149100990212     c     CONFERMA      BEGSR
149200990212     c                   clear                   fidn43ds
149300990219     c                   eval      i43sap = v1csap
149400990219     c                   eval      i43dst = v1cdis
149500990309     c                   eval      i43ind = v1cind
149600990215     c                   eval      i43are = v1care
149700990309     c                   eval      i43ina = v1cina
149800990315     c                   eval      i43ter = v1cter
149900990311     c                   eval      i43int = v1cint
150000990311     c                   eval      i43ntw = v1cntw
150100990406     c                   eval      i43tod = v1ctod
150200990406     c                   eval      i43toa = v1ctoa
150300990406     c                   eval      i43ton = v1cton
150400101118     c***                if        v1cusi='S' or v1cusn='S'
150500101118     c                   if        v1cusn='S'
150600090320     c                   eval      i43usi = 'S'
150700090320     c                   else
150800090320     c                   eval      i43usi = 'N'
150900090320     c                   endif
151000990512     c   01              eval      i43toi = v1ctoi
151100990512     c  N01              eval      i43toi = v1ctsn
151200101118     c*****              if        v1ccrp='S' or v1ccrn='S'
151300101118     c                   if        v1ccrn='S'
151400090320     c                   eval      i43crp = 'S'
151500090320     c                   else
151600090320     c                   eval      i43crp = 'N'
151700090320     c                   endif
151800040129     c                   eval      i43cfd = v1ccfd
151900990215     c                   eval      i43ord = v1cord
152000990212     c                   eval      i43prs = v1cprs
152100060206     c                   eval      i43tr1 = v1ctr1
152200060206     c                   eval      i43tr2 = v1ctr2
152300060206     c                   eval      i43tr3 = v1ctr3
152400990212     c                   eval      i43psl = v1cpsl
152500990212     c                   eval      i43tsl = v1ctsl
152600990212     c                   eval      i43fil = v1cfil
152700990212     c                   eval      i43aac = v1caac
152800990212     c                   eval      i43tad = v1ctad
152900990212     c                   eval      i43rgr = v1crgr
153000990916     c                   eval      i43rg2 = v1crg2
153100990916     c                   eval      i43rg3 = v1crg3
153200990212     c                   eval      i43tca = v1ctca
153300990212     c                   eval      i43tpl = v1ctpl
153400990212     c                   eval      i43dcd = v1cdcd
153500990212     c                   eval      i43dca = v1cdca
153600990218     c                   eval      i43dhd = v1cdhd
153700990218     c                   eval      i43dha = v1cdha
153800000215     c                   eval      i43sdl = v1csdl
153900040726     c                   eval      i43sdm = v1csdm
154000990212     c                   eval      i43amgdcd = wamgdcd
154100990212     c                   eval      i43amgdca = wamgdca
154200990218     c                   eval      i43amgdhd = wamgdhd
154300990218     c                   eval      i43amgdha = wamgdha
154400990218     c                   eval      i43cch = v1ccch
154500990218     c                   eval      i43lnp = v1clnp
154600990212     c                   eval      i43lna = v1clna
154700990212     c                   eval      i43dsd = v1cdsd
154800990212     c                   eval      i43dsa = v1cdsa
154900990212     c                   eval      i43amgdsd = wamgdsd
155000990212     c                   eval      i43amgdsa = wamgdsa
155100990315     c                   z-add     v1cksd        i43ksd
155200990315     c                   movel     v1ckld        i43ksd
155300990315     c                   movel     v1crsd        i43rsd
155400990315     c                   z-add     v1cksm        i43ksm
155500990315     c                   movel     v1cklm        i43ksm
155600990315     c                   movel     v1crsm        i43rsm
155700990315     c                   z-add     v1cksb        i43ksb
155800990315     c                   movel     v1cklb        i43ksb
155900990315     c                   movel     v1crsb        i43rsb
156000060124     c                   movel     v1cval        i43val
156100060125     c                   movel     v1cpkd        i43pkd
156200110210     c                   z-add     v1cksb2       i43ksb2
156300110210     c                   movel     v1cklb2       i43ksb2
156400110210     c                   z-add     v1cksb3       i43ksb3
156500110210     c                   movel     v1cklb3       i43ksb3
156600110210     c                   z-add     v1cksb4       i43ksb4
156700110210     c                   movel     v1cklb4       i43ksb4
156800110210     c                   z-add     v1cksb5       i43ksb5
156900110210     c                   movel     v1cklb5       i43ksb5
157000990315     c
157100990219     c* SE ditretto e area vuoti e ho impostato fil per ord 1 li riempo
157200050614     c                   if        i43ord='1' and i43fil>0 AND simfel=0 and
157300990219     c                             i43dst=*blanks and i43are=*blanks
157400990219     c     i43fil        chain     azorg01l                           30
157500990219     c                   movel     orgfl3        i43dst
157600990219     c                   movel     orgcar        i43are
157700990219     c                   endif
157800090320     c* solo per stampa vecchia
157900101118     c***                if        v1ccrN<>'S' and v1cusn<>'S'
158000101118     c***                if        i43ord='2' and i43prs>0 AND simfel=0 and
158100101118     c***                          i43dst=*blanks and i43are=*blanks
158200101118     c***  i43prs        chain     azorg01l                           30
158300101118     c***                movel     orgfl3        i43dst
158400101118     c***                movel     orgcar        i43are
158500101118     c***                endif
158600101118     c***                endif
158700990212      *
158800040209     c                   clear                   Wpgm             10
158900990507      *
159000990212     c                   movel     fidn43ds      kpjbu
159100990212     c
159200101118     c***                if        v1ccrN<>'S' and v1cusn<>'S'
159300090323     c* richiamo pgm aggiornamento distretto/area su report se richiesta
159400090323     c* creazione dati
159500101118     C***                if        i43crp = 'S'
159600101118     c***                eval      kcoaz='DN39'
159700101118     c***                eval      Wpgm = 'BCH10     '
159800101118     c***                call      Wpgm
159900101118     c***                parm                    kpjba
160000101118     c***                endif
160100090323     c
160200101118     c***                eval      kcoaz='DN44'
160300050708     c**                 eval      Wpgm = 'FIDN44R   '
160400101118     c***                eval      Wpgm = 'BCH10     '
160500101118     c***                else
160600090326     c                   eval      kcoaz='DN47'
160700090326     c**                 eval      Wpgm = 'FIDN47R   '
160800090326     c                   eval      Wpgm = 'BCH10     '
160900101118     c***                endif
161000040209     c                   call      Wpgm
161100990212     c                   parm                    kpjba
161200990212     c                   endsr
161300981203      *---------------------------------------------------------------------------------------------
161400981203** MSG  Lungh. 78                                                            *
161500990212Effettuare almeno una selezione                                                1
161600990212Punto operativo inesistente                                                    2
161700060601Tipo anomalia valido soltanto per NON eventi e Eventi con calcolo Responsabil.
161800990215Tipo anomalia inesistente                                                      4
161900990215Data errata                                                                    5
162000990215Data AL   MINORE  data DAL                                                     6
162100990215Ci sono errori nella videata di selezioni C.A.: premere F8 per correggere      7
162200990215Ci sono errori nella videata di selezioni bolle: premere F8 per correggere     8
162300990215Cod.Distretto errato                                                           9
162400990215Cod.Area errato                                                               10
162500990215Cod.Distretto 9 - SEDE  non selezinabile                                      11
162600070110Cod.Distretto non presente nell'organigramma                                  12
162700990215Cod.Area non congruente col distretto selezionato                             13
162800990218cod.Area di distretto 9 - SEDE  non selezionabile                             14
162900990218Cod.Area non presente nell'organigramma                                       15
163000990218Causale chiusura errata                                                       16
163100990218Se richieste C.A. aperte inutile selezione data o causale chiusura            17
163200990219Il P.O. non fa parte del distretto selezionato                                18
163300990219Il P.O. non fa parte dell'area selezionata                                    19
163400090326Elaborare "vecchio" e "nuovo" separatamente                                   20
163500990315Il P.O. non e' un terminal                                                    21
163600990315Immettere o il codice o la ragione sociale                                    22
163700990315Codice errato o inesistente                                                   23
163800000215Stampa dati di liquidazione solo per ordinamento= '1'                         24
163900040129Impostare ad 'S' per creare il file DETTAGLIO                                 25
164000040129Impostare ad 'N' per creare il file DETTAGLIO                                 26
164100050609Utente non abilitato alla Stampa delle C.A.                                   27
164200050610Utente non abilitato a richiedere il distretto indicato                       28
164300050610Utente non abilitato a richiedere l'Area indicata                             29
164400050610Utente non abilitato a richiedere il P.O. indicato                            30
164500050614Se immessa area non immettere terminal responsabile (o viceversa)             31
164600050610Il Terminal non fa parte del distretto selezionato                            32
164700050708Richiedere il terminal responsabile OPPURE il p.O. responsabile               33
164800060208Se immesso il"tipo responsabile" immettere anche il terminal o il P.O respons.34
164900060208Se immesso il "tipo selezione" immettere anche il P.O. per selezione          35
165000110209Codice beneficiario già inserito                                              36
165100110210Per la ricerca alfabetica premere F7                                          37
