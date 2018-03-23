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
014000170228     c                   clear                   v1crit
014100060120     c                   clear                   v1cval
014200060125     c                   clear                   v1cpkd
014300110209     c                   clear                   v1cklb2
014400110209     c                   clear                   v1cksb2
014500110209     c                   clear                   v1dksb2
014600110209     c                   clear                   v1cklb3
014700110209     c                   clear                   v1cksb3
014800110209     c                   clear                   v1dksb3
014900110209     c                   clear                   v1cklb4
015000110209     c                   clear                   v1cksb4
015100110209     c                   clear                   v1dksb4
015200110209     c                   clear                   v1cklb5
015300110209     c                   clear                   v1cksb5
015400110209     c                   clear                   v1dksb5
015500990311     c                   eval      v1cind='N'
015600990311     c                   eval      v1cina='N'
015700990311     c                   eval      v1cint='N'
015800990311     c                   eval      v1cord='2'
015900990406     c                   eval      v1ctod='N'
016000990406     c                   eval      v1ctoa='N'
016100990406     c                   eval      v1cton='N'
016200990406     c                   eval      v1ctod='N'
016300101118     c****               eval      v1cusi='N'
016400090401     c                   eval      v1cusn='S'
016500090325     c
016600990406     c                   eval      v1ctoi='N'
016700101118     c****               eval      v1ccrp='N'
016800090325     c                   eval      v1ccrn='N'
016900040129     c                   eval      v1ccfd='N'
017000990319     c
017100990319      * IMPOSTO IL P.O. IN GESTIONE
017200050906     c                   eval      *in35 = *off
017300990319     C                   IF        SIMFEL > 0
017400050613     c                   eval      v2ddis='T=Tutti'
017500050609     C                   IF        dutlpo = '2' or dutlpo = *blanks
017600050609     c                   movel     dutpou        v1cpsl
017700990319     C                   eval      *in06 = *on
017800990319    2C                   ENDIF
017900990319   X2C                   ELSE
018000990319     C                   eval      *in01 = *on
018100050906     c                   if        %subst(KNMUS:1:3) = 'EDP'
018200080306     c                             or %subst(KNMUS:1:3) = 'QUA'
018300150604     c                             or %subst(KNMUS:1:3) = 'RIC'
018400050906     c                   eval      *in35 = *on
018500050906     c                   endif
018600990319    2C                   ENDIF
018700101111     c                   if        KNMUS = 'GEN002'
018800101111     c                   eval      *in36 = *on
018900101111     c                   endif
019000101111     c
019100071204      * se non acceso il 35 verifico l'utente con utente responsabile ufficio danni
019200130806     c***********        clear                   tibs02ds
019300130806     C***********        MOVEL     'C'           t02mod
019400130806     C***********        MOVEL     knsif         t02sif
019500130806     C***********        MOVEL     'STD'         t02cod
019600130806     c***********        movel     '1'           t02ke1
019700130806     c***********        movel     'EUR'         t02ke2
019800071204      *
019900130806     C***********        CALL      'TIBS02R'
020000130806     C***********        PARM                    KPJBA
020100130806     C***********        PARM                    TIBS02DS
020200071204      *
020300130806     C***********        IF        T02err = *BLANKS
020400130806     C***********        movel     T02uni        DSTD
020500130806
020600150210     c                   if        KNMUS = 'ASS020'
020700150210      * non deve vedere il lancio del file WFRDN50F come GEN002
020800150210     c                   eval      *in36 = *on
020900071204     c                   endif
021000130806
021100130806     c***********        endif
021200071204     c
021300981203     c*
021400990215     c     FOR00         tag
021500990215     c                   if        simfel=0
021600990215     c                   exfmt     dn43d00
021700040129     c                   setoff                                       2890
021800040129     c                   movea     wZeros        *in(40)
021900990215     c* F3 = FINE
022000990215     c   KC              goto      fine
022100990215     c**
022200990215     c                   exsr      ctrd00
022300990215     c   90              goto      for00
022400990215     c                   endif
022500990215     c**
022600990315     c                   seton                                        54
022700990215     c
022800990212     c     FOR01         tag
022900990212     c                   exfmt     dn43d01
023000990212     c**
023100990212     c                   setoff                                       289040
023200990212     c                   setoff                                       414243
023300990212     c                   setoff                                       444546
023400990212     c                   setoff                                       474849
023500990218     c                   setoff                                       505152
023600990315     c                   setoff                                       535455
023700050610     c                   setoff                                       565758
023800981203     c                   clear                   v1cmsg
023900981203     c* F3 = FINE
024000990212     c   KC              goto      fine
024100990215     c* F12 = ritorno
024200990215     c   KL              goto      for00
024300990212     c
024400110209     c
024500990212     c* F8 - selezioni sulle bolle
024600990212     c   KH              goto      for02
024700990212     c*
024800990212      * Controlli
024900990212     c                   exsr      CTRD01
025000990212     c   90              goto      for01
025100990212     c                   exsr      CTRD02
025200990212     c   90              movel     msg(8)        v1cmsg
025300990212     c   90              goto      for01
025400110210     c
025500110210     c* F1 - selezioni Beneficiari aggiuntivi
025600110210     c                   if        *inka
025700110210     c                   exsr      Gesd03
025800110210     c                   goto      for01
025900110210     c                   endif
026000990212     c*
026100990212     c
026200990212     c                   if        not *inkh  AND not *inkf
026300990512     c                   seton                                        54
026400990212     c                   goto      for01
026500990212     c                   endif
026600990212     c*
026700990212     c* Nessuna selezione
026800990212     c
026900990212     c   KF              exsr      conferma
027000990315     c                   if        *inkf AND simfel=0
027100990315     c                   goto      iniziosede
027200990315     c                   endif
027300990315     c                   if        *inkf AND simfel>0
027400990315     c                   goto      iniziofil
027500990315     c                   endif
027600990212     c**
027700990212     c     FOR02         tag
027800990212     c**
027900990212     c                   exfmt     dn43d02
028000990212     c
028100990212     c                   setoff                                       289040
028200990212     c                   setoff                                       414243
028300990212     c                   setoff                                       444546
028400990212     c                   setoff                                       474849
028500990315     c                   setoff                                       505152
028600990212     c                   clear                   v1cmsg
028700990212     c* F3 - FINE
028800990212     c   KC              goto      fine
028900990212     c*
029000990212     c* F8 - selezioni sulle C.A.
029100990512     c   KH              seton                                        54
029200990212     c   KH              goto      for01
029300990212     c**
029400990215      * Controlli
029500990212     c                   exsr      CTRD02
029600990212     c   90              goto      for02
029700990215     c
029800990215     c                   exsr      CTRD01
029900990215     c   90              movel     msg(7)        v1cmsg
030000990215     c   90              goto      for02
030100990212     c
030200990212     c                   if        not *inkh  AND not *inkf
030300990212     c                   goto      for02
030400990212     c                   endif
030500990212     c
030600990212     c*
030700981203      * F6 = Conferma Stampa
030800990212     c   KF              EXSR      conferma
030900990323     c                   if        *inkf AND simfel=0
031000990323     c                   goto      iniziosede
031100990323     c                   endif
031200990323     c                   if        *inkf AND simfel>0
031300990323     c                   goto      iniziofil
031400990323     c                   endif
031500990212     c
031600981203      *
031700990212     c     fine          tag
031800990212     c                   clear                   tibs02ds
031900990212     C                   MOVEL     'C'           t02tla
032000990212      *
032100990212     C                   CALL      'TIBS02R'
032200990212     C                   PARM                    KPJBA
032300990212     C                   PARM                    TIBS02DS
032400990315     C                   clear                   TIBS69DS
032500990315     c                   eval      i69tla='C'
032600990315      *
032700990315     C                   CALL      'TIBS69R'
032800990315     C                   PARM                    tibs69DS
032900990315     C                   PARM                    DS_cnaco
033000990315     C                   PARM                    DS_cnind
033100990315     C                   PARM                    DS_cnclp
033200990315     C                   PARM                    DS_fncls
033300981203     c                   eval      *inlr = *on
033400110209      *--------------------------------------------------------------------*
033500110209      * Gestione video 3  - beneficiari aggiuntivi
033600110209      *--------------------------------------------------------------------*
033700110209     C     Gesd03        BEGSR
033800110209     c**
033900110209     c     FOR03         tag
034000110209     c**
034100110209     c                   exfmt     dn43d03
034200110209     c
034300110209     c                   setoff                                       289040
034400110209     c                   setoff                                       414243
034500110209     c                   setoff                                       444546
034600110209     c                   setoff                                       474849
034700110209     c                   clear                   v1cmsg
034800110209     c**
034900110209      * Controlli
035000110209     c                   exsr      CTRD03
035100110209     c   90              goto      for03
035200110211     c
035300110211     c* Esco solo se premuto F8
035400110211     c                   if        not *inkh
035500110211     c                   goto      for03
035600110211     c                   endif
035700110209     c
035800110210     c* se inserito almeno un codice metto F1 in reverse immage
035900110210     c                   if        v1cksb2>0 or v1cksb3>0 or v1cksb4>0 or
036000110210     c                             v1cksb5>0
036100110210     c                   seton                                        10
036200110210     c                   endif
036300110210     c
036400110209     c                   ENDSR
036500110209      *--------------------------------------------------------------------*
036600110209      * CONTROLLI VIDEO 3
036700110209      *--------------------------------------------------------------------*
036800110209     C     Ctrd03        BEGSR
036900110209     c                   clear                   $ksb(2)
037000110209     c                   clear                   $ksb(3)
037100110209     c                   clear                   $ksb(4)
037200110209     c                   clear                   $ksb(5)
037300110210     c                   setoff                                       10
037400110209     c
037500110209     c*  Beneficiario 2
037600110209     c                   eval      wkl=v1cklb2
037700110209     c                   eval      wks=v1cksb2
037800110209     c                   eval      wdks=v1dksb2
037900110210     c                   eval      wrs=v1crsb
038000110209     c                   EXSR      codice
038100110209     c                   eval      v1cklb2=wkl
038200110209     c                   eval      v1cksb2=wks
038300110209     c                   eval      v1dksb2=wdks
038400110210     c   90              seton                                        40
038500110210     c   90              leavesr
038600110209     c                   if        wcodice>0
038700110209     c                   eval      Indx=%lookup(wcodice:$ksb)
038800110209     c                   if        Indx>0
038900110209     c                   movel     msg(36)       v1cmsg
039000110209     c                   seton                                        289040
039100110209     c                   leavesr
039200110209     c                   else
039300110209     c                   eval      $ksb(2)=wcodice
039400110209     c                   endif
039500110209     c                   endif
039600110209     c
039700110209     c*  Beneficiario 3
039800110209     c                   eval      wkl=v1cklb3
039900110209     c                   eval      wks=v1cksb3
040000110209     c                   eval      wdks=v1dksb3
040100110210     c                   eval      wrs=v1crsb
040200110209     c                   EXSR      codice
040300110209     c                   eval      v1cklb3=wkl
040400110209     c                   eval      v1cksb3=wks
040500110209     c                   eval      v1dksb3=wdks
040600110210     c   90              seton                                        41
040700110210     c   90              leavesr
040800110209     c                   if        wcodice>0
040900110209     c                   eval      Indx=%lookup(wcodice:$ksb)
041000110209     c                   if        Indx>0
041100110209     c                   movel     msg(36)       v1cmsg
041200110209     c                   seton                                        289041
041300110209     c                   leavesr
041400110209     c                   else
041500110209     c                   eval      $ksb(3)=wcodice
041600110209     c                   endif
041700110209     c                   endif
041800110209     c*  Beneficiario 4
041900110209     c                   eval      wkl=v1cklb4
042000110209     c                   eval      wks=v1cksb4
042100110209     c                   eval      wdks=v1dksb4
042200110210     c                   eval      wrs=v1crsb
042300110209     c                   EXSR      codice
042400110209     c                   eval      v1cklb4=wkl
042500110209     c                   eval      v1cksb4=wks
042600110209     c                   eval      v1dksb4=wdks
042700110210     c   90              seton                                        42
042800110210     c   90              leavesr
042900110209     c                   if        wcodice>0
043000110209     c                   eval      Indx=%lookup(wcodice:$ksb)
043100110209     c                   if        Indx>0
043200110209     c                   movel     msg(36)       v1cmsg
043300110209     c                   seton                                        289042
043400110209     c                   leavesr
043500110209     c                   else
043600110209     c                   eval      $ksb(3)=wcodice
043700110209     c                   endif
043800110209     c                   endif
043900110209     c*  Beneficiario 5
044000110209     c                   eval      wkl=v1cklb5
044100110209     c                   eval      wks=v1cksb5
044200110209     c                   eval      wdks=v1dksb5
044300110210     c                   eval      wrs=v1crsb
044400110209     c                   EXSR      codice
044500110209     c                   eval      v1cklb5=wkl
044600110209     c                   eval      v1cksb5=wks
044700110209     c                   eval      v1dksb5=wdks
044800110210     c   90              seton                                        43
044900110210     c   90              leavesr
045000110209     c                   if        wcodice>0
045100110209     c                   eval      Indx=%lookup(wcodice:$ksb)
045200110209     c                   if        Indx>0
045300110209     c                   movel     msg(36)       v1cmsg
045400110209     c                   seton                                        289043
045500110209     c                   leavesr
045600110209     c                   else
045700110209     c                   eval      $ksb(5)=wcodice
045800110209     c                   endif
045900110209     c                   endif
046000110210     c
046100110209     c
046200110209     c                   ENDSR
046300990215      *--------------------------------------------------------------------*
046400990215      * CONTROLLI VIDEO 0
046500990215      *--------------------------------------------------------------------*
046600990215     C     Ctrd00        BEGSR
046700040129      *
046800040129      * Se richiesta "Creazione file dettaglio":
046900040129      * - deve essere specificato "Stampa uso interno: 'S'"
047000040129      * - gli altri flag della videata devono essere impostati 'N'
047100040129if  1c                   IF        V1Ccfd = 'S'
047200050906     c                             or V1Ccfd = 'P'
047300040129      *
047400040129     c                   clear                   wErr
047500040129sel 2c                   select
047600101118w   2c*****              when      V1Cusi = 'N'   and v1cusn='N'
047700101118w   2c                   when      v1cusn='N'
047800040129     c                   eval      wErr   = -43
047900040129w   2c                   when      V1Ctoi = 'S'
048000040129     c                   eval      wErr   = 44
048100040129w   2c                   when      V1Cind = 'S'
048200040129     c                   eval      wErr   = 45
048300040129w   2c                   when      V1Ctod = 'S'
048400040129     c                   eval      wErr   = 46
048500040129w   2c                   when      V1Cina = 'S'
048600040129     c                   eval      wErr   = 47
048700040129w   2c                   when      V1Ctoa = 'S'
048800040129     c                   eval      wErr   = 48
048900040129w   2c                   when      V1Cint = 'S'
049000040129     c                   eval      wErr   = 49
049100040129w   2c                   when      V1Cton = 'S'
049200040129     c                   eval      wErr   = 50
049300101118w   2c****               when      V1Ccrp = 'S'
049400101118     c****               eval      wErr   = 51
049500090320w   2c                   when      V1Ccrn = 'S'
049600090320     c                   eval      wErr   = 52
049700040129e   2c                   endsl
049800040129      *
049900040129if  2c                   if        wErr     <> *zeros
050000040129sel 3c                   select
050100040129w   3c                   when      wErr      < *zeros
050200040129     c                   eval      V1Cmsg    = Msg(25)
050300040129     c                   eval      wErr      = wErr * (-1)
050400040129w   3c                   when      wErr      > *zeros
050500040129     c                   eval      V1Cmsg    = Msg(26)
050600040129e   3c                   endsl
050700040129     c                   eval      *in(wErr) = *on
050800040129     c                   seton                                        28  90
050900040129     c                   goto      EndCT0
051000040129e   2c                   endif
051100040129e   1c                   ENDIF
051200040129      *
051300050610     c* controllo cod DISTRETTO
051400050610     c                   exsr      CtrDISTR
051500050610     c   90              goto      EndCT0
051600990215     c
051700990215     c* Cod Area      se elezionato
051800050610     c                   exsr      CtrAREA
051900050610     c   90              goto      EndCT0
052000090326     c
052100101118     c***                if        (v1cusi='S' or v1ccrp='S') and
052200101118     c***                          (v1cusn='S' or v1ccrn='S')
052300101118     c***                eval      V1Cmsg    = Msg(20)
052400101118     c***                seton                                        28  90
052500101118     c***                endif
052600990312     c
052700990215     c     endct0        ENDSR
052800050610      *--------------------------------------------------------------------*
052900050610     c* Cod Distretto se elezionato
053000050610      *--------------------------------------------------------------------*
053100050610     c     CtrDISTR      BEGSR
053200050610     c                   clear                   v1ddis
053300050610     c                   clear                   v2ddis
053400050613     c  n01              eval      v2ddis='T=Tutti'
053500050610     c     '?'           scan      v1cdis                                 90
053600050610     c                   if        *in90
053700050610     c                   movel     codut         §kut
053800050610     c                   movel     '17'          §cod
053900050610     c                   clear                   §key
054000050610     c                   call      'X§TABER'
054100050610     c                   parm                    §kut
054200050610     c                   parm                    §cod
054300050610     c                   parm                    §key
054400050610     c                   parm                    §des             25
054500050610     c                   movel     §key          v1cdis
054600050610     c                   movel     §des          v1ddis
054700050610     c                   movel     §des          v2ddis
054800050610     c                   seton                                        40
054900050610     c                   goto      endctrDISTR
055000050610     c                   endif
055100050610     c
055200050614    1c     v1cdis        ifne      *blanks
055300050614    2c                   if        (v1cdis<>*blanks and *in01) or
055400050614     c                             (v1cdis<>*blanks and not *in01 and
055500050614     c                              v1cdis<>'T')
055600050613     c
055700050610     c                   movel     '17'          cod
055800050610     c                   movel(p)  v1cdis        key
055900050610     c     ktab          chain     tabel                              30
056000050614    2c                   if        *in30 OR tblflg<>' '
056100050610     c                   movel     msg(9)        v1cmsg
056200050610     c                   seton                                        289040
056300050610     c                   goto      endctrDISTR
056400050614    2c                   endif
056500050610     c                   movel     tbluni        v1ddis
056600050610     c                   movel     tbluni        v2ddis
056700050610     C* Non è possibile richiedere il distretto '9' - SEDE
056800050614    2C                   if        v1cdis='9'
056900050610     c                   movel     msg(11)       v1cmsg
057000050610     c                   seton                                        289040
057100050610     c                   goto      endctrDISTR
057200050614    2c                   endif
057300050614     c
057400050614     c
057500050610     c* controllo se esiste in organigramma
057600050610     c     v1cdis        chain     azorg02l                           30
057700050610     c                   dow       not *in30 and orgfva<>' '
057800050610     c     v1cdis        reade     azorg02l                               30
057900050610     c                   enddo
058000050610     c                   if        *in30
058100050610     c                   movel     msg(12)       v1cmsg
058200050610     c                   seton                                        289040
058300050610     c                   goto      endctrDISTR
058400050610     c                   endif
058500050613     c                   endif
058600050610     c* Controllo se abilitato al distretto, solo se non impostata area
058700050610     c                   if        v1care<=*zeros
058800050610     c     v1cdis        lookup    $dig                                   30
058900050613     c                   if        not *in30 and not *in01  and v1cdis='T'
059000050613     c* Se messo "T" controllo  se può
059100050613     c                   if        o31gtd='T'
059200050613     c                   seton                                        30
059300050613     c                   endif
059400050613     c                   endif
059500050613     c
059600050613     c                   if        not *in30
059700050610     c                   movel     msg(28)       v1cmsg
059800050610     c                   seton                                        289040
059900050610     c                   goto      endctrDISTR
060000050610     c                   endif
060100050610     c                   endif
060200050610     c                   endif
060300050610     c     EndctrDISTR   ENDSR
060400050610      *--------------------------------------------------------------------*
060500050610     c* controllo cod AREA
060600050610      *--------------------------------------------------------------------*
060700050610     c     CtrAREA       BEGSR
060800050610     c                   clear                   v1dare
060900050610     c                   clear                   v2dare
061000050610     c     '?'           scan      v1care                                 90
061100070110    1c                   if        *in90
061200050610     c                   movel     codut         §kut
061300050610     c                   movel     '05'          §cod
061400050610     c                   clear                   §key
061500050610     c                   call      'X§TABER'
061600050610     c                   parm                    §kut
061700050610     c                   parm                    §cod
061800050610     c                   parm                    §key
061900050610     c                   parm                    §des
062000050610     c                   movel     §key          v1care
062100050610     c                   movel     §des          v1dare
062200050610     c                   movel     §des          v2dare
062300050610     c                   seton                                        41
062400050610     c                   goto      endctrAREA
062500070110    1c                   endif
062600070111    1c     v1care        ifeq      *zeros
062700050610     c                   clear                   v1care
062800070111    1c                   endif
062900070111    1c     v1care        ifne      *blanks
063000050610     c     v1care        andne     *zeros
063100050610     c                   movel     '05'          cod
063200050610     c                   movel(p)  v1care        key
063300050610     c     ktab          chain     tabel                              30
063400070111    2c                   if        *in30 OR tblflg<>' '
063500050610     c                   movel     msg(10)       v1cmsg
063600050610     c                   seton                                        289041
063700050610     c                   goto      endctrAREA
063800070111    2c                   endif
063900050610     c                   movel     tbluni        v1dare
064000050610     c                   movel     tbluni        v2dare
064100050610     c
064200070111    2c     v1care        ifle      *zeros
064300050610     c                   clear                   ware
064400050610     c                   else
064500050610     c                   movel     v1care        ware
064600070111    2c                   endif
064700050610     c
064800070111    2c                   if        v1cdis<>' '
064900050610     c     KORG          chain     azorg02l                           30
065000070111    3c                   dow       not *in30 and orgfva<>' '
065100050610     c     KORG          reade     azorg02l                               30
065200070111    3c                   enddo
065300070111    3c                   if        *in30
065400050610     c                   movel     msg(13)       v1cmsg
065500050610     c                   seton                                        289041
065600050610     c                   goto      endctrAREA
065700070111    3c                   endif
065800070111    2c                   endif
065900070110     c
066000050610     c* Controllo se utente abilitato
066100050610     c     v1care        lookup    $arg                                   30
066200070111    2c                   if        not *in30
066300050610     c                   movel     msg(29)       v1cmsg
066400050610     c                   seton                                        289041
066500050610     c                   goto      endctrAREA
066600070111    2c                   endif
066700050610     c
066800050610     c
066900070111    2c                   if        v1cdis=' '
067000050610     c     *loval        setll     azorg02l
067100050610     c                   read      azorg02l                               30
067200070110    3c                   dow       not *in30  and v1cdis=*blanks
067300070110    4c                   if        orgcar<>ware
067400050610     c     korg1         setgt     azorg02l
067500050610     c                   else
067600070110    5c     orgfva        ifeq      *blanks
067700050610     c                   movel     orgfl3        v1cdis
067800070110    5c                   endif
067900070110    4c                   endif
068000050610     c                   read      azorg02l                               30
068100070110    3c                   enddo
068200050610     c
068300050610     C* Non è possibile richiedere il distretto '9' - SEDE
068400070110    3C                   if        v1cdis='9'
068500050610     c                   clear                   v1cdis
068600050610     c                   movel     msg(14)       v1cmsg
068700050610     c                   seton                                        289041
068800050610     c                   goto      endctrAREA
068900070110    3c                   endif
069000050610     c* non trovata area sull'organigramma
069100070110    3C                   if        v1cdis=' '
069200050610     c                   clear                   v1cdis
069300050610     c                   movel     msg(15)       v1cmsg
069400050610     c                   seton                                        289041
069500050610     c                   goto      endctrAREA
069600070110    3c                   endif
069700050610     c
069800050610     c
069900050610     c* Decodifico codice distretto
070000050610     c                   movel     '17'          cod
070100050610     c                   movel(p)  v1cdis        key
070200050610     c     ktab          chain     tabel                              30
070300050610     c  N30              movel     tbluni        v1ddis
070400050610     c  N30              movel     tbluni        v2ddis
070500050610     c                   seton                                        90
070600070111    2c                   endif
070700050610     c
070800070111    1c                   endif
070900050610     c     endctrAREA    ENDSR
071000990215      *--------------------------------------------------------------------*
071100990215      * CONTROLLO SE EFFETTUATA ALMENO UNA SELEZIONE
071200990215      *--------------------------------------------------------------------*
071300990215     c     CTRSEL        begsr
071400990215     c* Effettuare almeno una selezione
071500990215     c     v1cprs        ifeq      0
071600990309     c     v1cter        andeq     0
071700990309     c     v1cfil        andeq     0
071800990215     c     v1cpsl        andeq     0
071900990215     c     v1ctad        andeq     *blanks
072000990215     c     v1cdcd        andeq     0
072100990215     c     v1ctpl        andeq     ' '
072200990215     c     v1ctca        andeq     ' '
072300990215     c     v1crgr        andeq     ' '
072400990916     c     v1crg2        andeq     ' '
072500990916     c     v1crg3        andeq     ' '
072600990215     c     v1clnp        andeq     0
072700990215     c     v1clna        andeq     0
072800990215     c     v1cdcd        andeq     0
072900990219     c     v1cdhd        andeq     0
073000990219     c     v1ccch        andeq     *blanks
073100990315     c     v1cklb        andeq     0
073200990315     c     v1cksb        andeq     0
073300990315     c     v1dksb        andeq     *blanks
073400990315     c     v1crsb        andeq     *blanks
073500990315     c     v1cklm        andeq     0
073600990315     c     v1cksm        andeq     0
073700990315     c     v1dksm        andeq     *blanks
073800990315     c     v1crsm        andeq     *blanks
073900990315     c     v1ckld        andeq     0
074000990315     c     v1cksd        andeq     0
074100990315     c     v1dksd        andeq     *blanks
074200990315     c     v1crsd        andeq     *blanks
074300060124     c     v1cval        andeq     *blanks
074400060125     c     v1cpkd        andeq     0
074500990215     c                   eval      v1cmsg=msg(1)
074600990215     c                   seton                                        2890
074700990215     c                   endif
074800990215     c                   endsr
074900981203      *--------------------------------------------------------------------*
075000990212      * CONTROLLI VIDEO 1
075100981203      *--------------------------------------------------------------------*
075200981203     C     Ctrd01        BEGSR
075300050610     c** controllo i campi  distretto a area
075400050610     c                   exsr      CtrDISTR
075500050610     c   90              goto      endct1
075600050610     c
075700050610     c                   exsr      CtrAREA
075800050610     c   90              goto      endct1
075900050610     c
076000050708     c* controllo se effettuata almeno una selezione
076100990215     c                   exsr      CTRSEL
076200990315     c   90              seton                                        54
076300990215     c   90              goto      endct1
076400990212     c
076500990212     c
076600990212     c* P.O. Responsabile
076700990212     c                   clear                   v1dprs
076800990212     c                   if        v1cprs>0
076900990212     c                   eval      wfil=v1cprs
077000990212     c                   exsr      CTRPO
077100050610     c   90              seton                                        57
077200990212     c   90              goto      endct1
077300990212     c                   eval      v1dprs=orgdes
077400050708     c
077500050708     c* Se immesso anche terminal responsabile, errore
077600050708     c                   if        v1cter>0
077700050708     c                   seton                                        579028
077800050708     c                   movel     MSG(33)       v1cmsg
077900050708     c                   goto      endct1
078000050708     c                   endif
078100050610     c
078200990212     c
078300050610     c* Se selezionato distretto o area controllo se
078400050610     c* congruente, sia per sede che filiale
078500050610     c                   if        v1cord='2'
078600050610     c* controllo se utente abilitato
078700050708     c**                 movel     v1cprs        w003a             3
078800050708     c**   w003a         lookup    $pog                                   30
078900050708     c**                 if        not *in30
079000050708     c**                 seton                                        579028
079100050708     c**                 movel     MSG(30)       v1cmsg
079200050708     c**                 goto      endct1
079300050708     c**                 endif
079400050610     c
079500990219     c                   if        orgfl3<>v1cdis  and v1cdis<>*blanks
079600050610     c                   seton                                        579028
079700990219     c                   movel     MSG(18)       v1cmsg
079800990219     c                   goto      endct1
079900990219     c                   endif
080000990219     c                   movel     orgcar        w003a             3
080100990219     c                   if        w003a<>v1care  and v1care<>*blanks
080200050610     c                   seton                                        579028
080300990219     c                   movel     MSG(19)       v1cmsg
080400990219     c                   goto      endct1
080500990219     c                   endif
080600990219     c                   endif
080700990212     c                   endif
080800990309     c
080900990309     c* terminal Responsabile
081000990309     c                   clear                   v1dter
081100990309     c                   if        v1cter>0
081200050610     c
081300990309     c                   eval      wfil=v1cter
081400990309     c                   exsr      CTRPO
081500990309     c   90              seton                                        54
081600990309     c   90              goto      endct1
081700990309     c                   eval      v1dter=orgdes
081800990315     c
081900990315     c* Deve essere un terminal di partenza
082000990315     c                   clear                   fnlv55ds
082100990315     c                   movel     'L'           d55tla
082200990315     c                   movel     'P'           d55tpt
082300990315     c                   z-add     v1cter        d55lin
082400990315     c                   z-add     dateu         d55drf
082500990315     c                   call      'FNLV55R'
082600990315     c                   parm                    fnlv55ds
082700990315     c                   if        d55tfp<>v1cter
082800990315     c                   seton                                        549028
082900990315     c                   movel     MSG(21)       v1cmsg
083000990315     c                   goto      endct1
083100990315     c                   endif
083200050610     c*
083300050610     c* Solo per ordinamento "2"
083400050610     c                   if        v1cord='2'
083500050610     c* se immesso terminal non immettere l'area
083600050610     c                   if        v1care<>*blanks and v1care<>*zeros
083700050610     c                   seton                                        549028
083800050610     c                   movel     MSG(31)       v1cmsg
083900050610     c                   goto      endct1
084000050610     c                   endif
084100050610     c
084200050610     c* controllo se utente abilitato al lancio del p.o.
084300050708     c**                 movel     v1cter        w003a             3
084400050708     c**   w003a         lookup    $pog                                   30
084500050708     c**                 if        not *in30
084600050708     c**                 seton                                        549028
084700050708     c**                 movel     MSG(30)       v1cmsg
084800050708     c**                 goto      endct1
084900050708     c**                 endif
085000050610     c                   if        orgfl3<>v1cdis  and v1cdis<>*blanks
085100050610     c                   seton                                        549028
085200050610     c                   movel     MSG(32)       v1cmsg
085300050610     c                   goto      endct1
085400050610     c                   endif
085500050610     c                   endif
085600050610     c                   endif
085700060208     c* Il tipo responsabile è richiedibile soltanto se immesso il teminal
085800060208     c*  o il p.o.
085900060208     c                   if        v1cprs=0 and v1cter=0
086000060208     c                   if        v1ctr1<>' ' or v1ctr2<>' ' or v1ctr3<>' '
086100060208     c                   movel     MSG(34)       v1cmsg
086200060208     c                   seton                                        549028
086300060208     c                   goto      endct1
086400060208     c                   endif
086500060208     c                   endif
086600981203
086700990212     c* P.O. Apertura C.A.
086800990212     c                   clear                   v1dfil
086900990212     c                   if        v1cfil>0
087000990212     c                   eval      wfil=v1cfil
087100990212     c                   exsr      CTRPO
087200050610     c   90              seton                                        58
087300990212     c   90              goto      endct1
087400990212     c                   eval      v1dfil=orgdes
087500990219     c
087600050610     c                   if        v1cord='1'
087700050610     c
087800050610     c* Se selezionato distretto o area controllo se
087900050610     c* congruente
088000990219     c                   if        orgfl3<>v1cdis  and v1cdis<>*blanks
088100050610     c                   seton                                        589028
088200990219     c                   movel     MSG(18)       v1cmsg
088300990219     c                   goto      endct1
088400990212     c                   endif
088500990219     c                   movel     orgcar        w003a             3
088600990219     c                   if        w003a<>v1care  and v1care<>*blanks
088700050610     c                   seton                                        589028
088800990219     c                   movel     MSG(19)       v1cmsg
088900990219     c                   goto      endct1
089000990219     c                   endif
089100990219     c                   endif
089200990219     c                   endif
089300990212     c
089400990212     c*  Anno apertura C.A.
089500990212     c                   if        v1caac<100 and v1caac>90
089600990212     c                   movel     19            v1caac
089700990212     c                   endif
089800990212     c                   if        v1caac<100 and v1caac<90 and v1caac>0
089900990212     c                   movel     20            v1caac
090000990212     c                   endif
090100990212
090200990212     c* P.O. per selezione
090300990212     c                   clear                   v1dpsl
090400990212     c                   if        v1cpsl>0
090500990212     c                   eval      wfil=v1cpsl
090600990212     c                   exsr      CTRPO
090700990212     c   90              seton                                        43
090800990212     c   90              goto      endct1
090900990212     c                   eval      v1dpsl=orgdes
091000050610     c* Controllo se utente abilitato
091100050708     c**                 movel     v1cpsl        w003a             3
091200050708     c**   w003a         lookup    $pog                                   30
091300050708     c**                 if        not *in30
091400050708     c**                 seton                                        439028
091500050708     c**                 movel     MSG(30)       v1cmsg
091600050708     c**                 goto      endct1
091700050708     c**                 endif
091800050610     c
091900060208     c                   else
092000060208     c                   if        v1ctsl<>' '
092100060208     c                   seton                                        439028
092200060208     c                   movel     MSG(35)       v1cmsg
092300060208     c                   goto      endct1
092400990212     c                   endif
092500060208     c                   endif
092600990212     c
092700990212     c* Tipo Anomalia
092800990212     c*  ricerca
092900990212     c                   clear                   v1dtad
093000990212     c     '?'           scan      v1ctad                                 90
093100990212     c                   clear                   tibs02ds
093200990212     c                   if        *in90
093300990212     C                   MOVEL     'R'           t02mod
093400990212     C                   MOVEL     knsif         t02sif
093500990212     C                   MOVEL     'TAD'         t02cod
093600990212      *
093700990212     C                   CALL      'TIBS02R'
093800990212     C                   PARM                    KPJBA
093900990212     C                   PARM                    TIBS02DS
094000990212      *
094100990212     C                   IF        T02err = *BLANKS
094200990212     C                   movel     T02uni        DTAD
094300990212     C                   movel     T02ke1        v1ctad
094400990212     C                   movel     §TADdesc      v1dtad
094500990212     c                   else
094600990212     c                   clear                   v1dtad
094700990212     c                   endif
094800990212     c
094900990212     c                   seton                                        44
095000990212     c                   goto      endct1
095100990212     c                   endif
095200990212     c
095300990212     c*  controllo
095400990212     c                   if        v1ctad<>'  '
095500990212     c                   clear                   tibs02ds
095600990212     C                   MOVEL     'C'           t02mod
095700990212     C                   MOVEL     knsif         t02sif
095800990212     C                   MOVEL     'TAD'         t02cod
095900990212     c                   movel     v1ctad        t02ke1
096000990212      *
096100990212     C                   CALL      'TIBS02R'
096200990212     C                   PARM                    KPJBA
096300990212     C                   PARM                    TIBS02DS
096400990212      *
096500990212     C                   IF        T02err = *BLANKS
096600990212     C                   movel     T02uni        DTAD
096700990212     c                   movel     §taddesc      v1dtad
096800990212     c
096900990212      * Se Anomalia per Eventi non è valida
097000060601      * si accetta solo un evento se ha il calcolo della responsabilità
097100060601     C                   IF        §TADragr = 'E'  and §tadcare<>'S'
097200990212     C                   seton                                        442890
097300990212     c                   movel     MSG(3)        v1cmsg
097400990218     c                   goto      endct1
097500990212     C                   ENDIF
097600990212     c                   else
097700990212     C                   seton                                        442890
097800990212     c                   movel     MSG(4)        v1cmsg
097900990218     c                   goto      endct1
098000990212     C                   ENDIF
098100990212     C                   ENDIF
098200990218     c* Causale Chiusura
098300990218     c*  ricerca
098400990218     c                   clear                   v1dcch
098500990218     c     '?'           scan      v1ccch                                 90
098600990218     c                   clear                   tibs02ds
098700990218     c                   if        *in90
098800990218     C                   MOVEL     'R'           t02mod
098900990218     C                   MOVEL     knsif         t02sif
099000990218     C                   MOVEL     'CCH'         t02cod
099100990218      *
099200990218     C                   CALL      'TIBS02R'
099300990218     C                   PARM                    KPJBA
099400990218     C                   PARM                    TIBS02DS
099500990218      *
099600990218     C                   IF        T02err = *BLANKS
099700990218     C                   movel     T02uni        DCCH
099800990218     C                   movel     T02ke1        v1ccch
099900990218     C                   movel     §CCHdesc      v1dcch
100000990218     c                   endif
100100990218     c
100200990218     c                   seton                                        53
100300990218     c                   goto      endct1
100400990218     c                   endif
100500990218     c
100600990218     c*  controllo
100700990218     c                   if        v1ccch<>'  '
100800990218     c                   clear                   tibs02ds
100900990218     C                   MOVEL     'C'           t02mod
101000990218     C                   MOVEL     knsif         t02sif
101100990218     C                   MOVEL     'CCH'         t02cod
101200990218     c                   movel     v1ccch        t02ke1
101300990218      *
101400990218     C                   CALL      'TIBS02R'
101500990218     C                   PARM                    KPJBA
101600990218     C                   PARM                    TIBS02DS
101700990218      *
101800990218     C                   IF        T02err = *BLANKS
101900990218     C                   movel     T02uni        DCCH
102000990218     c                   movel     §cchdesc      v1dcch
102100990218     c                   else
102200990218     C                   seton                                        532890
102300990218     c                   movel     MSG(16)       v1cmsg
102400990218     c                   goto      endct1
102500990218     c                   endif
102600990218     c                   endif
102700990212     c
102800990212     c* Data apertura C.A.
102900990212     c     v1cdcd        ifeq      0
103000990212     c     v1cdca        andgt     0
103100990212     C                   seton                                        452890
103200990212     c                   movel     MSG(5)        v1cmsg
103300990218     c                   goto      endct1
103400990212     C                   ENDIF
103500990212     c
103600990212     c                   clear                   wamgdcd
103700990212     c                   clear                   wamgdca
103800990212     c     v1cdcd        ifgt      0
103900990212     C                   move      V1cdcd        G02DAT
104000990212     C                   eval      g02err = *blanks
104100990212     C                   call      'XSRDA8'
104200990212     C                   parm                    WLBDAT
104300990212      *
104400990212     C                   if        g02err = '1'
104500990212     C                   seton                                        284590
104600990212     C                   movel     msg(5)        v1cmsg
104700990218     c                   goto      endct1
104800990212     c                   else
104900990212     c                   eval      wamgdcd=g02inv
105000990217     c                   eval      v1cdcd =g02dat
105100990212     c                   endif
105200990212     c                   endif
105300990212     c
105400990212     c     v1cdca        ifgt      0
105500990212     C                   move      V1cdca        G02DAT
105600990212     C                   eval      g02err = *blanks
105700990212     C                   call      'XSRDA8'
105800990212     C                   parm                    WLBDAT
105900990212      *  Errore
106000990212     C                   if        g02err = '1'
106100990212     C                   seton                                        284690
106200990212     C                   movel     msg(5)        v1cmsg
106300990212     c                   goto      endct1
106400990212     c                   else
106500990212     c                   eval      wamgdca=g02inv
106600990217     c                   eval      v1cdca =g02dat
106700990212     c
106800990212     c     wamgdca       iflt      wamgdcd
106900990212     C                   seton                                        284690
107000990212     C                   movel     msg(6)        v1cmsg
107100990212     c                   goto      endct1
107200990212     c                   endif
107300990212     c                   endif
107400990212     c                   endif
107500990218     c* Data chiusura C.A.
107600990218     c     v1cdhd        ifeq      0
107700990218     c     v1cdha        andgt     0
107800990218     C                   seton                                        512890
107900990218     c                   movel     MSG(5)        v1cmsg
108000990218     c                   goto      endct1
108100990218     C                   ENDIF
108200990218     c
108300990218     c                   clear                   wamgdhd
108400990218     c                   clear                   wamgdha
108500990218     c     v1cdhd        ifgt      0
108600990218     C                   move      V1cdhd        G02DAT
108700990218     C                   eval      g02err = *blanks
108800990218     C                   call      'XSRDA8'
108900990218     C                   parm                    WLBDAT
109000990218      *
109100990218     C                   if        g02err = '1'
109200990218     C                   seton                                        285190
109300990218     C                   movel     msg(5)        v1cmsg
109400990218     c                   goto      endct1
109500990218     c                   else
109600990218     c                   eval      wamgdhd=g02inv
109700990218     c                   eval      v1cdhd =g02dat
109800990218     c                   endif
109900990218     c                   endif
110000990218     c
110100990218     c     v1cdha        ifgt      0
110200990218     C                   move      V1cdha        G02DAT
110300990218     C                   eval      g02err = *blanks
110400990218     C                   call      'XSRDA8'
110500990218     C                   parm                    WLBDAT
110600990218      *  Errore
110700990218     C                   if        g02err = '1'
110800990218     C                   seton                                        285290
110900990218     C                   movel     msg(5)        v1cmsg
111000990218     c                   goto      endct1
111100990218     c                   else
111200990218     c                   eval      wamgdha=g02inv
111300990218     c                   eval      v1cdha =g02dat
111400990218     c
111500990218     c     wamgdha       iflt      wamgdhd
111600990218     C                   seton                                        285290
111700990218     C                   movel     msg(6)        v1cmsg
111800990218     c                   goto      endct1
111900990218     c                   endif
112000990218     c                   endif
112100990218     c                   endif
112200990218     c
112300990218     c
112400990218     c* Se richieste solo le aperte non posso chiedere causale chiusura
112500990218     c*  o date chiusura
112600990218     c                   if        v1ctca='A'
112700990218     c                   if        v1cdhd>0 or v1ccch<>*blanks
112800990218     C                   seton                                        285190
112900990218     C                   movel     msg(17)       v1cmsg
113000990218     c                   goto      endct1
113100990218     c                   endif
113200990218     c                   endif
113300990315     c
113400990315     c* Controllo Beneficiario se immesso
113500110209     c                   clear                   $ksb(1)
113600990315     c                   eval      wkl=v1cklb
113700990315     c                   eval      wks=v1cksb
113800990315     c                   eval      wdks=v1dksb
113900990315     c                   eval      wrs=v1crsb
114000990315     c                   EXSR      codice
114100990315     c                   eval      v1cklb=wkl
114200990315     c                   eval      v1cksb=wks
114300990315     c                   eval      v1dksb=wdks
114400990315     c   90              seton                                        55
114500990315     c   90              goto      endct1
114600110209     c
114700110209     c                   if        wcodice>0
114800110209     c                   eval      Indx=%lookup(wcodice:$ksb)
114900110209     c                   if        Indx>0
115000110209     c                   movel     msg(36)       v1cmsg
115100110209     c                   seton                                        289055
115200110209     c                   leavesr
115300110209     c                   else
115400110209     c                   eval      $ksb(1)=wcodice
115500110209     c                   endif
115600110209     c                   endif
115700000215     c* STampo dati di liquidazione solo se ordinamento =1
115800000215     c     v1csdl        ifeq      'S'
115900000215     c     v1cord        andeq     '2'
116000000215     C                   seton                                        285690
116100000215     C                   movel     msg(24)       v1cmsg
116200000215     c                   goto      endct1
116300000215     c                   endif
116400990212     c
116500990212     c     ENDCT1        ENDSR
116600990212      *--------------------------------------------------------------------*
116700990212      * CONTROLLI VIDEO
116800990212      *--------------------------------------------------------------------*
116900990212     C     CTRD02        BEGSR
117000110210     c                   setoff                                       11
117100990215     c                   exsr      CTRSEL
117200990215     c   90              seton                                        47
117300990215     c   90              goto      endct2
117400990212     c
117500990212     c* LInea partenza bolla
117600990212     c                   clear                   v1dlnp
117700990212     c                   if        v1clnp>0
117800990212     c                   eval      wfil=v1clnp
117900990212     c                   exsr      CTRPO
118000990212     c   90              seton                                        47
118100990212     c   90              goto      endct2
118200990212     c                   eval      v1dlnp=orgdes
118300110210     c                   seton                                        11
118400990212     c                   endif
118500990212     c
118600990212     c
118700990212     c* Linea arrivo   bolla
118800990212     c                   clear                   v1dlna
118900990212     c                   if        v1clna>0
119000990212     c                   eval      wfil=v1clna
119100990212     c                   exsr      CTRPO
119200990212     c   90              seton                                        48
119300990212     c   90              goto      endct2
119400990212     c                   eval      v1dlna=orgdes
119500110210     c                   seton                                        11
119600990212     c                   endif
119700990212     c
119800990212     c* Data spedizione
119900990212     c     v1cdsd        ifeq      0
120000990212     c     v1cdsa        andgt     0
120100990212     C                   seton                                        492890
120200990212     c                   movel     MSG(5)        v1cmsg
120300990212     C                   ENDIF
120400990212     c
120500990212     c                   clear                   wamgdsd
120600990212     c                   clear                   wamgdsa
120700990212     c     v1cdsd        ifgt      0
120800990212     C                   move      V1cdsd        G02DAT
120900990212     C                   eval      g02err = *blanks
121000990212     C                   call      'XSRDA8'
121100990212     C                   parm                    WLBDAT
121200990212      *
121300990212     C                   if        g02err = '1'
121400990212     C                   seton                                        285090
121500990212     C                   movel     msg(5)        v1cmsg
121600990212     c                   else
121700990212     c                   eval      wamgdsd=g02inv
121800990930     c                   eval      v1cdsd=g02dat
121900110210     c                   seton                                        11
122000990212     c                   endif
122100990212     c                   endif
122200990212     c
122300990212     c     v1cdsa        ifgt      0
122400990212     C                   move      V1cdsa        G02DAT
122500990212     C                   eval      g02err = *blanks
122600990212     C                   call      'XSRDA8'
122700990212     C                   parm                    WLBDAT
122800990212      *  Errore
122900990212     C                   if        g02err = '1'
123000990212     C                   seton                                        285090
123100990212     C                   movel     msg(5)        v1cmsg
123200990212     c                   goto      endct2
123300990212     c                   else
123400990212     c                   eval      wamgdsa=g02inv
123500990930     c                   eval      v1cdsa=g02dat
123600110210     c                   seton                                        11
123700990212     c
123800990212     c     wamgdsa       iflt      wamgdsd
123900990212     C                   seton                                        285090
124000990212     C                   movel     msg(6)        v1cmsg
124100990212     c                   goto      endct2
124200990212     c                   endif
124300990212     c                   endif
124400990212     c                   endif
124500990315     c* Controllo Mittente se immesso
124600990315     c                   eval      wkl=v1cklm
124700990315     c                   eval      wks=v1cksm
124800990315     c                   eval      wdks=v1dksm
124900990315     c                   eval      wrs=v1crsm
125000990315     c                   EXSR      codice
125100990315     c                   eval      v1cklm=wkl
125200990315     c                   eval      v1cksm=wks
125300990315     c                   eval      v1dksm=wdks
125400990315     c   90              seton                                        51
125500990315     c   90              goto      endct2
125600990315     c* Controllo Destinatario
125700990315     c                   eval      wkl=v1ckld
125800990315     c                   eval      wks=v1cksd
125900990315     c                   eval      wdks=v1dksd
126000990315     c                   eval      wrs=v1crsd
126100990315     c                   EXSR      codice
126200990315     c                   eval      v1ckld=wkl
126300990315     c                   eval      v1cksd=wks
126400990315     c                   eval      v1dksd=wdks
126500990315     c   90              seton                                        52
126600990315     c   90              goto      endct2
126700110210     c
126800110210     c                   if        v1cksd>0 or v1crsd<>*blanks or v1cksm>0 or
126900110210     c                             v1crsm<>*blanks
127000110210     c                   seton                                        11
127100110210     c                   endif
127200990212
127300990212     c     ENDCT2        ENDSR
127400990212      *------------------------------------------------------------------------*
127500000907      * CPNTROLLO P.O.
127600990212      *------------------------------------------------------------------------*
127700990212     C     CTRPO         BEGSR
127800990212     c     wfil          chain     azorg01l                           30
127900990212     c     *in30         ifeq      *on
128000990212     c     orgfva        orne      ' '
128100990212     c     orgfag        orne      'A'
128200990212     c     orgfag        andne     'F'
128300990212     c                   eval      v1cmsg=msg(2)
128400990212     c                   seton                                        2890
128500990212     c                   endif
128600990212     c                   ENDSR
128700990315      *--------------------------------------------------------------------*
128800990315      * CONTROLLI CODICE O RAGIONE SOCIALE
128900990315      *--------------------------------------------------------------------*
129000990315     C     CODICE        BEGSR
129100990315     c
129200110209     c* Se immesso codice non immettere ragione sociale e viceversa
129300990315     c                   if        wkl>0  OR wks>0 OR wdks<>*blanks
129400990315     c                   if        wrs<>*blanks
129500990315     c                   eval      v1cmsg=msg(22)
129600990315     c                   seton                                        2890
129700990315     c                   goto      endcodice
129800990315     c                   endif
129900990315     c                   endif
130000110210     c
130100110210     c* se ho immesso la ragione sociale senza, codice avviso per premere F7
130200110210     c                   if        wdks<>*blanks and wks=0 and not *inkg
130300110210     c                   eval      v1cmsg=msg(37)
130400110210     c                   seton                                        2890
130500110210     c                   goto      endcodice
130600110210     c                   endif
130700990315     c
130800990315     c* F7 - ricerca alfabetica
130900110210     c*  la attivo se codice vuoto e ragione sociale piena
131000110210     c                   if        *inkg and wdks<>*blanks and wks=0
131100990315     C* DESCRIZIONE AZIENDA
131200990315     C                   MOVEL     rsut          PAXDUT           30
131300990315     C* PAXSTA=9 ESCLUDO ANNULLATI
131400990315     C                   Z-ADD     9             PAXSTA
131500990315     C* CAPOCONTO CLIENTI
131600050609     C                   Z-ADD     dutkci        PAXCCC
131700990315     C* CODICE DA RICERCARE: SE IMMESSA LNP CERCO SOLO AL SUO INTERNO
131800990315     C                   MOVEL(P)  Wkl           ACOKSC
131900000907     C                   MOVEL     Wkl           PAXFLR
132000990315     C                   MOVEL(P)  Wdks          paxdmt
132100000907     C                   z-add     1             PAXNUM
132200990315     C*
132300000907     C                   CALL      'XALFA3BR'
132400990315     c                   PARM                    PAXDUT
132500990315     C                   PARM                    CODUT
132600990315     C                   PARM                    PAXDMT
132700990315     C                   PARM                    PAXCCC
132800990315     C                   PARM                    PAXSTA            1 0
132900990315     C                   PARM                    PAXFLR           90
133000990315     C                   PARM                    PAXDIT            3
133100000907     C                   PARM                    PAXNUM            2 0
133200000907     C                   PARM                    PAXKCM           80
133300000907     C                   PARM                    PAXKSM          140
133400000907     C                   PARM                    PAXKDM           60
133500990315     C*
133600990315     C* NON TROVATO O PREMUTO CMD12
133700990315    2C     PAXSTA        IFGT      -1
133800000907     c                   movel     paxksm        acoksc
133900000907     c                   movel     acoksc        wkl
134000990315     c                   move      acoksc        wks
134100990315     c                   movel     paxdmt        wdks
134200990315     c                   endif
134300000907     c                   seton                                          90
134400000907     c                   goto      endcodice
134500990315     c                   endif
134600990315     c
134700990315     c* Controllo codice
134800990315     c                   if        wkl=0 and wks>0
134900990315     c                   eval      v1cmsg=msg(23)
135000990315     c                   seton                                        2890
135100990315     c                   endif
135200990315     c
135300990315     c                   if        wkl>0 and wks>0
135400990315     c                   clear                   tibs69ds
135500990315     c                   movel     wcodice       I69kac
135600990315      *
135700990315     C                   CALL      'TIBS69R'
135800990315     C                   PARM                    tibs69DS
135900990315     C                   PARM                    DS_cnaco
136000990315     C                   PARM                    DS_cnind
136100990315     C                   PARM                    DS_cnclp
136200990315     C                   PARM                    DS_fncls
136300990315    2c                   if        o69err=' '
136400990315     c                   movel     acorag        wdks
136500990315     c                   else
136600990315     c                   eval      v1cmsg=msg(23)
136700990315     c                   seton                                        2890
136800990315     c                   endif
136900990315     c                   endif
137000990315     c
137100990315     c     ENDCODICE     ENDSR
137200981203      *------------------------------------------------------------------------*
137300981203      * ROUTINE INIZIALE
137400981203      *------------------------------------------------------------------------*
137500981203     C     *INZSR        BEGSR
137600981203      *
137700981203     C     *ENTRY        PLIST
137800981203     C                   PARM                    KPJBA
137900981203      *
138000050609     c     *dtaara       define    §azute        azuteds
138100050609     c     *dtaara       define    §datiute      ddatiute
138200050609     c                   in(E)     *dtaara
138300050609    1c                   If        %error  or RSUT = *blanks
138400050609     c                   Clear                   Tibs34ds
138500050609     c                   Call      'TIBS34R'
138600050609     c                   Parm                    Tibs34ds
138700050609     c                   In        *dtaara
138800050609    1c                   EndIf
138900050609
139000050609     c                   Clear                   wabi
139100050609     c                   Clear                   v1cmsg
139200050609     c                   Clear                   dLat
139300050609
139400050609      * Verifica errori e autorità profilo
139500050609s   1c                   Select
139600050609      * se ho errori nei dati utente esco dal pgm
139700050609w   1c                   When      DutErr = 'E'
139800050609     c                   GoTo      Fine
139900050609      * se non c'è l'abilitazione
140000050609      * --> se 1° livello, abilitazioni al terminal
140100050609      *     se 2° livello, abilitazioni al punto operativo
140200050609      *     se sede è impossibile ma se capita mando a fine il pgm
140300050609w   1c                   When      UteAut = *Blanks
140400050609if  2c                   If        DutLpo = '1'
140500050609     c                   Eval      wabi   = 'TP'
140600050609e   2c                   EndIf
140700050609if  2c                   If        DutLpo = '2'
140800050609     c                   Eval      wabi   = 'PO'
140900050609e   2c                   EndIf
141000050609if  2c                   If        DutLpo = 'S'
141100050609     c                   GoTo      Fine
141200050609e   2c                   EndIf
141300050609      * carica le abilitazioni del profilo
141400050609x   1c                   Other
141500050609     c                   Movel     UteFaf        Dute01
141600050609     c                   If        §UteEca <> *Blanks
141700050609     c                   Eval      wabi = §UteEca
141800050609     c                   Else
141900050609     c                   Eval      wabi = UteAut
142000050609     c                   EndIf
142100050609e   1c                   EndSl
142200050609
142300050609      * controllo se ok l'abilitazione dell'utente
142400050609     c                   Clear                   Tibs02ds
142500050609     c                   Eval      T02Mod = 'C'
142600050609     c                   Eval      T02Sif = knsif
142700050609     c                   Eval      T02Cod = 'LAT'
142800050609     c                   Movel(p)  wabi          T02Ke1
142900050609     c                   Call      'TIBS02R'
143000050609     c                   Parm                    kpjba
143100050609     c                   Parm                    Tibs02ds
143200050609if  1c                   If        T02Err = *Blanks
143300050609     c                   Eval      dLat = T02Uni
143400050609e   1c                   EndIf
143500050609      * errore o non abilitato
143600050609if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
143700050609     c                   Seton                                        28
143800050609     C                   movel     msg(27)       v1cmsg
143900050609   x1c                   Else
144000050609      * Reperimento dei P.O. gestibili dall'utente
144100050609     c                   clear                   TRUL31DS
144200050609     c                   clear                   TRUL31DS2
144300050609     c                   eval      I31abi = wabi
144400050609     c                   eval      I31cdi = DUTdis
144500050609     c                   eval      I31car = DUTare
144600050609     c                   eval      I31cpo = DUTpou
144700050609     c                   call      'TRUL31R'
144800050609     c                   parm                    KPJBA
144900050609     c                   parm                    TRUL31DS
145000050609     c                   parm                    TRUL31DS2
145100050609if  2c                   if        O31pog > *zeros
145200050610     c                   movea     O31pog        $pog
145300050610     c                   movea     O31arg        $arg
145400050610     c                   movea     O31dig        $dig
145500050609x   2c                   else
145600050609     c                   Seton                                        28
145700050609     c                   movel     MSG(27)       v1cmsg
145800050609e   2c                   endif
145900050609    1c                   endif
146000050609     c
146100050609     C                   Z-ADD     1             CODUT
146200050609     C**                 CALL      'X§PARUT'
146300050609     C**                 PARM                    UT§DSE
146400050609     C**                 MOVEL     RAGUT         RSUT
146500050609     C**                 MOVEL     REC80         CNCR80
146600981203
146700990315      * reperisco data e ora
146800990315     C                   TIME                    W0140
146900990315      * UDATE IN GGMMAAAA
147000990315     C                   MOVE      W0140         WDTGIO
147100990315      * UDATE IN AAAAMMGG
147200990315     C                   Z-ADD     WDTGIO        G02DAT
147300990315     C                   MOVEL     *BLANK        G02ERR
147400990315     C                   CALL      'XSRDA8'
147500990315     C                   PARM                    WLBDAT
147600990315     C                   MOVEL     G02INV        DATEU
147700981203      *
147800990215     c     ktab          klist
147900990215     c                   kfld                    codut
148000990215     c                   kfld                    cod
148100990215     c                   kfld                    key
148200990215     c     kORG          klist
148300990215     c                   kfld                    v1cdis
148400990215     c                   kfld                    ware
148500990215     c     KORG1         klist
148600990215     c                   kfld                    orgfl3
148700990215     c                   kfld                    orgcar
148800981203     C                   ENDSR
148900990215      *------------------------------------------------------------------------*
149000990215      * SOTTOMENTTO ELABORAZIONE
149100990215      *------------------------------------------------------------------------*
149200990212     c     CONFERMA      BEGSR
149300990212     c                   clear                   fidn43ds
149400990219     c                   eval      i43sap = v1csap
149500990219     c                   eval      i43dst = v1cdis
149600990309     c                   eval      i43ind = v1cind
149700990215     c                   eval      i43are = v1care
149800990309     c                   eval      i43ina = v1cina
149900990315     c                   eval      i43ter = v1cter
150000990311     c                   eval      i43int = v1cint
150100990311     c                   eval      i43ntw = v1cntw
150200990406     c                   eval      i43tod = v1ctod
150300990406     c                   eval      i43toa = v1ctoa
150400990406     c                   eval      i43ton = v1cton
150500101118     c***                if        v1cusi='S' or v1cusn='S'
150600101118     c                   if        v1cusn='S'
150700090320     c                   eval      i43usi = 'S'
150800090320     c                   else
150900090320     c                   eval      i43usi = 'N'
151000090320     c                   endif
151100990512     c   01              eval      i43toi = v1ctoi
151200990512     c  N01              eval      i43toi = v1ctsn
151300101118     c*****              if        v1ccrp='S' or v1ccrn='S'
151400101118     c                   if        v1ccrn='S'
151500090320     c                   eval      i43crp = 'S'
151600090320     c                   else
151700090320     c                   eval      i43crp = 'N'
151800090320     c                   endif
151900040129     c                   eval      i43cfd = v1ccfd
152000990215     c                   eval      i43ord = v1cord
152100990212     c                   eval      i43prs = v1cprs
152200060206     c                   eval      i43tr1 = v1ctr1
152300060206     c                   eval      i43tr2 = v1ctr2
152400060206     c                   eval      i43tr3 = v1ctr3
152500990212     c                   eval      i43psl = v1cpsl
152600990212     c                   eval      i43tsl = v1ctsl
152700990212     c                   eval      i43fil = v1cfil
152800990212     c                   eval      i43aac = v1caac
152900990212     c                   eval      i43tad = v1ctad
153000990212     c                   eval      i43rgr = v1crgr
153100990916     c                   eval      i43rg2 = v1crg2
153200990916     c                   eval      i43rg3 = v1crg3
153300990212     c                   eval      i43tca = v1ctca
153400990212     c                   eval      i43tpl = v1ctpl
153500990212     c                   eval      i43dcd = v1cdcd
153600990212     c                   eval      i43dca = v1cdca
153700990218     c                   eval      i43dhd = v1cdhd
153800990218     c                   eval      i43dha = v1cdha
153900000215     c                   eval      i43sdl = v1csdl
154000040726     c                   eval      i43sdm = v1csdm
154100170228     c                   eval      i43rit = v1crit
154200990212     c                   eval      i43amgdcd = wamgdcd
154300990212     c                   eval      i43amgdca = wamgdca
154400990218     c                   eval      i43amgdhd = wamgdhd
154500990218     c                   eval      i43amgdha = wamgdha
154600990218     c                   eval      i43cch = v1ccch
154700990218     c                   eval      i43lnp = v1clnp
154800990212     c                   eval      i43lna = v1clna
154900990212     c                   eval      i43dsd = v1cdsd
155000990212     c                   eval      i43dsa = v1cdsa
155100990212     c                   eval      i43amgdsd = wamgdsd
155200990212     c                   eval      i43amgdsa = wamgdsa
155300990315     c                   z-add     v1cksd        i43ksd
155400990315     c                   movel     v1ckld        i43ksd
155500990315     c                   movel     v1crsd        i43rsd
155600990315     c                   z-add     v1cksm        i43ksm
155700990315     c                   movel     v1cklm        i43ksm
155800990315     c                   movel     v1crsm        i43rsm
155900990315     c                   z-add     v1cksb        i43ksb
156000990315     c                   movel     v1cklb        i43ksb
156100990315     c                   movel     v1crsb        i43rsb
156200060124     c                   movel     v1cval        i43val
156300060125     c                   movel     v1cpkd        i43pkd
156400110210     c                   z-add     v1cksb2       i43ksb2
156500110210     c                   movel     v1cklb2       i43ksb2
156600110210     c                   z-add     v1cksb3       i43ksb3
156700110210     c                   movel     v1cklb3       i43ksb3
156800110210     c                   z-add     v1cksb4       i43ksb4
156900110210     c                   movel     v1cklb4       i43ksb4
157000110210     c                   z-add     v1cksb5       i43ksb5
157100110210     c                   movel     v1cklb5       i43ksb5
157200990315     c
157300990219     c* SE ditretto e area vuoti e ho impostato fil per ord 1 li riempo
157400050614     c                   if        i43ord='1' and i43fil>0 AND simfel=0 and
157500990219     c                             i43dst=*blanks and i43are=*blanks
157600990219     c     i43fil        chain     azorg01l                           30
157700990219     c                   movel     orgfl3        i43dst
157800990219     c                   movel     orgcar        i43are
157900990219     c                   endif
158000090320     c* solo per stampa vecchia
158100101118     c***                if        v1ccrN<>'S' and v1cusn<>'S'
158200101118     c***                if        i43ord='2' and i43prs>0 AND simfel=0 and
158300101118     c***                          i43dst=*blanks and i43are=*blanks
158400101118     c***  i43prs        chain     azorg01l                           30
158500101118     c***                movel     orgfl3        i43dst
158600101118     c***                movel     orgcar        i43are
158700101118     c***                endif
158800101118     c***                endif
158900990212      *
159000040209     c                   clear                   Wpgm             10
159100990507      *
159200990212     c                   movel     fidn43ds      kpjbu
159300990212     c
159400101118     c***                if        v1ccrN<>'S' and v1cusn<>'S'
159500090323     c* richiamo pgm aggiornamento distretto/area su report se richiesta
159600090323     c* creazione dati
159700101118     C***                if        i43crp = 'S'
159800101118     c***                eval      kcoaz='DN39'
159900101118     c***                eval      Wpgm = 'BCH10     '
160000101118     c***                call      Wpgm
160100101118     c***                parm                    kpjba
160200101118     c***                endif
160300090323     c
160400101118     c***                eval      kcoaz='DN44'
160500050708     c**                 eval      Wpgm = 'FIDN44R   '
160600101118     c***                eval      Wpgm = 'BCH10     '
160700101118     c***                else
160800090326     c                   eval      kcoaz='DN47'
160900090326     c**                 eval      Wpgm = 'FIDN47R   '
161000090326     c                   eval      Wpgm = 'BCH10     '
161100101118     c***                endif
161200040209     c                   call      Wpgm
161300990212     c                   parm                    kpjba
161400990212     c                   endsr
161500981203      *---------------------------------------------------------------------------------------------
161600981203** MSG  Lungh. 78                                                            *
161700990212Effettuare almeno una selezione                                                1
161800990212Punto operativo inesistente                                                    2
161900060601Tipo anomalia valido soltanto per NON eventi e Eventi con calcolo Responsabil.
162000990215Tipo anomalia inesistente                                                      4
162100990215Data errata                                                                    5
162200990215Data AL   MINORE  data DAL                                                     6
162300990215Ci sono errori nella videata di selezioni C.A.: premere F8 per correggere      7
162400990215Ci sono errori nella videata di selezioni bolle: premere F8 per correggere     8
162500990215Cod.Distretto errato                                                           9
162600990215Cod.Area errato                                                               10
162700990215Cod.Distretto 9 - SEDE  non selezinabile                                      11
162800070110Cod.Distretto non presente nell'organigramma                                  12
162900990215Cod.Area non congruente col distretto selezionato                             13
163000990218cod.Area di distretto 9 - SEDE  non selezionabile                             14
163100990218Cod.Area non presente nell'organigramma                                       15
163200990218Causale chiusura errata                                                       16
163300990218Se richieste C.A. aperte inutile selezione data o causale chiusura            17
163400990219Il P.O. non fa parte del distretto selezionato                                18
163500990219Il P.O. non fa parte dell'area selezionata                                    19
163600090326Elaborare "vecchio" e "nuovo" separatamente                                   20
163700990315Il P.O. non e' un terminal                                                    21
163800990315Immettere o il codice o la ragione sociale                                    22
163900990315Codice errato o inesistente                                                   23
164000000215Stampa dati di liquidazione solo per ordinamento= '1'                         24
164100040129Impostare ad 'S' per creare il file DETTAGLIO                                 25
164200040129Impostare ad 'N' per creare il file DETTAGLIO                                 26
164300050609Utente non abilitato alla Stampa delle C.A.                                   27
164400050610Utente non abilitato a richiedere il distretto indicato                       28
164500050610Utente non abilitato a richiedere l'Area indicata                             29
164600050610Utente non abilitato a richiedere il P.O. indicato                            30
164700050614Se immessa area non immettere terminal responsabile (o viceversa)             31
164800050610Il Terminal non fa parte del distretto selezionato                            32
164900050708Richiedere il terminal responsabile OPPURE il p.O. responsabile               33
165000060208Se immesso il"tipo responsabile" immettere anche il terminal o il P.O respons.34
165100060208Se immesso il "tipo selezione" immettere anche il P.O. per selezione          35
165200110209Codice beneficiario già inserito                                              36
165300110210Per la ricerca alfabetica premere F7                                          37
