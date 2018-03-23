000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000101141112     h dftactgrp(*no) actgrp(*caller)
000200140919     H* FNLRY09R *---------------------------------------------------*
000300140919     H* - Driver di controllo presenza / esecuzione indicazioni di cons.
000400110610     h*
000500000000     H*--------------------------------------------------------------*
000600140919     FTIIDC05l  IF   E           K DISK
000700140929     FTIIDC01l  IF   E           K DISK    rename(tiidc000:tiidc001)
000800030722     F**
000900070313     d
001000141021     D fnlry09ds     E DS                  prefix(OL_)
001100141021     D fnlry09ds2    E DS                  inz
001200141027     D fnlry03ds2    E DS
001300140930     D fnlry02ds     E DS
001400140919     D kpjba         E DS
001500070313     d
001600140929     d WELADATA        s                   like(IDCELADATA) INZ(0)
001700140206
001800900518     C***************************************************************
001900000000     C     *ENTRY        PLIST
002000141013     c                   parm                    kpjba
002100140919     C                   PARM                    fnlry09ds
002200940223     C*---------------------------------------------------------------*
002300141021     c* verifico se passata nuova o vecchia DS
002400141021     c                   movel     kpjbu         fnlry09ds2
002500141022
002600141022     c* controllo che sia davvero passata la nuova DS
002700141029     c                   if        ILRY09TCH <>' '
002800141022     c                   monitor
002900141022     c                   if        ILRY09NSP>0 or ILRY09FGS>0
003000141022     c                   endif
003100141022     c                   on-error
003200141022     c                   clear                   ILRY09TCH
003300141022     c                   endmon
003400141022     c                   else
003500141022     c                   clear                   ILRY09TCH
003600141022     c                   endif
003700141022     c
003800141021     c                   select
003900141021     c* passata solo DS vecchi --> sposto i campi
004000141022     c                   when      %parms=2
004100141021     c                   clear                   fnlry09ds2
004200141021     c                   eval      ILRY09TLA  =ol_ILRY09TLA
004300141111     c                   if        ilry09tla<>'C'
004400141021     c                   eval      ILRY09Tch  =ol_ILRY09Tch
004500141021     c                   eval      ILRY09AAS  =ol_ILRY09AAS
004600141021     c                   eval      ILRY09lnp  =ol_ILRY09lnp
004700141021     c                   eval      ILRY09nrs  =ol_ILRY09nrs
004800141021     c                   eval      ILRY09nsp  =ol_ILRY09nsp
004900141021     c                   eval      ILRY09fgs  =ol_ILRY09fgs
005000141111     c                   endif
005100141022     c                   clear                   ol_OLry09esit
005200141022     c                   clear                   ol_OLry09err
005300141022     c                   clear                   ol_OLry09msg
005400141021     c
005500141021     c* non passata nessuna DS --> pulisco la ds nuova e darà errore
005600141021     c                   when      %parms=1 and ILRY09TCH =' '
005700141021     c                   clear                   fnlry09ds2
005800141022     c                   endsl
005900141021     c
006000141021     C*---------------------------------------------------------------*
006100140922     C* TIPO LANCIO, ilry09TLA "C" -           CHIUSO CON LR
006200140922     C* TIPO LANCIO, ilry09TLA "L" - ELABORO E CHIUDO CON LR
006300140922     C* TIPO LANCIO, ilry09TLA " " - ELABORO E CHIUDO IN RETRN
006400941216     C*
006500140919    1C     ilry09tla     IFNE      'C'
006600140919     c                   clear                   OLry09esit
006700141022     c                   clear                   OLry09esdi
006800141022     c                   clear                   OLry09esal
006900140919     c                   clear                   OLry09err
007000140919     c                   clear                   OLry09msg
007100051123     c
007200140919     c* controlli formali dei parametri ricevuti
007300140919     c                   exsr      sr_chkform
007400140919
007500140919     c                   if        olry09err=' '
007600140919     c                   select
007700140919     c* Richiesto check
007800140922     c                   when      ilry09tch='C'
007900140919     c                   exsr      sr_Check
008000141029     c* Richiesto check di tutto per una spedizione
008100141029     c                   when      ilry09tch='T'
008200141029     c                   exsr      sr_CheckT
008300140919     c* Richiesta esecuzione indicazioni
008400140922     c                   when      ilry09tch='E'
008500140919     c                   exsr      sr_esegui
008600141001     c                   exsr      sr_Check
008700140919     c                   endsl
008800140919     c*
008900140205    1c                   endif
009000140919    1c                   endif
009100141022
009200141022     c                   if        %parms=2
009300141022     c                   eval      ol_olry09esit=olry09esit
009400141022     c                   eval      ol_olry09err =olry09err
009500141022     c                   eval      ol_olry09msg =olry09msg
009600141024     c                   else
009700141024     c                   movel     fnlry09ds2    kpjbu
009800141022     c                   endif
009900051123     c
010000140922    1C     ilry09tla     IFNE      ' '
010100051123     C                   SETON                                        LR
010200051123     C                   ELSE
010300051123     C                   SETON                                        RT
010400140205    1C                   ENDIF
010500140206     C**************************************************************************
010600140919     C     sr_chkform    BEGSR
010700141029     c* tipo chiamata obbligatorio "C" o "E"  "T"
010800140919     c                   if        ilry09tch<> 'C' and
010900141029     c                             ilry09tch<> 'E' and ilry09tch<> 'T'
011000140919     c                   eval      olry09err='1'
011100140919     c                   eval      olry09msg='Errore tipo chiamata'
011200140922     c                   leavesr
011300140919     c                   endif
011400141029     c* tipo chiamata "T" solo se c'è spedizione
011500141029     c                   if        ilry09tch= 'T'  and ilry09nsp=0
011600141029     c                   eval      olry09err='1'
011700141029     c                   eval      olry09msg='Per chiamata"T" immettere numero-
011800141029     c                              spedizione'
011900141029     c                   leavesr
012000141029     c                   endif
012100140929     c* Filiale gestione obbligatoria se non passato il numero di spedizione
012200140929     c                   if        ilry09fgs=0 and ilry09nsp=0
012300140919     c                   eval      olry09err='2'
012400140930     c                   eval      olry09msg='Filiale gestione o numero spedi+
012500140930     c                             zione obbligatorio'
012600140922     c                   leavesr
012700140919     c                   endif
012800140206     c                   endsr
012900140206     C**************************************************************************
013000140919     C     sr_check      BEGSR
013100140929     c                   eval      olry09esit='0'
013200141030     c                   if        ilry09tdis=' '  or ilry09tdis='D'
013300141022     c                   eval      olry09esdi='0'
013400141030     c                   endif
013500141030     c                   if        ilry09tdis=' '  or ilry09tdis='E'
013600141022     c                   eval      olry09esal='0'
013700141030     c                   endif
013800141030     c
013900141022     c                   clear                   finelet           1
014000141022
014100141022     c                   if        ilry09nsp>0
014200140929     c     kidc1         setll     tiidc01l
014300140929     c     kidc1         reade     tiidc01l
014400141022     c                   dow       not %eof(tiidc01l)
014500141022     c* DA elaborare
014600141022     c                   if        idcelaflg='E'and
014700141022     c                             (ilry09fgs=0 or ilry09fgs=idcfgs)
014800141022     c                   exsr      Esiti
014900141022
015000141022     c                   if        finelet='S'
015100141022     c                   leave
015200141022     c                   endif
015300141022     c                   endif
015400141022
015500140929     c     kidc1         reade     tiidc01l
015600140929     c                   enddo
015700141022
015800140919     c                   else
015900140919     c     ilry09fgs     setll     tiidc05l
016000141022     c     ilry09fgs     reade     tiidc05l
016100141022     c                   dow       not %eof(tiidc05l)
016200141022     c                   exsr      Esiti
016300141022     c                   if        finelet='S'
016400141022     c                   leave
016500141022     c                   endif
016600141022
016700141022     c     ilry09fgs     reade     tiidc05l
016800141022     c                   enddo
016900141022     c                   endif
017000141022     c
017100140206     c                   endsr
017200141029     C**************************************************************************
017300141029     C     sr_checkT     BEGSR
017400141029     c                   eval      olry09esit='0'
017500141029     c                   eval      olry09esdi='0'
017600141029     c                   eval      olry09esal='0'
017700141029     c                   clear                   finelet           1
017800141029
017900141029     c     kidc1         setll     tiidc01l
018000141029     c     kidc1         reade     tiidc01l
018100141029     c                   dow       not %eof(tiidc01l)
018200141029     c* DA elaborare tutte le dispo anche ancora da caricare in automatico
018300141029     c                   if        ilry09fgs=0 or ilry09fgs=idcfgs
018400141029     c                   exsr      Esiti
018500141029
018600141029     c                   if        finelet='S'
018700141029     c                   leave
018800141029     c                   endif
018900141029     c                   endif
019000141029
019100141029     c     kidc1         reade     tiidc01l
019200141029     c                   enddo
019300141029     c
019400141029     c                   endsr
019500141022     c
019600141022     C**************************************************************************
019700141022     c     Esiti         BEGSR
019800141022     c                   select
019900141022     c                   when      ILRY09TDIS=' '
020000141022     c                   eval      olry09esit='1'
020100141022     c* imposto i singoli check
020200141022     c                   if        idctipodis='3' and idcfla='S'
020300141022     c                   eval      olry09esdi='1'
020400141022     c                   endif
020500141022     c                   if        idctipodis<>'3' or idcfla<>'S'
020600141022     c                   eval      olry09esal='1'
020700141022     c                   endif
020800141022
020900141022     c                   if        olry09esdi='1'  and olry09esal='1'
021000141022     c                   eval      finelet='S'
021100141022     c                   endif
021200141022
021300141022     c                   when      ILRY09TDIS='D' and idctipodis='3' and
021400141022     c                             idcfla='S'
021500141022     c                   eval      olry09esit='1'
021600141022     c                   eval      olry09esdi='1'
021700141022     c                   eval      finelet='S'
021800141022     c                   when      ILRY09TDIS='E' and (idctipodis<>'3' or
021900141022     c                             idcfla<>'S')
022000141022     c                   eval      olry09esit='1'
022100141022     c                   eval      olry09esal='1'
022200141022     c                   eval      finelet='S'
022300141022     c                   endsl
022400141022     c
022500141022     c                   ENDSR
022600140919     C**************************************************************************
022700140919     C     sr_esegui     BEGSR
022800140919     c                   if        ilry09nsp>0
022900140929     c     kidc1         setll     tiidc01l
023000140929     c     kidc1         reade     tiidc01l
023100140929     c                   dow       not %eof(tiidc01l)
023200140919     c
023300140929     C                   IF        IDCELAFLG='E' and
023400140929     c                             (ilry09fgs=0 or ilry09fgs=idcfgs)
023500141030     c* verifico il tipo disposizione
023600141030     c                   if        ilry09tdis='D' and idcfla<>'S'
023700141030     c                   leave
023800141030     c                   endif
023900141030     c                   if        ilry09tdis='E' and idcfla= 'S'
024000141030     c                   leave
024100141030     c                   endif
024200141030     c
024300141027     c                   clear                   fnlry03ds2
024400140919     c                   eval      ilry03aas=idcaas
024500140919     c                   eval      ilry03lnp=idclnp
024600140919     c                   eval      ilry03nrs=idcnrs
024700140919     c                   eval      ilry03nsp=idcnsp
024800140919     c                   eval      ilry03prg=idcprg
024900141006     c                   eval      ilry03fgs=idcfgs
025000140919     c                   eval      ilry03tipd=idctipodis
025100141027     c                   eval      ilry03df12=ILRY09DF12
025200141027     c                   eval      ilry03df06=ILRY09DF06
025300141027     c                   eval      ilry03df21=ILRY09DF21
025400141027     c                   eval      ilry03ef12=ILRY09eF12
025500141027     c                   eval      ilry03ef06=ILRY09eF06
025600141027     c                   eval      ilry03ef21=ILRY09eF21
025700141028     c                   eval      ilry03ffr =ILRY09ffr
025800141028     c
025900141028     c                   eval      ilry03pss =ILRY09pss
026000141028     c                   eval      ilry03mdb4=ILRY09mdb4
026100141028     c                   eval      ilry03mdb5=ILRY09mdb5
026200141028     c                   eval      ilry03psl =ILRY09psl
026300141028     c                   eval      ilry03bsl =ILRY09bsl
026400141028     c                   eval      ilry03prb4=ILRY09prb4
026500141028     c                   eval      ilry03prb5=ILRY09prb5
026600141028     c                   eval      ilry03mde =ILRY09mde
026700141028     c                   eval      ilry03pre =ILRY09pre
026800141028     c                   eval      ilry03mdb =ILRY09mdb
026900141028     c                   eval      ilry03prb =ILRY09prb
027000141028     c                   movel     fnlry03ds2    kpjbu
027100140919     c                   call      'FNLRY03R'
027200140919     c                   parm                    kpjba
027300141028     c                   movel     kpjbu         fnlry03ds2
027400141028     c
027500141028     c* se c'è errore lo passo nel fnlry09ds
027600141028     c                   if        olry03err<>*blanks
027700141028     c                   eval      olry09err=olry03err
027800141028     c                   eval      olry09msg=olry03msg
027900140929     c                   endif
028000141028     c                   endif
028100140929
028200140929     c     kidc1         reade     tiidc01l
028300140919     c                   enddo
028400140919     c                   else
028500140930     c                   clear                   fnlry02ds
028600140930     c                   eval      ILRY02fgs=%editc(ILRY09FGS:'X')
028700141028     c
028800141029     c                   select
028900141029     c                   when      %parms=2
029000141029     c                   eval      ilry02dir=' '
029100141029
029200141029     c                   when      ilry09tdis='D'
029300141028     c                   eval      ilry02dir='I'
029400141029
029500141029     c                   other
029600141028     c                   eval      ILRY02DIR='E'
029700141029     c                   endsl
029800141028     c
029900140930     c                   movel(P)  fnlry02ds     kpjbu
030000140919     c                   call      'FNLRY02R'
030100140919     c                   parm                    kpjba
030200140919     c                   endif
030300141028     c
030400140919     c                   endsr
030500991206     C**************************************************************************
030600051123     C     *INZSR        BEGSR
030700051123     c
030800140919     c     kidc5         Klist
030900140919     c                   Kfld                    ilry09fgs
031000140919     c                   Kfld                    ilry09aas
031100140919     c                   Kfld                    ilry09lnp
031200140919     c                   Kfld                    ilry09nrs
031300140919     c                   Kfld                    ilry09nsp
031400140929     c     kidc1         Klist
031500140929     c                   Kfld                    ilry09aas
031600140929     c                   Kfld                    ilry09lnp
031700140929     c                   Kfld                    ilry09nrs
031800140929     c                   Kfld                    ilry09nsp
031900140929     c                   Kfld                    WELADATA
032000140206     c
032100070313     c                   endsr
