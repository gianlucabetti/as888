000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200141112     h dftactgrp(*no) actgrp(*caller)
000300140919     H* FNLRY09R *---------------------------------------------------*
000400140919     H* - Driver di controllo presenza / esecuzione indicazioni di cons.
000500110610     h*
000600000000     H*--------------------------------------------------------------*
000700140919     FTIIDC05l  IF   E           K DISK
000800140929     FTIIDC01l  IF   E           K DISK    rename(tiidc000:tiidc001)
000900030722     F**
001000070313     d
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
002100141021     c                   movel     kpjbu         fnlry09ds2
002200151119     C*---------------------------------------------------------------*
002300141022
002400141022     c* controllo che sia davvero passata la nuova DS
002500141029     c                   if        ILRY09TCH <>' '
002600141022     c                   monitor
002700141022     c                   if        ILRY09NSP>0 or ILRY09FGS>0
002800141022     c                   endif
002900141022     c                   on-error
003000141022     c                   clear                   ILRY09TCH
003100141022     c                   endmon
003200141022     c                   else
003300141022     c                   clear                   ILRY09TCH
003400141022     c                   endif
003500141022     c
003600141021     c* non passata nessuna DS --> pulisco la ds nuova e darà errore
003700151119     c                   if        ILRY09TCH =' '
003800141021     c                   clear                   fnlry09ds2
003900151119     c                   endif
004000141021     c
004100141021     C*---------------------------------------------------------------*
004200140922     C* TIPO LANCIO, ilry09TLA "C" -           CHIUSO CON LR
004300140922     C* TIPO LANCIO, ilry09TLA "L" - ELABORO E CHIUDO CON LR
004400140922     C* TIPO LANCIO, ilry09TLA " " - ELABORO E CHIUDO IN RETRN
004500941216     C*
004600140919    1C     ilry09tla     IFNE      'C'
004700140919     c                   clear                   OLry09esit
004800141022     c                   clear                   OLry09esdi
004900141022     c                   clear                   OLry09esal
005000151116     c                   clear                   OLry09reso
005100140919     c                   clear                   OLry09err
005200140919     c                   clear                   OLry09msg
005300051123     c
005400140919     c* controlli formali dei parametri ricevuti
005500140919     c                   exsr      sr_chkform
005600140919
005700140919     c                   if        olry09err=' '
005800140919     c                   select
005900140919     c* Richiesto check
006000140922     c                   when      ilry09tch='C'
006100140919     c                   exsr      sr_Check
006200141029     c* Richiesto check di tutto per una spedizione
006300141029     c                   when      ilry09tch='T'
006400141029     c                   exsr      sr_CheckT
006500140919     c* Richiesta esecuzione indicazioni
006600140922     c                   when      ilry09tch='E'
006700140919     c                   exsr      sr_esegui
006800141001     c                   exsr      sr_Check
006900140919     c                   endsl
007000140919     c*
007100140205    1c                   endif
007200140919    1c                   endif
007300141022
007400141024     c                   movel     fnlry09ds2    kpjbu
007500051123     c
007600140922    1C     ilry09tla     IFNE      ' '
007700051123     C                   SETON                                        LR
007800051123     C                   ELSE
007900051123     C                   SETON                                        RT
008000140205    1C                   ENDIF
008100140206     C**************************************************************************
008200140919     C     sr_chkform    BEGSR
008300141029     c* tipo chiamata obbligatorio "C" o "E"  "T"
008400140919     c                   if        ilry09tch<> 'C' and
008500141029     c                             ilry09tch<> 'E' and ilry09tch<> 'T'
008600140919     c                   eval      olry09err='1'
008700140919     c                   eval      olry09msg='Errore tipo chiamata'
008800140922     c                   leavesr
008900140919     c                   endif
009000141029     c* tipo chiamata "T" solo se c'è spedizione
009100141029     c                   if        ilry09tch= 'T'  and ilry09nsp=0
009200141029     c                   eval      olry09err='1'
009300141029     c                   eval      olry09msg='Per chiamata"T" immettere numero-
009400141029     c                              spedizione'
009500141029     c                   leavesr
009600141029     c                   endif
009700140929     c* Filiale gestione obbligatoria se non passato il numero di spedizione
009800140929     c                   if        ilry09fgs=0 and ilry09nsp=0
009900140919     c                   eval      olry09err='2'
010000140930     c                   eval      olry09msg='Filiale gestione o numero spedi+
010100140930     c                             zione obbligatorio'
010200140922     c                   leavesr
010300140919     c                   endif
010400140206     c                   endsr
010500140206     C**************************************************************************
010600140919     C     sr_check      BEGSR
010700140929     c                   eval      olry09esit='0'
010800141030     c                   if        ilry09tdis=' '  or ilry09tdis='D'
010900141022     c                   eval      olry09esdi='0'
011000151116     c                   eval      olry09reso='0'
011100141030     c                   endif
011200141030     c                   if        ilry09tdis=' '  or ilry09tdis='E'
011300141022     c                   eval      olry09esal='0'
011400141030     c                   endif
011500141030     c
011600141022     c                   clear                   finelet           1
011700141022
011800141022     c                   if        ilry09nsp>0
011900140929     c     kidc1         setll     tiidc01l
012000140929     c     kidc1         reade     tiidc01l
012100141022     c                   dow       not %eof(tiidc01l)
012200141022     c* DA elaborare
012300141022     c                   if        idcelaflg='E'and
012400141022     c                             (ilry09fgs=0 or ilry09fgs=idcfgs)
012500141022     c                   exsr      Esiti
012600141022
012700141022     c                   if        finelet='S'
012800141022     c                   leave
012900141022     c                   endif
013000141022     c                   endif
013100141022
013200140929     c     kidc1         reade     tiidc01l
013300140929     c                   enddo
013400141022
013500140919     c                   else
013600140919     c     ilry09fgs     setll     tiidc05l
013700141022     c     ilry09fgs     reade     tiidc05l
013800141022     c                   dow       not %eof(tiidc05l)
013900141022     c                   exsr      Esiti
014000141022     c                   if        finelet='S'
014100141022     c                   leave
014200141022     c                   endif
014300141022
014400141022     c     ilry09fgs     reade     tiidc05l
014500141022     c                   enddo
014600141022     c                   endif
014700141022     c
014800140206     c                   endsr
014900141029     C**************************************************************************
015000141029     C     sr_checkT     BEGSR
015100141029     c                   eval      olry09esit='0'
015200141029     c                   eval      olry09esdi='0'
015300141029     c                   eval      olry09esal='0'
015400151116     c                   eval      olry09reso='0'
015500141029     c                   clear                   finelet           1
015600141029
015700141029     c     kidc1         setll     tiidc01l
015800141029     c     kidc1         reade     tiidc01l
015900141029     c                   dow       not %eof(tiidc01l)
016000141029     c* DA elaborare tutte le dispo anche ancora da caricare in automatico
016100141029     c                   if        ilry09fgs=0 or ilry09fgs=idcfgs
016200141029     c                   exsr      Esiti
016300141029
016400141029     c                   if        finelet='S'
016500141029     c                   leave
016600141029     c                   endif
016700141029     c                   endif
016800141029
016900141029     c     kidc1         reade     tiidc01l
017000141029     c                   enddo
017100141029     c
017200141029     c                   endsr
017300141022     c
017400141022     C**************************************************************************
017500141022     c     Esiti         BEGSR
017600141022     c                   select
017700141022     c                   when      ILRY09TDIS=' '
017800141022     c                   eval      olry09esit='1'
017900141022     c* imposto i singoli check
018000141022     c                   if        idctipodis='3' and idcfla='S'
018100141022     c                   eval      olry09esdi='1'
018200141022     c                   endif
018300151116     c                   if        idctipodis='5' and idcfla='R'
018400151116     c                   eval      olry09esdi='1'
018500151116     c                   eval      olry09reso='1'
018600151116     c                   endif
018700151116     c****               if        idctipodis<>'3' or idcfla<>' '
018800151116     c                   if        idcfla= ' '
018900141022     c                   eval      olry09esal='1'
019000141022     c                   endif
019100141022
019200141022     c                   if        olry09esdi='1'  and olry09esal='1'
019300141022     c                   eval      finelet='S'
019400141022     c                   endif
019500141022
019600141022     c                   when      ILRY09TDIS='D' and idctipodis='3' and
019700141022     c                             idcfla='S'
019800141022     c                   eval      olry09esit='1'
019900141022     c                   eval      olry09esdi='1'
020000141022     c                   eval      finelet='S'
020100151116     c                   when      ILRY09TDIS='D' and idctipodis='5' and
020200151116     c                             idcfla='R'
020300151116     c                   eval      olry09esit='1'
020400151116     c                   eval      olry09esdi='1'
020500151116     c                   eval      olry09reso='1'
020600151116     c                   eval      finelet='S'
020700151116     c**                 when      ILRY09TDIS='E' and (idctipodis<>'3' or
020800151116     c**                           idcfla<>'S')
020900151116
021000151116     c                   when      ILRY09TDIS='E' and idcfla=' '
021100141022     c                   eval      olry09esit='1'
021200141022     c                   eval      olry09esal='1'
021300141022     c                   eval      finelet='S'
021400141022     c                   endsl
021500141022     c
021600141022     c                   ENDSR
021700140919     C**************************************************************************
021800140919     C     sr_esegui     BEGSR
021900140919     c                   if        ilry09nsp>0
022000140929     c     kidc1         setll     tiidc01l
022100140929     c     kidc1         reade     tiidc01l
022200140929     c                   dow       not %eof(tiidc01l)
022300140919     c
022400140929     C                   IF        IDCELAFLG='E' and
022500140929     c                             (ilry09fgs=0 or ilry09fgs=idcfgs)
022600141030     c* verifico il tipo disposizione
022700151116     c                   if        ilry09tdis='D' and idcfla= ' '
022800141030     c                   leave
022900141030     c                   endif
023000151116     c                   if        ilry09tdis='E' and idcfla<>' '
023100141030     c                   leave
023200141030     c                   endif
023300141030     c
023400141027     c                   clear                   fnlry03ds2
023500140919     c                   eval      ilry03aas=idcaas
023600140919     c                   eval      ilry03lnp=idclnp
023700140919     c                   eval      ilry03nrs=idcnrs
023800140919     c                   eval      ilry03nsp=idcnsp
023900140919     c                   eval      ilry03prg=idcprg
024000141006     c                   eval      ilry03fgs=idcfgs
024100140919     c                   eval      ilry03tipd=idctipodis
024200141027     c                   eval      ilry03df12=ILRY09DF12
024300141027     c                   eval      ilry03df06=ILRY09DF06
024400141027     c                   eval      ilry03df21=ILRY09DF21
024500141027     c                   eval      ilry03ef12=ILRY09eF12
024600141027     c                   eval      ilry03ef06=ILRY09eF06
024700141027     c                   eval      ilry03ef21=ILRY09eF21
024800141028     c                   eval      ilry03ffr =ILRY09ffr
024900141028     c
025000141028     c                   eval      ilry03pss =ILRY09pss
025100141028     c                   eval      ilry03mdb4=ILRY09mdb4
025200141028     c                   eval      ilry03mdb5=ILRY09mdb5
025300141028     c                   eval      ilry03psl =ILRY09psl
025400141028     c                   eval      ilry03bsl =ILRY09bsl
025500141028     c                   eval      ilry03prb4=ILRY09prb4
025600141028     c                   eval      ilry03prb5=ILRY09prb5
025700141028     c                   eval      ilry03mde =ILRY09mde
025800141028     c                   eval      ilry03pre =ILRY09pre
025900141028     c                   eval      ilry03mdb =ILRY09mdb
026000141028     c                   eval      ilry03prb =ILRY09prb
026100141028     c                   movel     fnlry03ds2    kpjbu
026200140919     c                   call      'FNLRY03R'
026300140919     c                   parm                    kpjba
026400141028     c                   movel     kpjbu         fnlry03ds2
026500141028     c
026600141028     c* se c'è errore lo passo nel fnlry09ds
026700141028     c                   if        olry03err<>*blanks
026800141028     c                   eval      olry09err=olry03err
026900141028     c                   eval      olry09msg=olry03msg
027000140929     c                   endif
027100141028     c                   endif
027200140929
027300140929     c     kidc1         reade     tiidc01l
027400140919     c                   enddo
027500140919     c                   else
027600140930     c                   clear                   fnlry02ds
027700140930     c                   eval      ILRY02fgs=%editc(ILRY09FGS:'X')
027800141028     c
027900141029     c                   select
028000141029     c                   when      ilry09tdis='D'
028100141028     c                   eval      ilry02dir='I'
028200141029
028300141029     c                   other
028400141028     c                   eval      ILRY02DIR='E'
028500141029     c                   endsl
028600141028     c
028700140930     c                   movel(P)  fnlry02ds     kpjbu
028800140919     c                   call      'FNLRY02R'
028900140919     c                   parm                    kpjba
029000140919     c                   endif
029100141028     c
029200140919     c                   endsr
029300991206     C**************************************************************************
029400051123     C     *INZSR        BEGSR
029500051123     c
029600140919     c     kidc5         Klist
029700140919     c                   Kfld                    ilry09fgs
029800140919     c                   Kfld                    ilry09aas
029900140919     c                   Kfld                    ilry09lnp
030000140919     c                   Kfld                    ilry09nrs
030100140919     c                   Kfld                    ilry09nsp
030200140929     c     kidc1         Klist
030300140929     c                   Kfld                    ilry09aas
030400140929     c                   Kfld                    ilry09lnp
030500140929     c                   Kfld                    ilry09nrs
030600140929     c                   Kfld                    ilry09nsp
030700140929     c                   Kfld                    WELADATA
030800140206     c
030900070313     c                   endsr
