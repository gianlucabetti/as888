000100150611     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'TIBS')
000200030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000300150611     H* FNLRu3R *----------------------------------------------------*
000400150611     H* - Ricerca e decodifica stato spedizione
000500000000     H*--------------------------------------------------------------*
000600150611     FFIARP01L  IF   E           K DISK
000700150611     Ftabel00f  IF   E           K DISK
000800030722     F**
000900070313     d
001000150611     D azlin00f      E DS                  BASED(nullPtr)
001100150611     D cvtLinguaISO2ToTntbe...
001200150611     D                 PR                  LIKE(linTntbe)
001300150611     D  rqsISO2                            LIKE(ilru3lin)
001400150611     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
001500150611     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
001600150611     D  rpyTntbe                           LIKE(linTntbe)
001700150611     D                                     OPTIONS(*NOPASS:*OMIT)
001800150611     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
001900150611     D  msgId                         7A   CONST
002000150611     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
002100150611     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
002200150611     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
002300150611     D                                     VARYING
002400150611     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
002500150611     D                                     VARYING
002600150611     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
002700150611     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
002800051123      * ds di passaggio dati
002900150611     D fnlru3ds      E DS
003000070313     d
003100150611     D tnsd99ds      E DS
003200150611     D trulor2ds     E DS
003300150611     d
003400150611     D tibs02ds      E DS
003500150611     d xgiolavds     e ds
003600070314     D KPJBA         E DS
003700150611      * stato data prev cons
003800150611     d dspc          E DS
003900150611     d
004000150611     d DS2A          E DS
004100150611     d dvpooraris    e ds                  inz
004200150611
004300150611     d wlbda8          DS                  INZ                                  *controlla data
004400150611     d  g08dat                        8  0                                       -data dritta
004500150611     d  g08inv                        8  0                                       -data invertita
004600150611     d  g08err                        1                                          -errore
004700150611     d  g08tgi                        5  0                                       -giorni fra date
004800150611      * scomposizione data generica
004900150611     d dsdat           DS                  INZ                                  *data
005000150611     d  dsann                         4  0                                       -anno
005100150611     d  dsmes                         2  0                                       -mese
005200150611     d  dsgio                         2  0                                       -giorno
005300150611      * scomposizione data generica (anno + mese/giorno)
005400150611     d                 DS                  INZ                                  *data aa + mm/gg
005500150611     d  dsannB                 1      4  0                                       -anno
005600150611     d  dsmgsB                 5      8  0                                       -mese/giorno
005700150611     d dsdatB                  1      8  0                                      *data aa + mm/gg
005800150611      * composizione data generica editata
005900160506     d dsdate          DS                  INZ
006000150611     d  dsgioe                        2  0                                       -giorno
006100150611     d  dsvd1                         1                                          -'/'
006200150611     d  dsmese                        2  0                                       -mese
006300150611     d  dsvd2                         1                                          -'/'
006400150611     d  dsanne                        4  0                                       -anno
006500150611      * scomposizione ora generica
006600150611     d dsora           DS                  INZ                                  *ora
006700150611     d  dshh                          2  0                                       -ore
006800150611     d  dsmm                          2  0                                       -minuti
006900150611      * composizione ora generica editata
007000150611     d dsorae          DS                  INZ                                  *ora
007100150611     d  dshhe                         2  0                                       -ore
007200150611     d  dsvo1                         1                                          -':'
007300150611     d  dsmme                         2  0                                       -minuti
007400150611
007500150611     d wdat            S              8  0                                      *data (aaaammgg)
007600150611     d wdate           S             10                                         *data (gg/mm/aaaa)
007700150611     d wora            S              4  0                                      *ora  (hhmm)
007800150611     d worae           S              5                                         *ora  (hh:mm)
007900150611     D orincono74      S              5
008000150611     D orficono74      S              5
008100150611     D kcod            S                   like(tblcod)
008200150611     D kkey            S                   like(tblkey)
008300150611     d orarich         S              4  0 inz                                  *ora  (hhmm)
008400150611     d wParm           s            512a   inz
008500150611     d Wcev            S              3    inz                                  *ora  (hhmm)
008600150611     D w_dallemsg      S              5a
008700150611     D w_allemsg       S              5a
008800150611     D w_dalle         S               t
008900150611     D w_alle          S               t
009000150611     D w_dalleoser     S               t
009100150611     D w_alleoser      S               t
009200150611     d langTntbe       S                   LIKE(T02lin)
009300120417     d
009400140205     d Datasys         s               d   datfmt(*iso) inz(*sys)
009500900518     C***************************************************************
009600000000     C     *ENTRY        PLIST
009700150611     C                   PARM                    kpjba
009800150611     C                   PARM                    fnlru3ds
009900150611     C                   PARM                    tnsd99ds
010000150611     C                   PARM                    trulor2ds
010100940223     C*---------------------------------------------------------------*
010200150611     C* TIPO LANCIO, ilru3TLA "C" -           CHIUSO CON LR
010300150611     C* TIPO LANCIO, ilru3TLA "L" - ELABORO E CHIUDO CON LR
010400150611     C* TIPO LANCIO, ilru3TLA " " - ELABORO E CHIUDO IN RETRN
010500941216     C*
010600150611    1C     ilru3TLA      IFNE      'C'
010700150611     c                   clear                   olru3err
010800150611     c                   clear                   olru3msg
010900150611     c                   clear                   olru3spc
011000150611     c                   clear                   olru3dee
011100150611     c                   clear                   olru3dec1
011200150611     c                   clear                   olru3dec2
011300150611     c                   clear                   olru3not1
011400150611     c                   clear                   olru3not2
011500150611     c                   clear                   ds2a
011600150611     c                   clear                   pdaavv            1
011700150611     c                   clear                   pdagia            1
011800150611     c                   clear                   orarich
011900150611     c                   clear                   wricons           1
012000150611     c                   clear                   orincono74
012100150611     c                   clear                   orficono74
012200150611     c
012300150611     c
012400150611     c* controllo codice lingua
012500150611     c                   if        ilru3lin<>'en' and ilru3lin<>'fr'and
012600150611     c                             ilru3lin<>'de' and ilru3lin<>'it'
012700150611     c                   clear                   ilru3lin
012800150611     c                   endif
012900150611
013000150611     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(ilru3lin)
013100150611     c* stato spedizione da tnsd99ds
013200150611     c                   eval      olru3spc = d98spcdee
013300150611     c                   eval      olru3dee = d98dee
013400160721
013500160721     c* Se int bolle AS400 e stato CON e solo incasso -->
013600160721     c*  visualizzo nuovo stato
013700160721     c                   if        ilru3mod=' ' and  olru3spc='CON'  and
013800160721     c                             (ilru3icc='R' or ilru3icc=' ' or
013900160721     c                              ilru3ica=' ' or ilru3ica='R')
014000160721     c                   eval      olru3spc='COI'
014100160721     c                   endif
014200150611
014300150611     c* STATO DA PDA
014400160721     c                   if        ilru3ucev<>*blanks  and d98dcm=0
014500150611     c* Consegnata
014600160721     c                   if        ILRU3ucev='  C'
014700150611     C                   eval      OLRU3SPC='CON'
014800150611     c                   clear                   olru3dee
014900150611     c                   else
015000161104     c                   if        ilru3itcev='D'
015100150611     c                   move      'S'           wricons
015200161104     c                   endif
015300150611
015400150611     c                   eval      kcod='2A'
015500150611     c                   eval      kkey=ilru3ucev
015600150611     c     ktab          chain     tabel00f
015700150611     c                   if        %found(tabel00f)
015800150611     c                   movel     tbluni        ds2a
015900150611     c                   endif
016000150611     c
016100161111    3c                   if        ilru3itcev='D'
016200161111    4c                   if        §2aftc='A'
016300150611     c                   eval      PDAavv='S'
016400161111    4c                   endif
016500161111    4c                   if        §2aftc='G'
016600150611     c                   eval      PDAgia='S'
016700161111    4c                   endif
016800161111    3c                   endif
016900161111
017000150611     c                   endif
017100160721     c                   endif
017200150611     c
017300150611     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
017400150611     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
017500150611    1c                   if        PDAavv='S' and olru3spc ='DDC' and ilru3ntc=0
017600150611     c                   eval      olru3spc ='RIC'
017700150611     c* se presente FIARP --> visualizzo la data richiesta
017800150611     c     karp          chain     fiarp01l
017900150611    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
018000150611     c                   movel     arpdcr        olru3dee
018100150611
018200150611     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
018300150611    3C                   IF        ARPHCR>0
018400150611     c                   eval      orarich=arphcr
018500150611    3c                   endif
018600150611   x2c                   else
018700150611     c*  se presente DOPO IL imposto la data in olru3dee
018800150611    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
018900150611     c                   eval      olru3dee=arpdcr
019000150611    3c                   endif
019100150611     c* PRIMA DEL come se non ci fosse...
019200150611     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
019300150611    3c                   if        olru3dee>0
019400150611     c                   Clear                   xgiolavds
019500150611     c                   Eval      Ixglpa  = 'A'
019600150611     c                   Eval      IxglFil = ilru3lna
019700150611     c                   Eval      Ixgldata = olru3dee
019800150611     c                   Eval      Ixgladd = 'S'
019900150611     c                   Eval      Ixgllav = 'S'
020000150611     c                   eval      ixglgg  = 1
020100150611     c                   Call      'XGIOLAV'
020200150611     c                   parm                    xgiolavds
020300150611     c                   movel     oxgldata      olru3dee
020400150611    3c                   endif
020500150611    2c                   endif
020600150611    1c                   endif
020700150611     c
020800150611    1c                   if        PDAavv='S' and olru3spc ='DDC' and ilru3ntc>0
020900150611     c                   eval      olru3spc ='ATT'
021000150611     c                   endif
021100150611    1c                   if        PDAgia='S' and olru3spc ='DDC'
021200150611     c                   eval      olru3spc ='GIA'
021300150611     c                   endif
021400150611     c
021500150611     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
021600150611     c*  lo status diventa RIC
021700150611     c                   if        olru3spc ='PRV'  and ilru3ucev  ='RIC'
021800150611     c                   eval      olru3spc ='RIC'
021900150611     c                   endif
022000160506
022100160506     c* se int bolle AS400 e stato OLP o PRG e distinta fittizia -->
022200160506     c*  visualizzo nuovo stato
022300160506     c                   if        ilru3mod=' ' and (olru3spc='OLP' or
022400160506     c                             olru3spc='PRG') and d98disf='F'
022500160506     c                   eval      olru3spc='FIT'
022600160506     c                   endif
022700161027     c
022800161027     c* se consegnata e c'è c.a. devo passare seconda riga piena
022900161027     c                   if        olru3spc='CON' and ilru3ca<>' ' and
023000161027     c                             ILRU3MOD=' '
023100161027     c
023200161027     c                   eval      olru3spc='CAN'
023300161027     c                   endif
023400150611     c
023500150611     c
023600150611     c* decodifica stato da mettere nei campi deco e note
023700150611     c                   clear                   dspc
023800150611     C                   clear                   tibs02ds
023900150611     C                   EVAL      t02Cod = 'SPC'
024000150611     C                   EVAL      t02Ke1 = olru3spc
024100150611     C                   EXSR      getTntbe00f
024200161028    1c                   IF        t02Err <>'E'
024300150611     c
024400150611     c                   eval       olru3dec1 =§SPCDEC1
024500150611     c                   eval       olru3dec2 =§SPCDEC2
024600161028    2c                   if        ILRU3MOD=' '
024700161028    3c                   if        §spcdeib1<>*blanks
024800160506     c                   eval       olru3dec1 =§SPCDEIB1
024900160506     c                   eval       olru3dec2 =§SPCDEIB2
025000161028    3c                   endif
025100161027     c* per int bolle di sede, se  c.a. , visualizzo se aperta o chiusa nella seconda descr
025200161028    3c                   if        olru3dec2=*blanks and olru3spc='CA ' and
025300161027     c                             ilru3ca<>*blanks
025400161028    4c                   if        ilru3ca='A'
025500161027     c                   eval       olru3dec2 ='APERTA'
025600161027     c                   else
025700161027     c                   eval       olru3dec2 ='CHIUSA'
025800161028    4c                   endif
025900161028    3c                   endif
026000161027     c
026100161028    2c                   endif
026200150611     c*
026300150611     c* se il campo note iniziano con TIS significa che si tratta di un msg
026400150611     c                   eval       olru3not1 =§SPCNOT1
026500150611     c                   eval       olru3not2 =§SPCNOT2
026600161028
026700161028    2c                   if        olru3spc='CAN' and olru3not2=*blanks and
026800161028     c                             ILRU3MOD=' '
026900161028    3c                   if        ilru3ca='A'
027000161028     c                   eval       olru3not2 ='APERTA'
027100161028     c                   else
027200161028     c                   eval       olru3not2 ='CHIUSA'
027300161028    3c                   endif
027400161028    2c                   endif
027500161028    1c                   endif
027600161107     c* se la fase è giacenza, visualizzo anche la fase della giacenza
027700161107    2c                   if        olru3spc='GIA' and olru3not2=*blanks and
027800161107     c                             ILRU3MOD=' ' and d98gfas<>*blanks
027900161107     c                   eval      kcod='2Y'
028000161107     c                   eval      kkey=d98gfas
028100161107     c     ktab          chain     tabel00f
028200161107     c                   if        %found(tabel00f)
028300161107     c                   movel     tbluni        olru3not2
028400161107    1c                   endif
028500161107    1c                   endif
028600150611
028700150611     c* Imposto gli orari
028800150611    1c                   if        OOR2PRESD > 0  and wricons=' '
028900150611     c                   MOVEL     OOR2PRESD     wora
029000150611     c                   EXSR      edtora
029100150611     c                   eval      orincono74  =  worae
029200150611    2c                   if        OOR2PRESa > 0
029300150611     c                   MOVEL     OOR2PRESa     wora
029400150611     c                   EXSR      edtora
029500150611     c                   eval      ORficono74  =  worae
029600150611    2c                   endif
029700150611   x1c                   else
029800150611    2c                   if        OOR2STIS > 0
029900150611     c                   MOVEL     OOR2STIS      wora
030000150611     c                   EXSR      edtora
030100150611     c                   eval      orincono74  =  worae
030200150611    2c                   endif
030300150611    2c                   if        OOR2STFS > 0
030400150611     c                   MOVEL     OOR2STFS      wora
030500150611     c                   EXSR      edtora
030600150611     c                   eval      ORficono74  =  worae
030700150611    2c                   endif
030800150611    1c                   endif
030900150611     c* note 1
031000150611    1c                   if        %subst(olru3not1:1:3)='TIS'
031100150611     c                   clear                   wparm
031200150611     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
031300150611    2c                   if        D98DTCTD<>*blanks
031400150611     c                   movel     D98DTCTD      wdat
031500150611     c                   else
031600150611     c                   movel     olru3dee      wdat
031700150611     c                   endif
031800150611     c                   exsr      edtdat
031900150611     c
032000150611                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
032100150611     c                   eval      olru3not1 = rtvMsgLang(
032200150611     c                             %subst(olru3not1:1:7):ilru3lin:wparm)
032300150611    1c                   endif
032400150611     c* note 2
032500150611    1c                   if        %subst(olru3not2:1:3)='TIS'
032600150611     c                   clear                   wparm
032700150611     c* Se lo stato è PRG passo orario limite uscita aut
032800150611     c                   select
032900150611    2c                   when      olru3spc ='PRG'
033000150611    3c                   IF        d98picklim  >*zeros
033100150611
033200150611     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
033300150611     c                                   (%subst(d98picklim:3:2))
033400150611   x3c                   else
033500150611     c                   eval      wparm='09:30'
033600150611     c* reperisco orario di fine picking
033700150611     c* filiale / data
033800150611     C                   clear                   tibs02ds
033900150611     C                   EVAL      t02Cod = 'OLP'
034000150611     C                   EVAL      t02Ke1 = %editc(ilru3lna:'X')
034100150611     C                   EVAL      t02Ke2 = %editc(%dec(datasys):'X')
034200150611     C                   EXSR      getTntbe00f
034300150611     c* filiale
034400150611    4c                   IF        t02Err = 'E'
034500150611     C                   clear                   tibs02ds
034600150611     C                   EVAL      t02Cod = 'OLP'
034700150611     C                   EVAL      t02Ke1 = %editc(ilru3lna:'X')
034800150611     C                   EXSR      getTntbe00f
034900150611    4c                   endif
035000150611     c* Generico 000
035100150611    4c                   IF        t02Err = 'E'
035200150611     C                   clear                   tibs02ds
035300150611     C                   EVAL      t02Cod = 'OLP'
035400150611     C                   EVAL      t02Ke1 = '000'
035500150611     C                   EXSR      getTntbe00f
035600150611    4c                   endif
035700150611    4c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
035800150611     c                   eval      wparm=%subst(t02uni:1:2)+':'+
035900150611     c                             %subst(t02uni:3:2)
036000150611    4c                   endif
036100150611    3c                   endif
036200150611     c                   eval      olru3not2 = rtvMsgLang(
036300150611     c                             %subst(olru3not2:1:7):ilru3lin:wparm)
036400150611    2c                   other
036500150611     c* altri casi
036600150611     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
036700150611    3c                   if        ilru3ntc>0 and ilru3hcr>0 and orarich=0 and
036800150611     c                             ilru3tcr=' ' and ilru3dcr>0 and
036900150611     c                             ilru3dcr >= olru3dee
037000150611     c                   eval      orarich=ilru3hcr
037100150611    3c                   endif
037200150611     c
037300150611    3c                   if        orarich>0
037400150611     c                   exsr      sr_dallealle
037500150611     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
037600150611     c                   eval      olru3not2 = rtvMsgLang(
037700150611     c                             %subst(olru3not2:1:7):ilru3lin:wparm)
037800150611     c
037900150611   x3c                   else
038000150611     c                   clear                   olru3not2
038100150611    3c                   endif
038200150611     c
038300150611    2c                   endsl
038400150611     c
038500150611    1c                   endif
038600150611     c
038700160506     c* Se stato EXPORT --> verifico la data invio al partner
038800160719    1c                   if        olru3spc ='EXP' and ilru3mod=' '
038900160506     c                   if        ilru3dip>0
039000160506     c                   movel     ilru3dip      wdat
039100160506     c                   exsr      edtdat
039200160506     c                   else
039300160506     c                   clear                   wdate
039400160506     c                   endif
039500160506     c
039600160506     c                   eval      olru3not1=%trim(olru3not1)+ ' ' +wdate
039700160506    1c                   endif
039800150611     c
039900070314     c
040000070314     c     fine          TAG
040100070314     c
040200070314     c                   endif
040300051123     c
040400150611     C     ilru3tla      IFNE      ' '
040500051123     C                   SETON                                        LR
040600150611     c                   clear                   tibs02ds
040700140205     c                   eval      t02tla='C'
040800140205     c                   call      'TIBS02R'
040900150611     c                   parm                    kpjba
041000150611     c                   parm                    tibs02ds
041100140205     c
041200051123     C                   ELSE
041300051123     C                   SETON                                        RT
041400051123     C                   ENDIF
041500991206     C**************************************************************************
041600051123     C     *INZSR        BEGSR
041700051123     c
041800150611     c     kARP          Klist
041900150611     c                   Kfld                    ilru3Aas
042000150611     c                   Kfld                    ilru3Lnp
042100150611     c                   Kfld                    ilru3nrs
042200150611     c                   Kfld                    ilru3nsp
042300150611     C     Ktab          KLIST
042400150611     C                   KFLD                    codut             1 0
042500150611     C                   KFLD                    kcod
042600150611     C                   KFLD                    kkey
042700160523     c                   z-add     1             codut
042800051123     c
042900070313     c                   endsr
043000150611     c*--------------------------------------------------------------------------------------------*
043100150611     c* edita data da aaaammgg in gg/mm/aaaa
043200150611     c*--------------------------------------------------------------------------------------------*
043300150611     c     edtdat        BEGSR
043400150611     c*
043500150611     c                   MOVEL     *blanks       wdate
043600150611     c*
043700150611     c                   MOVEL     wdat          dsdat
043800150611     c                   MOVEL     dsgio         dsgioe
043900150611     c                   MOVEL     '.'           dsvd1
044000150611     c                   MOVEL     dsmes         dsmese
044100150611     c                   MOVEL     '.'           dsvd2
044200150611     c                   MOVEL     dsann         dsanne
044300150611     c                   MOVEL     dsdate        wdate
044400150611     c*
044500150611     c                   ENDSR
044600150611     c*--------------------------------------------------------------------------------------------*
044700150611     c* edita ora da hhmm in hh:mm
044800150611     c*--------------------------------------------------------------------------------------------*
044900150611     c     edtora        BEGSR
045000150611     c*
045100150611     c                   MOVEL     *blanks       worae
045200150611     c*
045300150611     c                   MOVEL     wora          dsora
045400150611     c                   MOVEL     dshh          dshhe
045500150611     c                   MOVEL     '.'           dsvo1
045600150611     c                   MOVEL     dsmm          dsmme
045700150611     c                   MOVEL     dsorae        worae
045800150611     c*
045900150611     c                   ENDSR
046000150611     ***********************************************************************************************
046100150611     ** Reperisco tabella da TNTBE00F.
046200150611     ***********************************************************************************************
046300150611     C     getTntbe00f   BEGSR
046400150611     C                   EVAL      t02mod = 'C'
046500150611     C                   EVAL      t02Lin = langTntbe
046600150611     C                   CALL      'TIBS02R'
046700150611     c                   parm                    kpjba
046800150611     c                   parm                    tibs02ds
046900150611     c                   select
047000150611     C                   WHEN      t02Cod = 'SPC'
047100150611     C                   EVAL      dSPC = t02Uni
047200150611     C                   ENDSL
047300150611     C                   ENDSR
047400150611
047500150611      /free
047600150611       //--------------------------------------------------------------
047700160506       //?Determinazione del "Dalle - Alle" per messaggio di conferma   ?
047800150611       //--------------------------------------------------------------
047900150611       BEGSR  sr_dallealle;
048000150611       clear  w_dallemsg ;
048100150611       clear  w_allemsg ;
048200150611
048300150611       // Reperimento dati tabella VPO - ORARISER
048400150611       if §VPORST_MW=0  ;
048500150611       clear tibs02ds;
048600150611       clear dvpooraris;
048700150611       t02cod='VPO';
048800150611       t02ke1='ORARISER';
048900150611       exsr gettntbe00f ;
049000150611       if t02err=*blanks;
049100150611          dvpooraris=t02uni;
049200150611       endif;
049300150611       endif;
049400150611
049500150611            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
049600150611            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
049700150611       //   determino orari servizio comprensivi della tolleranza
049800150611               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
049900150611               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
050000150611
050100150611            if w_dalle<w_dalleoser;
050200150611               w_dalle=w_dalleoser;
050300150611               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
050400150611            endif;
050500150611
050600150611            if w_alle>w_alleoser;
050700150611               w_alle=w_alleoser;
050800150611               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
050900150611                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
051000150611               endif;
051100150611            endif;
051200150611            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
051300150611            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
051400150611
051500150611       endsr;
051600150611      /end-free
