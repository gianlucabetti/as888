000100030414     h DECEDIT('0,') DATEDIT(*DMY.)
000200011108      *--------------------------------------------------------------------------------------------*
000300030414      * Gestione Password ristampa segnacolli                                                      *
000400011108      *--------------------------------------------------------------------------------------------*
000500030414
000600030508     fTNTB41D   cf   e             Workstn
000700030414     fTntbe01l  uf a e           k Disk
000800030414     fAzorg01l  if   e           k Disk
000900030414
001000030415      *--------------------------------------------------------------------------------------------*
001100030414
001200080319     d C_DigitN        c                   const('1234567890')
001300080526     d c_F03           c                   const(x'33')
001400080319
001500030508     d skTab           s              3    dim(10) ctdata perrcd(1)
001600080129     d skDesPgm        s             40    dim(10) alt(skTab)
001700030415     d Msg             s             78    dim(10) ctdata perrcd(1)
001800030414
001900030415     d dRsa          e ds
002000030508     d dMtc          e ds
002100040421     d dYna          e ds
002200070212     d dsdp          e ds
002300030414     d Tibs02ds      e ds
002400030414
002500030415     d Kpjba         e ds
002600020715
002700020715     d DdatiUte      e ds
002800020715     d AzuteDs       e ds                  ExtName(Azute00f)
002900080526     d dUTE01        e ds
003000020715     d Tibs34Ds      e ds                  Inz
003100080129      * DS per TRUL31R - Caricamento P.O. abilitati
003200080129     d Trul31Ds      e ds
003300080129     d  Opg                   10    759  0 dim(250)
003400030414
003500080526     d                sds
003600080526     d  SDSpgm           *proc
003700030508
003800040421     D Parametri       ds
003900040421     d  wTabella                      3    inz(*blanks)
004000040421     d  wTasto                        1                                         Tasto di uscita
004100080129     d xi              s              2  0 inz(*zeros)
004200030414
004300011108      *--------------------------------------------------------------------------------------------*
004400030414
004500011108      * Gestione video
004600030414
004700030414     c                   Clear                   V1Pwdold
004800030414     c                   Clear                   V1Pwdnew
004900030414     c                   Clear                   V1Pwdtst
005000030415
005100030415     c                   Do        *Hival
005200030414
005300030508     c                   Exfmt     TB41d01
005400030414
005500030414     c                   setoff                                       28
005600030415
005700080319sel 2c                   select
005800080526      * Utente non abilitato
005900080529     c                   when          §UtePwdPda <> 'S'
006000080530     c                             and (wTabella  =  'RSA'
006100080530     c                              or  wTabella  =  'MTC'
006200080530     c                              or  wtabella  =  'SDP')
006300080526     c                   eval      wTasto = c_F03
006400080526     c                   leave
006500030414      * F3=Fine
006600080319w   2c                   when      *inKC
006700080526     c                   eval      wTasto = c_F03
006800080319     c                   leave
006900030415
007000030414      * F18=Cambio P.O. in gestione
007100080319w   2c                   when      *inKS
007200080319     c                   eval      *in05 = *on
007300080319     c                   iter
007400080319e   2c                   endsl
007500030414
007600030414      * 05 on --> controllo P.O. in gestione
007700030414     c                   If        *In05
007800030414     c                   ExSr      Sr_ContrFgs
007900030414     c  n28              Eval      *In05 = *Off
008000030414     c   28              Iter
008100030414     c                   EndIf
008200030414
008300030414      * Controlli
008400030414     c                   ExSr      Sr_Contr
008500030414
008600030415      * F6=Conferma
008700080319if  2c                   if        *inKF  and  NOT *in28
008800080319     c                   ExSr      Sr_Aggiorna
008900080319     c                   Leave
009000080319e   2c                   endif
009100030414
009200030414     c                   EndDo
009300030414
009400040421     C                   Eval      Kpjbu = Parametri
009500030414     c                   Eval      *InLr = *On
009600030414
009700030414      *--------------------------------------------------------------------------------------------*
009800030414      * Controllo il p.o. in gestione -
009900030414      *--------------------------------------------------------------------------------------------*
010000030414     c     Sr_ContrFgs   BegSr
010100030414
010200080319     c                   eval      *in40 = *off
010300030414     c                   Clear                   V1dFgs
010400030414
010500080319if  1c                   IF        %subst(KNMUS : 1 : 3) <> 'EDP'
010600080129     c     V1Fgs         Lookup    Opg                                    30
010700030414     c                   If        Not *In30
010800030414     c                   Eval      *In40 = *On
010900030414     c                   Eval      *In28 = *On
011000030414     c                   Eval      V1Msg = Msg(01)
011100080319     c                   leavesr
011200080319     c                   EndIf
011300080319e   1c                   ENDIF
011400030414
011500030414      * Decodifica
011600030414     c     V1Fgs         Chain     Azorg01L
011700030414     c                   If        %Found(Azorg01l) and OrgFva = *Blanks
011800030414     c                   Movel     OrgDes        V1dFgs
011900030414     c                   EndIf
012000030414
012100030508      * ricarico Tabella
012200030508     c                   Clear                   dRsa
012300030508     c                   Clear                   dMtc
012400040421     c                   Clear                   dYna
012500070212     c                   clear                   dsdp
012600030414     c                   Clear                   Tibs02Ds
012700030414     c                   Eval      T02Mod = 'C'
012800030414     c                   Eval      T02Sif = Knsif
012900030508     c                   Eval      T02Cod = wTabella
013000030414     c                   Movel(p)  V1Fgs         T02Ke1
013100030414     c                   Call      'TIBS02R'
013200030414     c                   Parm                    Kpjba
013300030414     c                   Parm                    Tibs02Ds
013400030414     c                   If        T02Err = *Blanks
013500030508     c*
013600030508     c* A seconda della tabella che si sta gestendo...
013700040421     C                   Select
013800040421     c                   When      wTabella = 'RSA'
013900030508     c                   Movel     T02Uni        dRsa
014000040421     c                   When      wTabella = 'MTC'
014100030508     c                   Movel     T02Uni        dMtc
014200040421     c                   When      wTabella = 'YNA'
014300040421     c                   MoveL     T02Uni        dYna
014400070212     c                   when      wtabella = 'SDP'
014500070212     c                   movel     t02uni        dsdp
014600040421     c                   EndSl
014700030508     c*
014800030414     c                   EndIf
014900030414
015000030414     c                   EndSr
015100030414
015200030414      *--------------------------------------------------------------------------------------------*
015300030414      * Controllo la videata -
015400030414      *--------------------------------------------------------------------------------------------*
015500030414     c     Sr_Contr      BegSr
015600030414
015700030415     c                   setoff                                       414243
015800030415
015900080319      * A seconda della tabella che si sta gestendo...
016000080319      *
016100080319sel 1c                   SELECT
016200080319      *
016300080319w   1c                   WHEN      wTabella = 'RSA'
016400030415      * Password attuale
016500080319sel 2c                   Select
016600080319w   2c                   When      V1pwdold = *Blanks and §RsaPwd <> *Blanks
016700030415     c                   Eval      *In41 = *On
016800030415     c                   Eval      *In28 = *On
016900030415     c                   Eval      V1Msg = Msg(2)
017000080319     c                   LeaveSr
017100080319w   2c                   When      V1pwdold <> §RsaPwd and
017200080319     c                             §RsaPwd <> *Blanks
017300030508     c                   Eval      *In41 = *On
017400030415     c                   Eval      *In28 = *On
017500030415     c                   Eval      V1Msg = Msg(3)
017600030415     c                   Clear                   V1Pwdold
017700080319     c                   LeaveSr
017800080319e   2c                   EndSl
017900040421
018000080319w   1c                   WHEN      wTabella = 'MTC'
018100030508      * Password attuale
018200080319sel 2c                   Select
018300080319sel 2c                   When      V1pwdold = *Blanks and §MtcPwd <> *Blanks
018400030508     c                   Eval      *In41 = *On
018500030508     c                   Eval      *In28 = *On
018600030508     c                   Eval      V1Msg = Msg(2)
018700080319     c                   LeaveSr
018800080319w   2c                   When      V1pwdold <> §MtcPwd and
018900030508     c                             §MtcPwd <> *Blanks
019000030508     c                   Eval      *In41 = *On
019100030508     c                   Eval      *In28 = *On
019200030508     c                   Eval      V1Msg = Msg(3)
019300030508     c                   Clear                   V1Pwdold
019400080319     c                   LeaveSr
019500080319e   2c                   EndSl
019600040421
019700080319w   1c                   WHEN      wTabella = 'YNA'
019800040421      * Password attuale
019900080319sel 2c                   Select
020000080319w   2c                   When      V1pwdold = *Blanks and §YnaPwd <> *Blanks
020100040421     c                   Eval      *In41 = *On
020200040421     c                   Eval      *In28 = *On
020300040421     c                   Eval      V1Msg = Msg(2)
020400080319     c                   LeaveSr
020500080319w   2c                   When      V1pwdold <> §YnaPwd and
020600040421     c                             §YnaPwd <> *Blanks
020700040421     c                   Eval      *In41 = *On
020800040421     c                   Eval      *In28 = *On
020900040421     c                   Eval      V1Msg = Msg(3)
021000040421     c                   Clear                   V1Pwdold
021100080319     c                   LeaveSr
021200080319e   2c                   EndSl
021300070212
021400080319w   1c                   WHEN      wtabella = 'SDP'
021500070212      * Password attuale
021600080319sel 2c                   select
021700080319w   2c                   when      v1pwdold = *blanks and §sdppwd <> *blanks
021800070212     c                   eval      *in41 = *on
021900070212     c                   eval      *in28 = *on
022000070212     c                   eval      v1msg = msg(2)
022100080319     c                   leavesr
022200080319w   2c                   when      v1pwdold <> §sdppwd and
022300070212     c                             §sdppwd <> *blanks
022400070212     c                   eval      *in41 = *on
022500070212     c                   eval      *in28 = *on
022600070212     c                   eval      v1msg = msg(3)
022700070212     c                   clear                   v1pwdold
022800080319     c                   leavesr
022900080319e   2c                   endsl
023000080319
023100080319e   1c                   ENDSL
023200030415
023300030415      * Password nuova
023400080319sel 1c                   Select
023500080319w   1c                   When      V1pwdnew = *Blanks
023600030415     c                   Eval      *In42 = *On
023700030415     c                   Eval      *In28 = *On
023800030415     c                   Eval      V1Msg = Msg(4)
023900080319     c                   LeaveSr
024000080319w   1c                   When      V1pwdold = V1pwdnew
024100030415     c                   Eval      *In42 = *On
024200030415     c                   Eval      *In28 = *On
024300030415     c                   Eval      V1Msg = Msg(7)
024400030415     c                   Clear                   V1Pwdnew
024500080319     c                   LeaveSr
024600080319e   1c                   EndSl
024700030415
024800030415      * Password di verifica
024900080319sel 1c                   Select
025000080319w   1c                   When      V1pwdtst = *Blanks
025100030415     c                   Eval      *In43 = *On
025200030415     c                   Eval      *In28 = *On
025300030415     c                   Eval      V1Msg = Msg(5)
025400080319     c                   LeaveSr
025500080319w   1c                   When      V1pwdnew <> V1pwdtst
025600030415     c                   Eval      *In43 = *On
025700030415     c                   Eval      *In28 = *On
025800030415     c                   Eval      V1Msg = Msg(6)
025900030415     c                   Clear                   V1Pwdtst
026000080319     c                   LeaveSr
026100080319e   1c                   EndSl
026200030414
026300080319     c                   EndSr
026400030415
026500030415      *--------------------------------------------------------------------------------------------*
026600030508      * Aggiorno la tabella
026700030415      *--------------------------------------------------------------------------------------------*
026800030415     c     Sr_Aggiorna   BegSr
026900030415
027000030415     c                   Movel(p)  V1Fgs         TbeKe1
027100030415     c                   Clear                   TbeKe2
027200030415     c                   Clear                   TbeLin
027300030415     c                   Clear                   TbeSif
027400030415     c     kTbe          Chain     Tntbe01l
027500030508     c*
027600030508     c* A seconda della tabella che si sta gestendo...
027700080319sel 1c                   SELECT
027800080319      *
027900080319w   1c                   WHEN      wTabella = 'RSA'
028000030508     c                   Z-Add     *Date         §RsaDta
028100030415     c                   Eval      §RsaPwd = V1pwdnew
028200030415     c                   Movel(p)  dRsa          TbeUni
028300030725     c                   Eval      TbeFtt = 'S'
028400030725     c                   Eval      TbeFlt = V1Fgs
028500040421
028600080319w   1c                   WHEN      wTabella = 'MTC'
028700030508     c                   Z-Add     *Date         §MtcDta
028800030508     c                   Eval      §MtcPwd = V1pwdnew
028900030508     c                   Movel(p)  dMtc          TbeUni
029000030725     c                   If        V1Fgs = 046
029100030725     c                   Eval      TbeFtt = *blanks
029200030725     c                   Eval      TbeFlt = *zeros
029300030725     c                   Else
029400030725     c                   Eval      TbeFtt = 'S'
029500030725     c                   Eval      TbeFlt = V1Fgs
029600030725     c                   Endif
029700040421
029800080319w   1c                   WHEN      wTabella = 'YNA'
029900040421     c                   Z-Add     *Date         §YnaDta
030000040421     c                   Eval      §YnaPwd = V1pwdnew
030100040421     c                   Movel(p)  dYna          TbeUni
030200040421     c                   Clear                   TbeFtt                         Non trasmettere
030300040421     c                   Clear                   TbeFlt
030400070212
030500080319w   1c                   WHEN      wtabella = 'SDP'
030600070212     c                   z-add     *date         §sdpdta
030700070212     c                   eval      §sdppwd = v1pwdnew
030800070212     c                   movel(p)  dsdp          tbeuni
030900070212     c                   eval      tbeftt = 'S'
031000070212     c                   eval      tbeflt = v1fgs
031100080319      *
031200080319     c                   ENDSL
031300030508     c*
031400030415     c                   If        %Found(Tntbe01l)
031500030415     c                   Update    Tntbe000
031600030415     c                   Else
031700030415     c                   Movel(p)  V1Fgs         TbeKe1
031800030415     c                   Clear                   TbeKe2
031900030415     c                   Clear                   TbeLin
032000030415     c                   Clear                   TbeSif
032100030415     c                   Write     Tntbe000
032200030415     c                   EndIf
032300030415
032400030415     c                   EndSr
032500030415
032600011108      *--------------------------------------------------------------------------------------------*
032700030414      * *Inzsr - operazioni iniziali
032800011108      *--------------------------------------------------------------------------------------------*
032900030414     c     *Inzsr        BegSr
033000030414
033100030415     c     *Entry        Plist
033200030415     c                   Parm                    Kpjba
033300030508     c*
033400040421     c                   movel(p)  kpjbu         Parametri
033500030508     c                   movel(p)  wTabella      TbeCod
033600030414
033700030415     c     kTbe          Klist
033800030508     c                   Kfld                    TbeCod
033900030415     c                   Kfld                    TbeKe1
034000030415     c                   Kfld                    TbeKe2
034100030415     c                   Kfld                    TbeLin
034200030415     c                   Kfld                    TbeSif
034300030414
034400011108      * Reperisce dati utente
034500020715     c     *dtaara       define    §azute        azuteds
034600020715     c     *dtaara       define    §datiute      ddatiute
034700020715
034800020715     c                   in(E)     *dtaara
034900020715     c                   if        %Error or rsut = *blanks
035000020715     c                   clear                   tibs34ds
035100020715     c                   call      'TIBS34R'
035200020715     c                   parm                    Tibs34Ds
035300020715     c                   in        *dtaara
035400020715     c                   endif
035500080526      *
035600080526     c                   movel     UTEfaf        dUTE01
035700080526
035800080526     c                   eval      V1pgm  = SDSpgm
035900030414
036000030414      * Imposto p.o. gestione
036100030414     c                   Movel     DutPou        V1Fgs
036200030415     c     V1Fgs         Chain     Azorg01l
036300030414     c                   If        %Found(Azorg01l) and OrgFva = *Blanks
036400030414     c                   Movel     OrgDes        V1dfgs
036500030414     c                   EndIf
036600030508
036700030508      * Imposto la descrizione a video della tabella che si sta gestendo
036800080129     c                   Z-Add     1             xi
036900080129     c     wTabella      Lookup    skTab(xi)                              55
037000030508     c                   If        %found
037100080129     c                   Eval      V1DES = skDesPgm(xi)
037200030508     c                   Else
037300030508     c                   Eval      V1DES = *all'*'
037400030508     c                   Endif
037500030508     c
037600030414
037700080526     *** * Abilito F18 solo se il p.o. utente è simfel
037800080526     ***c                   If        DutPou <> Simfel
037900080526      * Abilito F18 solo se l'utente è EDP* o POC*
038000080526     c                   If            (%subst(KNMUS:1:3) <> 'EDP'
038100080526     c                             and  %subst(KNMUS:1:3) <> 'POC')
038200030414     c                   Eval      *In06 = *On
038300030414     c                   EndIF
038400080526
038500080526      * Se utente NON abilitato: visualizza messaggio per uscire
038600080526      *   subito dal programma
038700080529     c                   if            §UtePwdPda <> 'S'
038800080530     c                             and (wTabella  =  'RSA'
038900080530     c                              or  wTabella  =  'MTC'
039000080530     c                              or  wtabella  =  'SDP')
039100080526     c                   eval      *in28 = *on
039200080526     c                   eval      V1Msg = Msg(8)
039300080526     c                   leavesr
039400080526     c                   endif
039500030414
039600030414      * Carico schiera P.O. gestiti
039700080129     c                   clear                   TRUL31ds
039800080129     c                   eval      I31cdi = DUTdis
039900080129     c                   eval      I31car = DUTare
040000080129     c                   eval      I31cpo = DUTpou
040100080129     c                   eval      I31abi = 'AP'
040200080129     c                   call      'TRUL31R'
040300080129     c                   parm                    Kpjba
040400080129     c                   parm                    TRUL31ds
040500030414
040600030508      * Carico Tabella
040700030508     c                   Clear                   dRsa
040800030508     c                   Clear                   dMtc
040900040421     c                   Clear                   dYna
041000070212     c                   clear                   dsdp
041100030414     c                   Clear                   Tibs02Ds
041200030414     c                   Eval      T02Mod = 'C'
041300030414     c                   Eval      T02Sif = Knsif
041400030508     c                   Eval      T02Cod = wTabella
041500030414     c                   Movel(p)  V1Fgs         T02Ke1
041600030414     c                   Call      'TIBS02R'
041700030414     c                   Parm                    Kpjba
041800030414     c                   Parm                    Tibs02Ds
041900030414     c                   If        T02Err = *Blanks
042000030508     c*
042100030508     c* A seconda della tabella che si sta gestendo...
042200040421     C                   Select
042300040421     c                   When      wTabella = 'RSA'
042400030508     c                   Movel     T02Uni        dRsa
042500040421     c                   When      wTabella = 'MTC'
042600030508     c                   Movel     T02Uni        dMtc
042700040421     c                   When      wTabella = 'YNA'
042800040421     c                   Movel     T02Uni        dYna
042900070212     c                   when      wtabella = 'SDP'
043000070212     c                   movel     t02uni        dsdp
043100040421     c                   EndSl
043200030508     c*
043300030414     c                   EndIf
043400030414
043500030414     c                   EndSr
043600030414
043700080129** - skTAB / skDESpgm                        *
043800080129RSA   Ristampa segnacolli in arrivo                                           01
043900080129MTC   Manutenzione tariffe  clienti                                           02
044000080129YNA    Gestione note di accredito                                             03
044100080214SDP  Sblocco distinte scaricate a PDA                                         04
044200080408                                                                              05
044300030508                                                                              06
044400030508                                                                              07
044500030508                                                                              08
044600030508                                                                              09
044700030508                                                                              10
044800000000** -MSG-
044900080129Fil. gestione errata o non gestibile                                          01
045000030415Inserire la password attuale                                                  02
045100030415Password attutale errata                                                      03
045200030415Inserire la nuova password                                                    04
045300030415Inserire la password per la verifica                                          05
045400030415La password di verifica non coincide con la nuova appena inserita             06
045500030415La nuova password deve essere diversa da quella attuale                       07
045600080526Utente NON abilitato. Premere Invio per uscire dal programma.                 08
045700030508                                                                              09
045800030508                                                                              10
