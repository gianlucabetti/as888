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
001300080319
001400030508     d skTab           s              3    dim(10) ctdata perrcd(1)
001500080129     d skDesPgm        s             40    dim(10) alt(skTab)
001600030415     d Msg             s             78    dim(10) ctdata perrcd(1)
001700030414
001800030415     d dRsa          e ds
001900030508     d dMtc          e ds
002000040421     d dYna          e ds
002100070212     d dsdp          e ds
002200030414     d Tibs02ds      e ds
002300030414
002400030415     d Kpjba         e ds
002500020715
002600020715     d DdatiUte      e ds
002700020715     d AzuteDs       e ds                  ExtName(Azute00f)
002800020715     d Tibs34Ds      e ds                  Inz
002900080129      * DS per TRUL31R - Caricamento P.O. abilitati
003000080129     d Trul31Ds      e ds
003100080129     d  Opg                   10    759  0 dim(250)
003200030414
003300030414     d                SDS
003400030414     d  VTPGM                  1     10
003500030508
003600040421     D Parametri       ds
003700040421     d  wTabella                      3    inz(*blanks)
003800040421     d  wTasto                        1                                         Tasto di uscita
003900080129     d xi              s              2  0 inz(*zeros)
004000030414
004100011108      *--------------------------------------------------------------------------------------------*
004200030414
004300011108      * Gestione video
004400030414
004500030414     c                   Clear                   V1Pwdold
004600030414     c                   Clear                   V1Pwdnew
004700030414     c                   Clear                   V1Pwdtst
004800030415
004900030415     c                   Do        *Hival
005000030414
005100030508     c                   Exfmt     TB41d01
005200030414
005300030414     c                   setoff                                       28
005400030415
005500080319sel 2c                   select
005600030414      * F3=Fine
005700080319w   2c                   when      *inKC
005800080319     c                   leave
005900030415
006000030414      * F18=Cambio P.O. in gestione
006100080319w   2c                   when      *inKS
006200080319     c                   eval      *in05 = *on
006300080319     c                   iter
006400080319e   2c                   endsl
006500030414
006600030414      * 05 on --> controllo P.O. in gestione
006700030414     c                   If        *In05
006800030414     c                   ExSr      Sr_ContrFgs
006900030414     c  n28              Eval      *In05 = *Off
007000030414     c   28              Iter
007100030414     c                   EndIf
007200030414
007300030414      * Controlli
007400030414     c                   ExSr      Sr_Contr
007500030414
007600030415      * F6=Conferma
007700080319if  2c                   if        *inKF  and  NOT *in28
007800080319     c                   ExSr      Sr_Aggiorna
007900080319     c                   Leave
008000080319e   2c                   endif
008100030414
008200030414     c                   EndDo
008300030414
008400040421     C                   Eval      Kpjbu = Parametri
008500030414     c                   Eval      *InLr = *On
008600030414
008700030414      *--------------------------------------------------------------------------------------------*
008800030414      * Controllo il p.o. in gestione -
008900030414      *--------------------------------------------------------------------------------------------*
009000030414     c     Sr_ContrFgs   BegSr
009100030414
009200080319     c                   eval      *in40 = *off
009300030414     c                   Clear                   V1dFgs
009400030414
009500080319if  1c                   IF        %subst(KNMUS : 1 : 3) <> 'EDP'
009600080129     c     V1Fgs         Lookup    Opg                                    30
009700030414     c                   If        Not *In30
009800030414     c                   Eval      *In40 = *On
009900030414     c                   Eval      *In28 = *On
010000030414     c                   Eval      V1Msg = Msg(01)
010100080319     c                   leavesr
010200080319     c                   EndIf
010300080319e   1c                   ENDIF
010400030414
010500030414      * Decodifica
010600030414     c     V1Fgs         Chain     Azorg01L
010700030414     c                   If        %Found(Azorg01l) and OrgFva = *Blanks
010800030414     c                   Movel     OrgDes        V1dFgs
010900030414     c                   EndIf
011000030414
011100030508      * ricarico Tabella
011200030508     c                   Clear                   dRsa
011300030508     c                   Clear                   dMtc
011400040421     c                   Clear                   dYna
011500070212     c                   clear                   dsdp
011600030414     c                   Clear                   Tibs02Ds
011700030414     c                   Eval      T02Mod = 'C'
011800030414     c                   Eval      T02Sif = Knsif
011900030508     c                   Eval      T02Cod = wTabella
012000030414     c                   Movel(p)  V1Fgs         T02Ke1
012100030414     c                   Call      'TIBS02R'
012200030414     c                   Parm                    Kpjba
012300030414     c                   Parm                    Tibs02Ds
012400030414     c                   If        T02Err = *Blanks
012500030508     c*
012600030508     c* A seconda della tabella che si sta gestendo...
012700040421     C                   Select
012800040421     c                   When      wTabella = 'RSA'
012900030508     c                   Movel     T02Uni        dRsa
013000040421     c                   When      wTabella = 'MTC'
013100030508     c                   Movel     T02Uni        dMtc
013200040421     c                   When      wTabella = 'YNA'
013300040421     c                   MoveL     T02Uni        dYna
013400070212     c                   when      wtabella = 'SDP'
013500070212     c                   movel     t02uni        dsdp
013600040421     c                   EndSl
013700030508     c*
013800030414     c                   EndIf
013900030414
014000030414     c                   EndSr
014100030414
014200030414      *--------------------------------------------------------------------------------------------*
014300030414      * Controllo la videata -
014400030414      *--------------------------------------------------------------------------------------------*
014500030414     c     Sr_Contr      BegSr
014600030414
014700030415     c                   setoff                                       414243
014800030415
014900080319      * A seconda della tabella che si sta gestendo...
015000080319      *
015100080319sel 1c                   SELECT
015200080319      *
015300080319w   1c                   WHEN      wTabella = 'RSA'
015400030415      * Password attuale
015500080319sel 2c                   Select
015600080319w   2c                   When      V1pwdold = *Blanks and §RsaPwd <> *Blanks
015700030415     c                   Eval      *In41 = *On
015800030415     c                   Eval      *In28 = *On
015900030415     c                   Eval      V1Msg = Msg(2)
016000080319     c                   LeaveSr
016100080319w   2c                   When      V1pwdold <> §RsaPwd and
016200080319     c                             §RsaPwd <> *Blanks
016300030508     c                   Eval      *In41 = *On
016400030415     c                   Eval      *In28 = *On
016500030415     c                   Eval      V1Msg = Msg(3)
016600030415     c                   Clear                   V1Pwdold
016700080319     c                   LeaveSr
016800080319e   2c                   EndSl
016900040421
017000080319w   1c                   WHEN      wTabella = 'MTC'
017100030508      * Password attuale
017200080319sel 2c                   Select
017300080319sel 2c                   When      V1pwdold = *Blanks and §MtcPwd <> *Blanks
017400030508     c                   Eval      *In41 = *On
017500030508     c                   Eval      *In28 = *On
017600030508     c                   Eval      V1Msg = Msg(2)
017700080319     c                   LeaveSr
017800080319w   2c                   When      V1pwdold <> §MtcPwd and
017900030508     c                             §MtcPwd <> *Blanks
018000030508     c                   Eval      *In41 = *On
018100030508     c                   Eval      *In28 = *On
018200030508     c                   Eval      V1Msg = Msg(3)
018300030508     c                   Clear                   V1Pwdold
018400080319     c                   LeaveSr
018500080319e   2c                   EndSl
018600040421
018700080319w   1c                   WHEN      wTabella = 'YNA'
018800040421      * Password attuale
018900080319sel 2c                   Select
019000080319w   2c                   When      V1pwdold = *Blanks and §YnaPwd <> *Blanks
019100040421     c                   Eval      *In41 = *On
019200040421     c                   Eval      *In28 = *On
019300040421     c                   Eval      V1Msg = Msg(2)
019400080319     c                   LeaveSr
019500080319w   2c                   When      V1pwdold <> §YnaPwd and
019600040421     c                             §YnaPwd <> *Blanks
019700040421     c                   Eval      *In41 = *On
019800040421     c                   Eval      *In28 = *On
019900040421     c                   Eval      V1Msg = Msg(3)
020000040421     c                   Clear                   V1Pwdold
020100080319     c                   LeaveSr
020200080319e   2c                   EndSl
020300070212
020400080319w   1c                   WHEN      wtabella = 'SDP'
020500070212      * Password attuale
020600080319sel 2c                   select
020700080319w   2c                   when      v1pwdold = *blanks and §sdppwd <> *blanks
020800070212     c                   eval      *in41 = *on
020900070212     c                   eval      *in28 = *on
021000070212     c                   eval      v1msg = msg(2)
021100080319     c                   leavesr
021200080319w   2c                   when      v1pwdold <> §sdppwd and
021300070212     c                             §sdppwd <> *blanks
021400070212     c                   eval      *in41 = *on
021500070212     c                   eval      *in28 = *on
021600070212     c                   eval      v1msg = msg(3)
021700070212     c                   clear                   v1pwdold
021800080319     c                   leavesr
021900080319e   2c                   endsl
022000080319
022100080319e   1c                   ENDSL
022200030415
022300030415      * Password nuova
022400080319sel 1c                   Select
022500080319w   1c                   When      V1pwdnew = *Blanks
022600030415     c                   Eval      *In42 = *On
022700030415     c                   Eval      *In28 = *On
022800030415     c                   Eval      V1Msg = Msg(4)
022900080319     c                   LeaveSr
023000080319w   1c                   When      V1pwdold = V1pwdnew
023100030415     c                   Eval      *In42 = *On
023200030415     c                   Eval      *In28 = *On
023300030415     c                   Eval      V1Msg = Msg(7)
023400030415     c                   Clear                   V1Pwdnew
023500080319     c                   LeaveSr
023600080319e   1c                   EndSl
023700030415
023800030415      * Password di verifica
023900080319sel 1c                   Select
024000080319w   1c                   When      V1pwdtst = *Blanks
024100030415     c                   Eval      *In43 = *On
024200030415     c                   Eval      *In28 = *On
024300030415     c                   Eval      V1Msg = Msg(5)
024400080319     c                   LeaveSr
024500080319w   1c                   When      V1pwdnew <> V1pwdtst
024600030415     c                   Eval      *In43 = *On
024700030415     c                   Eval      *In28 = *On
024800030415     c                   Eval      V1Msg = Msg(6)
024900030415     c                   Clear                   V1Pwdtst
025000080319     c                   LeaveSr
025100080319e   1c                   EndSl
025200030414
025300080319     c                   EndSr
025400030415
025500030415      *--------------------------------------------------------------------------------------------*
025600030508      * Aggiorno la tabella
025700030415      *--------------------------------------------------------------------------------------------*
025800030415     c     Sr_Aggiorna   BegSr
025900030415
026000030415     c                   Movel(p)  V1Fgs         TbeKe1
026100030415     c                   Clear                   TbeKe2
026200030415     c                   Clear                   TbeLin
026300030415     c                   Clear                   TbeSif
026400030415     c     kTbe          Chain     Tntbe01l
026500030508     c*
026600030508     c* A seconda della tabella che si sta gestendo...
026700080319sel 1c                   SELECT
026800080319      *
026900080319w   1c                   WHEN      wTabella = 'RSA'
027000030508     c                   Z-Add     *Date         §RsaDta
027100030415     c                   Eval      §RsaPwd = V1pwdnew
027200030415     c                   Movel(p)  dRsa          TbeUni
027300030725     c                   Eval      TbeFtt = 'S'
027400030725     c                   Eval      TbeFlt = V1Fgs
027500040421
027600080319w   1c                   WHEN      wTabella = 'MTC'
027700030508     c                   Z-Add     *Date         §MtcDta
027800030508     c                   Eval      §MtcPwd = V1pwdnew
027900030508     c                   Movel(p)  dMtc          TbeUni
028000030725     c                   If        V1Fgs = 046
028100030725     c                   Eval      TbeFtt = *blanks
028200030725     c                   Eval      TbeFlt = *zeros
028300030725     c                   Else
028400030725     c                   Eval      TbeFtt = 'S'
028500030725     c                   Eval      TbeFlt = V1Fgs
028600030725     c                   Endif
028700040421
028800080319w   1c                   WHEN      wTabella = 'YNA'
028900040421     c                   Z-Add     *Date         §YnaDta
029000040421     c                   Eval      §YnaPwd = V1pwdnew
029100040421     c                   Movel(p)  dYna          TbeUni
029200040421     c                   Clear                   TbeFtt                         Non trasmettere
029300040421     c                   Clear                   TbeFlt
029400070212
029500080319w   1c                   WHEN      wtabella = 'SDP'
029600070212     c                   z-add     *date         §sdpdta
029700070212     c                   eval      §sdppwd = v1pwdnew
029800070212     c                   movel(p)  dsdp          tbeuni
029900070212     c                   eval      tbeftt = 'S'
030000070212     c                   eval      tbeflt = v1fgs
030100080319      *
030200080319     c                   ENDSL
030300030508     c*
030400030415     c                   If        %Found(Tntbe01l)
030500030415     c                   Update    Tntbe000
030600030415     c                   Else
030700030415     c                   Movel(p)  V1Fgs         TbeKe1
030800030415     c                   Clear                   TbeKe2
030900030415     c                   Clear                   TbeLin
031000030415     c                   Clear                   TbeSif
031100030415     c                   Write     Tntbe000
031200030415     c                   EndIf
031300030415
031400030415     c                   EndSr
031500030415
031600011108      *--------------------------------------------------------------------------------------------*
031700030414      * *Inzsr - operazioni iniziali
031800011108      *--------------------------------------------------------------------------------------------*
031900030414     c     *Inzsr        BegSr
032000030414
032100030415     c     *Entry        Plist
032200030415     c                   Parm                    Kpjba
032300030508     c*
032400040421     c                   movel(p)  kpjbu         Parametri
032500030508     c                   movel(p)  wTabella      TbeCod
032600030414
032700030415     c     kTbe          Klist
032800030508     c                   Kfld                    TbeCod
032900030415     c                   Kfld                    TbeKe1
033000030415     c                   Kfld                    TbeKe2
033100030415     c                   Kfld                    TbeLin
033200030415     c                   Kfld                    TbeSif
033300030414
033400011108      * Reperisce dati utente
033500020715     c     *dtaara       define    §azute        azuteds
033600020715     c     *dtaara       define    §datiute      ddatiute
033700020715
033800020715     c                   in(E)     *dtaara
033900020715     c                   if        %Error or rsut = *blanks
034000020715     c                   clear                   tibs34ds
034100020715     c                   call      'TIBS34R'
034200020715     c                   parm                    Tibs34Ds
034300020715     c                   in        *dtaara
034400020715     c                   endif
034500030414
034600030414      * Imposto p.o. gestione
034700030414     c                   Movel     DutPou        V1Fgs
034800030415     c     V1Fgs         Chain     Azorg01l
034900030414     c                   If        %Found(Azorg01l) and OrgFva = *Blanks
035000030414     c                   Movel     OrgDes        V1dfgs
035100030414     c                   EndIf
035200030508
035300030508      * Imposto la descrizione a video della tabella che si sta gestendo
035400080129     c                   Z-Add     1             xi
035500080129     c     wTabella      Lookup    skTab(xi)                              55
035600030508     c                   If        %found
035700080129     c                   Eval      V1DES = skDesPgm(xi)
035800030508     c                   Else
035900030508     c                   Eval      V1DES = *all'*'
036000030508     c                   Endif
036100030508     c
036200030414
036300030414      * Abilito F18 solo se il p.o. utente è simfel
036400030414     c                   If        DutPou <> Simfel
036500030414     c                   Eval      *In06 = *On
036600030414     c                   EndIF
036700030414
036800030414      * Carico schiera P.O. gestiti
036900080129     c                   clear                   TRUL31ds
037000080129     c                   eval      I31cdi = DUTdis
037100080129     c                   eval      I31car = DUTare
037200080129     c                   eval      I31cpo = DUTpou
037300080129     c                   eval      I31abi = 'AP'
037400080129     c                   call      'TRUL31R'
037500080129     c                   parm                    Kpjba
037600080129     c                   parm                    TRUL31ds
037700030414
037800030508      * Carico Tabella
037900030508     c                   Clear                   dRsa
038000030508     c                   Clear                   dMtc
038100040421     c                   Clear                   dYna
038200070212     c                   clear                   dsdp
038300030414     c                   Clear                   Tibs02Ds
038400030414     c                   Eval      T02Mod = 'C'
038500030414     c                   Eval      T02Sif = Knsif
038600030508     c                   Eval      T02Cod = wTabella
038700030414     c                   Movel(p)  V1Fgs         T02Ke1
038800030414     c                   Call      'TIBS02R'
038900030414     c                   Parm                    Kpjba
039000030414     c                   Parm                    Tibs02Ds
039100030414     c                   If        T02Err = *Blanks
039200030508     c*
039300030508     c* A seconda della tabella che si sta gestendo...
039400040421     C                   Select
039500040421     c                   When      wTabella = 'RSA'
039600030508     c                   Movel     T02Uni        dRsa
039700040421     c                   When      wTabella = 'MTC'
039800030508     c                   Movel     T02Uni        dMtc
039900040421     c                   When      wTabella = 'YNA'
040000040421     c                   Movel     T02Uni        dYna
040100070212     c                   when      wtabella = 'SDP'
040200070212     c                   movel     t02uni        dsdp
040300040421     c                   EndSl
040400030508     c*
040500030414     c                   EndIf
040600030414
040700030414     c                   EndSr
040800030414
040900080129** - skTAB / skDESpgm                        *
041000080129RSA   Ristampa segnacolli in arrivo                                           01
041100080129MTC   Manutenzione tariffe  clienti                                           02
041200080129YNA    Gestione note di accredito                                             03
041300080214SDP  Sblocco distinte scaricate a PDA                                         04
041400080408                                                                              05
041500030508                                                                              06
041600030508                                                                              07
041700030508                                                                              08
041800030508                                                                              09
041900030508                                                                              10
042000000000** -MSG-
042100080129Fil. gestione errata o non gestibile                                          01
042200030415Inserire la password attuale                                                  02
042300030415Password attutale errata                                                      03
042400030415Inserire la nuova password                                                    04
042500030415Inserire la password per la verifica                                          05
042600030415La password di verifica non coincide con la nuova appena inserita             06
042700030415La nuova password deve essere diversa da quella attuale                       07
042800080408                                                                              08
042900030508                                                                              09
043000030508                                                                              10
