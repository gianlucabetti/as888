000100990212     H DECEDIT('0,') DATEDIT(*DMY.)
000200061127      *--------------------------------------------------------------*
000300061127      *  ELENCO clienti con diversa partiva iva o codice fiscale
000400061127      *--------------------------------------------------------------*
000500061127     Ftnta75d   CF   E             WORKSTN
000600981203     FAZORG01L  IF   E           K DISK
000700990215     FTABEL00F  IF   E           K DISK
000800990215     FAZORG02L  IF   E           K DISK    rename(azorg:azorg002)
000900981203
001000050609     d wabi            s                   like(UteAut)
001100990215     D §KUT            s                   like(codut)
001200990215     D §COD            s                   like(TBLCOD)
001300990215     D §KEY            s                   like(TBLkey)
001400990215     D COD             s                   like(TBLCOD)
001500990215     D KEY             s                   like(TBLkey)
001600061127     D WFIL            s                   like(orgfil)
001700990215     D WARE            s                   like(ORGCAR)
001800040129     d wZeros          s             11    inz(*all'0')
001900040129     d wErr            s              2  0 inz
002000981203
002100061127     D MSG             S             78    DIM(15) CTDATA PERRCD(1)
002200050610     D $pog            s              3    dim(250)
002300050610     D $DIG            s              1    dim(20)
002400050610     D $ARG            s              3    dim(50)
002500981203
002600061127     D tnta75ds      E DS
002700050609     D TIBS34DS      E DS
002800050609     D TRUL31DS      E DS
002900050609     D TRUL31DS2     E DS
003000990212     D TIBS02DS      E DS
003100050609     d Azuteds       e ds                  extname(Azute00f)
003200050609     d dDatiute      e ds
003300050609     d dLat          e ds
003400050609     d dute01        e ds
003500990315     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003600061127     d WPGM            s             10
003700981203
003800981203     D KPJBA         E DS
003900990215      *------------------------------------------------------------------------*
004000061127     c     INIZIO        TAG
004100990311     c                   clear                   v1ddis
004200990311     c                   clear                   v1dare
004300061127     c                   clear                   v1dpo
004400061127     c                   clear                   v1cdis
004500061127     c                   clear                   v1care
004600061127     c                   clear                   v1cpo
004700990319     c
004800990319      * IMPOSTO IL P.O. IN GESTIONE
004900990319     C                   IF        SIMFEL > 0
005000061127     c                   movel     dutpou        v1cpo
005100061127     c     dutpou        chain     azorg01l
005200061127     c                   if        %found(azorg01l)
005300061127     c                   movel     orgdes        v1dpo
005400061127     c                   endif
005500061127     c
005600990319    2C                   ENDIF
005700981203     c*
005800061127     c     FOR01         tag
005900061127     c                   exfmt     ta75d01
006000040129     c                   setoff                                       2890
006100040129     c                   movea     wZeros        *in(40)
006200990215     c* F3 = FINE
006300990215     c   KC              goto      fine
006400990215     c**
006500061127     c                   exsr      ctrd01
006600061127     c  nkf
006700061127     cor 90              goto      for01
006800990212     c*
006900981203      * F6 = Conferma Stampa
007000990212     c   KF              EXSR      conferma
007100990212     c
007200981203      *
007300990212     c     fine          tag
007400990212     c                   clear                   tibs02ds
007500990212     C                   MOVEL     'C'           t02tla
007600990212      *
007700990212     C                   CALL      'TIBS02R'
007800990212     C                   PARM                    KPJBA
007900990212     C                   PARM                    TIBS02DS
008000981203     c                   eval      *inlr = *on
008100990215      *--------------------------------------------------------------------*
008200061127      * CONTROLLI VIDEO 1
008300990215      *--------------------------------------------------------------------*
008400061127     C     Ctrd01        BEGSR
008500061127     c*
008600040129      *
008700061127     c* Cod DISTRETTO  se selezinato
008800050610     c                   exsr      CtrDISTR
008900061127     c   90              goto      EndCT1
009000990215     c
009100061127     c* Cod Area       se elezionato
009200050610     c                   exsr      CtrAREA
009300061127     c   90              goto      EndCT1
009400061127     c
009500061127     c* P.Operativo    se selezionato
009600061127     c                   clear                   v1dpo
009700061127     c                   if        v1cpo<>*blanks and v1cpo<>'000'
009800061127     c                   exsr      CTRPO
009900061127     c   90              seton                                        57
010000061127     c   90              goto      endct1
010100061127     c
010200061127     c                   eval      v1dpo =orgdes
010300061127     c                   endif
010400990312     c
010500061127     c     endct1        ENDSR
010600050610      *--------------------------------------------------------------------*
010700050610     c* Cod Distretto se elezionato
010800050610      *--------------------------------------------------------------------*
010900050610     c     CtrDISTR      BEGSR
011000050610     c                   clear                   v1ddis
011100050610     c     '?'           scan      v1cdis                                 90
011200050610     c                   if        *in90
011300050610     c                   movel     codut         §kut
011400050610     c                   movel     '17'          §cod
011500050610     c                   clear                   §key
011600050610     c                   call      'X§TABER'
011700050610     c                   parm                    §kut
011800050610     c                   parm                    §cod
011900050610     c                   parm                    §key
012000050610     c                   parm                    §des             25
012100050610     c                   movel     §key          v1cdis
012200050610     c                   movel     §des          v1ddis
012300050610     c                   seton                                        40
012400050610     c                   goto      endctrDISTR
012500050610     c                   endif
012600050610     c
012700050614    1c     v1cdis        ifne      *blanks
012800061127    2c                   if        v1cdis<>*blanks
012900050613     c
013000050610     c                   movel     '17'          cod
013100050610     c                   movel(p)  v1cdis        key
013200050610     c     ktab          chain     tabel                              30
013300050614    2c                   if        *in30 OR tblflg<>' '
013400061127     c                   movel     msg(2)        v1cmsg
013500050610     c                   seton                                        289040
013600050610     c                   goto      endctrDISTR
013700050614    2c                   endif
013800050610     c                   movel     tbluni        v1ddis
013900050610     C* Non è possibile richiedere il distretto '9' - SEDE
014000050614    2C                   if        v1cdis='9'
014100061127     c                   movel     msg(4)        v1cmsg
014200050610     c                   seton                                        289040
014300050610     c                   goto      endctrDISTR
014400050614    2c                   endif
014500050614     c
014600050614     c
014700050610     c* controllo se esiste in organigramma
014800050610     c     v1cdis        chain     azorg02l                           30
014900050610     c                   dow       not *in30 and orgfva<>' '
015000050610     c     v1cdis        reade     azorg02l                               30
015100050610     c                   enddo
015200050610     c                   if        *in30
015300061127     c                   movel     msg(05)       v1cmsg
015400050610     c                   seton                                        289040
015500050610     c                   goto      endctrDISTR
015600050610     c                   endif
015700050613     c                   endif
015800050610     c* Controllo se abilitato al distretto, solo se non impostata area
015900050610     c                   if        v1care<=*zeros
016000050610     c     v1cdis        lookup    $dig                                   30
016100050613     c
016200050613     c                   if        not *in30
016300061127     c                   movel     msg(10)       v1cmsg
016400050610     c                   seton                                        289040
016500050610     c                   goto      endctrDISTR
016600050610     c                   endif
016700050610     c                   endif
016800061127     c
016900050610     c                   endif
017000050610     c     EndctrDISTR   ENDSR
017100050610      *--------------------------------------------------------------------*
017200050610     c* controllo cod AREA
017300050610      *--------------------------------------------------------------------*
017400050610     c     CtrAREA       BEGSR
017500050610     c                   clear                   v1dare
017600050610     c     '?'           scan      v1care                                 90
017700050610     c                   if        *in90
017800050610     c                   movel     codut         §kut
017900050610     c                   movel     '05'          §cod
018000050610     c                   clear                   §key
018100050610     c                   call      'X§TABER'
018200050610     c                   parm                    §kut
018300050610     c                   parm                    §cod
018400050610     c                   parm                    §key
018500050610     c                   parm                    §des
018600050610     c                   movel     §key          v1care
018700050610     c                   movel     §des          v1dare
018800050610     c                   seton                                        41
018900050610     c                   goto      endctrAREA
019000050610     c                   endif
019100061127     c
019200050610     c     v1care        ifeq      *zeros
019300050610     c                   clear                   v1care
019400050610     c                   endif
019500061127     c
019600050610     c     v1care        ifne      *blanks
019700050610     c     v1care        andne     *zeros
019800050610     c                   movel     '05'          cod
019900050610     c                   movel(p)  v1care        key
020000050610     c     ktab          chain     tabel                              30
020100050610     c                   if        *in30 OR tblflg<>' '
020200061127     c                   movel     msg(03)       v1cmsg
020300050610     c                   seton                                        289041
020400050610     c                   goto      endctrAREA
020500050610     c                   endif
020600050610     c                   movel     tbluni        v1dare
020700050610     c
020800050610     c     v1care        ifle      *zeros
020900050610     c                   clear                   ware
021000050610     c                   else
021100050610     c                   movel     v1care        ware
021200050610     c                   endif
021300050610     c
021400050610     c                   if        v1cdis<>' '
021500050610     c     KORG          chain     azorg02l                           30
021600050610     c                   dow       not *in30 and orgfva<>' '
021700050610     c     KORG          reade     azorg02l                               30
021800050610     c                   enddo
021900050610     c                   if        *in30
022000061127     c                   movel     msg(06)       v1cmsg
022100050610     c                   seton                                        289041
022200050610     c                   goto      endctrAREA
022300050610     c                   endif
022400050610     c
022500050610     c                   else
022600050610     c* Controllo se utente abilitato
022700050610     c     v1care        lookup    $arg                                   30
022800050610     c                   if        not *in30
022900061127     c                   movel     msg(11)       v1cmsg
023000050610     c                   seton                                        289041
023100050610     c                   goto      endctrAREA
023200050610     c                   endif
023300050610     c
023400050610     c
023500050610     c     *loval        setll     azorg02l
023600050610     c                   read      azorg02l                               30
023700050610     c                   dow       not *in30  and v1cdis=*blanks
023800050610     c                   if        orgcar<>ware
023900050610     c     korg1         setgt     azorg02l
024000050610     c                   else
024100050610     c     orgfva        ifeq      *blanks
024200050610     c                   movel     orgfl3        v1cdis
024300050610     c                   endif
024400050610     c                   endif
024500050610     c                   read      azorg02l                               30
024600050610     c                   enddo
024700050610     c
024800050610     C* Non è possibile richiedere il distretto '9' - SEDE
024900050610     C                   if        v1cdis='9'
025000050610     c                   clear                   v1cdis
025100061127     c                   movel     msg(07)       v1cmsg
025200050610     c                   seton                                        289041
025300050610     c                   goto      endctrAREA
025400050610     c                   endif
025500050610     c* non trovata area sull'organigramma
025600050610     C                   if        v1cdis=' '
025700050610     c                   clear                   v1cdis
025800061127     c                   movel     msg(08)       v1cmsg
025900050610     c                   seton                                        289041
026000050610     c                   goto      endctrAREA
026100050610     c                   endif
026200050610     c
026300050610     c
026400050610     c* Decodifico codice distretto
026500050610     c                   movel     '17'          cod
026600050610     c                   movel(p)  v1cdis        key
026700050610     c     ktab          chain     tabel                              30
026800050610     c  N30              movel     tbluni        v1ddis
026900050610     c                   seton                                        90
027000050610     c
027100050610     c                   endif
027200050610     c                   endif
027300050610     c     endctrAREA    ENDSR
027400990212      *------------------------------------------------------------------------*
027500061127      * CONTROLLO P.O.
027600990212      *------------------------------------------------------------------------*
027700990212     C     CTRPO         BEGSR
027800061127     c* eseguo ricerca p.o. se richiesto
027900061127     C     '?'           scan      v1cpo                                  21
028000061127     c                   if        *in21
028100061127     C                   CALL      'TNSD24R'
028200061127     C                   PARM                    §fil              3
028300061127     C                   PARM                    §Tip              1
028400061127     C                   PARM                    §§Des            25
028500061127     C                   PARM                    §§Pra             3
028600061127     C                   movel     §fil          V1cpo
028700061127     c                   seton                                        4290
028800061127     C                   GOTO      endctrPO
028900061127     C                   ENDIF
029000061127     c*
029100061127      * Controllo che il codice filiale immesso sia numerico
029200061127     C                   TESTN                   V1cpo                36
029300061127     c                   move      v1cpo         w001a             1
029400061127     c*
029500061127     c                   if        not *in36 or w001a<*zeros
029600061127     c                   eval      v1cmsg=msg(1)
029700061127     c                   seton                                        289042
029800061127     C                   GOTO      endctrPO
029900061127     C                   END
030000061127     C*
030100061127     C                   movel     v1cpo         wfil
030200061127     c
030300990212     c     wfil          chain     azorg01l                           30
030400990212     c     *in30         ifeq      *on
030500990212     c     orgfva        orne      ' '
030600990212     c     orgfag        orne      'A'
030700990212     c     orgfag        andne     'F'
030800061127     c                   eval      v1cmsg=msg(1)
030900990212     c                   seton                                        2890
031000990212     c                   endif
031100061127     c* controllo se utente abilitato
031200061127     c     v1cpo         lookup    $pog                                   30
031300061127     c                   if        not *in30
031400061127     c                   movel     msg(12)       v1cmsg
031500061127     c                   seton                                        289042
031600061127     c                   goto      endctrPO
031700061127     c                   endif
031800061130     c* Se impostato anche area o distretto, verifico se corretti
031900061130     c                   if        v1care<>*blanks  and orgcar<>ware
032000061130     c                   movel     msg(13)       v1cmsg
032100061130     c                   seton                                        289042
032200061130     c                   goto      endctrPO
032300061130     c                   endif
032400061130     c                   if        v1cdis<>*blanks  and orgfl3<>v1cdis
032500061130     c                   movel     msg(14)       v1cmsg
032600061130     c                   seton                                        289042
032700061130     c                   goto      endctrPO
032800061130     c                   endif
032900061130     c
033000061127     c     EndctrPO      ENDSR
033100981203      *------------------------------------------------------------------------*
033200981203      * ROUTINE INIZIALE
033300981203      *------------------------------------------------------------------------*
033400981203     C     *INZSR        BEGSR
033500981203      *
033600981203     C     *ENTRY        PLIST
033700981203     C                   PARM                    KPJBA
033800981203      *
033900050609     c     *dtaara       define    §azute        azuteds
034000050609     c     *dtaara       define    §datiute      ddatiute
034100050609     c                   in(E)     *dtaara
034200050609    1c                   If        %error  or RSUT = *blanks
034300050609     c                   Clear                   Tibs34ds
034400050609     c                   Call      'TIBS34R'
034500050609     c                   Parm                    Tibs34ds
034600050609     c                   In        *dtaara
034700050609    1c                   EndIf
034800050609
034900050609     c                   Clear                   wabi
035000050609     c                   Clear                   v1cmsg
035100050609     c                   Clear                   dLat
035200050609
035300050609      * Verifica errori e autorità profilo
035400050609s   1c                   Select
035500050609      * se ho errori nei dati utente esco dal pgm
035600050609w   1c                   When      DutErr = 'E'
035700050609     c                   GoTo      Fine
035800050609      * se non c'è l'abilitazione
035900050609      * --> se 1° livello, abilitazioni al terminal
036000050609      *     se 2° livello, abilitazioni al punto operativo
036100050609      *     se sede è impossibile ma se capita mando a fine il pgm
036200050609w   1c                   When      UteAut = *Blanks
036300050609if  2c                   If        DutLpo = '1'
036400050609     c                   Eval      wabi   = 'TP'
036500050609e   2c                   EndIf
036600050609if  2c                   If        DutLpo = '2'
036700050609     c                   Eval      wabi   = 'PO'
036800050609e   2c                   EndIf
036900050609if  2c                   If        DutLpo = 'S'
037000050609     c                   GoTo      Fine
037100050609e   2c                   EndIf
037200050609      * carica le abilitazioni del profilo
037300050609x   1c                   Other
037400050609     c                   Eval      wabi = UteAut
037500050609e   1c                   EndSl
037600050609
037700050609      * controllo se ok l'abilitazione dell'utente
037800050609     c                   Clear                   Tibs02ds
037900050609     c                   Eval      T02Mod = 'C'
038000050609     c                   Eval      T02Sif = knsif
038100050609     c                   Eval      T02Cod = 'LAT'
038200050609     c                   Movel(p)  wabi          T02Ke1
038300050609     c                   Call      'TIBS02R'
038400050609     c                   Parm                    kpjba
038500050609     c                   Parm                    Tibs02ds
038600050609if  1c                   If        T02Err = *Blanks
038700050609     c                   Eval      dLat = T02Uni
038800050609e   1c                   EndIf
038900050609      * errore o non abilitato
039000050609if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
039100050609     c                   Seton                                        28
039200061127     C                   movel     msg(09)       v1cmsg
039300050609   x1c                   Else
039400050609      * Reperimento dei P.O. gestibili dall'utente
039500050609     c                   clear                   TRUL31DS
039600050609     c                   clear                   TRUL31DS2
039700050609     c                   eval      I31abi = wabi
039800050609     c                   eval      I31cdi = DUTdis
039900050609     c                   eval      I31car = DUTare
040000050609     c                   eval      I31cpo = DUTpou
040100050609     c                   call      'TRUL31R'
040200050609     c                   parm                    KPJBA
040300050609     c                   parm                    TRUL31DS
040400050609     c                   parm                    TRUL31DS2
040500050609if  2c                   if        O31pog > *zeros
040600050610     c                   movea     O31pog        $pog
040700050610     c                   movea     O31arg        $arg
040800050610     c                   movea     O31dig        $dig
040900050609x   2c                   else
041000050609     c                   Seton                                        28
041100061127     c                   movel     MSG(09)       v1cmsg
041200050609e   2c                   endif
041300050609    1c                   endif
041400050609     c
041500050609     C                   Z-ADD     1             CODUT
041600981203      *
041700990215     c     ktab          klist
041800990215     c                   kfld                    codut
041900990215     c                   kfld                    cod
042000990215     c                   kfld                    key
042100990215     c     kORG          klist
042200990215     c                   kfld                    v1cdis
042300990215     c                   kfld                    ware
042400990215     c     KORG1         klist
042500990215     c                   kfld                    orgfl3
042600990215     c                   kfld                    orgcar
042700981203     C                   ENDSR
042800990215      *------------------------------------------------------------------------*
042900990215      * SOTTOMENTTO ELABORAZIONE
043000990215      *------------------------------------------------------------------------*
043100990212     c     CONFERMA      BEGSR
043200061127     c                   clear                   tnta75ds
043300061127     c
043400061127     c                   if        v1cpo>*zeros
043500061127     c                   movel     v1cpo         ita75cpo
043600061127     c                   endif
043700061127     c                   eval      ita75dpo = v1dpo
043800990315     c
043900990219     c* SE ditretto e area vuoti e ho impostato fil per ord 1 li riempo
044000061127     c                   if        v1cpo >*zeros and
044100061127     c                             v1cdis=*blanks and v1care=*blanks
044200061127     c     ita75cpo      chain     azorg01l                           30
044300061127     c                   movel     orgcar        v1care
044400061127     c
044500061127     c                   exsr      CtrAREA
044600061130     c
044700061130     c* Se utente non abilitato all'rea del p.o., non decodifico
044800061130     c*  e non imposto i campi della DS per area e distretto
044900061130     c                   if        *in28
045000061130     c                   clear                   v1care
045100061130     c                   clear                   v1dare
045200990219     c                   endif
045300061130     c                   endif
045400061130     c
045500061127     c                   eval      ita75cdis= v1cdis
045600061127     c                   eval      ita75ddis= v1ddis
045700061127     c
045800061130     c                   if        v1care>*zeros
045900061130     c                   movel     v1care        ita75care
046000061130     c                   endif
046100061127     c                   eval      ita75dare= v1dare
046200990507      *
046300061127     c                   movel     tnta75ds      kpjbu
046400990212     c
046500061127     c                   eval      kcoaz='TA76'
046600061130     c                   eval      Wpgm = 'TNTA76R   '
046700061130     c**                 eval      Wpgm = 'BCH10     '
046800040209     c                   call      Wpgm
046900990212     c                   parm                    kpjba
047000990212     c                   endsr
047100981203      *-------------------------------------------------------------------------
047200981203** MSG  Lungh. 78                                                            *
047300061127Punto operativo inesistente                                                    1
047400061127Cod.Distretto errato                                                           2
047500061127Cod.Area errato                                                                3
047600061127Cod.Distretto 9 - SEDE  non selezinabile                                       4
047700061127Cod.Distretto non presente nell'oranigramma                                    5
047800061127Cod.Area non congruente col distretto selezionato                              6
047900061127cod.Area di distretto 9 - SEDE  non selezionabile                              7
048000061127Cod.Area non presente nell'organigramma                                        8
048100061127Utente non abilitato alla Stampa                                               9
048200061127Utente non abilitato a richiedere il distretto indicato                       10
048300061127Utente non abilitato a richiedere l'Area indicata                             11
048400061127Utente non abilitato a richiedere il P.O.                                     12
048500061130P.Operativo non congruente con codice Area selezionato                        13
048600061130P.Operativo non congruente con codice distretto selezionato                   14
