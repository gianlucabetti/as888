000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200090618     H* TNTAA1R  *---------------------------------------------------*
000300090618     H*        CONTROLLO abilitazioni utente                         *
000400000000     H*--------------------------------------------------------------*
000500090703     Fazute01l  IF   E           K DISK
000600090703     Fcnclp00F  IF   E           K DISK
000700090624     Ftncpo01l  IF   E           K DISK
000800091023     Ftivis05l  IF   E           K DISK    usropn
000900090713     Ftiatc01l  IF   E           K DISK    usropn
001000981118     FTABEL00F  IF   E           K DISK
001100090624     d
001200090624     d POG             S              3    DIM(250)
001300940802     D*
001400090624     D kpjba         E DS
001500090624     D tntaa1ds      E DS
001600981118     D DS01          E DS
001700090618     d Azuteds       e ds                  extname(Azute00f)
001800090618     d dDatiute      e ds
001900090618     d dLat          e ds
002000090618     d dute01        e ds
002100090624     d trul31ds      e ds
002200090624     d tibs02ds      e ds
002300090624     d Tibs34ds      e ds
002400090618
002500090618     d savpru          s                   like(itaa1pru)
002600090619     d wabi            s                   like(UteAut)
002700090619     d wcaut           s                   like(itaa1caut)
002800090619     d savcaut         s                   like(itaa1caut)
002900090619     d codut           s              1  0
003000090624     d alfafil         s              3
003100090624     d noabi           s              1
003200090629     d w005a           s              5
003300090619     d kcod            s                   like(tblcod)
003400090619     d kkey            s                   like(tblkey)
003500090619     d wcmm            s                   like(itaa1cmm)
003600090618
003700090618     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
003800090618     D                                     TUVWYXZ')
003900090618     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
004000090618     D                                     tuvwyxz')
004100940802     C*****************************************************************
004200940802     C* RIEPILOGO INDICATORI
004300940802     C*****************************************************************
004400940802     C*****************************************************************
004500000000     C     *ENTRY        PLIST
004600090618     C                   PARM                    kpjba
004700090618     C                   MOVEL     KPJBU         tntaa1ds
004800981118     C*
004900090619    0c                   if        itaa1tla<>'C'
005000090619     C                   CLEAR                   Otaa1err
005100090626     C                   CLEAR                   Otaa1fabi
005200090626     C                   CLEAR                   Otaa1cabi
005300090629     C                   CLEAR                   Otaa1dupcl
005400090629     C                   CLEAR                   Otaa1car
005500090629     C                   CLEAR                   Otaa1cmm
005600090629     C                   CLEAR                   Otaa1argf
005700090619     c
005800090619     c                   exsr      Rep_utente
005900000000     C*---------------------------------------------------------------*
006000090619     c* Se l'utente prevede della abilitazioni  controllo
006100090619     c* nello specifico
006200090629     c
006300090626    1c                   if        otaa1fabi=' '
006400090629     c
006500090624     c* Immessa FILIALE
006600090629    2c                   if        itaa1FIL>*zeros and otaa1fabi<>'S'
006700090619     C                   MOVEL     itaa1fil      alfafil
006800090619     c     alfafil       lookup    pog                                    90
006900090619    3c                   if        not *in90
007000090626     c                   eval      otaa1fabi='N'
007100090624     C                   ELSE
007200090626     c                   eval      otaa1fabi='S'
007300090619    3c                   endif
007400090619    2c                   endif
007500090619     c
007600090624     c* Immesso COMMERCIALE
007700090626    2c                   if        itaa1cmm>*zeros  and otaa1fabi<>'S'
007800090619     c                   eval      wcmm=itaa1cmm
007900090626     c                   eval      otaa1cmm=itaa1cmm
008000090619     c                   exsr      CtrCOMM
008100090624    2c                   endif
008200090624     C
008300090624     c* Immesso CODICE CLIENTE
008400090626    2c                   if        itaa1KSC>*zeros  and otaa1fabi<>'S'
008500090629     c
008600090629     c* Se si tratta di tariffa di cartello, controllo il relativo flag
008700090629     c                   movel     itaa1KSC      w005a
008800090629     c
008900090629    3c                   If        w005a = '88888'
009000090629    4c                   if        §utecar <> 'S'
009100090629     c                   eval      otaa1fabi='N'
009200090629     c                   else
009300090629     c                   eval      otaa1fabi='S'
009400090629    4c                   endif
009500090629   x3c                   else
009600090629     c
009700090624     c     kclp          chain     cnclp00f
009800090629    4c                   if        not %found(cnclp00f)
009900090626     c                   eval      otaa1err='S'
010000090626     c                   eval      otaa1fabi='N'
010100090624     c                   else
010200090624     c                   eval      wcmm=clpage
010300090624     c                   eval      otaa1cmm=clpage
010400090624     c                   exsr      CtrCOMM
010500090629    4c                   endif
010600090629    3c                   endif
010700090624    2c                   endif
010800090624     c* Immesso POTENZIALE
010900090626    2c                   if        itaa1cpo>*zeros  and otaa1fabi<>'S'
011000090624     c     itaa1cpo      chain     tncpo01l
011100090624    3c                   if        not %found(tncpo01l)
011200090626     c                   eval      otaa1err='S'
011300090626     c                   eval      otaa1fabi='N'
011400090626   x3c                   else
011500090626     c*
011600090626     C                   MOVEL     cpoflt        alfafil
011700090626     c     alfafil       lookup    pog                                    90
011800090626    4c                   if        *in90
011900090626     c                   eval      otaa1fabi='S'
012000090626    4c                   endif
012100090626    4c                   if        cpocmm>0
012200090624     c                   eval      wcmm=cpocmm
012300090624     c                   eval      otaa1cmm=cpocmm
012400090624     c                   exsr      CtrCOMM
012500090626   x4c                   else
012600090626    5c                   if        not *in90
012700090626     c                   eval      otaa1fabi='N'
012800090626    5c                   endif
012900090626    4c                   endif
013000090626     c
013100090626    3c                   endif
013200090624    2c                   endif
013300090702     c
013400090713     c* Immesso NUMERO TRATTATIVA COMMERCIALE
013500090702    2c                   if        itaa1NRV>*zeros  and otaa1fabi<>'S'
013600090706     c* se sono in sede --> errore
013700090713     c* sE sono in filiale --> apro il file e controllo
013800110915    3c*                  if        simfel=0
013900110915     c*                  eval      otaa1err='S'
014000110915     c*                  eval      otaa1fabi='N'
014100110915   x3c*                  else
014200090706     c
014300091023    4c                   if        not %open(tivis05l)
014400091023     c                   open      tivis05l
014500090706    4c                   endif
014600090706     c
014700090713     C* Per il momento controllo solo il commerciale esterno
014800090713     c*  fino a nuove disposizioni
014900091023     c     itaa1NRV      chain     TIVIS05L
015000091023    4c                   if        not %found(TIVIS05L)
015100090702     c                   eval      otaa1err='S'
015200090702     c                   eval      otaa1fabi='N'
015300090706   x4c                   else
015400090702     c                   eval      wcmm=viscmm
015500090702     c                   eval      otaa1cmm=viscmm
015600090702     c                   exsr      CtrCOMM
015700090706    4c                   endif
015800110915    3c*                  endif
015900090702    2c                   endif
016000090713     c
016100090713     c* Immesso NUMERO ATTIVITA' COMMERCIALE
016200090713    2c                   if        itaa1ATN>*zeros  and otaa1fabi<>'S'
016300090713     c* se sono in sede --> errore
016400090713     c* sE sono in filiale --> apro il file e controllo
016500090713    3c                   if        simfel=0
016600090713     c                   eval      otaa1err='S'
016700090713     c                   eval      otaa1fabi='N'
016800090713   x3c                   else
016900090713     c
017000090713    4c                   if        not %open(TIATC01l)
017100090713     c                   open      tiatc01l
017200090713    4c                   endif
017300090713
017400090713     c     katc          chain     TIatc01L
017500090713    4c                   if        not %found(TIATC01L)
017600090713     c                   eval      otaa1err='S'
017700090713     c                   eval      otaa1fabi='N'
017800090713   x4c                   else
017900090713     c*  se c'e' commerciale esecuzione attività --> controlo questo
018000090713     c*   altrimenti  commerciale attività
018100090713     c                   if        atccco>0
018200090713     c                   eval      wcmm=atccco
018300090713     c                   eval      otaa1cmm=atccco
018400090713     c                   else
018500090713     c                   eval      wcmm=atccmm
018600090713     c                   eval      otaa1cmm=atccmm
018700090713     c                   endif
018800090713     c
018900090713     c                   exsr      CtrCOMM
019000090713    4c                   endif
019100090713    3c                   endif
019200090713    2c                   endif
019300090713     c
019400090702
019500090702
019600090624    1c                   endif
019700090626     c                   movel     tntaa1ds      kpjbu
019800090624    0c                   endif
019900090619     c
020000930210     C*
020100130409    0C     Itaa1tla      IFNE      ' '
020200000000     C                   SETON                                        LR
020300981118     C                   ELSE
020400981118     C                   SETON                                        RT
020500090624    0C                   ENDIF
020600090618
020700090618     C*****************************************************************
020800090618     c     Rep_utente    BEGSR
020900090618     c* Profilo
021000090619    1c                   if        itaa1pru=*blanks
021100090618     c                   movel     knmus         itaa1pru
021200090619    1c                   endif
021300090706     c
021400090706     c* trasforma il contenuto del campo in lettere maiuscole
021500090706     c                   clear                   wcaut
021600090706    2c                   if        Itaa1caut<> *blanks
021700090706     c                   exsr      Maiusc
021800090706     c                   endif
021900090618     c
022000090618     c     *dtaara       define    §azute        azuteds
022100090618     c     *dtaara       define    §datiute      ddatiute
022200090618     c                   in(E)     *dtaara
022300090618     c
022400090703     c* Carico dati se non ancora fatto
022500090703    1c                   If        %error
022600090618     c                   Clear                   Tibs34ds
022700090624     c                   eval      i34ute=itaa1pru
022800090618     c                   Call      'TIBS34R'
022900090618     c                   Parm                    Tibs34ds
023000090618     c                   In        *dtaara
023100090619    1c                   EndIf
023200090618
023300090703     c* Carico abilitazioni se profilo diverso da profilo utente
023400090703     c*  o è la prima volta
023500100519      * oppure ho chiesto forzata l'abilitazione
023600090619    0c                   If        %error  or itaa1pru<>savpru
023700090706     c                             or wcaut<>savcaut
023800100519     c                             or (ITAA1abi <> *blanks and
023900100519     c                                 wabi <> ITAA1abi)
024000090618     c                   Clear                   wabi
024100090618     c                   Clear                   dLat
024200090619     c                   Clear                   NOabi
024300090703     c
024400090703     c                   if        itaa1pru<>knmus
024500090703     c     itaa1pru      chain     azute01l
024600090703     c                   if        not %found(Azute01l)
024700090703     c                   eval      noabi   ='N'
024800090703     c                   endif
024900090703     c                   endif
025000090703     c*
025100090618
025200090618      * Verifica errori e autorità profilo
025300090619s    c                   Select
025400090618      * se ho errori nei dati utente esco dal pgm
025500090619w   1c                   When      DutErr = 'E'
025600090624     c                   eval      noabi   ='N'
025700090624     c
025800090618      * se non c'è l'abilitazione
025900090618      * --> se 1° livello, abilitazioni al terminal
026000090618      *     se 2° livello, abilitazioni al punto operativo
026100090618w   1c                   When      UteAut = *Blanks
026200090618if  2c                   If        DutLpo = '1'
026300090618     c                   Eval      wabi   = 'TP'
026400090618e   2c                   EndIf
026500090618if  2c                   If        DutLpo = '2'
026600090618     c                   Eval      wabi   = 'PO'
026700090618e   2c                   EndIf
026800090618if  2c                   If        DutLpo = 'S'
026900090618     c                   Eval      wabi   = 'AZ'
027000090618e   2c                   EndIf
027100090618      * carica le abilitazioni del profilo
027200090618      * per la nuova offerta
027300090618x   1c                   Other
027400090618     c                   Movel     UteFaf        Dute01
027500090618     c
027600090624    2c                   if        Itaa1caut= *blanks
027700090619     c                   Eval      wabi = UteAut
027800090619   z2c                   else
027900090618     c*
028000090706    3c                   if        wcaut<>'UTEAUT'
028100090618     c
028200090619     c                   select
028300090619    4c                   when      wcaut='§UTEGTC'and
028400090619     c                             §utegtc<>*blanks
028500090618     c                   Eval      wabi = §UteGtc
028600090619     c
028700120330    4c                   when      wcaut='§UTEMBS'and
028800120330     c                             §utembs<>*blanks
028900120330     c                   Eval      wabi = §Utembs
029000120330     c
029100090619    4c                   when      wcaut='§UTECTC'and
029200090619     c                             §utectc<>*blanks
029300090619     c                   Eval      wabi = §Utectc
029400090619     c
029500090619    4c                   when      wcaut='§UTEPOT'and
029600090619     c                             §utepot<>*blanks
029700090619     c                   Eval      wabi = §Utepot
029800090619     c
029900090619    4c                   when      wcaut='§UTECLI'and
030000090619     c                             §utecli<>*blanks
030100090619     c                   Eval      wabi = §Utecli
030200120330
030300101011    4c                   when      wcaut='§UTEIST'and
030400101011     c                             §uteist<>*blanks
030500101011     c                   Eval      wabi = §Uteist
030600090707     c                   other
030700090707     c                   Eval      wabi = UteAut
030800090619e   4c                   EndSl
030900090619    3c                   endif
031000090619    2c                   endif
031100100519      * se richiesta l'abilitazione forzata la imposto
031200100519     c                   IF        ITAA1abi <> *blanks
031300100519     c                   Eval      wabi = ITAA1abi
031400100519     c                   ENDIF
031500090619     c
031600090619e   1c                   EndSl
031700090618
031800090618      * controllo se ok l'abilitazione dell'utente
031900090618     c                   Clear                   Tibs02ds
032000090618     c                   Eval      T02Mod = 'C'
032100090618     c                   Eval      T02Sif = knsif
032200090618     c                   Eval      T02Cod = 'LAT'
032300090618     c                   Movel(p)  wabi          T02Ke1
032400090618     c                   Call      'TIBS02R'
032500090618     c                   Parm                    kpjba
032600090618     c                   Parm                    Tibs02ds
032700090618if  1c                   If        T02Err = *Blanks
032800090618     c                   Eval      dLat = T02Uni
032900090618e   1c                   EndIf
033000090618      * errore o non abilitato
033100090618if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
033200090619     c                   eval      noabi   ='N'
033300090619e   1c                   Else
033400090618
033500090618      * Reperimento dei P.O. gestibili dall'utente
033600090618      * per la nuova offerta
033700090618     c                   clear                   TRUL31DS
033800090618     c                   eval      I31abi = wabi
033900090618     c                   eval      I31cdi = DUTdis
034000090618     c                   eval      I31car = DUTare
034100090618     c                   eval      I31cpo = DUTpou
034200090618     c                   call      'TRUL31R'
034300090618     c                   parm                    KPJBA
034400090618     c                   parm                    TRUL31DS
034500090618if  2c                   if        O31pog > *zeros
034600090624     c                   movea     O31pog        pog
034700090618x   2c                   else
034800090619     c                   eval      NOabi='N'
034900090618e   2c                   endif
035000090619e   1c                   endif
035100090706     c                   eval      savcaut=wcaut
035200090619     c                   eval      savpru =itaa1pru
035300090619e   0c                   endif
035400090629     c
035500090619     c* nessuna abilitazione
035600090626     c                   eval      otaa1fabi=noabi
035700090629     c* Livelli autorizzazione
035800090626     c                   eval      otaa1cabi=wabi
035900090629     c                   eval      otaa1car =§utecar
036000090629     c                   eval      otaa1dupcl=§utedupcli
036100090713     c                   eval      otaa1FLNA =§uteflna
036200090618
036300090618     c                   ENDSR
036400090619     c*-------------------------------------------------------------------------
036500090619     c     Maiusc        BEGSR
036600090619     C                   MOVEL     CMAIUS        MA               26
036700090619     C                   MOVEL     CMINUS        MI               26
036800090619     C     MI:MA         XLATE     itaa1caut     wcaut
036900090619     c                   ENDSR
037000090619     C*---------------------------------------------------------------*
037100090619     c     CtrCOMM       BEGSR
037200090619     C                   MOVEL     '01'          kCOD
037300090619     C                   MOVEL(P)  wcmm          kKEY
037400090619     C     KTAB          CHAIN     TABEL                              31
037500090619    1C     *IN31         IFEQ      *OFF
037600090619     C                   MOVEL     TBLUNI        DS01
037700090619     C                   ELSE
037800090619     C                   CLEAR                   DS01
037900090626     c* Errore --> codice non trovato
038000090626     c                   eval      otaa1err='S'
038100090626     c                   eval      otaa1fabi='N'
038200090619    1C                   ENDIF
038300090619     C**
038400090619     C                   MOVEL     §01RGF        otaa1argf
038500090619     C**
038600090626     C** Se ancora non abilitato e non trovato errore
038700090626    1c                   if        otaa1fabi<>'S'  and otaa1err=' '
038800090619     C                   MOVEL     wcmm          alfafil
038900090626     c                   setoff                                       90
039000090619     c     alfafil       lookup    pog                                    90
039100090619     c
039200090619     C* CERCO COMMERCIALE UNIFICANTE DEL CODICE PASSATO
039300090626    2c                   if        not *in90
039400090619     C                   MOVEL     §01RGF        alfafil
039500090619     c     alfafil       lookup    pog                                    90
039600090626    2c                   endif
039700090619     c
039800090626    2c                   if        not *in90
039900090626     c                   eval      otaa1fabi='N'
040000090624     c                   else
040100090626     c                   eval      otaa1fabi='S'
040200090626    2c                   endif
040300090626    1c                   endif
040400090624     c
040500090619     c                   ENDSR
040600090619     C*---------------------------------------------------------------*
040700090619     c     *inzsr        BEGSR
040800090619     c     ktab          klist
040900090619     c                   kfld                    codut
041000090619     c                   kfld                    kcod
041100090619     c                   kfld                    kkey
041200090624     c     kclp          klist
041300090624     c                   kfld                    codut
041400090624     c                   kfld                    dutkci
041500090624     c                   kfld                    itaa1ksc
041600090713     c     katc          klist
041700090713     c                   kfld                    itaa1tat
041800090713     c                   kfld                    itaa1atn
041900090713     c                   kfld                    itaa1atnp
042000090619     c
042100090619     C                   Z-ADD     1             CODUT
042200090619     c
042300090619     c                   ENDSR
