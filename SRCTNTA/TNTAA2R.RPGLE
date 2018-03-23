000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200090618     H* TNTAA1R  *---------------------------------------------------*
000300090618     H*        CONTROLLO abilitazioni utente                         *
000400000000     H*--------------------------------------------------------------*
000500090703     Fazute01l  IF   E           K DISK
000600090703     Fcnclp00F  IF   E           K DISK
000700090624     Ftncpo01l  IF   E           K DISK
000800091023     Ftivis05l  IF   E           K DISK    usropn
000900090713     Ftiatc01l  IF   E           K DISK    usropn
001000130912     fAZCMM01L  if   e           k disk
001100130912     d*
001200090624     d POG             S              3    DIM(250)
001300940802     D*
001400090624     D kpjba         E DS
001500090624     D tntaa1ds      E DS
001600090618     d Azuteds       e ds                  extname(Azute00f)
001700090618     d dDatiute      e ds
001800090618     d dLat          e ds
001900090618     d dute01        e ds
002000090624     d trul31ds      e ds
002100090624     d tibs02ds      e ds
002200090624     d Tibs34ds      e ds
002300090618
002400090618     d savpru          s                   like(itaa1pru)
002500090619     d wabi            s                   like(UteAut)
002600090619     d wcaut           s                   like(itaa1caut)
002700090619     d savcaut         s                   like(itaa1caut)
002800090619     d codut           s              1  0
002900090624     d alfafil         s              3
003000090624     d noabi           s              1
003100090629     d w005a           s              5
003200090619     d wcmm            s                   like(itaa1cmm)
003300090618
003400090618     D CMAIUS          C                   CONST('ABCDEFGHIJKLMNOPQRS-
003500090618     D                                     TUVWYXZ')
003600090618     D CMINUS          C                   CONST('abcdefghijklmnopqrs-
003700090618     D                                     tuvwyxz')
003800940802     C*****************************************************************
003900940802     C* RIEPILOGO INDICATORI
004000940802     C*****************************************************************
004100940802     C*****************************************************************
004200000000     C     *ENTRY        PLIST
004300090618     C                   PARM                    kpjba
004400090618     C                   MOVEL     KPJBU         tntaa1ds
004500981118     C*
004600090619    0c                   if        itaa1tla<>'C'
004700090619     C                   CLEAR                   Otaa1err
004800090626     C                   CLEAR                   Otaa1fabi
004900090626     C                   CLEAR                   Otaa1cabi
005000090629     C                   CLEAR                   Otaa1dupcl
005100090629     C                   CLEAR                   Otaa1car
005200090629     C                   CLEAR                   Otaa1cmm
005300090629     C                   CLEAR                   Otaa1argf
005400130912     c*
005500090619     c                   exsr      Rep_utente
005600000000     C*---------------------------------------------------------------*
005700090619     c* Se l'utente prevede della abilitazioni  controllo
005800090619     c* nello specifico
005900130912     c*
006000090626    1c                   if        otaa1fabi=' '
006100130912     c*
006200090624     c* Immessa FILIALE
006300090629    2c                   if        itaa1FIL>*zeros and otaa1fabi<>'S'
006400090619     C                   MOVEL     itaa1fil      alfafil
006500090619     c     alfafil       lookup    pog                                    90
006600090619    3c                   if        not *in90
006700090626     c                   eval      otaa1fabi='N'
006800090624     C                   ELSE
006900090626     c                   eval      otaa1fabi='S'
007000090619    3c                   endif
007100090619    2c                   endif
007200130912     c*
007300090624     c* Immesso COMMERCIALE
007400090626    2c                   if        itaa1cmm>*zeros  and otaa1fabi<>'S'
007500090619     c                   eval      wcmm=itaa1cmm
007600090626     c                   eval      otaa1cmm=itaa1cmm
007700090619     c                   exsr      CtrCOMM
007800090624    2c                   endif
007900130912     C*
008000090624     c* Immesso CODICE CLIENTE
008100090626    2c                   if        itaa1KSC>*zeros  and otaa1fabi<>'S'
008200090629     c
008300090629     c* Se si tratta di tariffa di cartello, controllo il relativo flag
008400090629     c                   movel     itaa1KSC      w005a
008500130912     c*
008600090629    3c                   If        w005a = '88888'
008700090629    4c                   if        §utecar <> 'S'
008800090629     c                   eval      otaa1fabi='N'
008900090629     c                   else
009000090629     c                   eval      otaa1fabi='S'
009100090629    4c                   endif
009200090629   x3c                   else
009300130912     c*
009400090624     c     kclp          chain     cnclp00f
009500090629    4c                   if        not %found(cnclp00f)
009600090626     c                   eval      otaa1err='S'
009700090626     c                   eval      otaa1fabi='N'
009800090624     c                   else
009900090624     c                   eval      wcmm=clpage
010000090624     c                   eval      otaa1cmm=clpage
010100090624     c                   exsr      CtrCOMM
010200090629    4c                   endif
010300090629    3c                   endif
010400090624    2c                   endif
010500090624     c* Immesso POTENZIALE
010600090626    2c                   if        itaa1cpo>*zeros  and otaa1fabi<>'S'
010700090624     c     itaa1cpo      chain     tncpo01l
010800090624    3c                   if        not %found(tncpo01l)
010900090626     c                   eval      otaa1err='S'
011000090626     c                   eval      otaa1fabi='N'
011100090626   x3c                   else
011200090626     c*
011300090626     C                   MOVEL     cpoflt        alfafil
011400090626     c     alfafil       lookup    pog                                    90
011500090626    4c                   if        *in90
011600090626     c                   eval      otaa1fabi='S'
011700090626    4c                   endif
011800090626    4c                   if        cpocmm>0
011900090624     c                   eval      wcmm=cpocmm
012000090624     c                   eval      otaa1cmm=cpocmm
012100090624     c                   exsr      CtrCOMM
012200090626   x4c                   else
012300090626    5c                   if        not *in90
012400090626     c                   eval      otaa1fabi='N'
012500090626    5c                   endif
012600090626    4c                   endif
012700130912     c*
012800090626    3c                   endif
012900090624    2c                   endif
013000090702     c
013100090713     c* Immesso NUMERO TRATTATIVA COMMERCIALE
013200090702    2c                   if        itaa1NRV>*zeros  and otaa1fabi<>'S'
013300090706     c* se sono in sede --> errore
013400090713     c* sE sono in filiale --> apro il file e controllo
013500110915    3c*                  if        simfel=0
013600110915     c*                  eval      otaa1err='S'
013700110915     c*                  eval      otaa1fabi='N'
013800110915   x3c*                  else
013900130912     c*
014000091023    4c                   if        not %open(tivis05l)
014100091023     c                   open      tivis05l
014200090706    4c                   endif
014300130912     c*
014400090713     C* Per il momento controllo solo il commerciale esterno
014500090713     c*  fino a nuove disposizioni
014600091023     c     itaa1NRV      chain     TIVIS05L
014700091023    4c                   if        not %found(TIVIS05L)
014800090702     c                   eval      otaa1err='S'
014900090702     c                   eval      otaa1fabi='N'
015000090706   x4c                   else
015100090702     c                   eval      wcmm=viscmm
015200090702     c                   eval      otaa1cmm=viscmm
015300090702     c                   exsr      CtrCOMM
015400090706    4c                   endif
015500110915    3c*                  endif
015600090702    2c                   endif
015700130912     c*
015800090713     c* Immesso NUMERO ATTIVITA' COMMERCIALE
015900090713    2c                   if        itaa1ATN>*zeros  and otaa1fabi<>'S'
016000090713     c* se sono in sede --> errore
016100090713     c* sE sono in filiale --> apro il file e controllo
016200090713    3c                   if        simfel=0
016300090713     c                   eval      otaa1err='S'
016400090713     c                   eval      otaa1fabi='N'
016500090713   x3c                   else
016600130912     c*
016700090713    4c                   if        not %open(TIATC01l)
016800090713     c                   open      tiatc01l
016900090713    4c                   endif
017000130912      *
017100090713     c     katc          chain     TIatc01L
017200090713    4c                   if        not %found(TIATC01L)
017300090713     c                   eval      otaa1err='S'
017400090713     c                   eval      otaa1fabi='N'
017500090713   x4c                   else
017600090713     c*  se c'e' commerciale esecuzione attività --> controlo questo
017700090713     c*   altrimenti  commerciale attività
017800090713     c                   if        atccco>0
017900090713     c                   eval      wcmm=atccco
018000090713     c                   eval      otaa1cmm=atccco
018100090713     c                   else
018200090713     c                   eval      wcmm=atccmm
018300090713     c                   eval      otaa1cmm=atccmm
018400090713     c                   endif
018500130912     c*
018600090713     c                   exsr      CtrCOMM
018700090713    4c                   endif
018800090713    3c                   endif
018900090713    2c                   endif
019000090713     c
019100090702
019200090702
019300090624    1c                   endif
019400090626     c                   movel     tntaa1ds      kpjbu
019500090624    0c                   endif
019600130912     c*
019700930210     C*
019800130409    0C     Itaa1tla      IFNE      ' '
019900000000     C                   SETON                                        LR
020000981118     C                   ELSE
020100981118     C                   SETON                                        RT
020200090624    0C                   ENDIF
020300090618
020400090618     C*****************************************************************
020500090618     c     Rep_utente    BEGSR
020600090618     c* Profilo
020700090619    1c                   if        itaa1pru=*blanks
020800090618     c                   movel     knmus         itaa1pru
020900090619    1c                   endif
021000130912     c*
021100090706     c* trasforma il contenuto del campo in lettere maiuscole
021200090706     c                   clear                   wcaut
021300090706    2c                   if        Itaa1caut<> *blanks
021400090706     c                   exsr      Maiusc
021500090706     c                   endif
021600130912     c*
021700090618     c     *dtaara       define    §azute        azuteds
021800090618     c     *dtaara       define    §datiute      ddatiute
021900090618     c                   in(E)     *dtaara
022000130912     c*
022100090703     c* Carico dati se non ancora fatto
022200090703    1c                   If        %error
022300090618     c                   Clear                   Tibs34ds
022400090624     c                   eval      i34ute=itaa1pru
022500090618     c                   Call      'TIBS34R'
022600090618     c                   Parm                    Tibs34ds
022700090618     c                   In        *dtaara
022800090619    1c                   EndIf
022900130912      *
023000090703     c* Carico abilitazioni se profilo diverso da profilo utente
023100090703     c*  o è la prima volta
023200100519      * oppure ho chiesto forzata l'abilitazione
023300090619    0c                   If        %error  or itaa1pru<>savpru
023400090706     c                             or wcaut<>savcaut
023500100519     c                             or (ITAA1abi <> *blanks and
023600100519     c                                 wabi <> ITAA1abi)
023700090618     c                   Clear                   wabi
023800090618     c                   Clear                   dLat
023900090619     c                   Clear                   NOabi
024000130912     c*
024100090703     c                   if        itaa1pru<>knmus
024200090703     c     itaa1pru      chain     azute01l
024300090703     c                   if        not %found(Azute01l)
024400090703     c                   eval      noabi   ='N'
024500090703     c                   endif
024600090703     c                   endif
024700090703     c*
024800130912      *
024900090618      * Verifica errori e autorità profilo
025000090619s    c                   Select
025100090618      * se ho errori nei dati utente esco dal pgm
025200090619w   1c                   When      DutErr = 'E'
025300090624     c                   eval      noabi   ='N'
025400130912     c*
025500090618      * se non c'è l'abilitazione
025600090618      * --> se 1° livello, abilitazioni al terminal
025700090618      *     se 2° livello, abilitazioni al punto operativo
025800090618w   1c                   When      UteAut = *Blanks
025900090618if  2c                   If        DutLpo = '1'
026000090618     c                   Eval      wabi   = 'TP'
026100090618e   2c                   EndIf
026200090618if  2c                   If        DutLpo = '2'
026300090618     c                   Eval      wabi   = 'PO'
026400090618e   2c                   EndIf
026500090618if  2c                   If        DutLpo = 'S'
026600090618     c                   Eval      wabi   = 'AZ'
026700090618e   2c                   EndIf
026800090618      * carica le abilitazioni del profilo
026900090618      * per la nuova offerta
027000090618x   1c                   Other
027100090618     c                   Movel     UteFaf        Dute01
027200130912     c*
027300090624    2c                   if        Itaa1caut= *blanks
027400090619     c                   Eval      wabi = UteAut
027500090619   z2c                   else
027600090618     c*
027700090706    3c                   if        wcaut<>'UTEAUT'
027800130912     c*
027900090619     c                   select
028000090619    4c                   when      wcaut='§UTEGTC'and
028100090619     c                             §utegtc<>*blanks
028200090618     c                   Eval      wabi = §UteGtc
028300090619     c
028400120330    4c                   when      wcaut='§UTEMBS'and
028500120330     c                             §utembs<>*blanks
028600120330     c                   Eval      wabi = §Utembs
028700120330     c
028800090619    4c                   when      wcaut='§UTECTC'and
028900090619     c                             §utectc<>*blanks
029000090619     c                   Eval      wabi = §Utectc
029100130912     c*
029200090619    4c                   when      wcaut='§UTEPOT'and
029300090619     c                             §utepot<>*blanks
029400090619     c                   Eval      wabi = §Utepot
029500130912     c*
029600090619    4c                   when      wcaut='§UTECLI'and
029700090619     c                             §utecli<>*blanks
029800090619     c                   Eval      wabi = §Utecli
029900130912      *
030000101011    4c                   when      wcaut='§UTEIST'and
030100101011     c                             §uteist<>*blanks
030200101011     c                   Eval      wabi = §Uteist
030300090707     c                   other
030400090707     c                   Eval      wabi = UteAut
030500090619e   4c                   EndSl
030600090619    3c                   endif
030700090619    2c                   endif
030800100519      * se richiesta l'abilitazione forzata la imposto
030900100519     c                   IF        ITAA1abi <> *blanks
031000100519     c                   Eval      wabi = ITAA1abi
031100100519     c                   ENDIF
031200130912     c*
031300090619e   1c                   EndSl
031400130912      *
031500090618      * controllo se ok l'abilitazione dell'utente
031600090618     c                   Clear                   Tibs02ds
031700090618     c                   Eval      T02Mod = 'C'
031800090618     c                   Eval      T02Sif = knsif
031900090618     c                   Eval      T02Cod = 'LAT'
032000090618     c                   Movel(p)  wabi          T02Ke1
032100090618     c                   Call      'TIBS02R'
032200090618     c                   Parm                    kpjba
032300090618     c                   Parm                    Tibs02ds
032400090618if  1c                   If        T02Err = *Blanks
032500090618     c                   Eval      dLat = T02Uni
032600090618e   1c                   EndIf
032700090618      * errore o non abilitato
032800090618if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
032900090619     c                   eval      noabi   ='N'
033000090619e   1c                   Else
033100130912      *
033200090618      * Reperimento dei P.O. gestibili dall'utente
033300090618      * per la nuova offerta
033400090618     c                   clear                   TRUL31DS
033500090618     c                   eval      I31abi = wabi
033600090618     c                   eval      I31cdi = DUTdis
033700090618     c                   eval      I31car = DUTare
033800090618     c                   eval      I31cpo = DUTpou
033900090618     c                   call      'TRUL31R'
034000090618     c                   parm                    KPJBA
034100090618     c                   parm                    TRUL31DS
034200090618if  2c                   if        O31pog > *zeros
034300090624     c                   movea     O31pog        pog
034400090618x   2c                   else
034500090619     c                   eval      NOabi='N'
034600090618e   2c                   endif
034700090619e   1c                   endif
034800090706     c                   eval      savcaut=wcaut
034900090619     c                   eval      savpru =itaa1pru
035000090619e   0c                   endif
035100130912     c*
035200090619     c* nessuna abilitazione
035300090626     c                   eval      otaa1fabi=noabi
035400090629     c* Livelli autorizzazione
035500090626     c                   eval      otaa1cabi=wabi
035600090629     c                   eval      otaa1car =§utecar
035700090629     c                   eval      otaa1dupcl=§utedupcli
035800090713     c                   eval      otaa1FLNA =§uteflna
035900130912      *
036000090618     c                   ENDSR
036100090619     c*-------------------------------------------------------------------------
036200090619     c     Maiusc        BEGSR
036300090619     C                   MOVEL     CMAIUS        MA               26
036400090619     C                   MOVEL     CMINUS        MI               26
036500090619     C     MI:MA         XLATE     itaa1caut     wcaut
036600090619     c                   ENDSR
036700090619     C*---------------------------------------------------------------*
036800090619     c     CtrCOMM       BEGSR
036900130912     C     wCMM          CHAIN     AZCMM000                           31
037000130912    1C     *IN31         IFEQ      *ON
037100090626     c* Errore --> codice non trovato
037200090626     c                   eval      otaa1err='S'
037300090626     c                   eval      otaa1fabi='N'
037400130912     c                   eval      CMMuni = wCMM
037500090619    1C                   ENDIF
037600090619     C**
037700130912     C                   MOVEL     CMMuni        otaa1argf
037800090619     C**
037900090626     C** Se ancora non abilitato e non trovato errore
038000090626    1c                   if        otaa1fabi<>'S'  and otaa1err=' '
038100090619     C                   MOVEL     wcmm          alfafil
038200090626     c                   setoff                                       90
038300090619     c     alfafil       lookup    pog                                    90
038400130912     c*
038500090619     C* CERCO COMMERCIALE UNIFICANTE DEL CODICE PASSATO
038600090626    2c                   if        not *in90
038700130912     C                   MOVEL     CMMuni        alfafil
038800090619     c     alfafil       lookup    pog                                    90
038900090626    2c                   endif
039000130912     c*
039100090626    2c                   if        not *in90
039200090626     c                   eval      otaa1fabi='N'
039300090624     c                   else
039400090626     c                   eval      otaa1fabi='S'
039500090626    2c                   endif
039600090626    1c                   endif
039700130912     c*
039800090619     c                   ENDSR
039900090619     C*---------------------------------------------------------------*
040000090619     c     *inzsr        BEGSR
040100130912     c*
040200090624     c     kclp          klist
040300090624     c                   kfld                    codut
040400090624     c                   kfld                    dutkci
040500090624     c                   kfld                    itaa1ksc
040600090713     c     katc          klist
040700090713     c                   kfld                    itaa1tat
040800090713     c                   kfld                    itaa1atn
040900090713     c                   kfld                    itaa1atnp
041000130912     c*
041100130912     C                   Z-ADD     1             CODUT
041200130912     c*
041300090619     c                   ENDSR
