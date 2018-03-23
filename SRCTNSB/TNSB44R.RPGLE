000100051219     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200051219
000300051219      *------------------------------------------------------------------------*
000400051219      *           Controllo manca Tariffa Filiale / Sede                       *
000500051219      *------------------------------------------------------------------------*
000600051219
000700051219      *------------------------------------------------------------------------*
000800051219      *   N O T E
000900051219      *------------------------------------------------------------------------*
001000051219      *
001100051219      *------------------------------------------------------------------------*
001200051219
001300051219     fAzorg01l  if   e           k Disk
001400051219     fCnaco00f  if   e           k Disk
001500060131     fTabel00f  if   e           k Disk
001600051219     fTnsb44d   cf   e             Workstn
001700051219
001800051219      *------------------------------------------------------------------------*
001900051219      *   C A M P I   D I   L A V O R O
002000051219      *------------------------------------------------------------------------*
002100051222     d dateu           s              8  0
002200051223     d w0140           s             14  0
002300051223     d wdata           s              8  0
002400051223     d comfil          s              3
002500060131     d comare          s              3
002600060208     d com8a           s              8
002700051222
002800060131     d wabi            s                   like(UteAut)
002900060131     d Keycod          s                   like(TblCod)
003000060131     d Keykey          s                   like(Tblkey)
003100051223     d codut           s              1  0 Inz(1)
003200051219
003300051219      *------------------------------------------------------------------------*
003400051219      *   S C H I E R E
003500051219      *------------------------------------------------------------------------*
003600051219
003700060130      * Aree gestibili dall'utente
003800060130     d arg             s              3    dim(250) inz(*Zeros)
003900051222      * P.O. gestibili dall'utente
004000051222     d fig             s              3    dim(250) inz(*Zeros)
004100051219
004200051219      *------------------------------------------------------------------------*
004300051219      *   D S   I N T E R N E / E S T E R N E
004400051219      *------------------------------------------------------------------------*
004500051219
004600051219     d wlbdat          ds                  Inz
004700051219     d  g02dat                 1      8  0
004800051219     d  g02inv                 9     16  0
004900051219     d  g02err                17     17
005000051219     d  g02tgi                18     22  0
005100051219
005200051219     d Tnsb44ds      e ds                  inz
005300051219
005400051219     d Trul06ds      e ds
005500051219     d  l1                     1     90  0 Dim(30)
005600051219     d AzuteDs       e ds                  ExtName(AzUte00F)
005700051219     d DDatiUte      e ds
005800051222     d Tibs34Ds      e ds
005900051223     d Tibs02ds      e ds
006000051222     d Trul31ds      e ds
006100060130     d Trul31ds2     e ds
006200051222     d Dute01        e ds
006300051222     d dLat          e ds
006400051219
006500051219     d Kpjba         e ds
006600051219
006700051219      *------------------------------------------------------------------------*
006800051219      *   I N D I C A T O R I
006900051219      *------------------------------------------------------------------------*
007000051222      * 05 -  ON se sono in sede
007100060222      * 10 -  ON utente autorizzato all'area
007200060130      * 11 -  ON utente autorizzato ad una sola area
007300051222      * 30    Errore generico
007400100519      * 38/55 Errori a video
007500051219      * 90    Errore generico a video
007600051219      *------------------------------------------------------------------------*
007700051219
007800051219      *------------------------------------------------------------------------*
007900051222      * CICLO PRINCIPALE
008000051222      *------------------------------------------------------------------------*
008100051222
008200051222     c                   Do        *Hival
008300051222      *
008400051222     c                   exfmt     Sb44d01
008500051222      *
008600100519      * se lanciato da un sistema informativo errato vado a fine
008700100519     c                   If        *in55
008800100519     c                   leave
008900100519     c                   endif
009000100519      *
009100051222     c                   eval      *in90 = *off
009200051222      *
009300051222      * se indicatore di errore 39 acceso vado a fine
009400051222      * se F3 / KC                 acceso vado a fine
009500051222     c                   if        *in39  or *inkc
009600051222     c                   leave
009700051222     c                   endif
009800051222      *
009900051222     c                   exsr      SrContrD01
010000051222      * Errore generico
010100051222     c                   If        *in90
010200051222     c                   iter
010300051222     c                   endif
010400051222      * F6 / KF Conferma
010500051222     c                   If        *inkf
010600051222     c                   exsr      SrConfe
010700060104     c                   leave
010800051222     c                   endif
010900051222      *
011000051222     c                   enddo
011100051222      *
011200051222     c     fine          tag
011300051222     c                   eval      *inlr = *on
011400051222      *------------------------------------------------------------------------*
011500051222      * ROUTINE CONTROLLO DELLA VIDEATA
011600051222      *------------------------------------------------------------------------*
011700051222     c     SrContrD01    BegSr
011800051222
011900051222     c                   Do        *hival
012000060131      * controllo AREA
012100051222      *
012200060131     c                   If        VidAre = 0 and *in05=*off and VidFil = 0
012300060308     c****                         and *in11 = *off
012400060201      * se uguale a zero e non sono in sede errore e linea a zero  e ho solo un'area
012500051222     c                   eval      *in49 = *on
012600051222     c                   eval      *in90 = *on
012700051222     c                   leave
012800051222     c                   endif
012900051222      * se diverso da zero e non sono un secondo livello deve essere
013000060131      * un'area     e deve essere presente nella schiera delle aree da gestire
013100060131
013200060131      * controllo validità dell'area
013300060104
013400060201     c                   clear                   Desare
013500060201
013600060131     c                   If        VidAre > 0
013700060131
013800060131      * area
013900060131     c                   movel     '05'          Keycod
014000060131     c                   movel     *blanks       keykey
014100060131     c                   movel     vidare        keykey
014200060131     c     keytab        chain     tabel00f
014300060131
014400060131     c                   If        not %found(Tabel00f) or tblflg <> ' '
014500051222     c                   eval      *in47 = *on
014600051222     c                   eval      *in90 = *on
014700051222     c                   leave
014800051222     c                   endif
014900060131      * Decodifico l'area
015000060131     c                   eval      DesAre = Tbluni
015100060131     c                   eval      D44Dar = Tbluni
015200060131     c                   eval      D44Are = VidAre
015300051222
015400060201     c                   endif
015500060201
015600060131      * Verifico se utente è autorizzato ad usare questa area
015700051222
015800060131     c                   If        VidAre > 0 and DutLpo <> '2'
015900060131     c                   movel     VidAre        ComAre
016000060131     c     ComAre        lookup    Arg                                    30
016100051222     c                   If        not %Found
016200051222     c                   eval      *in38 = *on
016300051222     c                   eval      *in90 = *on
016400051222     c                   leave
016500051222     c                   endif
016600051222     c                   endif
016700051222
016800060104
016900051222      *
017000051222      * Controllo LINEA DA CONTROLLARE
017100051222      *
017200060201
017300060201     c                   clear                   desfil
017400051222
017500051222      * Linea non può essere non valorizzata se siamo un secondo livello quindi imposto
017600051222      * P.O. utente
017700051222     c                   If        VidFil = 0 and DutLpo = '2'
017800051222     c                   Eval      VidFil = DutPou
017900051222     c                   Eval      DesFil = DutDpo
018000051222     c                   endif
018100051222      * Non si può mettere la linea sede
018200051222     c                   If        VidFil = 046
018300051222     c                   eval      *in40 = *on
018400051222     c                   eval      *in90 = *on
018500051222     c                   leave
018600051222     c                   endif
018700051222      *
018800051222      * Controllo validità
018900051223     c                   If        VidFil > 0
019000051222     c     VidFil        Chain     Azorg01l
019100051222      *
019200051222     c                   If        not %found(Azorg01l)
019300051222     c                   eval      *in41 = *on
019400051222     c                   eval      *in90 = *on
019500051222     c                   leave
019600051222     c                   endif
019700051222      *  Annullato / No filiale no Agenzia  ERRORE
019800051222     c                   If        Orgfva <> ' ' or (orgfag <> 'A' and
019900051222     c                             Orgfag <> 'F')
020000051222     c                   eval      *in41 = *on
020100051222     c                   eval      *in90 = *on
020200051222     c                   leave
020300051222     c                   endif
020400051222     c                   eval      DesFil = OrgDes
020500051222     c                   eval      D44Dfi = OrgDes
020600051223     c                   eval      D44Fil = VidFil
020700060131      * Se immesso l'Area il P.O.  deve appartenere all'area
020800060131     c                   If        VidAre > 0  and orgcar <> vidare
020900051222     c                   eval      *in48 = *on
021000051222     c                   eval      *in90 = *on
021100051222     c                   leave
021200051222     c                   endif
021300051222     c                   endif
021400051222
021500051222      * Verifico se utente è autorizzato ad usare questo P.O.
021600051222
021700051223     c                   movel     VidFil        ComFil
021800051223     c     ComFil        lookup    Fig                                    30
021900051222     c                   If        not %Found
022000051222     c                   eval      *in51 = *on
022100051222     c                   eval      *in90 = *on
022200051222     c                   leave
022300051222     c                   endif
022400051222
022500051223      * Controllo Date spedizione
022600051223
022700051223      * Data Spedizione DAL
022800051223     c                   eval      G02Dat = VidDsd
022900051223     c                   clear                   G02Inv
023000051223     c                   clear                   G02Err
023100051223     c                   call      'XSRDA8'
023200051223     c                   parm                    WlbDat
023300051223     c                   If        G02Err = '1'
023400051223     c                   eval      *in42 = *on
023500051223     c                   eval      *in90 = *on
023600051223     c                   leave
023700051223     c                   endif
023800051223     c                   eval      VidDsd = G02Dat
023900051223     c                   eval      D44Dsd = G02INV
024000051223
024100051223      * Data Spedizione AL
024200051223     c                   If        VidDsa > 0
024300051223     c                   eval      G02Dat = VidDsa
024400051223     c                   clear                   G02Inv
024500051223     c                   clear                   G02Err
024600051223     c                   call      'XSRDA8'
024700051223     c                   parm                    WlbDat
024800051223     c                   If        G02Err = '1'
024900051223     c                   eval      *in43 = *on
025000051223     c                   eval      *in90 = *on
025100051223     c                   leave
025200051223     c                   endif
025300051223     c                   eval      VidDsa = G02Dat
025400051223     c                   eval      D44Dsa = G02INV
025500051223
025600051223      * Data Dal minore di Data Al
025700051223     c                   If        D44Dsd > D44Dsa
025800051223     c                   eval      *in44 = *on
025900051223     c                   eval      *in90 = *on
026000051223     c                   leave
026100051223     c                   endif
026200051223
026300051223     c                   Else
026400051223
026500051223     c                   clear                   D44dsa
026600051223     c                   endif
026700051223
026800051223      * Controllo Codice Cliente
026900051223
027000051223     c                   If        VidCli > 0
027100051223      * Verifico l'esistenza
027200051223     c     KeyAco        Chain     Cnaco00f
027300051223     c                   If        not %found(Cnaco00f)
027400051223     c                   eval      *in46 = *on
027500051223     c                   eval      *in90 = *on
027600051223     c                   leave
027700051223     c                   endif
027800051223
027900051223     c                   eval      VidCld = AcoRag
028000051223      * se cliente esistente verifico se appartiene ad un P.O. autorizzato all'utente
028100051223     c                   movel     VidCli        ComFil
028200051223     c     ComFil        lookup    Fig                                    30
028300051223     c                   If        not %Found
028400051223     c                   eval      *in45 = *on
028500051223     c                   eval      *in90 = *on
028600051223     c                   leave
028700051223     c                   endif
028800051223     c                   eval      D44Cli =  VidCli
028900060210     c                   eval      D44Rag =  Acorag
029000051223     c                   endif
029100051223
029200051223      * Controllo i vari flag
029300051223     c                   If        VidImp = 'S' and VidCli = 0
029400060127     c                   eval      *in52 = *on
029500060127     c                   eval      *in90 = *on
029600060127     c                   leave
029700051223     c                   endif
029800060208     c                   If        VidImp = 'S' and VidCli > 0  and
029900060208     c                             Vidspp = 'S'
030000060208     c                   eval      *in53 = *on
030100060208     c                   eval      *in90 = *on
030200060208     c                   leave
030300060208     c                   endif
030400051223
030500060223      * Controllo flag totale per area
030600060223      * solo se non inserito codice cliente e punto operativo
030700060223     c                   If        Vidtot = 'S' and
030800060223     c                             (vidcli > 0 or vidfil > 0)
030900060223     c                   eval      *in54 = *on
031000060223     c                   eval      *in90 = *on
031100060223     c                   leave
031200060223     c                   endif
031300060223
031400051223     c                   leave
031500051223
031600051222     c                   enddo
031700051222
031800051222     c                   endsr
031900051223      *------------------------------------------------------------------------*
032000051223      * ROUTINE CONFERMA DELLA VIDEATA
032100051223      *------------------------------------------------------------------------*
032200051223     c     SrConfe       BegSr
032300051223
032400051223      * Preaparazione della DS per il lancio
032500060223      * se data al uguale a zero  metto uguale a data dal
032600060223     c                   If        d44dsa = 0
032700060223     c                   eval      d44dsa = d44dsd
032800060223     c                   endif
032900051223
033000051223     c                   eval      D44Fsi = VidImp
033100051223     c                   eval      D44Spp = VidSpp
033200051223     c                   eval      D44Crf = VidCrf
033300060208      * se si desidera il file imposto il nome del membro
033400060208     c                   if        D44Crf = 'S'
033500060208     c                   movel     dateu         com8a
033600060208     c                   eval      D44mbr = 'S'+ com8a
033700060208     c                   endif
033800051223      * se non si desiderano stampe separate per terminal o per P.O. valorizzo il flag di stampa
033900051223      * unica
034000060201     c                   If        D44Spp = 'N'
034100051223     c                   eval      D44Sun = 'S'
034200051223     c                   eval      D44Out = 'U'                                 Coda stampa utente
034300051223     c                   else
034400051223     c                   eval      D44sun = 'N'
034500060201     c                   eval      D44Out = 'O'                                 Coda stampa organigr
034600051223     c                   endif
034700060222      * Totale per area
034800060222     c                   eval      D44Tot = VidTot
034900060222      * stampante dei totali  se posso selezionare una sola area (11 spento) stampante utente
035000060222      * altrimenti separata per area tabella 05
035100060222     c                   if        not  *in11
035200060227     c                   eval      D44Tou = 'U'
035300060222     c                   else
035400060227     c                   eval      D44tou = 'T'
035500060222     c                   endif
035600120308
035700120308      *?Imposto 'M' in D44gio in questo modo il TNSB45R riconosce il lancio da menù
035800120308     c                   eval      D44gio = 'M'
035900051223
036000051223     c                   movel     tnsb44ds      Kpjbu
036100051223     c                   movel     'SB45'        Kcoaz
036200060104      * BCH09 se sono in sede  *in05 = *on
036300060104     c                   If        *in05
036400051223     c                   move      '3'           Kritb
036500051223     c                   call      'BCH09'
036600051223     c                   parm                    Kpjba
036700060104     c                   endif
036800060104      * BCH10
036900051223     c                   If        Kritb = ' '
037000060306     c                   call      'BCH10'
037100060306     c****               call      'TNSB45R'
037200060227     C                   parm                    Kpjba
037300051223     c                   endif
037400060104
037500060104     c
037600051223      *
037700051223     c                   Endsr
037800051222      *
037900051219      *------------------------------------------------------------------------*
038000051219      * ROUTINE INIZIALE
038100051219      *------------------------------------------------------------------------*
038200051219     c     *Inzsr        BegSr
038300051219
038400051219     c     *entry        plist
038500051219     c                   parm                    Kpjba
038600051222      * Reperisco data corrente
038700051222     c                   time                    w0140
038800051222     c                   move      w0140         wData
038900051222     c                   clear                   WLBdat
039000051222     c                   eval      G02dat = wData
039100051222     c                   call      'XSRDA8'
039200051222     c                   parm                    WLBdat
039300051222     c                   z-add     G02inv        DateU
039400051219
039500051219     c     *dtaara       define    §azute        azuteds
039600051219     c     *dtaara       define    §datiute      ddatiute
039700051219     c                   in(E)     *dtaara
039800051219     c                   If        %error  or RSUT = *blanks
039900051219     c                   CLEAR                   tibs34ds
040000051219     c                   CALL      'TIBS34R'
040100051219     c                   PARM                    tibs34ds
040200051219     c                   in        *dtaara
040300051219     c                   EndIf
040400051219
040500051222      * -> Imposto il p.o. utente
040600051219      * Utente di sede
040700051219if  1c                   If        DutPou = 46
040800051219     c                   Eval      *In05 = *On
040900100519      * -> Verifico il sistema informartivo
041000100519     c                   If        Knsif <> 'GAITRA201' and
041100100519     c                             Knsif <> 'GAITRAPSP'
041200100519     c                   eval      *in55 = *on
041300100519     c                   eval      *in90 = *on
041400100519     c                   endif
041500060201   x1c                   Endif
041600051219      * Utente di Filiale proteggo il campo
041700051219      * se non è un primo livello
041800051223     c                   If        Dutlpo = '2'
041900051222     c                   Eval      VidFil = DutPou
042000051222     c                   Eval      DesFil = DutDpo
042100051223     c                   Eval      *In10 = *Off
042200051223     c                   endif
042300051223      * se  è un primo livello
042400051223     c                   If        Dutlpo = '1'
042500051222     c                   Eval      *In05 = *Off
042600051219    1c                   EndIf
042700051222
042800051222     c                   Clear                   wabi
042900051222     c                   Clear                   dLat
043000051222
043100051222      * Verifica errori e autorità profilo
043200051222s   1c                   Select
043300051222      * se ho errori nei dati utente esco dal pgm
043400051222w   1c                   When      DutErr = 'E'
043500051222     c                   GoTo      Fine
043600051222      * se non c'è l'abilitazione
043700051222      * --> se 1° livello, abilitazioni al terminal
043800051222      *     se 2° livello, abilitazioni al punto operativo
043900051222      *     se sede è impossibile ma se capita mando a fine il pgm
044000051222w   1c                   When      UteAut = *Blanks
044100051222if  2c                   If        DutLpo = '1'
044200051222     c                   Eval      wabi   = 'TP'
044300051222e   2c                   EndIf
044400051222if  2c                   If        DutLpo = '2'
044500051222     c                   Eval      wabi   = 'PO'
044600051222e   2c                   EndIf
044700051222if  2c                   If        DutLpo = 'S'
044800051222     c                   GoTo      Fine
044900051222e   2c                   EndIf
045000051222      * carica le abilitazioni del profilo
045100051222x   1c                   Other
045200051222     c                   Movel     UteFaf        Dute01
045300051222     c                   If        §UteGtc <> *Blanks
045400051222     c                   Eval      wabi = §UteGtc
045500051222     c                   Else
045600051222     c                   Eval      wabi = UteAut
045700051222     c                   EndIf
045800051222e   1c                   EndSl
045900051222
046000051222      * controllo se ok l'abilitazione dell'utente
046100051222     c                   Clear                   Tibs02ds
046200051222     c                   Eval      T02Mod = 'C'
046300051222     c                   Eval      T02Sif = knsif
046400051222     c                   Eval      T02Cod = 'LAT'
046500051222     c                   Movel(p)  wabi          T02Ke1
046600051222     c                   Call      'TIBS02R'
046700051222     c                   Parm                    kpjba
046800051222     c                   Parm                    Tibs02ds
046900051222if  1c                   If        T02Err = *Blanks
047000051222     c                   Eval      dLat = T02Uni
047100051222e   1c                   EndIf
047200051222      * errore o non abilitato
047300051222if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
047400051222     c                   Seton                                        3990
047500051222x   1c                   Else
047600051222      *
047700051222      * Reperimento dei P.O. gestibili dall'utente
047800051222     c                   clear                   TRUL31DS
047900060201     c                   clear                   TRUL31DS2
048000051222     c                   eval      I31abi = wabi
048100051222     c                   eval      I31cdi = DUTdis
048200051222     c                   eval      I31car = DUTare
048300051222     c                   eval      I31cpo = DUTpou
048400051222     c                   call      'TRUL31R'
048500051222     c                   parm                    KPJBA
048600051222     c                   parm                    TRUL31DS
048700060130     c                   parm                    TRUL31DS2
048800051222if  2c                   if        O31pog > *zeros
048900051222     c                   movea     O31pog        fig
049000060130     c                   movea     O31arg        arg
049100051222x   2c                   else
049200051222     c                   Seton                                        3990
049300051222e   2c                   endif
049400051222
049500051222e   1c                   endif
049600051223
049700051223      * imposto i campi del video
049800051223
049900051223     c                   eval      VidImp = 'N'
050000051223     c                   eval      VidSpp = 'N'
050100051223     c                   eval      VidCrf = 'N'
050200060223     c                   eval      VidTot = 'N'
050300060130      * imposto indicatore
050400060201     c                   if        arg(1) > *zeros
050500060130     c                   seton                                        10
050600060130      * verifico se esistono + aree
050700060201     c                   eval      *in11 = (arg(2) <> *zeros)
050800060130
050900060130     c  n11              movel     arg(1)        vidare
051000060130
051100060130     c                   endif
051200051223      *
051300051223      *------------------------------------------------------------------------*
051400051223      * KLIST
051500051223      *------------------------------------------------------------------------*
051600051223
051700051223      *---- File Cnaco00f
051800051223
051900051223     c     KeyAco        Klist
052000051223     c                   Kfld                    Codut
052100051223     c                   Kfld                    DutKci
052200051223     c                   Kfld                    VidCli
052300060131
052400060131      *---- File Tabel00f
052500060131
052600060131     c     KeyTab        Klist
052700060131     c                   Kfld                    Codut
052800060131     c                   Kfld                    Keycod
052900060131     c                   Kfld                    Keykey
053000051223      *
053100051219     c                   EndSr
