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
007400060208      * 38/53 Errori a video
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
008600051222     c                   eval      *in90 = *off
008700051222      *
008800051222      * se indicatore di errore 39 acceso vado a fine
008900051222      * se F3 / KC                 acceso vado a fine
009000051222     c                   if        *in39  or *inkc
009100051222     c                   leave
009200051222     c                   endif
009300051222      *
009400051222     c                   exsr      SrContrD01
009500051222      * Errore generico
009600051222     c                   If        *in90
009700051222     c                   iter
009800051222     c                   endif
009900051222      * F6 / KF Conferma
010000051222     c                   If        *inkf
010100051222     c                   exsr      SrConfe
010200060104     c                   leave
010300051222     c                   endif
010400051222      *
010500051222     c                   enddo
010600051222      *
010700051222     c     fine          tag
010800051222     c                   eval      *inlr = *on
010900051222      *------------------------------------------------------------------------*
011000051222      * ROUTINE CONTROLLO DELLA VIDEATA
011100051222      *------------------------------------------------------------------------*
011200051222     c     SrContrD01    BegSr
011300051222
011400051222     c                   Do        *hival
011500060131      * controllo AREA
011600051222      *
011700060131     c                   If        VidAre = 0 and *in05=*off and VidFil = 0
011800060308     c****                         and *in11 = *off
011900060201      * se uguale a zero e non sono in sede errore e linea a zero  e ho solo un'area
012000051222     c                   eval      *in49 = *on
012100051222     c                   eval      *in90 = *on
012200051222     c                   leave
012300051222     c                   endif
012400051222      * se diverso da zero e non sono un secondo livello deve essere
012500060131      * un'area     e deve essere presente nella schiera delle aree da gestire
012600060131
012700060131      * controllo validità dell'area
012800060104
012900060201     c                   clear                   Desare
013000060201
013100060131     c                   If        VidAre > 0
013200060131
013300060131      * area
013400060131     c                   movel     '05'          Keycod
013500060131     c                   movel     *blanks       keykey
013600060131     c                   movel     vidare        keykey
013700060131     c     keytab        chain     tabel00f
013800060131
013900060131     c                   If        not %found(Tabel00f) or tblflg <> ' '
014000051222     c                   eval      *in47 = *on
014100051222     c                   eval      *in90 = *on
014200051222     c                   leave
014300051222     c                   endif
014400060131      * Decodifico l'area
014500060131     c                   eval      DesAre = Tbluni
014600060131     c                   eval      D44Dar = Tbluni
014700060131     c                   eval      D44Are = VidAre
014800051222
014900060201     c                   endif
015000060201
015100060131      * Verifico se utente è autorizzato ad usare questa area
015200051222
015300060131     c                   If        VidAre > 0 and DutLpo <> '2'
015400060131     c                   movel     VidAre        ComAre
015500060131     c     ComAre        lookup    Arg                                    30
015600051222     c                   If        not %Found
015700051222     c                   eval      *in38 = *on
015800051222     c                   eval      *in90 = *on
015900051222     c                   leave
016000051222     c                   endif
016100051222     c                   endif
016200051222
016300060104
016400051222      *
016500051222      * Controllo LINEA DA CONTROLLARE
016600051222      *
016700060201
016800060201     c                   clear                   desfil
016900051222
017000051222      * Linea non può essere non valorizzata se siamo un secondo livello quindi imposto
017100051222      * P.O. utente
017200051222     c                   If        VidFil = 0 and DutLpo = '2'
017300051222     c                   Eval      VidFil = DutPou
017400051222     c                   Eval      DesFil = DutDpo
017500051222     c                   endif
017600051222      * Non si può mettere la linea sede
017700051222     c                   If        VidFil = 046
017800051222     c                   eval      *in40 = *on
017900051222     c                   eval      *in90 = *on
018000051222     c                   leave
018100051222     c                   endif
018200051222      *
018300051222      * Controllo validità
018400051223     c                   If        VidFil > 0
018500051222     c     VidFil        Chain     Azorg01l
018600051222      *
018700051222     c                   If        not %found(Azorg01l)
018800051222     c                   eval      *in41 = *on
018900051222     c                   eval      *in90 = *on
019000051222     c                   leave
019100051222     c                   endif
019200051222      *  Annullato / No filiale no Agenzia  ERRORE
019300051222     c                   If        Orgfva <> ' ' or (orgfag <> 'A' and
019400051222     c                             Orgfag <> 'F')
019500051222     c                   eval      *in41 = *on
019600051222     c                   eval      *in90 = *on
019700051222     c                   leave
019800051222     c                   endif
019900051222     c                   eval      DesFil = OrgDes
020000051222     c                   eval      D44Dfi = OrgDes
020100051223     c                   eval      D44Fil = VidFil
020200060131      * Se immesso l'Area il P.O.  deve appartenere all'area
020300060131     c                   If        VidAre > 0  and orgcar <> vidare
020400051222     c                   eval      *in48 = *on
020500051222     c                   eval      *in90 = *on
020600051222     c                   leave
020700051222     c                   endif
020800051222     c                   endif
020900051222
021000051222      * Verifico se utente è autorizzato ad usare questo P.O.
021100051222
021200051223     c                   movel     VidFil        ComFil
021300051223     c     ComFil        lookup    Fig                                    30
021400051222     c                   If        not %Found
021500051222     c                   eval      *in51 = *on
021600051222     c                   eval      *in90 = *on
021700051222     c                   leave
021800051222     c                   endif
021900051222
022000051223      * Controllo Date spedizione
022100051223
022200051223      * Data Spedizione DAL
022300051223     c                   eval      G02Dat = VidDsd
022400051223     c                   clear                   G02Inv
022500051223     c                   clear                   G02Err
022600051223     c                   call      'XSRDA8'
022700051223     c                   parm                    WlbDat
022800051223     c                   If        G02Err = '1'
022900051223     c                   eval      *in42 = *on
023000051223     c                   eval      *in90 = *on
023100051223     c                   leave
023200051223     c                   endif
023300051223     c                   eval      VidDsd = G02Dat
023400051223     c                   eval      D44Dsd = G02INV
023500051223
023600051223      * Data Spedizione AL
023700051223     c                   If        VidDsa > 0
023800051223     c                   eval      G02Dat = VidDsa
023900051223     c                   clear                   G02Inv
024000051223     c                   clear                   G02Err
024100051223     c                   call      'XSRDA8'
024200051223     c                   parm                    WlbDat
024300051223     c                   If        G02Err = '1'
024400051223     c                   eval      *in43 = *on
024500051223     c                   eval      *in90 = *on
024600051223     c                   leave
024700051223     c                   endif
024800051223     c                   eval      VidDsa = G02Dat
024900051223     c                   eval      D44Dsa = G02INV
025000051223
025100051223      * Data Dal minore di Data Al
025200051223     c                   If        D44Dsd > D44Dsa
025300051223     c                   eval      *in44 = *on
025400051223     c                   eval      *in90 = *on
025500051223     c                   leave
025600051223     c                   endif
025700051223
025800051223     c                   Else
025900051223
026000051223     c                   clear                   D44dsa
026100051223     c                   endif
026200051223
026300051223      * Controllo Codice Cliente
026400051223
026500051223     c                   If        VidCli > 0
026600051223      * Verifico l'esistenza
026700051223     c     KeyAco        Chain     Cnaco00f
026800051223     c                   If        not %found(Cnaco00f)
026900051223     c                   eval      *in46 = *on
027000051223     c                   eval      *in90 = *on
027100051223     c                   leave
027200051223     c                   endif
027300051223
027400051223     c                   eval      VidCld = AcoRag
027500051223      * se cliente esistente verifico se appartiene ad un P.O. autorizzato all'utente
027600051223     c                   movel     VidCli        ComFil
027700051223     c     ComFil        lookup    Fig                                    30
027800051223     c                   If        not %Found
027900051223     c                   eval      *in45 = *on
028000051223     c                   eval      *in90 = *on
028100051223     c                   leave
028200051223     c                   endif
028300051223     c                   eval      D44Cli =  VidCli
028400060210     c                   eval      D44Rag =  Acorag
028500051223     c                   endif
028600051223
028700051223      * Controllo i vari flag
028800051223     c                   If        VidImp = 'S' and VidCli = 0
028900060127     c                   eval      *in52 = *on
029000060127     c                   eval      *in90 = *on
029100060127     c                   leave
029200051223     c                   endif
029300060208     c                   If        VidImp = 'S' and VidCli > 0  and
029400060208     c                             Vidspp = 'S'
029500060208     c                   eval      *in53 = *on
029600060208     c                   eval      *in90 = *on
029700060208     c                   leave
029800060208     c                   endif
029900051223
030000060223      * Controllo flag totale per area
030100060223      * solo se non inserito codice cliente e punto operativo
030200060223     c                   If        Vidtot = 'S' and
030300060223     c                             (vidcli > 0 or vidfil > 0)
030400060223     c                   eval      *in54 = *on
030500060223     c                   eval      *in90 = *on
030600060223     c                   leave
030700060223     c                   endif
030800060223
030900051223     c                   leave
031000051223
031100051222     c                   enddo
031200051222
031300051222     c                   endsr
031400051223      *------------------------------------------------------------------------*
031500051223      * ROUTINE CONFERMA DELLA VIDEATA
031600051223      *------------------------------------------------------------------------*
031700051223     c     SrConfe       BegSr
031800051223
031900051223      * Preaparazione della DS per il lancio
032000060223      * se data al uguale a zero  metto uguale a data dal
032100060223     c                   If        d44dsa = 0
032200060223     c                   eval      d44dsa = d44dsd
032300060223     c                   endif
032400051223
032500051223     c                   eval      D44Fsi = VidImp
032600051223     c                   eval      D44Spp = VidSpp
032700051223     c                   eval      D44Crf = VidCrf
032800060208      * se si desidera il file imposto il nome del membro
032900060208     c                   if        D44Crf = 'S'
033000060208     c                   movel     dateu         com8a
033100060208     c                   eval      D44mbr = 'S'+ com8a
033200060208     c                   endif
033300051223      * se non si desiderano stampe separate per terminal o per P.O. valorizzo il flag di stampa
033400051223      * unica
033500060201     c                   If        D44Spp = 'N'
033600051223     c                   eval      D44Sun = 'S'
033700051223     c                   eval      D44Out = 'U'                                 Coda stampa utente
033800051223     c                   else
033900051223     c                   eval      D44sun = 'N'
034000060201     c                   eval      D44Out = 'O'                                 Coda stampa organigr
034100051223     c                   endif
034200060222      * Totale per area
034300060222     c                   eval      D44Tot = VidTot
034400060222      * stampante dei totali  se posso selezionare una sola area (11 spento) stampante utente
034500060222      * altrimenti separata per area tabella 05
034600060222     c                   if        not  *in11
034700060227     c                   eval      D44Tou = 'U'
034800060222     c                   else
034900060227     c                   eval      D44tou = 'T'
035000060222     c                   endif
035100051223
035200051223     c                   movel     tnsb44ds      Kpjbu
035300051223     c                   movel     'SB45'        Kcoaz
035400060104      * BCH09 se sono in sede  *in05 = *on
035500060104     c                   If        *in05
035600051223     c                   move      '3'           Kritb
035700051223     c                   call      'BCH09'
035800051223     c                   parm                    Kpjba
035900060104     c                   endif
036000060104      * BCH10
036100051223     c                   If        Kritb = ' '
036200060306     c                   call      'BCH10'
036300060306     c****               call      'TNSB45R'
036400060227     C                   parm                    Kpjba
036500051223     c                   endif
036600060104
036700060104     c
036800051223      *
036900051223     c                   Endsr
037000051222      *
037100051219      *------------------------------------------------------------------------*
037200051219      * ROUTINE INIZIALE
037300051219      *------------------------------------------------------------------------*
037400051219     c     *Inzsr        BegSr
037500051219
037600051219     c     *entry        plist
037700051219     c                   parm                    Kpjba
037800051222      * Reperisco data corrente
037900051222     c                   time                    w0140
038000051222     c                   move      w0140         wData
038100051222     c                   clear                   WLBdat
038200051222     c                   eval      G02dat = wData
038300051222     c                   call      'XSRDA8'
038400051222     c                   parm                    WLBdat
038500051222     c                   z-add     G02inv        DateU
038600051219
038700051219     c     *dtaara       define    §azute        azuteds
038800051219     c     *dtaara       define    §datiute      ddatiute
038900051219     c                   in(E)     *dtaara
039000051219     c                   If        %error  or RSUT = *blanks
039100051219     c                   CLEAR                   tibs34ds
039200051219     c                   CALL      'TIBS34R'
039300051219     c                   PARM                    tibs34ds
039400051219     c                   in        *dtaara
039500051219     c                   EndIf
039600051219
039700051222      * -> Imposto il p.o. utente
039800051219      * Utente di sede
039900051219if  1c                   If        DutPou = 46
040000051219     c                   Eval      *In05 = *On
040100060201   x1c                   Endif
040200051219      * Utente di Filiale proteggo il campo
040300051219      * se non è un primo livello
040400051223     c                   If        Dutlpo = '2'
040500051222     c                   Eval      VidFil = DutPou
040600051222     c                   Eval      DesFil = DutDpo
040700051223     c                   Eval      *In10 = *Off
040800051223     c                   endif
040900051223      * se  è un primo livello
041000051223     c                   If        Dutlpo = '1'
041100051222     c                   Eval      *In05 = *Off
041200051219    1c                   EndIf
041300051222
041400051222     c                   Clear                   wabi
041500051222     c                   Clear                   dLat
041600051222
041700051222      * Verifica errori e autorità profilo
041800051222s   1c                   Select
041900051222      * se ho errori nei dati utente esco dal pgm
042000051222w   1c                   When      DutErr = 'E'
042100051222     c                   GoTo      Fine
042200051222      * se non c'è l'abilitazione
042300051222      * --> se 1° livello, abilitazioni al terminal
042400051222      *     se 2° livello, abilitazioni al punto operativo
042500051222      *     se sede è impossibile ma se capita mando a fine il pgm
042600051222w   1c                   When      UteAut = *Blanks
042700051222if  2c                   If        DutLpo = '1'
042800051222     c                   Eval      wabi   = 'TP'
042900051222e   2c                   EndIf
043000051222if  2c                   If        DutLpo = '2'
043100051222     c                   Eval      wabi   = 'PO'
043200051222e   2c                   EndIf
043300051222if  2c                   If        DutLpo = 'S'
043400051222     c                   GoTo      Fine
043500051222e   2c                   EndIf
043600051222      * carica le abilitazioni del profilo
043700051222x   1c                   Other
043800051222     c                   Movel     UteFaf        Dute01
043900051222     c                   If        §UteGtc <> *Blanks
044000051222     c                   Eval      wabi = §UteGtc
044100051222     c                   Else
044200051222     c                   Eval      wabi = UteAut
044300051222     c                   EndIf
044400051222e   1c                   EndSl
044500051222
044600051222      * controllo se ok l'abilitazione dell'utente
044700051222     c                   Clear                   Tibs02ds
044800051222     c                   Eval      T02Mod = 'C'
044900051222     c                   Eval      T02Sif = knsif
045000051222     c                   Eval      T02Cod = 'LAT'
045100051222     c                   Movel(p)  wabi          T02Ke1
045200051222     c                   Call      'TIBS02R'
045300051222     c                   Parm                    kpjba
045400051222     c                   Parm                    Tibs02ds
045500051222if  1c                   If        T02Err = *Blanks
045600051222     c                   Eval      dLat = T02Uni
045700051222e   1c                   EndIf
045800051222      * errore o non abilitato
045900051222if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
046000051222     c                   Seton                                        3990
046100051222x   1c                   Else
046200051222      *
046300051222      * Reperimento dei P.O. gestibili dall'utente
046400051222     c                   clear                   TRUL31DS
046500060201     c                   clear                   TRUL31DS2
046600051222     c                   eval      I31abi = wabi
046700051222     c                   eval      I31cdi = DUTdis
046800051222     c                   eval      I31car = DUTare
046900051222     c                   eval      I31cpo = DUTpou
047000051222     c                   call      'TRUL31R'
047100051222     c                   parm                    KPJBA
047200051222     c                   parm                    TRUL31DS
047300060130     c                   parm                    TRUL31DS2
047400051222if  2c                   if        O31pog > *zeros
047500051222     c                   movea     O31pog        fig
047600060130     c                   movea     O31arg        arg
047700051222x   2c                   else
047800051222     c                   Seton                                        3990
047900051222e   2c                   endif
048000051222
048100051222e   1c                   endif
048200051223
048300051223      * imposto i campi del video
048400051223
048500051223     c                   eval      VidImp = 'N'
048600051223     c                   eval      VidSpp = 'N'
048700051223     c                   eval      VidCrf = 'N'
048800060223     c                   eval      VidTot = 'N'
048900060130      * imposto indicatore
049000060201     c                   if        arg(1) > *zeros
049100060130     c                   seton                                        10
049200060130      * verifico se esistono + aree
049300060201     c                   eval      *in11 = (arg(2) <> *zeros)
049400060130
049500060130     c  n11              movel     arg(1)        vidare
049600060130
049700060130     c                   endif
049800051223      *
049900051223      *------------------------------------------------------------------------*
050000051223      * KLIST
050100051223      *------------------------------------------------------------------------*
050200051223
050300051223      *---- File Cnaco00f
050400051223
050500051223     c     KeyAco        Klist
050600051223     c                   Kfld                    Codut
050700051223     c                   Kfld                    DutKci
050800051223     c                   Kfld                    VidCli
050900060131
051000060131      *---- File Tabel00f
051100060131
051200060131     c     KeyTab        Klist
051300060131     c                   Kfld                    Codut
051400060131     c                   Kfld                    Keycod
051500060131     c                   Kfld                    Keykey
051600051223      *
051700051219     c                   EndSr
