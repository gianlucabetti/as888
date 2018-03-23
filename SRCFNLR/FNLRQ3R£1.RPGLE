000100101213     H*PARMS DFTACTGRP(*CALLER)
000200000810     H DECEDIT('0,') DATEDIT(*DMY.)
000300101222      *--------------------------------------------------------------*
000400101222      *     - Controlli distinta consegna                            *
000500101221      *--------------------------------------------------------------*
000600101213     Ffidst01L  IF   E           K DISK    rename(FIDST000:FIDST01)
000700101213     Ffidst00f  uF   E             DISK    rename(FIDST000:FIDSTFIS)
000800101223     F                                     infds(dstds) commit
000900080416     Ffidst09L  IF   E           K DISK
001000940321      *---------------------------------------------------------------
001100030730     d* schiere
001200030730      *---------------------------------------------------------------
001300101221     D ERR             S             66    DIM(50) CTDATA PERRCD(1)             MSG ERRORE
001400101213      *---------------------------------------------------------------
001500030730     d* ds esterne
001600030730      *---------------------------------------------------------------
001700030730     D KPJBA         E DS
001800030730     D ddatiute      e ds
001900030730     D azuteds       e ds                  extname(AZUTE00F)
002000030730     D tibs34ds      E DS                  inz
002100101220      * msg allocazione  inviato all'utente
002200101216     D trul82ds      E DS
002300080416     D ddstflr       E DS
002400101214     D DDGD          E DS
002500070316     D og146         E DS                  inz
002600080416     D og148         E DS                  inz
002700031125      * DS per richiamo da fnlr50r x togliere bolla da distinta
002800101222     D fnlrq3ds      E DS
002900101214     D WLBDA8          DS
003000101214     D  G02DAT                 1      8  0
003100101214     D  G02INV                 9     16  0
003200101214     D  G02ERR                17     17
003300101214     D  G02TGI                18     22  0
003400101214      *
003500101214     d Wdata8          DS
003600101214     d  dadata                 1      8  0
003700101214     d  adata                  9     16  0
003800101214     d  GioLav                17     21  0
003900101214      *
004000101214     D DATPAR          DS
004100101214     D  GIODAT                 1      8  0
004200101214     D  GIOINV                 9     16  0
004300101214     D  GIOTGI                17     21  0
004400030730     d*---------------------------------------------------------------*
004500030730     d* campi di comodo                                               *
004600030730     d*---------------------------------------------------------------*
004700030730     d dtaeur          s               d   datfmt(*eur)
004800030730     d dtaiso          s               d
004900101214     d dtaiso_oggi     s               d   datfmt(*iso)
005000101214     d dtaiso_dst      s               d   datfmt(*iso)
005100101213      *-----
005200101213     D Found_Dist      S              1    INZ('N')
005300101213     D Error_Dist      S              1    INZ('N')
005400101213     D Rec_allocato    S              1    INZ('N')
005500101213     d wNFV            s                   like(dstNFV)
005600101213     d wNPG            s                   like(dstNPG)
005700101213     d wFGS            s                   like(dstFGS)
005800101213     D*-------------
005900101213     d dstds           ds
006000101213     d  dst_nrr              397    400b 0
006100101222     C*-----------------------------------------------------*
006200101222      * Foglio chiuso o Annullato
006300101222     C     dstFCF        ifEQ      'S'
006400101222     C     dstATB        orNE      ' '
006500101222     c                   eval      O98TIPERR = '01'
006600101222     c                   eval      O98msgERR = ERR(01)
006700101222     c                   eval      O98msgER2 = 'Nr.Dist.:'
006800101222     c                                        + %EDITC(dstNFV:'Z')
006900101222     c                   seton                                        90
007000101222     c                   leaveSR
007100101222     c                   endIF
007200101222     C*
007300101222     C*  Aggancio tabella 'DGD' x reperire LIMITI CONTROLLO DISTINTA
007400101222     C                   MOVEL     'DGD'         KCOD1
007500101222     C                   MOVEL     *BLANKS       KKEY1
007600101222     C                   MOVEL     '1'           KKEY1
007700101222     C     KTBE          CHAIN     TNTBE01L
007800101222     C                   clear                   DDGD
007900101222     C                   IF        %Found(TNTBE01L)
008000101222     C                   MOVEL     TBEUNI        DDGD
008100101222     C                   END
008200101222      *
008300101222      *  Limite di manutenzionabilit@ in giorni (15) della distinta
008400101222     C* VERIFICO CHE LA DATA DELLA DISTINTA NON SIA ANTECEDENTE O
008500101222     C* SEGUENTE ALLA UDATE DI OLTRE 15 GIORNI
008600101222     C* PER TUTTO IL MESE DI AGOSTO NON CONTROLLO i 15 GG
008700101222      *
008800101222     C                   move      *date         dtaeur
008900101222     C                   move      dtaeur        dtaiso_oggi
009000101222      *----
009100101222     C                   move      dstDFV        dtaeur
009200101222     C                   move      dtaeur        dtaiso_dst
009300101222      * ---------------
009400101222      *controllo range fra le date di oggi e quella immessa
009500101222     c                   eval      dadata = dstdfv
009600101222     c                   eval      adata  = dateu
009700101222     c                   CALL      'XSRLAV8'
009800101222     c                   PARM                    Wdata8
009900101222     c                   if        giolav > §dgdgg
010000101222     c                   eval      O98TIPERR = '10'
010100101222     c                   eval      O98msgERR = ERR(10)
010200101222     c                   eval      O98msgER2 = 'Sono trascorsi '
010300101222     c                             + %EDITC(giolav:'Z') + ' giorni. Il max. son-
010400101222     c                             o' + %EDITC(§dgdgg:'Z')
010500101222     c                   seton                                        90
010600101222     c                   leaveSR
010700101222     c                   end
010800101222     C*---- controlla quanti giorni sono trascorsi.
010900101222     C                   MOVE      DATEU         WMGUDT            4 0
011000101222     C                   MOVEL     WMGUDT        WMUDT             2 0
011100101222     C                   Z-ADD     §DGDGG        NRGIO             3 0
011200101222     C     WMGUDT        IFGE      §DGDEI
011300101222     C     WMGUDT        ANDLE     §DGDEF
011400101222     C                   Z-ADD     §DGDGE        NRGIO
011500101222     C                   END
011600101222     C     DATEU         IFGE      §DGDII
011700101222     C     DATEU         ANDLE     §DGDIF
011800101222     C                   Z-ADD     §DGDGI        NRGIO
011900101222     C                   END
012000101222     C     G02TGI        SUB       NRGIO         GIOTGI
012100101222     C                   CALL      'XSRGI8'
012200101222     C                   PARM                    DATPAR
012300101222     C                   Z-ADD     GIOINV        DATM15            8 0
012400101222     C     DATM15        IFGT      DATEU
012500101222     c                   eval      O98TIPERR = '10'
012600101222     c                   eval      O98msgERR = ERR(10)
012700101222     c                   eval      O98msgER2 = 'Data :'
012800101222     c                             + %EDITC(DATm15:'Y')
012900101222     c                   seton                                        90
013000101222     c                   leaveSR
013100101222     C                   END
013200101222     C     G02TGI        ADD       NRGIO         GIOTGI
013300101222     C                   CALL      'XSRGI8'
013400101222     C                   PARM                    DATPAR
013500101222     C                   Z-ADD     GIOINV        DATP15            8 0
013600101222     C     DATP15        IFLT      DATEU
013700101222     c                   eval      O98TIPERR = '10'
013800101222     c                   eval      O98msgERR = ERR(10)
013900101222     c                   eval      O98msgER2 = 'Data :'
014000101222     c                             + %EDITC(DATp15:'Y')
014100101222     c                   seton                                        90
014200101222     c                   leaveSR
014300101222     C                   END
014400101222     C*-----------------------------------------------------*
014500101222     C*    Operazioni iniziali
014600101222     C*-----------------------------------------------------*
014700101222     C     *INZSR        BEGSR
014800101222     C*
014900101222     C     *ENTRY        PLIST
015000101222     C                   PARM                    KPJBA
015100101222     c                   seton                                        LR
015200101222     C* Inverto la data del giorno
015300101222     C                   TIME                    WHHDAT           14 0
015400101222     C                   MOVEL     WHHDAT        HHMMss            6 0          *ORA/MINUTI/sec
015500101222     C                   MOVE      WHHDAT        G02DAT
015600101222     C                   MOVE      *BLANKS       G02ERR
015700101222     C                   CALL      'XSRDA8'
015800101222     C                   PARM                    WLBDA8
015900101222     C                   Z-ADD     G02INV        DATEU             8 0
016000101222     c     *dtaara       define    §azute        azuteds
016100101222     c     *dtaara       define    §datiute      ddatiute
016200101222     C                   in(E)     *dtaara
016300101222     c*
016400101222     C                   IF        %Error  or  RSUT = *blanks
016500101222     C                   call      'TIBS34R'
016600101222     C                   parm                    Tibs34Ds
016700101222     C                   in        *dtaara
016800101222     c                   ENDIF
016900101222     C                   move      *date         dtaeur
017000101222     C                   move      dtaeur        dtaiso
017100101222     c                   move      dtaiso        oggin             8 0
017200101222     c                   move      oggin         oggi              8
017300101222     c*
017400101222     C     dutpou        CHAIN     AZORG01l
017500101222    1C                   IF        %found(azorg01l)
017600101222     C                   MOVEL     ORGde6        og146
017700101222     c                   movel     orgde8        og148
017800101222     c                   end
017900101222      * se c'è una data in organigramma
018000101222     c                   move      §ogdda        data_org          8 0
018100101222     C                   MOVE      *BLANK        test_Parallelo    1
018200101222      *---------------------------*
018300101222      * CHIAVI                                                        *
018400101222      *---------------------------*
018500101222     C     Kdst01        KLIST
018600101222     C                   KFLD                    wnpg
018700101222     C                   KFLD                    Wnfv
018800101222     C                   KFLD                    wfgs
018900101222     C*
019000101222     C     KTBE          KLIST
019100101222     C                   KFLD                    KCOD1
019200101222     C                   KFLD                    KKEY1
019300101222     C*
019400101222     C     Kapd          KLIST
019500101222     C                   KFLD                    apdtip
019600101222     C                   KFLD                    v03pdr
019700101222     c                   movel     'A'           apdtip
019800101222     C     KTAB          KLIST
019900101222     C                   KFLD                    tblkut
020000101222     C                   KFLD                    TBLCOD
020100101222     C                   KFLD                    TBLKEY
020200101222     C     KARB          KLIST
020300101222     C                   KFLD                    ARBAAS
020400101222     C                   KFLD                    ARBLNP
020500101222     C                   KFLD                    ARBNRS
020600101222     C                   KFLD                    ARBNSP
020700101222     C     Kdst          KLIST
020800101222     C                   KFLD                    dstfgs
020900101222     C                   KFLD                    dstnpg
021000101222     C                   KFLD                    dstnfv
021100101222     C     KLBL          KLIST
021200101222     C                   KFLD                    LBLAAP
021300101222     C                   KFLD                    LBLLPP
021400101222     C                   KFLD                    LBLNRP
021500101222     C                   KFLD                    LBLNSP
021600101222     c                   eval      dstnpg = 4
021700101222     C     KFVV          KLIST
021800101222     C                   KFLD                    FVVNPG
021900101222     C                   KFLD                    FVVNFV
022000101222     C                   KFLD                    FVVFGS
022100101222     C     KEVB          KLIST
022200101222     C                   KFLD                    KAAS
022300101222     C                   KFLD                    KLNP
022400101222     C                   KFLD                    KNRS
022500101222     C                   KFLD                    KNSP
022600101222     C                   KFLD                    KCEV
022700101222     C*
022800101222     C     KANM          KLIST
022900101222     C                   KFLD                    WAA4
023000101222     C                   KFLD                    ARBLNP
023100101222     C                   KFLD                    ARBNRS
023200101222     C                   KFLD                    ARBNSP
023300101222     C                   KFLD                    §§§CAA            3 0
023400101222     C*
023500101222     C     *LIKE         DEFINE    ARBIFP        KFGS
023600101222     C     *LIKE         DEFINE    ARBNDC        KNDC
023700101222     C     *LIKE         DEFINE    ARBDDC        KDDC
023800101222     C     *LIKE         DEFINE    EVBAAS        KAAS
023900101222     C     *LIKE         DEFINE    EVBLNP        KLNP
024000101222     C     *LIKE         DEFINE    EVBNRS        KNRS
024100101222     C     *LIKE         DEFINE    EVBNSP        KNSP
024200101222     C     *LIKE         DEFINE    EVBCEV        KCEV
024300101222     C     *LIKE         DEFINE    TBECOD        KCOD1
024400101222     C     *LIKE         DEFINE    TBEKE1        KKEY1
024500101222     C*
024600101222     C                   ENDSR
024700101214     C*-----------------------------------------------------*
024800101214     C* pgm per forzare modifica distinta fase CUS          *
024900101214     C*-----------------------------------------------------*
025000101214     C     srforza       BEGSR
025100101213     C* chiamo il pgm se distinta pda
025200080416     c                   if        (dstpda = 'E' or dstpda ='C') or
025300101222     c                             (§DSTTSTPDA='E' or §DSTTSTPDA='C')
025400080416     c                   clear                   fidg44ds
025500080416     c                   eval      co44fgs=dstfgs
025600080416     c                   eval      co44ndc=dstnfv
025700080416     c                   eval      co44wnd = 'S'
025800080416     c                   eval      co44td = 'E'
025900080416     c                   movel(p)  fidg44ds      kpjbu
026000080416     c                   call      'FIDG44R'
026100080416     c                   parm                    kpjba
026200080416     c                   movel     kpjbu         fidg44ds
026300080416     c* se mi ritorna che non ha forzato accendo indicatore d'errore
026400080416     c                   if        co44fcus = 'N'
026500080416     c                   seton                                        90
026600101214      * tipo di errore se richiamato
026700101214     c                   eval      O98TIPERR = '07'
026800101214     c                   eval      O98msgERR = ERR(07)
026900101217     c                   eval      O98msgER2 = 'il FIDG44R x il controllo fase -
027000101217     c                             CUS ha risposto in errore'
027100080416     c                   end
027200080416     c* se forzato proseguo con l'aggiornamento
027300080416     c                   if        co44fcus = 'S'
027400080416     c                   eval      *inkf ='1'
027500080416     c                   end
027600080416     c                   end
027700080416     C*
027800080416     C                   ENDSR
027900101214      *
028000030730     C                   Z-ADD     4             FVVNPG
028100030730     C                   Z-ADD     arbndc        FVVNFV
028200030730     C                   Z-ADD     arbifp        FVVFGS
028300030730     C     KFVV          CHAIN     FNFVV01L
028400101220      *
028500101222      *  Se x qualche motivo il foglio non è più abilitato
028600101220     c                   if        not %found(fnfvv01l)   or
028700101220     c                             fvvatb = '*'           or
028800101220     c                             fvvfcf = *blanks
028900101220      *
029000101222     c* se non è disponibile il foglio
029100101220     c                   z-add     0             v03ifp
029200101220     c                   z-add     0             v03ndc
029300101220     c                   z-add     0             v03ddc
029400101220     c                   z-add     0             v03pdr
029500101220     c                   clear                   v03rsc
029600101220     c                   clear                   v03fpp
029700101220     c                   seton                                        0690
029800101220      * Distinta in questo momento prima dell'aggiornamento non accessibile
029900101220     c                   eval      O98TIPERR = '01'
030000101220     c                   eval      O98msgERR = ERR(01)
030100101220     c                   eval      O98msgER2 = 'NON aggiornata Distinta:'
030200101220     c                                 + %EDITC(fvvnfv:'Z')
030300101220     c                   leavesr
030400101220     c                   end
030500101220      **  CONTROLLO IN GENERALE SE DISTINTA E' ALLOCATA.
030600101222     c                   if        I98ALCDST <> ' '
030700101220      **  allocazione distinta
030800101220     c                   exsr      CHAIN_Dist
030900101220     c                   if        Rec_Allocato = 'S'
031000101220     c                   eval      O98TIPERR = '06'
031100101220     c                   eval      O98msgERR = ERR(06)
031200101220     c                   return
031300101220     c                   end
031400101220      **
031500101220     c                   end
031600030730     c*
031700030730     c                   endsr
031800101215      *---------------------------------------------------------------*
031900101215      *   Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
032000101215      *---------------------------------------------------------------*
032100101215     C     Chain_Dist    BEGSR
032200101215     C*
032300101213      * inizializza il trovato/non trovato
032400101213     c                   eval      Found_Dist = 'N'
032500101213     c                   eval      Error_Dist = 'N'
032600101213     c                   eval      rec_allocato = 'N'
032700101213      * Categoria Foglio x controllo nr.distinta
032800101213     C                   z-add     4             wNPG
032900101221     c                   move      n98FGS        wfgs
033000101221     c                   move      n98NDC        wNFV
033100101213     C*
033200101215      *    Dalla Vista logica prende il NrelRec  ?
033300101214     C     Kdst01        CHAIN     fidst01l
033400101213     C*
033500101215      *  Se non trovava il record allocava l'ultima distinta che aveva
033600101222      *  trovato poichè il nrelrec. non si resettava ma era impostato
033700101215      *  da prima.
033800101213     c                   if        not %Found(fidst01l)
033900101213     c                   eval      dst_nrr = *hival
034000101213     c                   end
034100101213     C*
034200101215      *    x agganciare il Fisico solo x allocare il Record  ?
034300101213     C     dst_nrr       CHAIN(e)  fidst00F
034400101222      *  è stata usata questa tecnica perchè l'aggiornamento del FIDST
034500101222      *  è fatto mediante il FIDG37R e,sotto COMIT,xchè tutto funzioni
034600101215      *  correttamente,il file deve essere condiviso in SHARE altrimenti
034700101222      *  non funziona. (Si è scelto di utilizzare x tutti il fisico)
034800101213     c*
034900101213     c                   if        %Error
035000101213     c                   eval      Error_Dist = 'S'
035100101222      * se il record è allocato mando messaggio info all'utente che alloca il rcd
035200101216     c*
035300101216     c                   clear                   trul82ds
035400101222     c                   eval      ul82§rrn = dst_nrr
035500101222     c                   eval      ul82§fil = 'FIDST00F'
035600101222     c                   eval      ul82§win = 'S'
035700101222     c                   eval      ul82§f7  = 'S'
035800101222     c                   eval      ul82§num = 2
035900101222     c                   eval      ul82§att = 2
036000101222     c                   eval      ul82§mss = 'Si sta bloccando la Distinta: '
036100101216     c                             + %editc(Wnfv:'Z') +
036200101216     c                             ' SI PREGA DI USCIRE dal lavoro|'
036300101222     c                   Eval      UL82§msw = 'La Distinta '
036400101222     c                             + %editc(Wnfv:'Z') +  ' è gi@ utilizzata da -
036500101222     c                              un altro lavoro e non è manutenzionabile'
036600101216      *
036700101216      * chiamo il pgm che manda il messaggio info all'utente
036800101216     c                   call(e)   'TRUL82R'
036900101216     c                   parm                    trul82ds
037000101216      *
037100101222     c                   if        ul82§sts = 'A'
037200101214     c                   eval      Rec_Allocato = 'S'
037300101216     c                   endif
037400101216     C*
037500101216     c                   end
037600101213      * Trovato record
037700101213     c                   if        %Found(fidst00F)
037800101213     c                   eval      Found_Dist = 'S'
037900101216     C*
038000101215      *    qui l'Update serve esclusivamente per tenere allocato  ?
038100101222      *    il record in quanto nel CLLE x il COMMIT è    ?
038200101215      *    stato attivato lo STRCMTCTL con *CHG.    ?
038300101214     c                   update    fidstFIS
038400101213     c                   endIF
038500101213     C*
038600101213     C     ENDChain      ENDSR
038700101213**  err
038800101214Distinta non accessibile. Chiusa o Annullata                       01
038900101214Distinta non accessibile. Non usa il PDA.                          02
039000101214Data dist.anteriore o super.alla data odierna dei gg.ammessi       03
039100101221Linea arrivo spedizione non in gestione.                           04
039200101221Spedizione Inesistente. Non trovata in Arrivo.                     05
039300101213Distinta non accessibile. Allocata da altro lavoro.                06
039400101217Aggiornamento Distinta NON eseguito correttamente.                 07
039500101222Sped. impegnata da altro lavoro non è possibile Proseguire.        08
039600101214Parametri passati non in formato numerico o mancanti               09
039700101214Data distinta superiore alla data odierna dei gg.ammessi           10
039800101222La Distinta non è la stessa presente sulla Bolla                   11
039900101215Aggiornamento fallito. Bolla utilizzata da un altro lavoro.        12
040000101222La Bolla è già assegnata ad una altra Distinta                     13
040100101221Parametro Filiale non in formato numerico o mancante               14
040200101221Parametro Nr.Distinta non in formato numerico o mancante           15
040300101221Parametro Anno Spediz.non in formato numerico o mancante           16
040400101221Parametro LNP  Spediz.non in formato numerico o mancante           17
040500101221Parametro Num. Spediz.non in formato numerico o mancante           18
040600101222La Spedizione non è in distinta. Non è possibile toglierla.        19
