000101110104     h dftactgrp(*no) actgrp(*caller)
000200000810     H DECEDIT('0,') DATEDIT(*DMY.)
000300101222      *--------------------------------------------------------------*
000400101222      *     - Controlli distinta consegna                            *
000500101221      *--------------------------------------------------------------*
000600110103     Ffnfvv01L  IF   E           K DISK
000601110103     Fazorg01l  IF   E           K DISK
000602110103     Ftntbe01l  IF   E           K DISK
000603110103     Ffidst01L  IF   E           K DISK    infds(dstds)
000700101213     Ffidst00f  uF   E             DISK    rename(FIDST000:FIDSTFIS)
000701110103     F                                     commit
001000940321      *---------------------------------------------------------------
001100030730     d* schiere
001200030730      *---------------------------------------------------------------
001300110103     D ERR             S             66    DIM(05) CTDATA PERRCD(1)             MSG ERRORE
001400101213      *---------------------------------------------------------------
001500030730     d* ds esterne
001600030730      *---------------------------------------------------------------
001700030730     D KPJBA         E DS
001800030730     D ddatiute      e ds
001900030730     D azuteds       e ds                  extname(AZUTE00F)
001901110103     D fidstds       e ds                  extname(fidst00f)
002000030730     D tibs34ds      E DS                  inz
002001110103     D fidg44ds      E DS                  inz
002100101220      * msg allocazione  inviato all'utente
002200101216     D trul82ds      E DS
002300080416     D ddstflr       E DS
002400101214     D DDGD          E DS
002600080416     D og148         E DS                  inz
002700031125      * DS per richiamo da fnlr50r x togliere bolla da distinta
002800101222     D fnlrq3ds      E DS
002801110103      *
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
005800101213     D*-------------
005900101213     d dstds           ds
006000101213     d  dst_nrr              397    400b 0
006100101222     C*-----------------------------------------------------*
006101110103     c                   exsr      Chain_Dist
006200101222      * Foglio chiuso o Annullato
006300101222     C     dstFCF        ifEQ      'S'
006400101222     C     dstATB        orNE      ' '
006600110103     c                   eval      LRQ3MSGO  = ERR(01)
006603110103     c                   exsr      srfine
007100101222     c                   endIF
007300101222     C*  Aggancio tabella 'DGD' x reperire LIMITI CONTROLLO DISTINTA
007400110103     C                   MOVEL     'DGD'         tbecod
007600110103     C                   MOVEL(p)  '1'           tbeke1
007700101222     C     KTBE          CHAIN     TNTBE01L
007800101222     C                   clear                   DDGD
007900101222     C                   IF        %Found(TNTBE01L)
008000101222     C                   MOVEL     TBEUNI        DDGD
008100101222     C                   END
008300101222      *  Limite di manutenzionabilit@ in giorni (15) della distinta
008400101222     C* VERIFICO CHE LA DATA DELLA DISTINTA NON SIA ANTECEDENTE O
008500101222     C* SEGUENTE ALLA UDATE DI OLTRE 15 GIORNI
008600101222     C* PER TUTTO IL MESE DI AGOSTO NON CONTROLLO i 15 GG
009400101222      *controllo range fra le date di oggi e quella immessa
009500101222     c                   eval      dadata = dstdfv
009600101222     c                   eval      adata  = dateu
009700101222     c                   CALL      'XSRLAV8'
009800101222     c                   PARM                    Wdata8
009900101222     c                   if        giolav > §dgdgg
010100110103     c                   eval      LRQ3MSGO  = ERR(05)
010600110103     c                   exsr      srfine
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
012600110103     c                   eval      LRQ3MSGO  = ERR(05)
013000110103     c                   exsr      srfine
013100101222     C                   END
013200101222     C     G02TGI        ADD       NRGIO         GIOTGI
013300101222     C                   CALL      'XSRGI8'
013400101222     C                   PARM                    DATPAR
013500101222     C                   Z-ADD     GIOINV        DATP15            8 0
013600101222     C     DATP15        IFLT      DATEU
013800110103     c                   eval      LRQ3MSGO  = ERR(05)
014200110103     c                   exsr      srfine
014300101222     C                   END
014301110103     c* se alloco richiedo anche la forzatura in caso di fase CUS
014302110103     c                   if        LRQ3ALCI = 'S'
014303110103     c                   exsr      srforza
014304110103     c                   end
014305110103     c                   exsr      srfine
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
026800110103     c                   eval      LRQ3MSGO  = ERR(04)
026801110103     c                   exsr      srfine
027100080416     c                   end
027600080416     c                   end
027800080416     C                   ENDSR
031800101215      *---------------------------------------------------------------*
031900101215      *   Aggancia la Distinta x allocarla ed in seguito x aggiornarla ?
032000101215      *---------------------------------------------------------------*
032100101215     C     Chain_Dist    BEGSR
033100101213     C*
033300110103     C     Kdst          CHAIN     fidst01l
033400101213     C*
033800101213     c                   if        not %Found(fidst01l)
033802110103     c                   eval      LRQ3MSGO  = ERR(02)
033803110103     c                   exsr      srfine
034000101213     c                   end
034001110103     c                   movel(p)  dstflr        ddstflr
034002110103      *  aggancio il Fisico solo x allocare il Record  ?
034003110103     c                   if        LRQ3ALCI = 'S'
034300101213     C     dst_nrr       CHAIN(e)  fidst00F
034900101213     c                   if        %Error
035300101216     c                   clear                   trul82ds
035400101222     c                   eval      ul82§rrn = dst_nrr
035500101222     c                   eval      ul82§fil = 'FIDST00F'
035600101222     c                   eval      ul82§win = 'S'
035700101222     c                   eval      ul82§f7  = 'S'
035800101222     c                   eval      ul82§num = 2
035900101222     c                   eval      ul82§att = 2
036000101222     c                   eval      ul82§mss = 'Si sta bloccando la Distinta: '
036100110103     c                             + %editc(dstnfv:'Z') +
036200101216     c                             ' SI PREGA DI USCIRE dal lavoro|'
036300101222     c                   Eval      UL82§msw = 'La Distinta '
036400110103     c                             + %editc(dstnfv:'Z') +
036401110103     c                             ' è già utilizzata da -
036500101222     c                              un altro lavoro e non è manutenzionabile'
036700101216      * chiamo il pgm che manda il messaggio info all'utente
036800101216     c                   call(e)   'TRUL82R'
036900101216     c                   parm                    trul82ds
037100101222     c                   if        ul82§sts = 'A'
037102110103     c                   eval      LRQ3MSGO  = ERR(03)
037103110103     c                   exsr      srfine
037300101216     c                   endif
037500101216     c                   end
038401110103     c                   endIF
038402110103      * controllo anche il file FNFVV
038403110103     C                   Z-ADD     dstnpg        FVVNPG
038404110103     C                   Z-ADD     dstnfv        FVVNFV
038405110103     C                   Z-ADD     dstfgs        FVVFGS
038406110103     C     KFVV          CHAIN     FNFVV01L
038409110103     c                   if        not %found(fnfvv01l)   or
038410110103     c                             fvvatb = '*'           or
038411110103     c                             fvvfcf <> *blanks
038414110103     c                   eval      LRQ3MSGO  = ERR(01)
038415110103     c                   exsr      srfine
038416110103     c                   end
038500110103     C*
038600110103     C                   ENDSR
038601110103     C*-----------------------------------------------------*
038602110103     C*    Operazioni finali
038603110103     C*-----------------------------------------------------*
038604110103     C     srfine        BEGSR
038605110103     c                   if        LRQ3MSGO <> ' '
038606110103     c                   eval      LRQ3errO = '1'
038608110103     c                   else
038609110103     c                   if        %Found(fidst00F) and LRQ3alci = 'S'
038610110103      *    qui l'Update serve esclusivamente per tenere allocato  ?
038611110103      *    il record in quanto nel CLLE x il COMMIT è    ?
038612110103      *    stato attivato lo STRCMTCTL con *CHG.    ?
038613110103     c                   update    fidstFIS
038614110103     c                   endIF
038615110103     c                   endIF
038616110103     c                   movel     fnlrq3ds      kpjbu
038617110103     c                   seton                                        LR
038618110104     c                   return
038619110103     C                   ENDSR
038620110103     C*-----------------------------------------------------*
038621110103     C*    Operazioni iniziali
038622110103     C*-----------------------------------------------------*
038623110103     C     *INZSR        BEGSR
038624110103     C*
038625110103     C     *ENTRY        PLIST
038626110103     C                   PARM                    KPJBA
038627110103     C                   PARM                    fidstds
038628110103     c                   movel     kpjbu         fnlrq3ds
038629110103     c                   clear                   LRQ3errO
038630110103     c                   clear                   LRQ3MSGO
038631110103     c                   clear                   fidstds
038632110103     C* Inverto la data del giorno
038633110103     C                   TIME                    WHHDAT           14 0
038634110103     C                   MOVEL     WHHDAT        HHMMss            6 0          *ORA/MINUTI/sec
038635110103     C                   MOVE      WHHDAT        G02DAT
038636110103     C                   MOVE      *BLANKS       G02ERR
038637110103     C                   CALL      'XSRDA8'
038638110103     C                   PARM                    WLBDA8
038639110103     C                   Z-ADD     G02INV        DATEU             8 0
038640110103     c     *dtaara       define    §azute        azuteds
038641110103     c     *dtaara       define    §datiute      ddatiute
038642110103     C                   in(E)     *dtaara
038643110103     c*
038644110103     C                   IF        %Error  or  RSUT = *blanks
038645110103     C                   call      'TIBS34R'
038646110103     C                   parm                    Tibs34Ds
038647110103     C                   in        *dtaara
038648110103     c                   ENDIF
038649110103     c*
038650110103     C     dutpou        CHAIN     AZORG01l
038651110103    1C                   IF        %found(azorg01l)
038652110103     c                   movel     orgde8        og148
038653110103     c                   end
038654110103      * CHIAVI                                                        *
038655110103     C     KTBE          KLIST
038656110103     C                   KFLD                    tbecod
038657110103     C                   KFLD                    tbeke1
038658110103     C     Kdst          KLIST
038659110103     C                   KFLD                    LRQ3npgI
038660110103     C                   KFLD                    LRQ3ndcI
038661110103     C                   KFLD                    LRQ3FGSI
038662110103     C     KFVV          KLIST
038663110103     C                   KFLD                    FVVNPG
038664110103     C                   KFLD                    FVVNFV
038665110103     C                   KFLD                    FVVFGS
038666110103     C*
038667110103     C                   ENDSR
038700101213**  err
038800101214Distinta non accessibile. Chiusa o Annullata                       01
038900110103Distinta non trovata                                               02
039300110103Distinta non accessibile. Allocata da altro lavoro.                03
039400110103Forzatura non eseguita                                             04
039700110103Data distinta superiore alla data odierna dei gg.ammessi           05
