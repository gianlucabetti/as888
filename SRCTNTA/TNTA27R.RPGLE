000100080410      *===============================================================*
000200080410      * TNTA27R    * Invio E-mail ad assicurazione dei mandati variati*
000300080410      *              nel giorno richiesto a video                     *
000400080410      *===============================================================*
000500080410     h decedit('0,') datedit(*DMY.)
000600080410     h option(*nodebugio)
000700011015
000800011015     FTITPT01L  IP   E           K DISK
000900011015     FTABEL00F  IF   E           K DISK
001000080410     fTNTBE01l  if   e           k disk
001100080410     FTNTA27P   O    E             PRINTER oflind(*IN01) usropn
001200011015
001300011015      *****************************************************************
001400011015
001500011015      *   S C H I E R E
001600011015
001700011016     D*
001800080410     D CMM             S             70    DIM(1) CTDATA PERRCD(1)
001900011016     D* Max 99 codici consegna particolari !!!
002000011016     D FTC             s                   dim(99) like(TPTftc)
002100011015
002200011015      *   D S   I N T E R N E / E S T E R N E
002300011015
002400080409     D Parm27          DS
002500080409     D  ParDay                 1      8  0
002600011016
002700011016     D G02DS           DS
002800011016     D  G02DAT                 1      8  0
002900011016     D  G02INV                 9     16  0
003000011016     D  G02ERR                17     17
003100011016     D  G02TGI                18     22  0
003200080527
003300080527     d Wdata8          DS
003400080527     d  dadata                 1      8  0
003500080527     d  adata                  9     16  0
003600080527     d  GioLav                17     21  0
003700011015
003800011015     D KPJBA         E DS
003900080410
004000080410     d Azuteds       e ds                  Extname(Azute00f)
004100080410     d dDatiute      e ds
004200080410     d Tibs34ds      e ds                  Inz
004300080410
004400011016     D DS1P          E DS
004500011016     D DSTR          E DS
004600011016     D DSTA01        E DS
004700080410
004800011017     D TIBS69DS      E DS                  INZ
004900011017     D DS_cnaco      E DS                  extname(CNACO00F) INZ
005000011017     D DS_cnind      E DS                  extname(CNIND00F) INZ
005100011017     D DS_cnclp      E DS                  extname(CNCLP00F) INZ
005200011017     D DS_fncls      E DS                  extname(FNCLS00F) INZ
005300080410
005400080410     d dmradan       e ds
005500080410     d trtcm1ds      e ds
005600080410
005700011015      *   V A R I A B I L I
005800011015
005900080527     d w0140           s             14  0
006000080527     d wdata           s              8  0
006100080527     d dataiso         s               d   datfmt(*iso)
006200080527     d dataiso1        s               d   datfmt(*iso)
006300011016     D Ok_Rec          S              1
006400011015     D X               S              3  0
006500080410     D svcom           s             80
006600080410     D lungm           S             15  5 inz(80)
006700011015
006800080410     D*
006900080527     D Cosobj          C                   CONST('Elenco mandati Bart-
007000080527     D                                     olini')
007100011015      *****************************************************************
007200011016      * RIEPILOGO INDICATORI
007300011016      *****************************************************************
007400011016      * 01    - OVERFLOW
007500011016      * 30    - DI COMODO
007600011016      *****************************************************************
007700011015
007800011016     C                   exsr      Sr_Seleziona
007900011016      *
008000011016IF  1C                   if        Ok_Rec = *on
008100011016      *
008200011015     C                   clear                   DETTA
008300011015      *
008400011015     C                   exsr      Sr_Decodifica
008500011015      * Testata
008600011016      *
008700011016E   1C                   endif
008800011016      *
008900011017     CLR                 exsr      Sr_Fine
009000011016
009100011016      *---------------------------------------------------------------*
009200011016      * SELEZIONA
009300011016      *---------------------------------------------------------------*
009400011016     C     Sr_Seleziona  BEGSR
009500011016
009600011016     C                   eval      Ok_Rec =  *on
009700080410
009800011016S   1C                   select
009900011016    1C                   when      TPTatb = 'A'
010000011016     C                   eval      Ok_Rec =  *off
010100080409    1C                   when      TPTduv <> PARday
010200011016     C                   eval      Ok_Rec =  *off
010300011016X   1C                   other
010400011016     C     TPTftc        lookup    FTC                                    30
010500011016IF  2C                   if        not *in30
010600011016     C                   eval      Ok_Rec =  *off
010700011016E   2C                   endif
010800011016E   1C                   endsl
010900011016
011000011017     C                   ENDSR
011100011016
011200011015
011300011015      *---------------------------------------------------------------*
011400011015      * DECODIFICA
011500011015      *---------------------------------------------------------------*
011600011015     C     Sr_Decodifica BEGSR
011700011015
011800080409     C* recupero anagrafica clienti
011900011016
012000011015     C* Cliente
012100011017     C                   z-add     TPTksc        DKSC
012200011017IF  1C                   if        TPTksc <> I69kac
012300011017     C                   clear                   DS_cnaco
012400011017     C                   reset                   TIBS69DS
012500011017     C                   z-add     TPTksc        I69kac
012600080410     C                   z-add     TPTksc        I69kin
012700011017     C                   call      'TIBS69R'
012800011017     C                   parm                    tibs69DS
012900011017     C                   parm                    DS_cnaco
013000011017     C                   parm                    DS_cnind
013100011017     C                   parm                    DS_cnclp
013200011017     C                   parm                    DS_fncls
013300011017IF  2C                   if        O69err <> *on
013400011015     C                   movel     ACOrag        DKSCD
013500080410     C                   movel     indvia        DKSCi
013600080410     C                   movel     indcit        DKSCl
013700080410     C                   movel     indprv        DKSCp
013800011016E   2C                   else
013900011016     C                   movel     *all'? '      DKSCD
014000011016E   2C                   endif
014100080409      * stampa solo per cliente diverso
014200080409IF  2C                   if        *in01
014300080409     C                   write     testa1
014400080409     C                   write     testa2
014500080409     C                   eval      *in01 = *off
014600080409E   2C                   endif
014700080409      * Dettaglio
014800080409     C                   write     detta                                01
014900011016E   1C                   endif
015000011015
015100011015
015200011017     C                   ENDSR
015300011016
015400011015      *---------------------------------------------------------------*
015500011015      * ROUTINE INIZIALE
015600011015      *---------------------------------------------------------------*
015700011015     C     *INZSR        BEGSR
015800011015
015900011015     C     *ENTRY        PLIST
016000011015     C                   PARM                    KPJBA
016100080409     C                   movel     kpjbu         Parm27
016200011015      *
016300080410     C                   Z-ADD     1             CODUT             1 0
016400080410      *
016500080410     c     *dtaara       define    §azute        azuteds
016600080410     c     *dtaara       define    §datiute      ddatiute
016700080410     c                   in(E)     *dtaara
016800080410     c                   If        %error  or RSUT = *blanks
016900080410     c                   CLEAR                   tibs34ds
017000080410     c                   CALL      'TIBS34R'
017100080410     c                   PARM                    tibs34ds
017200080410     c                   in        *dtaara
017300080410     c                   EndIf
017400080527
017500080527     c                   If        Kpjbu  = *blanks
017600080527
017700080527      * Se è uguale a zero prendo la prima data lavorativa inferiore ad oggi
017800080527      * Reperisco data corrente
017900080527     c                   time                    w0140
018000080527     c                   move      w0140         wData
018100080527
018200080527     c                   clear                   g02ds
018300080527     c                   eval      G02dat = wData
018400080527     c                   call      'XSRDA8'
018500080527     c                   parm                    g02ds
018600080527     c                   move      G02inv        Dataiso
018700080527
018800080527     c                   do        *hival
018900080527      * elaboro il giorno precedente se non festivo
019000080527     c     dataiso       subdur    1:*d          dataiso1
019100080527      * controllo se udate è festivo non lancio l'elaborazione
019200080527     c                   move      dataiso1      dadata
019300080527     c                   move      dataiso1      adata
019400080527     c                   CALL      'XSRLAV8'
019500080527     c                   PARM                    Wdata8
019600080527     c                   if        giolav = 0
019700080527     c                   eval      dataiso = dataiso1
019800080527     c                   iter
019900080527     c                   else
020000080527     c                   move      dataiso1      ParDay
020100080527     c                   leave
020200080527     c                   endif
020300080527     c                   enddo
020400080527     c                   endif
020500080527      *
020600080527IF  1C                   if        ParDay <> 0
020700080527     C                   clear                   G02DS
020800080527     C                   z-add     ParDay        G02INV
020900080527     C                   movel     '3'           G02ERR
021000080527     C                   call      'XSRDA8'
021100080527     C                   parm                    G02DS
021200080527     C                   z-add     G02DAT        TDTday
021300080527     C                   endif
021400011015      *
021500011015     C                   movel     *date         WDATE
021600011016      *
021700011016      * intabello le tariffe particolari da considerare
021800011016      * e stampo i parametri (comprese tali tariffe)
021900080410     c* eseguo ovrprtf  sul file di spool e stampo l'intestazione
022000080410
022100080410     c                   exsr      ovrmail
022200011016     C                   write     testa1
022300080410     C                   write     testa2
022400011016     C                   movel     *off          *in01
022500011016     C                   clear                   X
022600011016     C                   clear                   FTC
022700011016     C                   movel     '1P'          kcod
022800011016     C     ktab2         setll     TABEL00F
022900011016     C     ktab2         reade     TABEL00F
023000011016     C*
023100011016IF  1C                   dow       not %eof(tabel00f)
023200011016     C                   movel     TBLuni        DS1P
023300011016     C*
023400011016IF  2C                   if        §1Ptps = 'S'
023500011016     C                   add       1             X
023600011016     C                   movel     TBLkey        FTC(X)
023700011016     C*
023800011016     C*
023900011016IF  3C                   if        X = 99
024000011016     C                   leave
024100011016E   3C                   endif
024200011016     C*
024300011016E   2C                   endif
024400011016     C     ktab2         reade     TABEL00F
024500011016     C*
024600011016E   1C                   enddo
024700011015
024800011016     C* file TABEL00F
024900011016     C     ktab          KLIST
025000011016     C                   KFLD                    codut
025100011016     C                   KFLD                    kcod
025200011016     C                   KFLD                    kkey
025300011016     C     ktab2         KLIST
025400011016     C                   KFLD                    codut
025500011016     C                   KFLD                    kcod
025600080410     c*
025700080410     C     ktbe          klist
025800080410     C                   kfld                    tbecod
025900080410     C                   kfld                    tbeke1
026000080410     C                   kfld                    tbeke2
026100011016
026200011016     C     *like         define    TBLcod        kcod
026300011016     C     *like         define    TBLkey        kkey
026400011015
026500011017     C                   ENDSR
026600080410      *************************************************************************
026700080410      * ROUTINE PER CREARE SPOOL PER INVIO e-mail
026800080410      *************************************************************************
026900080410     C     OVRmail       BEGSR
027000080410     c
027100080410     c* Reperisco dati in tabella mra per OVRTPRTF
027200080410     c                   move      'MRA'         tbecod
027300080410     c                   movel(p)  'TNTA27R'     tbeke1
027400080410     c                   move      *blank        tbeke2
027500080410     c                   clear                   dmradan
027600080410     c                   clear                   trtcm1ds
027700080410     c     ktbe          chain     tntbe01l
027800080410     c                   if        %found
027900080410     c                   movel     tbeuni        dmradan
028000080410     c                   eval      §cm1dst = §mraddest
028100080410     c                   move      dutpou        §cm1po
028200080410     c                   move      '0'           §cm1sts
028300080410     c                   movel     §mradreg      §cm1tips
028400080410     c                   movel     §mradidpro    §cm1idp
028500080410     c                   eval      §cm1mitt = §mradmitt
028600080410     c                   eval      §cm1var= '*OBJM*' + cosobj
028700080410     c                   endif
028800080410      *
028900080410      * ESEGUO OVRPRTF PER CREARE UNO SPOOL PER LA LETTERA DA INVIARE
029000080410      *   TRAMITE E-mail all'assicurazione
029100080410      *
029200080410     c*
029300080410     c* eseguo ovprtf
029400080410     c                   eval      %subst(cmm: 23: 10) = §MRADOUTQI
029500080410     c                   movea     cmm(1)        svcom
029600080410     c                   eval      cmmm = %trim(svcom) + trtcm1ds
029700080410     c                             + ''')'
029800080410     c                   eval      lungm = %len(%trim(cmmm))
029900080410     C                   CALL      'QCMDEXC'                            H1
030000080410     C                   PARM                    CMMm            500
030100080410     C                   PARM                    LUNGM
030200080410     c
030300080410     C*
030400080410     C                   OPEN      TNTA27P
030500080410     C*
030600080410     C                   ENDSR
030700011017
030800011017      *---------------------------------------------------------------*
030900011017      * ROUTINE FINALE
031000011017      *---------------------------------------------------------------*
031100011017     C     Sr_Fine       BEGSR
031200011017
031300011017     C                   write     fine
031400011017      *
031500011017     C                   reset                   TIBS69DS
031600011017     C                   eval      I69TLA  = 'C'
031700011017     C                   call      'TIBS69R'
031800011017     C                   parm                    TIBS69DS
031900011017
032000011017     C                   ENDSR
032100011017      *
032200080410**
032300080410OVRPRTF TNTA27P  OUTQ(          ) USRDFNDTA('
