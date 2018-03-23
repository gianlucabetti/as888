000100110218      *PARMS CLOSQLCSR(*ENDMOD)
000200110218      *PARMS DYNUSRPRF(*OWNER)
000300000713     H DECEDIT('0,') DATEDIT(*YMD) DATFMT(*ISO)
000400110217      * FNLV77R ******************************************************
000500110217      *         CONTROLLO CAP spedizioni clienti         - BATCH -   *
000600000711      *         -------------------------                ---------   *
000700000704      ****************************************************************
000800110225     ftitas33c  if   e           k disk    usropn
000900110225     F                                     extfile(wlibsede)
001000110225     ftita430c  if   e           k disk    usropn
001100110225     F                                     extfile(wlibsede)
001200111219     fTABEL00F  if   e           k disk
001300110308
001400110309     D ERR             S             78    DIM(11) CTDATA PERRCD(1)
001500110309     d erc             s              2    dim(11) alt(err)
001600111219      * - Dati da tab. "3A" = Codici bolla
001700111219     d $3A             s                   dim( 50) like(tascbo)  inz
001800111219     d $3Atb1          s                   dim( 50) like(§3Atb1)  inz
001900111219     d $3Atb2          s                   dim( 50) like(§3Atb2)  inz
002000111219     d $3Afca          s                   dim( 50) like(§3Afca)  inz
002100000713      *  DS PER RICHIAMO FNLV13R (CONTROLLO CAP)
002200000713     D DSSI95        E DS                  EXTNAME(TISI95DS)
002300000713     D DSLV13        E DS                  EXTNAME(FNLV13DS)
002400110217      *
002500111219     D fnlv10ds      E DS
002600111219     D fnlv76ds      E DS
002700110218     d tibs10ds      e ds
002800110218     d skc                           11    DIM(500) overlay(d10skc)
002900110218     d TIBS69DS      E DS                  INZ
003000110218     d DS_cnaco      E DS                  extname(CNACO00F)
003100110218     d DS_cnind      E DS                  extname(CNIND00F)
003200110218     d DS_cnclp      E DS                  extname(CNCLP00F)
003300110218     d DS_fncls      E DS                  extname(FNCLS00F)
003400110217      *
003500000713      *  DS KPJBA
003600000711     D KPJBA         E DS
003700111219     D ds3a          E DS
003800100429     d
003900110218     D DTA4A         E DS
004000110218     D KTRC            S                   LIKE(TA4trc)
004100110217     d k_cli           s                   like(tasccm)
004200110217     d k_aas           s                   Like(tasaas)
004300110217     d k_mgs           s                   Like(tasmgs)
004400110713     d w_o95lia        s                   Like(o95lia)
004500110309     d n               s              2  0
004600111219     d xy              s              3  0 inz
004700111219     d xx              s              3  0 inz
004800110225     D Wlibsede        S             21
004900110225     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS33C')
005000110225     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS33C')
005100110225     D WlibsedeP4      S             21    inz('GAITRAGRPS/TITA430C')
005200110225     D WlibsedeB4      S             21    INZ('GAITRAGRU /TITA430C')
005300000705      ****************************************************************
005400000705      * RIEPILOGO INDICATORI:                                        *
005500000705      ****************************************************************
005600000704      *
005700000704     C     *ENTRY        PLIST
005800000704     C                   PARM                    KPJBA
005900110217     C                   MOVEL     KPJBU         fnlv76ds
006000110217      *
006100110217     c     kt33          klist
006200110217     c                   kfld                    k_cli
006300110217     c                   kfld                    k_aas
006400110217     c                   kfld                    k_mgs
006500110218     C     ktastrc       KLIST
006600110218     C                   KFLD                    tasAAS
006700110218     C                   KFLD                    tasLNP
006800110218     C                   KFLD                    tasNRS
006900110218     C                   KFLD                    tasNSP
007000110218     C                   KFLD                    Ktrc
007100111219     c     K02TABEL      klist
007200111219     c                   kfld                    TBLkut
007300111219     c                   kfld                    TBLcod
007400111219      * Caricamento tab. "3A" = Codici Bolla
007500111219      *   (selezione delle sole bolle normali)
007600111219     c                   clear                   xx
007700111219     c                   eval      TBLkut  = 1
007800111219     c                   eval      TBLcod  = '3A'
007900111219     c     K02TABEL      setll     TABEL
008000111219     c     K02TABEL      reade     TABEL
008100111219do  1c                   DOW       NOT %eof(TABEL00F)
008200111219if  2c                   if        TBLflg  = *blanks
008300111219     c                   movel     TBLuni        ds3A
008400111219if  3c                   if        §3Arbl  = 'N'
008500111219     c                   add       1             xx
008600111219     c                   movel     TBLkey        $3A(xx)
008700111219     c                   movel     §3Atb1        $3Atb1(xx)
008800111219     c                   movel     §3Atb2        $3Atb2(xx)
008900111219     c                   z-add     §3Afca        $3Afca(xx)
009000111219e   3c                   endif
009100111219e   2c                   endif
009200111219     c     K02tabel      reade     TABEL
009300111219e   1c                   ENDDO
009400111219      *
009500020417
009600110225     c* pulisco file di work
009700110225     C/EXEC SQL
009800111220     C+ DELETE FROM WFEIV00F where eivjob<>'          '
009900110225     C/END-EXEC
010000110225     c                   If        %subst(knsif:7:1) = 'P'
010100110225     c                   eval      wlibsede=wlibsedep
010200110225     c                   else
010300110225     c                   eval      wlibsede=wlibsedeb
010400110225     c                   endif
010500110225     c                   open      titas33c
010600110225     c                   If        %subst(knsif:7:1) = 'P'
010700110225     c                   eval      wlibsede=wlibsedep4
010800110225     c                   else
010900110225     c                   eval      wlibsede=wlibsedeb4
011000110225     c                   endif
011100110225     c                   open      tita430c
011200110218     c* cliente da elaborare: singolo o unificante
011300110221     c                   if        lv76kscu='S'
011400110217     c                   z-add     lv76ksc       k_cli
011500110218     c                   exsr      sr_elabora
011600110218     c                   else
011700110218     c* verifico se codice è figlio
011800110218     c                   clear                   tibs10ds
011900110218     c                   eval      d10tle='ST'
012000110218     c                   eval      d10paf='P'
012100110218     c                   eval      d10cod=lv76ksc
012200110218     c                   call      'TIBS10R'
012300110218     c                   parm                    tibs10ds
012400110218     c                   if        d10cop=0
012500110218     c* se non è figlio verifico se è padre
012600110218     c                   clear                   tibs10ds
012700110218     c                   eval      d10tle='ST'
012800110218     c                   eval      d10paf='F'
012900110218     c                   eval      d10cod=lv76ksc
013000110218     c                   call      'TIBS10R'
013100110218     c                   parm                    tibs10ds
013200110218     c                   endif
013300110218     c                   if        d10cop>0
013400110218     C     1             DO        500           H                 3 0
013500110218     C     SKC(H)        IFeq      *ZEROS
013600110218     c                   leave
013700110218     c                   endif
013800110218     c                   move      skc(H)        k_cli
013900110218     c                   exsr      sr_elabora
014000110218     c                   enddo
014100110218     c                   else
014200110223     c* se non è padre e non è figlio è un cliente singolo
014300110218     c                   z-add     lv76ksc       k_cli
014400110223     c                   exsr      sr_elabora
014500110218     c                   endif
014600110218     c                   endif
014700110218      *
014800000712      *
014900110217     c* chiudo pgm
015000110217     c                   clear                   dslv13
015100110217     c                   clear                   dssi95
015200110217     c                   movel     'C'           i13tla
015300110217     C                   CALL      'FNLV13R'
015400110217     C                   PARM                    KPJBA
015500110217     C                   PARM                    DSLV13
015600110217     C                   PARM                    DSSI95
015700110217     C                   PARM                    flgbac            1
015800110217      *
015900110217     C                   SETON                                        LR
016000110218      *--------------------------------------------------------------*
016100110218      * Elaborazione bolle di un ccm                                 *
016200110218      *--------------------------------------------------------------*
016300110218     c     sr_elabora    begsr
016400110218     c                   z-add     lv76aas       k_aas
016500110218     c                   z-add     lv76mgs       k_mgs
016600110218     c* richiamo tibs69r per recuperare la ragione sociale del cliente
016700110218     c                   clear                   DS_cnaco
016800110218     c                   clear                   DS_cnind
016900110218     c                   clear                   DS_cnclp
017000110218     c                   clear                   DS_fncls
017100110218     c                   z-add     k_cli         i69kac
017200110218     c                   call      'TIBS69R'
017300110218     c                   parm                    tibs69ds
017400110218     c                   parm                    ds_cnaco
017500110218     c                   parm                    ds_cnind
017600110218     c                   parm                    ds_cnclp
017700110218     c                   parm                    ds_fncls
017800110218      *
017900110218     c     kt33          setll     titas33c
018000110218     c                   do        *hival
018100110218     c     k_cli         reade     titas33c
018200110218     c* Fine lettura per fine file o superamento data max
018300110218     c                   if        %eof(titas33c) or
018400110218     c                             tasaas>lv76aasa or
018500110218     c                             tasmgs>lv76mgsa
018600110218     c                   leave
018700110218     c                   endif
018800110712     c                   clear                   w_o95lia
018900110218     c                   exsr      chkcap
019000110218     c                   enddo
019100110218     c                   endsr
019200000713      *
019300000712      *--------------------------------------------------------------*
019400000712      * CHKCAP - SUBROUTINE CONTROLLO CAP                            *
019500000712      *--------------------------------------------------------------*
019600000713      * NB: VIENE RICHIAMATO "FNLV13R" ED IN BASE AL RITORNO DEI     *
019700000713      *     PARAMETRI STAMPO ERRORE OPPURE NO;                       *
019800000713      *     POSSONO ESSERE EFFETTUATI 2 TENTATIVI DI CONTROLLO:      *
019900000713      *     IL PRIMO: NORMALE                                        *
020000070116      *     IL SECONDO1: SE HO TROVATO CAP OBSOLETO AL PRIMO         *
020100070116      *     IL SECONDO2: SE ERRORE DI PROVINCIA AL PRIMO             *
020200000713      *---------------------------------------------------------------
020300000713      *
020400000712     C     CHKCAP        BEGSR
020500000713      *
020600111219     C                   CLEAR                   fnlv10ds
020700000713     C                   CLEAR                   DSSI95
020800000713      *
020900111219     c                   eval      I95dat  = tasaas * 10000 + tasmgs
021000000713      *
021100110217     C                   MOVEL     taslod        I95LOC
021200110217     C                   MOVEL     tascad        I95CAP
021300110217     C                   MOVEL     tascad        I95CAP
021400110217     C                   MOVEL     tasprd        I95PRV
021500110217     C                   MOVEL     tasnzd        I95NAR
021600111219     c                   eval      I95ffd  = tasffd
021700111219     c                   eval      I95tsp  = tastsp
021800111219     c                   eval      I95tfp  = tastfp
021900111219     c                   eval      I95lkg  = taspkb
022000111219     c                   eval      I95lmc  = tasvlb
022100111219     c                   eval      I95fi2  = tasftc+ tastc2
022200111219     c                   if        taslna=400
022300111219     c                   eval      I95fi1  = 'S'
022400111219     c                   endif
022500111219     c                   eval      i95tpo  = tastbl
022600111219     c                   eval      i95fca  = 'N'
022700111219      *
022800111219     c                   eval      xy      = 1
022900111219     c     tascbo        lookup    $3A(xy)                                10
023000111219      *
023100111219if  1c                   if        *in10
023200111219      * - Flag C/Assegno
023300111219if  2c                   if        $3Afca(xy)    <> *zeros
023400111219     c                   eval      i95fca  = 'S'
023500111219e   2c                   endif
023600111219      *
023700111219e   1c                   endif
023800111219      *
023900000713      *
024000101122     c
024100111219     c* Imposto campi DS
024200111219     c                   eval      ilv10ccm=tasccm
024300111219     c                   eval      ilv10rmn=tasrmn
024400111219     c                   eval      ilv10rsd=tasrsd
024500111219     c                   eval      ilv10ind=tasind
024600111219     c                   eval      ilv10lna=taslna
024700111219     c                   eval      ilv10znc=tasznc
024800111220     c                   eval      ilv10job=knmeb
024900111219     c                   call      'FNLV10R'
025000111219     c                   parm                    kpjba
025100111219     c                   parm                    dssi95
025200111219     c                   parm                    fnlv10ds
025300111219     c
025400111219     c                   ENDSR
025500110308**
025600110309C.A.P. incongruente con provincia e/o localita'                               50
025700110309C.A.P. inesistente                                                            51
025800110712C.A.P. obsoleto                                                               52
025900110712CAP errato                                                                    54
026000110309Inserito un CAP generico di citta' che ha il Viario                           55
026100110712C.A.P. o località obsoleti                                                    31
026200110713Provincia/CAP errato                                                          40
