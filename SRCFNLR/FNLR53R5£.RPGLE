000100000810     H DECEDIT('0,') DATEDIT(*DMY.)
000200030829      * FNLR53R5*----------------------------------------------------*
000300930428      *---------*                                                    *
000400030730      *     - toglie spedizione da distinta consegna -               *
000500960702     F*--------------------------------------------------------------*
000600030827     Ffnlr53d5  cf   e             workstn
000700030827     Fazorg01l  if   e           k disk
000800030827     Ffnarb01l  if   e           k disk
000900030827     Ffnarb70l  if   e           k disk    rename(fnarb000:fnarb70)
001000030827     Ffnarbs1L  uf   e           k disk    rename(fnarbs00:fnarbs1)
001100030827     Ffnarbs0f  o    e           k disk
001200940321      *---------------------------------------------------------------
001300030730     d* schiere
001400030730      *---------------------------------------------------------------
001500030730     d* ds esterne
001600030730      *---------------------------------------------------------------
001700030730     D KPJBA         E DS
001800030730     D ddatiute      e ds
001900030730     D azuteds       e ds                  extname(AZUTE00F)
002000030730     D tibs34ds      E DS                  inz
002100030827     D og143         e ds
002200030901     D param           DS            39
002300030827     D  dis                    1     35  0 dim(5)
002400030827     D  filges                36     38  0
002500030901     d  errwf                 39     39
002600030730     c*controllo check digits
002700030730     D trul28ds      E DS
002800030730      *---------------------------------------------------------------
002900030730     d* ds interne
003000030730      *---------------------------------------------------------------
003100030618     c* ds per sottocampare il barcode della spedizione
003200030618     D ds_pis          DS
003300030618     D  ds_pisaas                     2  0
003400030618     D  ds_pislnp                     3  0
003500030618     D  ds_pisnrs                     2  0
003600030618     D  ds_pisnsp                     7  0
003700030623     D  ds_chkdgt                     1  0
003800030730     d*---------------------------------------------------------------*
003900030730     d* campi di comodo                                               *
004000030730     d*---------------------------------------------------------------*
004100030827     d distinta        s                   like(arbndc)
004200030827     d i               s              3  0
004300030827     d modo            s              1    inz('L')
004400030730     d savchk          s                   like(o28ckd)
004500030730     d barcode         s             14
004600030730     d dtaeur          s               d   datfmt(*eur)
004700030730     d dtaiso          s               d
004800911016      *---------------------------------------------------------------*
004900911016      * FLUSSO PRINCIPALE                                             *
005000911016      *---------------------------------------------------------------*
005100030730     C                   DO        *HIVAL
005200030730      *
005300030730     c* accendo o spengo l'indicatore 73 che mi serve per il posizionamento
005400030730     c* 73 in caso di errore si posiziona sul barcode altrimenti sul numero
005500030730     c* della spedizione
005600030730     c                   if        *in90
005700030827     c                   if        (v03pis <> 0 and v03nsp <> 0) or
005800030827     c                             (v03pis =  0  and v03nsp = 0)
005900030827     c     modo          comp      'L'                                    73
006000030730     c                   end
006100030827     c                   if        (v03pis <> 0 and v03nsp = 0) or
006200030827     c                             (v03pis = 0 and v03nsp <> 0)
006300030827     c     v03pis        comp      0                                  7373
006400030730     c                   end
006500030730     c                   setoff                                       90
006600030826     C                   write     LR53D501
006700030730     c                   seton                                        90
006800030730     c                   end
006900030730     c*
007000030826     C                   EXFMT     LR53D501
007100030730     c* fine lavoro
007200941107     C     *INKC         IFEQ      '1'
007300031008     C                   move      '1'           errwf
007400031008     c                   goto      fine
007500941107     C                   END
007600030828      * completa il caricamento da arb per le bolle non spuntate
007700030828     c                   if        *inkj
007800030828     c                   exsr      completa
007900030828     c                   leave
008000030828     c                   endif
008100030730     c* controlli
008200030730     c                   exsr      srctr
008300030730     c   90              iter
008400030730     c*inizializza
008500030730     c                   exsr      srinz
008600030730     c                   enddo
008700030901     c     *loval        setll     fnarbs1l
008800030901     c                   read      fnarbs1l                               99
008900030901     c                   move      *in99         errwf
009000941107     C*
009100031008     c     fine          tag
009200030730     C                   SETON                                        LR
009300030730     c**********************************************************************
009400030730     C     srinz         BEGSR
009500030730     c**********************************************************************
009600030826     C* Pulisco i campi che devo visualizzare nel fm2
009700030730     C                   clear                   V03pis
009800030730     C                   MOVE      *year         V03ANS
009900030730     C                   Z-ADD     *ZEROS        V03LNP
010000030730     C                   Z-ADD     *ZEROS        V03NRS
010100030730     C                   Z-ADD     *ZEROS        V03NSP
010200030730     c                   endsr
010300030730     c**********************************************************************
010400030730     C     srctr         BEGSR
010500030730     c**********************************************************************
010600030620     C                   SETOFF                                       90
010700030730     c                   do
010800030618     c* se il campo v03pis è impostato valorizzo i campi della spedizione
010900030620     c* solo se non sono stati impostati a mano o al giro di barcode che poi
011000030620     c* ha dato l'errore
011100030618     c                   clear                   ds_pis
011200030827     c                   if        v03pis <> 0
011300030828     c                   move      v03pis        ds_pis
011400030623     c                   if        v03nsp = 0 and
011500030620     c                             v03lnp = 0 and v03nrs = 0
011600030730     C                   MOVE      *year         V03ANS
011700030730     c                   move      ds_pisaas     v03ans
011800030618     c                   move      ds_pislnp     v03lnp
011900030618     c                   move      ds_pisnrs     v03nrs
012000030618     c                   move      ds_pisnsp     v03nsp
012100030623     c* controllo il check digits
012200030623     c                   exsr      srchkdgt
012300030730     c                   if        *in10
012400030730     c                   leave
012500030618     c                   end
012600030623     c                   else
012700030730     c                   move      v03ans        com02             2 0
012800030623     c                   if        com02<>ds_pisaas or v03lnp<>ds_pislnp or
012900030623     c                             v03nrs<>ds_pisnrs or v03nsp<>ds_pisnsp
013000030730     c                   seton                                        1190
013100030730     c                   leave
013200030623     c                   end
013300030623     c                   end
013400030623     c                   end
013500030730     C* F2-Inserimento spedizione da manuale a pistola o viceversa
013600030730     c* serve per il posizionamento sulla spedizione o sul barcode
013700030730     C                   if        *inkb
013800030827     c                   if        modo = 'L'
013900030827     c                   movel     *blanks       modo
014000030703     c                   eval      v03f2 = 'F2=Pistola'
014100030703     c                   seton                                        90
014200030703     c                   setoff                                       7336
014300030730     c                   leave
014400030703     c                   else
014500030827     c                   movel     'L'           modo
014600030703     c                   eval      v03f2 = 'F2=Manuale'
014700030703     c                   seton                                        907336
014800030730     c                   leave
014900030703     c                   end
015000030730     C                   END
015100911017     C* Controllo se la spedizione è esistente o valida su video
015200030730     C     V03ANS        IFEQ      *ZEROS
015300030730     C                   SETON                                        0190
015400030730     c                   leave
015500911017     C                   END
015600911017     C     V03LNP        IFEQ      *ZEROS
015700030730     C                   SETON                                        0290
015800030730     c                   leave
015900911017     C                   END
016000911017     C     V03NSP        IFEQ      *ZEROS
016100030730     C                   SETON                                        0390
016200030730     c                   leave
016300911017     C                   END
016400030730     C                   Z-ADD     V03ANS        ARBAAS
016500911017     C                   Z-ADD     V03LNP        ARBLNP
016600911017     C                   Z-ADD     V03NRS        ARBNRS
016700911017     C                   Z-ADD     V03NSP        ARBNSP
016800030827     C* AGGANCIO FNARBs per verificare se l'ho già spuntata
016900030827     C     KARB          CHAIN     FNARBs1L
017000030827     c                   if        %found(fnarbs1l)
017100030827     c                   seton                                        0790
017200030730     c                   leave
017300030730     c                   end
017400030827     C* AGGANCIO FNARB
017500030827     C     KARB          CHAIN     FNARB01L
017600030827     c                   if        not %found(fnarb01l)
017700030827     c                   seton                                        0590
017800030827     c                   leave
017900030827     c                   end
018000030730     C* controllo la distinta
018100030909     c                   if        arbndc > 0
018200030827     c     arbndc        lookup    dis                                    89
018300030828     c                   if        not *in89
018400030828     c                   seton                                        0890
018500030827     c                   leave
018600030828     c                   endif
018700030909     c                   else
018800030909     c                   seton                                        0890
018900030909     c                   leave
019000030909     c                   endif
019100030827      *non ci sono errori scrivo il W.F.
019200030827     c                   eval      arbprg = arbprg + 1
019300030827     c                   move      'B'           arbtpr
019301110214     c                   move      'S'           arbscn
019400030827     c                   write     fnarbs00
019500030730     c                   enddo
019600030730     C* se non c'è l'errore ind.11 (scelto sia spedizione che barcode)
019700030730     c* azzero la spedizione o il barcode a seconda di come sto inserendo
019800030730     c* i riferimenti della spedizione
019900030730     c                   if        not *in11
020000030730     C                   MOVE      *year         V03ANS
020100030730     C                   Z-ADD     *ZEROS        V03LNP
020200030730     C                   Z-ADD     *ZEROS        V03NRS
020300030730     C                   Z-ADD     *ZEROS        V03NSP
020400030730     C                   clear                   V03pis
020500030730     c                   end
020600030730     c*
020700911017     C                   ENDSR
020800911018     C*-----------------------------------------------------*
020900030623     C* controllo check digits
021000911018     C*-----------------------------------------------------*
021100030623     C     srchkdgt      BEGSR
021200030623     c                   movel     ds_pis        barcode
021300030623     C                   clear                   trul28ds
021400030623     c                   eval      i28mod = 'BAR'
021500030623     c                   eval      i28cod = barcode
021600030623     c                   call      'TRUL28R1'
021700030623     c                   parm                    trul28ds
021800030623     c* se reperito barcode imposto i campi in stampa
021900030623     c                   if        o28err <> *blanks or  o28cod <> ds_pis
022000030623     c                   seton                                        4090
022100030623     c                   end
022200030623     C                   ENDSR
022300030730     C*-----------------------------------------------------*
022400030827     C* completa- aggiungo a FNARBs le spedizioni non spuntate
022500030730     C*-----------------------------------------------------*
022600030827     c     completa      begsr
022700030827     c                   do        5             i
022800030827     c                   if        dis(i) > 0
022900030827     c                   move      dis(i)        distinta
023000030827     c                   else
023100030827     c                   leave
023200030827     c                   endif
023300030827     c     karb70        setll     fnarb70l
023400030827     c                   do        *hival
023500030827     c     karb70        reade     fnarb70l                               88
023600030827     c   88              leave
023700030829      *cerco il network per inserire le bolle poste alla fine
023800030829     c     arblnp        chain     azorg01l
023900030829     c                   if        %found(azorg01l)
024000030829     c                   move      orgde3        og143
024100030829     c                   endif
024200030829
024300030829     c     karb          chain     fnarbs1l
024400030829     c                   if        not %found(fnarbs1l)
024500040820     c*                  if        §ogntw <> 'PPT'
024600030829     c                   move      'A'           arbtpr
024601110214     c                   clear                   arbscn
024700040820     c*                  else
024800040820     c*                  move      'C'           arbtpr
024900040820     c*                  endif
025000030829     c                   z-add     0             arbprg
025100030829     c                   move      '§'           arbcmc
025200030829     c                   write     fnarbs00
025300030829     c                   endif
025400030829     c                   enddo
025500030829     c                   enddo
025600030730     c*
025700030730     c                   endsr
025800940322     C*-----------------------------------------------------*
025900941107     C*    Operazioni iniziali
026000941107     C*-----------------------------------------------------*
026100940322     C     *INZSR        BEGSR
026200950907     C*
026300940322     C     *ENTRY        PLIST
026400030827     C                   PARM                    param
026500020905     c*
026600020905     c     *dtaara       define    §azute        azuteds
026700020905     c     *dtaara       define    §datiute      ddatiute
026800020905     C                   in(E)     *dtaara
026900020905     C                   IF        %Error  or  RSUT = *blanks
027000020905     C                   call      'TIBS34R'
027100020905     C                   parm                    Tibs34Ds
027200020905     C                   in        *dtaara
027300020905     c                   ENDIF
027400030827     c                   eval      v03f2 = 'F2=Manuale'
027500030827     c                   seton                                        36
027600940322      * CHIAVI                                                        *
027700030730     C     KARB          KLIST
027800940322     C                   KFLD                    ARBAAS
027900940322     C                   KFLD                    ARBLNP
028000940322     C                   KFLD                    ARBNRS
028100940322     C                   KFLD                    ARBNSP
028200940322     C*
028300030827     C     KARB70        KLIST
028400030827     C                   KFLD                    filges9           9 0
028500030827     C                   KFLD                    distinta
028600030827     c                   z-add     filges        filges9
028700030827
028800020905     C     *LIKE         DEFINE    ARBIFP        KFGS
028900020905     C     *LIKE         DEFINE    ARBNDC        KNDC
029000960702     C     *LIKE         DEFINE    ARBDDC        KDDC
029100030730     C*
029200030826     C                   MOVEL     'FNLR53R5'    VIDPGM
029300030730     C                   MOVE      *year         V03ANS
029400940322     C*
029500940322     C                   ENDSR
