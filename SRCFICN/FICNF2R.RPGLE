000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000607      *
000300100928     Fficnf2d   CF   E             WORKSTN
000400030604     f                                     sfile(video2:snrr1)
000500030709     Fazorg01l  if   E           k disk
000600030714     Ffiapd01l  if   E           k disk
000700100928     Ffiatt03l  if   E           k disk
000800090513     ftabel00f  if   e           k disk
000900011228      *
001000020103     D Psds           SDS
001100020103     D  PgmName          *PROC
001200011228     D Kpjba         E DS
001300011228      *
001400100318     D daut          E DS
001500100928     d ficnf1ds      e ds
001600100617     d dvpocont      e ds
001700100617     d tibs02ds      e ds
001800030604     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001900030604     D CNCR80        E DS
002000030617     d Tibs36Ds      e ds
002100100928     d ficn70ds      e ds
002200011228      *
002300100928     D dtaiso                          d
002400100928     D dtaeur                          d   datfmt(*eur)
002500100928     D x               S              4  0
002600030604     D snrr1           S              5i 0
002700030613     D WLBDA8          DS
002800030613     D  G02DAT                 1      8  0
002900030613     D  G02INV                 9     16  0
003000030613     D  G02ERR                17     17
003100030613     D  G02TGI                18     22  0
003200011227      *
003300100928     c                   clear                   df1err
003400000627      *
003500030604     C                   EXSR      Carvideo2
003600000627      *
003700100928     C                   if        ok = 0
003800100928     c                   eval      df1err = '1'
003900100928     c                   exsr      uscita
004000100928     c                   end
004100100928     c*
004200100928     c                   do        *hival
004300030604     C                   WRITE     video2z
004400030604     C                   EXFMT     video2c
004500011228     c* fine
004600100928     C   kc              leave
004700011227     C                   EXSR      GestioneSFL
004800011227     C                   ENDDO
004900100928
005000100928     C                   EXSR      Uscita
005100011228     c**********************************************************************
005200011228     c* carica sfl
005300011228     c**********************************************************************
005400030604     C     Carvideo2     BEGSR
005500030604     C                   EVAL      snrr1   = 0
005600011228     C                   EVAL      *IN90 = *ON
005700020219     C                   EVAL      *IN91 = *OFF
005800030604     C                   WRITE     video2c
005900011228     C                   EVAL      *IN90 = *OFF
006000100928     c*
006100030714     c     kattd         setll     fiatt03l
006200030701     c                   do        *hival
006300030714     c     kattd         reade     fiatt03l                               97
006400030701     c   97              leave
006500100928     c* controllo se presente la targa
006600100928     c                   if        attflr = ' '
006700100928     c                   iter
006800100928     c                   end
006900101001     c* controllo se passata la targa del rimorchio
007000100928     c                   if        df1flr <> ' ' and attflr <> df1flr
007100100928     c                   iter
007200100928     c                   end
007300100928     C*
007400030701     c                   move      *blanks       v1csce
007500030709     c                   move      attVAD        vattVAD
007600030709     c                   move      attFGP        vattFGP
007700030709     c     attfgp        chain     azorg01l                           95
007800030714     c  n95              movel     orgdes        vattfgpd
007900030714     c   95              clear                   vattfgpd
008000030709     c                   move      attFGA        vattFGA
008100030709     c     attfga        chain     azorg01l                           95
008200030709     c  n95              movel     orgdes        vattfgad2
008300030709     c   95              clear                   vattfgad2
008400030714     c                   move      attORPr       vattORPr
008500030722     c                   if        attpdr > 0
008600030722     c                   move      attPDR        vattPDR
008700030722     c                   else
008800030722     c                   clear                   vattPDR
008900030722     c                   endif
009000030722     c                   move      vattpdr       vattpdrk
009100030722     c     kapd          chain     fiapd01l                           95
009200090507     c  n95              movel     apdrsf        vattpdrd2
009300030722     c   95              clear                   vattpdrd2
009400100928     c                   movel     attflr        vattflr
009500030701      *esco per raggiunta massima capacità sfl
009600030617     c                   if        snrr1 > 9990
009700030617     c                   leave
009800030617     c                   endif
009900030617
010000030604     C                   EVAL      snrr1 = snrr1 + 1
010100030604     C                   WRITE     video2
010200011228     C                   ENDDO
010300011228
010400030604     C                   IF        snrr1  > 0
010500020219     C                   EVAL      *IN91 = *ON
010600020219     C                   MOVE      1             OK                1 0
010700020219     C                   ELSE
010800020219     C                   MOVE      0             OK                1 0
010900030701     C                   ENDIF
011000030604     C                   EVAL      snrr1  = 1
011100011228     C                   ENDSR
011200011227     c**********************************************************************
011300011227     c* gestione sfl
011400011227     c**********************************************************************
011500000613     C     GestioneSFL   BEGSR
011600000627      *
011700030605      * Elaborazione opzioni.
011800011228     c*
011900030605     c                   do        *hival        X                 4 0
012000030605     c     X             CHAIN     video2                             50
012100030605     C   50              LEAVE
012200030604     C                   IF        v1csce <> *blanks
012300100928     c                   movel     vattflr       df1flr
012400100928     C                   EXSR      Uscita
012500011228     C                   ENDIF
012600030605     C                   enddo
012700000627      *
012800030703     C                   ENDSR
012900011227     c**********************************************************************
013000000607     C     *INZSR        BEGSR
013100011227     c**********************************************************************
013200000607      *
013300000607     C     *ENTRY        PLIST
013400000607     C                   PARM                    Kpjba
013500100928     c                   movel     kpjbu         ficnf1ds
013600100928     c                   clear                   df1err
013700100928     C*
013800100928     C                   movel     df1ddc        dtaiso
013900100928     C                   movel     dtaiso        dtaeur
014000100928     C                   movel     dtaeur        vdata1
014100100928     c*
014200030604     C                   Z-ADD     1             CODUT
014300030604     C                   CALL      'X§PARUT'
014400030604     C                   PARM                    UTEDSE
014500030604     C                   MOVEL     REC80         CNCR80
014600030703     C                   MOVEL     knsif         vknsif
014700030703     C                   MOVEL     knmus         vknmus
014800030702      *reperimento data
014900030702     C                   TIME                    W0120            14 0
015000030702     C                   MOVE      W0120         WDAT              8 0
015100030702     C*
015200030702     C                   Z-ADD     WDAT          G02DAT
015300030702     C                   MOVEL     *BLANK        G02ERR
015400030702     C                   CALL      'XSRDA8'
015500030702     C                   PARM                    WLBDA8
015600030702     C* UDATE A 8 IN AAAA/MM/GG
015700030717     C                   Z-ADD     G02INV        WUDAte            8 0
015800030702     C                   Z-ADD     G02INV        WUDA              8 0
015900030606      *profilo di filiale attivo indicatore di protezione campi non manutenzionabili
016000030617     C                   CLEAR                   Tibs36Ds
016100030617     C                   EVAL      I36ute = knmus
016200030617     C                   CALL      'TIBS36R'
016300030617     C                   PARM                    tibs36ds
016400100617      * controllo se attivare o no controllo autorizzazioni e gestione terminal
016500100617     c                   clear                   tibs02ds
016600100617     c                   clear                   dvpocont
016700100617     c                   eval      t02mod = 'C'
016800100617     c                   eval      t02sif = knsif
016900100617     c                   eval      t02cod = 'VPO'
017000100617     c                   eval      t02ke1 = 'CONT'
017100100617     c                   call      'TIBS02R'
017200100617     c                   parm                    kpjba
017300100617     c                   parm                    tibs02ds
017400100617    2c                   if        t02err = *blanks
017500100617     c                   eval      dvpocont  = t02uni
017600100617    2c                   endif
017700100318      * controllo autorizzazione del profilo
017800100617     c                   if        wuda    >= §vpoaut
017900100318     c                   movel     o36pou        i70pog
018000100318     c                   movel     o36pou        i70pge
018100100318     c                   movel     knmus         i70prf
018200100318     c                   call      'FICN70R'
018300100318     c                   parm                    ficn70ds
018400100318     c                   movel     o70uni        daut
018500100318     c                   if        §AFFCPSTD <> 'S'
018600100318     c                   exsr      uscita
018700100318     c                   endif
018800100617     c                   endif
018900100318
019000100928     c                   if        df1fgp <> 0 and df1fgp <> 046
019100100928     c                   movel     df1fgp        vattfgp
019200100928     c                   else
019300100928     c                   movel     o36pou        vattfgp
019400100928     c                   end
019500000607      *
019600030711     C     Kattd         KLIST
019700030714     C                   KFLD                    vattfgp
019800100928     C                   KFLD                    df1ddc
019900030714     C     Kapd          KLIST
020000030714     C                   KFLD                    tipoa             1
020100030722     C                   KFLD                    vattpdrk          7 0
020200100928     C                   movel     'D'           tipoa
020300011227     C                   ENDSR
020400100928     c**********************************************************************
020500100928     c* uscita
020600100928     c**********************************************************************
020700100928     C     Uscita        BEGSR
020800100928      *
020900100928     c                   movel     ficnf1ds      kpjbu
021000100928     C                   EVAL      *INLR = *ON
021100100928     C                   RETURN
021200100928      *
021300100928     C                   ENDSR
