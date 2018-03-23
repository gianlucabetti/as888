000100981204      *----------------------------------------------------*
000200990217      *  Controllo colli non consegnati per spedizione
000300981204      *----------------------------------------------------*
000400981204
000500981204      *--------------------------------------------------------------*
000600981204      *  RIEPILOGO INDICATORI
000700981204      *--------------------------------------------------------------*
000800981204      * 30 - ERRORE comodo
000900981204      * 31 - ERRORE comodo
001000981204      * 32 - ERRORE comodo
001100981204      * 33 - ERRORE comodo
001200981204      *--------------------------------------------------------------*
001300981204
001400981204     H DECEDIT('0,') DATEDIT(*DMY.)
001500981204
001600990217     FFNDCT01L  IF   E           K DISK
001700990217     FFNDCD02L  IF   E           K DISK
001800981209     FFNBLT01L  IF   E           K DISK
001900981209     FFNART01L  IF   E           K DISK
002000981204
002100981204     D UT§DSE0F      E DS
002200990217     D fIDN53DS      E DS
002300981204     D DTAD          E DS
002400990219     D TIBS02DS      E DS
002500981204
002600981204     D KPJBA         E DS
002700981204
002800990219     D Bolle           S              1
002900981204      *------------------------------------------------------------------------*
003000981204      *
003100981204     IFNBLT000
003200981204     I              BLTATB                      ARTATB
003300981204     I              BLTAAS                      ARTAAS
003400981204     I              BLTLNP                      ARTLNP
003500981204     I              BLTNRS                      ARTNRS
003600981204     I              BLTNSP                      ARTNSP
003700981204     I              BLTLNA                      ARTLNA
003800981204     I              BLTFLS                      ARTFLS
003900981204     I              BLTNSC                      ARTNSC
004000981204     I              BLTVUC                      ARTVUC
004100981204     I              BLTFVC                      ARTFVC
004200981204     I              BLTPUC                      ARTPUC
004300981204     I              BLTFPC                      ARTFPC
004400981204     I              BLTDFV                      ARTDFV
004500981204     I              BLTNPP                      ARTNPP
004600981204     I              BLTNFV                      ARTNFV
004700981204     I              BLTFLP                      ARTFLP
004800981204     I              BLTDET                      ARTDET
004900981204     I              BLTDUT                      ARTDUT
005000981204     I              BLTDAM                      ARTDAM
005100981204     I              BLTDCM                      ARTDCM
005200981204     I              BLTCAN                      ARTCAN
005300981204     I              BLTFL1                      ARTFL1
005400981204     I              BLTFL2                      ARTFL2
005500981204     I              BLTFTR                      ARTFTR
005600981204     I              BLTDTR                      ARTDTR
005700981204      *------------------------------------------------------------------------*
005800981204
005900981204      * aggancio dettaglio colli bolle arrivo
006000990219     c     Kspe          chain     fnart01l                           30
006100990219     c                   if        *in30
006200981204      * aggancio dettaglio colli bolle partenza se non trovato in arrivo
006300990219     c     kspe          chain     fnblt01l                           30
006400990219     c  n30              eval      bolle = 'P'
006500990219     c*
006600990219     c                   else
006700990219     c*
006800990219     c                   eval      bolle = 'A'
006900990219     c*
007000990219     c                   endif
007100990219     c* per 30 spento
007200990219     c                   dow       not *in30
007300990219     c* controllo la data consegna del singolo collo
007400990219     c                   if        artdcm = 0
007500990219     c* e se la data è uguale a zero controllo se collo presente in altre CA
007600990219     c     kdcd          chain     fndcd02l                           31
007700990226     c* se annullato o chiuso non lo considero
007800990226     c                   if        not *in31 and (dcddch > 0 or dcdatb <> ' ')
007900990226     c                   eval      *in31 = *on
008000990226     c                   endif
008100990219     c* se esistente in altre CA controllo se tipo anomalia mancanza / avaria resa o consegnata
008200990219     c                   if        not *in31
008300990219     c     kdct          chain     fndct01l                           32
008400990219     c*
008500990219     c                   if        not *in32
008600990219     c* controllo  tipo anomalia
008700990219     c                   exsr      chtad
008800990219     c* controllo il raggruppamento
008900990219     c* mancanza
009000990219     c                   if        §tadragr = 'M'
009100990219     c* avaria
009200990219     c                             or  §tadragr = 'V'  and
009300990324     c* disposizione merce resa o consegnata o distrutta
009400990324     c                             (dctdsd='R' or dctdsd='C' or dctdsd='D')
009500990219     c* non considero il collo da consegnare altrimenti
009600990219     c*
009700990219     c                   else
009800990219     c*
009900990219     c                   eval      o53err = 'E'
010000990219     c                   endif
010100990219     c*
010200990219     c                   endif
010300990219     c*
010400990219     c                   else
010500990219     c*
010600990219     c                   eval      o53err = 'E'
010700990219     c                   endif
010800990219     c*
010900990219     c                   endif
011000990219     c*
011100990219     c                   if        bolle = 'A'
011200990219     c     Kspe          reade     fnart01l                               30
011300990219     c                   endif
011400990219     c*
011500990224     c                   if        bolle = 'P'
011600990219     c     Kspe          reade     fnblt01l                               30
011700990219     c                   endif
011800990219      *
011900981204     c                   enddo
012000981204      *
012100990217      *
012200990223     c                   movel     fidn53ds      kpjbu
012300990217     c                   eval      *inlr = *on
012400981204
012500990219      *****************************************************************
012600990219      *  REPERIMENTO DATI TIPO ANOMALIA
012700990219      *****************************************************************
012800990219     C     CHTAD         BEGSR
012900990219      *
013000990219     c                   clear                   DTAD
013100990219      *
013200990219     c                   clear                   TIBS02DS
013300990219     C                   MOVEL     'C'           t02mod
013400990219     C                   MOVEL     knsif         t02sif
013500990219     C                   MOVEL     'TAD'         t02cod
013600990219     C                   MOVEL(P)  dcttad        t02ke1
013700990219      *
013800990219     C                   CALL      'TIBS02R'
013900990219     C                   PARM                    KPJBA
014000990219     C                   PARM                    TIBS02DS
014100990219      *
014200990219     C                   if        t02err = *BLANKS
014300990219     C                   MOVEL     T02UNI        DTAD
014400990219     C                   endif
014500990219      *
014600990219     C                   ENDSR
014700981204      *------------------------------------------------------------------------*
014800981204      * ROUTINE INIZIALE
014900981204      *------------------------------------------------------------------------*
015000981204     C     *INZSR        BEGSR
015100981204      *
015200981204     C     *ENTRY        PLIST
015300981204     C                   PARM                    KPJBA
015400990217     c                   movel     kpjbu         fidn53ds
015500981204      *
015600981204     C*----------------------------------------------------*
015700981204     C* KEY   KEY    KEY   KEY   KEY   KEY   KEY   KEY  KEY
015800981204     C*----------------------------------------------------*
015900981204      *
016000981204     c* ACCESSO fndcd
016100990217     C     kdcd          KLIST                                                  *
016200990219     C                   KFLD                    artfls                         *
016300990219     C                   KFLD                    artlna                         *
016400990219     C                   KFLD                    artnrs                         *
016500990219     C                   KFLD                    artnsc                         *
016600990219     c* ACCESSO fndct
016700990219     C     kdct          KLIST                                                  *
016800990219     C                   KFLD                    dcdaac                         *
016900990226     C                   KFLD                    dcdfil                         *
017000990219     C                   KFLD                    dcdnca                         *
017100981204      *
017200990217     c* ACCESSO FNART/FNBLT
017300981209     C     kspe          KLIST                                                  *
017400990217     C                   KFLD                    i53aas                         *
017500990217     C                   KFLD                    i53lnp                         *
017600990217     C                   KFLD                    i53nrs                         *
017700990217     C                   KFLD                    i53nsp                         *
017800990219     c*
017900981204     C                   ENDSR
