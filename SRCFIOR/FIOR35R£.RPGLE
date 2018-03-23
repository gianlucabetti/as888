000100030410      * FIOR35R *-----------------------------------------------------*
000200030428      *      STAMPA PDC E ANAGRAFICA RITIRI - lancio                *
000300030410      *---------------------------------------------------------------*
000400030410     H DECEDIT('0,') DATEDIT(*DMY.)
000500030410      *
000600030410     FFior35d   CF   E             WORKSTN
000700030410      *
000800030424      * Schiera per caricamento L1
000900030424     D L1              s              3  0 DIM(30)
001000030410      *
001100030424      * Ds per Trul06r - caricamento £1
001200030410     D Dsul06        e ds                  EXTNAME(trul06ds)
001300030424     D LIA                     1     90  0
001400030410     D                                     DIM(30)
001500030410      *
001600030424      * Parametri di selezione
001700030417     D Param           ds
001800030507     D $fil                           3
001900030507     D $ArspcSN                       1
002000030507     D $CblSN                         1
002100030507     D $AcrSN                         1
002200030507     D $RagSN                         1
002300051003     d $tcrsn                         1
002400030410      *
002500030424     D tibs34ds      e ds                  inz
002600020517     D ddatiute      e ds
002700020517     D azuteds       e ds                  extname(AZUTE00F)
002800030424     D kpjba         e ds
002900030410      *---------------------------------------------------------------*
003000030410      *      INDICATORI  USATI                                        *
003100030424      *---------------------------------------------------------------*
003200030424      * 28    - Indicatore di errore generico su videata
003300030424      * 49    - P.O obbligatorio
003400030424      * 50    - P.O errato
003500030424      * 51    - Scelte parametri incompatibili
003600050929      * 52    - Scelte parametri incompatibili
003700030410      *---------------------------------------------------------------*
003800030410      *
003900030410      * Definizioni
004000921201     C                   EXSR      DEFVAR
004100030410      *
004200030424     C                   DOW       $Fine = *off
004300030410
004400030410     C                   exfmt     OR35D01
004500030410      *
004600030410      * Controlli 1° videata
004700030410     C                   exsr      Sr_Contr01
004800030424      *
004900030410      * F3=Fine
005000030424     C   KC              move      *on           $Fine
005100030410      *
005200030410      * F6=Conferma
005300030424     C   KF              if        *in28 = *off
005400030507     C                   move      vfil          $Fil
005500030507     C                   move      VArspc        $ArspcSN
005600030507     C                   move      VCbl          $CblSN
005700030507     C                   move      VAcr          $AcrSN
005800030507     C                   move      VRag          $RagSN
005900051003     c                   Eval      $tcrsn = vtcr
006000030417     C                   move      Param         kpjbu
006100030410      *
006200030416      * viene sottomessa la stampa
006300030410      *
006400030416     C                   MOVEL     'OR36'        KCOAZ
006500030416     C                   CALL      'BCH10'
006600030416     C                   PARM                    KPJBA
006700030410      *
006800030410     C                   z-add     *zeros        vfil
006900030416     C                   move      *on           $Fine
007000030410      *
007100030410     C                   endif
007200030410     C                   ENDDO
007300030410
007400030410     C                   seton                                        LR
007500030410      *---------------------------------------------------------------*
007600030417      * ROUTINE DI CONTROLLI                                          *
007700030410      *---------------------------------------------------------------*
007800030410     C     Sr_Contr01    BEGSR
007900030410      *
008000030418     C                   setoff                                       284950
008100050929     C                   setoff                                       5152
008200030410      *
008300030410     C                   IF        VFil = *zeros
008400030410     C                   eval      *in28 = *on
008500030410     C                   eval      *in49 = *on
008600030410     C                   ELSE
008700030410      * P.O errato
008800030410     C     VFil          lookup    L1                                     21
008900030410     C  n21              eval      *in28 = *on
009000030410     C  n21              eval      *in50 = *on
009100030410     C                   ENDIF
009200030410      *
009300030417      * Selezioni in alternativa
009400031120      * se richiedo la stampa dei clienti che non hanno l'angrafica ritiri non
009500031120      * posso richiedere il confronto delle ragioni sociali
009600031120     C                   IF        VAcr = 'N' and VRag = 'S'
009700030417     C                   eval      *in28 = *on
009800030417     C                   eval      *in51 = *on
009900030417     C                   ENDIF
010000051003      * se non richiedo la stampa dei clienti con angrafica ritiri non
010100051003      * posso richiedere quelli che hanno il tipo frequenza = M
010200051003     c                   If        vacr <> 'S' and vtcr <> 'I'
010300050929     c                   Eval      *In28 = *On
010400050929     c                   Eval      *In52 = *On
010500050929     c                   EndIf
010600030417      *
010700030410     C                   ENDSR
010800030410      *---------------------------------------------------------------*
010900030410      *                    DEFVAR                                     *
011000030410      *---------------------------------------------------------------*
011100921201     C     DEFVAR        BEGSR
011200030410      *
011300930406     C     *ENTRY        PLIST
011400930406     C                   PARM                    KPJBA
011500030410      *
011600020517     c     *dtaara       define    §azute        azuteds
011700020517     c     *dtaara       define    §datiute      ddatiute
011800020517     C                   in(E)     *dtaara
011900020517     C                   IF        %error  or RSUT = *blanks
012000020517     C                   CLEAR                   tibs34ds
012100020517     C                   CALL      'TIBS34R'
012200020517     C                   PARM                    tibs34ds
012300020517     C                   in        *dtaara
012400020517     c                   endif
012500030424      *   Verifica errori
012600030424      *   controllo se ho errori nei dati utente
012700020520     c                   if        DUTERR = 'E'
012800030410     c                   eval      $Fine = *on
012900020520     c                   endif
013000030410     C
013100030424      *     Carico tabella filiali meccanizzate tariffe £1
013200030410     C                   clear                   Dsul06
013300030410     C                   move      '£1'          D06Cod
013400030410     C                   movel     Simfel        D06Key
013500030410     C                   movel     Dsul06        kpjbu
013600030410     C                   call      'TRUL06R'
013700030410     C                   parm                    kpjba
013800030410     C                   movel     kpjbu         Dsul06
013900030410     C                   movea     LIA           L1
014000030410     C                   clear                   kpjbu
014100030410      *
014200030424      * Inizializzo  videata
014300030428     C                   move      DutPou        VFil
014400030507     C                   move      'S'           Varspc
014500031118     C                   move      'I'           VCbl
014600031120     C                   move      ' '           VAcr
014700030424     C                   move      'N'           VRag
014800051003     c                   Eval      vtcr = 'I'
014900030424     C                   move      *off          $Fine             1
015000030424      *
015100030410     C                   ENDSR
