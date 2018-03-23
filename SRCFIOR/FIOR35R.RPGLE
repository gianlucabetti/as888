000100030410      * FIOR35R *-----------------------------------------------------*
000200030428      *      STAMPA PDC E ANAGRAFICA RITIRI - lancio                *
000300030410      *---------------------------------------------------------------*
000400030410     H DECEDIT('0,') DATEDIT(*DMY.)
000500030410      *
000600030410     FFior35d   CF   E             WORKSTN
000700030410      *
000800130620     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
000900030410      *
001000130620     d TRUL31DS      e ds
001100030410      *
001200030424      * Parametri di selezione
001300030417     D Param           ds
001400030507     D $fil                           3
001500030507     D $ArspcSN                       1
001600030507     D $CblSN                         1
001700030507     D $AcrSN                         1
001800030507     D $RagSN                         1
001900051003     d $tcrsn                         1
002000030410      *
002100030424     D tibs34ds      e ds                  inz
002200020517     D ddatiute      e ds
002300020517     D azuteds       e ds                  extname(AZUTE00F)
002400130620     d TIBS02DS      e ds
002500130620     d dLAT          e ds
002600130620     d dUTE01        e ds
002700130620
002800030424     D kpjba         e ds
002900130620
003000130620     d wabi            s                   like(UTEaut)
003100130620     d w003a           s              3
003200030410      *---------------------------------------------------------------*
003300030410      *      INDICATORI  USATI                                        *
003400030424      *---------------------------------------------------------------*
003500030424      * 28    - Indicatore di errore generico su videata
003600030424      * 49    - P.O obbligatorio
003700030424      * 50    - P.O errato
003800030424      * 51    - Scelte parametri incompatibili
003900050929      * 52    - Scelte parametri incompatibili
004000030410      *---------------------------------------------------------------*
004100030410      *
004200030410      * Definizioni
004300921201     C                   EXSR      DEFVAR
004400030410      *
004500030424     C                   DOW       $Fine = *off
004600030410
004700030410     C                   exfmt     OR35D01
004800030410      *
004900030410      * Controlli 1° videata
005000030410     C                   exsr      Sr_Contr01
005100030424      *
005200030410      * F3=Fine
005300030424     C   KC              move      *on           $Fine
005400030410      *
005500030410      * F6=Conferma
005600030424     C   KF              if        *in28 = *off
005700030507     C                   move      vfil          $Fil
005800030507     C                   move      VArspc        $ArspcSN
005900030507     C                   move      VCbl          $CblSN
006000030507     C                   move      VAcr          $AcrSN
006100030507     C                   move      VRag          $RagSN
006200051003     c                   Eval      $tcrsn = vtcr
006300030417     C                   move      Param         kpjbu
006400030410      *
006500030416      * viene sottomessa la stampa
006600030410      *
006700030416     C                   MOVEL     'OR36'        KCOAZ
006800030416     C                   CALL      'BCH10'
006900030416     C                   PARM                    KPJBA
007000030410      *
007100030410     C                   z-add     *zeros        vfil
007200030416     C                   move      *on           $Fine
007300030410      *
007400030410     C                   endif
007500030410     C                   ENDDO
007600030410
007700030410     C                   seton                                        LR
007800030410      *---------------------------------------------------------------*
007900030417      * ROUTINE DI CONTROLLI                                          *
008000030410      *---------------------------------------------------------------*
008100030410     C     Sr_Contr01    BEGSR
008200030410      *
008300030418     C                   setoff                                       284950
008400050929     C                   setoff                                       5152
008500030410      *
008600030410     C                   IF        VFil = *zeros
008700030410     C                   eval      *in28 = *on
008800030410     C                   eval      *in49 = *on
008900030410     C                   ELSE
009000030410      * P.O errato
009100130620     c                   move      VFil          w003a
009200130620     c     w003a         lookup    SKpog                                  21
009300030410     C  n21              eval      *in28 = *on
009400030410     C  n21              eval      *in50 = *on
009500030410     C                   ENDIF
009600030410      *
009700030417      * Selezioni in alternativa
009800031120      * se richiedo la stampa dei clienti che non hanno l'angrafica ritiri non
009900031120      * posso richiedere il confronto delle ragioni sociali
010000031120     C                   IF        VAcr = 'N' and VRag = 'S'
010100030417     C                   eval      *in28 = *on
010200030417     C                   eval      *in51 = *on
010300030417     C                   ENDIF
010400051003      * se non richiedo la stampa dei clienti con angrafica ritiri non
010500051003      * posso richiedere quelli che hanno il tipo frequenza = M
010600051003     c                   If        vacr <> 'S' and vtcr <> 'I'
010700050929     c                   Eval      *In28 = *On
010800050929     c                   Eval      *In52 = *On
010900050929     c                   EndIf
011000030417      *
011100030410     C                   ENDSR
011200030410      *---------------------------------------------------------------*
011300030410      *                    DEFVAR                                     *
011400030410      *---------------------------------------------------------------*
011500921201     C     DEFVAR        BEGSR
011600030410      *
011700930406     C     *ENTRY        PLIST
011800930406     C                   PARM                    KPJBA
011900030410      *
012000020517     c     *dtaara       define    §azute        azuteds
012100020517     c     *dtaara       define    §datiute      ddatiute
012200020517     C                   in(E)     *dtaara
012300020517     C                   IF        %error  or RSUT = *blanks
012400020517     C                   CLEAR                   tibs34ds
012500020517     C                   CALL      'TIBS34R'
012600020517     C                   PARM                    tibs34ds
012700020517     C                   in        *dtaara
012800020517     c                   endif
012900130620
013000130620      * controllo abilitazione utente
013100130620     c                   clear                   wabi
013200130620     c                   clear                   dLAT
013300130620     c                   eval      dUTE01 = UTEfaf
013400130620
013500130620      * Verifica errori e autorità profilo
013600130620     c                   SELECT
013700130620      * se ho errori nei dati utente esco dal pgm
013800130620     c                   WHEN      DUTerr = 'E'
013900030410     c                   eval      $Fine = *on
014000130620      * carica abilitazioni del profilo
014100130620     c                   WHEN      §UTEorm <> *blanks
014200130620     c                   eval      wabi = §UTEorm
014300130620     c                   WHEN      UTEaut <> *blanks
014400130620     c                   eval      wabi = UTEaut
014500130620      * se non c'è l'abilitazione
014600130620      * --> se 1° livello, abilitazioni al terminal
014700130620      *     se 2° livello, abilitazioni al punto operativo
014800130620      *     se sede no abilitazioni
014900130620     c                   OTHER
015000130620     c                   IF        DUTlpo = '1'
015100130620     c                   eval      wabi   = 'TP'
015200130620     c                   ENDIF
015300130620     c                   IF        DUTlpo = '2'
015400130620     c                   eval      wabi   = 'PO'
015500130620     c                   ENDIF
015600130620     c                   ENDSL
015700130620
015800130620      * controllo se ok l'abilitazione dell'utente
015900130620     c                   clear                   TIBS02DS
016000130620     c                   eval      T02mod = 'C'
016100130620     c                   eval      T02sif = knsif
016200130620     c                   eval      T02cod = 'LAT'
016300130620     c                   eval      T02ke1 = wabi
016400130620     c                   Call      'TIBS02R'
016500130620     c                   Parm                    KPJBA
016600130620     c                   Parm                    TIBS02DS
016700130620     c                   IF        T02err <> *blanks or §LATabi = 'S'
016800130620     c                   eval      $Fine = *on
016900130620     c                   leavesr
017000130620     c                   ENDIF
017100130620     c                   eval      dLAT = T02uni
017200130620
017300130620      * Reperimento delle filiali gestibili dall'utente
017400130620     c                   clear                   TRUL31DS
017500130620     c                   eval      I31abi = wabi
017600130620     c                   eval      I31cdi = DUTdis
017700130620     c                   eval      I31car = DUTare
017800130620     c                   eval      I31cpo = DUTpou
017900130620     c                   call      'TRUL31R'
018000130620     c                   parm                    KPJBA
018100130620     c                   parm                    TRUL31DS
018200130620     c                   IF        O31pog > *zeros
018300130620     c                   movea     O31pog        SKpog
018400130620     c                   ELSE
018500130620     c                   eval      $Fine = *on
018600130620     c                   leavesr
018700130620     c                   ENDIF
018800030410     C
018900030410      *
019000030424      * Inizializzo  videata
019100030428     C                   move      DutPou        VFil
019200030507     C                   move      'S'           Varspc
019300031118     C                   move      'I'           VCbl
019400031120     C                   move      ' '           VAcr
019500030424     C                   move      'N'           VRag
019600051003     c                   Eval      vtcr = 'I'
019700030424     C                   move      *off          $Fine             1
019800030424      *
019900030410     C                   ENDSR
