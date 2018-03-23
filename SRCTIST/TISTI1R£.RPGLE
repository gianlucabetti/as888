000100980506      *------------------------------------------------------------------------*
000200110111      * Gestione statistiche clienti Roma dopo fatturazione                    *
000300980506      *------------------------------------------------------------------------*
000400980506     H DECEDIT('0,') DATEDIT(*DMY.)
000500980506      *--------------------------------------------
000600110111     Ftisti1D   CF   E             WORKSTN sfile(sti1s03:nrr1)
000700990719     F                                     infds ($Infds)
000800990916     FTNTBE01L  uf a E           k DISK
000900110111     Ftabel00f  if   E           k DISK
001000110111     Fcnaco00f  if   E           k DISK
001100110111     Fcnclp00f  if   E           k DISK
001200980506      *--------------------------------------------
001300990924     D savnrr          S                   LIKE(nrr1)
001400990927     D saverr          S                   LIKE(nrr1)
001500110111     D kcod            S                   LIKE(tbecod) inz('SFR')
001600990719     D kkey1           S                   LIKE(tbeke1)
001700990719     D kkey2           S                   LIKE(tbeke2)
001800110111     D kkut            S                   LIKE(acokut)
001900110111     D kksc            S                   LIKE(acoksc)
002000990706      *
002100990127     D MSG             S             78    DIM(6) CTDATA PERRCD(1)              MSG VIDEO
002200980507      *--------------------------------------------
002300980507     D KPJBA         E DS
002400990706     D CNCR80        E DS
002500990706     D UT§DSE0F      E DS
002600990927     d TIBS02DS      E DS
002700990706     D dtreds        E DS                  extname(dtre)
002800990719     d$infds           ds
002900990729     d  $xrn01               378    379b 0
003000110111      * - Parametri per richiamo al pgm di controllo profilo utenti
003100110111     d TIBS34ds      e ds
003200110111      * - Ds di riferimento al file esterno AzUte00F
003300110111     d AZUTEds       e ds                  ExtName(AzUte00F)
003400110111      * - Ds per dati organigramma
003500110111     d dDatiUte      e ds
003600110111      *
003700110111      *--------------------------------------------
003800980908     C*  RIEPILOGO INDICATORI
003900980908      *--------------------------------------------
004000980908      * 20/22 - Gestione SUBFILE
004100980908      * 28    - ERRORE GENERICO EMISSIONE CAMPO MESSAGGI
004200980908      * 30/34 - di comodo
004300990127      * 40/42 - Errori a video
004400980908      *----------------------------------------------------------
004500980506      *
004600980506      * EMISSIONE PRIMA VIDEATA
004700980506     C     EMD01         TAG
004800110111     c                   clear                   v1cragr
004900110111     c
005000110111     C     for01         TAG
005100110111     C                   EXFMT     sti1D01
005200980506     C                   CLEAR                   V1CMSG
005300980506     C                   eval      *in28 = *OFF
005400980506      * F3=FINE
005500981030     c   KC              goto      fine
005600110111     c
005700110111     c                   exsr      contr1
005800110111     c
005900110111     c                   if        *in90
006000110111     c                   goto      for01
006100110111     c                   endif
006200990924     C
006300990924      * EMISSIONE SECONDA VIDEATA                                           --
006400110111     c     emd02         tag
006500990924      *
006600980717      * gestione subfile tipo anomalie valide
006700990924     c                   exsr      carsbf
006800990924      *
006900990924     c                   exsr      gessbf
007000990927     c
007100990927      * F12=RITORNO  o dopo aggiornamento ritorno a 1 videata
007200990927     C  nkc              GOTO      EMD01
007300990924      * F 3-FINE
007400990924     B*
007500980506     C     FINE          TAG
007600980506     C                   EVAL      *INLR = *ON
007700110111      *****************************************************************
007800110111      *   controllo cidice raggruppamento
007900110111      *****************************************************************
008000110111     C     Contr1        BEGSR
008100110111     C*
008200110111     c     '?'           scan      v1cragr                                90
008300110111     c                   if        *in90
008400110111     C                   MOVEL     'R'           t02mod
008500110111     C                   MOVEL     knsif         t02sif
008600110111     C                   MOVEL     'SFR'         t02cod
008700110111      *
008800110111     C                   CALL      'TIBS02R'
008900110111     C                   PARM                    KPJBA
009000110111     C                   PARM                    TIBS02DS
009100110111      *
009200110111     C                   IF        T02err = *BLANKS
009300110111     C                   movel     T02ke1        v1cragr
009400110111     c                   else
009500110111     c                   clear                   v1cragr
009600110111     c                   endif
009700110111     c
009800110111     c                   leavesr
009900110111     c                   endif
010000110111     c
010100110111     c*  controllo
010200110111     C                   MOVEL     'C'           t02mod
010300110111     C                   MOVEL     knsif         t02sif
010400110111     C                   MOVEL     'SFR'         t02cod
010500110111     C                   MOVEL     v1cragr       t02ke1
010600110111      *
010700110111     C                   CALL      'TIBS02R'
010800110111     C                   PARM                    KPJBA
010900110111     C                   PARM                    TIBS02DS
011000110111      *
011100110111     C                   IF        T02err<> *BLANKS
011200110111     c                   seton                                        2890
011300110111     c                   eval      v1cmsg=msg(1)
011400110111     c                   leavesr
011500110111     c                   endif
011600110111     c*
011700110111     c                   ENDSR
011800980717      *****************************************************************
011900980717      *   ROUTINE DI CARICAMENTI SUBFILE
012000980717      *****************************************************************
012100110111     C     Carsbf        begsr
012200110111      * pulizia del subfile dettaglio anagrafica fase
012300110111     c                   clear                   sti1s03
012400110111     c                   eval      *IN22 = *ON
012500110111     c                   WRITE     sti1c03
012600110111     c                   setoff                                       202122
012700110111     c                   clear                   nrr1
012800110111     c                   clear                   $xrn01
012900110111     C
013000110111      * caricamento subfile per codice raggruppamento
013100110111     c
013200110111     c                   z-add     *zeros        nrr1              4 0
013300110111     c                   eval      kkey1=v1cragr
013400110111     c
013500110111     c     ktbe          setll     tntbe01l
013600110111     c     ktbe          reade(n)  tntbe01l
013700110111     c
013800110111     c                   dow       not %eof(tntbe01l)
013900110111     c
014000110111     c                   movel     tbeuni        v3cksc
014100110111     c* Chain su cnaco per la ragione sociale
014200110111     c                   if        v3cksc>*zeros
014300110111     c                   movel     v3cksc        kksc
014400110111     c     kaco          chain     cnaco00f
014500110111     c                   if        %found(cnaco00f)
014600110111     c                   movel     acorag        v3dksc
014700110111     c                   endif
014800110111     c
014900110111     c     kaco          chain     cnclp00f
015000110111     c                   if        %found(cnclp00f)
015100110111     c                   movel     clpage        v3ccmm
015200110111     c                   endif
015300110111     c
015400110111     c                   endif
015500110111     c*
015600990927     c                   exsr      scrivi
015700110111     c
015800110111     c     ktbe          reade(n)  tntbe01l
015900980717     c                   enddo
016000110111     c
016100990730     c                   z-add     1             nrr1
016200980717      *
016300980717     c                   endsr
016400990916     c*--------------------------------------------------------
016500990916     c     scrivi        begsr
016600990916     c*--------------------------------------------------------
016700990916     c                   add       1             nrr1
016800110111     c                   write     sti1s03
016900990929     c                   setoff                                       27
017000990916     c                   endsr
017100980717      *****************************************************************
017200980717      *   ROUTINE DI gestione del subfile
017300980717      *****************************************************************
017400980717     C     GESSBF        BEGSR
017500990924     C*
017600980717     c     nrr1          comp      *zeros                             2020
017700980717     c                   seton                                        21
017800990924     c
017900110111     c                   write     sti1z03
018000990924     c
018100110111     c                   exfmt     sti1c03
018200990927     c                   clear                   v1cmsg
018300990927     c                   setoff                                       282990
018400990924     C* F 3 - FINE
018500990924     c   kC              MOVEL     '1'           WFINE             1
018600990927     c   kC              goto      endsbf
018700990924     C* F12 - RITORNO
018800990927     c   kl              goto      endsbf
018900990924     C*
019000990927     c                   z-add     1             nrr1
019100990927     c                   do        *hival
019200110111     c                   readc     sti1s03                                32
019300990726     c   32              leave
019400990927     c                   setoff                                       282927
019500110111     c                   clear                   wricerca          1
019600990924     C*
019700990924     C* F6 - CONFERMA
019800990927     c   kf              if        saverr=0
019900990927     C                   EXSR      AGGIORNA
020000990927     c                   LEAVE
020100990927     c                   endif
020200990927     c
020300990709     c                   enddo
020400980717      *
020500990927     c     endsbf        endsr
020600980717      *
020700990924     c*---------------------------------------------------------------------
020800110111     c* Aggiorna tabelaa SFR
020900990924     c*---------------------------------------------------------------------
021000990924     c     aggiorna      begsr
021100990924     c
021200990924     c                   z-add     1             nrr1
021300110111     c     nrr1          chain     sti1s03                            30
021400990924     c     *in30         doweq     *off
021500990924     c*
021600110111     c     v3cksc        ifne      *blanks
021700990924     c                   endif
021800990924     c
021900990924     c                   add       1             nrr1
022000110111     c     nrr1          chain     sti1s03                            30
022100990924     c                   enddo
022200990924     c
022300990924     c                   endsr
022400980506      *****************************************************************
022500980506      *   ROUTINE INIZIALE
022600980506      *****************************************************************
022700980506     C     *INZSR        BEGSR
022800980506      *
022900980506     C     *ENTRY        PLIST
023000980506     C                   PARM                    KPJBA
023100980506      *
023200980506      *
023300980717      * accesso tntbe
023400990719     c     ktbe2         klist
023500110111     C                   kfld                    kcod
023600990719     C                   kfld                    kkey1
023700990719     C                   kfld                    kkey2
023800110111     c     ktbe          klist
023900110111     C                   kfld                    kcod
024000110111     C                   kfld                    kkey1
024100110111     c     kaco          klist
024200110111     C                   kfld                    kkut
024300110111     C                   kfld                    dutkci
024400110111     C                   kfld                    kksc
024500110111     c
024600110111     c                   exsr      sr_DatiJob
024700110111     c*
024800980717      *
024900980506     C                   ENDSR
025000110111      *---------------------------------------------------------------*
025100110111      *?Reperimento dati del job (Utente/Operativi)                  ?*
025200110111      *---------------------------------------------------------------*
025300110111     c     sr_DatiJob    BEGSR
025400110111      *
025500110111     c     *dtaara       define    §azute        AZUTEds
025600110111     c     *dtaara       define    §datiute      dDatiUte
025700110111      *
025800110111     c                   in(E)     *dtaara
025900110111     c                   IF        %Error or RSUT = *blanks
026000110111     c                   clear                   TIBS34ds
026100110111     c                   call      'TIBS34R'
026200110111     c                   parm                    TIBS34ds
026300110111     c                   in        *dtaara
026400110111     c                   ENDIF
026500110111      *
026600110111     c                   ENDSR
026700980508** MSG  (Lungh. 78)                                                          *
026800110111Raggruppamento errato
026900980508Eseguita la stampa richiesta                                                       5
027000990927Descrizione obbligatoria
027100990927Immettere cod.raggruppamento NUOVO!!
027200990927Immettere Codice raggr. valido                                                     1
