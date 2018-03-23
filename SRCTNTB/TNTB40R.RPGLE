000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200030128     F* tntb40r * -----------------------------------------*
000300980522     F*          Ricerca / controllo tabelle
000400000000     F*----------------------------------------------------*
000500980522     Ftntbe01l  iF   E           K DISK
000600980525     Ftntbe02l  iF   E           K DISK
000700980525     F                                     RENAME(tntbe000:tntbe002)
000800030128     Ftntb40d   CF   E             WORKSTN  USROPN
000900980522     F                                     SFILE(bs02s01:nrr2)
001000980526     D MSG             S             78    DIM(04) CTDATA PERRCD(1)             MSG ERRORI
001100000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001200000000     D KPJBA         E DS
001300030128     D Tntb40        E DS                  extname(tntb40ds)
001400160303     D Tibs36ds      E DS
001500971016     DWDATA            S               D   DATFMT(*EUR)
001600980522     D Kke1            S                   LIKE(tbeke1)
001700980522     D Kke2            S                   LIKE(tbeke2)
001800980522     D Klin            S                   LIKE(tbelin)
001900980522     D Ksif            S                   LIKE(tbesif)
002000980522     D Kcod            S                   LIKE(tbecod)
002100030128     D DCRE          E DS
002200971021     F*----------------------------------------------------*
002300971021      * INDICATORI
002400971021     c* 28   - emetto campo messaggi
002500971021     c* 30/31- comodo
002600971021     c* 70   - sfl control
002700971021     c* 71   - sfldsp
002800000000     C*----------------------------------------------------*
002900000000      * CHIAVI
003000971016     C*----------------------------------------------------*
003100980522     C     KTbe1         KLIST                                                  *
003200971016     C                   KFLD                    kcod                           *
003300971016     C                   KFLD                    kke1                           *
003400971016     C                   KFLD                    kke2                           *
003500971016     C                   KFLD                    klin                           *
003600971016     C                   KFLD                    ksif                           *
003700020211      *
003800980525     C     KTbe2         KLIST                                                  *
003900980525     C                   KFLD                    kcod                           *
004000980525     C                   KFLD                    klin                           *
004100980525     C                   KFLD                    ksif                           *
004200020211      *
004300020211     C     KTbe3         KLIST                                                  *
004400020211     C                   KFLD                    kcod                           *
004500020211     C                   KFLD                    klin                           *
004600020211     C                   KFLD                    ksif                           *
004700020211     C                   KFLD                    kKe1
004800020211      *
004900020211     C     KTbe4         KLIST                                                  *
005000020211     C                   KFLD                    kcod                           *
005100020211     C                   KFLD                    klin                           *
005200020211     C                   KFLD                    ksif                           *
005300020211     C                   KFLD                    kKe1
005400020211     C                   KFLD                    kKe2
005500971016     C*----------------------------------------------------*
005600000000     C     *ENTRY        PLIST                                                  *
005700000000     C                   PARM                    KPJBA                          *
005800030128     c                   parm                    tntb40
005900160303      *  In base all'utente prende il P.O.
006000160303     C                   CLEAR                   Tibs36Ds
006100160303     c                   EVAL      I36ute = Knmus
006200160303     c                   EVAL      I36Tla = 'L'
006300160303     C                   CALL      'TIBS36R'
006400160303     C                   PARM                    Tibs36Ds
006500160303      *
006600981029     c* c= chiusura file
006700981029     c     t02tla        ifne      'C'
006800890429     C                   Z-ADD     1             CODUT
006900000000     C                   CALL      'XPARUT'
007000000000     C                   PARM                    PARUT
007100000000     C                   MOVEL     RAGUT         RSUT             20
007200971016     C*
007300980526?    C* Controllo se e' stata richiesto CONTROLLO O RICERCA TABELLA
007400980522     c* CONTROLLO
007500980522     c                   if        t02mod='C'
007600980522     c                   exsr      SUB_CONTR
007700980526     C                   else
007800030128     c  n99              open      tntb40d
007900990104     c  n99              seton                                        99
008000980522     c* RICERCA
008100980522     c                   exsr      SUB_RICER
008200980525     c* gestione
008300980525     c                   exsr      SUB_gesbf
008400020215     c* se il flag di 'nessuna' selezione è acceso segnalo l'errore
008500020215     c                   if        t02err = ' ' and $NoSel =*on
008600980526     c                   move      'E'           t02err
008700980526     c                   move      msg(4)        t02msg
008800980526     c                   endif
008900980526     c                   endif
009000981029     c                   endif
009100980522     c*
009200981029     c     t02tla        ifne      ' '
009300980526     C                   eval      *inlr = *on                                       *
009400981029     c                   else
009500981029     C                   eval      *inrt = *on                                       *
009600981029     c                   endif
009700980522     C*-------------------------------------------------------------------------*
009800980522     c* SUB_CONTR Routine di Controllo
009900980522     C*-------------------------------------------------------------------------*
010000980522     c     SUB_CONTR     BEGSR
010100981030     c* verifico se esiste con il mio sistema informativo
010200981030     C* testata tabella
010300981030     c                   clear                   kcod
010400981030     c                   movel     *zeros        kke1
010500981030     c                   move      t02cod        kke1
010600981030     c                   clear                   kke2
010700981030     c                   move      t02lin        klin
010800981030     c                   move      t02sif        ksif
010900981030     c     ktbe1         chain     tntbe01l                           90
011000981102     c* imposto la chiave con il mio sistema informativo
011100981102     c                   move      t02cod        kcod
011200981102     c                   move      t02ke1        kke1
011300981102     c                   move      t02ke2        kke2
011400981102     c                   move      t02lin        klin
011500981030     c* se esiste in testata cerco la mia tabella solo con il sistema informativo passato
011600981030     c                   if        not *in90
011700980522     c* imposto la chiave con il mio sistema informativo
011800980522     c                   move      t02sif        ksif
011900980525     c     ktbe1         chain     tntbe01l                           30
012000981030     c*
012100981030     c* se non esiste la testata con il mio sistema informativo cerco la tabella con il sistema
012200981030     c* informativo a blank
012300981030     c                   else
012400981030     c                   clear                   ksif
012500981030     c     ktbe1         chain     tntbe01l                           30
012600981030     c                   endif
012700981030     c* se non trovato  ERRORE
012800981030     c                   if        *in30=*on
012900980522     c                   move      'E'           t02err
013000980522     c                   move      msg(1)        t02msg
013100980522     c                   endif
013200981030     c* se trovato ma annullato
013300981030     c                   if        *in30=*off and tbeatb <> *blank
013400980522     c                   move      'A'           t02err
013500980522     c                   move      msg(2)        t02msg
013600980522     c                   endif
013700980522     c* se non c'è errore passo il record
013800980522     c                   if        t02err <> 'E'
013900980522     c                   move      tbeatb        t02atb
014000980522     c                   move      tbesif        t02sif
014100980522     c                   move      tbelin        t02lin
014200980522     c                   move      tbecod        t02cod
014300980522     c                   move      tbeke1        t02ke1
014400980522     c                   move      tbeke2        t02ke2
014500980522     c                   move      tbeuni        t02uni
014600980522     c                   move      tbeftt        t02ftt
014700980522     c                   move      tbeflt        t02flt
014800980522     c                   move      tbeftr        t02ftr
014900980522     c                   z-add     tbedtr        t02dtr
015000980522     c                   endif
015100980522     c*
015200980522     c                   endsr
015300980522     C*-------------------------------------------------------------------------*
015400980522     c* SUB_RICER Routine di Ricerca
015500980522     C*-------------------------------------------------------------------------*
015600980525     c     SUB_RICER     BEGSR
015700980522     C*
015800980526     c* definisco i campi del control
015900980526     c                   move      *blanks       kcod
016000980526     c                   move      t02lin        klin
016100980526     c                   move      t02sif        ksif
016200980526     c                   movel     *zeros        kke1
016300980526     c                   move      t02cod        kke1
016400980526     c                   move      *blanks       kke2
016500980526     C     Ktbe1         chain     tntbe01l                           31
016600980526     c* se non l'ho trovato cerco senza il sistema informativo specifico
016700980526     c                   if        *in31 = *on
016800980526     c                   move      *blanks       ksif
016900980526     C     Ktbe1         chain     tntbe01l                           31
017000980526     c                   endif
017100980526     c*
017200980526     c                   if        *in31 = *off
017300980526     c                   move      tbeke1        v2crcd
017400980526     c                   move      tbeapl        v2crap
017500980526     c                   movel     tbeuni        v2crun
017600980526     c                   endif
017700980526     c*
017800980522     C                   Z-ADD     0             nrr2              4 0          *
017900971021     C                   Z-ADD     1             CURS2                          *
018000981030     c* pulisco il subfile
018100981030     c                   seton                                        70
018200981030     c                   write     bs02c01
018300981030     c                   setoff                                       70
018400981030     c* imposto la chiave a parte il sistema informativo che rimane uguale all'ultima
018500981030     c* chain fatta
018600980525     c                   move      t02cod        kcod
018700980525     c                   move      t02lin        klin
018800881025     C*
018900980525     C*-------- CARICA SUBFILE ----------------------------*
019000981030     C*                        SISTEMA INFORMATIVO  VALORIZZATO come da testata tabella
019100020211      *
019200030514      * Posizionamento
019300030514     C     Ktbe2         SETLL     tntbe02l                                     *
019400030514     C                   DO        *hival
019500980525     C     Ktbe2         READE     tntbe02l                               30
019600030514     c   30              leave
019700000000     C*
019800980525     c                   if        tbeatb = ' '
019900030128     C                   MOVEL     tbeuni        DCre                           *
020000121001      * Causale NON Utilizzabile
020100121001     c                   if        §CrUTI = 'N'
020200121001     c                   iter
020300121001     c                   end
020400160303      * Causale NON Utilizzabile SEDE o filiale non la carica nella ricerca
020500160303     c                   if        §Crfise  = ' ' and o36pou = 046 or
020600160303     c                             §Crfise  = 'S' and o36pou <> 046
020700160303     c                   iter
020800160303     c                   end
020900121001      *
021000030514     C* il tipo servizio è = ' ' solo per autotrasportatori
021100030514     c                   if        t02TSR = ' '
021200030514     c                   if        §CrTSR <>'R' and §crtsr <> 'C'
021300030514     c                   iter
021400030514     c                   end
021500030514     c                   else
021600030514     c                   if        §CrTSR <> t02tsr
021700030514     c                   iter
021800030514     c                   end
021900030514     c                   end
022000030514     c*
022100971021     C                   ADD       1             nrr2                           *
022200971015     c                   clear                   v2copz
022300980525     C                   MOVE      tbeke1        v2cke1                         *
022400980525     C                   MOVE      tbeke2        v2cke2                         *
022500971015     C                   MOVEL     tbeuni        v2cdes                         *
022600971016     C                   MOVEL     tbelin        v2clin                         *
022700971016     C                   MOVEL     tbesif        v2hsif                         *
022800971020     C                   MOVE      tbesif        v2ccsf                         *
022900980526     C                   WRITE     bs02s01                                      *
023000971021     c                   endif
023100121001      *
023200971015     C                   ENDdo                                                  *
023300980525     c*
023400980525     c                   endsr
023500980525     C*-------------------------------------------------------------------------*
023600980525     c* SUB_GESBF Routine Gestione Subfile
023700980525     C*-------------------------------------------------------------------------*
023800980525     c     SUB_GESBF     BEGSR
023900980525     C*
024000971015     C* nessun record caricato
024100980526    1c                   if        nrr2 = 0
024200980525     c                   eval      *in71 = *off
024300980526     c* errore
024400980526     c                   move      'E'           t02err
024500980526     c                   move      msg(3)        t02msg
024600980526     c
024700980526    xc                   else
024800980525     c                   eval      *in71 = *on
024900980526    1c                   endif
025000000000     C*
025100881025?    C     T1TAG         TAG                                                    *
025200980526     C                   WRITE     bs02z01                                      *
025300980526     C                   WRITE     bs02t01                                      *
025400981030?   1C                   do        *hival                                       *
025500971015     c* sfl controlo
025600980526     C                   EXFMT     bs02c01                                      *
025700020215     C                   move      *off          $NoSel            1
025800981030     c* F12= fine lavoro
025900980526    2C                   if        *inkl = *off                                 *
026000971015     c*
026100971028     c* per 71 on non leggo perche' non ci sono record
026200980526     c   71              readc     bs02s01                                30
026300980526    3c   71              dow       *in30 = *off
026400971016     c* imposto la key
026500980526    4c                   if        v2copz = '1'
026600980526     c                   move      v2crcd        kcod
026700980526     c                   movel     v2cke1        kke1
026800980526     c                   move      v2cke2        kke2
026900971016     c                   move      v2clin        klin
027000971016     c                   movel     v2hsif        ksif
027100980526     c* aggancio il record selezionato
027200980526     c     ktbe1         chain     tntbe01l                           31
027300980526     c                   move      tbeatb        t02atb
027400980526     c                   move      tbesif        t02sif
027500980526     c                   move      tbelin        t02lin
027600980526     c                   move      tbeapl        t02apl
027700980526     c                   move      tbecod        t02cod
027800980526     c                   move      tbeke1        t02ke1
027900980526     c                   move      tbeke2        t02ke2
028000980526     c                   move      tbeuni        t02uni
028100980526     c                   move      tbeftt        t02ftt
028200980526     c                   move      tbeflt        t02flt
028300980526     c                   move      tbeftr        t02ftr
028400980526     c                   z-add     tbedtr        t02dtr
028500980526     c                   move      ' '           t02err
028600980526     c                   move      *blanks       t02msg
028700980526     c                   eval      *in30 = *on
028800980526     c                   eval      *inkl = *on
028900980526   x4c                   else
029000980526     c                   readc     bs02s01                                30
029100980526     c
029200980526    4c                   endif
029300980526    3C                   enddo
029400020215   2eC                   else
029500020215     C                   move      *on           $NoSel
029600980526    2c                   endif                                                  *kl
029700981030    1C  nkl              enddo                                                  *do  hival
029800971021     c**
029900971016     c                   endsr
030000980526**
030100980526Tabella inesistente
030200980526Tabella annullato
030300980526Errore non trovati dati per questa tabella
030400980526Non e' stato selezionato nulla
