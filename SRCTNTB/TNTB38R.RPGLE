000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200051121     F* tntb38r * -----------------------------------------*
000300980522     F*          Ricerca / controllo tabelle
000400000000     F*----------------------------------------------------*
000500980522     Ftntbe01l  iF   E           K DISK
000600980525     Ftntbe02l  iF   E           K DISK
000700980525     F                                     RENAME(tntbe000:tntbe002)
000800051121     Ftntb38d   CF   E             WORKSTN  USROPN
000900980522     F                                     SFILE(bs02s01:nrr2)
001000980526     D MSG             S             78    DIM(04) CTDATA PERRCD(1)             MSG ERRORI
001100000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
001200000000     D KPJBA         E DS
001300051121     D Tntb38        E DS                  extname(tntb38ds)
001400971016     DWDATA            S               D   DATFMT(*EUR)
001500980522     D Kke1            S                   LIKE(tbeke1)
001600980522     D Kke2            S                   LIKE(tbeke2)
001700980522     D Klin            S                   LIKE(tbelin)
001800980522     D Ksif            S                   LIKE(tbesif)
001900980522     D Kcod            S                   LIKE(tbecod)
002100971021     F*----------------------------------------------------*
002200971021      * INDICATORI
002300971021     c* 28   - emetto campo messaggi
002400971021     c* 30/31- comodo
002500971021     c* 70   - sfl control
002600971021     c* 71   - sfldsp
002700000000     C*----------------------------------------------------*
002800000000      * CHIAVI
002900971016     C*----------------------------------------------------*
003000980522     C     KTbe1         KLIST                                                  *
003100971016     C                   KFLD                    kcod                           *
003200971016     C                   KFLD                    kke1                           *
003300971016     C                   KFLD                    kke2                           *
003400971016     C                   KFLD                    klin                           *
003500971016     C                   KFLD                    ksif                           *
003600020211      *
003700980525     C     KTbe2         KLIST                                                  *
003800980525     C                   KFLD                    kcod                           *
003900980525     C                   KFLD                    klin                           *
004000980525     C                   KFLD                    ksif                           *
004100020211      *
004200020211     C     KTbe3         KLIST                                                  *
004300020211     C                   KFLD                    kcod                           *
004400020211     C                   KFLD                    klin                           *
004500020211     C                   KFLD                    ksif                           *
004600020211     C                   KFLD                    kKe1
004700020211      *
004800020211     C     KTbe4         KLIST                                                  *
004900020211     C                   KFLD                    kcod                           *
005000020211     C                   KFLD                    klin                           *
005100020211     C                   KFLD                    ksif                           *
005200020211     C                   KFLD                    kKe1
005300020211     C                   KFLD                    kKe2
005400971016     C*----------------------------------------------------*
005500000000     C     *ENTRY        PLIST                                                  *
005600000000     C                   PARM                    KPJBA                          *
005700051121     c                   parm                    tntb38
005800981029     c* c= chiusura file
005900051121     c     t38tla        ifne      'C'
006000890429     C                   Z-ADD     1             CODUT
006100000000     C                   CALL      'XPARUT'
006200000000     C                   PARM                    PARUT
006300000000     C                   MOVEL     RAGUT         RSUT             20
006400971016     C*
006500980526?    C* Controllo se e' stata richiesto CONTROLLO O RICERCA TABELLA
006600980522     c* CONTROLLO
006700051121     c                   if        t38mod='C'
006800980522     c                   exsr      SUB_CONTR
006900980526     C                   else
007000051121     c  n99              open      tntb38d
007100990104     c  n99              seton                                        99
007101051202     c* abilito l'opzione di gestione se profilo EDP*
007102051213     c                   if        %subst(knmus: 1: 3)='EDP' and t38cod='TUT'
007103051202     c                   seton                                        40
007104051202     c                   end
007105051202     c                   do        *hival
007200980522     c* RICERCA
007300980522     c                   exsr      SUB_RICER
007400980525     c* gestione
007500980525     c                   exsr      SUB_gesbf
007600020215     c* se il flag di 'nessuna' selezione è acceso segnalo l'errore
007700051121     c                   if        t38err = ' ' and $NoSel =*on
007800051121     c                   move      'E'           t38err
007900051121     c                   move      msg(4)        t38msg
008000980526     c                   endif
008001051202     c                   if        *inkl and ($NoSel =*on or v2copz='1')
008002051202     c                   leave
008003051202     c                   endif
008004051202     c                   enddo
008100980526     c                   endif
008200981029     c                   endif
008300980522     c*
008400051121     c     t38tla        ifne      ' '
008500980526     C                   eval      *inlr = *on                                       *
008600981029     c                   else
008700981029     C                   eval      *inrt = *on                                       *
008800981029     c                   endif
008900980522     C*-------------------------------------------------------------------------*
009000980522     c* SUB_CONTR Routine di Controllo
009100980522     C*-------------------------------------------------------------------------*
009200980522     c     SUB_CONTR     BEGSR
009300981030     c* verifico se esiste con il mio sistema informativo
009400981030     C* testata tabella
009500981030     c                   clear                   kcod
009600981030     c                   movel     *zeros        kke1
009700051121     c                   move      t38cod        kke1
009800981030     c                   clear                   kke2
009900051121     c                   move      t38lin        klin
010000051121     c                   move      t38sif        ksif
010100981030     c     ktbe1         chain     tntbe01l                           90
010200981102     c* imposto la chiave con il mio sistema informativo
010300051121     c                   move      t38cod        kcod
010400051121     c                   move      t38ke1        kke1
010500051121     c                   move      t38ke2        kke2
010600051121     c                   move      t38lin        klin
010700981030     c* se esiste in testata cerco la mia tabella solo con il sistema informativo passato
010800981030     c                   if        not *in90
010900980522     c* imposto la chiave con il mio sistema informativo
011000051121     c                   move      t38sif        ksif
011100980525     c     ktbe1         chain     tntbe01l                           30
011200981030     c*
011300981030     c* se non esiste la testata con il mio sistema informativo cerco la tabella con il sistema
011400981030     c* informativo a blank
011500981030     c                   else
011600981030     c                   clear                   ksif
011700981030     c     ktbe1         chain     tntbe01l                           30
011800981030     c                   endif
011900981030     c* se non trovato  ERRORE
012000981030     c                   if        *in30=*on
012100051121     c                   move      'E'           t38err
012200051121     c                   move      msg(1)        t38msg
012300980522     c                   endif
012400981030     c* se trovato ma annullato
012500981030     c                   if        *in30=*off and tbeatb <> *blank
012600051121     c                   move      'A'           t38err
012700051121     c                   move      msg(2)        t38msg
012800980522     c                   endif
012900980522     c* se non c'è errore passo il record
013000051121     c                   if        t38err <> 'E'
013100051121     c                   move      tbeatb        t38atb
013200051121     c                   move      tbesif        t38sif
013300051121     c                   move      tbelin        t38lin
013400051121     c                   move      tbecod        t38cod
013500051121     c                   move      tbeke1        t38ke1
013600051121     c                   move      tbeke2        t38ke2
013700051121     c                   move      tbeuni        t38uni
013800051121     c                   move      tbeftt        t38ftt
013900051121     c                   move      tbeflt        t38flt
014000051121     c                   move      tbeftr        t38ftr
014100051121     c                   z-add     tbedtr        t38dtr
014200980522     c                   endif
014300980522     c*
014400980522     c                   endsr
014500980522     C*-------------------------------------------------------------------------*
014600980522     c* SUB_RICER Routine di Ricerca
014700980522     C*-------------------------------------------------------------------------*
014800980525     c     SUB_RICER     BEGSR
014900980522     C*
015000980526     c* definisco i campi del control
015100980526     c                   move      *blanks       kcod
015200051121     c                   move      t38lin        klin
015300051121     c                   move      t38sif        ksif
015400980526     c                   movel     *zeros        kke1
015500051121     c                   move      t38cod        kke1
015600980526     c                   move      *blanks       kke2
015700980526     C     Ktbe1         chain     tntbe01l                           31
015800980526     c* se non l'ho trovato cerco senza il sistema informativo specifico
015900980526     c                   if        *in31 = *on
016000980526     c                   move      *blanks       ksif
016100980526     C     Ktbe1         chain     tntbe01l                           31
016200980526     c                   endif
016300980526     c*
016400980526     c                   if        *in31 = *off
016500980526     c                   move      tbeke1        v2crcd
016600980526     c                   move      tbeapl        v2crap
016700980526     c                   movel     tbeuni        v2crun
016800980526     c                   endif
016900980526     c*
017000980522     C                   Z-ADD     0             nrr2              4 0          *
017100971021     C                   Z-ADD     1             CURS2                          *
017200981030     c* pulisco il subfile
017300981030     c                   seton                                        70
017400981030     c                   write     bs02c01
017500981030     c                   setoff                                       70
017600981030     c* imposto la chiave a parte il sistema informativo che rimane uguale all'ultima
017700981030     c* chain fatta
017800051121     c                   move      t38cod        kcod
017900051121     c                   move      t38lin        klin
018000881025     C*
018100980525     C*-------- CARICA SUBFILE ----------------------------*
018200981030     C*                        SISTEMA INFORMATIVO  VALORIZZATO come da testata tabella
018300020211      *
018400030514      * Posizionamento
018500030514     C     Ktbe2         SETLL     tntbe02l                                     *
018600030514     C                   DO        *hival
018700980525     C     Ktbe2         READE     tntbe02l                               30
018800030514     c   30              leave
018900000000     C*
019000051206     c                   if        tbeatb = ' ' or (tbeatb <>' ' and
019001051206     c                             %subst(knmus: 1: 3)='EDP')
020200030514     c*
020300971021     C                   ADD       1             nrr2                           *
020400971015     c                   clear                   v2copz
020500980525     C                   MOVE      tbeke1        v2cke1                         *
020600980525     C                   MOVE      tbeke2        v2cke2                         *
020700971015     C                   MOVEL     tbeuni        v2cdes                         *
020701051206     C                   MOVEL     tbeatb        v2catb                         *
021100980526     C                   WRITE     bs02s01                                      *
021200971021     c                   endif
021300971015     C                   ENDdo                                                  *
021400980525     c*
021500980525     c                   endsr
021600980525     C*-------------------------------------------------------------------------*
021700980525     c* SUB_GESBF Routine Gestione Subfile
021800980525     C*-------------------------------------------------------------------------*
021900980525     c     SUB_GESBF     BEGSR
022000980525     C*
022100971015     C* nessun record caricato
022200980526    1c                   if        nrr2 = 0
022300980525     c                   eval      *in71 = *off
022400980526     c* errore
022500051121     c                   move      'E'           t38err
022600051121     c                   move      msg(3)        t38msg
022700980526     c
022800980526    xc                   else
022900980525     c                   eval      *in71 = *on
023000980526    1c                   endif
023100000000     C*
023200881025?    C     T1TAG         TAG                                                    *
023300980526     C                   WRITE     bs02z01                                      *
023400980526     C                   WRITE     bs02t01                                      *
023500981030?   1C                   do        *hival                                       *
023600971015     c* sfl controlo
023700980526     C                   EXFMT     bs02c01                                      *
023800020215     C                   move      *off          $NoSel            1
023801051202     c* inserimento
023802051202     c                   if        *inkj
023803051202     c                   clear                   v2cke1
023804051202     c                   call      'TNTBTUTR'
023805051202     c                   parm                    kpjba
023806051202     c                   parm      'TUT'         v2crcd
023807051202     c                   parm                    v2cke1
023808051227     c                   parm                    t38lin
023809051227     c                   parm                    t38sif
023810051202     c                   leave
023811051202     c                   end
023900981030     c* F12= fine lavoro
024000980526    2C                   if        *inkl = *off                                 *
024100971015     c*
024200971028     c* per 71 on non leggo perche' non ci sono record
024300980526     c   71              readc     bs02s01                                30
024400980526    3c   71              dow       *in30 = *off
024500971016     c* imposto la key
024501051202     c                   select
024600051202    4c                   when      v2copz = '1'
024700980526     c                   move      v2crcd        kcod
024800980526     c                   movel     v2cke1        kke1
024900980526     c                   move      v2cke2        kke2
025000051227     c                   move      t38lin        klin
025200980526     c* aggancio il record selezionato
025300980526     c     ktbe1         chain     tntbe01l                           31
025400051121     c                   move      tbeatb        t38atb
025500051121     c                   move      tbesif        t38sif
025600051121     c                   move      tbelin        t38lin
025700051121     c                   move      tbeapl        t38apl
025800051121     c                   move      tbecod        t38cod
025900051121     c                   move      tbeke1        t38ke1
026000051121     c                   move      tbeke2        t38ke2
026100051121     c                   move      tbeuni        t38uni
026200051121     c                   move      tbeftt        t38ftt
026300051121     c                   move      tbeflt        t38flt
026400051121     c                   move      tbeftr        t38ftr
026500051121     c                   z-add     tbedtr        t38dtr
026600051121     c                   move      ' '           t38err
026700051121     c                   move      *blanks       t38msg
026800980526     c                   eval      *in30 = *on
026900980526     c                   eval      *inkl = *on
026901051202    4c                   when      v2copz = '2' and *in40
026902051202     c                   call      'TNTBTUTR'
026903051202     c                   parm                    kpjba
026904051202     c                   parm                    v2crcd
026905051202     c                   parm                    v2cke1
026906051227     c                   parm                    t38lin
026907051227     c                   parm                    t38sif
026909051202     c                   eval      *in30 = *on
026910051202     c                   eval      *inkl = *on
027000051202   x4c                   other
027100980526     c                   readc     bs02s01                                30
027200980526     c
027300051202    4c                   endsl
027400980526    3C                   enddo
027500020215   2eC                   else
027600020215     C                   move      *on           $NoSel
027700980526    2c                   endif                                                  *kl
027800981030    1C  nkl              enddo                                                  *do  hival
027900971021     c**
028000971016     c                   endsr
028100980526**
028200980526Tabella inesistente
028300980526Tabella annullato
028400980526Errore non trovati dati per questa tabella
028500980526Non e' stato selezionato nulla
