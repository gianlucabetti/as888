000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200980522     F* tibs02r * -----------------------------------------*
000300980522     F*          Ricerca / controllo tabelle
000400000000     F*----------------------------------------------------*
000500060531     Ftntbe11l  iF   E           K DISK
000600060531     Ftntbe12l  iF   E           K DISK
000700060531     F                                     RENAME(tntbe000:tntbe012)
000800990104     Ftibs02d   CF   E             WORKSTN  USROPN
000900980522     F                                     SFILE(bs02s01:nrr2)
001000120716      * -?Caratteri minuscoli?
001100120716     d c_Minusc        c                   const('abcdefghijklm+
001200120716     d                                            nopqrstuvwxyz+
001300120716     d                                            ‡·‚„‰Â+
001400120716     d                                            ÊÁ+
001500120716     d                                            ËÈÍÎ+
001600120716     d                                            ÏÌÓÔ+
001700120716     d                                            ÚÛÙıˆ+
001800120716     d                                            ˘˙˚¸')
001900120716      * -?Caratteri maiuscoli?
002000120716     d c_Maiusc        c                   const('ABCDEFGHIJKLM+
002100120716     d                                            NOPQRSTUVWXYZ+
002200120716     d                                            ¿¡¬√ƒ≈+
002300120716     d                                            ∆«+
002400120716     d                                            »… À+
002500120716     d                                            ÃÕŒœ+
002600120716     d                                            “”‘’÷+
002700120716     d                                            Ÿ⁄€‹')
002800060531     D MSG             S             78    DIM(05) CTDATA PERRCD(1)             MSG ERRORI
002900000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
003000000000     D KPJBA         E DS
003100980522     D TIBS02        E DS                  extname(tibs02ds)
003200971016     DWDATA            S               D   DATFMT(*EUR)
003300980522     D Kke1            S                   LIKE(tbeke1)
003400980522     D Kke2            S                   LIKE(tbeke2)
003500980522     D Klin            S                   LIKE(tbelin)
003600980522     D Ksif            S                   LIKE(tbesif)
003700980522     D Kcod            S                   LIKE(tbecod)
003800120716     d SavC1Cke1       s                   like(C1Cke1)  inz
003900120716     d SavC1Cke2       s                   like(C1Cke2)  inz
004000120716     d SavW1Cdes       s                   like(W1Cdes)  inz
004100971021     F*----------------------------------------------------*
004200971021      * INDICATORI
004300971021     c* 28   - emetto campo messaggi
004400971021     c* 30/31- comodo
004500120716     c* 41   - filtro attivo (F4=Ricerca)
004600971021     c* 70   - sfl control
004700971021     c* 71   - sfldsp
004800120716     c* 72   - sflEnd
004900000000     C*----------------------------------------------------*
005000000000      * CHIAVI
005100971016     C*----------------------------------------------------*
005200980522     C     KTbe1         KLIST                                                  *
005300060531     C                   KFLD                    klin                           *
005400971016     C                   KFLD                    kcod                           *
005500971016     C                   KFLD                    kke1                           *
005600971016     C                   KFLD                    kke2                           *
005700971016     C                   KFLD                    ksif                           *
005800020211      *
005900980525     C     KTbe2         KLIST                                                  *
006000060531     C                   KFLD                    klin                           *
006100980525     C                   KFLD                    kcod                           *
006200980525     C                   KFLD                    ksif                           *
006300020211      *
006400020211     C     KTbe3         KLIST                                                  *
006500060531     C                   KFLD                    klin                           *
006600020211     C                   KFLD                    kcod                           *
006700020211     C                   KFLD                    ksif                           *
006800020211     C                   KFLD                    kKe1
006900020211      *
007000020211     C     KTbe4         KLIST                                                  *
007100060531     C                   KFLD                    klin                           *
007200020211     C                   KFLD                    kcod                           *
007300020211     C                   KFLD                    ksif                           *
007400020211     C                   KFLD                    kKe1
007500020211     C                   KFLD                    kKe2
007600971016     C*----------------------------------------------------*
007700000000     C     *ENTRY        PLIST                                                  *
007800000000     C                   PARM                    KPJBA                          *
007900980522     c                   parm                    tibs02
008000981029     c* c= chiusura file
008100981029     c     t02tla        ifne      'C'
008200890429     C                   Z-ADD     1             CODUT
008300000000     C                   CALL      'XPARUT'
008400000000     C                   PARM                    PARUT
008500000000     C                   MOVEL     RAGUT         RSUT             20
008600971016     C*
008700980526?    C* Controllo se e' stata richiesto CONTROLLO O RICERCA TABELLA
008800980522     c* CONTROLLO
008900980522     c                   if        t02mod='C'
009000980522     c                   exsr      SUB_CONTR
009100980526     C                   else
009200990104     c  n99              open      tibs02d
009300990104     c  n99              seton                                        99
009400980522     c* RICERCA
009500980522     c                   exsr      SUB_RICER
009600980525     c* gestione
009700120716     c                   dou       *inKL = *on
009800980525     c                   exsr      SUB_gesbf
009900120716     c                   enddo
010000020215     c* se il flag di 'nessuna' selezione Ë acceso segnalo l'errore
010100020215     c                   if        t02err = ' ' and $NoSel =*on
010200980526     c                   move      'E'           t02err
010300980526     c                   move      msg(4)        t02msg
010400980526     c                   endif
010500980526     c                   endif
010600981029     c                   endif
010700980522     c*
010800981029     c     t02tla        ifne      ' '
010900980526     C                   eval      *inlr = *on                                       *
011000981029     c                   else
011100981029     C                   eval      *inrt = *on                                       *
011200981029     c                   endif
011300980522     C*-------------------------------------------------------------------------*
011400980522     c* SUB_CONTR Routine di Controllo
011500980522     C*-------------------------------------------------------------------------*
011600980522     c     SUB_CONTR     BEGSR
011700981030     c* verifico se esiste con il mio sistema informativo
011800981030     C* testata tabella
011900981030     c                   clear                   kcod
012000981030     c                   movel     *zeros        kke1
012100981030     c                   move      t02cod        kke1
012200981030     c                   clear                   kke2
012300981030     c                   move      t02lin        klin
012400981030     c                   move      t02sif        ksif
012500060531     c     ktbe1         chain     tntbe11l                           90
012600981102     c* imposto la chiave con il mio sistema informativo
012700981102     c                   move      t02cod        kcod
012800981102     c                   move      t02ke1        kke1
012900981102     c                   move      t02ke2        kke2
013000981102     c                   move      t02lin        klin
013100981030     c* se esiste in testata cerco la mia tabella solo con il sistema informativo passato
013200981030     c                   if        not *in90
013300980522     c* imposto la chiave con il mio sistema informativo
013400980522     c                   move      t02sif        ksif
013500060531     c     ktbe1         chain     tntbe11l                           30
013600981030     c*
013700981030     c* se non esiste la testata con il mio sistema informativo cerco la tabella con il sistema
013800981030     c* informativo a blank
013900981030     c                   else
014000981030     c                   clear                   ksif
014100060531     c     ktbe1         chain     tntbe11l                           30
014200981030     c                   endif
014300060531
014400060531      * se Ë stata richiesta una tabella in lingua e non la trovo imposto errore e cerco i dati in
014500060531      * italiano
014600060531     c                   if        klin <> *blanks and
014700060531     c                             (*in30 or (not *in30 and tbeatb <> *blanks))
014800060531     c                   eval      t02err = 'W'
014900060531     c                   eval      t02msg = msg(05)
015000060531     c                   clear                   klin
015100060531     c     ktbe1         chain     tntbe11l                           30
015200060531     c                   endif
015300060531
015400981030     c* se non trovato  ERRORE
015500981030     c                   if        *in30=*on
015600980522     c                   move      'E'           t02err
015700980522     c                   move      msg(1)        t02msg
015800980522     c                   endif
015900981030     c* se trovato ma annullato
016000981030     c                   if        *in30=*off and tbeatb <> *blank
016100980522     c                   move      'A'           t02err
016200980522     c                   move      msg(2)        t02msg
016300980522     c                   endif
016400980522     c* se non c'Ë errore passo il record
016500980522     c                   if        t02err <> 'E'
016600980522     c                   move      tbeatb        t02atb
016700980522     c                   move      tbesif        t02sif
016800980522     c                   move      tbelin        t02lin
016900980522     c                   move      tbecod        t02cod
017000980522     c                   move      tbeke1        t02ke1
017100980522     c                   move      tbeke2        t02ke2
017200980522     c                   move      tbeuni        t02uni
017300980522     c                   move      tbeftt        t02ftt
017400980522     c                   move      tbeflt        t02flt
017500980522     c                   move      tbeftr        t02ftr
017600980522     c                   z-add     tbedtr        t02dtr
017700980522     c                   endif
017800980522     c*
017900980522     c                   endsr
018000980522     C*-------------------------------------------------------------------------*
018100980522     c* SUB_RICER Routine di Ricerca
018200980522     C*-------------------------------------------------------------------------*
018300980525     c     SUB_RICER     BEGSR
018400980522     C*
018500980526     c* definisco i campi del control
018600980526     c                   move      *blanks       kcod
018700980526     c                   move      t02lin        klin
018800980526     c                   move      t02sif        ksif
018900980526     c                   movel     *zeros        kke1
019000980526     c                   move      t02cod        kke1
019100980526     c                   move      *blanks       kke2
019200060531     C     Ktbe1         chain     tntbe11l                           31
019300980526     c* se non l'ho trovato cerco senza il sistema informativo specifico
019400980526     c                   if        *in31 = *on
019500980526     c                   move      *blanks       ksif
019600060531     C     Ktbe1         chain     tntbe11l                           31
019700980526     c                   endif
019800980526     c*
019900980526     c                   if        *in31 = *off
020000980526     c                   move      tbeke1        v2crcd
020100980526     c                   move      tbeapl        v2crap
020200980526     c                   movel     tbeuni        v2crun
020300980526     c                   endif
020400120716     c*
020500120716     c                   clear                   C1Cke1
020600120716     c                   clear                   C1Cke2
020700120716     c                   clear                   BS02W01
020800120716     c*
020900981030     c* imposto la chiave a parte il sistema informativo che rimane uguale all'ultima
021000981030     c* chain fatta
021100980525     c                   move      t02cod        kcod
021200980525     c                   move      t02lin        klin
021300020211     c                   move      *blanks       kke1
021400020211     c                   movel     t02ke1        kke1
021500020211     c                   move      *blanks       kke2
021600020226     c                   movel     t02ke2        kke2
021700120716     c*
021800120716     c* Caricamento subfile
021900120716     c                   exsr      sr_CarSFL
022000120716     c*
022100120716     c                   ENDSR
022200881025     C*
022300120716     C*-------------------------------------------------------------------------*
022400980525     C*-------- CARICA SUBFILE ----------------------------*
022500981030     C*                        SISTEMA INFORMATIVO  VALORIZZATO come da testata tabella
022600120716     C*-------------------------------------------------------------------------*
022700120716     c     sr_CarSFL     BEGSR
022800120716     c*
022900120716     c                   eval      SavC1Cke1 = C1Cke1
023000120716     c                   eval      SavC1Cke2 = C1Cke2
023100120716     c                   eval      SavW1Cdes = W1Cdes
023200120716     c*
023300120716     C                   Z-ADD     0             nrr2              4 0          *
023400120716     C                   Z-ADD     1             CURS2                          *
023500120716     c* pulisco il subfile
023600120716     c                   seton                                        70
023700120716     c                   write     bs02c01
023800120716     c                   setoff                                       70  72
023900020211      *
024000020211      * Posizionamento a seconda delle chiavi passate
024100020211     C     Kke2          ifne      *blanks
024200060531     C     Ktbe4         SETLL     tntbe12l                                     *
024300020211     C                   else
024400060531     C     Ktbe3         SETLL     tntbe12l                                     *
024500020211     C                   endif
024600020211      *
024700060531     C     Ktbe2         READE     tntbe12l                               30
024800000000     C*
024900971021     C     *in30         DOweq     *off
025000980525     c                   if        tbeatb = ' '
025100120716     c                   if        (C1Cke1 = *blank  or  TBEke1 >= C1Cke1)  and
025200120716     c                             (C1Cke2 = *blank  or  TBEke2 >= C1Cke2)
025300120716     c                   If        W1Cdes = *blank  or
025400120716     c                             %scan( %trim( W1Cdes ) :
025500120716     c                               %xlate( c_Minusc : c_Maiusc : TBEuni ) )
025600120716     c                                 > *zero
025700971021     C                   ADD       1             nrr2                           *
025800971015     c                   clear                   v2copz
025900980525     C                   MOVE      tbeke1        v2cke1                         *
026000980525     C                   MOVE      tbeke2        v2cke2                         *
026100971015     C                   MOVEL     tbeuni        v2cdes                         *
026200971016     C                   MOVEL     tbelin        v2clin                         *
026300971016     C                   MOVEL     tbesif        v2hsif                         *
026400971020     C                   MOVE      tbesif        v2ccsf                         *
026500980526     C                   WRITE     bs02s01                                      *
026600120716     c                   EndIf
026700120716     c                   endif
026800971021     c                   endif
026900971015     c**
027000060531     C     Ktbe2         READE     tntbe12l                               30
027100971015     C                   ENDdo                                                  *
027200120716     c*
027300120716     c                   eval      *in72 = *on
027400120716     c                   eval      *in41 = (W1Cdes <> *blank)
027500980525     c*
027600980525     c                   endsr
027700980525     C*-------------------------------------------------------------------------*
027800980525     c* SUB_GESBF Routine Gestione Subfile
027900980525     C*-------------------------------------------------------------------------*
028000980525     c     SUB_GESBF     BEGSR
028100980525     C*
028200971015     C* nessun record caricato
028300980526    1c                   if        nrr2 = 0
028400980525     c                   eval      *in71 = *off
028500980526     c* errore
028600980526     c                   move      'E'           t02err
028700980526     c                   move      msg(3)        t02msg
028800980526     c
028900980526    xc                   else
029000980525     c                   eval      *in71 = *on
029100980526    1c                   endif
029200000000     C*
029300980526     C                   WRITE     bs02z01                                      *
029400980526     C                   WRITE     bs02t01                                      *
029500981030?   1C                   do        *hival                                       *
029600971015     c* sfl controlo
029700980526     C                   EXFMT     bs02c01                                      *
029800020215     C                   move      *off          $NoSel            1
029900981030     c* F12= fine lavoro
030000120716    2C                   if        *inkl = *on
030100120716     C                   move      *on           $NoSel
030200120716     c                   leavesr
030300120716    2c                   endif
030400120716     c* F4=Ricerca
030500120716     c                   if        *inKD
030600120716     c                   exsr      sr_GesW01
030700120716     c                   leavesr
030800120716     c                   endif
030900971015     c*
031000971028     c* per 71 on non leggo perche' non ci sono record
031100980526     c   71              readc     bs02s01                                30
031200980526    3c   71              dow       *in30 = *off
031300971016     c* imposto la key
031400980526    4c                   if        v2copz = '1'
031500980526     c                   move      v2crcd        kcod
031600980526     c                   movel     v2cke1        kke1
031700980526     c                   move      v2cke2        kke2
031800971016     c                   move      v2clin        klin
031900971016     c                   movel     v2hsif        ksif
032000980526     c* aggancio il record selezionato
032100060531     c     ktbe1         chain     tntbe11l                           31
032200980526     c                   move      tbeatb        t02atb
032300980526     c                   move      tbesif        t02sif
032400980526     c                   move      tbelin        t02lin
032500980526     c                   move      tbeapl        t02apl
032600980526     c                   move      tbecod        t02cod
032700980526     c                   move      tbeke1        t02ke1
032800980526     c                   move      tbeke2        t02ke2
032900980526     c                   move      tbeuni        t02uni
033000980526     c                   move      tbeftt        t02ftt
033100980526     c                   move      tbeflt        t02flt
033200980526     c                   move      tbeftr        t02ftr
033300980526     c                   z-add     tbedtr        t02dtr
033400980526     c                   move      ' '           t02err
033500980526     c                   move      *blanks       t02msg
033600980526     c                   eval      *in30 = *on
033700980526     c                   eval      *inkl = *on
033800120716     c                   leavesr
033900980526   x4c                   else
034000980526     c                   readc     bs02s01                                30
034100980526     c
034200980526    4c                   endif
034300980526    3C                   enddo
034400120716     c*
034500120716     c                   if        C1Cke1 <> SavC1Cke1  or
034600120716     c                             C1Cke2 <> SavC1Cke2
034700120716     c                   exsr      sr_CarSFL
034800120716     c                   leavesr
034900120716     c                   endif
035000120716     c*
035100120716?   1C                   enddo                                                  *do  hival
035200971021     c**
035300971016     c                   endsr
035400120716      *-------------------------------------------------------------------------*
035500120716      * sr_GESW01 Routine Gestione Window
035600120716      *-------------------------------------------------------------------------*
035700120716     c     sr_GESW01     BEGSR
035800120716      *
035900120716     c                   exfmt     BS02W01
036000120716      *
036100120716     c                   if        *inKL
036200120716     c                   eval      W1Cdes = SavW1Cdes
036300120716     c                   eval      *inKL  = *off
036400120716     c                   leavesr
036500120716     c                   endif
036600120716      *
036700120716     c                   exsr      sr_CarSFL
036800120716     c                   eval      *in71 = (nrr2 > 0)
036900120716     c                   WRITE     bs02z01
037000120716     c                   WRITE     bs02t01
037100120716      *
037200120716     c                   ENDSR
037300980526**
037400980526Tabella inesistente
037500980526Tabella annullato
037600980526Errore non trovati dati per questa tabella
037700980526Non e' stato selezionato nulla
037800060531Tabella in lingua inesistente passata tabella in italiano
