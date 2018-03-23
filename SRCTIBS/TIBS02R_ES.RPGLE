000100120920     H DEBUG DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
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
008200971016     C*
008300980526?    C* Controllo se e' stata richiesto CONTROLLO O RICERCA TABELLA
008400980522     c* CONTROLLO
008500980522     c                   if        t02mod='C'
008600980522     c                   exsr      SUB_CONTR
008700980526     C                   else
008800120920     c
008900120920     c*Solo una volta ...
009000120920     c                   if        not *in99
009100120920     C                   Z-ADD     1             CODUT
009200120920     C                   CALL      'XPARUT'
009300120920     C                   PARM                    PARUT
009400120920     C                   MOVEL     RAGUT         RSUT             20
009500120920     c
009600120920     c                   open      tibs02d
009700120920     c                   seton                                        99
009800120920     c                   endif
009900120920     c
010000980522     c* RICERCA
010100980522     c                   exsr      SUB_RICER
010200980525     c* gestione
010300120716     c                   dou       *inKL = *on
010400980525     c                   exsr      SUB_gesbf
010500120716     c                   enddo
010600020215     c* se il flag di 'nessuna' selezione Ë acceso segnalo l'errore
010700020215     c                   if        t02err = ' ' and $NoSel =*on
010800980526     c                   move      'E'           t02err
010900980526     c                   move      msg(4)        t02msg
011000980526     c                   endif
011100980526     c                   endif
011200981029     c                   endif
011300980522     c*
011400981029     c     t02tla        ifne      ' '
011500980526     C                   eval      *inlr = *on                                       *
011600981029     c                   else
011700981029     C                   eval      *inrt = *on                                       *
011800981029     c                   endif
011900980522     C*-------------------------------------------------------------------------*
012000980522     c* SUB_CONTR Routine di Controllo
012100980522     C*-------------------------------------------------------------------------*
012200980522     c     SUB_CONTR     BEGSR
012300981030     c* verifico se esiste con il mio sistema informativo
012400981030     C* testata tabella
012500981030     c                   clear                   kcod
012600981030     c                   movel     *zeros        kke1
012700981030     c                   move      t02cod        kke1
012800981030     c                   clear                   kke2
012900981030     c                   move      t02lin        klin
013000981030     c                   move      t02sif        ksif
013100060531     c     ktbe1         chain     tntbe11l                           90
013200981102     c* imposto la chiave con il mio sistema informativo
013300981102     c                   move      t02cod        kcod
013400981102     c                   move      t02ke1        kke1
013500981102     c                   move      t02ke2        kke2
013600981102     c                   move      t02lin        klin
013700981030     c* se esiste in testata cerco la mia tabella solo con il sistema informativo passato
013800981030     c                   if        not *in90
013900980522     c* imposto la chiave con il mio sistema informativo
014000120920     c                   move      t02sif        ksif
014100120920     c     ktbe1         chain     tntbe11l                           30
014200981030     c*
014300981030     c* se non esiste la testata con il mio sistema informativo cerco la tabella con il sistema
014400981030     c* informativo a blank
014500981030     c                   else
014600981030     c                   clear                   ksif
014700060531     c     ktbe1         chain     tntbe11l                           30
014800981030     c                   endif
014900060531
015000060531      * se Ë stata richiesta una tabella in lingua e non la trovo imposto errore e cerco i dati in
015100060531      * italiano
015200060531     c                   if        klin <> *blanks and
015300060531     c                             (*in30 or (not *in30 and tbeatb <> *blanks))
015400060531     c                   eval      t02err = 'W'
015500060531     c                   eval      t02msg = msg(05)
015600060531     c                   clear                   klin
015700060531     c     ktbe1         chain     tntbe11l                           30
015800060531     c                   endif
015900060531
016000981030     c* se non trovato  ERRORE
016100981030     c                   if        *in30=*on
016200980522     c                   move      'E'           t02err
016300980522     c                   move      msg(1)        t02msg
016400980522     c                   endif
016500981030     c* se trovato ma annullato
016600981030     c                   if        *in30=*off and tbeatb <> *blank
016700980522     c                   move      'A'           t02err
016800980522     c                   move      msg(2)        t02msg
016900980522     c                   endif
017000980522     c* se non c'Ë errore passo il record
017100980522     c                   if        t02err <> 'E'
017200980522     c                   move      tbeatb        t02atb
017300980522     c                   move      tbesif        t02sif
017400980522     c                   move      tbelin        t02lin
017500980522     c                   move      tbecod        t02cod
017600980522     c                   move      tbeke1        t02ke1
017700980522     c                   move      tbeke2        t02ke2
017800980522     c                   move      tbeuni        t02uni
017900980522     c                   move      tbeftt        t02ftt
018000980522     c                   move      tbeflt        t02flt
018100980522     c                   move      tbeftr        t02ftr
018200980522     c                   z-add     tbedtr        t02dtr
018300980522     c                   endif
018400980522     c*
018500980522     c                   endsr
018600980522     C*-------------------------------------------------------------------------*
018700980522     c* SUB_RICER Routine di Ricerca
018800980522     C*-------------------------------------------------------------------------*
018900980525     c     SUB_RICER     BEGSR
019000980522     C*
019100980526     c* definisco i campi del control
019200980526     c                   move      *blanks       kcod
019300980526     c                   move      t02lin        klin
019400980526     c                   move      t02sif        ksif
019500980526     c                   movel     *zeros        kke1
019600980526     c                   move      t02cod        kke1
019700980526     c                   move      *blanks       kke2
019800060531     C     Ktbe1         chain     tntbe11l                           31
019900980526     c* se non l'ho trovato cerco senza il sistema informativo specifico
020000980526     c                   if        *in31 = *on
020100980526     c                   move      *blanks       ksif
020200060531     C     Ktbe1         chain     tntbe11l                           31
020300980526     c                   endif
020400980526     c*
020500980526     c                   if        *in31 = *off
020600980526     c                   move      tbeke1        v2crcd
020700980526     c                   move      tbeapl        v2crap
020800980526     c                   movel     tbeuni        v2crun
020900980526     c                   endif
021000120716     c*
021100120716     c                   clear                   C1Cke1
021200120716     c                   clear                   C1Cke2
021300120716     c                   clear                   BS02W01
021400120716     c*
021500981030     c* imposto la chiave a parte il sistema informativo che rimane uguale all'ultima
021600981030     c* chain fatta
021700980525     c                   move      t02cod        kcod
021800980525     c                   move      t02lin        klin
021900020211     c                   move      *blanks       kke1
022000020211     c                   movel     t02ke1        kke1
022100020211     c                   move      *blanks       kke2
022200020226     c                   movel     t02ke2        kke2
022300120716     c*
022400120716     c* Caricamento subfile
022500120716     c                   exsr      sr_CarSFL
022600120716     c*
022700120716     c                   ENDSR
022800881025     C*
022900120716     C*-------------------------------------------------------------------------*
023000980525     C*-------- CARICA SUBFILE ----------------------------*
023100981030     C*                        SISTEMA INFORMATIVO  VALORIZZATO come da testata tabella
023200120716     C*-------------------------------------------------------------------------*
023300120716     c     sr_CarSFL     BEGSR
023400120716     c*
023500120716     c                   eval      SavC1Cke1 = C1Cke1
023600120716     c                   eval      SavC1Cke2 = C1Cke2
023700120716     c                   eval      SavW1Cdes = W1Cdes
023800120716     c*
023900120716     C                   Z-ADD     0             nrr2              4 0          *
024000120716     C                   Z-ADD     1             CURS2                          *
024100120716     c* pulisco il subfile
024200120716     c                   seton                                        70
024300120716     c                   write     bs02c01
024400120716     c                   setoff                                       70  72
024500020211      *
024600020211      * Posizionamento a seconda delle chiavi passate
024700020211     C     Kke2          ifne      *blanks
024800060531     C     Ktbe4         SETLL     tntbe12l                                     *
024900020211     C                   else
025000060531     C     Ktbe3         SETLL     tntbe12l                                     *
025100020211     C                   endif
025200020211      *
025300060531     C     Ktbe2         READE     tntbe12l                               30
025400000000     C*
025500971021     C     *in30         DOweq     *off
025600980525     c                   if        tbeatb = ' '
025700120716     c                   if        (C1Cke1 = *blank  or  TBEke1 >= C1Cke1)  and
025800120716     c                             (C1Cke2 = *blank  or  TBEke2 >= C1Cke2)
025900120716     c                   If        W1Cdes = *blank  or
026000120716     c                             %scan( %trim( W1Cdes ) :
026100120716     c                               %xlate( c_Minusc : c_Maiusc : TBEuni ) )
026200120716     c                                 > *zero
026300971021     C                   ADD       1             nrr2                           *
026400971015     c                   clear                   v2copz
026500980525     C                   MOVE      tbeke1        v2cke1                         *
026600980525     C                   MOVE      tbeke2        v2cke2                         *
026700971015     C                   MOVEL     tbeuni        v2cdes                         *
026800971016     C                   MOVEL     tbelin        v2clin                         *
026900971016     C                   MOVEL     tbesif        v2hsif                         *
027000971020     C                   MOVE      tbesif        v2ccsf                         *
027100980526     C                   WRITE     bs02s01                                      *
027200120716     c                   EndIf
027300120716     c                   endif
027400971021     c                   endif
027500971015     c**
027600060531     C     Ktbe2         READE     tntbe12l                               30
027700971015     C                   ENDdo                                                  *
027800120716     c*
027900120716     c                   eval      *in72 = *on
028000120716     c                   eval      *in41 = (W1Cdes <> *blank)
028100980525     c*
028200980525     c                   endsr
028300980525     C*-------------------------------------------------------------------------*
028400980525     c* SUB_GESBF Routine Gestione Subfile
028500980525     C*-------------------------------------------------------------------------*
028600980525     c     SUB_GESBF     BEGSR
028700980525     C*
028800971015     C* nessun record caricato
028900980526    1c                   if        nrr2 = 0
029000980525     c                   eval      *in71 = *off
029100980526     c* errore
029200980526     c                   move      'E'           t02err
029300980526     c                   move      msg(3)        t02msg
029400980526     c
029500980526    xc                   else
029600980525     c                   eval      *in71 = *on
029700980526    1c                   endif
029800000000     C*
029900980526     C                   WRITE     bs02z01                                      *
030000980526     C                   WRITE     bs02t01                                      *
030100981030?   1C                   do        *hival                                       *
030200971015     c* sfl controlo
030300980526     C                   EXFMT     bs02c01                                      *
030400020215     C                   move      *off          $NoSel            1
030500981030     c* F12= fine lavoro
030600120716    2C                   if        *inkl = *on
030700120716     C                   move      *on           $NoSel
030800120716     c                   leavesr
030900120716    2c                   endif
031000120716     c* F4=Ricerca
031100120716     c                   if        *inKD
031200120716     c                   exsr      sr_GesW01
031300120716     c                   leavesr
031400120716     c                   endif
031500971015     c*
031600971028     c* per 71 on non leggo perche' non ci sono record
031700980526     c   71              readc     bs02s01                                30
031800980526    3c   71              dow       *in30 = *off
031900971016     c* imposto la key
032000980526    4c                   if        v2copz = '1'
032100980526     c                   move      v2crcd        kcod
032200980526     c                   movel     v2cke1        kke1
032300980526     c                   move      v2cke2        kke2
032400971016     c                   move      v2clin        klin
032500971016     c                   movel     v2hsif        ksif
032600980526     c* aggancio il record selezionato
032700060531     c     ktbe1         chain     tntbe11l                           31
032800980526     c                   move      tbeatb        t02atb
032900980526     c                   move      tbesif        t02sif
033000980526     c                   move      tbelin        t02lin
033100980526     c                   move      tbeapl        t02apl
033200980526     c                   move      tbecod        t02cod
033300980526     c                   move      tbeke1        t02ke1
033400980526     c                   move      tbeke2        t02ke2
033500980526     c                   move      tbeuni        t02uni
033600980526     c                   move      tbeftt        t02ftt
033700980526     c                   move      tbeflt        t02flt
033800980526     c                   move      tbeftr        t02ftr
033900980526     c                   z-add     tbedtr        t02dtr
034000980526     c                   move      ' '           t02err
034100980526     c                   move      *blanks       t02msg
034200980526     c                   eval      *in30 = *on
034300980526     c                   eval      *inkl = *on
034400120716     c                   leavesr
034500980526   x4c                   else
034600980526     c                   readc     bs02s01                                30
034700980526     c
034800980526    4c                   endif
034900980526    3C                   enddo
035000120716     c*
035100120716     c                   if        C1Cke1 <> SavC1Cke1  or
035200120716     c                             C1Cke2 <> SavC1Cke2
035300120716     c                   exsr      sr_CarSFL
035400120716     c                   leavesr
035500120716     c                   endif
035600120716     c*
035700120716?   1C                   enddo                                                  *do  hival
035800971021     c**
035900971016     c                   endsr
036000120716      *-------------------------------------------------------------------------*
036100120716      * sr_GESW01 Routine Gestione Window
036200120716      *-------------------------------------------------------------------------*
036300120716     c     sr_GESW01     BEGSR
036400120716      *
036500120716     c                   exfmt     BS02W01
036600120716      *
036700120716     c                   if        *inKL
036800120716     c                   eval      W1Cdes = SavW1Cdes
036900120716     c                   eval      *inKL  = *off
037000120716     c                   leavesr
037100120716     c                   endif
037200120716      *
037300120716     c                   exsr      sr_CarSFL
037400120716     c                   eval      *in71 = (nrr2 > 0)
037500120716     c                   WRITE     bs02z01
037600120716     c                   WRITE     bs02t01
037700120716      *
037800120716     c                   ENDSR
037900980526**
038000980526Tabella inesistente
038100980526Tabella annullato
038200980526Errore non trovati dati per questa tabella
038300980526Non e' stato selezionato nulla
038400060531Tabella in lingua inesistente passata tabella in italiano
