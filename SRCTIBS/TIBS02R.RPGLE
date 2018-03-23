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
002800160829     D MSG             S             78    DIM(06) CTDATA PERRCD(1)             MSG ERRORI
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
011700170316     c*
011800170316     c* inizializzo i dati di output
011900170316     c                   clear                   t02uni
012000170316     c                   clear                   t02ftt
012100170316     c                   clear                   t02flt
012200170316     c                   clear                   t02ftr
012300170316     c                   clear                   t02dtr
012400170316     c                   clear                   t02err
012500170316     c                   clear                   t02msg
012600981030     c* verifico se esiste con il mio sistema informativo
012700981030     C* testata tabella
012800981030     c                   clear                   kcod
012900981030     c                   movel     *zeros        kke1
013000981030     c                   move      t02cod        kke1
013100981030     c                   clear                   kke2
013200981030     c                   move      t02lin        klin
013300981030     c                   move      t02sif        ksif
013400060531     c     ktbe1         chain     tntbe11l                           90
013500981102     c* imposto la chiave con il mio sistema informativo
013600981102     c                   move      t02cod        kcod
013700981102     c                   move      t02ke1        kke1
013800981102     c                   move      t02ke2        kke2
013900981102     c                   move      t02lin        klin
014000981030     c* se esiste in testata cerco la mia tabella solo con il sistema informativo passato
014100981030     c                   if        not *in90
014200980522     c* imposto la chiave con il mio sistema informativo
014300980522     c                   move      t02sif        ksif
014400060531     c     ktbe1         chain     tntbe11l                           30
014500981030     c*
014600981030     c* se non esiste la testata con il mio sistema informativo cerco la tabella con il sistema
014700981030     c* informativo a blank
014800981030     c                   else
014900981030     c                   clear                   ksif
015000060531     c     ktbe1         chain     tntbe11l                           30
015100981030     c                   endif
015200060531
015300060531      * se Ë stata richiesta una tabella in lingua e non la trovo imposto errore e cerco i dati in
015400060531      * italiano
015500060531     c                   if        klin <> *blanks and
015600060531     c                             (*in30 or (not *in30 and tbeatb <> *blanks))
015700060531     c                   eval      t02err = 'W'
015800060531     c                   eval      t02msg = msg(05)
015900060531     c                   clear                   klin
016000060531     c     ktbe1         chain     tntbe11l                           30
016100060531     c                   endif
016200060531
016300981030     c* se non trovato  ERRORE
016400981030     c                   if        *in30=*on
016500980522     c                   move      'E'           t02err
016600980522     c                   move      msg(1)        t02msg
016700980522     c                   endif
016800981030     c* se trovato ma annullato
016900981030     c                   if        *in30=*off and tbeatb <> *blank
017000980522     c                   move      'A'           t02err
017100980522     c                   move      msg(2)        t02msg
017200980522     c                   endif
017300980522     c* se non c'Ë errore passo il record
017400980522     c                   if        t02err <> 'E'
017500980522     c                   move      tbeatb        t02atb
017600980522     c                   move      tbesif        t02sif
017700980522     c                   move      tbelin        t02lin
017800980522     c                   move      tbecod        t02cod
017900980522     c                   move      tbeke1        t02ke1
018000980522     c                   move      tbeke2        t02ke2
018100980522     c                   move      tbeuni        t02uni
018200980522     c                   move      tbeftt        t02ftt
018300980522     c                   move      tbeflt        t02flt
018400980522     c                   move      tbeftr        t02ftr
018500980522     c                   z-add     tbedtr        t02dtr
018600980522     c                   endif
018700980522     c*
018800980522     c                   endsr
018900980522     C*-------------------------------------------------------------------------*
019000980522     c* SUB_RICER Routine di Ricerca
019100980522     C*-------------------------------------------------------------------------*
019200980525     c     SUB_RICER     BEGSR
019300980522     C*
019400980526     c* definisco i campi del control
019500980526     c                   move      *blanks       kcod
019600980526     c                   move      t02lin        klin
019700980526     c                   move      t02sif        ksif
019800980526     c                   movel     *zeros        kke1
019900980526     c                   move      t02cod        kke1
020000980526     c                   move      *blanks       kke2
020100060531     C     Ktbe1         chain     tntbe11l                           31
020200980526     c* se non l'ho trovato cerco senza il sistema informativo specifico
020300980526     c                   if        *in31 = *on
020400980526     c                   move      *blanks       ksif
020500060531     C     Ktbe1         chain     tntbe11l                           31
020600980526     c                   endif
020700980526     c*
020800980526     c                   if        *in31 = *off
020900980526     c                   move      tbeke1        v2crcd
021000980526     c                   move      tbeapl        v2crap
021100980526     c                   movel     tbeuni        v2crun
021200980526     c                   endif
021300120716     c*
021400120716     c                   clear                   C1Cke1
021500120716     c                   clear                   C1Cke2
021600120716     c                   clear                   BS02W01
021700120716     c*
021800981030     c* imposto la chiave a parte il sistema informativo che rimane uguale all'ultima
021900981030     c* chain fatta
022000980525     c                   move      t02cod        kcod
022100980525     c                   move      t02lin        klin
022200020211     c                   move      *blanks       kke1
022300020211     c                   movel     t02ke1        kke1
022400020211     c                   move      *blanks       kke2
022500020226     c                   movel     t02ke2        kke2
022600120716     c*
022700120716     c* Caricamento subfile
022800120716     c                   exsr      sr_CarSFL
022900120716     c*
023000120716     c                   ENDSR
023100881025     C*
023200120716     C*-------------------------------------------------------------------------*
023300980525     C*-------- CARICA SUBFILE ----------------------------*
023400981030     C*                        SISTEMA INFORMATIVO  VALORIZZATO come da testata tabella
023500120716     C*-------------------------------------------------------------------------*
023600120716     c     sr_CarSFL     BEGSR
023700120716     c*
023800120716     c                   eval      SavC1Cke1 = C1Cke1
023900120716     c                   eval      SavC1Cke2 = C1Cke2
024000120716     c                   eval      SavW1Cdes = W1Cdes
024100120716     c*
024200120716     C                   Z-ADD     0             nrr2              4 0          *
024300120716     C                   Z-ADD     1             CURS2                          *
024400120716     c* pulisco il subfile
024500120716     c                   seton                                        70
024600120716     c                   write     bs02c01
024700120716     c                   setoff                                       70  72
024800020211      *
024900020211      * Posizionamento a seconda delle chiavi passate
025000020211     C     Kke2          ifne      *blanks
025100060531     C     Ktbe4         SETLL     tntbe12l                                     *
025200020211     C                   else
025300060531     C     Ktbe3         SETLL     tntbe12l                                     *
025400020211     C                   endif
025500020211      *
025600060531     C     Ktbe2         READE     tntbe12l                               30
025700000000     C*
025800971021     C     *in30         DOweq     *off
025900980525     c                   if        tbeatb = ' '
026000120716     c                   if        (C1Cke1 = *blank  or  TBEke1 >= C1Cke1)  and
026100120716     c                             (C1Cke2 = *blank  or  TBEke2 >= C1Cke2)
026200120716     c                   If        W1Cdes = *blank  or
026300120716     c                             %scan( %trim( W1Cdes ) :
026400120716     c                               %xlate( c_Minusc : c_Maiusc : TBEuni ) )
026500120716     c                                 > *zero
026600160829     c                   if        Nrr2  >= *hival
026700160829     c                   eval      VIDmsg = Msg(06)
026800160829     c                   seton                                        28
026900160829     c                   leave
027000160829     c                   endif
027100971021     C                   ADD       1             nrr2                           *
027200971015     c                   clear                   v2copz
027300980525     C                   MOVE      tbeke1        v2cke1                         *
027400980525     C                   MOVE      tbeke2        v2cke2                         *
027500971015     C                   MOVEL     tbeuni        v2cdes                         *
027600971016     C                   MOVEL     tbelin        v2clin                         *
027700971016     C                   MOVEL     tbesif        v2hsif                         *
027800971020     C                   MOVE      tbesif        v2ccsf                         *
027900980526     C                   WRITE     bs02s01                                      *
028000120716     c                   EndIf
028100120716     c                   endif
028200971021     c                   endif
028300971015     c**
028400060531     C     Ktbe2         READE     tntbe12l                               30
028500971015     C                   ENDdo                                                  *
028600120716     c*
028700160829     c                   if        NOT *in28  and  Nrr2 <= 9999
028800120716     c                   eval      *in72 = *on
028900160829     c                   endif
029000120716     c                   eval      *in41 = (W1Cdes <> *blank)
029100980525     c*
029200980525     c                   endsr
029300980525     C*-------------------------------------------------------------------------*
029400980525     c* SUB_GESBF Routine Gestione Subfile
029500980525     C*-------------------------------------------------------------------------*
029600980525     c     SUB_GESBF     BEGSR
029700980525     C*
029800971015     C* nessun record caricato
029900980526    1c                   if        nrr2 = 0
030000980525     c                   eval      *in71 = *off
030100980526     c* errore
030200980526     c                   move      'E'           t02err
030300980526     c                   move      msg(3)        t02msg
030400980526     c
030500980526    xc                   else
030600980525     c                   eval      *in71 = *on
030700980526    1c                   endif
030800000000     C*
030900980526     C                   WRITE     bs02z01                                      *
031000980526     C                   WRITE     bs02t01                                      *
031100981030?   1C                   do        *hival                                       *
031200971015     c* sfl controlo
031300980526     C                   EXFMT     bs02c01                                      *
031400160829     c                   eval      *in28 = *off
031500020215     C                   move      *off          $NoSel            1
031600981030     c* F12= fine lavoro
031700120716    2C                   if        *inkl = *on
031800120716     C                   move      *on           $NoSel
031900120716     c                   leavesr
032000120716    2c                   endif
032100120716     c* F4=Ricerca
032200120716     c                   if        *inKD
032300120716     c                   exsr      sr_GesW01
032400120716     c                   leavesr
032500120716     c                   endif
032600971015     c*
032700971028     c* per 71 on non leggo perche' non ci sono record
032800980526     c   71              readc     bs02s01                                30
032900980526    3c   71              dow       *in30 = *off
033000971016     c* imposto la key
033100980526    4c                   if        v2copz = '1'
033200980526     c                   move      v2crcd        kcod
033300980526     c                   movel     v2cke1        kke1
033400980526     c                   move      v2cke2        kke2
033500971016     c                   move      v2clin        klin
033600971016     c                   movel     v2hsif        ksif
033700980526     c* aggancio il record selezionato
033800060531     c     ktbe1         chain     tntbe11l                           31
033900980526     c                   move      tbeatb        t02atb
034000980526     c                   move      tbesif        t02sif
034100980526     c                   move      tbelin        t02lin
034200980526     c                   move      tbeapl        t02apl
034300980526     c                   move      tbecod        t02cod
034400980526     c                   move      tbeke1        t02ke1
034500980526     c                   move      tbeke2        t02ke2
034600980526     c                   move      tbeuni        t02uni
034700980526     c                   move      tbeftt        t02ftt
034800980526     c                   move      tbeflt        t02flt
034900980526     c                   move      tbeftr        t02ftr
035000980526     c                   z-add     tbedtr        t02dtr
035100980526     c                   move      ' '           t02err
035200980526     c                   move      *blanks       t02msg
035300980526     c                   eval      *in30 = *on
035400980526     c                   eval      *inkl = *on
035500120716     c                   leavesr
035600980526   x4c                   else
035700980526     c                   readc     bs02s01                                30
035800980526     c
035900980526    4c                   endif
036000980526    3C                   enddo
036100120716     c*
036200120716     c                   if        C1Cke1 <> SavC1Cke1  or
036300120716     c                             C1Cke2 <> SavC1Cke2
036400120716     c                   exsr      sr_CarSFL
036500120716     c                   leavesr
036600120716     c                   endif
036700120716     c*
036800120716?   1C                   enddo                                                  *do  hival
036900971021     c**
037000971016     c                   endsr
037100120716      *-------------------------------------------------------------------------*
037200120716      * sr_GESW01 Routine Gestione Window
037300120716      *-------------------------------------------------------------------------*
037400120716     c     sr_GESW01     BEGSR
037500120716      *
037600120716     c                   exfmt     BS02W01
037700120716      *
037800120716     c                   if        *inKL
037900120716     c                   eval      W1Cdes = SavW1Cdes
038000120716     c                   eval      *inKL  = *off
038100120716     c                   leavesr
038200120716     c                   endif
038300120716      *
038400120716     c                   exsr      sr_CarSFL
038500120716     c                   eval      *in71 = (nrr2 > 0)
038600120716     c                   WRITE     bs02z01
038700120716     c                   WRITE     bs02t01
038800120716      *
038900120716     c                   ENDSR
039000980526**
039100980526Tabella inesistente
039200980526Tabella annullato
039300980526Errore non trovati dati per questa tabella
039400980526Non e' stato selezionato nulla
039500060531Tabella in lingua inesistente passata tabella in italiano
039600160829SUPERATO IL NUMERO MASSIMO DI RECORD VISUALIZZABILI NEL SUBFILE (9999)
