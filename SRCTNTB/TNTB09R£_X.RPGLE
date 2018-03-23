000100980909
000200980909      *------------------------------------------------------------------------*
000300000907      * GESTIONE TABELLA DEP : Dati DEPOT DPD                                  *
000400980909      *------------------------------------------------------------------------*
000500980909
000600980909      *--------------------------------------------
000700980909      *  RIEPILOGO INDICATORI
000800980909      *--------------------------------------------
000900981102      * 01 - RECORD annullato
001000991001      * 02 - acceso IMMISSIONE - spento MANUTENZIONE
001100990305      * 30 - Comodo
001200980909      *--------------------------------------------
001300980506     H DECEDIT('0,') DATEDIT(*DMY.)
001400980909
001500000907     FTNTB09D   CF   E             WORKSTN
001600000907     F                                     sfile(tn09sfl:nrr)
001700981102     FTNTBE01L  UF A E           K DISK
001800000622     FTNTBE02L  IF   E           K DISK    RENAME(TNTBE000:TNTBE002)
001900000620     FTABEL00F  IF   E           K DISK
002000981102      *--------------------------------------------
002100000907     d DDEP          E DS
002200000907     D MSG             S             78    DIM(6) CTDATA PERRCD(1)              MSG VIDEO
002300981102      *
002400981102     D Kttcod          S                   LIKE(TBEcod)
002500000907     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000DEP')
002600000907     D Kcod            S                   LIKE(TBEcod)  INZ('DEP')
002700981102     D Kke1            S                   LIKE(TBEke1)
002800981102     D Kke2            S                   LIKE(TBEke2)
002900981102     D Klin            S                   LIKE(TBElin)
003000981102     D Ksif            S                   LIKE(TBEsif)
003100000619      *
003200000620     D Kttkut          S                   LIKE(TBLkut)
003300000620     D Ktlcod          S                   LIKE(TBLcod)
003400000619     D Kttkey          S                   LIKE(TBLkey)
003500981102      *
003600980507     D KPJBA         E DS
003700000619     D DCPT          E DS
003800981102     D UT§DSE0F      E DS
003900000619      *********************************************
004000980506      * EMISSIONE PRIMA VIDEATA
004100000619      *********************************************
004200981117     C                   DO        *hival
004300000621      *
004400000707     C     emetti        tag
004500000907     C                   EXFMT     tn09D01
004600980930      *
004700980909     C                   clear                   V1CMSG
004800000619      *
004900000619     C                   eval      *in28 = *off
005000980506      * F3=FINE
005100000619     C   KC              leave
005200000622      *
005300000907     C     '?'           scan      v9cdep                                 21
005400000907     c     *in21         ifeq      *on
005500000622     c                   exsr      gessfl
005600000907      * F3=FINE
005700000907     C   KC              leave
005800000907     C   KL              clear                   v9cdep
005900091106     C   KL              clear                   v9chub
006000000707     C   KL              goto      emetti
006100000622     c                   endif
006200000622      *
006300000619      * CONTROLLI PRIMA VIDEATA
006400981102     C                   EXSR      CTRD01
006500981102      *
006600991001     C   90              iter
006700000621      *
006800000907      * GESTIONE SECONDA VIDEATA
006900000620     C                   EXSR      RIED02
007000000621      *
007100000907      * CONTROLLI SECONDA VIDEATA
007200000620     C                   EXSR      GESD02
007300000621      *
007400000907     c                   move      *blanks       v9cdep
007500091106     c                   move      *blanks       v9chub
007600000620      *
007700000621     C   90              iter
007800000620      *
007900000620     C                   enddo
008000981102      *
008100980506     C                   EVAL      *INLR = *ON
008200981102      *****************************************************************
008300981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
008400000621      *   controllo che i campi sono valorizzati, che il codice cliente
008500000621      *   sia esistente e che il punto operativo sia delle poste
008600981102      *****************************************************************
008700981102     C     CTRD01        BEGSR
008800981102      *
008900000907     c                   setoff                                       3090
009000000619      *
009100000907     c     v9cdep        ifeq      *blank
009200000907     c     v9cdep        oreq      *zeros
009300091106     c***  v9chub        oreq      *blank
009400091106     c***  v9chub        oreq      *zeros
009500000907     c                   seton                                        309028
009600000907     C                   movel     msg(1)        V1CMSG
009700000619     c                   endif
009800000707      *
009900000907     C   08*in90         ifeq      *off
010000000907     c                   movel(P)  V9cdep        Kke1
010100091106     c                   movel     v9cHUB        Kke2
010200000707     c     KTBE          CHAIN(N)  TNTBE000                           02
010300000707     C     *in02         ifeq      *off
010400000907     C                   seton                                        309028
010500000907     C                   movel     msg(2)        V1CMSG
010600000707     C                   endif
010700000707     C                   endif
010800000707      *
010900000707      *   Accesso a tntbe01l per verificare l'esistenza del record
011000000707      *   .. controllo per duplicazione (non puo' esistere già)
011100000907      *
011200000707     C   08*in90         ifeq      *off
011300000707      *
011400000707      *  emissione window per richiesta da chi duplicare
011500091106     C                   move      videpw        videpwCOM         4
011600091106     C                   move      vihubw        vihubwCOM         4
011700000907     C     wind          TAG
011800000907     C                   EXFMT     tn09WIN
011900000907     C                   clear                   V4CMSG           40
012000000907     C                   setoff                                       31
012100000907      * non deve permettere l'annullamento con lo 03 => ON
012200000907     C                   seton                                        03
012300000707     C   KL              seton                                        90
012400000907     C   KL              move      videpwCOM     videpw
012500091106     C   KL              move      vihubwCOM     vihubw
012600000707     C   KL              goto      escsub
012700000707      *
012800000907     c                   movel(P)  videpw        Kke1
012900091106     c                   movel(P)  vihubw        Kke2
013000000707     c     KTBE          CHAIN(N)  TNTBE000                           02
013100000707     C     *in02         ifeq      *on
013200000907     C                   seton                                        31
013300000907     C                   movel     msg(3)        V4CMSG
013400000907     C                   write     tn09WIN
013500000907     C                   goto      wind
013600000907      * emette l'errore
013700000707     C                   else
013800000707     C                   move      tbeuni        tbeuniWORK      256
013900000707     C                   endif
014000000707      *
014100000707     C                   endif
014200000619      *
014300000707     C     escsub        ENDSR
014400000620      *****************************************************************
014500000620      *   ROUTINE DI CARICAMENTO VIDEO DA FILE
014600000620      *****************************************************************
014700000620     C     RIED02        BEGSR
014800000620      *
014900000620     c                   clear                   TNTBE000
015000000907     c                   clear                   ddep
015100000621      *   Pulisco campi primo video e richiamo routine pulizia
015200000621      *   campi secodo video
015300000621     c                   exsr      pulizia
015400000622      *
015500000907     c                   movel(P)  V9cdep        Kke1
015600091106     c                   movel(P)  v9cHUB        Kke2
015700000620      *
015800000621      *   Accesso a tntbe01l per verificare l'esistenza del record
015900000620     c     KTBE          CHAIN     TNTBE000                           02
016000000620      *
016100000907      *   Per record trovato valorizzo la tabella dsDEP e quindi il video
016200000707     C     *in08         ifeq      *off
016300000907     c  N02              movel     TBEuni        Ddep
016400000907     c   02              clear                   ddep
016500000707     C                   else
016600000907     C                   movel     tbeuniWORK    ddep
016700000707     C                   endif
016800000621      *
016900000907     C                   movel     V9cdep        V9cdep2
017000091106     C                   movel     V9cHUB        V9cHUB2
017100000620      *
017200000620     C                   ENDSR
017300980506      *****************************************************************
017400000619      *   ROUTINE PER GESTIONE SECONDA VIDEATA
017500980506      *****************************************************************
017600000619     c     GESD02        BEGSR
017700980909      *
017800000619     c                   setoff                                       90
017900000623      *
018000000907     C                   movel     §depdes       v9cdes2
018100000907     C                   z-add     §depksc       v9cksc2
018200000907     C                   z-add     §deplnp       v9clnp2
018300000922     C                   z-add     §deptar       v9ctar2
018400051219     C                   movel     §depctm       v9ctm2
018500051219     C                   movel     §depwrk       v9edi2
018600000619      *
018700000619     c                   do        *hival
018800000619      *
018900000907     c                   exfmt     tn09d02
019000000620      *
019100000619     c                   eval      *in28 = *off
019200000619      * --------------
019300000619      * F3 = Fine
019400000619      *---------------
019500000619     c   kc              leave
019600000619      * --------------
019700000619      * F12 = Ritorno
019800000619      *---------------
019900000619     c     *inkl         ifeq      *on
020000000621     c                   seton                                        90
020100000621     c                   leave
020200000619     c                   endif
020300000619      *
020400000621      *    Richiamo routine di controllo secondo video
020500000619     c                   exsr      ctrd02
020600000619      *
020700000619     c   28              iter
020800000619      *
020900000621      *   Per F6 richiamo routine di aggiornamento o immissione
021000000907     c                   if        *inkf=*on  or *in16=*on
021100000619     c                   exsr      riecdd
021200000907     C                   eval      *in16=*off
021300000619     c                   leave
021400000619     c                   endif
021500000619      *
021600000619     c                   enddo
021700000619      *
021800000619     c                   endsr
021900000619      ********************************************************************
022000000619      *  ROUTINE DI CONTROLLO SECONDA VIDEATA
022100000619      ********************************************************************
022200000619     C     CTRD02        BEGSR
022300000619      *
022400000907     C                   clear                   V2CMSG
022500000907      *
022600000621     c                   setoff                                       505152
022700000907     c                   setoff                                       9028
022800000907      *
022900000621      *    Controllo se i campi sono valorizzati
023000000907     c     v9cdes2       ifeq      *blanks
023100000907     c                   seton                                        509028
023200000907     C                   movel     msg(1)        V2CMSG
023300000620     c                   endif
023400000620      *
023500000907     c     v9clnp2       ifeq      *zeros
023600000907     c                   seton                                        519028
023700000907     C                   movel     msg(1)        V2CMSG
023800000620     c                   endif
023900000620      *
024000000907     c     v9cksc2       ifeq      *zeros
024100000907     c                   seton                                        529028
024200000907     C                   movel     msg(1)        V2CMSG
024300000620     c                   endif
024400000619      *
024500980506     C                   ENDSR
024600980507      *****************************************************************
024700980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
024800980507      *****************************************************************
024900981117     C     RIECDD        BEGSR
025000980909      *
025100000621      *   Carico i campi del video già controllati nella tabella dscpt
025200000621      *
025300000907     c                   movel     V9cdes2       §depdes
025400000907     c                   movel     V9clnp2       §deplnp
025500000907     c                   movel     V9cksc2       §depksc
025600000922     c                   movel     V9ctar2       §deptar
025700051219     c                   movel     V9ctm2        §depctm
025800051219     c                   movel     V9edi2        §depwrk
025900000621      *
026000000907     c                   movel(p)  Ddep          TBEuni
026100981102      *
026200000619     c                   movel     'S'           TBEftt
026300000619     c                   movel     *blanks       TBEatb
026400980909      *
026500981102     c                   clear                   TBEftr
026600981102      *
026700000621      *   Se è un record già esistente aggiorno altrimenti scrivo su file
026800981102     c                   IF        *IN02 = *ON
026900000619     c                   z-add     0             TBEflt
027000991001     c                   clear                   TBEdtr
027100981102     c                   movel     Ksif          TBEsif
027200981102     c                   movel     Klin          TBElin
027300000914     c                   movel     'DP'          TBEapl
027400000914     c                   movel     Kcod          TBEcod
027500000907     C                   movel(p)  V9cdep        TBEke1
027600091106     C                   movel(p)  v9chub        TBEke2
027700981102     C                   WRITE     TNTBE000
027800981102      *
027900981102     c                   ELSE
028000000914     C   16              eval      TBEATB='A'
028100000914     C                   UPDATE    TNTBE000
028200981102     c                   ENDIF
028300980507      *
028400000907      *  riabilita l'annullamento se passato dalla window di copia
028500000907     C                   setoff                                       03
028600000907      *
028700980507     C                   ENDSR
028800000621      *****************************************************************
028900000621      *   ROUTINE DI PULIZIA SECONDA VIDEATA
029000000621      *****************************************************************
029100000621     C     PULIZIA       BEGSR
029200000621      *
029300000907     C                   movel     *blanks       V9cdes2
029400000907     C                   z-add     *zeros        V9cksc2
029500000907     C                   z-add     *zeros        V9clnp2
029600000922     C                   z-add     *zeros        V9ctar2
029700051219     C                   move      *blank        V9ctm2
029800051219     C                   move      'S'           V9edi2
029900000621      *
030000000621     C                   ENDSR
030100000622      *****************************************************************
030200000622      *   ROUTINE DI GESTIONE SUBFILE
030300000622      *****************************************************************
030400000622     C     GESSFL        BEGSR
030500000623      *
030600000622     c                   clear                   nrr               4 0
030700000623     c                   eval      *in23='0'
030800000622      *
030900000622     c                   eval      *in29='1'
031000000907     c                   write     tn09ctl
031100000622     c                   eval      *in29='0'
031200000622      *
031300000622     c     ktbe1         chain     tntbe002                           25
031400000622      *
031500000622     c                   dow       not *in25
031600000622      *
031700000622     c     tbeatb        ifeq      *blanks
031800000907     c                   movel     tbeke1        v9depsf
031900091106     c                   movel     tbeke2        v9HUBsf
032000000918      *
032100000907     c                   movel(p)  TBEuni        Ddep
032200000622      *
032300000907     c                   movel     §depdes       v9dessf
032400000907     c                   movel     §deplnp       v9lnpsf
032500000907     c                   movel     §depksc       v9kscsf
032600000922     c                   movel     §deptar       v9tarsf
032700051219     c                   movel     §depctm       v9ctmsf
032800051219     c                   movel     §depwrk       v9vabsf
032900000907      *
033000000622     c                   add       1             nrr
033100000907     c                   write     tn09sfl
033200000622      *
033300000622     c                   endif
033400000622      *
033500000622     c     ktbe1         reade     tntbe002                               25
033600000622      *
033700000622     c                   enddo
033800000622      *
033900000622     c                   exsr      gessf2
034000000622      *
034100000622     c                   endsr
034200000622      *****************************************************************
034300000622      *   ROUTINE gestione secondo video da subfile
034400000622      *****************************************************************
034500000622     C     GESSF2        BEGSR
034600000622      *
034700000622     c                   if        nrr>0
034800000622     c                   eval      nrr=1
034900000623     c                   eval      *in23='1'
035000000622     c                   endif
035100000622      *
035200000907     c                   write     tn09z03
035300000907     c                   exfmt     tn09ctl
035400000622      *
035500000907     c   KC
035600000907     cor KL              goto      esci
035700000622      *
035800000907     c   23              readc     tn09sfl                                04
035900000622      *
036000000623     c   23              dow       *in04=*off
036100000622      *
036200000907      * se ha scelto il record lo deve riportare sulla 2°videata
036300000907     c     v9opzsf       ifne      *blanks
036400000907     c                   movel     v9depsf       v9cdep
036500091106     c                   movel     v9hubsf       v9chub
036600000623     c                   exsr      ried02
036700000623     c                   eval      *in04=*on
036800000907     c                   move      *blanks       v9opzsf
036900000622     c                   else
037000000907      *      rilegge se non ha scelto nulla
037100000907     c                   readc     tn09sfl                                04
037200000622     c                   endif
037300000622      *
037400000622     c                   enddo
037500000622      *
037600000707     c     esci          ENDSR
037700980506      *****************************************************************
037800980506      *   ROUTINE INIZIALE
037900980506      *****************************************************************
038000980506     C     *INZSR        BEGSR
038100980506      *
038200980506     C     *ENTRY        PLIST
038300980506     C                   PARM                    KPJBA
038400981102      *
038500981102     C                   Z-ADD     1             CODUT
038600981102     C                   CALL      'X§PARUT'
038700981102     C                   PARM                    UT§DSE0F
038800981102     C                   MOVEL     RAGUT         RSUT
038900981102      *
039000981102      * Controllo Testata Tabella
039100000725     C     KTBETT        CHAIN(N)  TNTBE000                           28
039200981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
039300981117     C                   eval      *inlr = *on
039400981102     c                   ELSE
039500981102     c                   movel     TBEsif        Ksif
039600981102     c                   movel     TBElin        Klin
039700981102     c                   ENDIF
039800981102      *
039900981102     C     KTBE          klist
040000981102     C                   KFLD                    Kcod
040100981102     C                   KFLD                    Kke1
040200981102     C                   KFLD                    Kke2
040300981102     C                   KFLD                    Klin
040400981102     C                   KFLD                    Ksif
040500000619     c*                  clear                   Kke2
040600981102      *
040700981102     C     KTBETT        klist
040800981102     C                   KFLD                    KTTcod
040900981102     C                   KFLD                    KTTke1
041000981102      *
041100000619     C     KTTABL        klist
041200000619     C                   KFLD                    KTTkut
041300000620     C                   KFLD                    KTlcod
041400000619     C                   KFLD                    KTTkey
041500000622      *
041600000622     C     KTBE1         klist
041700000622     C                   KFLD                    Kcod
041800000622     C                   KFLD                    Klin
041900000622     C                   KFLD                    Ksif
042000000619      *
042100000907     C                   eval      videpw = *blank
042200000707      *
042300980506     C                   ENDSR
042400980909      *****************************************************************
042500980909** MSG  (Lungh. 78)                                                          *
042600000907Il dato è obbligatorio .                                                       1
042700000907Il codice esiste già e non può essere creato duplicandolo!                     2
042800000907Il codice da duplicare inesistente !                                           3
