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
008900120514     c                   setoff                                       88
009000000907     c                   setoff                                       3090
009100000619      *
009200000907     c     v9cdep        ifeq      *blank
009300000907     c     v9cdep        oreq      *zeros
009400000907     c                   seton                                        309028
009500000907     C                   movel     msg(1)        V1CMSG
009600000619     c                   endif
009700000707      *
009800000907     C   08*in90         ifeq      *off
009900000907     c                   movel(P)  V9cdep        Kke1
010000091106     c                   movel     v9cHUB        Kke2
010100000707     c     KTBE          CHAIN(N)  TNTBE000                           02
010200000707     C     *in02         ifeq      *off
010300000907     C                   seton                                        309028
010400000907     C                   movel     msg(2)        V1CMSG
010500000707     C                   endif
010600000707     C                   endif
010700000707      *
010800000707      *   Accesso a tntbe01l per verificare l'esistenza del record
010900000707      *   .. controllo per duplicazione (non puo' esistere già)
011000000907      *
011100000707     C   08*in90         ifeq      *off
011200120511      *
011300120511      *  quale tipo di duplica si vuole fare ?
011400120511      **  singolo record oppure tutti
011500120511      *
011600120514     c                   movel(P)  V9cdep        NEWCOD
011700120514     c                   move      'T'           scelta
011800120511     c     fai_UNAscelta tag
011900120511     c                   exfmt     TN09QUALE
012000120511      *
012100120511     c     *inKL         ifeq      *on
012200120511     c                   seton                                        90
012300120511     c                   leavesr
012400120511     c                   else
012500120514     c                   if        scelta= *blank or *inKC
012600120511     c                   goto      fai_UNAscelta
012700120511     c                   end
012800120511     c                   end
012900120511      ***
013000120511      *  x singolo codice di duplicazione
013100120511     c                   if        scelta='S'
013200120511      *
013300000707      *  emissione window per richiesta da chi duplicare
013400091106     C                   move      videpw        videpwCOM         4
013500091106     C                   move      vihubw        vihubwCOM         4
013600000907     C     wind          TAG
013700120511      **
013800000907     C                   EXFMT     tn09WIN
013900120514     C   KC              goto      wind
014000120511      **
014100000907     C                   clear                   V4CMSG           40
014200000907     C                   setoff                                       31
014300000907      * non deve permettere l'annullamento con lo 03 => ON
014400000907     C                   seton                                        03
014500000707     C   KL              seton                                        90
014600000907     C   KL              move      videpwCOM     videpw
014700091106     C   KL              move      vihubwCOM     vihubw
014800000707     C   KL              goto      escsub
014900000707      *
015000000907     c                   movel(P)  videpw        Kke1
015100091106     c                   movel(P)  vihubw        Kke2
015200000707     c     KTBE          CHAIN(N)  TNTBE000                           02
015300000707     C     *in02         ifeq      *on
015400000907     C                   seton                                        31
015500000907     C                   movel     msg(3)        V4CMSG
015600000907     C                   write     tn09WIN
015700000907     C                   goto      wind
015800000907      * emette l'errore
015900000707     C                   else
016000000707     C                   move      tbeuni        tbeuniWORK      256
016100000707     C                   endif
016200120511      *
016300120511      * Altrimenti li vuole duplicare x Linea di Partenza
016400120511     C                   elseIf    scelta ='T'
016500120511      *
016600120511      * vuole una linea da cui copiare
016700120511     c                   clear                   cpyLNP
016800120511     c     DECIDI_Linea  tag
016900120511     C                   EXFMT     tn09CPY
017000120514     C   KC              goto      DECIDI_Linea
017100120511      *
017200120511     c     *inKL         ifeq      *on
017300120511     c                   eval      scelta= *blank
017400120511     c                   goto      fai_UNAscelta
017500120511     c                   else
017600120511      **
017700120511      * se NON c'è la LINEA oppure se non si è confermato
017800120511     c                   if        cpyLNP= 0 or *INkf = *Off
017900120511     c                   goto      DECIDI_Linea
018000120511     c                   else
018100120511      **
018200120511      **  copia di massa tutti i records per la linea selezionata
018300120511      **
018400120511     c                   exsr      copia_LINEA
018500120511      **
018600120514      **  Ricordare di trasmettere le tabelle in filiale
018700120514     C                   EXFMT     tn09TRSM
018800120511     c                   seton                                        90
018900120511     c                   leaveSR
019000120511     c                   end
019100120511      *
019200120511     c                   end
019300120511      ***
019400120511     C                   endif
019500120511      *
019600000707     C                   endif
019700000619      *
019800000707     C     escsub        ENDSR
019900000620      *****************************************************************
020000000620      *   ROUTINE DI CARICAMENTO VIDEO DA FILE
020100000620      *****************************************************************
020200000620     C     RIED02        BEGSR
020300000620      *
020400000620     c                   clear                   TNTBE000
020500000907     c                   clear                   ddep
020600000621      *   Pulisco campi primo video e richiamo routine pulizia
020700000621      *   campi secodo video
020800000621     c                   exsr      pulizia
020900000622      *
021000000907     c                   movel(P)  V9cdep        Kke1
021100091106     c                   movel(P)  v9cHUB        Kke2
021200000620      *
021300000621      *   Accesso a tntbe01l per verificare l'esistenza del record
021400000620     c     KTBE          CHAIN     TNTBE000                           02
021500000620      *
021600000907      *   Per record trovato valorizzo la tabella dsDEP e quindi il video
021700000707     C     *in08         ifeq      *off
021800000907     c  N02              movel     TBEuni        Ddep
021900000907     c   02              clear                   ddep
022000000707     C                   else
022100000907     C                   movel     tbeuniWORK    ddep
022200000707     C                   endif
022300000621      *
022400000907     C                   movel     V9cdep        V9cdep2
022500091106     C                   movel     V9cHUB        V9cHUB2
022600120514      * se annullato
022700120514     c                   setoff                                       88
022800120514     c                   if        tbeATB = 'A'
022900120514     c                   setoN                                        88
023000120514     c                   end
023100120514      *
023200000620     C                   ENDSR
023300980506      *****************************************************************
023400000619      *   ROUTINE PER GESTIONE SECONDA VIDEATA
023500980506      *****************************************************************
023600000619     c     GESD02        BEGSR
023700980909      *
023800000619     c                   setoff                                       90
023900000623      *
024000000907     C                   movel     §depdes       v9cdes2
024100000907     C                   z-add     §depksc       v9cksc2
024200000907     C                   z-add     §deplnp       v9clnp2
024300000922     C                   z-add     §deptar       v9ctar2
024400051219     C                   movel     §depctm       v9ctm2
024500051219     C                   movel     §depwrk       v9edi2
024600000619      *
024700000619     c                   do        *hival
024800000619      *
024900000907     c                   exfmt     tn09d02
025000000620      *
025100000619     c                   eval      *in28 = *off
025200000619      * --------------
025300000619      * F3 = Fine
025400000619      *---------------
025500000619     c   kc              leave
025600000619      * --------------
025700000619      * F12 = Ritorno
025800000619      *---------------
025900000619     c     *inkl         ifeq      *on
026000000621     c                   seton                                        90
026100000621     c                   leave
026200000619     c                   endif
026300000619      *
026400000621      *    Richiamo routine di controllo secondo video
026500000619     c                   exsr      ctrd02
026600000619      *
026700000619     c   28              iter
026800000619      *
026900000621      *   Per F6 richiamo routine di aggiornamento o immissione
027000000907     c                   if        *inkf=*on  or *in16=*on
027100000619     c                   exsr      riecdd
027200000907     C                   eval      *in16=*off
027300000619     c                   leave
027400000619     c                   endif
027500000619      *
027600000619     c                   enddo
027700000619      *
027800000619     c                   endsr
027900000619      ********************************************************************
028000000619      *  ROUTINE DI CONTROLLO SECONDA VIDEATA
028100000619      ********************************************************************
028200000619     C     CTRD02        BEGSR
028300000619      *
028400000907     C                   clear                   V2CMSG
028500000907      *
028600000621     c                   setoff                                       505152
028700000907     c                   setoff                                       9028
028800000907      *
028900000621      *    Controllo se i campi sono valorizzati
029000000907     c     v9cdes2       ifeq      *blanks
029100000907     c                   seton                                        509028
029200000907     C                   movel     msg(1)        V2CMSG
029300000620     c                   endif
029400000620      *
029500000907     c     v9clnp2       ifeq      *zeros
029600000907     c                   seton                                        519028
029700000907     C                   movel     msg(1)        V2CMSG
029800000620     c                   endif
029900000620      *
030000000907     c     v9cksc2       ifeq      *zeros
030100000907     c                   seton                                        529028
030200000907     C                   movel     msg(1)        V2CMSG
030300000620     c                   endif
030400000619      *
030500980506     C                   ENDSR
030600980507      *****************************************************************
030700980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
030800980507      *****************************************************************
030900981117     C     RIECDD        BEGSR
031000980909      *
031100000621      *   Carico i campi del video già controllati nella tabella dscpt
031200000621      *
031300000907     c                   movel     V9cdes2       §depdes
031400000907     c                   movel     V9clnp2       §deplnp
031500000907     c                   movel     V9cksc2       §depksc
031600000922     c                   movel     V9ctar2       §deptar
031700051219     c                   movel     V9ctm2        §depctm
031800051219     c                   movel     V9edi2        §depwrk
031900000621      *
032000000907     c                   movel(p)  Ddep          TBEuni
032100981102      *
032200000619     c                   movel     'S'           TBEftt
032300000619     c                   movel     *blanks       TBEatb
032400980909      *
032500981102     c                   clear                   TBEftr
032600981102      *
032700000621      *   Se è un record già esistente aggiorno altrimenti scrivo su file
032800981102     c                   IF        *IN02 = *ON
032900000619     c                   z-add     0             TBEflt
033000991001     c                   clear                   TBEdtr
033100981102     c                   movel     Ksif          TBEsif
033200981102     c                   movel     Klin          TBElin
033300000914     c                   movel     'DP'          TBEapl
033400000914     c                   movel     Kcod          TBEcod
033500000907     C                   movel(p)  V9cdep        TBEke1
033600091106     C                   movel(p)  v9chub        TBEke2
033700120511     C                   WRITE     TNTBE000                             99
033800981102      *
033900981102     c                   ELSE
034000000914     C   16              eval      TBEATB='A'
034100000914     C                   UPDATE    TNTBE000
034200981102     c                   ENDIF
034300980507      *
034400000907      *  riabilita l'annullamento se passato dalla window di copia
034500000907     C                   setoff                                       03
034600000907      *
034700980507     C                   ENDSR
034800000621      *****************************************************************
034900000621      *   ROUTINE DI PULIZIA SECONDA VIDEATA
035000000621      *****************************************************************
035100000621     C     PULIZIA       BEGSR
035200000621      *
035300000907     C                   movel     *blanks       V9cdes2
035400000907     C                   z-add     *zeros        V9cksc2
035500000907     C                   z-add     *zeros        V9clnp2
035600000922     C                   z-add     *zeros        V9ctar2
035700051219     C                   move      *blank        V9ctm2
035800051219     C                   move      'S'           V9edi2
035900000621      *
036000000621     C                   ENDSR
036100000622      *****************************************************************
036200000622      *   ROUTINE DI GESTIONE SUBFILE
036300000622      *****************************************************************
036400000622     C     GESSFL        BEGSR
036500000623      *
036600000622     c                   clear                   nrr               4 0
036700000623     c                   eval      *in23='0'
036800000622      *
036900000622     c                   eval      *in29='1'
037000000907     c                   write     tn09ctl
037100000622     c                   eval      *in29='0'
037200000622      *
037300000622     c     ktbe1         chain     tntbe002                           25
037400000622      *
037500000622     c                   dow       not *in25
037600000622      *
037700000622     c     tbeatb        ifeq      *blanks
037800000907     c                   movel     tbeke1        v9depsf
037900091106     c                   movel     tbeke2        v9HUBsf
038000000918      *
038100000907     c                   movel(p)  TBEuni        Ddep
038200000622      *
038300000907     c                   movel     §depdes       v9dessf
038400000907     c                   movel     §deplnp       v9lnpsf
038500000907     c                   movel     §depksc       v9kscsf
038600000922     c                   movel     §deptar       v9tarsf
038700051219     c                   movel     §depctm       v9ctmsf
038800051219     c                   movel     §depwrk       v9vabsf
038900000907      *
039000000622     c                   add       1             nrr
039100000907     c                   write     tn09sfl
039200000622      *
039300000622     c                   endif
039400000622      *
039500000622     c     ktbe1         reade     tntbe002                               25
039600000622      *
039700000622     c                   enddo
039800000622      *
039900000622     c                   exsr      gessf2
040000000622      *
040100000622     c                   endsr
040200000622      *****************************************************************
040300000622      *   ROUTINE gestione secondo video da subfile
040400000622      *****************************************************************
040500000622     C     GESSF2        BEGSR
040600000622      *
040700000622     c                   if        nrr>0
040800000622     c                   eval      nrr=1
040900000623     c                   eval      *in23='1'
041000000622     c                   endif
041100000622      *
041200000907     c                   write     tn09z03
041300000907     c                   exfmt     tn09ctl
041400000622      *
041500000907     c   KC
041600000907     cor KL              goto      esci
041700000622      *
041800000907     c   23              readc     tn09sfl                                04
041900000622      *
042000000623     c   23              dow       *in04=*off
042100000622      *
042200000907      * se ha scelto il record lo deve riportare sulla 2°videata
042300000907     c     v9opzsf       ifne      *blanks
042400000907     c                   movel     v9depsf       v9cdep
042500091106     c                   movel     v9hubsf       v9chub
042600000623     c                   exsr      ried02
042700000623     c                   eval      *in04=*on
042800000907     c                   move      *blanks       v9opzsf
042900000622     c                   else
043000000907      *      rilegge se non ha scelto nulla
043100000907     c                   readc     tn09sfl                                04
043200000622     c                   endif
043300000622      *
043400000622     c                   enddo
043500000622      *
043600000707     c     esci          ENDSR
043700980506      *****************************************************************
043800980506      *   ROUTINE INIZIALE
043900980506      *****************************************************************
044000980506     C     *INZSR        BEGSR
044100980506      *
044200980506     C     *ENTRY        PLIST
044300980506     C                   PARM                    KPJBA
044400981102      *
044500981102     C                   Z-ADD     1             CODUT
044600981102     C                   CALL      'X§PARUT'
044700981102     C                   PARM                    UT§DSE0F
044800981102     C                   MOVEL     RAGUT         RSUT
044900981102      *
045000981102      * Controllo Testata Tabella
045100000725     C     KTBETT        CHAIN(N)  TNTBE000                           28
045200981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
045300981117     C                   eval      *inlr = *on
045400981102     c                   ELSE
045500981102     c                   movel     TBEsif        Ksif
045600981102     c                   movel     TBElin        Klin
045700981102     c                   ENDIF
045800981102      *
045900981102     C     KTBE          klist
046000981102     C                   KFLD                    Kcod
046100981102     C                   KFLD                    Kke1
046200981102     C                   KFLD                    Kke2
046300981102     C                   KFLD                    Klin
046400981102     C                   KFLD                    Ksif
046500000619     c*                  clear                   Kke2
046600981102      *
046700981102     C     KTBETT        klist
046800981102     C                   KFLD                    KTTcod
046900981102     C                   KFLD                    KTTke1
047000981102      *
047100000619     C     KTTABL        klist
047200000619     C                   KFLD                    KTTkut
047300000620     C                   KFLD                    KTlcod
047400000619     C                   KFLD                    KTTkey
047500000622      *
047600000622     C     KTBE1         klist
047700000622     C                   KFLD                    Kcod
047800000622     C                   KFLD                    Klin
047900000622     C                   KFLD                    Ksif
048000120511      *
048100120511     C     KCOPIA        klist
048200120511     C                   KFLD                    Kcod
048300120511     C                   KFLD                    KEY_depot
048400000619      *
048500000907     C                   eval      videpw = *blank
048600000707      *
048700980506     C                   ENDSR
048800980909      *****************************************************************
048900120511      *     copia tutti i records necessari x attivare un nuovo DEPOT
049000120511      *****************************************************************
049100120511     C     Copia_LINEA   begSR
049200120511      *
049300120511     c     *LIKE         DEFINE    TBEKE1        KEY_DEPOT
049400120511     C                   CLEAR                   KEY_depot
049500120511      *
049600120511      *  aggancia la linea x reperire un DEPOT da cui copiare\
049700120511     c     Kcod          setll     tntbe01l
049800120511     c     Kcod          reade     tntbe01l
049900120511      *
050000120511     C                   Dow       not %Eof(Tntbe01l)
050100120511     c                   eval      ddep = tbeUNI
050200120511      *   una volta trovata la LINEA salva la chiave per copiare
050300120511     c                   if        §DEPLNP = cpyLNP
050400120511     c                   eval      KEY_DEPOT = TBEKE1
050500120511     c                   leave
050600120511     c                   end
050700120511     c     Kcod          reade     tntbe01l
050800120511     C                   ENDdo
050900120511      *
051000120511      *  Adesso esegue la copia esatta dei records cambiando la chiave
051100120511     C     KCOPIA        setll     tntbe01l
051200120511     c     KCOPIA        reade     tntbe01l
051300120511      *
051400120511     C                   Dow       not %Eof(Tntbe01l)
051500120511      *
051600120511     C                   movel(p)  V9cdep        TBEke1
051700120511     C                   WRITE     TNTBE000                             99
051800120511      *
051900120511     c     KCOPIA        reade     tntbe01l
052000120511     C                   ENDdo
052100120511      *
052200120511     C                   ENDSR
052300120511      *****************************************************************
052400980909** MSG  (Lungh. 78)                                                          *
052500000907Il dato è obbligatorio .                                                       1
052600000907Il codice esiste già e non può essere creato duplicandolo!                     2
052700000907Il codice da duplicare inesistente !                                           3
