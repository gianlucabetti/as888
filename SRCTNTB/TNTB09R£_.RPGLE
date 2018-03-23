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
018600120511     c                   seton                                        90
018700120511     c                   leaveSR
018800120511     c                   end
018900120511      *
019000120511     c                   end
019100120511      ***
019200120511     C                   endif
019300120511      *
019400000707     C                   endif
019500000619      *
019600000707     C     escsub        ENDSR
019700000620      *****************************************************************
019800000620      *   ROUTINE DI CARICAMENTO VIDEO DA FILE
019900000620      *****************************************************************
020000000620     C     RIED02        BEGSR
020100000620      *
020200000620     c                   clear                   TNTBE000
020300000907     c                   clear                   ddep
020400000621      *   Pulisco campi primo video e richiamo routine pulizia
020500000621      *   campi secodo video
020600000621     c                   exsr      pulizia
020700000622      *
020800000907     c                   movel(P)  V9cdep        Kke1
020900091106     c                   movel(P)  v9cHUB        Kke2
021000000620      *
021100000621      *   Accesso a tntbe01l per verificare l'esistenza del record
021200000620     c     KTBE          CHAIN     TNTBE000                           02
021300000620      *
021400000907      *   Per record trovato valorizzo la tabella dsDEP e quindi il video
021500000707     C     *in08         ifeq      *off
021600000907     c  N02              movel     TBEuni        Ddep
021700000907     c   02              clear                   ddep
021800000707     C                   else
021900000907     C                   movel     tbeuniWORK    ddep
022000000707     C                   endif
022100000621      *
022200000907     C                   movel     V9cdep        V9cdep2
022300091106     C                   movel     V9cHUB        V9cHUB2
022400120514      * se annullato
022500120514     c                   setoff                                       88
022600120514     c                   if        tbeATB = 'A'
022700120514     c                   setoN                                        88
022800120514     c                   end
022900120514      *
023000000620     C                   ENDSR
023100980506      *****************************************************************
023200000619      *   ROUTINE PER GESTIONE SECONDA VIDEATA
023300980506      *****************************************************************
023400000619     c     GESD02        BEGSR
023500980909      *
023600000619     c                   setoff                                       90
023700000623      *
023800000907     C                   movel     §depdes       v9cdes2
023900000907     C                   z-add     §depksc       v9cksc2
024000000907     C                   z-add     §deplnp       v9clnp2
024100000922     C                   z-add     §deptar       v9ctar2
024200051219     C                   movel     §depctm       v9ctm2
024300051219     C                   movel     §depwrk       v9edi2
024400000619      *
024500000619     c                   do        *hival
024600000619      *
024700000907     c                   exfmt     tn09d02
024800000620      *
024900000619     c                   eval      *in28 = *off
025000000619      * --------------
025100000619      * F3 = Fine
025200000619      *---------------
025300000619     c   kc              leave
025400000619      * --------------
025500000619      * F12 = Ritorno
025600000619      *---------------
025700000619     c     *inkl         ifeq      *on
025800000621     c                   seton                                        90
025900000621     c                   leave
026000000619     c                   endif
026100000619      *
026200000621      *    Richiamo routine di controllo secondo video
026300000619     c                   exsr      ctrd02
026400000619      *
026500000619     c   28              iter
026600000619      *
026700000621      *   Per F6 richiamo routine di aggiornamento o immissione
026800000907     c                   if        *inkf=*on  or *in16=*on
026900000619     c                   exsr      riecdd
027000000907     C                   eval      *in16=*off
027100000619     c                   leave
027200000619     c                   endif
027300000619      *
027400000619     c                   enddo
027500000619      *
027600000619     c                   endsr
027700000619      ********************************************************************
027800000619      *  ROUTINE DI CONTROLLO SECONDA VIDEATA
027900000619      ********************************************************************
028000000619     C     CTRD02        BEGSR
028100000619      *
028200000907     C                   clear                   V2CMSG
028300000907      *
028400000621     c                   setoff                                       505152
028500000907     c                   setoff                                       9028
028600000907      *
028700000621      *    Controllo se i campi sono valorizzati
028800000907     c     v9cdes2       ifeq      *blanks
028900000907     c                   seton                                        509028
029000000907     C                   movel     msg(1)        V2CMSG
029100000620     c                   endif
029200000620      *
029300000907     c     v9clnp2       ifeq      *zeros
029400000907     c                   seton                                        519028
029500000907     C                   movel     msg(1)        V2CMSG
029600000620     c                   endif
029700000620      *
029800000907     c     v9cksc2       ifeq      *zeros
029900000907     c                   seton                                        529028
030000000907     C                   movel     msg(1)        V2CMSG
030100000620     c                   endif
030200000619      *
030300980506     C                   ENDSR
030400980507      *****************************************************************
030500980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
030600980507      *****************************************************************
030700981117     C     RIECDD        BEGSR
030800980909      *
030900000621      *   Carico i campi del video già controllati nella tabella dscpt
031000000621      *
031100000907     c                   movel     V9cdes2       §depdes
031200000907     c                   movel     V9clnp2       §deplnp
031300000907     c                   movel     V9cksc2       §depksc
031400000922     c                   movel     V9ctar2       §deptar
031500051219     c                   movel     V9ctm2        §depctm
031600051219     c                   movel     V9edi2        §depwrk
031700000621      *
031800000907     c                   movel(p)  Ddep          TBEuni
031900981102      *
032000000619     c                   movel     'S'           TBEftt
032100000619     c                   movel     *blanks       TBEatb
032200980909      *
032300981102     c                   clear                   TBEftr
032400981102      *
032500000621      *   Se è un record già esistente aggiorno altrimenti scrivo su file
032600981102     c                   IF        *IN02 = *ON
032700000619     c                   z-add     0             TBEflt
032800991001     c                   clear                   TBEdtr
032900981102     c                   movel     Ksif          TBEsif
033000981102     c                   movel     Klin          TBElin
033100000914     c                   movel     'DP'          TBEapl
033200000914     c                   movel     Kcod          TBEcod
033300000907     C                   movel(p)  V9cdep        TBEke1
033400091106     C                   movel(p)  v9chub        TBEke2
033500120511     C                   WRITE     TNTBE000                             99
033600981102      *
033700981102     c                   ELSE
033800000914     C   16              eval      TBEATB='A'
033900000914     C                   UPDATE    TNTBE000
034000981102     c                   ENDIF
034100980507      *
034200000907      *  riabilita l'annullamento se passato dalla window di copia
034300000907     C                   setoff                                       03
034400000907      *
034500980507     C                   ENDSR
034600000621      *****************************************************************
034700000621      *   ROUTINE DI PULIZIA SECONDA VIDEATA
034800000621      *****************************************************************
034900000621     C     PULIZIA       BEGSR
035000000621      *
035100000907     C                   movel     *blanks       V9cdes2
035200000907     C                   z-add     *zeros        V9cksc2
035300000907     C                   z-add     *zeros        V9clnp2
035400000922     C                   z-add     *zeros        V9ctar2
035500051219     C                   move      *blank        V9ctm2
035600051219     C                   move      'S'           V9edi2
035700000621      *
035800000621     C                   ENDSR
035900000622      *****************************************************************
036000000622      *   ROUTINE DI GESTIONE SUBFILE
036100000622      *****************************************************************
036200000622     C     GESSFL        BEGSR
036300000623      *
036400000622     c                   clear                   nrr               4 0
036500000623     c                   eval      *in23='0'
036600000622      *
036700000622     c                   eval      *in29='1'
036800000907     c                   write     tn09ctl
036900000622     c                   eval      *in29='0'
037000000622      *
037100000622     c     ktbe1         chain     tntbe002                           25
037200000622      *
037300000622     c                   dow       not *in25
037400000622      *
037500000622     c     tbeatb        ifeq      *blanks
037600000907     c                   movel     tbeke1        v9depsf
037700091106     c                   movel     tbeke2        v9HUBsf
037800000918      *
037900000907     c                   movel(p)  TBEuni        Ddep
038000000622      *
038100000907     c                   movel     §depdes       v9dessf
038200000907     c                   movel     §deplnp       v9lnpsf
038300000907     c                   movel     §depksc       v9kscsf
038400000922     c                   movel     §deptar       v9tarsf
038500051219     c                   movel     §depctm       v9ctmsf
038600051219     c                   movel     §depwrk       v9vabsf
038700000907      *
038800000622     c                   add       1             nrr
038900000907     c                   write     tn09sfl
039000000622      *
039100000622     c                   endif
039200000622      *
039300000622     c     ktbe1         reade     tntbe002                               25
039400000622      *
039500000622     c                   enddo
039600000622      *
039700000622     c                   exsr      gessf2
039800000622      *
039900000622     c                   endsr
040000000622      *****************************************************************
040100000622      *   ROUTINE gestione secondo video da subfile
040200000622      *****************************************************************
040300000622     C     GESSF2        BEGSR
040400000622      *
040500000622     c                   if        nrr>0
040600000622     c                   eval      nrr=1
040700000623     c                   eval      *in23='1'
040800000622     c                   endif
040900000622      *
041000000907     c                   write     tn09z03
041100000907     c                   exfmt     tn09ctl
041200000622      *
041300000907     c   KC
041400000907     cor KL              goto      esci
041500000622      *
041600000907     c   23              readc     tn09sfl                                04
041700000622      *
041800000623     c   23              dow       *in04=*off
041900000622      *
042000000907      * se ha scelto il record lo deve riportare sulla 2°videata
042100000907     c     v9opzsf       ifne      *blanks
042200000907     c                   movel     v9depsf       v9cdep
042300091106     c                   movel     v9hubsf       v9chub
042400000623     c                   exsr      ried02
042500000623     c                   eval      *in04=*on
042600000907     c                   move      *blanks       v9opzsf
042700000622     c                   else
042800000907      *      rilegge se non ha scelto nulla
042900000907     c                   readc     tn09sfl                                04
043000000622     c                   endif
043100000622      *
043200000622     c                   enddo
043300000622      *
043400000707     c     esci          ENDSR
043500980506      *****************************************************************
043600980506      *   ROUTINE INIZIALE
043700980506      *****************************************************************
043800980506     C     *INZSR        BEGSR
043900980506      *
044000980506     C     *ENTRY        PLIST
044100980506     C                   PARM                    KPJBA
044200981102      *
044300981102     C                   Z-ADD     1             CODUT
044400981102     C                   CALL      'X§PARUT'
044500981102     C                   PARM                    UT§DSE0F
044600981102     C                   MOVEL     RAGUT         RSUT
044700981102      *
044800981102      * Controllo Testata Tabella
044900000725     C     KTBETT        CHAIN(N)  TNTBE000                           28
045000981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
045100981117     C                   eval      *inlr = *on
045200981102     c                   ELSE
045300981102     c                   movel     TBEsif        Ksif
045400981102     c                   movel     TBElin        Klin
045500981102     c                   ENDIF
045600981102      *
045700981102     C     KTBE          klist
045800981102     C                   KFLD                    Kcod
045900981102     C                   KFLD                    Kke1
046000981102     C                   KFLD                    Kke2
046100981102     C                   KFLD                    Klin
046200981102     C                   KFLD                    Ksif
046300000619     c*                  clear                   Kke2
046400981102      *
046500981102     C     KTBETT        klist
046600981102     C                   KFLD                    KTTcod
046700981102     C                   KFLD                    KTTke1
046800981102      *
046900000619     C     KTTABL        klist
047000000619     C                   KFLD                    KTTkut
047100000620     C                   KFLD                    KTlcod
047200000619     C                   KFLD                    KTTkey
047300000622      *
047400000622     C     KTBE1         klist
047500000622     C                   KFLD                    Kcod
047600000622     C                   KFLD                    Klin
047700000622     C                   KFLD                    Ksif
047800120511      *
047900120511     C     KCOPIA        klist
048000120511     C                   KFLD                    Kcod
048100120511     C                   KFLD                    KEY_depot
048200000619      *
048300000907     C                   eval      videpw = *blank
048400000707      *
048500980506     C                   ENDSR
048600980909      *****************************************************************
048700120511      *     copia tutti i records necessari x attivare un nuovo DEPOT
048800120511      *****************************************************************
048900120511     C     Copia_LINEA   begSR
049000120511      *
049100120511     c     *LIKE         DEFINE    TBEKE1        KEY_DEPOT
049200120511     C                   CLEAR                   KEY_depot
049300120511      *
049400120511      *  aggancia la linea x reperire un DEPOT da cui copiare\
049500120511     c     Kcod          setll     tntbe01l
049600120511     c     Kcod          reade     tntbe01l
049700120511      *
049800120511     C                   Dow       not %Eof(Tntbe01l)
049900120511     c                   eval      ddep = tbeUNI
050000120511      *   una volta trovata la LINEA salva la chiave per copiare
050100120511     c                   if        §DEPLNP = cpyLNP
050200120511     c                   eval      KEY_DEPOT = TBEKE1
050300120511     c                   leave
050400120511     c                   end
050500120511     c     Kcod          reade     tntbe01l
050600120511     C                   ENDdo
050700120511      *
050800120511      *  Adesso esegue la copia esatta dei records cambiando la chiave
050900120511     C     KCOPIA        setll     tntbe01l
051000120511     c     KCOPIA        reade     tntbe01l
051100120511      *
051200120511     C                   Dow       not %Eof(Tntbe01l)
051300120511      *
051400120511     C                   movel(p)  V9cdep        TBEke1
051500120511     C                   WRITE     TNTBE000                             99
051600120511      *
051700120511     c     KCOPIA        reade     tntbe01l
051800120511     C                   ENDdo
051900120511      *
052000120511     C                   ENDSR
052100120511      *****************************************************************
052200980909** MSG  (Lungh. 78)                                                          *
052300000907Il dato è obbligatorio .                                                       1
052400000907Il codice esiste già e non può essere creato duplicandolo!                     2
052500000907Il codice da duplicare inesistente !                                           3
