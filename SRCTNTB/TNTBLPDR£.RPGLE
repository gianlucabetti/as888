000100980909
000200980909      *------------------------------------------------------------------------*
000300150923      * GESTIONE TABELLA LPD : Dati LINEA di PARTENZA di DPD conferma GEODATA  *
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
001500150923     FTNTBLPDD  CF   E             WORKSTN
001600000907     F                                     sfile(tn09sfl:nrr)
001700981102     FTNTBE01L  UF A E           K DISK
001800000622     FTNTBE02L  IF   E           K DISK    RENAME(TNTBE000:TNTBE002)
001900000620     FTABEL00F  IF   E           K DISK
002000151012     FAZORG01L  IF   E           K DISK
002100981102      *--------------------------------------------
002200150923     d DLPD          E DS
002300000907     D MSG             S             78    DIM(6) CTDATA PERRCD(1)              MSG VIDEO
002400981102      *
002500981102     D Kttcod          S                   LIKE(TBEcod)
002600150923     D Kttke1          S                   LIKE(TBEke1)  INZ('000000000000LPD')
002700150923     D Kcod            S                   LIKE(TBEcod)  INZ('LPD')
002800981102     D Kke1            S                   LIKE(TBEke1)
002900981102     D Kke2            S                   LIKE(TBEke2)
003000981102     D Klin            S                   LIKE(TBElin)
003100981102     D Ksif            S                   LIKE(TBEsif)
003200000619      *
003300000620     D Kttkut          S                   LIKE(TBLkut)
003400000620     D Ktlcod          S                   LIKE(TBLcod)
003500000619     D Kttkey          S                   LIKE(TBLkey)
003600981102      *
003700980507     D KPJBA         E DS
003800151012     D OG143         E DS
003900981102     D UT§DSE0F      E DS
004000151012     d TIBS69DS      E DS                  INZ
004100151012     d DS_cnaco      E DS                  extname(CNACO00F)
004200151012     d DS_cnind      E DS                  extname(CNIND00F)
004300151012     d DS_cnclp      E DS                  extname(CNCLP00F)
004400151012     d DS_fncls      E DS                  extname(FNCLS00F)
004500151012     d digits          c                   '0123456789'
004600000619      *********************************************
004700980506      * EMISSIONE PRIMA VIDEATA
004800000619      *********************************************
004900981117     C                   DO        *hival
005000000621      *
005100000707     C     emetti        tag
005200000907     C                   EXFMT     tn09D01
005300980930      *
005400980909     C                   clear                   V1CMSG
005500000619      *
005600000619     C                   eval      *in28 = *off
005700980506      * F3=FINE
005800000619     C   KC              leave
005900000622      *
006000150923     C     '?'           scan      v9clnp                                 21
006100000907     c     *in21         ifeq      *on
006200000622     c                   exsr      gessfl
006300000907      * F3=FINE
006400000907     C   KC              leave
006500150923     C   KL              clear                   v9clnp
006600000707     C   KL              goto      emetti
006700000622     c                   endif
006800000622      *
006900000619      * CONTROLLI PRIMA VIDEATA
007000981102     C                   EXSR      CTRD01
007100981102      *
007200991001     C   90              iter
007300000621      *
007400000907      * GESTIONE SECONDA VIDEATA
007500000620     C                   EXSR      RIED02
007600000621      *
007700000907      * CONTROLLI SECONDA VIDEATA
007800000620     C                   EXSR      GESD02
007900000621      *
008000150923     c                   move      *blanks       v9clnp
008100000620      *
008200000621     C   90              iter
008300000620      *
008400000620     C                   enddo
008500981102      *
008600980506     C                   EVAL      *INLR = *ON
008700981102      *****************************************************************
008800981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
008900000621      *   controllo che i campi sono valorizzati, che il codice cliente
009000000621      *   sia esistente e che il punto operativo sia delle poste
009100981102      *****************************************************************
009200981102     C     CTRD01        BEGSR
009300981102      *
009400120514     c                   setoff                                       88
009500151012     c                   setoff                                       5090
009600000619      *
009700150923     c     v9clnp        ifeq      *blank
009800150923     c     v9clnp        oreq      *zeros
009900151012     c                   seton                                        509028
010000000907     C                   movel     msg(1)        V1CMSG
010100151012     c                   else
010200151012     c* deve essere numerico
010300151012     c     digits        check     v9clnp
010400151012     c                   if        %Found
010500151012     c                   seton                                        509028
010600151012     C                   movel     msg(5)        V1CMSG
010700151012     c                   else
010800151012     c                   move      v9clnp        KORGfil           3 0
010900151012     c     KORGfil       chain     azorg01l
011000151012     c                   if        not %Found(azorg01l)
011100151012     c                   seton                                        509028
011200151012     C                   movel     msg(4)        V1CMSG
011300151012     c                   else
011400151012     c                   movel     orgde3        OG143
011500151012     c                   if        §ogNTW <> 'DPD'
011600151012     c                   seton                                        509028
011700151012     C                   movel     msg(4)        V1CMSG
011800151012     c                   endif
011900151012     c                   endif
012000151012     c                   endif
012100151012     c                   endif
012200000707      *
012300151012      *  Se richiesta DUPLICA (08=ON)
012400151016     c                   if        *in08 and %subst(knmus:1:3)='EDP'
012500151012     c                   exsr      DUPLICA
012600151012     c   KL              goto      escsub
012700151012     C                   endif
012800000619      *
012900151012     C     escsub        ENDSR
013000000620      *****************************************************************
013100151012      *   duplicare un record
013200000620      *****************************************************************
013300151012     C     DUPLICA       BEGSR
013400000620      *
013500151012     C     *in90         ifeq      *off
013600151012     c                   movel(P)  V9clnp        Kke1
013700151012     c                   movel     *blanks       Kke2
013800151012     c     KTBE          CHAIN(N)  TNTBE000                           02
013900151012     C     *in02         ifeq      *off
014000151012     C                   seton                                        509028
014100151012     C                   movel     msg(2)        V1CMSG
014200151012     C                   endif
014300151012     C                   endif
014400151012      *   Accesso a tntbe01l per verificare l'esistenza del record
014500151012      *   .. controllo per duplicazione (non puo' esistere già)
014600151012      *
014700151012     C     *in90         ifeq      *off
014800151012      *
014900151012      *  quale tipo di duplica si vuole fare ?
015000151012      **  singolo record oppure tutti
015100151012      *
015200151012     c                   movel(P)  V9clnp        NEWCOD
015300151012     c                   move      'T'           scelta
015400151012     c     fai_UNAscelta tag
015500151012     c                   exfmt     TN09QUALE
015600151012      *
015700151012     c     *inKL         ifeq      *on
015800151012     c                   seton                                        90
015900151012     c                   leavesr
016000151012     c                   else
016100151012     c                   if        scelta= *blank or *inKC
016200151012     c                   goto      fai_UNAscelta
016300151012     c                   end
016400151012     c                   end
016500151012      ***
016600151012      *  x singolo codice di duplicazione
016700151012     c                   if        scelta='S'
016800151012      *
016900151012      *  emissione window per richiesta da chi duplicare
017000151012     C                   move      vilnpw        vilnpwCOM         4
017100151012     C     wind          TAG
017200151012      **
017300151012     C                   EXFMT     tn09WIN
017400151012     C   KC              goto      wind
017500151012      **
017600151012     C                   clear                   V4CMSG           40
017700151012     C                   setoff                                       31
017800151012      * non deve permettere l'annullamento con lo 03 => ON
017900151012     C                   seton                                        03
018000151012     C   KL              seton                                        90
018100151012     C   KL              move      vilnpwCOM     vilnpw
018200151012     c   KL              leaveSR
018300151012      *
018400151012     c                   movel(P)  vilnpw        Kke1
018500151012     c     KTBE          CHAIN(N)  TNTBE000                           02
018600151012     C     *in02         ifeq      *on
018700151012     C                   seton                                        31
018800151012     C                   movel     msg(3)        V4CMSG
018900151012     C                   write     tn09WIN
019000151012     C                   goto      wind
019100151012      * emette l'errore
019200151012     C                   else
019300151012     C                   move      tbeuni        tbeuniWORK      256
019400151012     C                   endif
019500151012      *
019600151012      * Altrimenti li vuole duplicare x Linea di Partenza
019700151012     C                   elseIf    scelta ='T'
019800151012      *
019900151012      * vuole una linea da cui copiare
020000151012     c                   clear                   cpyLNP
020100151012     c     DECIDI_Linea  tag
020200151012     C                   EXFMT     tn09CPY
020300151012     C   KC              goto      DECIDI_Linea
020400151012      *
020500151012     c     *inKL         ifeq      *on
020600151012     c                   eval      scelta= *blank
020700151012     c                   goto      fai_UNAscelta
020800151012     c                   else
020900151012      **
021000151012      * se NON c'è la LINEA oppure se non si è confermato
021100151012     c                   if        cpyLNP= '000' or *INkf = *Off or
021200151012     c                             cpyLNP= '   '
021300151012     c                   goto      DECIDI_Linea
021400151012     c                   else
021500151012      **
021600151012      **  copia di massa tutti i records per la linea selezionata
021700151012      **
021800151012     c                   exsr      copia_LINEA
021900151012      **
022000151012      **  Ricordare di trasmettere le tabelle in filiale
022100151012     C                   EXFMT     tn09TRSM
022200151012     c                   seton                                        90
022300151012     c                   leaveSR
022400151012     c                   end
022500151012      *
022600151012     c                   end
022700151012      ***
022800151012     C                   endif
022900151012     C                   endif
023000151012      *
023100151012     C                   ENDSR
023200151012      *****************************************************************
023300151012      *   ROUTINE DI CARICAMENTO VIDEO DA FILE
023400151012      *****************************************************************
023500151012     C     RIED02        BEGSR
023600151012      *
023700000620     c                   clear                   TNTBE000
023800150923     c                   clear                   dlpD
023900000621      *   Pulisco campi primo video e richiamo routine pulizia
024000000621      *   campi secodo video
024100000621     c                   exsr      pulizia
024200000622      *
024300150923     c                   movel(P)  V9clnp        Kke1
024400000620      *
024500000621      *   Accesso a tntbe01l per verificare l'esistenza del record
024600000620     c     KTBE          CHAIN     TNTBE000                           02
024700000620      *
024800150923      *   Per record trovato valorizzo la tabella dslnp e quindi il video
024900000707     C     *in08         ifeq      *off
025000150923     c  N02              movel     TBEuni        Dlpd
025100150923     c   02              clear                   dLPD
025200000707     C                   else
025300150923     C                   movel     tbeuniWORK    dLPD
025400000707     C                   endif
025500000621      *
025600150923     C                   movel     V9clnp        V9clnp2
025700120514      * se annullato
025800120514     c                   setoff                                       88
025900120514     c                   if        tbeATB = 'A'
026000120514     c                   setoN                                        88
026100120514     c                   end
026200120514      *
026300000620     C                   ENDSR
026400980506      *****************************************************************
026500000619      *   ROUTINE PER GESTIONE SECONDA VIDEATA
026600980506      *****************************************************************
026700000619     c     GESD02        BEGSR
026800980909      *
026900151012     c                   setoff                                       515253
027000000619     c                   setoff                                       90
027100000623      *
027200150923     C                   movel     §lpddes       v9cdes2
027300150925     C                   movel     §lpdctm       v9ctm2
027400150925     C                   movel     §lpdwrk       v9edi2
027500150925      *
027600150923     C                   z-add     §lpdksc       v9cksc2
027700151012     c                   clear                   v9d1
027800151012     c                   z-add     §lpdkscre     V9kscre
027900151012     c                   clear                   v9d2
028000150923     C                   z-add     §lpdtar       v9ctar2
028100150923     C                   z-add     §lpdtarvtg    v9ctarv
028200151005     C                   z-add     §lpdtarlhs    v9ctarl
028300151015     C                   z-add     §lpdtarlhv    v9ctarlv
028400150925     C                   movel     §lpdggi       v9cggi
028500151006     C                   z-add     §lpdtre       v9ctre2
028600151006     C                   z-add     §lpdtrevtg    v9ctrev
028700151006     C                   z-add     §lpdtrelhs    v9ctrel
028800151015     C                   z-add     §lpdtrelhv    v9ctrelv
028900151012      *  solo per emettere e decodificare
029000151012     c                   exsr      CTRD02
029100000619      *
029200000619     c                   do        *hival
029300000619      *
029400000907     c                   exfmt     tn09d02
029500000620      *
029600000619     c                   eval      *in28 = *off
029700000619      * --------------
029800000619      * F3 = Fine
029900000619      *---------------
030000000619     c   kc              leave
030100000619      * --------------
030200000619      * F12 = Ritorno
030300000619      *---------------
030400000619     c     *inkl         ifeq      *on
030500000621     c                   seton                                        90
030600000621     c                   leave
030700000619     c                   endif
030800000619      *
030900000621      *    Richiamo routine di controllo secondo video
031000000619     c                   exsr      ctrd02
031100000619      *
031200000619     c   28              iter
031300000619      *
031400000621      *   Per F6 richiamo routine di aggiornamento o immissione
031500000907     c                   if        *inkf=*on  or *in16=*on
031600000619     c                   exsr      riecdd
031700000907     C                   eval      *in16=*off
031800000619     c                   leave
031900000619     c                   endif
032000000619      *
032100000619     c                   enddo
032200000619      *
032300000619     c                   endsr
032400000619      ********************************************************************
032500000619      *  ROUTINE DI CONTROLLO SECONDA VIDEATA
032600000619      ********************************************************************
032700151012     C     CTRD02        BEGSR
032800000619      *
032900000907     C                   clear                   V2CMSG
033000000907      *
033100151012     c                   setoff                                       515253
033200150925     c                   setoff                                       9028
033300000907      *
033400000621      *    Controllo se i campi sono valorizzati
033500000907     c     v9cdes2       ifeq      *blanks
033600151012     c                   seton                                        519028
033700000907     C                   movel     msg(1)        V2CMSG
033800000620     c                   endif
033900151012      * Cliente
034000000907     c     v9cksc2       ifeq      *zeros
034100151012     c                   seton                                        529028
034200000907     C                   movel     msg(1)        V2CMSG
034300151012     c                   else
034400151012     C                   clear                   DS_cnaco
034500151012     C                   clear                   DS_cnind
034600151012     C                   clear                   DS_cnclp
034700151012     C                   clear                   DS_fncls
034800151012     c                   move      v9cksc2       i69kac
034900151012     C                   CALL      'TIBS69R'
035000151012     C                   PARM                    tibs69DS
035100151012     C                   PARM                    DS_cnaco
035200151012     C                   PARM                    DS_cnind
035300151012     C                   PARM                    DS_cnclp
035400151012     C                   PARM                    DS_fncls
035500151012      *
035600151012     C                   if        O69ERR = *blank
035700151012     c                   clear                   v9d1
035800151012     c                   movel     acorag        v9d1
035900151012     C                   else
036000151012     c                   seton                                        529028
036100151012     C                   movel     msg(3)        V2CMSG
036200151012     c                   end
036300151012     C*
036400151012     c                   endif
036500151012      *
036600151012      * Cliente
036700151012     c     v9kscre       ifeq      *zeros
036800151012     c                   seton                                        539028
036900151012     C                   movel     msg(1)        V2CMSG
037000151012     c                   else
037100151012     C                   clear                   DS_cnaco
037200151012     C                   clear                   DS_cnind
037300151012     C                   clear                   DS_cnclp
037400151012     C                   clear                   DS_fncls
037500151012     c                   move      v9kscre       i69kac
037600151012     C                   CALL      'TIBS69R'
037700151012     C                   PARM                    tibs69DS
037800151012     C                   PARM                    DS_cnaco
037900151012     C                   PARM                    DS_cnind
038000151012     C                   PARM                    DS_cnclp
038100151012     C                   PARM                    DS_fncls
038200151012      *
038300151012     C                   if        O69ERR = *blank
038400151012     c                   clear                   v9d2
038500151012     c                   movel     acorag        v9d2
038600151012     C                   else
038700151012     c                   seton                                        539028
038800151012     C                   movel     msg(3)        V2CMSG
038900151012     c                   end
039000151012     C*
039100151012     c                   endif
039200151012      *
039300000619      *
039400980506     C                   ENDSR
039500980507      *****************************************************************
039600980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
039700980507      *****************************************************************
039800981117     C     RIECDD        BEGSR
039900980909      *
040000000621      *   Carico i campi del video già controllati nella tabella dscpt
040100000621      *
040200150923     c                   movel     V9cdes2       §lpddes
040300150923     c                   movel     V9cksc2       §lpdksc
040400150923     c                   movel     V9ctar2       §lpdtar
040500150923     c                   movel     V9ctarv       §lpdtarvtg
040600151005     c                   movel     V9ctarl       §lpdtarlhs
040700151015     c                   movel     V9ctarlv      §lpdtarlhv
040800150923     c                   movel     V9ctm2        §lpdctm
040900150923     c                   movel     V9edi2        §lpdwrk
041000150925     c                   movel     V9cggi        §lpdggi
041100151006     c                   movel     V9ctre2       §lpdtre
041200151006     c                   movel     V9ctrev       §lpdtrevtg
041300151006     c                   movel     V9ctrel       §lpdtrelhs
041400151012     c                   movel     V9kscre       §lpdkscre
041500151015     c                   movel     V9ctrelv      §lpdtrelhv
041600000621      *
041700150923     c                   movel(p)  Dlpd          TBEuni
041800981102      *
041900000619     c                   movel     'S'           TBEftt
042000000619     c                   movel     *blanks       TBEatb
042100980909      *
042200981102     c                   clear                   TBEftr
042300981102      *
042400000621      *   Se è un record già esistente aggiorno altrimenti scrivo su file
042500981102     c                   IF        *IN02 = *ON
042600000619     c                   z-add     0             TBEflt
042700991001     c                   clear                   TBEdtr
042800981102     c                   movel     Ksif          TBEsif
042900981102     c                   movel     Klin          TBElin
043000000914     c                   movel     'DP'          TBEapl
043100000914     c                   movel     Kcod          TBEcod
043200150923     C                   movel(p)  V9clnp        TBEke1
043300150923     C                   clear                   TBEke2
043400120511     C                   WRITE     TNTBE000                             99
043500981102      *
043600981102     c                   ELSE
043700000914     C   16              eval      TBEATB='A'
043800000914     C                   UPDATE    TNTBE000
043900981102     c                   ENDIF
044000980507      *
044100000907      *  riabilita l'annullamento se passato dalla window di copia
044200000907     C                   setoff                                       03
044300000907      *
044400980507     C                   ENDSR
044500000621      *****************************************************************
044600000621      *   ROUTINE DI PULIZIA SECONDA VIDEATA
044700000621      *****************************************************************
044800000621     C     PULIZIA       BEGSR
044900000621      *
045000000907     C                   movel     *blanks       V9cdes2
045100000907     C                   z-add     *zeros        V9cksc2
045200151012     C                   z-add     *zeros        V9kscre
045300150923     C                   move      *zeros        V9clnp2
045400000922     C                   z-add     *zeros        V9ctar2
045500151006     C                   z-add     *zeros        V9ctarl
045600151006     C                   z-add     *zeros        V9ctarv
045700151006     C                   z-add     *zeros        V9ctre2
045800151006     C                   z-add     *zeros        V9ctrel
045900151006     C                   z-add     *zeros        V9ctrev
046000051219     C                   move      *blank        V9ctm2
046100051219     C                   move      'S'           V9edi2
046200150925     C                   z-add     *zeros        V9cggi
046300000621      *
046400000621     C                   ENDSR
046500000622      *****************************************************************
046600000622      *   ROUTINE DI GESTIONE SUBFILE
046700000622      *****************************************************************
046800000622     C     GESSFL        BEGSR
046900000623      *
047000000622     c                   clear                   nrr               4 0
047100000623     c                   eval      *in23='0'
047200000622      *
047300000622     c                   eval      *in29='1'
047400000907     c                   write     tn09ctl
047500000622     c                   eval      *in29='0'
047600000622      *
047700000622     c     ktbe1         chain     tntbe002                           25
047800000622      *
047900000622     c                   dow       not *in25
048000000622      *
048100000622     c     tbeatb        ifeq      *blanks
048200150923     c                   movel     tbeke1        v9lnpsf
048300000918      *
048400150923     c                   movel(p)  TBEuni        Dlpd
048500000622      *
048600150923     c                   movel     §lpddes       v9dessf
048700150923     c                   movel     §lpdksc       v9kscsf
048800150923     c                   movel     §lpdtar       v9tarsf
048900150923     c                   movel     §lpdtarvtg    v9tarv
049000151005     c                   movel     §lpdtarlhs    v9tarl
049100150923     c                   movel     §lpdctm       v9ctmsf
049200150923     c                   movel     §lpdwrk       v9vabsf
049300150925     c                   movel     §lpdggi       v9ggi
049400000907      *
049500000622     c                   add       1             nrr
049600000907     c                   write     tn09sfl
049700000622      *
049800000622     c                   endif
049900000622      *
050000000622     c     ktbe1         reade     tntbe002                               25
050100000622      *
050200000622     c                   enddo
050300000622      *
050400000622     c                   exsr      gessf2
050500000622      *
050600000622     c                   endsr
050700000622      *****************************************************************
050800000622      *   ROUTINE gestione secondo video da subfile
050900000622      *****************************************************************
051000000622     C     GESSF2        BEGSR
051100000622      *
051200000622     c                   if        nrr>0
051300000622     c                   eval      nrr=1
051400000623     c                   eval      *in23='1'
051500000622     c                   endif
051600000622      *
051700000907     c                   write     tn09z03
051800000907     c                   exfmt     tn09ctl
051900000622      *
052000000907     c   KC
052100000907     cor KL              goto      esci
052200000622      *
052300000907     c   23              readc     tn09sfl                                04
052400000622      *
052500000623     c   23              dow       *in04=*off
052600000622      *
052700000907      * se ha scelto il record lo deve riportare sulla 2°videata
052800000907     c     v9opzsf       ifne      *blanks
052900150923     c                   movel     v9lnpsf       v9clnp
053000000623     c                   exsr      ried02
053100000623     c                   eval      *in04=*on
053200000907     c                   move      *blanks       v9opzsf
053300000622     c                   else
053400000907      *      rilegge se non ha scelto nulla
053500000907     c                   readc     tn09sfl                                04
053600000622     c                   endif
053700000622      *
053800000622     c                   enddo
053900000622      *
054000000707     c     esci          ENDSR
054100980506      *****************************************************************
054200980506      *   ROUTINE INIZIALE
054300980506      *****************************************************************
054400980506     C     *INZSR        BEGSR
054500980506      *
054600980506     C     *ENTRY        PLIST
054700980506     C                   PARM                    KPJBA
054800981102      *
054900981102     C                   Z-ADD     1             CODUT
055000981102     C                   CALL      'X§PARUT'
055100981102     C                   PARM                    UT§DSE0F
055200981102     C                   MOVEL     RAGUT         RSUT
055300981102      *
055400151012      * E' stato tolto l'F08 dal video (08) per la duplica dei record in immissione
055500151012     c                   setoff                                       08
055600151012      *
055700981102      * Controllo Testata Tabella
055800000725     C     KTBETT        CHAIN(N)  TNTBE000                           28
055900981102     c                   IF        *IN28 = *ON  or  TBEatb <> *blanks
056000981117     C                   eval      *inlr = *on
056100981102     c                   ELSE
056200981102     c                   movel     TBEsif        Ksif
056300981102     c                   movel     TBElin        Klin
056400981102     c                   ENDIF
056500981102      *
056600981102     C     KTBE          klist
056700981102     C                   KFLD                    Kcod
056800981102     C                   KFLD                    Kke1
056900981102     C                   KFLD                    Kke2
057000981102     C                   KFLD                    Klin
057100981102     C                   KFLD                    Ksif
057200000619     c*                  clear                   Kke2
057300981102      *
057400981102     C     KTBETT        klist
057500981102     C                   KFLD                    KTTcod
057600981102     C                   KFLD                    KTTke1
057700981102      *
057800000619     C     KTTABL        klist
057900000619     C                   KFLD                    KTTkut
058000000620     C                   KFLD                    KTlcod
058100000619     C                   KFLD                    KTTkey
058200000622      *
058300000622     C     KTBE1         klist
058400000622     C                   KFLD                    Kcod
058500000622     C                   KFLD                    Klin
058600000622     C                   KFLD                    Ksif
058700120511      *
058800120511     C     KCOPIA        klist
058900120511     C                   KFLD                    Kcod
059000150923     C                   KFLD                    key_linea
059100000619      *
059200150923     C                   eval      vilnpw = *blank
059300000707      *
059400980506     C                   ENDSR
059500980909      *****************************************************************
059600120511      *     copia tutti i records necessari x attivare un nuovo DEPOT
059700120511      *****************************************************************
059800120511     C     Copia_LINEA   begSR
059900120511      *
060000150923     c     *LIKE         DEFINE    TBEKE1        key_linea
060100150923     C                   CLEAR                   key_linea
060200120511      *
060300120511      *  aggancia la linea x reperire un DEPOT da cui copiare\
060400120511     c     Kcod          setll     tntbe01l
060500120511     c     Kcod          reade     tntbe01l
060600120511      *
060700120511     C                   Dow       not %Eof(Tntbe01l)
060800150923     c                   eval      dLPD = tbeUNI
060900120511      *   una volta trovata la LINEA salva la chiave per copiare
061000150923     c                   if        tbeKE1  = cpyLNP
061100150923     c                   eval      key_linea = TBEKE1
061200120511     c                   leave
061300120511     c                   end
061400120511     c     Kcod          reade     tntbe01l
061500120511     C                   ENDdo
061600120511      *
061700120511      *  Adesso esegue la copia esatta dei records cambiando la chiave
061800120511     C     KCOPIA        setll     tntbe01l
061900120511     c     KCOPIA        reade     tntbe01l
062000120511      *
062100120511     C                   Dow       not %Eof(Tntbe01l)
062200120511      *
062300150923     C                   movel(p)  V9clnp        TBEke1
062400150923     c                   clear                   TBEke2
062500120511     C                   WRITE     TNTBE000                             99
062600120511      *
062700120511     c     KCOPIA        reade     tntbe01l
062800120511     C                   ENDdo
062900120511      *
063000120511     C                   ENDSR
063100120511      *****************************************************************
063200980909** MSG  (Lungh. 78)                                                          *
063300151012Il dato è obbligatorio!!!                                                      1
063400000907Il codice esiste già e non può essere creato duplicandolo!                     2
063500151012Il codice è inesistente!!!                                                     3
063600151012Non è una Linea DPD!!!                                                         4
063700151012Il codice deve essere solo Numerico!!!                                         5
