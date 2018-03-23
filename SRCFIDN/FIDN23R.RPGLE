000100980506      *------------------------------------------------------------------------*
000200981022      * GESTIONE FASE RIFIUTO LIQUIDAZIONE TRANSATTIVA                         *
000300980506      *------------------------------------------------------------------------*
000400990809
000500980803      ****************************************************************
000600980803      *  FASI C.A. FISSATE A PROGRAMMA  (ricercare con 0 davanti)
000700980803      ****************************************************************
000800981022      *    110 = Rieseguo Liquidazione Transattiva  (WFAFI)
000900980803      ***************************************************************
001000990809
001100980506     H DECEDIT('0,') DATEDIT(*DMY.)
001200980506      *--------------------------------------------
001300981022     Ffidn23D   CF   E             WORKSTN
001400981022     Ffndka01L  UF A E           k DISK
001500981022     Ffnspe01l  if   E           k DISK
001600981022     Ffndct01l  uf   E           k DISK
001700981022     Ffndcl01l  uf   E           k DISK
001800981022     Ffndcf01l  if A E           k DISK
001900981022     fTNTMD01L  IF   E           K DISK
002000990809     fTITAS30C  IF   E           K DISK
002100981022     Fazorg01l  if   E           k DISK
002200981022     Ftabel00f  if   E           k DISK
002300981022      ****************************************************************
002400981022      *  RIEPILOGO INDICATORI
002500981022      ***************************************************************
002600981022      * 21/27- COMODO
002700981022      * 28   - ERRORE GENERICO
002800981022      * 29   - RIEMISSIONE FORMATO VIDEO
002900981022      * 30/31- COMODO
003000981022      * 90   - COMODO
003100981022      * 99   - COMODO
003200981022      *--------------------------------------------
003300981022     D                SDS
003400981022     D  VTCPGM                 1     10
003500981022      *
003600981022     D Fidn00ds      E DS
003700981022     d  dsnumca                6     19
003800981022     D Fidn05ds      E DS
003900081218     D FIDNBEDS      E DS
004000981022     D DSDN10        E DS                  EXTNAME(FIDN10DS)
004100981022     D fndfa         E DS                  EXTNAME(FNDFA00F)
004200981022      * Ds passaggio parametri a pgm ricerca tabelle
004300981022     D DSbs02        E DS                  EXTNAME(tibs02ds)
004400010905     D*** Dscv          E DS
004500981022     D Dtad          E DS
004600981230     D DSTB          E DS
004700981230     D DDCT01        E DS
004800990311     D TIBS69DS      E DS
004900990311     d DS_aco        E DS                  extname(CNACO00F)
005000990311     d DS_ind        E DS                  extname(CNIND00F)
005100990311     d DS_clp        E DS                  extname(CNCLP00F)
005200990311     d DS_cls        E DS                  extname(FNCLS00F)
005300981022
005400981022      *--------------------------------------------
005500981022     D KPJBA         E DS
005600981022     D CNCR80        E DS
005700981022     D UT§DSE0F      E DS
005800981022      *--------------------------------------------
005900981022     D ccc             S                   LIKE(acokcc)
006000981022     D ksc             S                   LIKE(acoksc)
006100981022     D dateu           s              8  0
006200981022     D min             S              2  0
006300981022     D W0140           S             14  0
006400981022     D WDTGIO          S              8  0
006500981022     D WORA            S              6  0
006600981022     D ora             S              2  0
006700981022     D XX              s              2  0
006800981022     D flgdel          s              2
006900981022     D prima           s              1
007000981022     D vtcksm          S              7  0
007100981022     D vtcksd          S              7  0
007200981022     D §des            S             30
007300981022     D w0033           s              3  3
007400981022     D w0133           s             13  3
007500981022     D implir          s             15  3
007600981230     D W§dctlnpc       s              3  0
007700990309     D W§dctport       s                   LIKE(§dctport)
007800981022     D wvlt            s                   LIKE(V1Cvpr)
007900981022     D §cod            S                   LIKE(TBLcod)
008000981022     D §key            S                   LIKE(TBLkey)
008100981022     D Wdfs            S                   LIKE(dcfdfc)
008200981022     D kcap            s              9
008300981022     D Kaac            S                   LIKE(dctaac)
008400981022     D Kfil            S                   LIKE(V1Cfil)
008500981022     D Knca            S                   LIKE(V1Cnca)
008600981022     d Ktrc            s                   like(dkatrc)
008700981022     d                                     inz('C')                             * trc dka cli.contat
008800981022     D Kfls            S                   LIKE(spefls)
008900981022     d                                     inz ('L')                            * codice luogo
009000981022     D Kcod            S                   LIKE(specod)
009100981022     d                                     inz ('006')                          * num.codice luogo
009200981022     D paxccc          s                   LIKE(ACOkcc)
009300981022     D paxdmt          s                   LIKE(ACOrag)
009400981022     d Wdatac          s              8  0
009500981022     d Wdaspe          s              8  0
009600981022     d Wfafi           s                   like(i00fca)
009700981022     d                                     inz(110)
009800981022      *
009900981022     D WLBDAT          DS                  INZ
010000981022     D  G02DAT                 1      8  0
010100981022     D  G02INV                 9     16  0
010200981022     D  G02ERR                17     17
010300981022     D  G02TGI                18     22  0
010400981022     D VIDIVA          DS
010500981022     D  w2civa                 1     14
010600981022     D  w2cive                15     16
010700981217      *  titolo videata (lunghezza massima 34)
010800981217     D TIT_1           C                   CONST('*** RIFIUTO ASSEGNO DI TRAENZ-
010900981217     D                                     A ***')
011000981022      *
011100981022      * ---------------------------------------------------------
011200981022      *
011300981022      * EMISSIONE PRIMA VIDEATA
011400981022    1C                   do        *hival
011500981022     C                   EXFMT     FI23D01
011600981022      *
011700981022     c                   SELECT
011800981022      * F3=FINE
011900981022     C                   WHEN      *inkc = *on
012000981022     c                   eval      o00f03 = 'S'
012100981022     c                   leave
012200981022      * F12=PRECEDENTE
012300981022     C                   WHEN      *inkl = *on
012400981022     c                   eval      o00f12 = 'S'
012500981022     c                   leave
012600981022      * F18= NOTE
012700981022     C                   WHEN      *inks = *on
012800981022     c                   exsr      SUB_note
012900981022     C                   ITER
013000981022     c* F06= CONFERMA RIFIUTO PAGAMENTO
013100981022     C                   WHEN      *inkf = *on
013200981022     c                   exsr      SUB_RIFIU
013300981022     c                   leave
013400981022      *
013500981022     c                   endsl
013600981022      *
013700981022    1c                   enddo                                                  *fine ciclo KC *on
013800981022      *
013900981022     c                   clear                   dsdn10
014000981022     c                   movel     'C'           i10tla
014100981022     c                   movel     dsdn10        kpjbu
014200981022     c                   call      'FIDN10R'
014300981022     c                   parm                    kpjba
014400981022      *
014500981022     c*
014600981022     c                   movel     fidn00ds      kpjbu
014700981022      *
014800981029     c                   clear                   dsbs02
014900981029     c                   movel     'C'           t02tla
015000981029     c                   call      'TIBS02R'
015100981029     c                   parm                    kpjba
015200981029     c                   parm                    dsbs02
015300981022     C                   EVAL      *INLR = *ON
015400981022      *****************************************************************
015500981022      *   ROUTINE SCRITTURA NOTE
015600981022      *****************************************************************
015700981022     C     SUB_NOTE      BEGSR
015800981022     c* valorizzo la DS di gestione note/descrizioni e chiamo il PGM FIDN10R
015900981022     C                   CLEAR                   DSDN10
016000981022     c                   move      v1cdca        i10dta
016100981022     c                   movel     'N'           i10trc
016200981022     c                   movel     'C'           i10tpd
016300981022     c                   movel     i00fca        i10nks
016400981022     c                   movel     dsnumca       i10nkt
016500981022     c                   movel     'M'           i10flm
016600981022     c                   movel(P)  dsdn10        kpjbu
016700981022     C                   CALL      'FIDN10R'                            99
016800981022     C                   PARM                    kpjba
016900981022     c
017000981022     c                   endsr
017100981022      *****************************************************************
017200981022      *   ROUTINE SCRITTURA FASE RIFIUTO
017300981022      *****************************************************************
017400981022     C     SUB_RIFIU     BEGSR
017500981105      *  caricamento file storico della fase   scrittura fase 250
017600981022     C                   movel     i00aac        Dcfaac
017700981022     C                   movel     i00fil        Dcffil
017800981022     C                   z-add     i00nca        Dcfnca
017900981022      *
018000981022     C                   movel     i00fca        Dcffca
018100981022     C                   movel     dctptr        Dcfptr
018200981022     C                   z-add     dateu         Dcfdfc
018300981022     C                   movel     wora          Dcfhfc
018400981022     C                   movel     i00fgs        Dcffev
018500981022     C                   movel     knmus         Dcfpru
018600981022      *
018700981022     c                   clear                   dcflet
018800981022     c                   clear                   dcfftr
018900981022     c                   clear                   dcfdtr
019000981022      *
019100981022     c                   WRITE     fndcf000
019200981022      * scrivo i dati della fase precedente
019300981022     c                   exsr      SUB_CAMBIO
019400981125      *  caricamento file storico della fase  di ripristino alla fase iniziale fittizia
019500981125     C                   movel     i00aac        Dcfaac
019600981125     C                   movel     i00fil        Dcffil
019700981125     C                   z-add     i00nca        Dcfnca
019800981125      *
019900981125     C                   movel     dctfca        Dcffca
020000981125     C                   movel     dctptr        Dcfptr
020100981125     C                   z-add     dateu         Dcfdfc
020200981125     C                   movel     wora          Dcfhfc
020300981125     C                   movel     i00fgs        Dcffev
020400981125     C                   movel     knmus         Dcfpru
020500981125      *
020600981125     c                   clear                   dcflet
020700981125     c                   clear                   dcfftr
020800981125     c                   clear                   dcfdtr
020900981125      *
021000981125     c                   WRITE     fndcf000
021100981022      *
021200981022     c* conferma note
021300981022     C                   CLEAR                   DSDN10
021400981022     C                   MOVEL     'C'           I10FLM
021500981022     c                   movel     'C'           i10tpd
021600981022     c                   movel     'N'           i10trc
021700981022     c                   movel(P)  dsnumca       i10nkt
021800981022     c                   movel     i00fca        i10nks
021900981022     c                   movel(P)  dsdn10        kpjbu
022000981022     C                   CALL      'FIDN10R'                            99
022100981022     C                   PARM                    kpjba
022200981022     c
022300981022      *
022400981022     c                   endsr
022500981022      *****************************************************************
022600981022      *   ROUTINE CAMBIO LIQUIDAZIONE TRANSATTIVA
022700981022      *****************************************************************
022800981022     C     SUB_CAMBIO    BEGSR
022900981022      *
023000981022      * CAMBIA IL MODO  DI TRANSARE
023100981022      * torno alla fase successiva la conferma
023200981022     c                   z-add     wfafi         dctfca
023300981102      *
023400981022     c                   clear                   fidn05ds
023500981030    2C                   if        DCTnev > *zeros
023600981022     c                   movel     'E'           I05fpe
023700981022   x2c                   else
023800981022     c                   movel     'N'           i05fpe
023900981022    1C                   endif
024000981030    2C                   if        DCTnde > *zeros
024100981030     c                   movel     'D'           I05fde
024200981102   x2c                   else
024300981102     c                   movel     'N'           i05fde
024400981030    1C                   endif
024500981022     C                   movel     'F'           I05MOD
024600981022     C                   movel     dctfca        i05fca
024700981022     C                   movel     dctfpr        i05fpr
024800981022     C                   movel     dctptr        i05tpc
024900981022     C                   movel     'O'           I05ffs
025000981022     c                   z-add     DCFdfc        i05dta
025100981022      * valorizzo il tipo anomalia CA
025200981022     c                   move      DCTtad        i05tad
025300981125      * Valorizzo numero CA
025400981125     c                   z-add     DCTaac        i05aac
025500981125     c                   z-add     DCTfil        i05fil
025600981125     c                   z-add     DCTnca        i05nca
025700981022      * richiamo il pgm di ricerca
025800981022     C                   call      'FIDN05R'
025900981022     C                   parm                    kpjba
026000981022     C                   parm                    fidn05ds
026100981022    1C                   if        o05err = ' '
026200981022     C                   movel     o05rec        fndfa
026300981022     c* sede
026400981022    2c                   if        dfagfs = 'S'
026500981022     c                   z-add     46            dctgfc
026600981022    2c                   endif
026700981022     c* P O partenza
026800981022    2c                   if        dfagfs = 'P'
026900981230     c                   z-add     W§dctlnpc     dctgfc
027000981022    2c                   endif
027100981022     c* P O arrivo
027200981022    2c                   if        dfagfs = 'A'
027300981022     c                   z-add     i00lna        dctgfc
027400981022    2c                   endif
027500981023     c* P O apertura C.A.
027600981023    2c                   if        dfagfs = 'F'
027700981023     c                   z-add     i00fil        dctgfc
027800981023    2c                   endif
027900981022    1c                   endif
028000990311     c* pulisco il flag dctpaga nel campo dctflo
028100990311     c                   clear                   §dctpaga
028200990311     c                   movel     ddct01        dctflo
028300981022     c* trasmissione
028400981222     c                   clear                   dctft1
028500981222     c                   clear                   dctft2
028600981022     c*
028700981022     c                   update    fndct000
028800981022      * pulisco i campi dell'importo della liquidazione transattiva /presunta/
028900981022      * l'importo trattenuto non lo pulisco perchè non so quando è stato inserito
029000981022     c* aggancio il file liquidazione ca
029100981022     c     kdct          chain     fndcl01l                           30
029200981022     c                   clear                   dclipr
029300981022     c                   clear                   dclvpr
029400981022     c                   clear                   dclcpr
029500981022     c                   clear                   dclipl
029600981022     c                   clear                   dclvpl
029700981022     c                   clear                   dclcpl
029800990311     c* pulisco  i dati relativi il pagamento (ABI/CAB, numero assegno, dta pagamento)
029900990311     c                   clear                   dclfic
030000990311     c                   clear                   dclabi
030100990311     c                   clear                   dclcab
030200990311     c                   clear                   dclasn
030300990311     c                   clear                   dcldea
030400990311     c*
030500981022     c                   clear                   dclftr
030600981022     c  n30              update    fndcl000
030700150701
030800150701      /free
030900150701       //?Cancella rcd 'P' in FNDKA, beneficiario
031000150701         chain (kaac:kfil:knca:'P') FNDKA01L;
031100150701         IF  %found(FNDKA01L);
031200150701           delete FNDKA000;
031300150701         ENDIF;
031400150701      /end-free
031500981022     c*
031600981022     c                   endsr
031700981022      *****************************************************************
031800981022      *   ROUTINE INIZIALE
031900981022      *****************************************************************
032000981022     C     *INZSR        BEGSR
032100981022      *
032200981022     C     *ENTRY        PLIST
032300981022     C                   PARM                    KPJBA
032400981022     C                   MOVEL     KPJBU         fidn00ds
032500981022      *
032600981022     C                   Z-ADD     1             CODUT
032700981022     C                   CALL      'X§PARUT'
032800981022     C                   PARM                    UT§DSE0F
032900981022     C                   MOVEL     RAGUT         RSUT
033000981022     C                   MOVEL     REC80         CNCR80
033100981022     C* ACCESSO   TABEL
033200981022     C     KTAB          KLIST
033300981022     C                   KFLD                    CODUT
033400981022     C                   KFLD                    §COD
033500981022     C                   KFLD                    §KEY
033600981022      * accesso fnspe01l
033700981022     c     Kspe          KLIST
033800981022     C                   KFLD                    kfls
033900981022     C                   KFLD                    v1cksm
034000981022     C                   KFLD                    kcod
034100981022      * accesso fndct01l
034200981022     C     Kdct          klist
034300981022     C                   KFLD                    kaac
034400981022     C                   KFLD                    kfil
034500981022     C                   KFLD                    knca
034600981022      * accesso fndka01l
034700981022     C     Kdka          klist
034800981022     C                   KFLD                    kaac
034900981022     C                   KFLD                    kfil
035000981022     C                   KFLD                    knca
035100981022     c                   KFLD                    ktrc
035200981022      * accesso fndcf01l
035300981022     c     kdcf          klist
035400981022     c                   kfld                    dctaac
035500981022     c                   kfld                    dctfil
035600981022     c                   kfld                    dctnca
035700981022      * accesso archivio bolle
035800981022     c     kbol01        klist
035900981022     c                   kfld                    i00aas
036000981022     c                   kfld                    i00lnp
036100981022     c                   kfld                    i00nrs
036200981022     c                   kfld                    i00nsp
036300981022      *
036400981027     C                   TIME                    W0140
036500981027      *
036600981022     C                   MOVE      W0140         WDTGIO
036700981022     C                   MOVEL     W0140         WORA
036800981022      * UDATE IN AAAAMMGG
036900981022     C                   Z-ADD     WDTGIO        G02DAT
037000981022     C                   MOVEL     *BLANK        G02ERR
037100981022     C                   CALL      'XSRDA8'
037200981022     C                   PARM                    WLBDAT
037300981022     C                   MOVEL     G02INV        DATEU
037400981211      * titolo pgm
037500981217     c                   movel     *blanks       vcttit
037600981217     c                   movel     TIT_1         vcttit
037700981217      *
037800981022     c                   move      i00fil        v1cfil
037900981022      * valorizzo il capo anno e numero C.A.
038000981022     c                   z-add     i00nca        v1cnca
038100981022      * aggancio la C.A. per recuperare i dati anagrafici della persona
038200981022     c                   z-add     i00aac        Kaac
038300981022     c                   z-add     i00fil        Kfil
038400981022     c                   z-add     i00nca        Knca
038500981022     C     kdct          CHAIN     fndct000                           31
038600981230     c*
038700981230     c                   movel     dctflo        ddct01
038800981230     c                   movel     §dctlnpc      W§DCTLNPC
038900981022      * MITTENTE o DESTINATARIO
039000981126      * decodifico il beneficiario
039100981022     c                   if        dctptr = 'D'
039200981022     c                   eval      v1mide = 'DESTINATARIO   '
039300981022     c                   else
039400981022     c                   eval      v1mide = 'MITTENTE       '
039500981022     c                   endif
039600981022      * calcolo data apertura C.A.
039700981022     C                   eval      wdatac = (DCTaac * 10000) + DCTmgc
039800981022     c*
039900981022     C                   CLEAR                   WLBDAT
040000981022     C                   Z-ADD     wdatac        G02inv
040100981022     C                   movel     '3'           G02err
040200981022     C                   CALL      'XSRDA8'
040300981022     C                   PARM                    WLBDAT
040400981022     c                   z-add     g02dat        v1cdca
040500981126      * recupero se esiste l'Interlocutore
040600981022     c                   clear                   dsdn10
040700981022     c                   movel     'C'           i10tpd
040800981022     c                   movel     'R'           i10flm
040900981022     c                   movel     'N'           i10std
041000981022     c                   movel     'D'           i10trc
041100981022     c                   movel     dsnumca       i10nkt
041200981022     c                   movel(P)  dsdn10        kpjbu
041300981022     C                   CALL      'FIDN10R'                            99
041400981022     C                   PARM                    kpjba
041500981022     c                   movel     kpjbu         dsdn10
041600981022     c     o10err        ifeq      *blanks
041700981022     c                   movel     o10not        v1cnco
041800981022     c                   else
041900981022     c                   movel     *blanks       v1cnco
042000981022     c                   end
042100981022      *  recupero l'importo preventivo del danno e la sua divisa
042200981023     c                   z-add     dctipv        v1cipv
042300981023     c                   move      dctvpv        v1cvpv
042400981022      * recupero il mittente ed il destinatario
042500981022     c     kbol01        chain     tntmd000                           30
042600990809     c     kbol01        chain     TITAS30c                           30
042700981230      * se recupero rileggo
042800981230     C                   movel     'TB'          §cod
042900981230     C                   movel(P)  tastbl        §key
043000981230     C     ktab          chain     tabel00f                           31
043100981230     C                   movel     tbluni        dstb
043200981230     c                   if        §TBrbl = 'R'
043300990809     c     kbol01        reade     TITAS30c                               30
043400981230     c                   endif
043500981022      * numero spedizione
043600981022     c                   movel     dctlnp        v1clnp
043700981022     c                   movel     dctnrs        v1cnrs
043800981022     c                   movel     dctnsp        v1cnsp
043900981022      * recupero data spedizione
044000981022     C                   eval      wdaspe = (i00aas * 10000) + tasmgs
044100981022     c*
044200981022     C                   CLEAR                   WLBDAT
044300981022     C                   Z-ADD     wdaspe        G02inv
044400981022     C                   movel     '3'           G02err
044500981022     C                   CALL      'XSRDA8'
044600981022     C                   PARM                    WLBDAT
044700981022     c                   z-add     g02dat        v1cdsp
044800981022      * decodifico la Linea di arrivo spedizione
044900981022     c                   move      i00lna        v1clna
045000981022     c     i00lna        chain     azorg                              31
045100981022     c  n31              movel     orgdes        v1dlna
045200981022     c
045300990311      * decodifico il tipo anomalia
045400990311     c                   clear                   dsbs02
045500990311     c                   clear                   dtad
045600990311     C                   MOVEL     'C'           t02mod
045700990311     C                   MOVEL     knsif         t02sif
045800990311     C                   MOVEL     'TAD'         t02cod
045900990311     C                   MOVEL(P)  dcttad        t02ke1
046000990311      *
046100990311     C                   CALL      'TIBS02R'
046200990311     C                   PARM                    KPJBA
046300990311     C                   PARM                    dsbs02
046400990311      *
046500990311    2C                   if        t02err = *BLANKS
046600990311     C                   MOVEL     T02UNI        DTAD
046700990311     c                   movel     §taddesc      v1dtad
046800990311     C                   MOVEL(P)  dcttad        v1ctad
046900990311    2C                   endif
047000981022      * Controllo il tipo bolla se franco o assegnato
047100981022      *
047200981022      * dati bolla
047300990309     c* in caso di cambio di porto il tipo porto è il contrario di quello della CA
047400990309     c                   SELECT
047500990309     c*
047600110504    1c                   when      §DCTcca <> '9'
047700990309     c                   eval      W§dctport = §dctport
047800990309     c*
047900990309    2c                   when      §dctport = 'F'
048000990309     c                   eval      W§dctport = 'A'
048100990309     c*
048200990309    2c                   when      §dctport = 'A'
048300990309     c                   eval      W§dctport = 'F'
048400990309     c*
048500990309    2c                   endsl
048600990309     c*
048700990309     c                   IF        W§dctport = 'F'
048800981022     c                   move      tasksc        VTCksm
048900981022     c                   move      *zeros        VTCksd
049000981022     c                   ELSE
049100981022     c                   move      tasccm        VTCksm
049200981022     c                   movel     tasksc        VTCksd
049300981022     c                   ENDIF
049400981022      *
049500081218      * altrimanti cerco il beneficiario con il programma esterno
049600081218     c                   clear                   fidnbeds
049700081218      *
049800081218     c                   movel     'B'           IBEmod
049900081218     c                   z-add     dctaac        IBEaac
050000081218     c                   z-add     dctfil        IBEfil
050100081218     c                   z-add     dctnca        IBEnca
050200081218     c                   z-add     dctaas        IBEaas
050300081218     c                   z-add     dctlnp        IBElnp
050400081218     c                   z-add     dctnrs        IBEnrs
050500081218     c                   z-add     dctnsp        IBEnsp
050600081218     c                   z-add     dctksc        IBEksc
050700081218     c                   movel     dctptr        IBEptr
050800081218     c                   movel     dctflo        IBEflo
050900081218     c                   movel     I00tpb        IBEtpb
051000081218     c                   If        %subst(knsif:7:1) = 'P'
051100081218     c                   eval      IBEsif = 'P'
051200081218     c                   EndIf
051300081218      *
051400081218     c                   call      'FIDNBER'
051500081218     c                   parm                    fidnbeds
051600081218      *
051700081218     c                   If        oberag <> *blanks
051800081218      *
051900081218     c                   movel     oberag        v1crsm
052000081218     c                   movel     obecap        v1ccam
052100081218     c                   movel     obeloc        v1clom
052200081218     c                   movel     obeprv        v1cprm
052300081218     c                   movel     obetel        v1ctel
052400081218     c                   movel     obefax        v1cfax
052500081218     c                   movel     obenaz        §key
052600081219     c                   movel     obeksc        v1cksm
052700081218     c                   endif
052800990311      *
052900981022      *
053000981022     c* decodifico la nazione se è diversa da blank
053100981022    1c                   if        §key <> *blanks
053200981022     c     ktab          chain     tabel                              30
053300981022    2c                   if        *in30 = *off and
053400981022     c                             tblflg = ' '
053500981022     c                   movel     tbluni        v1cnaz
053600981022    2c                   endif
053700981022    1c                   endif
053800981023      * recupero l'importo liquidato e trattenuto
053900981023     c* aggancio il file liquidazione ca
054000981023     c     kdct          chain     fndcl01l                           30
054100981023     c                   z-add     dclipr        v1cipr
054200981023     c                   move      dclvpr        v1cvpr
054300981022     c                   z-add     dclipl        v1cipl
054400981022     c                   move      dclvpl        v1cvpl
054500981022     c                   z-add     dclipt        v1cipt
054600981022     c                   move      dclvpt        v1cvpt
054700981023     C                   CLEAR                   WLBDAT
054800981023     C                   Z-ADD     dcldpt        G02inv
054900981023     C                   movel     '3'           G02err
055000981023     C                   CALL      'XSRDA8'
055100981023     C                   PARM                    WLBDAT
055200981023     c                   z-add     g02dat        v1cdpt
055300981023      *
055400981022      *
055500981022     C                   ENDSR
055600981022      *****************************************************************
