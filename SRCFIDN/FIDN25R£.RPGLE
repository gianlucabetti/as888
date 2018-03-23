000100981008      *-------------------------------------------------------------------------*
000200981106      *    ELENCO C.A. IN LIQUIDAZIONE TRANSATTIVA DA CONTABILIZZARE            *
000300981008      *-------------------------------------------------------------------------*
000400980721
000500980721      ****************************************************************
000600981026      *  FASI C.A. FISSATE A PROGRAMMA
000700981201      *_______________________________________________________________
000800981106      * 900 - Chiusura CA    (WCHCA)
000900980721      ***************************************************************
001000981201      *  CAUSALE CHIUSURA  FISSATA A PROGRAMMA
001100981201      *_______________________________________________________________
001200981202      * 05  - Chiusura dopo contabilizzazione ca     wcch
001300981201      ***************************************************************
001400980623
001500980521      ****************************************************************
001600980521      *  RIEPILOGO INDICATORI
001700980521      ***************************************************************
001800980904      * 30 - COMODO
001900980904      * 31 - COMODO
002000980904      ***************************************************************
002100980623
002200980521     H DECEDIT('0,') DATEDIT(*DMY.)
002300980623
002400981027     FFIDN00D   CF   E             WORKSTN  sfile(FI00S02:nrr1)
002500981027     FFNDCT01L  UF   E           K DISK
002600981106     FFNDCD01L  UF   E           K DISK
002700080626     FFNDCL01L  UF A E           K DISK
002800981106     FFNDKA01L  IF   E           K DISK
002900981106     FTNTMD01L  IF   E           K DISK
003000990809     FTITAS30C  IF   E           K DISK
003100981106     FFNSPE01L  IF   E           k DISK
003200981106     FCNACO00f  IF   E           k DISK
003300981106     FCNIND00f  IF   E           k DISK
003400980929     FFNDCF01L  UF A E           K DISK
003500981230     Ftabel00f  IF   E           k DISK
003600980521      *------------------------------------------------------------------------*
003700981106     D ccc             S                   LIKE(acokcc)
003800981106     D ksc             S                   LIKE(acoksc)
003900981106     D WCHCA           s                   like(dctfca)
004000981106     D                                     inz(900)
004100981201     d wcch            s                   like(dctcch)
004200981201     d                                     inz('05')
004300981106     d Ktrc            s                   like(dkatrc)
004400981106     d                                     inz('C')                             * trc dka cli.contat
004500981106     D Kfls            S                   LIKE(spefls)
004600981106     d                                     inz ('L')                            * codice luogo
004700981106     D Kcod            S                   LIKE(specod)
004800981106     d                                     inz ('006')                          * num.codice luogo
004900981111     D MITT            S              4    inz('MITT')
005000981111     D DEST            S              4    inz('DEST')
005100981106     D TRAT            S            198    INZ(*all'-')
005200981106     D W0040           S              4  0
005300980721     D W0140           S             14  0
005400980521     D Wdtgio          S              8  0
005500980521     D dateu           S              8  0
005600980529     D nrr1            S              4  0
005700980623     D nrr2            S              4  0
005800980929     D WORA            S              4  0
005900981106     D tes1            s            100
006000981106     D tes2            s             98
006100981230     D §cod            S                   LIKE(TBLcod)
006200981230     D §key            S                   LIKE(TBLkey)
006300981106     d wtcksm          S                   like(dctksc)
006400981106     d wtcksd          S                   like(dctksc)
006500990309     d w§dctport       S                   like(§dctport)
006600981106     d stpksc          S                   like(dctksc)
006700981111     d stprsd          S             45
006800981106     d stprsm          S                   like(dkarag)
006900981106     d stpinm          S                   like(dkavia)
007000981106     d stpcam          S                   like(dkacap)
007100981106     d stplom          S                   like(dkaloc)
007200981106     d stpprm          S                   like(dkaprv)
007300981106     d stpnaz          S                   like(dkanaz)
007400981106     d stpdpt          S                   like(dcldpt)
007500981111     d stprmn          S                   like(tasrmn)
007600981111     d stpfmd          S              4
007700981027
007800980521      *   D S   I N T E R N E / E S T E R N E
007900980521     D WLBDAT          DS                  INZ
008000980521     D  G02DAT                 1      8  0
008100980521     D  G02INV                 9     16  0
008200980521     D  G02ERR                17     17
008300980521     D  G02TGI                18     22  0
008400980525      *
008500981106     d TCUDS           DS
008600981106     d  f1                     1      1
008700981106     d  f3                     3      3
008800981106     d  f2                     2      2
008900981106     d  f4                     4      4
009000981106     d  f56                    5      6
009100980915      *----------------------------------
009200981110     D TES             S            100    DIM(8) CTDATA PERRCD(1)              TESTATA STAMPA
009300981106      *----------------------------------
009400980929     D FIDN00DS      E DS
009500981230     D DDCT01        E DS
009600981230     D DSTB          E DS
009700980521     D KPJBA         E DS
009800981106     D UT§DSE0F      E DS
009900981106     d  tcu                  398    697
010000981106     D                                     DIM(50)
010100981106     d  kcu                  698    847P 0
010200981106     D                                     DIM(50)
010300981106     D                                     PACKEVEN
010400981106      *--------------------------------------------
010500980930      *
010600980521     D                SDS
010700980521     D  VTCPGM                 1     10
010800980623      *------------------------------------------------------------------------*
010900980521      *---------------  C A L C O L O  ----------------------------------------*
011000980521      *------------------------------------------------------------------------*
011100980623
011200981026      * Aggiorno testata C.A. e file fasi
011300981027     c                   IF        i00nce = 1
011400981106      * Stampo la singola CA
011500981106     C     KDCT00        CHAIN     FNDCT000                           30
011600981230     c*
011700981230     c                   movel     dctflo        ddct01
011800981230     c*
011900981106     c  n30              exsr      SUB_STAMPA
012000981106      *
012100981027     c                   ELSE
012200981106      * Stampo tutte le CA selezionate
012300981027     c                   exsr      CA_multi
012400981027     c                   ENDIF
012500981026
012600980907     c                   EVAL      *INLR = *ON
012700980910      *****************************************************************
012800981106      * Routine di Stampa singola CA ed aggiornamento dei files
012900980910      *****************************************************************
013000981106     C     SUB_STAMPA    BEGSR
013100981106
013200981106      * Stampo i dati della C.A.
013300981106     c                   EXSR      SUB_STCA
013400981106     c*
013500981106      * Imposto campi per trasmissione dati  e chiusura C.A.
013600981027     c                   EXSR      RIEfndct
013700981027
013800981106      * Aggiorno testata C.A.
013900981106     c                   EXCEPT    UPDDCT
014000981106
014100981106      * Imposto e Aggiorno dettaglio C.A. come da testata CA
014200981106     c                   EXSR      UPDDCD
014300981027
014400981027      * Scrivo fasi C.A.
014500981027     c                   EXSR      WRTDCF
014600981027
014700981027     c                   ENDSR
014800981027      *****************************************************************
014900981027      * AGGIORNO FILES LEGGENDO SUBFILE
015000981027      *****************************************************************
015100981027     C     CA_multi      BEGSR
015200981027
015300981026      * Leggo  subfile
015400980929     c                   z-add     1             nrr2
015500981027     c     NRR2          CHAIN     FI00S02                            31
015600981026      *
015700981027      * Elaboro se C.A. non esclusa e scelta non bloccata
015800981027     c     *IN31         DOWEQ     *OFF
015900981027     c                   IF        V2Csce <> 'E'  and  V2Hi82 <> '1'
016000981027      *
016100981027     C     KDCT          CHAIN     FNDCT000                           30
016200981027      *
016300981106      * Stampo ed aggiorno i dati della singola CA
016400981106     c                   EXSR      SUB_STAMPA
016500981027      *
016600981027     c                   ENDIF
016700981027      *
016800980929     c                   add       1             nrr2
016900981027     c     NRR2          CHAIN     FI00S02                            31
017000980929     c                   ENDDO
017100980929      *
017200980910     c                   ENDSR
017300981106      *****************************************************************
017400981106      * Routine Recupero i dati da stampare
017500981106      *****************************************************************
017600981106     C     SUB_STCA      BEGSR
017700981106     c*
017800981106     c* pulisco i campi
017900981110     c                   clear                   stprsd
018000981111     c                   clear                   stprmn
018100981106     c                   clear                   stprsm
018200981106     c                   clear                   stpcam
018300981106     c                   clear                   stplom
018400981106     c                   clear                   stpinm
018500981106     c                   clear                   stpprm
018600981106     c                   clear                   stpnaz
018700981106      *
018800981126     c* se c'è il beneficiario alternativo prendo i suoi dati
018900981106      * prima guardo come codice e poi come dati
019000981106     c                   move      dctksc        w0040
019100981106    1c                   if        w0040 <> 8888 and w0040 <> 0000
019200981106      * cerco i dati anagrafici
019300981106     c                   z-add     dctksc        stpksc
019400981106     c                   z-add     dctkcc        ccc
019500981106     c                   z-add     dctksc        ksc
019600040519      * prima sull'anagrafico luoghi
019700040519      * cerco il codice luogo 008
019800040519     c                   movel     '008'         KCOD
019900040519     c     kSpe          Chain     Fnspe01l                           30
020000040519if  2c                   If        *In30
020100040519      * cerco il codice luogo 006
020200040519     c                   movel     '006'         KCOD
020300040519     c     kSpe          Chain     Fnspe01l                           30
020400040519    2c                   EndIf
020500040519if  2c                   If        Not *In30
020600040519     c                   movel     sperag        stprsm
020700040519     c                   movel     specap        stpcam
020800040519     c                   movel     speloc        stplom
020900040519     c                   movel     spepro        stpprm
021000040519     c                   movel     spenaz        stpnaz
021100040519   x2c                   Else
021200040519      * se non ho trovato nessun luogo cerco sull'anagrafica clienti
021300981106     c     kaco          chain     cnaco000                           30
021400981106     c* annullato
021500981106    2c  n30acoflg        ifeq      '*'
021600981106     c                   seton                                        30
021700981106    2c                   end
021800981106     c  n30              movel     acorag        stprsm
021900981106      * recupero l'indirizzo del cliente
022000981106     c     kaco          chain     cnind000                           30
022100981106     c                   movel     indcae        stpcam
022200981106     c                   movel     indcit        stplom
022300981106     c                   movel     indsta        stpnaz
022400981106     c                   movel     indvia        stpinm
022500040519e   2c                   EndIf
022600981106     c*
022700981106   x1c                   else
022800981106     c* se è = 8888 vado a cercare in FNDKA
022900981106    2c                   if        w0040 = 8888
023000981106     c     kdka          chain     fndka01l                           30
023100981106    3c     *in30         ifeq      *off
023200981106     c                   movel     dkarag        stprsm
023300981106     c                   movel     dkavia        stpinm
023400981106     c                   movel     dkacap        stpcam
023500981106     c                   movel     dkaloc        stplom
023600981106     c                   movel     dkaprv        stpprm
023700981106     c                   movel     dkanaz        stpnaz
023800981106    3c                   endif
023900981106    2c                   endif
024000981106     c* se è = 0000 vado in bolla a cercare i codici
024100981106    2c                   if        w0040 = 0000
024200990809     c     kbol01        chain     TITAS30c                           30
024300981230      * se recupero rileggo
024400981230     C                   movel     'TB'          §cod
024500981230     C                   movel(P)  tastbl        §key
024600981230     C     ktab          chain     tabel00f                           31
024700981230     C                   movel     tbluni        dstb
024800981230     c                   if        §TBrbl = 'R'
024900990809     c     kbol01        reade     TITAS30c                               30
025000981230     c                   endif
025100981230     c                   z-add     tasrmn        stprmn
025200990309      * Se assegnato in partenza verifico il codice destinatario
025300990309     c* in caso di cambio di porto il tipo porto è il contrario di quello della CA
025400990309     c                   SELECT
025500990309     c*
025600990309    1c                   when      tascca <> '9'
025700990309     c                   eval      W§dctport = §dctport
025800990309     c*
025900990309    2c                   when      §dctport = 'F'
026000990309     c                   eval      W§dctport = 'A'
026100990309     c*
026200990309    2c                   when      §dctport = 'A'
026300990309     c                   eval      W§dctport = 'F'
026400990309     c*
026500990309    2c                   endsl
026600990309    1c*
026700981106      * dati bolla
026800990309    3c                   IF        w§dctport = 'F'
026900981110     c                   move      tasksc        wtcksm
027000981110     c                   move      *zeros        wtcksd
027100981110     c                   ELSE
027200981110     c                   move      tasccm        wtcksm
027300981110     c                   movel     tasksc        wtcksd
027400981110    3c                   ENDIF
027500981110    3c                   if        dctptr = 'M'
027600981110     c                   z-add     wtcksm        stpksc
027700981110     c                   else
027800981110     c                   z-add     wtcksd        stpksc
027900981110    3c                   endif
028000981106     c* se il codice non è a zeros
028100981110    3c                   if        stpksc <> *zeros
028200981106     c* cerco il codice luogo 008 di questo codice
028300981110     c                   movel     '008'         KCOD
028400981110     c     kspe          chain     fnspe01l                           30
028500981110    4c     *in30         ifeq      *on
028600981106     c* provo con coidce luogo 006
028700981110     c                   movel     '006'         KCOD
028800981110     c     kspe          chain     fnspe01l                           30
028900981110    4c                   endif
029000981110     c*
029100981110    4c     *in30         ifeq      *off
029200981110     c                   movel     sperag        stprsm
029300981110     c                   movel     specap        stpcam
029400981110     c                   movel     speloc        stplom
029500981110     c                   movel     spepro        stpprm
029600981110     c                   movel     spenaz        stpnaz
029700981111    4c                   endif
029800981111    3c                   endif
029900981111     c* se il codice intestatario non esiste o se non sono stati trovati dei codici luoghi
030000981111     c* per il codice vado a recuperare i dati dal file TNTMD di sede
030100981111    3c                   if        *in30 = *on or
030200981111     c                             stpksc = *zeros
030300981111     c*
030400981111     c     kbol01        chain     tntmd01l                           30
030500981106      *
030600981106      * vado a recuperare i dati mittente o destinatario
030700981111    4c                   if        *in30 = *off
030800981111    5c                   if        dctptr = 'M'
030900981111     c                   movel     tmdrsd        stprsd
031000981106     c                   movel     tmdrsm        stprsm
031100981106     c                   movel     tmdinm        stpinm
031200981106     c                   move      tmdcam        stpcam
031300981106     c                   movel     tmdlom        stplom
031400981106     c                   movel     tmdprm        stpprm
031500981106     c                   movel     tmdnzm        stpnaz
031600981111   x5c                   else
031700981111     c                   movel     tmdrsm        stprsd
031800981106     c                   movel     tmdrsd        stprsm
031900981106     c                   movel     tmdind        stpinm
032000981106     c                   move      tmdcad        stpcam
032100981106     c                   movel     tmdlod        stplom
032200981106     c                   movel     tmdprd        stpprm
032300981106     c                   movel     tmdnzd        stpnaz
032400981111    5c                   endif
032500981111    4c                   endif
032600981111    3c                   endif
032700981106    2c                   endif
032800981106    1c                   endif
032900981111     c* se il campo del destinatario è = *blanks vado a recuperare dal file TNTMD
033000981111    1c                   if        stprsd = *blanks
033100981111     c     kbol01        chain     tntmd01l                           30
033200981111    2c                   if        dctptr = 'M'
033300981111     c  n30              movel     tmdrsd        stprsd
033400981111     c                   else
033500981111     c  n30              movel     tmdrsm        stprsd
033600981111    2c                   endif
033700981111    1c                   endif
033800981111     c* Definisco a chi si riferisce l'altra ragione sociale
033900981111    2c                   if        dctptr = 'M'
034000981111     c                   movel     dest          stpfmd
034100981111     c                   else
034200981111     c                   movel     mitt          stpfmd
034300981111    2c                   endif
034400990809     c* se il campo riferimento mittente è = 0 lo recupero da TITAS
034500981111    1c                   if        stprmn = *zeros
034600990809     c     kbol01        chain     TITAS30c                           30
034700981230      * se recupero rileggo
034800981230     C                   movel     'TB'          §cod
034900981230     C                   movel(P)  tastbl        §key
035000981230     C     ktab          chain     tabel00f                           31
035100981230     C                   movel     tbluni        dstb
035200981230     c                   if        §TBrbl = 'R'
035300990809     c     kbol01        reade     TITAS30c                               30
035400981230     c                   endif
035500981230     c                   z-add     tasrmn        stprmn
035600981111    1C                   ENDIF
035700981111     c*
035800981106      * recupero l'importo liquidato e trattenuto
035900981106     c* aggancio il file liquidazione ca
036000981106     c     kdcd          chain     fndcl01l                           30
036100080626      * se non lo trovo lo valorizzo
036200080626     c                   if        not %Found(fndcl01l)
036300080626     c                   clear                   fndcl000
036400080626     c                   movel     dctaac        Dclaac
036500080626     c                   movel     dctfil        Dclfil
036600080626     c                   z-add     dctnca        Dclnca
036700080626     c* importo concordato
036800080626     c                   z-add     dctipv        dclipr
036900080626     c                   movel     dctvpv        dclvpr
037000080626     c                   z-add     dctipv        dclipl
037100080626     c                   movel     dctvpv        dclvpl
037200080626     c                   clear                   dclftr
037300080626     c                   write     fndcl000
037400080626     c                   else
037500080626     c                   if        dclipr = 0
037600080626     c                   z-add     dctipv        dclipr
037700080626     c                   movel     dctvpv        dclvpr
037800080626     c                   endif
037900080626     c                   if        dclipl = 0
038000080626     c                   z-add     dclipr        dclipl
038100080626     c                   movel     dclvpr        dclvpl
038200080626     c                   endif
038300080626
038400080626     c                   update    fndcl000
038500080626     c                   endif
038600080626     c*
038700981106     C                   CLEAR                   WLBDAT
038800981106     C                   Z-ADD     dcldpt        G02inv
038900981106     C                   movel     '3'           G02err
039000981106     C                   CALL      'XSRDA8'
039100981106     C                   PARM                    WLBDAT
039200020821     c                   z-add     g02dat        stpdpt
039300981106      *
039400981106     C                   endsr
039500981027      *****************************************************************
039600981027      * IMPOSTO CAMPI DI FNDCT
039700981027      *****************************************************************
039800981027     C     RIEfndct      BEGSR
039900981027      *
040000981201     c                   movel     wcch          DCTcch
040100981106     c                   z-add     dateu         DCTdch
040200981106     c                   z-add     WCHCA         DCTfca
040300981222     c                   clear                   DCTft1
040400981222     c                   clear                   DCTft2
040500981027      *
040600981027     c                   ENDSR
040700981106      *****************************************************************
040800981106      * AGGIORNO FNDCD Dettaglio C.A.
040900981106      *****************************************************************
041000981106     C     UPDDCD        BEGSR
041100981106     c*
041200981106     c     kdcd          setll     fndcd01l
041300981106     c     kdcd          reade     fndcd01l                               31
041400981106     c*
041500981106      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
041600981106     c                   dow       *in31 = *off
041700981201     c                   eval      DCdcch = dctcch
041800981106     c                   eval      DCddch = dctdch
041900981222     c                   eval      DCdftr = dctft1
042000981106     c                   update    fndcd000
042100981106     c*
042200981106     c     kdcd          reade     fndcd01l                               31
042300981106     c*
042400981106     c                   enddo
042500981106     c*
042600981106     c                   endsr
042700981027      *****************************************************************
042800981106      * AGGIORNO FNDCF FASI C.A.  sia la fase che eseguo + la chiusura
042900981027      *****************************************************************
043000981027     C     WRTDCF        BEGSR
043100981027      *
043200980929     C                   z-add     DCTaaC        DCFaac
043300980929     C                   z-add     DCTfil        DCFfil
043400980929     C                   z-add     DCTnca        DCFnca
043500981106     c                   z-add     i00fca        DCFfca
043600980929     c                   z-add     dateu         DCFdfc
043700980929     c                   z-add     wora          DCFhfc
043800981027     c                   z-add     I00fgs        DCFfev
043900980929     c                   movel     knmus         DCFpru
044000981211     c                   movel     dctptr        DCFptr
044100980929      *
044200980929     c                   WRITE     FNDCF000
044300981106      * chiusura C.A.
044400981106     c                   z-add     wchca         DCFfca
044500981106     c                   WRITE     FNDCF000
044600980929      *
044700980929     c                   ENDSR
044800980910      *****************************************************************
044900980527      * ROUTINE INIZIALE
045000980527      *****************************************************************
045100980527     C     *INZSR        BEGSR
045200980615      *
045300980521     C     *ENTRY        PLIST
045400980521     C                   PARM                    KPJBA
045500981026      *
045600981026     C                   MOVEL     KPJBU         FIDN00DS
045700981106      *
045800981106     C                   Z-ADD     1             CODUT
045900981106     C                   CALL      'X§PARUT'
046000981106     C                   PARM                    UT§DSE0F
046100981106     C                   MOVEL     RAGUT         RSUT             20
046200981106     C*--- RICERCA CAPOCONTI
046300981106     C                   DO        50            X                 2 0
046400981106     C                   MOVE      TCU(X)        TCUDS
046500981106     C     f56           CABNE     'CG'          END1
046600981106     C     f4            COMP      '1'                                    21
046700981106     C     f4            COMP      '2'                                    22
046800981106     C     f4            COMP      '3'                                    23
046900981106     C     f4            COMP      '6'                                    27
047000981106     C** 1 CLIENTI   21
047100981106     C** 2 FORNITORI 22
047200981106     C** 3 AGENTI    23
047300981106     C     f3            comp      '0'                                242425
047400981106     C     f3            comp      'I'                                    26
047500981106     C** 0 ITALIA   25
047600981106     C** 1 ESTERO   24
047700981106     ** I CAPO CONTO IVA
047800981106     c                   if        *in21 = *on
047900981106     C   24              Z-ADD     KCU(X)        KCE               4 0
048000981106     C   25              Z-ADD     KCU(X)        KCI               4 0
048100981106     c                   end
048200981106     c                   if        *in22 = *on
048300981106     C   24              Z-ADD     KCU(X)        KFE               4 0
048400981106     C   25              Z-ADD     KCU(X)        KFI               4 0
048500981106     c                   end
048600981106     c                   if        *in23 = *on
048700981106     C   24              Z-ADD     KCU(X)        KAE               4 0
048800981106     C   25              Z-ADD     KCU(X)        KAI               4 0
048900981106     c                   end
049000981106     C   26              Z-ADD     KCU(X)        KIVA              4 0
049100981106     C   27              Z-ADD     KCU(X)        KBNA              4 0
049200981106     C     end1          tag
049300981106     C                   end
049400981106     C                   SETOFf                                       212223
049500981106     C                   sETOFf                                       242526
049600981106     C                   SETOFf                                       27
049700981106      *
049800980907      *
049900980721      * reperisco data e ora
050000980721     C                   TIME                    W0140
050100980521      * UDATE IN GGMMAAAA
050200980521     C                   MOVE      W0140         WDTGIO
050300980929     C                   MOVEL     W0140         WORA
050400980521      * UDATE IN AAAAMMGG
050500980521     C                   Z-ADD     WDTGIO        G02DAT
050600980521     C                   MOVEL     *BLANK        G02ERR
050700980521     C                   CALL      'XSRDA8'
050800980521     C                   PARM                    WLBDAT
050900980521     C                   MOVEL     G02INV        DATEU
051000981027      *
051100981026     C     KDCT00        KLIST
051200981026     C                   KFLD                    I00AAC
051300981026     C                   KFLD                    I00FIL
051400981026     C                   KFLD                    I00NCA
051500980522      *
051600980929     C     KDCT          KLIST
051700980929     C                   KFLD                    V2HAAC
051800980929     C                   KFLD                    V2CFIL
051900980929     C                   KFLD                    V2CNCA
052000981106      *
052100981106     C     KDCD          KLIST
052200981106     C                   KFLD                    DCTAAC
052300981106     C                   KFLD                    DCTFIL
052400981106     C                   KFLD                    DCTNCA
052500981106      * accesso cnaco / cnind
052600981106     C     Kaco          klist
052700981106     C                   KFLD                    codut
052800981106     C                   KFLD                    ccc
052900981106     C                   KFLD                    ksc
053000981106      * accesso fndka01l
053100981106     C     KDKA          klist
053200981106     C                   KFLD                    dctaac
053300981106     C                   KFLD                    dctfil
053400981106     C                   KFLD                    dctnca
053500981106     c                   KFLD                    ktrc
053600981106      * accesso archivio bolle
053700981106     c     kbol01        klist
053800981106     c                   kfld                    dctaas
053900981106     c                   kfld                    dctlnp
054000981106     c                   kfld                    dctnrs
054100981106     c                   kfld                    dctnsp
054200981106      * accesso fnspe01l
054300981106     c     Kspe          KLIST
054400981106     C                   KFLD                    kfls
054500981106     C                   KFLD                    stpksc
054600981106     C                   KFLD                    kcod
054700981230     C* ACCESSO   TABEL
054800981230     C     KTAB          KLIST
054900981230     C                   KFLD                    CODUT
055000981230     C                   KFLD                    §COD
055100981230     C                   KFLD                    §KEY
055200981027      *
055300981106      * Stampo la Testata dell'elenco
055400981106     c                   movel     tes(1)        tes1
055500981110     c                   movel     tes(4)        tes2
055600981106      *
055700980521     C                   ENDSR
055800980521      *---------------------------------------------------------------------------------------------
055900981027     OFNDCT000  E            UPDDCT
056000981106     O                       DCTdch
056100981202     O                       DCTcch
056200980929     O                       DCTfca
056300981222     O                       DCTft1
056400981222     O                       DCTft2
056500990125** TES  (Lungh. 100)   -Disegno stampa-
056600981111Anno P.O. N°C.A. Ragione Sociale Intestatario                     Ragione Sociale
056700981110                 Indirizzo
056800981111                 Cap       Localita'            Pr Naz
056900981110                 Riferim.Mittente   IMPORTO CONCORDATO VAL  IMPORTO TRATTENUTO VAL  In Data   !  !
057000981110xxxx xxx xxxxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
057100981110                 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
057200981111                 xxxxxxxxx xxxxxxxxxxxxxxxxxxxx xx xxx
057300981111xxxxxxxxxxxx xxxx xxxxxxxxxxxxxxx  xxx.xxx.xxx.xxx,xxx xxx xxx.xxx.xxx.xxx,xxx xxx xx/xx/xxxx !  !
