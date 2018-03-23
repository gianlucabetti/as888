000100160620     H DECEDIT('0,') DATEDIT(*DMY.)
000200950315      *---------*----------------------------------------------------*
000300160614      *         - Gestione resi DPD                       -          *
000400000426      *                                                              *
000500141020      *  Il programma prevede il caricamento a video delle spedizioni
000600160614      *  inserite nel file FIBARDPD1F per essere rese o comunque elaborate
000700141020      *
000800950315      *--------------------------------------------------------------*
000900160614     FFNLG27d   CF   E             WORKSTN SFILE(Lg270s1:NRR3)
001000160616     ffibardpd1fuf   e             disk    infds(bardpd)
001100160614     ffnarb01l  if   e           k disk
001200160614     ftigcp01l  if   e           k disk
001300160615     ftabel00f  if   e           k disk
001400950320      *----------------------------------------------------------------*
001500950320      *    s c h i e r e                                               *
001600950320      *----------------------------------------------------------------*
001700160616     d bardpd          ds
001800160617     d  dpdnrr               397    400i 0
001900950315     D KPJBA         E DS
002000140130      *
002100140130     d  svkpjbu        s                   like(kpjbu)
002200140130      *
002300140130     c*
002400160614     D trul90ds      E DS
002500141022     D fnlg82ds      E DS
002600080707     d trul06ds      e ds
002700160614     d  lin                    1     90  0 dim(30)
002800110118     D ddatiute      e ds
002900110118     D azuteds       e ds                  extname(AZUTE00F)
003000110118     D tibs34ds      E DS                  inz
003100141021      * DS per richiamo pgm FNLR36R interrogazione bolle
003200141021     D DSLR36          DS
003300141021     D  P36AAS                14     17  0
003400141021     D  P36LNP                18     20  0
003500141021     D  P36NRS                21     22  0
003600141021     D  P36NSP                23     29  0
003700141021     D  P36F03                30     30
003800141021     D  P36FLG                31     31
003900081117     D                SDS
004000081117     D  NOMPGM                 1     10
004100110117     D dtaiso          s               d
004200110117     D dtaeur          s               d   datfmt(*eur)
004300110117     D oggi            S              8  0
004400141028     d NotUsed         S             16A
004500141028     d ReturnSize      S              9B 0
004600141028     d SizeList        S              9B 0
004700141028     d stmrec1         S              4  0
004800141028     d*---
004900141028     d MaxKey          C                   2
005000141028     d Ascendente      C                   1
005100141028     d Discendente     C                   2
005200141028     d Carattere       C                   6
005300141028     d Put             C                   1
005400141028     d EndPut          C                   2
005500141028     d Get             C                   3
005600141028     d Numerico        C                   8
005700141028     d SflRcd          DS
005800160614     d  V3cLNa
005900160614     d  V3CRSD
006000160614     d  V3cpar
006100141028     d  V3CSCE
006200160614     d  V3hAAS
006300160614     d  V3hLNP
006400160614     d  V3hNRS
006500160614     d  V3hNSP
006600160614     d  V3Cfgs
006700160615     d  V3Chfs
006800160615     d  V3Cdfs
006900160617     d  V3hfgc
007000160617     d  V3hagc
007100160617     d  V3hngc
007200160614     d  V3hind
007300160614     d  V3hsce
007400160616     d  V3hNrr
007500160616     d  V3cgia
007600141028     d  Selected                      1A
007700141028
007800141028      /COPY QSYSINC/QRPGLESRC,QLGSORT
007900141028     d QLGKL                         16    DIM(MaxKey)
008000141028     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
008100141028     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
008200141028     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
008300141028     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
008400141028      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
008500141028      /COPY QSYSINC/QRPGLESRC,QUSEC
008600160615     D CMA4f6          S              1    DIM(130) CTDATA PERRCD(65)
008700160615     D CMA5f6          S              1    DIM(130) CTDATA PERRCD(65)
008800160615     D CMETf6          S              1    DIM(130) CTDATA PERRCD(65)
008900950315      *---------------------------------------------------------------*
009000950315      * FLUSSO PRINCIPALE                                             *
009100950315      *---------------------------------------------------------------*
009200080508     C*  Caricamento subfile errori
009300141028     c     inizio        tag
009400080508     C                   EXSR      INZS01
009500101220     c*  Se il subfile degli errori è vuoto forzo l'uscita dal pgm
009600080526     C     NRR3          IFEQ      0
009700101223     c                   exsr      srfine
009800080526     C                   END
009900141028     C                   Z-ADD     nrr3          NRRlast           4 0
010000141028     c                   exsr      sort
010100080512     C                   Z-ADD     1             NRR3
010200141023     c     emetti        tag
010300101223      *  gestione subfile degli errori
010400080508     C                   DO        *hival
010500160614     C                   WRITE     lg270z1
010600160614     C                   EXFMT     lg270c1
010700080508      *  Fine Lavoro
010800101220     c                   If        *inKC  = '1'
010900101222     C                   exsr      srfine
011000080508     C                   END
011100080508      *  Guida
011200101220     c                   If        *inKL  = '1'
011300101222     C                   exsr      srfine
011400080508     C                   END
011500141023      *  sel. tutte
011600141023     c                   If        *inKa  = '1'
011700141023     c                   clear                   nr                4 0
011800141023     c                   do        *hival        nr
011900160614     C     nr            chain     lg270s1                            32
012000141023     c                   if        *in32
012100141023     c                   leave
012200141023     c                   end
012300141023     c                   if        v3hsce = *blank
012400141023     c                   move      '1'           v3csce
012500141023     c                   end
012600141030     c                   setoff                                       8788
012700160614     c                   update    lg270s1
012800141023     c                   enddo
012900141023     c                   goto      emetti
013000141023     C                   END
013100080508      *  Loop di controlli
013200080508     C                   EXSR      CTR01
013300080701     c   28              iter
013400141023      * per conferme esegue opzioni di aggiornamento
013500141023     c                   if        *inkf
013600141021     c                   exsr      sraggio
013700141028     c                   goto      inizio
013800141023     c                   endif
013900080508     C                   ENDdo
014000141021      * lancia programmi di esecuzione per opzione
014100080508     c*
014200080508     c                   exsr      srfine
014300950317      *---------------------------------------------------------------*
014400950317      * Gestione videata subfile errori                               *
014500950317      *---------------------------------------------------------------*
014600080508     C     INZS01        BEGSR
014700080508     C                   SETOFF                                       202190
014800160614     C                   WRITE     lg270c1
014900950317     C                   SETON                                        2021
015000080512     C                   Z-ADD     0             NRR3              4 0
015100101223     c*
015200101223     c                   exsr      srsfl
015300101223     c*
015400080523     c                   endsr
015500080523     c**********************************************************************
015600101223     c* memorizzo spedizione errata nel sfl
015700080523     c**********************************************************************
015800101223     c     srsfl         begsr
015900101223     c* se ho solo un'anomalia a livello di spedizione, la carico,
016000101223     c* altrimenti aggancio il dettaglio
016100160614     c     *start        setll     fibardpd1f
016200101223     c                   do        *hival
016300160614     c                   read      fibardpd1f
016400160614     c                   if        %eof(fibardpd1f)
016500101223     c                   leave
016600101223     c                   end
016700160616     c                   if        br1dpdsts <> *blank
016800160616     c                   iter
016900160616     c                   endif
017000160614     c     br1dpdlna     lookup    lin                                    90
017100160614     c  n90              iter
017200160614     C                   Z-ADD     BR1DPDLNa     v3clna
017300160614     C                   move      BR1DPDPRN     v3cpar
017400160614     C                   Z-ADD     BR1DPDFGS     v3cfgs
017500160614     C                   move      BR1DPDDFS     dtaiso
017600160614     C                   move      dtaiso        dtaeur
017700160614     C                   move      dtaeur        v3cdfs
017800160615     C                   Z-ADD     BR1DPDHms     v3chfs
017900160614     C                   Z-ADD     BR1DPDAAS     v3haas
018000160614     C                   Z-ADD     BR1DPDLNP     v3hlnp
018100160614     C                   Z-ADD     BR1DPDNRS     v3hnrs
018200160614     C                   Z-ADD     BR1DPDNSP     v3hnsp
018300160616     C                   Z-ADD     dpdnrr        v3hnrr
018400160614     c     karb          chain     fnarb01l
018500160614     c                   if        %found(fnarb01l)
018600141021     C                   movel(p)  arbrsd        v3crsd
018700160614     c                   endif
018800141022      *
018900160614     c                   if        arbdcm > 0
019000160614     c                   iter
019100141021     c                   endif
019200160616     C                   clear                   v3cgia
019300160617     C                   clear                   v3hagc
019400160617     C                   clear                   v3hngc
019500160617     C                   clear                   v3hfgc
019600160616     c     kgia          chain     tigcp01l
019700160616     c                   if        %found(tigcp01l) and gcpfas < 40
019800160616     C                   move      'G'           v3cgia
019900160617     C                   z-add     gcpagc        v3hagc
020000160617     C                   z-add     gcpngc        v3hngc
020100160617     C                   z-add     gcpfgc        v3hfgc
020200160616     c                   endif
020300160616      *
020400080526     C                   ADD       1             NRR3
020500160614     C                   WRITE     lg270s1
020600101223     c                   enddo
020700080526     c                   endsr
020800101223      *---------------------------------------------------------------*
020900101223      *  Loop di errori  subfile errori                               *
021000101223      *---------------------------------------------------------------*
021100101223     C     CTR01         BEGSR
021200110104     c                   setoff                                       284044
021300950315      *
021400110104     c                   clear                   nr                4 0
021500110104     c                   do        *hival        nr
021600160614     C     nr            chain     lg270s1                            32
021700110104     c                   if        *in32
021800110104     c                   leave
021900110104     c                   end
022000141023     c                   select
022100141023      *  Interrogazione bolla
022200141023     C                   when      v3csce = '5'
022300141023     c                   clear                   v3csce
022400141023     C                   MOVEL     '1'           P36FLG
022500160614     C                   Z-ADD     V3hAAS        P36AAS
022600160614     C                   Z-ADD     V3hLNP        P36LNP
022700160614     C                   Z-ADD     V3hNSP        P36NSP
022800160614     C                   Z-ADD     V3hNRS        P36NRS
022900141023     C                   MOVEL(P)  DSLR36        KPJBU
023000141023     C                   CALL      'FNLR36R'
023100141023     c                   PARM                    KPJBA
023200141023     c                   endsl
023300160614     C                   UPDATE    lg270s1
023400141022     c   28              leave
023500141022     C                   ENDdo
023600141022     c*
023700141022     c                   z-add     1             nrr3
023800950317     C*
023900950317     C                   ENDSR
024000080707      *---------------------------------------------------------------*
024100141027      * esegue opzioni con cmd di conferma
024200080707      *---------------------------------------------------------------*
024300110119     C     sraggio       BEGSR
024400140130      **
024500141021     c                   clear                   nr                4 0
024600141021     c                   do        *hival        nr
024700160614     C     nr            chain     lg270s1                            32
024800141021     c                   if        *in32
024900141021     c                   leave
025000141021     c                   end
025100141118     c                   setoff                                       28
025200141021     c                   select
025300160614      *  inserisce spedizione vistata senza eseguire reso
025400141023     C                   when      v3csce = '2'
025500141023     c                   exsr      liberaSPE
025600141023     c                   seton                                        44
025700141023     c                   move      '1'           v3hsce
025800160614      *  Esegue reso
025900141021     C                   when      v3csce = '1'
026000141023      * non ho disposizioni dirottamento diretto
026100141113     c                   clear                   fnlg82ds
026200141022     c                   eval      D82ges  = 'A'
026300160616     c                   if        v3cgia <> *blank
026400160616     c                   eval      D82gia  = 'S'
026500160616     c                   else
026600141022     c                   eval      D82gia  = 'N'
026700160616     c                   endif
026800160614     c                   move      v3haas        d82AAS
026900160614     c                   move      v3hlnp        d82LNP
027000160614     c                   move      v3hnrs        d82NRS
027100160614     c                   move      v3hnsp        d82NSP
027200160616     c                   move      v3clna        d82fgs
027300160617     c                   move      v3hagc        d82agc
027400160617     c                   move      v3hngc        d82ngc
027500160617     c                   move      v3hfgc        d82fgc
027600160614     c                   eval      D82dis  = 'R'
027700141022     c                   eval      D82tri  = 'C'
027800160615     C                   eval      d82MOB  =   d90MdB
027900160614     C                   eval      d82PRB  =   d90PRB
028000160615     C                   eval      d82MOE  =   d90MdE
028100160614     C                   eval      d82PRE  =   d90PRE
028200160614     C                   eval      d82PSS  =   d90PSS
028300160614     C                   eval      d82MDB4 =   d90MDB4
028400160614     C                   eval      d82MDB5 =   d90MDB5
028500160614     C                   eval      d82BSL  =   d90BSL
028600160614     C                   eval      d82PSL  =   d90PSL
028700160614     C                   eval      d82PRB4 =   d90PRB4
028800160614     C                   eval      d82PRB5 =   d90PRB5
028900141022     c                   movel(p)  fnlg82ds      kpjbu
029000141022     c                   call      'FNLG20R'
029100141022     c                   parm                    kpjba
029200141022     c                   movel(p)  kpjbu         fnlg82ds
029300160617     c     karb          chain     fnarb01l
029400160617     c                   if        arbdcm = 0  or
029500160617     c                             d82f12 = '1'
029600160614     c*                  clear                   v3hind
029700141027     c                   else
029800160614     c                   exsr      liberaspe
029900141027     c                   endif
030000141023     c*
030100141021     C                   ENDsl
030200141021      *  Aggiorno subfile
030300141021     c     v3hind        comp      ' '                                88
030400141027     c     v3hsce        comp      ' '                                44
030500141021     c                   clear                   v3csce
030600160614     C                   UPDATE    lg270s1
030700141023     c                   clear                   v3hsce
030800141028     c                   setoff                                       4440
030900110119     c                   enddo
031000140130      **
031100141021     c                   z-add     1             nrr3
031200110119     C                   ENDSR
031300141022      *_______________________________________________________
031400141022     c     LiberaSpe     begsr
031500141022      *_______________________________________________________
031600160616     c     v3hnrr        chain     fibardpd1f
031700160616     c                   move      v3csce        BR1DPDSTS
031800160614     c                   move      knmus         BR1DPDUTE
031900160614     c                   move      oggi          BR1DPDDAT
032000160614     c                   move      ora           BR1DPDORA
032100160614     c                   update    BARDPD001
032200141022     c                   endsr
032300110119      *---------------------------------------------------------------*
032400110119      * fine
032500110119      *---------------------------------------------------------------*
032600110119     C     srfine        BEGSR
032700080526     C*
032800080526     C                   SETON                                        LR
032900080710     c                   return
033000080707     c*
033100080707     C                   ENDSR
033200080509     C*-----------------------------------------------------*
033300080509     C     *INZSR        BEGSR
033400080509     C*-----------------------------------------------------*
033500080508      *
033600080508     C     *ENTRY        PLIST
033700101220     C                   PARM                    kpjba
033800160615     c     karb          klist
033900160615     c                   kfld                    v3haas
034000160615     c                   kfld                    v3hlnp
034100160615     c                   kfld                    v3hnrs
034200160615     c                   kfld                    v3hnsp
034300160616     c     kgia          klist
034400160616     c                   kfld                    v3haas
034500160616     c                   kfld                    v3hlnp
034600160616     c                   kfld                    v3hnrs
034700160616     c                   kfld                    v3hnsp
034800160616     c                   kfld                    wprg
034900160616     c                   z-add     0             wprg              2 0
035000140130      *
035100160615     c     ktab          klist
035200160615     c                   kfld                    tblkut
035300160615     c                   kfld                    tblcod
035400160615     c                   kfld                    tblkey
035500160615     c                   z-add     1             tblkut
035600160615      *
035700160614     C                   TIME                    WHHUDT           14 0
035800160614     C                   MOVE      WHHUDT        dtaeur
035900160614     C                   MOVE      dtaeur        dtaiso
036000160614     C                   MOVE      dtaiso        oggi
036100160614     C                   MOVEL     WHHUDT        ORA               6 0
036200080515     C* Decodifica Elaboratore
036300080515     c     *dtaara       define    §azute        azuteds
036400080515     c     *dtaara       define    §datiute      ddatiute
036500080515     C                   in(E)     *dtaara
036600080515     C                   IF        %Error  or  RSUT = *blanks
036700080515     C                   call      'TIBS34R'
036800080515     C                   parm                    Tibs34Ds
036900080515     C                   in        *dtaara
037000080515     c                   ENDIF
037100160614     C* Caricamento £7
037200160614     C* Caricamento £1
037300160615     c                   clear                   trul06ds
037400160615     C                   MOVEA     *ZEROS        lin
037500160614     C                   MOVEL     *BLANKS       TBLKEY
037600160614     C                   MOVEL     knmus         TBLKEY
037700160614     C                   MOVEL     '£7'          TBLCOD
037800160614     C     KTAB          CHAIN     TABEL                              31
037900160614     C     *IN31         IFEQ      '0'
038000160614     C     TBLFLG        ANDEQ     ' '
038100160615     C                   MOVEl     TBLUNI        trul06ds
038200160614     C                   ELSE
038300160614     c                   eval      d06cod = '£1'
038400160614     c                   movel     o34pou        d06key
038500160614     c                   movel     trul06ds      kpjbu
038600160614     c                   call      'TRUL06R'
038700160614     c                   parm                    kpjba
038800160614     c                   movel     kpjbu         trul06ds
038900160614     C                   ENDif
039000160614     c                   exsr      RICHSTAMPAN
039100080509     C                   ENDSR
039200160614     C**************************************************************************
039300160614      * RICHIEDO LE STAMPANTI per stampa LDV
039400160614     C**************************************************************************
039500160614     c     RICHSTAMPAN   BEGSR
039600160614     C                   CLEAR                   TRUL90DS
039700160614      *
039800160614     C                   MOVEL     'S'           d90RSB
039900160614     C                   MOVEL     'S'           d90RSE
040000160614     C                   CALL      'TRUL90R'
040100160614     C                   PARM                    KPJBA
040200160614     C                   PARM                    trul90DS
040300160614      *
040400160614     C* F3 - FINE
040500160614     c                   if        d90f3<>'1'
040600160614      *
040700160614      * OVRPRTF BOLLE A4
040800160614     C                   MOVEA     D90PRB4       CMA4f6(30)
040900160614     C                   MOVEA     D90MDB4       CMA4f6(52)
041000160614     C*
041100160614     C                   Z-ADD     130           LUNG             15 5
041200160614     C                   CLEAR                   COMMAN          130
041300160614     C                   MOVEA     CMA4f6        COMMAN
041400160614     C                   CALL      'QCMDEXC'                            91
041500160614     C                   PARM                    COMMAN
041600160614     C                   PARM                    LUNG
041700160614      *
041800160614     C                   if        not *in91
041900160614      *
042000160614      * OVRPRTF BOLLE A5
042100160614     C                   MOVEA     D90PRB5       CMA5f6(30)
042200160614     C                   MOVEA     D90MDB5       CMA5f6(52)
042300160614     C*
042400160614     C                   Z-ADD     130           LUNG
042500160614     C                   CLEAR                   COMMAN
042600160614     C                   MOVEA     CMA5f6        COMMAN
042700160614     C                   CALL      'QCMDEXC'                            91
042800160614     C                   PARM                    COMMAN
042900160614     C                   PARM                    LUNG
043000160614     c                   endif
043100160614     C                   if        not *in91
043200160614      *
043300160614      * OVRPRTF etichette
043400160614     C                   MOVEA     D90PRe        CMetf6(30)
043500160614     C                   MOVEA     D90MDE        CMetf6(52)
043600160614     C*
043700160614     C                   Z-ADD     130           LUNG
043800160614     C                   CLEAR                   COMMAN
043900160614     C                   MOVEA     CMetf6        COMMAN
044000160614     C                   CALL      'QCMDEXC'                            91
044100160614     C                   PARM                    COMMAN
044200160614     C                   PARM                    LUNG
044300160614     c                   endif
044400160614     c                   else
044500160614     c                   exsr      srfine
044600160614     c                   endif
044700160614      *
044800160614     c                   ENDSR
044900141028      *-------------------------------------------------------------------------
045000141028      * Ordinamento SFL
045100141028      *-------------------------------------------------------------------------
045200141028     C     sort          BEGSR
045300141028      *
045400141028     C                   CLEAR                   QLGSCB
045500141028     C                   CLEAR                   QLGSCB00
045600141028      *
045700141028      *
045800141028     C                   EVAL      QLGNBRK  =  1
045900141028      *
046000141028     ** prima x PDA è a posizione 1, numerico, 1 byte, descending.
046100141028     C                   EVAL      QLGSP      = 1
046200160615     C                   EVAL      QLGSS      = %SIZE(v3clna)
046300141028     C                   EVAL      QLGDT      = Numerico
046400141105     C                   EVAL      QLGSO      = discendente
046500141028      **
046600141028     C                   EVAL      QLGKL(1)   = QLGSKL
046700141028
046800141028      * Load other sort parameters.
046900141028     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
047000141028     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
047100141028     C                   EVAL      QLGRT     = 8
047200141028     C                   EVAL      QLGOKL    = 80
047300141028     C                   EVAL      QLGLKE    = 16
047400141028     C                   EVAL      QLGLSS    = 290
047500141028
047600141028      * Initialize Sort I/O API fields.
047700141028     C                   EVAL      QLGRL00  = QLGRL
047800141028     C                   EVAL      QLGRC00  = 1
047900141028     C                   CLEAR                   QUSEI
048000141028     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
048100141028
048200141028      * First step - Initialize the sort routine.
048300141028     C                   CALL      'QLGSORT'
048400141028     C                   PARM                    QLGSCB
048500141028     C                   PARM                    NotUsed
048600141028     C                   PARM                    NotUsed
048700141028     C                   PARM                    SizeList
048800141028     C                   PARM                    ReturnSize
048900141028     C                   PARM                    QUSEC
049000141028
049100141028      * Next step - Write records to I/O routine.
049200141028     C                   EVAL      QLGRT00 = Put
049300141028
049400141028     C     1             DO        nrrlast       x                 4 0
049500141028
049600160614     C     x             CHAIN     lg270s1
049700141028
049800141028     ** Solo le righe con Selected = 'Y' sono riordinate,
049900141028     ** quindi per fare un ordinamento di tutte le righe
050000141028     ** metto 'Y' sempre.
050100141028     C                   EVAL      Selected = 'Y'
050200141028
050300141028     C                   CLEAR                   QUSEI
050400141028     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
050500141028     C                   CALL      'QLGSRTIO'
050600141028     C                   PARM                    QLGSCB00
050700141028     C                   PARM                    SflRcd
050800141028     C                   PARM                    NotUsed
050900141028     C                   PARM                    SizeList
051000141028     C                   PARM                    NotUsed
051100141028     C                   PARM                    QUSEC
051200141028
051300141028     C                   ENDDO
051400141028
051500141028      * Next step - Signal end of input, clear subfile for reload.
051600141028     C                   EVAL      QLGRT00 = EndPut
051700141028     C                   CLEAR                   QUSEI
051800141028     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
051900141028
052000141028     C                   CALL      'QLGSRTIO'
052100141028     C                   PARM                    QLGSCB00
052200141028     C                   PARM                    SflRcd
052300141028     C                   PARM                    NotUsed
052400141028     C                   PARM                    SizeList
052500141028     C                   PARM                    NotUsed
052600141028     C                   PARM                    QUSEC
052700141028     C                   SETOFF                                       21
052800160614     C                   WRITE     lg270c1
052900141028     C                   SETON                                        21
053000141028      * Final step - Write the records back to the subfile.
053100141028     C                   EVAL      QLGRT00 = Get
053200141028
053300141028     C     1             DO        nrrlast       x
053400141028     C                   CLEAR                   QUSEI
053500141028     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
053600141028     C                   CALL      'QLGSRTIO'
053700141028     C                   PARM                    QLGSCB00
053800141028     C                   PARM                    NotUsed
053900141028     C                   PARM                    SflRcd
054000141028     C                   PARM                    QLGRL00
054100141028     C                   PARM                    NotUsed
054200141028     C                   PARM                    QUSEC
054300141028      *
054400141028     c                   z-add     x             nrr3
054500141028     c     v3hind        comp      ' '                                88
054600141028     c     v3hsce        comp      ' '                                44
054700160614     C                   WRITE     lg270s1
054800141028     C                   ENDDO
054900141028      *
055000141028     C                   ENDSR
055100160615      *-----------------------------------------------------------------------
055200160615**  cmA4f6
055300160615OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
055400160615USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
055500160615**  cmA5f6
055600160615OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
055700160615             USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
055800160615**  cmETf6
055900160615OVRPRTF FILE(FNLV22P)   OUTQ(xxxxxxxxxx) FORMTYPE('XXXXXXXXXX')
056000160615USRDTA('C.PO_DIR.') SECURE(*YES) SHARE(*YES)
