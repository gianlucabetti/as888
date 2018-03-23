000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200980721     F* fidn07r * -----------------------------------------*
000300980721     F* Ricerca Numero Evento
000400990118     F*-----------------                   ----------------*
000500990118     F* Se viene passata la fase Visualizzo solo gli eventi
000600990118     F* che hanno almeno una CA con fase minore a quella
000700990118     F* passata
000800990118     F*
000900990118     F* Vengono evidenziati gli Eventi che hanno tutte le
001000990118     F* CA in fase giusta
001100000000     F*----------------------------------------------------*
001200980721     Ffndet01l  iF   E           K DISK
001300981009     Ffndct03l  iF   E           K DISK
001400981012     Ffndct08l  iF   E           K DISK    rename(fndct000:fndct8)
001500980721     Ffndcs01l  iF   E           K DISK
001600140408     fFNDPT01L  if   e           k disk
001700140408     fTITAS30C  if   e           k disk    usropn
001800140408     fTABEL00F  if   e           k disk
001900140408     fTNTBE01L  if   e           k disk
002000980721     Fazorg01l  if   e           k disk
002100980721     Ffidn07d   CF   E             WORKSTN
002200980721     F                                     SFILE(dn07s01:nrr1)
002300140408
002400980722     D MSG             S             78    DIM(04) CTDATA PERRCD(1)             MSG ERRORI
002500140408     D sk_CCH          s                   like(DCTcch)  dim(99)
002600140408
002700000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
002800000000     D KPJBA         E DS
002900981110     D DTAD          E DS
003000140408     d DCCH          e ds                  inz
003100140408     d dsTB          e ds                  inz
003200981110     D dsbs02        E DS                  extname(tibs02ds)
003300980721     D fidn07        E DS                  extname(fidn07ds)
003400981005     D fidn06        E DS                  extname(fidn06ds)
003500981012     D fidn05        E DS                  extname(fidn05ds)
003600981012     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
003700140408      * Salvataggio dati letti in FNDCT03L o 08L
003800140408     d ds_FNDCT      e ds                  extname(FNDCT00F)  inz
003900140430     d*// ds_FNDCTx     e ds                  extname(FNDCT00F)  inz
004000140430     d*//                                     prefix(x)
004100140408      *
004200980717     D                SDS
004300980717     D  VTCPGM                 1     10
004400140408
004500140408       // -?Flag booleani?
004600140408     d $CAok_EvFo      s               n   inz
004700140408
004800140408       // -?Indici di schiera?
004900140408     d xx              s              3  0 inz
005000980721      *
005100980721     D Ktpd            S                   LIKE(DCStpd)
005200980721     D Knkt            S                   LIKE(DCSnkt)
005300980721     D Kstd            S                   LIKE(DCSstd)
005400980721     D Kdim            S                   LIKE(DCSdim)
005500980721     D Khim            S                   LIKE(DCShim)
005600980721     D Knks            S                   LIKE(DCSnks)
005700980721     D Ktrc            S                   LIKE(DCStrc)
005800980721     D Kpno            S                   LIKE(DCSpno)
005900980721      *
006000980722     d Wcdtd           s                   LIKE(V7Cdtd)
006100980722     d Wcdta           s                   LIKE(V7Cdta)
006200980722     d Wcpoc           s                   LIKE(V7Cpoc)
006300140408     d wIPV            s                   like(DCTipv)  inz
006400980722      *
006500980722     d  invdtd         s              8  0
006600980722     d  invdta         s              8  0
006700981012      *
006800140430     d wDate_DMY       s               d   datfmt(*DMY)    inz
006900981012     d wn14            S             14  0                                      *numerico 14
007000981012     d wn8             S              8  0                                      *numerico 8
007100981012     d datcor          S              8  0                                      *d.corrente a/m/g
007200981012      *
007300980721     d                 ds                  INZ
007400980721     d  detdat                 1      8  0
007500980721     d  detaae                 1      4  0
007600980721     d  detmge                 5      8  0
007700980721
007800980721     D DSNUMEV         DS                  INZ
007900980721     D  DSANEV                 1      4
008000980721     D  DSNUEV                 5     11
008100980721
008200980717     D WLBDAT          DS                  INZ
008300981012     D  G02DAT                 1      8  0
008400981012     D  G02INV                 9     16  0
008500981012     D  G02ERR                17     17
008600981012     D  G02TGI                18     22  0
008700140408
008800140408       //--------------------------------------------------------------
008900140408       //?Definizione aree dati.                                       ?
009000140408       //--------------------------------------------------------------
009100140408
009200140408       // -?Dati utente?
009300140408     d §AzUte        e ds                  extname(AZUTE00F)
009400140408     d                                     dtaara
009500140408     d §DatiUte      e ds                  extname(dDatiUte)
009600140408     d                                     dtaara
009700140408
009800140408       //--------------------------------------------------------------
009900140408       //?Definizione prototipi procedure.                             ?
010000140408       //--------------------------------------------------------------
010100140408
010200140408       // -?Reperimento dati utente?
010300140408     d TIBS34ds      e ds                  inz
010400140408      /copy gaitrasrc/srcProtoPR,TIBS34R
010500140408
010600971021     F*----------------------------------------------------*
010700971021      * INDICATORI
010800980722     c* 10/12- errori a video
010900140408      * 13   - trovata una C.A.
011000971021     c* 28   - emetto campo messaggi
011100980723     c* 30/32- comodo
011200140430      * 49   - Visualizza Importo Massimo Risarcibile
011300140430     c* 50/51- comodo sfl
011400971021     c* 70   - sfl control
011500971021     c* 71   - sfldsp
011600980722     c* 90   - Acceso quando devo riemettere il subfile
011700000000     C*----------------------------------------------------*
011800971016     C*
011900980522     c* RICERCA
012000981005     c                   if        i07mod<>' '
012100980722     c                   exsr      SUB_RICE1
012200980525     c* gestione
012300980525     c                   exsr      SUB_gesbf
012400980721     c* se il campo della ds O07NEV e' pulito vuol dire che non e' stato selezionato niente
012500980721     c                   if        o07nev = *zeros  and o07err = ' '
012600980721     c                   move      'E'           o07err
012700980721     c                   move      msg(1)        o07msg
012800980526     c                   endif
012900980526     c                   endif
013000980522     c*
013100980526     C                   eval      *inlr = *on                                       *
013200980522     C*-------------------------------------------------------------------------*
013300980722     c* SUB_RICE1 Routine di Ricerca
013400980522     C*-------------------------------------------------------------------------*
013500980722     c     SUB_RICE1     BEGSR
013600980522     C*
013700980722      * pulizia del subfile
013800140408     c                   setoff                                       717228
013900980722     c                   seton                                        70
014000980722     c                   write     dn07c01
014100980722     c                   setoff                                       70
014200140430      *
014300140430      * Impostazione indicatore per visualizzare "Importo Massimo Risarcibile"
014400140430     c                   eval      *in49 = (I07fca <> *zero)
014500980526     c*
014600980721     C                   Z-ADD     0             nrr1              4 0          *
014700971021     C                   Z-ADD     1             CURS2                          *
014800980721     C*
014900980525     C*-------- CARICA SUBFILE ----------------------------*
015000980601     C*
015100980721     C     *hival        setgt     fndet01l                                     *
015200980721     C                   readp     fndet01l                               30
015300000000     C*
015400980721    1C     *in30         doweq     *off
015500981012    2c                   if        detatb = ' ' and detnev > 0
015600980722     c* faccio i vari controlli in base alle selezioni inpostate a video
015700980722      *
015800980722      * se valorizzato le date controllo la data evento che sia compresa
015900980722      * nei limiti impostati a video
016000980722    3c                   if        invdtd > 0 and detdat >= invdtd
016100980722     c                             and detdat <= invdta
016200980722     c                             or invdtd = 0
016300980722      * se valorizzato P.O. controllo che sia uguale
016400980722    4C                   if        v7cpoc <> *blanks  and detfce = comlna
016500980722     c                             or comlna = *zeros
016600980722     c                   exsr      sub_wrtsf
016700980722    4c                   endif
016800980722    3c                   endif
016900980721    2c                   endif
017000971015     c**
017100980721     C                   readp     fndet01l                               30
017200980721    1C                   enddo                                                  *
017300980525     c*
017400140408      * Subfile-End
017500140408     c                   eval      *in72 = %eof(FNDET01L)
017600980722     C* nessun record caricato
017700980722    1c                   if        nrr1 = 0
017800980722     c                   eval      *in71 = *off
017900980722     c* errore
018000980722     c                   move      'E'           o07err
018100980722     c                   move      msg(1)        o07msg
018200980722     c
018300980722    xc                   else
018400980722     c                   eval      *in71 = *on
018500980722    1c                   endif
018600980722     c*
018700980525     c                   endsr
018800980525     C*-------------------------------------------------------------------------*
018900980525     c* SUB_GESBF Routine Gestione Subfile
019000980525     C*-------------------------------------------------------------------------*
019100980525     c     SUB_GESBF     BEGSR
019200980525     C*
019300000000     C*
019400980721?    C     t1tag         tag                                                    *
019500980526?   1C                   dow       *inkl = *off                                 *
019600981005     C                   write     dn07z01                                      *
019700971015     c* sfl controlo
019800980721     C                   exfmt     dn07c01                                      *
019900980722     c                   eval      *in90 = *off
020000980601     c* F12= fine lavoro
020100980526    2C                   if        *inkl = *off                                 *
020200980722      * controllo i campi selezione differente
020300980722     c                   exsr      sub_ctrctl
020400980722      * se e' stata richiesta  una nuova selezione oppure ci sono degli errori
020500980722      * riemetto la videata
020600990111    3c                   if        *in28 = *off  or *in90 = *on
020700971015     c*
020800971028     c* per 71 on non leggo perche' non ci sono record
020900980721     c   71              readc     dn07s01                                30
021000990111    4c                   dow       not *in30 and *in71
021100971016     c* imposto la key
021200981005     c                   SELECT
021300981005     C*
021400981005    5c                   WHEN      v7sopz = '1'
021500980721     c                   z-add     v7snev        O07nev
021600980723     c                   z-add     v7haae        O07aae
021700980721     c                   move      ' '           O07err
021800980721     c                   move      *blanks       O07msg
021900980526     c                   eval      *in30 = *on
022000980526     c                   eval      *inkl = *on
022100981005     c*
022200981005    5c                   WHEN      v7sopz = '5' and *in15=*on
022300981005     c                   z-add     v7snev        i06nev
022400981005     c                   z-add     v7haae        i06aae
022500981005     c                   move      'I'           I06MOD
022600981005     c                   movel     fidn06        kpjbu
022700981005     c                   call      'FIDN06R'
022800981005     c                   parm                    kpjba
022900981005     c                   movel     *blanks       v7sopz
023000981012     c                   if        v7hill <> ' '
023100981012     c                   seton                                        50
023200990118     c                   else
023300990118     c                   setoff                                       50
023400981012     c                   endif
023500981005     c                   update    dn07s01
023600981005     c*
023700981005    5c                   endsl
023800981005   x5c*
023900981005     c  n30              readc     dn07s01                                30
024000981005     c*
024100980722    4C                   enddo
024200980722    3c                   endif                                                  *kl
024300980722    2c                   endif                                                  *kl
024400980526    1C                   enddo                                                  *do  hival
024500971021     c**
024600971016     c                   endsr
024700980722     C*-------------------------------------------------------------------------*
024800980722     c* SUB_CTRCTL Routine di Controllo campi di input del control
024900980722     C*-------------------------------------------------------------------------*
025000980722     c     SUB_CTRCTL    BEGSR
025100980722     c*
025200980722     c                   setoff                                       101112
025300980722     c                   setoff                                       28
025400980722      * solo se e'stato modificato qualcosa ricarico il subfile
025500990111     c                   if        v7cdtd <> wcdtd  or
025600980722     c                             v7cdta <> wcdta  or
025700980722     c                             v7cpoc <> wcpoc
025800980722      *
025900990112     c                   clear                   wcdtd
026000990112     c                   clear                   wcdta
026100990112     c                   clear                   wcpoc
026200980722     c                   clear                   invdtd
026300980722     c                   clear                   invdta
026400980722     c                   clear                   comlna
026500980722     c                   clear                   v7cdpo
026600980722     c                   clear                   v7cmsg
026700980722      *
026800980722      * controllo la data da  se non è uguale a zero
026900980722    1c                   if        v7cdtd <> wcdtd and
027000980722     c                             v7cdtd  > *zeros
027100980722     C                   CLEAR                   WLBDAT
027200980722     C                   Z-ADD     v7cdtd        G02DAT
027300980722     C                   CALL      'XSRDA8'
027400980722     C                   PARM                    WLBDAT
027500980722    2C     G02ERR        IFEQ      '1'
027600980722     C                   MOVEL     MSG(2)        V7CMSG
027700980722     C                   SETON                                        1028
027800980722   X2C                   ELSE
027900980722     C                   MOVE      G02INV        INVdtd
028000980722     C                   MOVE      G02dat        v7cdtd
028100980722     C                   MOVE      G02dat        wcdtd
028200980722    2C                   END
028300980722    1C                   END
028400980722      * se la data finale è = 0 la valorizzo con quella iniziale
028500980722    1c                   if        v7cdtd > 0 and v7cdta = 0
028600980722     c                             and *in28 = *off
028700980722     c                   z-add     v7cdtd        v7cdta
028800980722     c                   z-add     invdtd        invdta
028900980722     c                   z-add     wcdtd         wcdta
029000980722    1c                   endif
029100980722      * controllo la data a  se non è uguale a zero
029200980722    1c                   if        v7cdta <>  wcdta  and
029300980722     c                             v7cdta  >  *zeros and
029400980722     c                             *in28 = *off
029500980722     C                   CLEAR                   WLBDAT
029600980722     C                   Z-ADD     v7cdta        G02DAT
029700980722     C                   CALL      'XSRDA8'
029800980722     C                   PARM                    WLBDAT
029900980722    2C     G02ERR        IFEQ      '1'
030000980722     C                   MOVEL     MSG(2)        V7CMSG
030100980722     C                   SETON                                        1128
030200980722   X2C                   ELSE
030300980722     C                   MOVE      G02INV        INVdta
030400980722     C                   MOVE      G02dat        v7cdta
030500980722     C                   MOVE      G02dat        wcdta
030600980722    2C                   END
030700980722    1C                   END
030800980722      * controllo se le date tra di loro sono corrette
030900980722    1c                   if        invdtd > invdta
031000980722     C                   seton                                        2810
031100980722     c                   movel     msg(4)        v7cmsg
031200980722     c                   endif
031300980722      * controllo il punto operativo che comunica l'evento
031400980722    1c                   if        v7cpoc <> wcpoc and  *in28 = *off and
031500980722     c                             v7cpoc <> *zeros and v7cpoc <> *blanks
031600980722     c
031700980722     C     '?'           SCAN      v7cpoc                                 30
031800980722      * Ricerca
031900980722    2C                   IF        *IN30 = *ON
032000980722     C                   MOVE      *BLANKS       §COD1
032100980722     C                   MOVE      *BLANKS       v7cpoc
032200980722     C                   CALL      'TNSD19R'
032300980722     C                   PARM                    §COD1             3
032400980722     C                   PARM                    §TIP              1
032500980722     C                   PARM                    §DES             25
032600980722     C                   MOVEL     §COD1         v7cpoc
032700980722     C                   MOVEL     §DES          V7cdpo
032800980722    2C                   ENDIF
032900980722     C*
033000980722     C                   testn                   V7cpoc               30
033100980722      * testo ultimo carattere
033200980722     C   30              move      V7cpoc        W001A             1
033300980722     C   30              eval      *in30 = (W001A >= '0')
033400980722      * Se codice non numerico errore
033500980722    2C                   IF        *IN30 = *OFF
033600980722     C                   seton                                        2812
033700980722     c                   movel     msg(3)        v7cmsg
033800980722   x2C                   ELSE
033900980722      * Controllo
034000980722     C*
034100980722    3C                   IF        v7cpoc <> *blanks
034200980722     C                   movel     v7cpoc        comlna            3 0
034300980722     C     comlna        chain     azorg01l                           30
034400980722    3C                   endif
034500980722     C*
034600980722     C* CONTROLLO SE ANNULLATO
034700980722     C* CONSIDERO SOLO I TIPI "F" E "A"
034800980722    3C  n30              if        orgfva <> *blank or
034900980722     C                             orgfag <> 'F'  and
035000980722     C                             orgfag <> 'A'
035100980722     C                   SETON                                        30
035200980722    3C                   ENDif
035300980722     C* DESCRIZIONE
035400980722     C  n30              MOVEL     ORGDES        V7cdpo
035500990111     C  n30              MOVEL     v7cpoc        wcpoc
035600980722    3C                   IF        *IN30 = *on
035700980722     C                   seton                                        2844
035800980722     c                   movel     msg(3)        v7cmsg
035900980722    3C                   Endif
036000980722    2C                   Endif
036100980722     c
036200980722    1c                   endif
036300980722     c*
036400980722     c  n28              exsr      SUB_RICE1
036500980722     c  n28              seton                                        90
036600980722     c                   endif
036700980722     c*
036800980722     c                   endsr
036900980722     C*-------------------------------------------------------------------------*
037000980722     c* SUB_WRTSF  Routine di Caricamento dati nel subfile
037100980722     C*-------------------------------------------------------------------------*
037200980722     c     SUB_WRTSF     BEGSR
037300140430      *
037400140430      * Decodifica Tipo Anomalia Danni
037500140430     c                   exsr      sr_DecodTAD
037600981012     c*
037700981012     c                   setoff                                       50
037800981012     c                   clear                   v7hill
037900140430     c*//                clear                   ds_FNDCTx
038000140408     c                   clear                   wIPV
038100981005      *
038200140408      * verifico se esiste almeno un ca aperta per quell'evento se passato un p.o.
038300140408      *                                                      o  se sono in sede
038400140408     c                   eval      *in13  = *off
038500981012    1c                   if        i07poi >0
038600981012     c*
038700981012if  2c                   if        i07poi = 046                                 *sede
038800140408     c     kdct8         setll     FNDCT8
038900140408     c     kdct8         reade     fndct08l                               13
039000990712     c                   dow       dctatb <> ' ' and not *in13
039100990712     c     kdct8         reade     fndct08l                               13
039200990712     c                   enddo
039300990712     c*
039400981012x   2c                   else
039500140408     c     kdct8         setll     FNDCT000
039600140408     c     kdct3         reade     fndct03l                               13
039700990712     c                   dow       dctatb <> ' ' and not *in13
039800990712     c     kdct3         reade     fndct03l                               13
039900990712     c  n13              enddo
040000981012e   2c                   endif
040100981012     c* verifico se ca aperta oppure già chiusa
040200990226    2c                   if        dctdch > 0 and not *in13
040300981012     c                   seton                                        13
040400981012    2c                   endif
040500981012     c*
040600981012    1c                   endif
040700140408      *
040800140408     c* se NON esiste almeno una  C.A. => esce
040900140408    1c                   if        *in13
041000140408     c                   leavesr
041100140408   -1c                   endif
041200140408      *
041300140430     c*//                eval      ds_FNDCTx = ds_FNDCT
041400140408      *
041500981009     c* se definita una fase CA nella DS (I07FCA) controllo in che stato sono le CA legate
041600990226     c                   if        i07fca <> 0  and dctfca < i07fca
041700140408      *
041800140408      * Controlla la fase della prima CA che trovo legata all'evento
041900981009     c                   exsr      verfca
042000140408      *
042100140408      * SOLO in Sede:
042200140408      *
042300140408do  2c                   DoW       I07poi = 046   and                           *sede
042400140408     c                             Not  %eof(FNDCT08L)
042500140408      *
042600140408      * - VERIFICA IMPORTO MASSIMO RISARCIBILE PER EVENTO FORTUITO
042700140408     c                   eval      $CAok_EvFo =  *off
042800140408if  3c                   if        §TADevfo   =  'S'
042900140408     c                   exsr      Ctr_IMRxEvFor
043000140408e   3c                   endif
043100140408      *
043200140408      * - Calcolo totale risarcibile
043300140408if  3c                   if            §TADevfo   = *blanks
043400140408     c                             or  §TADevfo   = 'S'
043500140408     c                             and $CAok_EvFo = *on
043600140408     c                   eval      wIPV += DCTipv
043700140408e   3c                   endif
043800140408      *
043900140408      * - Ricerca eventuale CA in fase 400   &
044000140408      * - Calcolo totale risarcibile
044100140408     c     kDCT8         reade     FNDCT8
044200140408      *
044300140430do  3c                   doW       Not  %eof(FNDCT08L)
044400140505     c                             and  DCTatb <> *blank
044500140408     c     kDCT8         reade     FNDCT8
044600140408e   3c                   endDo
044700140408      *
044800140408if  3c                   if        Not  %eof(FNDCT08L)
044900140505     c                             and  V7Hill =  *blank
045000140505     c                             and  DCTgfc =  I07poi
045100140505     c                             and  DCTfpr =  'P'
045200140408     c                   exsr      VerFCA
045300140408e   3c                   endif
045400140408      *
045500140408e   2c                   EndDo
045600140408      *
045700990226     c* scrivo il record
045800990226     c                   exsr      sub_wrt
045900140408      *
046000990226     c                   endif
046100140408      *
046200990226     c* se non è stata definita nessuna fase CA
046300990226     c                   if        i07fca  = 0
046400990226     c                   exsr      sub_wrt
046500990226     c                   endif
046600980722      *
046700980722     c                   endsr
046800140408      *
046900140408      *-------------------------------------------------------------------------*
047000140408      * Decodifica Tipo Anomalia Danni
047100140408      *-------------------------------------------------------------------------*
047200140408     c     sr_DecodTAD   BEGSR
047300140408      *
047400140408     c                   clear                   dTAD
047500140408     c                   clear                   dsBS02
047600140408      *
047700140408      * reperimento anomalia e sua descrizione
047800140408     c                   movel     'C'           t02mod
047900140408     c                   movel     knsif         t02sif
048000140408     C                   MOVEL(P)  dettad        t02ke1
048100140408     C                   movel     'TAD'         t02cod
048200140408      *
048300140408     C                   CALL      'TIBS02R'
048400140408     c                   parm                    KPJBA
048500140408     c                   parm                    dsBS02
048600140408      *
048700140408     c                   if        t02err =  *blanks
048800140408     c                   movel     t02uni        dTAD
048900140408     c                   endif
049000140408      *
049100140408     c                   ENDSR
049200140408      *
049300140408      *****************************************************************
049400140408      * CALCOLO IMPORTO MASSIMO RISARCIBILE X C.A. DI EVENTO FORTUITO *
049500140408      *****************************************************************
049600140408     c     Ctr_IMRxEvFor BEGSR
049700140408      *
049800140408      *  Verifica se Mandato Assicurativo
049900140408     c     K03DPT01      chain     FNDPT000
050000140408      *
050100140408if  1c                   IF            %found(FNDPT01L)
050200140408     c                             and DPTatb =  *blanks
050300140408     c                   eval      $CAok_EvFo =  *on
050400140408     c                   leavesr
050500140408e   1c                   ENDIF
050600140408      *
050700140408      *  Verifica se Importo da Assicurare in bolla
050800140408     c                   eval      *in30      =  *off
050900140408     c     K04TAS30      chain     TITAS30C                           30
051000140408      *    (?impossibile?che NON ci sia... )
051100140408      *  - se tipo bolla non è valido cerco altro rcd
051200140408     c                   eval      TBLkut = 1
051300140408     c                   eval      TBLcod = 'TB'
051400140408     c                   movel(p)  TAStbl        TBLkey
051500140424     c     K03TABEL      chain     TABEL
051600140408if  2c                   if        %found(TABEL00F)
051700140408     c                   movel     TBLuni        dsTB
051800140408if  3c                   if        §TBrbl = 'R'
051900140408     c     K04TAS30      reade     TITAS30C                               30
052000140408e   3c                   endif
052100140408e   2c                   endif
052200140408      *
052300140408if  2c                   IF        NOT *in30
052400140408     c                             and TASias >  *zeros
052500140408     c                             and TASfcm =  *blanks
052600140408     c                   eval      $CAok_EvFo =  *on
052700140408e   2c                   ENDIF
052800140408      *
052900140408     c                   ENDSR
053000140408      *
053100990226     C*-------------------------------------------------------------------------*
053200990226     c* scrittura del record nel subfile
053300990226     C*-------------------------------------------------------------------------*
053400990226     c     SUB_WRT       BEGSR
053500990226     c*
053600990226     C                   ADD       1             nrr1                           *
053700990226     c                   clear                   v7sopz
053800990226     c                   clear                   v7snev
053900990226     c                   clear                   v7haae
054000990226     c                   clear                   v7sdev
054100990226     c                   clear                   v7spos
054200990226     c                   clear                   v7stad
054300140424     c                   clear                   V7Simr
054400990226     c                   clear                   v7sdes
054500990226     C                   z-add     detnev        v7snev                         *
054600990226     C                   z-add     detaae        v7haae                         *
054700990226      *
054800990226     C                   clear                   wlbdat
054900990226     C                   z-add     detdat        G02inv
055000990226     C                   movel     '3'           G02err
055100990226     C                   call      'XSRDA8'
055200990226     C                   parm                    wlbdat
055300140430     c*//                movel     g02dat        v7sdev
055400140430     c                   eval      wDate_DMY = %date( G02dat : *EUR )
055500140430     c                   eval      V7Sdev    = %dec( wDate_DMY )
055600990226      *
055700990226      * Decodifica P.O. di comunicazione Evento
055800990226     C     detfce        chain     azorg01l                           30
055900990226     c                   if        orgde5 <> *blanks and *in30 = *off
056000990226     C                   movel     orgde5        V7spos
056100990226     c                   else
056200990226     C                   movel     orgdes        V7spos
056300990226     c                   endif
056400140408      * Descrizione anomalia
056500990226     C                   movel     §TADdesc      v7stad
056600990226      * Recupero la descrizione Evento
056700990226     c                   movel     'E'           ktpd
056800990226     c                   movel     'D'           ktrc
056900990226     C                   move      detaae        dsanev
057000990226     C                   move      detnev        dsnuev
057100990226     c                   movel     dsnumev       knkt
057200990226     c                   z-add     1             kpno
057300990226     c     kdcs          chain     fndcs01l                           30
057400990226     c  n30              movel     dcsnot        v7sdes
057500990226     c                   z-add     2             kpno
057600990226     c     kdcs          chain     fndcs01l                           30
057700140424     c  n30              eval      %subst( V7Sdes : %len(DCSnot) + 1 )
057800140424     c                                   = DCSnot
057900990226     c                   z-add     3             kpno
058000990226     c     kdcs          chain     fndcs01l                           30
058100140424     c  n30              eval      %subst( V7Sdes : (%len(DCSnot) * 2) + 1 )
058200140424     c                                   = DCSnot
058300140408      *
058400140408      * Già recuperato il Totale Massimo Risarcibile
058500140424     c                   if        wIPV > 999999999.99
058600140424     c                   z-add     *hival        V7Simr
058700140424     c                   else
058800140424     c                   z-add     wIPV          V7Simr
058900140424     c                   endif
059000990226     c*
059100990226     C                   write     dn07s01                                      *
059200990226     c*
059300990226      *
059400990226     c                   endsr
059500981009     C*-------------------------------------------------------------------------*
059600981009     c* verifico lo stato delle CA legate al singolo evento
059700981009     C*-------------------------------------------------------------------------*
059800981009     c     verfca        BEGSR
059900981009     c*
060000981012     c* calcolo la fase successiva
060100981012     c                   CLEAR                   FIDN05
060200981012     c                   movel     'F'           I05MOD
060300981012     c                   z-add     DCTfca        I05fca
060400981012     c                   movel     DCTfpr        I05fpr
060500981012     c                   movel     DCTptr        I05tpc
060600981012     c                   movel     'E'           I05fpe
060700981012     c                   movel     'O'           I05ffs
060800981012     c                   z-add     Datcor        i05dta
060900981012     c                   move      DCTtad        i05tad
061000981030     c                   IF        DCTnde > *zeros
061100981030     c                   movel     'D'           I05fde
061200981102e   2c                   ELSE
061300981102     c                   movel     'N'           I05fde
061400981102e   2c                   ENDIF
061500981125      * valorizzo testata c.a.
061600981125     c                   z-add     dctaac        i05aac
061700981125     c                   z-add     dctfil        i05fil
061800981125     c                   z-add     dctnca        i05nca
061900981012
062000981012     C                   CALL      'FIDN05R'
062100981012     c                   PARM                    KPJBA
062200981012     c                   PARM                    FIDN05
062300981012      *
062400981012if  2c                   IF        O05err = *blanks
062500981012     c                   movel     O05REC        DS_FNDFA
062600981012x   2c                   else
062700981012     c                   movel     '000'         dfafca
062800981012e   2c                   ENDIF
062900981012     c*  se fase calcolata uguale a quella passata accendo indicatore di reverse immage
063000981012if  2c                   if        dfafca   =  i07fca
063100981012     c                   seton                                        5051
063200981012     c                   eval      v7hill = 'Y'
063300140430     c*//                eval      ds_FNDCTx = ds_FNDCT
063400981012e   2c                   endif
063500981012     c*
063600981012     c                   endsr
063700981009      *
063800980721     C*-------------------------------------------------------------------------*
063900980721     c     *inzsr        BEGSR
064000981012     C*-------------------------------------------------------------------------*
064100980721      *
064200980721     C     *ENTRY        PLIST                                                  *
064300980721     C                   PARM                    KPJBA                          *
064400980721     c                   parm                    fidn07
064500140408      *
064600140408      * -?Reperimento dati job?
064700140408     c                   exsr      sr_DatiJob
064800140408      /free
064900140408         // -?Apertura TITAS30C se si è in SEDE?
065000140408         //if  I07poi = 046;
065100140408         if  %subst( KNSIF : 1 : 3 ) = 'GAI';
065200140408           open  TITAS30C;
065300140408         endif;
065400140408      /end-free
065500981012     c* calcola la data corrente
065600981012     c                   TIME                    wn14                           *ora(6) + data(8)
065700981012     c                   MOVE      wn14          wn8                            *data(8) in g/m/aa
065800981012     c                   Z-ADD     wn8           g02dat
065900981012     c                   Z-ADD     *zeros        g02inv
066000981012     c                   MOVEL     '0'           g02err
066100981012     c                   CALL      'XSRDA8'
066200981012     c                   PARM                    wlbdat
066300981012     c                   Z-ADD     g02inv        datcor                         *d.corrente aa/m/g
066400981030      *
066500980723      * controllo se e' valorizzato il P.O. interessato agli aventi in ricerca
066600980723      *
066700981005     c                   if        i07poi > 0   and
066800981030     c                             i07poi <> 46
066900980723      * decodifica
067000980723     c     i07poi        chain     azorg01l                           30
067100981012     c* n30              movel     orgdes        v7tdpo
067200980723      *
067300981012     c*                  z-add     i07poi        v7tcpo
067400980723     c                   endif
067500981005     c* se il campo I07MOD = I do la possibilità di ricercare il numero evento ma anche di interro-
067600981005     c* garne il dettaglio mentre con R faccio solo la visualizzazione
067700981005     c                   if        i07mod = 'I'
067800981005     c                   seton                                        15
067900981005     c                   endif
068000981030      * se passata la fase decodifico
068100981012     c                   if        i07fca > 0
068200981030      *
068300981012     c                   CLEAR                   FIDN05
068400981012     c                   movel     'C'           I05MOD
068500981012     c                   z-add     i07fca        I05fca
068600981012
068700981012     C                   CALL      'FIDN05R'
068800981012     c                   PARM                    KPJBA
068900981012     c                   PARM                    FIDN05
069000981012      *
069100981012     c                   IF        O05err = *blanks
069200981012     c                   movel     O05REC        DS_FNDFA
069300981012     c                   ENDIF
069400981012     c                   movel     dfadgc        v7dfca
069500981012     c                   endif
069600140408      *
069700140408      * - Memorizzazione tab. CCH in schiera per le sole causali di chiusure tecniche
069800140408     c                   clear                   XX
069900140408     c                   clear                   sk_CCH
070000140408     c                   eval      TBEcod  = 'CCH'
070100140408     c     TBEcod        setll     TNTBE000
070200140408do  1c                   DO        *hival
070300140408     c     TBEcod        reade     TNTBE000
070400140408      * - e.o.f.
070500140408if  2c                   if        %eof(TNTBE01L)
070600140408     c                   leave
070700140408e   2c                   endif
070800140408      * - rec. annullato
070900140408if  2c                   if            TBEatb <> *blank
071000140408     c                   iter
071100140408e   2c                   endif
071200140408      * - rec. da non memorizzare
071300140408     c                   movel     TBEuni        DCCH
071400140408if  2c                   if        §CCHchte <> 'S'
071500140408     c                   iter
071600140408e   2c                   endif
071700140408      * - memorizzazione causale
071800140408     c                   add       1             XX
071900140408     c                   movel     TBEke1        sk_CCH(xx)
072000140408e   1c                   ENDDO
072100980721      *
072200980721     c     kdcs          klist
072300980721     c                   kfld                    ktpd
072400980721     c                   kfld                    knkt
072500980721     c                   kfld                    kstd
072600980721     c                   kfld                    kdim
072700980721     c                   kfld                    khim
072800980721     c                   kfld                    knks
072900980721     c                   kfld                    ktrc
073000980721     c                   kfld                    kpno
073100980721     c**
073200980723     c* chiave file dettaglio  eventi
073300980723     c     KDED          klist
073400980723     C                   kfld                    detaae
073500980723     C                   kfld                    detnev
073600980723     c                   kfld                    i07poi
073700980723      **
073800980723     c     KDED1         klist
073900980723     C                   kfld                    detaae
074000980723     C                   kfld                    detnev
074100981009      **
074200981012     c     Kdct3         klist
074300981009     C                   kfld                    detaae
074400981009     C                   kfld                    detnev
074500981009     c                   kfld                    i07poi
074600981009      **
074700981009     c     Kdct8         klist
074800981009     C                   kfld                    detaae
074900981009     C                   kfld                    detnev
075000140408      * chiave file FNDPT01L
075100140408     c     K03DPT01      klist
075200140408     c                   kfld                    DCTaac
075300140408     c                   kfld                    DCTfil
075400140408     c                   kfld                    DCTnca
075500140408      * chiave file TITAS30C
075600140408     c     K04TAS30      klist
075700140408     c                   kfld                    DCTaas
075800140408     c                   kfld                    DCTlnp
075900140408     c                   kfld                    DCTnrs
076000140408     c                   kfld                    DCTnsp
076100140408      * chiave file tabelle
076200140408     c     K03TABEL      klist
076300140408     c                   kfld                    TBLkut
076400140408     c                   kfld                    TBLcod
076500140408     c                   kfld                    TBLkey
076600980723      *
076700980723     c                   clear                   o07aae
076800980723     c                   clear                   o07nev
076900980723     c                   clear                   o07err
077000980723     c                   clear                   o07msg
077100980723      *
077200980721     c                   endsr
077300140408      /free
077400140408       //--------------------------------------------------------------
077500140408       //?Reperimento Dati del job (Utente/Operativi).                 ?
077600140408       //--------------------------------------------------------------
077700140408       BEGSR  sr_DatiJob;
077800140408
077900140408         in(e) §AzUte;
078000140408         if NOT %error;
078100140408           in(e) §DatiUte;
078200140408         endif;
078300140408         if %error or RSut = *blank;
078400140408           tibs34r ( tibs34ds );
078500140408           in §AzUte;
078600140408           in §DatiUte;
078700140408         endif;
078800140408
078900140408       ENDSR;
079000140408      /end-free
079100980526**
079200980526Non e' stato selezionato nulla
079300980722Data errata
079400980722Punto operativo errato
079500980722Intervallo di date errato
