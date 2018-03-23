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
004700151026     d $CCH_Tecn       s               n   inz
004800140408
004900140408       // -?Indici di schiera?
005000140408     d xx              s              3  0 inz
005100980721      *
005200980721     D Ktpd            S                   LIKE(DCStpd)
005300980721     D Knkt            S                   LIKE(DCSnkt)
005400980721     D Kstd            S                   LIKE(DCSstd)
005500980721     D Kdim            S                   LIKE(DCSdim)
005600980721     D Khim            S                   LIKE(DCShim)
005700980721     D Knks            S                   LIKE(DCSnks)
005800980721     D Ktrc            S                   LIKE(DCStrc)
005900980721     D Kpno            S                   LIKE(DCSpno)
006000980721      *
006100980722     d Wcdtd           s                   LIKE(V7Cdtd)
006200980722     d Wcdta           s                   LIKE(V7Cdta)
006300980722     d Wcpoc           s                   LIKE(V7Cpoc)
006400140408     d wIPV            s                   like(DCTipv)  inz
006500980722      *
006600980722     d  invdtd         s              8  0
006700980722     d  invdta         s              8  0
006800981012      *
006900140430     d wDate_DMY       s               d   datfmt(*DMY)    inz
007000981012     d wn14            S             14  0                                      *numerico 14
007100981012     d wn8             S              8  0                                      *numerico 8
007200981012     d datcor          S              8  0                                      *d.corrente a/m/g
007300981012      *
007400980721     d                 ds                  INZ
007500980721     d  detdat                 1      8  0
007600980721     d  detaae                 1      4  0
007700980721     d  detmge                 5      8  0
007800980721
007900980721     D DSNUMEV         DS                  INZ
008000980721     D  DSANEV                 1      4
008100980721     D  DSNUEV                 5     11
008200980721
008300980717     D WLBDAT          DS                  INZ
008400981012     D  G02DAT                 1      8  0
008500981012     D  G02INV                 9     16  0
008600981012     D  G02ERR                17     17
008700981012     D  G02TGI                18     22  0
008800140408
008900140408       //--------------------------------------------------------------
009000140408       //?Definizione aree dati.                                       ?
009100140408       //--------------------------------------------------------------
009200140408
009300140408       // -?Dati utente?
009400140408     d §AzUte        e ds                  extname(AZUTE00F)
009500140408     d                                     dtaara
009600140408     d §DatiUte      e ds                  extname(dDatiUte)
009700140408     d                                     dtaara
009800140408
009900140408       //--------------------------------------------------------------
010000140408       //?Definizione prototipi procedure.                             ?
010100140408       //--------------------------------------------------------------
010200140408
010300140408       // -?Reperimento dati utente?
010400140408     d TIBS34ds      e ds                  inz
010500140408      /copy gaitrasrc/srcProtoPR,TIBS34R
010600140408
010700971021     F*----------------------------------------------------*
010800971021      * INDICATORI
010900980722     c* 10/12- errori a video
011000140408      * 13   - trovata una C.A.
011100971021     c* 28   - emetto campo messaggi
011200980723     c* 30/32- comodo
011300140430      * 49   - Visualizza Importo Massimo Risarcibile
011400140430     c* 50/51- comodo sfl
011500971021     c* 70   - sfl control
011600971021     c* 71   - sfldsp
011700980722     c* 90   - Acceso quando devo riemettere il subfile
011800000000     C*----------------------------------------------------*
011900971016     C*
012000980522     c* RICERCA
012100981005     c                   if        i07mod<>' '
012200980722     c                   exsr      SUB_RICE1
012300980525     c* gestione
012400980525     c                   exsr      SUB_gesbf
012500980721     c* se il campo della ds O07NEV e' pulito vuol dire che non e' stato selezionato niente
012600980721     c                   if        o07nev = *zeros  and o07err = ' '
012700980721     c                   move      'E'           o07err
012800980721     c                   move      msg(1)        o07msg
012900980526     c                   endif
013000980526     c                   endif
013100980522     c*
013200980526     C                   eval      *inlr = *on                                       *
013300980522     C*-------------------------------------------------------------------------*
013400980722     c* SUB_RICE1 Routine di Ricerca
013500980522     C*-------------------------------------------------------------------------*
013600980722     c     SUB_RICE1     BEGSR
013700980522     C*
013800980722      * pulizia del subfile
013900140408     c                   setoff                                       717228
014000980722     c                   seton                                        70
014100980722     c                   write     dn07c01
014200980722     c                   setoff                                       70
014300140430      *
014400140430      * Impostazione indicatore per visualizzare "Importo Massimo Risarcibile"
014500140430     c                   eval      *in49 = (I07fca <> *zero)
014600980526     c*
014700980721     C                   Z-ADD     0             nrr1              4 0          *
014800971021     C                   Z-ADD     1             CURS2                          *
014900980721     C*
015000980525     C*-------- CARICA SUBFILE ----------------------------*
015100980601     C*
015200980721     C     *hival        setgt     fndet01l                                     *
015300980721     C                   readp     fndet01l                               30
015400000000     C*
015500980721    1C     *in30         doweq     *off
015600981012    2c                   if        detatb = ' ' and detnev > 0
015700980722     c* faccio i vari controlli in base alle selezioni inpostate a video
015800980722      *
015900980722      * se valorizzato le date controllo la data evento che sia compresa
016000980722      * nei limiti impostati a video
016100980722    3c                   if        invdtd > 0 and detdat >= invdtd
016200980722     c                             and detdat <= invdta
016300980722     c                             or invdtd = 0
016400980722      * se valorizzato P.O. controllo che sia uguale
016500980722    4C                   if        v7cpoc <> *blanks  and detfce = comlna
016600980722     c                             or comlna = *zeros
016700980722     c                   exsr      sub_wrtsf
016800980722    4c                   endif
016900980722    3c                   endif
017000980721    2c                   endif
017100971015     c**
017200980721     C                   readp     fndet01l                               30
017300980721    1C                   enddo                                                  *
017400980525     c*
017500140408      * Subfile-End
017600140408     c                   eval      *in72 = %eof(FNDET01L)
017700980722     C* nessun record caricato
017800980722    1c                   if        nrr1 = 0
017900980722     c                   eval      *in71 = *off
018000980722     c* errore
018100980722     c                   move      'E'           o07err
018200980722     c                   move      msg(1)        o07msg
018300980722     c
018400980722    xc                   else
018500980722     c                   eval      *in71 = *on
018600980722    1c                   endif
018700980722     c*
018800980525     c                   endsr
018900980525     C*-------------------------------------------------------------------------*
019000980525     c* SUB_GESBF Routine Gestione Subfile
019100980525     C*-------------------------------------------------------------------------*
019200980525     c     SUB_GESBF     BEGSR
019300980525     C*
019400000000     C*
019500980721?    C     t1tag         tag                                                    *
019600980526?   1C                   dow       *inkl = *off                                 *
019700981005     C                   write     dn07z01                                      *
019800971015     c* sfl controlo
019900980721     C                   exfmt     dn07c01                                      *
020000980722     c                   eval      *in90 = *off
020100980601     c* F12= fine lavoro
020200980526    2C                   if        *inkl = *off                                 *
020300980722      * controllo i campi selezione differente
020400980722     c                   exsr      sub_ctrctl
020500980722      * se e' stata richiesta  una nuova selezione oppure ci sono degli errori
020600980722      * riemetto la videata
020700990111    3c                   if        *in28 = *off  or *in90 = *on
020800971015     c*
020900971028     c* per 71 on non leggo perche' non ci sono record
021000980721     c   71              readc     dn07s01                                30
021100990111    4c                   dow       not *in30 and *in71
021200971016     c* imposto la key
021300981005     c                   SELECT
021400981005     C*
021500981005    5c                   WHEN      v7sopz = '1'
021600980721     c                   z-add     v7snev        O07nev
021700980723     c                   z-add     v7haae        O07aae
021800980721     c                   move      ' '           O07err
021900980721     c                   move      *blanks       O07msg
022000980526     c                   eval      *in30 = *on
022100980526     c                   eval      *inkl = *on
022200981005     c*
022300981005    5c                   WHEN      v7sopz = '5' and *in15=*on
022400981005     c                   z-add     v7snev        i06nev
022500981005     c                   z-add     v7haae        i06aae
022600981005     c                   move      'I'           I06MOD
022700981005     c                   movel     fidn06        kpjbu
022800981005     c                   call      'FIDN06R'
022900981005     c                   parm                    kpjba
023000981005     c                   movel     *blanks       v7sopz
023100981012     c                   if        v7hill <> ' '
023200981012     c                   seton                                        50
023300990118     c                   else
023400990118     c                   setoff                                       50
023500981012     c                   endif
023600981005     c                   update    dn07s01
023700981005     c*
023800981005    5c                   endsl
023900981005   x5c*
024000981005     c  n30              readc     dn07s01                                30
024100981005     c*
024200980722    4C                   enddo
024300980722    3c                   endif                                                  *kl
024400980722    2c                   endif                                                  *kl
024500980526    1C                   enddo                                                  *do  hival
024600971021     c**
024700971016     c                   endsr
024800980722     C*-------------------------------------------------------------------------*
024900980722     c* SUB_CTRCTL Routine di Controllo campi di input del control
025000980722     C*-------------------------------------------------------------------------*
025100980722     c     SUB_CTRCTL    BEGSR
025200980722     c*
025300980722     c                   setoff                                       101112
025400980722     c                   setoff                                       28
025500980722      * solo se e'stato modificato qualcosa ricarico il subfile
025600990111     c                   if        v7cdtd <> wcdtd  or
025700980722     c                             v7cdta <> wcdta  or
025800980722     c                             v7cpoc <> wcpoc
025900980722      *
026000990112     c                   clear                   wcdtd
026100990112     c                   clear                   wcdta
026200990112     c                   clear                   wcpoc
026300980722     c                   clear                   invdtd
026400980722     c                   clear                   invdta
026500980722     c                   clear                   comlna
026600980722     c                   clear                   v7cdpo
026700980722     c                   clear                   v7cmsg
026800980722      *
026900980722      * controllo la data da  se non è uguale a zero
027000980722    1c                   if        v7cdtd <> wcdtd and
027100980722     c                             v7cdtd  > *zeros
027200980722     C                   CLEAR                   WLBDAT
027300980722     C                   Z-ADD     v7cdtd        G02DAT
027400980722     C                   CALL      'XSRDA8'
027500980722     C                   PARM                    WLBDAT
027600980722    2C     G02ERR        IFEQ      '1'
027700980722     C                   MOVEL     MSG(2)        V7CMSG
027800980722     C                   SETON                                        1028
027900980722   X2C                   ELSE
028000980722     C                   MOVE      G02INV        INVdtd
028100980722     C                   MOVE      G02dat        v7cdtd
028200980722     C                   MOVE      G02dat        wcdtd
028300980722    2C                   END
028400980722    1C                   END
028500980722      * se la data finale è = 0 la valorizzo con quella iniziale
028600980722    1c                   if        v7cdtd > 0 and v7cdta = 0
028700980722     c                             and *in28 = *off
028800980722     c                   z-add     v7cdtd        v7cdta
028900980722     c                   z-add     invdtd        invdta
029000980722     c                   z-add     wcdtd         wcdta
029100980722    1c                   endif
029200980722      * controllo la data a  se non è uguale a zero
029300980722    1c                   if        v7cdta <>  wcdta  and
029400980722     c                             v7cdta  >  *zeros and
029500980722     c                             *in28 = *off
029600980722     C                   CLEAR                   WLBDAT
029700980722     C                   Z-ADD     v7cdta        G02DAT
029800980722     C                   CALL      'XSRDA8'
029900980722     C                   PARM                    WLBDAT
030000980722    2C     G02ERR        IFEQ      '1'
030100980722     C                   MOVEL     MSG(2)        V7CMSG
030200980722     C                   SETON                                        1128
030300980722   X2C                   ELSE
030400980722     C                   MOVE      G02INV        INVdta
030500980722     C                   MOVE      G02dat        v7cdta
030600980722     C                   MOVE      G02dat        wcdta
030700980722    2C                   END
030800980722    1C                   END
030900980722      * controllo se le date tra di loro sono corrette
031000980722    1c                   if        invdtd > invdta
031100980722     C                   seton                                        2810
031200980722     c                   movel     msg(4)        v7cmsg
031300980722     c                   endif
031400980722      * controllo il punto operativo che comunica l'evento
031500980722    1c                   if        v7cpoc <> wcpoc and  *in28 = *off and
031600980722     c                             v7cpoc <> *zeros and v7cpoc <> *blanks
031700980722     c
031800980722     C     '?'           SCAN      v7cpoc                                 30
031900980722      * Ricerca
032000980722    2C                   IF        *IN30 = *ON
032100980722     C                   MOVE      *BLANKS       §COD1
032200980722     C                   MOVE      *BLANKS       v7cpoc
032300980722     C                   CALL      'TNSD19R'
032400980722     C                   PARM                    §COD1             3
032500980722     C                   PARM                    §TIP              1
032600980722     C                   PARM                    §DES             25
032700980722     C                   MOVEL     §COD1         v7cpoc
032800980722     C                   MOVEL     §DES          V7cdpo
032900980722    2C                   ENDIF
033000980722     C*
033100980722     C                   testn                   V7cpoc               30
033200980722      * testo ultimo carattere
033300980722     C   30              move      V7cpoc        W001A             1
033400980722     C   30              eval      *in30 = (W001A >= '0')
033500980722      * Se codice non numerico errore
033600980722    2C                   IF        *IN30 = *OFF
033700980722     C                   seton                                        2812
033800980722     c                   movel     msg(3)        v7cmsg
033900980722   x2C                   ELSE
034000980722      * Controllo
034100980722     C*
034200980722    3C                   IF        v7cpoc <> *blanks
034300980722     C                   movel     v7cpoc        comlna            3 0
034400980722     C     comlna        chain     azorg01l                           30
034500980722    3C                   endif
034600980722     C*
034700980722     C* CONTROLLO SE ANNULLATO
034800980722     C* CONSIDERO SOLO I TIPI "F" E "A"
034900980722    3C  n30              if        orgfva <> *blank or
035000980722     C                             orgfag <> 'F'  and
035100980722     C                             orgfag <> 'A'
035200980722     C                   SETON                                        30
035300980722    3C                   ENDif
035400980722     C* DESCRIZIONE
035500980722     C  n30              MOVEL     ORGDES        V7cdpo
035600990111     C  n30              MOVEL     v7cpoc        wcpoc
035700980722    3C                   IF        *IN30 = *on
035800980722     C                   seton                                        2844
035900980722     c                   movel     msg(3)        v7cmsg
036000980722    3C                   Endif
036100980722    2C                   Endif
036200980722     c
036300980722    1c                   endif
036400980722     c*
036500980722     c  n28              exsr      SUB_RICE1
036600980722     c  n28              seton                                        90
036700980722     c                   endif
036800980722     c*
036900980722     c                   endsr
037000980722     C*-------------------------------------------------------------------------*
037100980722     c* SUB_WRTSF  Routine di Caricamento dati nel subfile
037200980722     C*-------------------------------------------------------------------------*
037300980722     c     SUB_WRTSF     BEGSR
037400140430      *
037500140430      * Decodifica Tipo Anomalia Danni
037600140430     c                   exsr      sr_DecodTAD
037700981012     c*
037800981012     c                   setoff                                       50
037900981012     c                   clear                   v7hill
038000140430     c*//                clear                   ds_FNDCTx
038100140408     c                   clear                   wIPV
038200981005      *
038300140408      * verifico se esiste almeno un ca aperta per quell'evento se passato un p.o.
038400140408      *                                                      o  se sono in sede
038500140408     c                   eval      *in13  = *off
038600981012    1c                   if        i07poi >0
038700981012     c*
038800981012if  2c                   if        i07poi = 046                                 *sede
038900140408     c     kdct8         setll     FNDCT8
039000140408     c     kdct8         reade     fndct08l                               13
039100990712     c                   dow       dctatb <> ' ' and not *in13
039200990712     c     kdct8         reade     fndct08l                               13
039300990712     c                   enddo
039400990712     c*
039500981012x   2c                   else
039600140408     c     kdct8         setll     FNDCT000
039700140408     c     kdct3         reade     fndct03l                               13
039800990712     c                   dow       dctatb <> ' ' and not *in13
039900990712     c     kdct3         reade     fndct03l                               13
040000990712     c  n13              enddo
040100981012e   2c                   endif
040200981012     c* verifico se ca aperta oppure già chiusa
040300990226    2c                   if        dctdch > 0 and not *in13
040400981012     c                   seton                                        13
040500981012    2c                   endif
040600981012     c*
040700981012    1c                   endif
040800140408      *
040900140408     c* se NON esiste almeno una  C.A. => esce
041000140408    1c                   if        *in13
041100140408     c                   leavesr
041200140408   -1c                   endif
041300140408      *
041400140430     c*//                eval      ds_FNDCTx = ds_FNDCT
041500140408      *
041600981009     c* se definita una fase CA nella DS (I07FCA) controllo in che stato sono le CA legate
041700990226     c                   if        i07fca <> 0  and dctfca < i07fca
041800140408      *
041900140408      * Controlla la fase della prima CA che trovo legata all'evento
042000981009     c                   exsr      verfca
042100140408      *
042200140408      * SOLO in Sede:
042300140408      *
042400140408do  2c                   DoW       I07poi = 046   and                           *sede
042500140408     c                             Not  %eof(FNDCT08L)
042600151026      *
042700151026      * - VERIFICA SE CAUSALE CHIUSURA TECNICA
042800151026     c                   eval      $CCH_Tecn  = ( DCTcch <> *blank  and
042900151026     c                                   %lookup( DCTcch : sk_CCH ) > *zero )
043000140408      *
043100140408      * - VERIFICA IMPORTO MASSIMO RISARCIBILE PER EVENTO FORTUITO
043200140408     c                   eval      $CAok_EvFo =  *off
043300151026if  3c                   if        §TADevfo   =  'S'  and
043400151026     c                             $CCH_Tecn  =  *off
043500140408     c                   exsr      Ctr_IMRxEvFor
043600140408e   3c                   endif
043700140408      *
043800140408      * - Calcolo totale risarcibile
043900140408if  3c                   if            §TADevfo   = *blanks
044000151026     c                             and $CCH_Tecn  = *off
044100140408     c                             or  §TADevfo   = 'S'
044200140408     c                             and $CAok_EvFo = *on
044300140408     c                   eval      wIPV += DCTipv
044400140408e   3c                   endif
044500140408      *
044600140408      * - Ricerca eventuale CA in fase 400   &
044700140408      * - Calcolo totale risarcibile
044800140408     c     kDCT8         reade     FNDCT8
044900140408      *
045000140430do  3c                   doW       Not  %eof(FNDCT08L)
045100140505     c                             and  DCTatb <> *blank
045200140408     c     kDCT8         reade     FNDCT8
045300140408e   3c                   endDo
045400140408      *
045500140408if  3c                   if        Not  %eof(FNDCT08L)
045600140505     c                             and  V7Hill =  *blank
045700140505     c                             and  DCTgfc =  I07poi
045800140505     c                             and  DCTfpr =  'P'
045900140408     c                   exsr      VerFCA
046000140408e   3c                   endif
046100140408      *
046200140408e   2c                   EndDo
046300140408      *
046400990226     c* scrivo il record
046500990226     c                   exsr      sub_wrt
046600140408      *
046700990226     c                   endif
046800140408      *
046900990226     c* se non è stata definita nessuna fase CA
047000990226     c                   if        i07fca  = 0
047100990226     c                   exsr      sub_wrt
047200990226     c                   endif
047300980722      *
047400980722     c                   endsr
047500140408      *
047600140408      *-------------------------------------------------------------------------*
047700140408      * Decodifica Tipo Anomalia Danni
047800140408      *-------------------------------------------------------------------------*
047900140408     c     sr_DecodTAD   BEGSR
048000140408      *
048100140408     c                   clear                   dTAD
048200140408     c                   clear                   dsBS02
048300140408      *
048400140408      * reperimento anomalia e sua descrizione
048500140408     c                   movel     'C'           t02mod
048600140408     c                   movel     knsif         t02sif
048700140408     C                   MOVEL(P)  dettad        t02ke1
048800140408     C                   movel     'TAD'         t02cod
048900140408      *
049000140408     C                   CALL      'TIBS02R'
049100140408     c                   parm                    KPJBA
049200140408     c                   parm                    dsBS02
049300140408      *
049400140408     c                   if        t02err =  *blanks
049500140408     c                   movel     t02uni        dTAD
049600140408     c                   endif
049700140408      *
049800140408     c                   ENDSR
049900140408      *
050000140408      *****************************************************************
050100140408      * CALCOLO IMPORTO MASSIMO RISARCIBILE X C.A. DI EVENTO FORTUITO *
050200140408      *****************************************************************
050300140408     c     Ctr_IMRxEvFor BEGSR
050400140408      *
050500140408      *  Verifica se Mandato Assicurativo
050600140408     c     K03DPT01      chain     FNDPT000
050700140408      *
050800140408if  1c                   IF            %found(FNDPT01L)
050900140408     c                             and DPTatb =  *blanks
051000140408     c                   eval      $CAok_EvFo =  *on
051100140408     c                   leavesr
051200140408e   1c                   ENDIF
051300140408      *
051400140408      *  Verifica se Importo da Assicurare in bolla
051500140408     c                   eval      *in30      =  *off
051600140408     c     K04TAS30      chain     TITAS30C                           30
051700140408      *    (?impossibile?che NON ci sia... )
051800140408      *  - se tipo bolla non è valido cerco altro rcd
051900140408     c                   eval      TBLkut = 1
052000140408     c                   eval      TBLcod = 'TB'
052100140408     c                   movel(p)  TAStbl        TBLkey
052200140424     c     K03TABEL      chain     TABEL
052300140408if  2c                   if        %found(TABEL00F)
052400140408     c                   movel     TBLuni        dsTB
052500140408if  3c                   if        §TBrbl = 'R'
052600140408     c     K04TAS30      reade     TITAS30C                               30
052700140408e   3c                   endif
052800140408e   2c                   endif
052900140408      *
053000140408if  2c                   IF        NOT *in30
053100140408     c                             and TASias >  *zeros
053200140408     c                             and TASfcm =  *blanks
053300140408     c                   eval      $CAok_EvFo =  *on
053400140408e   2c                   ENDIF
053500140408      *
053600140408     c                   ENDSR
053700140408      *
053800990226     C*-------------------------------------------------------------------------*
053900990226     c* scrittura del record nel subfile
054000990226     C*-------------------------------------------------------------------------*
054100990226     c     SUB_WRT       BEGSR
054200990226     c*
054300990226     C                   ADD       1             nrr1                           *
054400990226     c                   clear                   v7sopz
054500990226     c                   clear                   v7snev
054600990226     c                   clear                   v7haae
054700990226     c                   clear                   v7sdev
054800990226     c                   clear                   v7spos
054900990226     c                   clear                   v7stad
055000140424     c                   clear                   V7Simr
055100990226     c                   clear                   v7sdes
055200990226     C                   z-add     detnev        v7snev                         *
055300990226     C                   z-add     detaae        v7haae                         *
055400990226      *
055500990226     C                   clear                   wlbdat
055600990226     C                   z-add     detdat        G02inv
055700990226     C                   movel     '3'           G02err
055800990226     C                   call      'XSRDA8'
055900990226     C                   parm                    wlbdat
056000140430     c*//                movel     g02dat        v7sdev
056100140430     c                   eval      wDate_DMY = %date( G02dat : *EUR )
056200140430     c                   eval      V7Sdev    = %dec( wDate_DMY )
056300990226      *
056400990226      * Decodifica P.O. di comunicazione Evento
056500990226     C     detfce        chain     azorg01l                           30
056600990226     c                   if        orgde5 <> *blanks and *in30 = *off
056700990226     C                   movel     orgde5        V7spos
056800990226     c                   else
056900990226     C                   movel     orgdes        V7spos
057000990226     c                   endif
057100140408      * Descrizione anomalia
057200990226     C                   movel     §TADdesc      v7stad
057300990226      * Recupero la descrizione Evento
057400990226     c                   movel     'E'           ktpd
057500990226     c                   movel     'D'           ktrc
057600990226     C                   move      detaae        dsanev
057700990226     C                   move      detnev        dsnuev
057800990226     c                   movel     dsnumev       knkt
057900990226     c                   z-add     1             kpno
058000990226     c     kdcs          chain     fndcs01l                           30
058100990226     c  n30              movel     dcsnot        v7sdes
058200990226     c                   z-add     2             kpno
058300990226     c     kdcs          chain     fndcs01l                           30
058400140424     c  n30              eval      %subst( V7Sdes : %len(DCSnot) + 1 )
058500140424     c                                   = DCSnot
058600990226     c                   z-add     3             kpno
058700990226     c     kdcs          chain     fndcs01l                           30
058800140424     c  n30              eval      %subst( V7Sdes : (%len(DCSnot) * 2) + 1 )
058900140424     c                                   = DCSnot
059000140408      *
059100140408      * Già recuperato il Totale Massimo Risarcibile
059200140424     c                   if        wIPV > 999999999.99
059300140424     c                   z-add     *hival        V7Simr
059400140424     c                   else
059500140424     c                   z-add     wIPV          V7Simr
059600140424     c                   endif
059700990226     c*
059800990226     C                   write     dn07s01                                      *
059900990226     c*
060000990226      *
060100990226     c                   endsr
060200981009     C*-------------------------------------------------------------------------*
060300981009     c* verifico lo stato delle CA legate al singolo evento
060400981009     C*-------------------------------------------------------------------------*
060500981009     c     verfca        BEGSR
060600981009     c*
060700981012     c* calcolo la fase successiva
060800981012     c                   CLEAR                   FIDN05
060900981012     c                   movel     'F'           I05MOD
061000981012     c                   z-add     DCTfca        I05fca
061100981012     c                   movel     DCTfpr        I05fpr
061200981012     c                   movel     DCTptr        I05tpc
061300981012     c                   movel     'E'           I05fpe
061400981012     c                   movel     'O'           I05ffs
061500981012     c                   z-add     Datcor        i05dta
061600981012     c                   move      DCTtad        i05tad
061700981030     c                   IF        DCTnde > *zeros
061800981030     c                   movel     'D'           I05fde
061900981102e   2c                   ELSE
062000981102     c                   movel     'N'           I05fde
062100981102e   2c                   ENDIF
062200981125      * valorizzo testata c.a.
062300981125     c                   z-add     dctaac        i05aac
062400981125     c                   z-add     dctfil        i05fil
062500981125     c                   z-add     dctnca        i05nca
062600981012
062700981012     C                   CALL      'FIDN05R'
062800981012     c                   PARM                    KPJBA
062900981012     c                   PARM                    FIDN05
063000981012      *
063100981012if  2c                   IF        O05err = *blanks
063200981012     c                   movel     O05REC        DS_FNDFA
063300981012x   2c                   else
063400981012     c                   movel     '000'         dfafca
063500981012e   2c                   ENDIF
063600981012     c*  se fase calcolata uguale a quella passata accendo indicatore di reverse immage
063700981012if  2c                   if        dfafca   =  i07fca
063800981012     c                   seton                                        5051
063900981012     c                   eval      v7hill = 'Y'
064000140430     c*//                eval      ds_FNDCTx = ds_FNDCT
064100981012e   2c                   endif
064200981012     c*
064300981012     c                   endsr
064400981009      *
064500980721     C*-------------------------------------------------------------------------*
064600980721     c     *inzsr        BEGSR
064700981012     C*-------------------------------------------------------------------------*
064800980721      *
064900980721     C     *ENTRY        PLIST                                                  *
065000980721     C                   PARM                    KPJBA                          *
065100980721     c                   parm                    fidn07
065200140408      *
065300140408      * -?Reperimento dati job?
065400140408     c                   exsr      sr_DatiJob
065500140408      /free
065600140408         // -?Apertura TITAS30C se si è in SEDE?
065700140408         //if  I07poi = 046;
065800140408         if  %subst( KNSIF : 1 : 3 ) = 'GAI';
065900140408           open  TITAS30C;
066000140408         endif;
066100140408      /end-free
066200981012     c* calcola la data corrente
066300981012     c                   TIME                    wn14                           *ora(6) + data(8)
066400981012     c                   MOVE      wn14          wn8                            *data(8) in g/m/aa
066500981012     c                   Z-ADD     wn8           g02dat
066600981012     c                   Z-ADD     *zeros        g02inv
066700981012     c                   MOVEL     '0'           g02err
066800981012     c                   CALL      'XSRDA8'
066900981012     c                   PARM                    wlbdat
067000981012     c                   Z-ADD     g02inv        datcor                         *d.corrente aa/m/g
067100981030      *
067200980723      * controllo se e' valorizzato il P.O. interessato agli aventi in ricerca
067300980723      *
067400981005     c                   if        i07poi > 0   and
067500981030     c                             i07poi <> 46
067600980723      * decodifica
067700980723     c     i07poi        chain     azorg01l                           30
067800981012     c* n30              movel     orgdes        v7tdpo
067900980723      *
068000981012     c*                  z-add     i07poi        v7tcpo
068100980723     c                   endif
068200981005     c* se il campo I07MOD = I do la possibilità di ricercare il numero evento ma anche di interro-
068300981005     c* garne il dettaglio mentre con R faccio solo la visualizzazione
068400981005     c                   if        i07mod = 'I'
068500981005     c                   seton                                        15
068600981005     c                   endif
068700981030      * se passata la fase decodifico
068800981012     c                   if        i07fca > 0
068900981030      *
069000981012     c                   CLEAR                   FIDN05
069100981012     c                   movel     'C'           I05MOD
069200981012     c                   z-add     i07fca        I05fca
069300981012
069400981012     C                   CALL      'FIDN05R'
069500981012     c                   PARM                    KPJBA
069600981012     c                   PARM                    FIDN05
069700981012      *
069800981012     c                   IF        O05err = *blanks
069900981012     c                   movel     O05REC        DS_FNDFA
070000981012     c                   ENDIF
070100981012     c                   movel     dfadgc        v7dfca
070200981012     c                   endif
070300140408      *
070400140408      * - Memorizzazione tab. CCH in schiera per le sole causali di chiusure tecniche
070500140408     c                   clear                   XX
070600140408     c                   clear                   sk_CCH
070700140408     c                   eval      TBEcod  = 'CCH'
070800140408     c     TBEcod        setll     TNTBE000
070900140408do  1c                   DO        *hival
071000140408     c     TBEcod        reade     TNTBE000
071100140408      * - e.o.f.
071200140408if  2c                   if        %eof(TNTBE01L)
071300140408     c                   leave
071400140408e   2c                   endif
071500140408      * - rec. annullato
071600140408if  2c                   if            TBEatb <> *blank
071700140408     c                   iter
071800140408e   2c                   endif
071900140408      * - rec. da non memorizzare
072000140408     c                   movel     TBEuni        DCCH
072100140408if  2c                   if        §CCHchte <> 'S'
072200140408     c                   iter
072300140408e   2c                   endif
072400140408      * - memorizzazione causale
072500140408     c                   add       1             XX
072600140408     c                   movel     TBEke1        sk_CCH(xx)
072700140408e   1c                   ENDDO
072800980721      *
072900980721     c     kdcs          klist
073000980721     c                   kfld                    ktpd
073100980721     c                   kfld                    knkt
073200980721     c                   kfld                    kstd
073300980721     c                   kfld                    kdim
073400980721     c                   kfld                    khim
073500980721     c                   kfld                    knks
073600980721     c                   kfld                    ktrc
073700980721     c                   kfld                    kpno
073800980721     c**
073900980723     c* chiave file dettaglio  eventi
074000980723     c     KDED          klist
074100980723     C                   kfld                    detaae
074200980723     C                   kfld                    detnev
074300980723     c                   kfld                    i07poi
074400980723      **
074500980723     c     KDED1         klist
074600980723     C                   kfld                    detaae
074700980723     C                   kfld                    detnev
074800981009      **
074900981012     c     Kdct3         klist
075000981009     C                   kfld                    detaae
075100981009     C                   kfld                    detnev
075200981009     c                   kfld                    i07poi
075300981009      **
075400981009     c     Kdct8         klist
075500981009     C                   kfld                    detaae
075600981009     C                   kfld                    detnev
075700140408      * chiave file FNDPT01L
075800140408     c     K03DPT01      klist
075900140408     c                   kfld                    DCTaac
076000140408     c                   kfld                    DCTfil
076100140408     c                   kfld                    DCTnca
076200140408      * chiave file TITAS30C
076300140408     c     K04TAS30      klist
076400140408     c                   kfld                    DCTaas
076500140408     c                   kfld                    DCTlnp
076600140408     c                   kfld                    DCTnrs
076700140408     c                   kfld                    DCTnsp
076800140408      * chiave file tabelle
076900140408     c     K03TABEL      klist
077000140408     c                   kfld                    TBLkut
077100140408     c                   kfld                    TBLcod
077200140408     c                   kfld                    TBLkey
077300980723      *
077400980723     c                   clear                   o07aae
077500980723     c                   clear                   o07nev
077600980723     c                   clear                   o07err
077700980723     c                   clear                   o07msg
077800980723      *
077900980721     c                   endsr
078000140408      /free
078100140408       //--------------------------------------------------------------
078200140408       //?Reperimento Dati del job (Utente/Operativi).                 ?
078300140408       //--------------------------------------------------------------
078400140408       BEGSR  sr_DatiJob;
078500140408
078600140408         in(e) §AzUte;
078700140408         if NOT %error;
078800140408           in(e) §DatiUte;
078900140408         endif;
079000140408         if %error or RSut = *blank;
079100140408           tibs34r ( tibs34ds );
079200140408           in §AzUte;
079300140408           in §DatiUte;
079400140408         endif;
079500140408
079600140408       ENDSR;
079700140408      /end-free
079800980526**
079900980526Non e' stato selezionato nulla
080000980722Data errata
080100980722Punto operativo errato
080200980722Intervallo di date errato
