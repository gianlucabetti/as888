000100990506     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990506     H*PARMS ACTGRP(QILE)
000300980910     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400951002?     *--------------------------------------------------------------*
000500951002      *     *----------------------------------------------*         *
000600031119      *      prestazioni occasionali autofattura fornitori           *
000700031119      *     *----------------------------------------------*         *
000800031120     Ffiott00f  uF a E             DISK
000900031120     Ffiapd01l  IF   E           K DISK
001000031127     Faitra03l  IF   E           K DISK
001100031119     Fansog01l  IF   E           K DISK
001200101116     F*****ansif01l  IF   E           K DISK
001300020417     Fanfrn01l  IF   E           K DISK
001400020417     Fanrco01l  IF   E           K DISK
001500020417     Ftntbe01l  IF   E           K DISK
001600031119     Fficn94D   CF   E             WORKSTN
001700980910?     *--------------------------------------------------------------*
001800980910?     *  DS                                                          *
001900980910?     *--------------------------------------------------------------*
002000980910     D DSFMT           DS
002100980910     D  $TASTO               369    369
002200980910     D  NRG                  370    370
002300980910     D  NCL                  371    371
002400980910     D* posizione cursore
002500980910     D CURSOR          DS
002600980910     D  RIRI                   1      2B 0 INZ
002700980910     D  R$$                    2      2
002800980910     D  COCO                   3      4B 0 INZ
002900980910     D  C$$                    4      4
003000031119     D PARAM           DS
003100031119     D  RRN                           9S 0
003200031119     D  OPZ                           2
003300031120     D  cmd                           2
003400951002     D KPJBA         E DS
003500031119     D tibs02ds      E DS
003600031119     D fnlv24ds      E DS
003700031120     D fiottds       E DS                  extname(fiott00f)
003800031120     D ddatiute      e ds
003900031120     D azuteds       e ds                  extname(AZUTE00F)
004000031120     D tibs34ds      E DS                  inz
004100031119     D dcaf          E DS
004200031203     D dblc          E DS
004300031203     D dbld          E DS
004400031203     D dblo          E DS
004500031119     D Ptr             S               *
004600031119     D                                     INZ(%ADDR(§CAFMES1))
004700031120     D mes             S                   LIKE(§CAFMES1)
004800031119     D                                     DIM(12)
004900031119     D                                     BASED(Ptr)
005000031119     D Ptr1            S               *
005100031119     D                                     INZ(%ADDR(§CAFIMP1))
005200031120     D imp             S                   LIKE(§CAFIMP1)
005300031119     D                                     DIM(12)
005400031119     D                                     BASED(Ptr1)
005500031119     D* Reperimento nome PGM
005600031119     D                SDS
005700031119     D  nompgm           *PROC
005800990506     D*-------------
005900031119     D com10           s             10    inz('0000000000')
006000031203     Ddataoggi         s               d   datfmt(*iso)
006100031203     Ddatapre          s               d   datfmt(*iso)
006200031203     Ddatapro          s               d   datfmt(*iso)
006300031203     Ddatablo          s               d   datfmt(*iso)
006400031203     Ddataini          s               d   datfmt(*iso)
006500031203     Ddatafin          s               d   datfmt(*iso)
006600031203     Ddataiso          s               d   datfmt(*iso)
006700031119     Ddataeur          s               d   datfmt(*eur)
006800040107     Ddatacom          s               d   datfmt(*iso)
006900031119     Dkksc             s                   like(v1cfor)
007000061206     dannopro          s              4  0
007100061206     dannoblo          s              4  0
007200031120     Ddutpous          s              3
007300031120     Dcom03            s              3
007400031121     Dottrrn           s             10  0 inz
007500031121     Dv1cimps          s                   like(v1cimp)
007600101116     ***************************************************************************
007700101116     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
007800101116     D ESITO_OK...
007900101116     D                 C                   0
008000101116     ***************************************************************************
008100101116     **
008200101116     ** Dichiarazione prototipi procedure esterne.
008300101116     **
008400101116     ***************************************************************************
008500101116      /DEFINE DFTACTGRP_YES
008600101116     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
008700101116      /UNDEFINE DFTACTGRP_YES
008800101116     ***************************************************************************
008900101116     **
009000101116     ** Definizione strutture dati.
009100101116     **
009200101116      **************************************************************************
009300101116     D tibsSocI0...
009400101116     D               E DS                  QUALIFIED
009500101116     D                                     INZ
009600101116     D tibsSocO0...
009700101116     D               E DS                  QUALIFIED
009800101116     D                                     INZ
009900101116     ***************************************************************************
010000101116     **
010100101116     ** Definizione variabili modulo/programma.
010200101116     **
010300101116     ***************************************************************************
010400101116     D prmRqsOpCode...
010500101116     D                 S             10A
010600101116     D prmRpyOpCode...
010700101116     D                 S             10A
010800101116     D prmRpyIdMsg...
010900101116     D                 S             10I 0
011000101116     D prmRqsFormato...
011100101116     D                 S             10A
011200101116     D prmRqsDataSize...
011300101116     D                 S             10I 0
011400101116     D prmRpyFormato...
011500101116     D                 S             10A
011600101116     D prmRpyDataSize...
011700101116     D                 S             10I 0
011800101116
011900951002?     *--------------------------------------------------------------*
012000951002?     *  CICLO PRINCIPALE                                            *
012100951002?     *--------------------------------------------------------------*
012200951002      *  Impostazione parametri prima videata
012300951002     C                   EXSR      INZ01
012400951002      *  Loop gestione videata
012500031120     C                   DO        *hival
012600951002     C                   EXSR      GESD01
012700031120     C                   ENDdo
012800951002?     *--------------------------------------------------------------*
012900951002?     *  INZ01: Inizializzazione prima videata                       *
013000951002?     *--------------------------------------------------------------*
013100951002     C     INZ01         BEGSR
013200031119      *INSERIMENTO
013300031120     c                   if        opz = '06' or opz = '03'
013400031120     c                   seton                                        06
013500031120     c                   end
013600031119      *VISUALIZZAZ.
013700031119     C     OPZ           COMP      '05'                                   05
013800031119     C                   SETOFF                                       28
013900031119     c                   if        rrn = 0
014000031119     C                   z-add     0             V1CPDR
014100031201     C                   movel     *blanks       V1ctip
014200031120     C                   movel     *blanks       V1dPDR
014300031119     C                   movel     *blanks       V1Csoc
014400031119     C                   movel     *blanks       V1dsoc
014500031119     C                   movel     *blanks       V1Cfor
014600031119     C                   movel     *blanks       V1dfor
014700031120     C                   Z-ADD     0             V1Cnft
014800031120     C                   Z-ADD     0             V1Cdft
014900031120     C                   Z-ADD     0             V1Cdti
015000031120     C                   Z-ADD     0             V1Cnrm
015100031119     C                   Z-ADD     0             V1Cimp
015200031121     C                   Z-ADD     0             V1Cimps
015300031119     C                   MOVEL     *blanks       v1ctab
015400031119     C                   MOVEL     *blanks       v1dtab
015500031119     C                   MOVEL     *blanks       v1cnot
015600031119     c                   else
015700031119     c     rrn           chain     fiott00f
015800031119     c                   if        %found(fiott00f)
015900031120     C                   MOVEL     otttab        v1ctab
016000031120     C                   MOVEL     ottnot        v1cnot
016100031120     C                   MOVEL     otttip        v1ctip
016200031119     C                   z-add     ottpdr        V1CPDR
016300031119     C                   movel     ottsoc        V1Csoc
016400031119     C                   movel     ottcdf        V1Cfor
016500031120     C                   move      ottddc        dataiso
016600031120     C                   move      dataiso       dataeur
016700031119     C                   move      dataeur       V1Cdti
016800031120     c                   if        not *in06
016900040113     C                   Z-ADD     ottqta        V1Cnrm
017000040113     C                   Z-ADD     otttim        V1Cimp
017100031121     C                   Z-ADD     otttim        V1Cimps
017200031120     c                   if        ottdft <> 0
017300031120     C                   move      ottdft        dataiso
017400031120     C                   move      dataiso       dataeur
017500031120     C                   move      dataeur       V1Cdft
017600031120     c                   end
017700031120     c                   Z-ADD     ottnff        V1Cnft
017800031120     c                   end
017900031119     c                   exsr      ctr01
018000031121     c* reimposto l'importo originale xchè nella subroutine ctr01
018100040107     c* potrebbe essere stato variato
018200031121     c                   if        otttim <> 0
018300031121     C                   Z-ADD     otttim        V1Cimp
018400031121     c                   clear                   msgv
018500031119     c                   end
018600031121     c                   end
018700031119     c                   end
018800031121     C                   movea     com10         *in(40)
018900080108     c                   setoff                                       2850
019000951002     C*
019100951002     C                   ENDSR
019200951002?     *--------------------------------------------------------------*
019300951002?     *  GESD01: Gestione prima videata                              *
019400951002?     *--------------------------------------------------------------*
019500951002     C     GESD01        BEGSR
019600951002      *
019700031119     c* visualizzazione
019800031119if  1C                   if        *in05 OR OPZ = '04'
019900031119     C                   write     y350d01
020000031119     C                   exfmt     PROTECT
020100031119x   1C                   else
020200031120     c                   if        *in28
020300031120     c                   setoff                                       28
020400031120     C                   write     y350d01
020500031120     c                   seton                                        28
020600031120     c                   end
020700031119     C                   EXFMT     y350d01
020800031119     c                   end
020900031119     c*
021000080108     C                   SETOFF                                       2850
021100031119     C                   movea     com10         *in(40)
021200951002      *  Fine Lavoro
021300951002     C     *INKC         IFEQ      '1'
021400031120     C     *INKl         orEQ      '1'
021500031119     C                   exsr      fine
021600951002     C                   END
021700020403     C*  effettua ricerche per fornitore
021800031119     c                   if        not *in05
021900980910     c                   if        *inkd = *on
022000980910     c                   exsr      search
022100980910     c                   end
022200040114      *  Controlli solo se non sono in visualizzaz. o cancellaz.
022300040114if  1C                   if        not *in05 and OPZ <> '04'
022400951002     C                   EXSR      CTR01
022500951002     C   28              GOTO      FINVD1
022600040114     c                   end
022700031120      *  aggiorno
022800031119     c                   if        *inkf
022900031119     C                   EXSR      aggio
023000031119     c                   exsr      fine
023100031119     c                   end
023200031119     c                   end
023300951002      *
023400951002     C     FINVD1        ENDSR
023500951002?     *--------------------------------------------------------------*
023600951002?     *  CTR01: Controlli prima videata                              *
023700951002?     *--------------------------------------------------------------*
023800951002     C     CTR01         BEGSR
023900031121     c                   clear                   msgv
024000031204     c                   seton                                        33
024100031119     c                   do
024200031119      *  Controllo tabella CAF
024300031119     c                   clear                   v1dtab
024400031119     c                   if        v1ctab =  ' '
024500031120     c                   seton                                        49
024600031119     c                   leave
024700031119     c                   else
024800031119     c                   movel     v1ctab        tbeke1
024900031203     c                   eval      tbecod='CAF'
025000031120     c     ktbe          chain     tntbe01l
025100031120     c                   if        not %found(tntbe01l) or tbeatb = 'A'
025200031119     c                   seton                                        49
025300031120     c                   leave
025400031119     c                   else
025500031119     c                   movel     tbeuni        dcaf
025600031119     c                   movel     §cafdes       v1dtab
025700031201     c                   movel     §caftip       v1ctip
025800031119     c                   end
025900031119     c                   end
026000031201     c* decodicfico tipo anagrafico
026100031201     c                   select
026200031201     c                   when      v1ctip = 'A'
026300031201     c                   eval      v1dtip = 'AUTOTRASPORTATORI'
026400031201     c                   when      v1ctip = 'C'
026500031201     c                   eval      v1dtip = 'COOPERATIVE'
026600031201     c                   when      v1ctip = 'D'
026700031201     c                   eval      v1dtip = 'AFFLUENZE/DEFLUENZE'
026800031201     c                   endsl
026900031120     c* data prestazione
027000031120     c                   if        v1cdti = 0
027100031120     C                   SETON                                        47
027200040107     C                   leave
027300031120     C                   ELSE
027400031120     c     *eur          test(d)                 v1cdti                 47
027500031120     c                   if        *in47
027600031120     c     *dmy          test(d)                 v1cdti                 47
027700031120     c  n47*dmy          move      v1cdti        dataeur
027800031120     c                   else
027900031120     c                   move      v1cdti        dataeur
028000031120     c                   end
028100031120     c  n47              move      dataeur       v1cdti
028200031120     c                   end
028300031120     c* controllo che la data sia in un mese in cui si può contabilizzare
028400031120     c                   if        not *in47
028500031120     c                   extrct    dataeur:*m    mm                2 0
028600031120     c                   if        mes(mm) = ' '
028700031120     c                   seton                                        43
028800031203     c                   leave
028900031120     c                   end
029000031203     c                   move      dataeur       datapre
029100040107     c                   else
029200040107     c                   leave
029300031120     c                   end
029400031204     c* se l'importo del mese nella tabella è 0 sproteggo il campo dell'imp.
029500031204     c* totale
029600031204     c                   if        §cafqta = 'N' and imp(mm) = 0
029700031204     c                   setoff                                       33
029800031204     c                   end
029900031203     c* reperisco la tabella BLC/BLD/BLO
030000031203     c                   exsr      srtabblo
030100031203     c* controllo che l'INSERIMENTO delle prestazioni avvenga nel mese in
030200031203     c* cui si può contabilizzare fino alla data blocco contabilizzazione
030300031203     c                   move      *date         dataeur
030400031203     c                   move      dataeur       dataoggi
030500031203     c* oggi non può essere minore della data di protocollo
030600031203     c                   if        dataoggi < datapro
030700031203     c                   seton                                        48
030800031203     c                   leave
030900031203     c                   end
031000040107     c**********************************************************************
031100040107     c* esempio: data blocco           10/12/03
031200040107     c*          data oggi              1/12/03
031300040107     c*          range prestazioni      1/11/03 - 30/11/03
031400040107     c*          range data immissione  1/11/03 - 10/12/03
031500040107     c* esempio: data blocco           10/12/03
031600040107     c*          data oggi             15/12/03
031700040107     c*          range prestazioni      1/12/03 - 31/12/03
031800040107     c*          range data immissione  1/12/03 - 10/01/03
031900040107     c**********************************************************************
032000040107     c* data inizio
032100040107     c                   move      datablo       comodo           10
032200040107     c                   move      '01'          comodo
032300040107     c                   move      comodo        dataini
032400061206     c* se oggi è < data blocco il range slitta indietro di un mese solo se
032500061206     c* anno protocollo <> da anno blocco
032600061206     c                   EXTRCT    datablo:*y    annoblo
032700061206     c                   EXTRCT    datapro:*y    annopro
032800140307     c*                  if        dataoggi<=datablo and annopro<>annoblo
032900140307     c                   if        dataoggi<=datablo
033000040107     c                   subdur    1:*m          dataini
033100040107     c                   end
033200040107     c* la data fine
033300031203     c     dataini       adddur    1:*m          datafin
033400031203     c                   subdur    1:*d          datafin
033500040107     c* la data prestazione deve essere in questi limiti
033600040107     c                   if        datapre>=dataini and datapre<=datafin
033700040107     c                   else
033800040107     c                   seton                                        47
033900040107     c                   leave
034000040107     c                   end
034100040107     c* oggi deve essere maggiore della data inizio e minore della data fine
034200040112     c* più i 15 gg ipotetici
034300040107     c                   eval      datacom = datafin
034400040112     c                   adddur    15:*d         datacom
034500040107     c                   if        dataoggi>=dataini and datapre<=datacom
034600040107     c                   else
034700040107     c                   seton                                        47
034800040107     c                   leave
034900040107     c                   end
035000031119      *  Controllo codice autotrasportatore
035100031119     c                   clear                   v1dpdr
035200031120     c                   if        v1cpdr =  0
035300031120     c                   seton                                        40
035400031120     c                   leave
035500031120     c                   else
035600031120     c* controllo il p.o.
035700140122     c                   movel     v1cpdr        com03
035800031120     c                   if        dutpous <> '046'
035900031120     c                   if        com03 <> dutpous
036000031120     c                   seton                                        40
036100031120     c                   leave
036200031120     c                   end
036300031120     c                   end
036400140122     c* chiodo momentaneo solo filiali 93 141 173 132
036500140122     c                   if        v1ctab = 'SPUC' and
036600140122     c                             com03 <> '093' and
036700140122     c                             com03 <> '141' and
036800140122     c                             com03 <> '132' and
036900140122     c                             com03 <> '173'
037000140122     c                   seton                                        40
037100140122     c                   leave
037200140122     c                   end
037300140122     c
037400031120     c*
037500031120     c     kapd          chain     fiapd01l
037600031120     c                   if        not %found(fiapd01l)
037700031119     c                   seton                                        40
037800031119     c                   leave
037900031119     c                   else
038000031127     c                   if        v1ctip = 'A' and v1cnot = ' '
038100140122     c                             and v1ctab = 'PUBC'
038200040112     c                   clear                   v1cnot
038300040112     c     v1cpdr        setll     aitra03l
038400040112     c                   do        *hival
038500040112     c     v1cpdr        reade     aitra03l
038600040112     c                   if        %eof(aitra03l)
038700040112     c                   leave
038800040112     c                   end
038900040112     c* controllo che non sia disaccreditato e che ne esista uno solo
039000040112     c                   if        tradfi <> 0
039100040112     c                   iter
039200040112     c                   end
039300040112     c*
039400040112     c                   if        v1cnot <> ' '
039500040112     c                   clear                   v1cnot
039600040112     c                   leave
039700040112     c                   end
039800040112     C*
039900031127     c                   eval      v1cnot = 'Targa ' + trataa
040000040112     c                   enddo
040100031127     c                   end
040200100223     c                   movel     apdrsf        v1dpdr
040300031119     c                   movel     apdcsf        v1csoc
040400031119     c                   move      *all'0'       v1cfor
040500031119     c                   move      apdksc        v1cfor
040600040112     c                   end
040700031119     c                   end
040800031120      *  decodifico codice società
040900101116     c**** v1csoc        chain     ansif01l
041000101116     c****               if        not %found(ansif01l) or
041100101116     c****                         (%found(ansif01l) and SIFDTFIVL <>
041200101116     c****                         *loval)
041300101116     c****               seton                                        50
041400101116     c****               else
041500101116     c****               movel     sifdesbrev    v1dsoc
041600101116     c****               end
041700101116      *
041800101116      **
041900101116     c                   clear                   v1dsoc
042000101116     c                   exsr      decod_societa
042100101116      *
042200031120      *  Decodifico codice fornitore
042300031119     C                   clear                   v1dfor
042400031119     C                   move      V1Cfor        kksc
042500980910     c                   movel     *blank        sogdes
042600980910     C     Kfrn          CHAIN     anfrn01l                           30
042700980910     C  N30Krco          CHAIN     anrco01l                           30
042800980910     C  N30rcosogg       CHAIN     ansog01l                           30
042900031119     C                   MOVEL(p)  sogdes        v1dfor
043000031120      *  Controllo il numero dei mesi da moltiplicare solo in immissione
043100031121     C                   IF        §cafqta = 'S' and v1cnrm = 0 and *in06
043200031120     C                   SETON                                        41
043300031119     C                   leave                                                  Data iniz.>fina.
043400951002     C                   END
043500031120     C                   IF        §cafqta = 'N' and v1cnrm <>0
043600031120     C                   SETON                                        41
043700031120     C                   leave                                                  Data iniz.>fina.
043800031120     C                   END
043900031120     C                   IF        v1cnrm <> 0 and v1cnrm > mm
044000031120     C                   SETON                                        41
044100031120     C                   leave                                                  Data iniz.>fina.
044200031120     C                   END
044300031121     C*
044400031121     c                   if        v1cnrm <> 0
044500031121     c     imp(mm)       mult      v1cnrm        v1cimp
044600031121     c                   else
044700031204     c* se l'importo del mese nella tabella è 0 obbligatorio importo totale
044800040107     c                   if        imp(mm) = 0 and
044900040107     c                             v1cimp = 0
045000031204     c                   seton                                        44
045100031204     c                   leave
045200031204     c                   end
045300140121     c                   if        imp(mm) <>0 and
045400140121     c                             v1cimp = 0
045500140121     c                   eval      v1cimp = imp(mm)
045600140121     c                   end
045700031125     c                   end
045800031121     c* se sono in modifica avviso che se si aggiorna l'importo cambierà
045900031121     c                   if        opz = '02' and v1cimp <> v1cimps
046000031121     c                   eval      msgv = 'Se si conferma l''aggionamento -
046100031121     c                             l''importo verrà cambiato'
046200031121     c                   end
046300031121     c* controllo che non esista già il record
046400031121     c                   exsr      srctrrec
046500031119     C                   ENDdo
046600031120     c*
046700031120     c                   if        *in40 or *in41 or *in47 or *in49 or *in42
046800080108     c                             or *in43 or *in48 or *in44 or *in50
046900031120     c                   seton                                        28
047000031120     c                   end
047100951003      *
047200031119     C                   ENDSR
047300020404     C************************************************************
047400031203     C* tabella blocchi
047500020404     C************************************************************
047600031203     C     srtabblo      BEGSR
047700031203      *
047800031203     c                   clear                   datablo
047900031203     c                   clear                   datapro
048000031203     c                   eval      tbeke1 = '1'
048100031203     c                   select
048200031203     c                   when      v1ctip = 'A'
048300031203     c                   eval      tbecod = 'BLC'
048400031203     c                   when      v1ctip = 'C'
048500031203     c                   eval      tbecod = 'BLO'
048600031203     c                   when      v1ctip = 'D'
048700031203     c                   eval      tbecod = 'BLD'
048800031203     c                   endsl
048900031203     c     ktbe          chain     tntbe01l
049000031203     c                   if        %found(tntbe01l) and tbeatb = ' '
049100031203     c                   select
049200031203     c                   when      v1ctip = 'A'
049300031203     c                   eval      dblc = tbeuni
049400031203     c                   move      §blcdbl       datablo
049500031203     c                   move      §blcdtp       datapro
049600031203     c                   when      v1ctip = 'C'
049700031203     c                   eval      dblo = tbeuni
049800031203     c                   move      §blodbl       datablo
049900031203     c                   move      §blodtp       datapro
050000031203     c                   when      v1ctip = 'D'
050100031203     c                   eval      dbld = tbeuni
050200031203     c                   move      §blddbl       datablo
050300031203     c                   move      §blddtp       datapro
050400031203     c                   endsl
050500031203     c                   end
050600031203      *
050700031203     C                   ENDSR
050800031203     C************************************************************
050900031203     C* RICERCHE
051000031203     C************************************************************
051100031203     C     SEARCH        BEGSR
051200980910     C* determino Riga/Colonna del cursore
051300980910     C                   MOVE      nrg           R$$
051400980910     C                   MOVE      ncl           C$$
051500980910     C                   Z-ADD     RIRI          CURR
051600980910     C                   Z-ADD     COCO          CURC
051700980910     C*
0518009809101    C                   SELECT
051900031119     C* FMTD1  - ricerca autotrasp.
052000031120    >C                   WHEn      h1nmfl = 'V1CPDR'
052100031119     C                   clear                   fnlv24ds
052200031120     c* imposto il p.o.
052300031120     c                   if        dutpous <> '046'
052400031120     C                   MOVEL     dutpou        d24fil
052500031120     c                   end
052600031120     C                   MOVEL     v1ctip        d24tip
052700031119     C                   MOVEL     'R'           d24FLG
052800031119     C                   MOVEL(p)  fnlv24ds      KPJBU
052900031119     C                   CALL      'FNLV24R'
053000031119     C                   PARM                    KPJBA
053100031119     C                   MOVEL     KPJBU         fnlv24ds
053200031119     c                   move      d24pdr        v1cpdr
053300031119     c                   movel     d24rsc        v1dpdr
053400031119     C* FMTD1  - ricerca tabella prestazioni
053500031119    >C     H1NMFL        WHEneq    'V1CTAB'
053600031119     C                   clear                   Tibs02DS
053700031119     C                   eval      t02tla = 'L'
053800031119     C                   movel     'R'           t02mod
053900031119     C                   movel     'CAF'         t02cod
054000031119     C                   CALL      'TIBS02R'
054100031119     C                   parm                    KPJBA
054200031119     C                   parm                    TIBS02DS
054300031119    2C                   IF        T02err = *blanks
054400031119     c                   movel     t02ke1        v1ctab
054500031119     c                   movel     t02uni        dcaf
054600031119     c                   movel     §cafdes       v1dtab
054700031119     c                   end
054800020510     C                   ENDsl
054900980910     C* imposto pos. del cursore
055000980910     C                   Z-ADD     CURR          H1RIGA
055100980910     C                   Z-ADD     CURC          H1COLO
055200980910     C                   ENDsr
055300980910     C*----------------------------------------------------*
055400031120     C* aggiornamento
055500980910     C*----------------------------------------------------*
055600031120     C     aggio         BEGSR
055700031121     c* inserimento, copia, modifica
055800031121     c                   if        *in06 or opz = '02'
055900031120     c                   clear                   fiott000
056000031120     C                   eval      ottpdr = V1CPDR
056100031120     C                   eval      ottsoc = V1Csoc
056200031120     C                   eval      ottcdf = V1Cfor
056300031120     C                   move      v1cdti        dataeur
056400031120     C                   move      dataeur       dataiso
056500031120     C                   move      dataiso       ottddc
056600040113     C                   eval      ottqta = V1Cnrm
056700040113     C                   eval      otttim = V1Cimp
056800040113     C                   eval      ottimp = imp(mm)
056900031120     C                   eval      otttab = v1ctab
057000031120     C                   eval      ottnot = v1cnot
057100031120     C                   eval      otttip = v1ctip
057200031121     c                   end
057300031121     c                   select
057400031121     c                   when      *in06
057500031120     c                   write     fiott000
057600031121     c                   when      opz = '02'
057700031121     c                   update    fiott000
057800031121     c                   when      opz = '04'
057900031121     c                   delete    fiott000
058000031121     c                   endsl
058100980910     C*
058200031120     C                   ENDsr
058300951006?     *--------------------------------------------------------------*
058400031120?     * srctrrec - controllo record duplicato                        *
058500951006?     *--------------------------------------------------------------*
058600031120     C     srctrrec      BEGSR
058700140123     c                   move      v1cdti        dataeur
058800140123     c                   extrct    dataeur:*Y    year              4 0
058900140122     c*                  move      dataeur       dataiso
059000140123     c                   move      00010101      com08i
059100140122     c                   movel     year          com08i            8 0
059200140123     c                   move      00011231      com08f
059300140122     c                   movel     year          com08f            8 0
059400951006      *
059500031120     C/EXEC SQL
059600031121     C+ DECLARE A1 CURSOR FOR SELECT rrn(fiott00f) FROM fiott00F WHERE
059700031121     C+ otttab = :v1ctab and ottddc between :com08i and :com08f and
059800031121     C+ ottpdr = :v1cpdr and ottsoc = :v1csoc and ottcdf = :v1cfor
059900031120     C/END-EXEC
060000031120      *
060100031120     C/EXEC SQL
060200031120     C+ OPEN A1
060300031120     C/END-EXEC
060400031120      *
060500031121     C                   DO        *hival
060600031120     C/EXEC SQL
060700031121     C+ FETCH  next FROM A1 INTO :ottrrn
060800031120     C/END-EXEC
060900031121     C* controllo numero relativo di record se sono in modifica
061000031121     C                   if        SqlCod=0
061100031121     C                   if        opz='02' and rrn<>ottrrn
061200031121     c                   seton                                        42
061300031121     c                   leave
061400031121     c                   end
061500031121     C                   if        *in06
061600031121     c                   seton                                        42
061700031121     c                   leave
061800031121     c                   end
061900031121     C                   else
062000031121     c                   leave
062100031121     C                   END
062200031121     C                   ENDDO
062300031120      *
062400031120     C/EXEC SQL
062500031120     C+ CLOSE A1
062600031120     C/END-EXEC
062700031120     C*
062800031120     C                   ENDSR
062900031120?     *--------------------------------------------------------------*
063000031120?     * fine: Operazioni finali
063100031120?     *--------------------------------------------------------------*
063200031120     C     fine          BEGSR
063300031120     c*
063400031120     c                   clear                   cmd
063500031120     c                   select
063600031120     c                   when      *inkc
063700031120     c                   move      '03'          cmd
063800031120     c                   when      *inkl
063900031120     c                   move      '12'          cmd
064000031120     c                   when      *inkf
064100031120     c                   move      '06'          cmd
064200031120     c                   endsl
064300031120     c                   movel(p)  param         kpjbu
064400031120     c                   seton                                        lr
064500101116      *
064600101116     ** Chiudo il programma.
064700101116     C                   CALL      'TIBSSOCR'
064800101116     C                   PARM      'FINALIZE'    prmRqsOpCode
064900101116     C                   PARM                    prmRpyOpCode
065000101116     C                   PARM                    prmRpyIdMsg
065100101116
065200031120     c                   return
065300031120     C*
065400031120     C                   ENDSR
065500031120?     *--------------------------------------------------------------*
065600031120?     * *INZSR: Operazioni iniziali                                  *
065700031120?     *--------------------------------------------------------------*
065800031120     C     *INZSR        BEGSR
065900031120      *
066000951006     C     *ENTRY        PLIST
066100951006     C                   PARM                    KPJBA
066200031119     C                   MOVEL     KPJBU         PARAM
066300101116     C*
066400101116     ** Inizializzo il programma.
066500101116     C                   CALL      'TIBSSOCR'
066600101116     C                   PARM      'INIT'        prmRqsOpCode
066700101116     C                   PARM                    prmRpyOpCode
066800101116     C                   PARM                    prmRpyIdMsg
066900101116
067000031120     c     *dtaara       define    §azute        azuteds
067100031120     c     *dtaara       define    §datiute      ddatiute
067200031120     C                   in(E)     *dtaara
067300031120     C                   IF        %Error  or  RSUT = *blanks
067400031120     C                   call      'TIBS34R'
067500031120     C                   parm                    Tibs34Ds
067600031120     C                   in        *dtaara
067700031120     c                   ENDIF
067800031120     c                   movel     dutpou        dutpous
067900031120     c*
068000031120     c                   select
068100031121     c                   when      opz = '02'
068200031121     c                   eval      decopz = '   MODIFICA'
068300031121     c                   when      opz = '03'
068400031121     c                   eval      decopz = '     COPIA'
068500031120     c                   when      opz = '04'
068600031120     c                   eval      decopz = '   CANCELLA'
068700031120     c                   when      opz = '05'
068800031120     c                   eval      decopz = '  VISUALIZZA'
068900031120     c                   when      opz = '06'
069000031120     c                   eval      decopz = ' INSERIMENTO'
069100031120     c                   endsl
069200100729     C                   movel     '000400'      forita            6
069300020423     C     Kfrn          KLIST
069400031119     C                   KFLD                    v1csoc
069500020423     C                   KFLD                    kksc
069600980910     C     Krco          KLIST
069700031119     C                   KFLD                    v1csoc
069800020417     C                   KFLD                    forita
069900980910     C                   KFLD                    KKSC
070000031119     C     Ktbe          KLIST
070100031119     C                   KFLD                    tbecod
070200031119     C                   KFLD                    tbeke1
070300031119     C     Kapd          KLIST
070400031119     C                   KFLD                    v1ctip
070500031120     C                   KFLD                    v1cpdr
070600951006      *  Definisco variabili
070700980910     C                   Z-ADD     0             CURR              2 0
070800980910     C                   Z-ADD     0             CURC              2 0
070900980910     c                   endsr
071000101116     C**************************************************************************
071100101116     C* decodifica la società
071200101116     C************************************************************
071300101116     C     Decod_societa begSR
071400101116      *
071500101116     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
071600101116     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
071700101116     C********           RESET                   tibsSocI0
071800101116     C                   EVAL      tibsSocI0.idSocieta = v1csoc
071900101116     C
072000101116     C                   CALL      'TIBSSOCR'
072100101116     C                   PARM      'GETANAGRAF'  prmRqsOpCode
072200101116     C                   PARM      *BLANK        prmRpyOpCode
072300101116     C                   PARM      *ZERO         prmRpyIdMsg
072400101116     C                   PARM      'TIBSSOCI0'   prmRqsFormato
072500101116     C                   PARM                    tibsSocI0
072600101116     C                   PARM                    prmRqsDataSize
072700101116     C                   PARM      'TIBSSOCO0'   prmRpyFormato
072800101116     C                   PARM                    tibsSocO0
072900101116     C                   PARM                    prmRpyDataSize
073000101116     c                   if         PRMRPYIDMSG >= 0
073100101116     c                              and TIBSSOCO0.IDSOCIETA <> *blank
073200101116     c                   eval       v1dsoc  = tibsSocO0.RAGSOCIALE
073300101116     c                   else
073400101116     c                   seton                                        50
073500101116     c                   end
073600101116      *
073700101116     C                   ENDSR
073800101116     C*--------------------------------------------------------------------
