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
007600141124     D Targa_da_anagrafica...
007700141124     D                 s              1    inz
007800150107     D Targa_uguale_a_anagrafica...
007900150107     D                 s              1    inz
008000141124     D Chars_Targa...
008100141124     D                 C                   '0123456789QWERTYUIOPASDFGHJKLZXCVBN-
008200141124     D                                     M'
008300101116     ***************************************************************************
008400101116     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
008500101116     D ESITO_OK...
008600101116     D                 C                   0
008700101116     ***************************************************************************
008800101116     **
008900101116     ** Dichiarazione prototipi procedure esterne.
009000101116     **
009100101116     ***************************************************************************
009200101116      /DEFINE DFTACTGRP_YES
009300101116     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
009400101116      /UNDEFINE DFTACTGRP_YES
009500101116     ***************************************************************************
009600101116     **
009700101116     ** Definizione strutture dati.
009800101116     **
009900101116      **************************************************************************
010000101116     D tibsSocI0...
010100101116     D               E DS                  QUALIFIED
010200101116     D                                     INZ
010300101116     D tibsSocO0...
010400101116     D               E DS                  QUALIFIED
010500101116     D                                     INZ
010600101116     ***************************************************************************
010700101116     **
010800101116     ** Definizione variabili modulo/programma.
010900101116     **
011000101116     ***************************************************************************
011100101116     D prmRqsOpCode...
011200101116     D                 S             10A
011300101116     D prmRpyOpCode...
011400101116     D                 S             10A
011500101116     D prmRpyIdMsg...
011600101116     D                 S             10I 0
011700101116     D prmRqsFormato...
011800101116     D                 S             10A
011900101116     D prmRqsDataSize...
012000101116     D                 S             10I 0
012100101116     D prmRpyFormato...
012200101116     D                 S             10A
012300101116     D prmRpyDataSize...
012400101116     D                 S             10I 0
012500101116
012600951002?     *--------------------------------------------------------------*
012700951002?     *  CICLO PRINCIPALE                                            *
012800951002?     *--------------------------------------------------------------*
012900951002      *  Impostazione parametri prima videata
013000951002     C                   EXSR      INZ01
013100951002      *  Loop gestione videata
013200031120     C                   DO        *hival
013300951002     C                   EXSR      GESD01
013400031120     C                   ENDdo
013500951002?     *--------------------------------------------------------------*
013600951002?     *  INZ01: Inizializzazione prima videata                       *
013700951002?     *--------------------------------------------------------------*
013800951002     C     INZ01         BEGSR
013900031119      *INSERIMENTO
014000031120     c                   if        opz = '06' or opz = '03'
014100031120     c                   seton                                        06
014200031120     c                   end
014300031119      *VISUALIZZAZ.
014400031119     C     OPZ           COMP      '05'                                   05
014500031119     C                   SETOFF                                       28
014600031119     c                   if        rrn = 0
014700031119     C                   z-add     0             V1CPDR
014800031201     C                   movel     *blanks       V1ctip
014900031120     C                   movel     *blanks       V1dPDR
015000031119     C                   movel     *blanks       V1Csoc
015100031119     C                   movel     *blanks       V1dsoc
015200031119     C                   movel     *blanks       V1Cfor
015300031119     C                   movel     *blanks       V1dfor
015400031120     C                   Z-ADD     0             V1Cnft
015500031120     C                   Z-ADD     0             V1Cdft
015600031120     C                   Z-ADD     0             V1Cdti
015700031120     C                   Z-ADD     0             V1Cnrm
015800031119     C                   Z-ADD     0             V1Cimp
015900031121     C                   Z-ADD     0             V1Cimps
016000031119     C                   MOVEL     *blanks       v1ctab
016100031119     C                   MOVEL     *blanks       v1dtab
016200031119     C                   MOVEL     *blanks       v1cnot
016300031119     c                   else
016400031119     c     rrn           chain     fiott00f
016500031119     c                   if        %found(fiott00f)
016600031120     C                   MOVEL     otttab        v1ctab
016700031120     C                   MOVEL     ottnot        v1cnot
016800031120     C                   MOVEL     otttip        v1ctip
016900031119     C                   z-add     ottpdr        V1CPDR
017000031119     C                   movel     ottsoc        V1Csoc
017100031119     C                   movel     ottcdf        V1Cfor
017200031120     C                   move      ottddc        dataiso
017300031120     C                   move      dataiso       dataeur
017400031119     C                   move      dataeur       V1Cdti
017500031120     c                   if        not *in06
017600040113     C                   Z-ADD     ottqta        V1Cnrm
017700040113     C                   Z-ADD     otttim        V1Cimp
017800031121     C                   Z-ADD     otttim        V1Cimps
017900031120     c                   if        ottdft <> 0
018000031120     C                   move      ottdft        dataiso
018100031120     C                   move      dataiso       dataeur
018200031120     C                   move      dataeur       V1Cdft
018300031120     c                   end
018400031120     c                   Z-ADD     ottnff        V1Cnft
018500031120     c                   end
018600031119     c                   exsr      ctr01
018700031121     c* reimposto l'importo originale xchè nella subroutine ctr01
018800040107     c* potrebbe essere stato variato
018900031121     c                   if        otttim <> 0
019000031121     C                   Z-ADD     otttim        V1Cimp
019100031121     c                   clear                   msgv
019200031119     c                   end
019300031121     c                   end
019400031119     c                   end
019500031121     C                   movea     com10         *in(40)
019600080108     c                   setoff                                       2850
019700951002     C*
019800951002     C                   ENDSR
019900951002?     *--------------------------------------------------------------*
020000951002?     *  GESD01: Gestione prima videata                              *
020100951002?     *--------------------------------------------------------------*
020200951002     C     GESD01        BEGSR
020300951002      *
020400031119     c* visualizzazione
020500031119if  1C                   if        *in05 OR OPZ = '04'
020600031119     C                   write     y350d01
020700031119     C                   exfmt     PROTECT
020800031119x   1C                   else
020900031120     c                   if        *in28
021000031120     c                   setoff                                       28
021100031120     C                   write     y350d01
021200031120     c                   seton                                        28
021300031120     c                   end
021400031119     C                   EXFMT     y350d01
021500031119     c                   end
021600031119     c*
021700080108     C                   SETOFF                                       2850
021800031119     C                   movea     com10         *in(40)
021900951002      *  Fine Lavoro
022000951002     C     *INKC         IFEQ      '1'
022100031120     C     *INKl         orEQ      '1'
022200031119     C                   exsr      fine
022300951002     C                   END
022400020403     C*  effettua ricerche per fornitore
022500031119     c                   if        not *in05
022600980910     c                   if        *inkd = *on
022700980910     c                   exsr      search
022800980910     c                   end
022900040114      *  Controlli solo se non sono in visualizzaz. o cancellaz.
023000040114if  1C                   if        not *in05 and OPZ <> '04'
023100951002     C                   EXSR      CTR01
023200951002     C   28              GOTO      FINVD1
023300040114     c                   end
023400031120      *  aggiorno
023500031119     c                   if        *inkf
023600031119     C                   EXSR      aggio
023700031119     c                   exsr      fine
023800031119     c                   end
023900031119     c                   end
024000951002      *
024100951002     C     FINVD1        ENDSR
024200951002?     *--------------------------------------------------------------*
024300951002?     *  CTR01: Controlli prima videata                              *
024400951002?     *--------------------------------------------------------------*
024500951002     C     CTR01         BEGSR
024600031121     c                   clear                   msgv
024700031204     c                   seton                                        33
024800031119     c                   do
024900031119      *  Controllo tabella CAF
025000031119     c                   clear                   v1dtab
025100031119     c                   if        v1ctab =  ' '
025200031120     c                   seton                                        49
025300031119     c                   leave
025400031119     c                   else
025500031119     c                   movel     v1ctab        tbeke1
025600031203     c                   eval      tbecod='CAF'
025700031120     c     ktbe          chain     tntbe01l
025800031120     c                   if        not %found(tntbe01l) or tbeatb = 'A'
025900031119     c                   seton                                        49
026000031120     c                   leave
026100031119     c                   else
026200031119     c                   movel     tbeuni        dcaf
026300031119     c                   movel     §cafdes       v1dtab
026400031201     c                   movel     §caftip       v1ctip
026500031119     c                   end
026600031119     c                   end
026700031201     c* decodicfico tipo anagrafico
026800031201     c                   select
026900031201     c                   when      v1ctip = 'A'
027000031201     c                   eval      v1dtip = 'AUTOTRASPORTATORI'
027100031201     c                   when      v1ctip = 'C'
027200031201     c                   eval      v1dtip = 'COOPERATIVE'
027300031201     c                   when      v1ctip = 'D'
027400031201     c                   eval      v1dtip = 'AFFLUENZE/DEFLUENZE'
027500031201     c                   endsl
027600031120     c* data prestazione
027700031120     c                   if        v1cdti = 0
027800031120     C                   SETON                                        47
027900040107     C                   leave
028000031120     C                   ELSE
028100031120     c     *eur          test(d)                 v1cdti                 47
028200031120     c                   if        *in47
028300031120     c     *dmy          test(d)                 v1cdti                 47
028400031120     c  n47*dmy          move      v1cdti        dataeur
028500031120     c                   else
028600031120     c                   move      v1cdti        dataeur
028700031120     c                   end
028800031120     c  n47              move      dataeur       v1cdti
028900031120     c                   end
029000031120     c* controllo che la data sia in un mese in cui si può contabilizzare
029100031120     c                   if        not *in47
029200031120     c                   extrct    dataeur:*m    mm                2 0
029300031120     c                   if        mes(mm) = ' '
029400031120     c                   seton                                        43
029500031203     c                   leave
029600031120     c                   end
029700031203     c                   move      dataeur       datapre
029800040107     c                   else
029900040107     c                   leave
030000031120     c                   end
030100031204     c* se l'importo del mese nella tabella è 0 sproteggo il campo dell'imp.
030200031204     c* totale
030300031204     c                   if        §cafqta = 'N' and imp(mm) = 0
030400031204     c                   setoff                                       33
030500031204     c                   end
030600031203     c* reperisco la tabella BLC/BLD/BLO
030700031203     c                   exsr      srtabblo
030800031203     c* controllo che l'INSERIMENTO delle prestazioni avvenga nel mese in
030900031203     c* cui si può contabilizzare fino alla data blocco contabilizzazione
031000031203     c                   move      *date         dataeur
031100031203     c                   move      dataeur       dataoggi
031200031203     c* oggi non può essere minore della data di protocollo
031300031203     c                   if        dataoggi < datapro
031400031203     c                   seton                                        48
031500031203     c                   leave
031600031203     c                   end
031700040107     c**********************************************************************
031800040107     c* esempio: data blocco           10/12/03
031900040107     c*          data oggi              1/12/03
032000040107     c*          range prestazioni      1/11/03 - 30/11/03
032100040107     c*          range data immissione  1/11/03 - 10/12/03
032200040107     c* esempio: data blocco           10/12/03
032300040107     c*          data oggi             15/12/03
032400040107     c*          range prestazioni      1/12/03 - 31/12/03
032500040107     c*          range data immissione  1/12/03 - 10/01/03
032600040107     c**********************************************************************
032700040107     c* data inizio
032800040107     c                   move      datablo       comodo           10
032900040107     c                   move      '01'          comodo
033000040107     c                   move      comodo        dataini
033100061206     c* se oggi è < data blocco il range slitta indietro di un mese solo se
033200061206     c* anno protocollo <> da anno blocco
033300061206     c                   EXTRCT    datablo:*y    annoblo
033400061206     c                   EXTRCT    datapro:*y    annopro
033500140307     c*                  if        dataoggi<=datablo and annopro<>annoblo
033600140307     c                   if        dataoggi<=datablo
033700040107     c                   subdur    1:*m          dataini
033800040107     c                   end
033900040107     c* la data fine
034000031203     c     dataini       adddur    1:*m          datafin
034100031203     c                   subdur    1:*d          datafin
034200040107     c* la data prestazione deve essere in questi limiti
034300040107     c                   if        datapre>=dataini and datapre<=datafin
034400040107     c                   else
034500040107     c                   seton                                        47
034600040107     c                   leave
034700040107     c                   end
034800040107     c* oggi deve essere maggiore della data inizio e minore della data fine
034900040112     c* più i 15 gg ipotetici
035000040107     c                   eval      datacom = datafin
035100040112     c                   adddur    15:*d         datacom
035200040107     c                   if        dataoggi>=dataini and datapre<=datacom
035300040107     c                   else
035400040107     c                   seton                                        47
035500040107     c                   leave
035600040107     c                   end
035700031119      *  Controllo codice autotrasportatore
035800031119     c                   clear                   v1dpdr
035900031120     c                   if        v1cpdr =  0
036000031120     c                   seton                                        40
036100031120     c                   leave
036200031120     c                   else
036300031120     c* controllo il p.o.
036400140122     c                   movel     v1cpdr        com03
036500031120     c                   if        dutpous <> '046'
036600031120     c                   if        com03 <> dutpous
036700031120     c                   seton                                        40
036800031120     c                   leave
036900031120     c                   end
037000031120     c                   end
037100140122     c* chiodo momentaneo solo filiali 93 141 173 132
037200140122     c                   if        v1ctab = 'SPUC' and
037300140122     c                             com03 <> '093' and
037400140122     c                             com03 <> '141' and
037500140122     c                             com03 <> '132' and
037600140122     c                             com03 <> '173'
037700140122     c                   seton                                        40
037800140122     c                   leave
037900140122     c                   end
038000140122     c
038100031120     c*
038200031120     c     kapd          chain     fiapd01l
038300031120     c                   if        not %found(fiapd01l)
038400031119     c                   seton                                        40
038500031119     c                   leave
038600031119     c                   else
038700141124      *
038800141124ab   c                   eval      Targa_da_anagrafica = *blank
038900150107ab   c                   eval      Targa_uguale_a_anagrafica = *blank
039000141124      *
039100150107      *  Solo per PUBBLICITA' e AUTISTI
039200150107      *    adesso rileva sempre la targa
039300150107      *   ....e NON solo se il campo era vuoto
039400150107     c                   if        v1ctip = 'A'
039500140122     c                             and v1ctab = 'PUBC'
039600150107     c********                     and v1cnot = ' '
039700141124      *
039800040112     c     v1cpdr        setll     aitra03l
039900141124      *
040000040112     c                   do        *hival
040100040112     c     v1cpdr        reade     aitra03l
040200040112     c                   if        %eof(aitra03l)
040300040112     c                   leave
040400040112     c                   end
040500040112     c* controllo che non sia disaccreditato e che ne esista uno solo
040600040112     c                   if        tradfi <> 0
040700040112     c                   iter
040800040112     c                   end
040900040112     c*
041000150107     c*  Imposta la targa se il campo è vuoto
041100150107     c                   if            v1cnot = ' '
041200031127     c                   eval      v1cnot = 'Targa ' + trataa
041300141124ab   c                   eval      Targa_da_anagrafica = 'S'
041400150107     c                   end
041500150107      *
041600150107      *  Controlla se sul campo note c'è la targa dell'anagrafica
041700150107     c     trataa        scan      v1cnot
041800150107     c                   if        %Found
041900150107ab   c                   eval      Targa_uguale_a_anagrafica = 'S'
042000150107     c                   end
042100150107      *
042200150107     c                   leave
042300150107      *
042400040112     c                   enddo
042500141125ab    *----
042600141125ab    * Controllo da eseguire solo per AUTISTI e per il tipo addebito PUBBLICITA
042700141125ab    * se nel campo Nota la Targa non è impostata dall'anagrafica AITRA
042800141125ab    *  Devono esserci almeno 6 caratteri alfanumerici esclusi simboli come
042900141125ab    *  punti, barre etc.
043000150107ab    *  Esegue il controllo anche se da Anagrafica hanno modificato la Targa
043100141125ab   c                   if        Targa_da_anagrafica <>'S'
043200150107     c                               or
043300150107ab   c                             Targa_da_anagrafica  ='S'   and
043400150107ab   c                             Targa_uguale_a_anagrafica <> 'S'
043500141125ab    *
043600141125ab   c                   clear                   lungh_targa       3 0
043700141125ab   c                   z-add     1             da                1 0
043800150107ab    *
043900150107      * se inizia con Targa: deve tenerne conto nel controllo
044000150107ab   c                   if        %subst(v1cnot:1:6) = 'TARGA ' or
044100150107ab   c                             %subst(v1cnot:1:6) = 'Targa '
044200150107ab   c                   eval          %subst(v1cnot:1:6) = 'Targa '
044300150107      * da Dopo lo Spazio  x trovare i caratteri non Corretti
044400141125ab   c                   z-add     7             da
044500141125ab   c                   end
044600141125ab    *
044700150107ab    * Cerca il primo carattere non previsto per la Targa:
044800150107ab   c     Chars_targa   check     v1cnot:da     finoa             3 0
044900150107     c                   if        not %Found
045000150107ab   c                   eval      finoa = %len(%trim(v1cNOT))
045100150107     c                   end
045200150107      *
045300150107      *   adesso conosce la lunghezza della Targa immessa  e non deve essere < 6
045400150107ab   c                   eval      lungh_targa = finoa - da
045500150107ab   c                   IF        lungh_targa < 6
045600141125ab   C                   SETON                                        45
045700141125ab   c                   end
045800141125ab    *
045900141125ab   c                   end
046000150107ab   c                   end
046100141125ab    *----
046200141124      *
046300100223     c                   movel     apdrsf        v1dpdr
046400031119     c                   movel     apdcsf        v1csoc
046500031119     c                   move      *all'0'       v1cfor
046600031119     c                   move      apdksc        v1cfor
046700040112     c                   end
046800031119     c                   end
046900031120      *  decodifico codice società
047000101116     c**** v1csoc        chain     ansif01l
047100101116     c****               if        not %found(ansif01l) or
047200101116     c****                         (%found(ansif01l) and SIFDTFIVL <>
047300101116     c****                         *loval)
047400101116     c****               seton                                        50
047500101116     c****               else
047600101116     c****               movel     sifdesbrev    v1dsoc
047700101116     c****               end
047800101116      *
047900101116      **
048000101116     c                   clear                   v1dsoc
048100101116     c                   exsr      decod_societa
048200101116      *
048300031120      *  Decodifico codice fornitore
048400031119     C                   clear                   v1dfor
048500031119     C                   move      V1Cfor        kksc
048600980910     c                   movel     *blank        sogdes
048700980910     C     Kfrn          CHAIN     anfrn01l                           30
048800980910     C  N30Krco          CHAIN     anrco01l                           30
048900980910     C  N30rcosogg       CHAIN     ansog01l                           30
049000031119     C                   MOVEL(p)  sogdes        v1dfor
049100031120      *  Controllo il numero dei mesi da moltiplicare solo in immissione
049200031121     C                   IF        §cafqta = 'S' and v1cnrm = 0 and *in06
049300031120     C                   SETON                                        41
049400031119     C                   leave                                                  Data iniz.>fina.
049500951002     C                   END
049600031120     C                   IF        §cafqta = 'N' and v1cnrm <>0
049700031120     C                   SETON                                        41
049800031120     C                   leave                                                  Data iniz.>fina.
049900031120     C                   END
050000031120     C                   IF        v1cnrm <> 0 and v1cnrm > mm
050100031120     C                   SETON                                        41
050200031120     C                   leave                                                  Data iniz.>fina.
050300031120     C                   END
050400031121     C*
050500031121     c                   if        v1cnrm <> 0
050600031121     c     imp(mm)       mult      v1cnrm        v1cimp
050700031121     c                   else
050800031204     c* se l'importo del mese nella tabella è 0 obbligatorio importo totale
050900040107     c                   if        imp(mm) = 0 and
051000040107     c                             v1cimp = 0
051100031204     c                   seton                                        44
051200031204     c                   leave
051300031204     c                   end
051400140121     c                   if        imp(mm) <>0 and
051500140121     c                             v1cimp = 0
051600140121     c                   eval      v1cimp = imp(mm)
051700140121     c                   end
051800031125     c                   end
051900031121     c* se sono in modifica avviso che se si aggiorna l'importo cambierà
052000031121     c                   if        opz = '02' and v1cimp <> v1cimps
052100031121     c                   eval      msgv = 'Se si conferma l''aggionamento -
052200031121     c                             l''importo verrà cambiato'
052300031121     c                   end
052400031121     c* controllo che non esista già il record
052500031121     c                   exsr      srctrrec
052600031119     C                   ENDdo
052700031120     c*
052800031120     c                   if        *in40 or *in41 or *in47 or *in49 or *in42
052900080108     c                             or *in43 or *in48 or *in44 or *in50
053000141124ab   c                             or *in45
053100031120     c                   seton                                        28
053200031120     c                   end
053300951003      *
053400031119     C                   ENDSR
053500020404     C************************************************************
053600031203     C* tabella blocchi
053700020404     C************************************************************
053800031203     C     srtabblo      BEGSR
053900031203      *
054000031203     c                   clear                   datablo
054100031203     c                   clear                   datapro
054200031203     c                   eval      tbeke1 = '1'
054300031203     c                   select
054400031203     c                   when      v1ctip = 'A'
054500031203     c                   eval      tbecod = 'BLC'
054600031203     c                   when      v1ctip = 'C'
054700031203     c                   eval      tbecod = 'BLO'
054800031203     c                   when      v1ctip = 'D'
054900031203     c                   eval      tbecod = 'BLD'
055000031203     c                   endsl
055100031203     c     ktbe          chain     tntbe01l
055200031203     c                   if        %found(tntbe01l) and tbeatb = ' '
055300031203     c                   select
055400031203     c                   when      v1ctip = 'A'
055500031203     c                   eval      dblc = tbeuni
055600031203     c                   move      §blcdbl       datablo
055700031203     c                   move      §blcdtp       datapro
055800031203     c                   when      v1ctip = 'C'
055900031203     c                   eval      dblo = tbeuni
056000031203     c                   move      §blodbl       datablo
056100031203     c                   move      §blodtp       datapro
056200031203     c                   when      v1ctip = 'D'
056300031203     c                   eval      dbld = tbeuni
056400031203     c                   move      §blddbl       datablo
056500031203     c                   move      §blddtp       datapro
056600031203     c                   endsl
056700031203     c                   end
056800031203      *
056900031203     C                   ENDSR
057000031203     C************************************************************
057100031203     C* RICERCHE
057200031203     C************************************************************
057300031203     C     SEARCH        BEGSR
057400980910     C* determino Riga/Colonna del cursore
057500980910     C                   MOVE      nrg           R$$
057600980910     C                   MOVE      ncl           C$$
057700980910     C                   Z-ADD     RIRI          CURR
057800980910     C                   Z-ADD     COCO          CURC
057900980910     C*
0580009809101    C                   SELECT
058100031119     C* FMTD1  - ricerca autotrasp.
058200031120    >C                   WHEn      h1nmfl = 'V1CPDR'
058300031119     C                   clear                   fnlv24ds
058400031120     c* imposto il p.o.
058500031120     c                   if        dutpous <> '046'
058600031120     C                   MOVEL     dutpou        d24fil
058700031120     c                   end
058800031120     C                   MOVEL     v1ctip        d24tip
058900031119     C                   MOVEL     'R'           d24FLG
059000031119     C                   MOVEL(p)  fnlv24ds      KPJBU
059100031119     C                   CALL      'FNLV24R'
059200031119     C                   PARM                    KPJBA
059300031119     C                   MOVEL     KPJBU         fnlv24ds
059400031119     c                   move      d24pdr        v1cpdr
059500031119     c                   movel     d24rsc        v1dpdr
059600031119     C* FMTD1  - ricerca tabella prestazioni
059700031119    >C     H1NMFL        WHEneq    'V1CTAB'
059800031119     C                   clear                   Tibs02DS
059900031119     C                   eval      t02tla = 'L'
060000031119     C                   movel     'R'           t02mod
060100031119     C                   movel     'CAF'         t02cod
060200031119     C                   CALL      'TIBS02R'
060300031119     C                   parm                    KPJBA
060400031119     C                   parm                    TIBS02DS
060500031119    2C                   IF        T02err = *blanks
060600031119     c                   movel     t02ke1        v1ctab
060700031119     c                   movel     t02uni        dcaf
060800031119     c                   movel     §cafdes       v1dtab
060900031119     c                   end
061000020510     C                   ENDsl
061100980910     C* imposto pos. del cursore
061200980910     C                   Z-ADD     CURR          H1RIGA
061300980910     C                   Z-ADD     CURC          H1COLO
061400980910     C                   ENDsr
061500980910     C*----------------------------------------------------*
061600031120     C* aggiornamento
061700980910     C*----------------------------------------------------*
061800031120     C     aggio         BEGSR
061900031121     c* inserimento, copia, modifica
062000031121     c                   if        *in06 or opz = '02'
062100031120     c                   clear                   fiott000
062200031120     C                   eval      ottpdr = V1CPDR
062300031120     C                   eval      ottsoc = V1Csoc
062400031120     C                   eval      ottcdf = V1Cfor
062500031120     C                   move      v1cdti        dataeur
062600031120     C                   move      dataeur       dataiso
062700031120     C                   move      dataiso       ottddc
062800040113     C                   eval      ottqta = V1Cnrm
062900040113     C                   eval      otttim = V1Cimp
063000040113     C                   eval      ottimp = imp(mm)
063100031120     C                   eval      otttab = v1ctab
063200031120     C                   eval      ottnot = v1cnot
063300031120     C                   eval      otttip = v1ctip
063400031121     c                   end
063500031121     c                   select
063600031121     c                   when      *in06
063700031120     c                   write     fiott000
063800031121     c                   when      opz = '02'
063900031121     c                   update    fiott000
064000031121     c                   when      opz = '04'
064100031121     c                   delete    fiott000
064200031121     c                   endsl
064300980910     C*
064400031120     C                   ENDsr
064500951006?     *--------------------------------------------------------------*
064600031120?     * srctrrec - controllo record duplicato                        *
064700951006?     *--------------------------------------------------------------*
064800031120     C     srctrrec      BEGSR
064900140123     c                   move      v1cdti        dataeur
065000140123     c                   extrct    dataeur:*Y    year              4 0
065100140122     c*                  move      dataeur       dataiso
065200140123     c                   move      00010101      com08i
065300140122     c                   movel     year          com08i            8 0
065400140123     c                   move      00011231      com08f
065500140122     c                   movel     year          com08f            8 0
065600951006      *
065700031120     C/EXEC SQL
065800031121     C+ DECLARE A1 CURSOR FOR SELECT rrn(fiott00f) FROM fiott00F WHERE
065900031121     C+ otttab = :v1ctab and ottddc between :com08i and :com08f and
066000031121     C+ ottpdr = :v1cpdr and ottsoc = :v1csoc and ottcdf = :v1cfor
066100031120     C/END-EXEC
066200031120      *
066300031120     C/EXEC SQL
066400031120     C+ OPEN A1
066500031120     C/END-EXEC
066600031120      *
066700031121     C                   DO        *hival
066800031120     C/EXEC SQL
066900031121     C+ FETCH  next FROM A1 INTO :ottrrn
067000031120     C/END-EXEC
067100031121     C* controllo numero relativo di record se sono in modifica
067200031121     C                   if        SqlCod=0
067300031121     C                   if        opz='02' and rrn<>ottrrn
067400031121     c                   seton                                        42
067500031121     c                   leave
067600031121     c                   end
067700031121     C                   if        *in06
067800031121     c                   seton                                        42
067900031121     c                   leave
068000031121     c                   end
068100031121     C                   else
068200031121     c                   leave
068300031121     C                   END
068400031121     C                   ENDDO
068500031120      *
068600031120     C/EXEC SQL
068700031120     C+ CLOSE A1
068800031120     C/END-EXEC
068900031120     C*
069000031120     C                   ENDSR
069100031120?     *--------------------------------------------------------------*
069200031120?     * fine: Operazioni finali
069300031120?     *--------------------------------------------------------------*
069400031120     C     fine          BEGSR
069500031120     c*
069600031120     c                   clear                   cmd
069700031120     c                   select
069800031120     c                   when      *inkc
069900031120     c                   move      '03'          cmd
070000031120     c                   when      *inkl
070100031120     c                   move      '12'          cmd
070200031120     c                   when      *inkf
070300031120     c                   move      '06'          cmd
070400031120     c                   endsl
070500031120     c                   movel(p)  param         kpjbu
070600031120     c                   seton                                        lr
070700101116      *
070800101116     ** Chiudo il programma.
070900101116     C                   CALL      'TIBSSOCR'
071000101116     C                   PARM      'FINALIZE'    prmRqsOpCode
071100101116     C                   PARM                    prmRpyOpCode
071200101116     C                   PARM                    prmRpyIdMsg
071300101116
071400031120     c                   return
071500031120     C*
071600031120     C                   ENDSR
071700031120?     *--------------------------------------------------------------*
071800031120?     * *INZSR: Operazioni iniziali                                  *
071900031120?     *--------------------------------------------------------------*
072000031120     C     *INZSR        BEGSR
072100031120      *
072200951006     C     *ENTRY        PLIST
072300951006     C                   PARM                    KPJBA
072400031119     C                   MOVEL     KPJBU         PARAM
072500101116     C*
072600101116     ** Inizializzo il programma.
072700101116     C                   CALL      'TIBSSOCR'
072800101116     C                   PARM      'INIT'        prmRqsOpCode
072900101116     C                   PARM                    prmRpyOpCode
073000101116     C                   PARM                    prmRpyIdMsg
073100101116
073200031120     c     *dtaara       define    §azute        azuteds
073300031120     c     *dtaara       define    §datiute      ddatiute
073400031120     C                   in(E)     *dtaara
073500031120     C                   IF        %Error  or  RSUT = *blanks
073600031120     C                   call      'TIBS34R'
073700031120     C                   parm                    Tibs34Ds
073800031120     C                   in        *dtaara
073900031120     c                   ENDIF
074000031120     c                   movel     dutpou        dutpous
074100031120     c*
074200031120     c                   select
074300031121     c                   when      opz = '02'
074400031121     c                   eval      decopz = '   MODIFICA'
074500031121     c                   when      opz = '03'
074600031121     c                   eval      decopz = '     COPIA'
074700031120     c                   when      opz = '04'
074800031120     c                   eval      decopz = '   CANCELLA'
074900031120     c                   when      opz = '05'
075000031120     c                   eval      decopz = '  VISUALIZZA'
075100031120     c                   when      opz = '06'
075200031120     c                   eval      decopz = ' INSERIMENTO'
075300031120     c                   endsl
075400100729     C                   movel     '000400'      forita            6
075500020423     C     Kfrn          KLIST
075600031119     C                   KFLD                    v1csoc
075700020423     C                   KFLD                    kksc
075800980910     C     Krco          KLIST
075900031119     C                   KFLD                    v1csoc
076000020417     C                   KFLD                    forita
076100980910     C                   KFLD                    KKSC
076200031119     C     Ktbe          KLIST
076300031119     C                   KFLD                    tbecod
076400031119     C                   KFLD                    tbeke1
076500031119     C     Kapd          KLIST
076600031119     C                   KFLD                    v1ctip
076700031120     C                   KFLD                    v1cpdr
076800951006      *  Definisco variabili
076900980910     C                   Z-ADD     0             CURR              2 0
077000980910     C                   Z-ADD     0             CURC              2 0
077100980910     c                   endsr
077200101116     C**************************************************************************
077300101116     C* decodifica la società
077400101116     C************************************************************
077500101116     C     Decod_societa begSR
077600101116      *
077700101116     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
077800101116     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
077900101116     C********           RESET                   tibsSocI0
078000101116     C                   EVAL      tibsSocI0.idSocieta = v1csoc
078100101116     C
078200101116     C                   CALL      'TIBSSOCR'
078300101116     C                   PARM      'GETANAGRAF'  prmRqsOpCode
078400101116     C                   PARM      *BLANK        prmRpyOpCode
078500101116     C                   PARM      *ZERO         prmRpyIdMsg
078600101116     C                   PARM      'TIBSSOCI0'   prmRqsFormato
078700101116     C                   PARM                    tibsSocI0
078800101116     C                   PARM                    prmRqsDataSize
078900101116     C                   PARM      'TIBSSOCO0'   prmRpyFormato
079000101116     C                   PARM                    tibsSocO0
079100101116     C                   PARM                    prmRpyDataSize
079200101116     c                   if         PRMRPYIDMSG >= 0
079300101116     c                              and TIBSSOCO0.IDSOCIETA <> *blank
079400101116     c                   eval       v1dsoc  = tibsSocO0.RAGSOCIALE
079500101116     c                   else
079600101116     c                   seton                                        50
079700101116     c                   end
079800101116      *
079900101116     C                   ENDSR
080000101116     C*--------------------------------------------------------------------
