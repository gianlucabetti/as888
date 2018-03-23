000100980910     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200951002?     *--------------------------------------------------------------*
000300951002      *     *----------------------------------------------*         *
000400031119      *      prestazioni occasionali autofattura fornitori           *
000500031119      *     *----------------------------------------------*         *
000600031120     Ffiott00f  uF a E             DISK
000700031120     Ffiapd01l  IF   E           K DISK
000800031127     Faitra03l  IF   E           K DISK
000900031119     Fansog01l  IF   E           K DISK
001000101116     F*****ansif01l  IF   E           K DISK
001100020417     Fanfrn01l  IF   E           K DISK
001200020417     Fanrco01l  IF   E           K DISK
001300020417     Ftntbe01l  IF   E           K DISK
001400031119     Fficn94D   CF   E             WORKSTN
001500980910?     *--------------------------------------------------------------*
001600980910?     *  DS                                                          *
001700980910?     *--------------------------------------------------------------*
001800980910     D DSFMT           DS
001900980910     D  $TASTO               369    369
002000980910     D  NRG                  370    370
002100980910     D  NCL                  371    371
002200980910     D* posizione cursore
002300980910     D CURSOR          DS
002400980910     D  RIRI                   1      2B 0 INZ
002500980910     D  R$$                    2      2
002600980910     D  COCO                   3      4B 0 INZ
002700980910     D  C$$                    4      4
002800031119     D PARAM           DS
002900031119     D  RRN                           9S 0
003000031119     D  OPZ                           2
003100031120     D  cmd                           2
003200951002     D KPJBA         E DS
003300031119     D tibs02ds      E DS
003400031119     D fnlv24ds      E DS
003500031120     D fiottds       E DS                  extname(fiott00f)
003600031120     D ddatiute      e ds
003700031120     D azuteds       e ds                  extname(AZUTE00F)
003800031120     D tibs34ds      E DS                  inz
003900031119     D dcaf          E DS
004000031203     D dblc          E DS
004100031203     D dbld          E DS
004200031203     D dblo          E DS
004300031119     D Ptr             S               *
004400031119     D                                     INZ(%ADDR(§CAFMES1))
004500031120     D mes             S                   LIKE(§CAFMES1)
004600031119     D                                     DIM(12)
004700031119     D                                     BASED(Ptr)
004800031119     D Ptr1            S               *
004900031119     D                                     INZ(%ADDR(§CAFIMP1))
005000031120     D imp             S                   LIKE(§CAFIMP1)
005100031119     D                                     DIM(12)
005200031119     D                                     BASED(Ptr1)
005300031119     D* Reperimento nome PGM
005400031119     D                SDS
005500031119     D  nompgm           *PROC
005600990506     D*-------------
005700031119     D com10           s             10    inz('0000000000')
005800031203     Ddataoggi         s               d   datfmt(*iso)
005900031203     Ddatapre          s               d   datfmt(*iso)
006000031203     Ddatapro          s               d   datfmt(*iso)
006100031203     Ddatablo          s               d   datfmt(*iso)
006200031203     Ddataini          s               d   datfmt(*iso)
006300031203     Ddatafin          s               d   datfmt(*iso)
006400031203     Ddataiso          s               d   datfmt(*iso)
006500031119     Ddataeur          s               d   datfmt(*eur)
006600040107     Ddatacom          s               d   datfmt(*iso)
006700031119     Dkksc             s                   like(v1cfor)
006800061206     dannopro          s              4  0
006900061206     dannoblo          s              4  0
007000031120     Ddutpous          s              3
007100031120     Dcom03            s              3
007200031121     Dottrrn           s             10  0 inz
007300031121     Dv1cimps          s                   like(v1cimp)
007400141124     D Targa_da_anagrafica...
007500141124     D                 s              1    inz
007600150107     D Targa_uguale_a_anagrafica...
007700150107     D                 s              1    inz
007800141124     D Chars_Targa...
007900141124     D                 C                   '0123456789QWERTYUIOPASDFGHJKLZXCVBN-
008000141124     D                                     M'
008100101116     ***************************************************************************
008200101116     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
008300101116     D ESITO_OK...
008400101116     D                 C                   0
008500101116     ***************************************************************************
008600101116     **
008700101116     ** Dichiarazione prototipi procedure esterne.
008800101116     **
008900101116     ***************************************************************************
009000101116      /DEFINE DFTACTGRP_YES
009100101116     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
009200101116      /UNDEFINE DFTACTGRP_YES
009300101116     ***************************************************************************
009400101116     **
009500101116     ** Definizione strutture dati.
009600101116     **
009700101116      **************************************************************************
009800101116     D tibsSocI0...
009900101116     D               E DS                  QUALIFIED
010000101116     D                                     INZ
010100101116     D tibsSocO0...
010200101116     D               E DS                  QUALIFIED
010300101116     D                                     INZ
010400101116     ***************************************************************************
010500101116     **
010600101116     ** Definizione variabili modulo/programma.
010700101116     **
010800101116     ***************************************************************************
010900101116     D prmRqsOpCode...
011000101116     D                 S             10A
011100101116     D prmRpyOpCode...
011200101116     D                 S             10A
011300101116     D prmRpyIdMsg...
011400101116     D                 S             10I 0
011500101116     D prmRqsFormato...
011600101116     D                 S             10A
011700101116     D prmRqsDataSize...
011800101116     D                 S             10I 0
011900101116     D prmRpyFormato...
012000101116     D                 S             10A
012100101116     D prmRpyDataSize...
012200101116     D                 S             10I 0
012300101116
012400951002?     *--------------------------------------------------------------*
012500951002?     *  CICLO PRINCIPALE                                            *
012600951002?     *--------------------------------------------------------------*
012700951002      *  Impostazione parametri prima videata
012800951002     C                   EXSR      INZ01
012900951002      *  Loop gestione videata
013000031120     C                   DO        *hival
013100951002     C                   EXSR      GESD01
013200031120     C                   ENDdo
013300951002?     *--------------------------------------------------------------*
013400951002?     *  INZ01: Inizializzazione prima videata                       *
013500951002?     *--------------------------------------------------------------*
013600951002     C     INZ01         BEGSR
013700031119      *INSERIMENTO
013800031120     c                   if        opz = '06' or opz = '03'
013900031120     c                   seton                                        06
014000031120     c                   end
014100031119      *VISUALIZZAZ.
014200031119     C     OPZ           COMP      '05'                                   05
014300031119     C                   SETOFF                                       28
014400031119     c                   if        rrn = 0
014500031119     C                   z-add     0             V1CPDR
014600031201     C                   movel     *blanks       V1ctip
014700031120     C                   movel     *blanks       V1dPDR
014800031119     C                   movel     *blanks       V1Csoc
014900031119     C                   movel     *blanks       V1dsoc
015000031119     C                   movel     *blanks       V1Cfor
015100031119     C                   movel     *blanks       V1dfor
015200031120     C                   Z-ADD     0             V1Cnft
015300031120     C                   Z-ADD     0             V1Cdft
015400031120     C                   Z-ADD     0             V1Cdti
015500031120     C                   Z-ADD     0             V1Cnrm
015600031119     C                   Z-ADD     0             V1Cimp
015700031121     C                   Z-ADD     0             V1Cimps
015800031119     C                   MOVEL     *blanks       v1ctab
015900031119     C                   MOVEL     *blanks       v1dtab
016000031119     C                   MOVEL     *blanks       v1cnot
016100031119     c                   else
016200031119     c     rrn           chain     fiott00f
016300031119     c                   if        %found(fiott00f)
016400031120     C                   MOVEL     otttab        v1ctab
016500031120     C                   MOVEL     ottnot        v1cnot
016600031120     C                   MOVEL     otttip        v1ctip
016700031119     C                   z-add     ottpdr        V1CPDR
016800031119     C                   movel     ottsoc        V1Csoc
016900031119     C                   movel     ottcdf        V1Cfor
017000031120     C                   move      ottddc        dataiso
017100031120     C                   move      dataiso       dataeur
017200031119     C                   move      dataeur       V1Cdti
017300031120     c                   if        not *in06
017400040113     C                   Z-ADD     ottqta        V1Cnrm
017500040113     C                   Z-ADD     otttim        V1Cimp
017600031121     C                   Z-ADD     otttim        V1Cimps
017700031120     c                   if        ottdft <> 0
017800031120     C                   move      ottdft        dataiso
017900031120     C                   move      dataiso       dataeur
018000031120     C                   move      dataeur       V1Cdft
018100031120     c                   end
018200031120     c                   Z-ADD     ottnff        V1Cnft
018300031120     c                   end
018400031119     c                   exsr      ctr01
018500031121     c* reimposto l'importo originale xchè nella subroutine ctr01
018600040107     c* potrebbe essere stato variato
018700031121     c                   if        otttim <> 0
018800031121     C                   Z-ADD     otttim        V1Cimp
018900031121     c                   clear                   msgv
019000031119     c                   end
019100031121     c                   end
019200031119     c                   end
019300031121     C                   movea     com10         *in(40)
019400080108     c                   setoff                                       2850
019500951002     C*
019600951002     C                   ENDSR
019700951002?     *--------------------------------------------------------------*
019800951002?     *  GESD01: Gestione prima videata                              *
019900951002?     *--------------------------------------------------------------*
020000951002     C     GESD01        BEGSR
020100951002      *
020200031119     c* visualizzazione
020300031119if  1C                   if        *in05 OR OPZ = '04'
020400031119     C                   write     y350d01
020500031119     C                   exfmt     PROTECT
020600031119x   1C                   else
020700031120     c                   if        *in28
020800031120     c                   setoff                                       28
020900031120     C                   write     y350d01
021000031120     c                   seton                                        28
021100031120     c                   end
021200031119     C                   EXFMT     y350d01
021300031119     c                   end
021400031119     c*
021500080108     C                   SETOFF                                       2850
021600031119     C                   movea     com10         *in(40)
021700951002      *  Fine Lavoro
021800951002     C     *INKC         IFEQ      '1'
021900031120     C     *INKl         orEQ      '1'
022000031119     C                   exsr      fine
022100951002     C                   END
022200020403     C*  effettua ricerche per fornitore
022300031119     c                   if        not *in05
022400980910     c                   if        *inkd = *on
022500980910     c                   exsr      search
022600980910     c                   end
022700040114      *  Controlli solo se non sono in visualizzaz. o cancellaz.
022800040114if  1C                   if        not *in05 and OPZ <> '04'
022900951002     C                   EXSR      CTR01
023000951002     C   28              GOTO      FINVD1
023100040114     c                   end
023200031120      *  aggiorno
023300031119     c                   if        *inkf
023400031119     C                   EXSR      aggio
023500031119     c                   exsr      fine
023600031119     c                   end
023700031119     c                   end
023800951002      *
023900951002     C     FINVD1        ENDSR
024000951002?     *--------------------------------------------------------------*
024100951002?     *  CTR01: Controlli prima videata                              *
024200951002?     *--------------------------------------------------------------*
024300951002     C     CTR01         BEGSR
024400031121     c                   clear                   msgv
024500031204     c                   seton                                        33
024600031119     c                   do
024700031119      *  Controllo tabella CAF
024800031119     c                   clear                   v1dtab
024900031119     c                   if        v1ctab =  ' '
025000031120     c                   seton                                        49
025100031119     c                   leave
025200031119     c                   else
025300031119     c                   movel     v1ctab        tbeke1
025400031203     c                   eval      tbecod='CAF'
025500031120     c     ktbe          chain     tntbe01l
025600031120     c                   if        not %found(tntbe01l) or tbeatb = 'A'
025700031119     c                   seton                                        49
025800031120     c                   leave
025900031119     c                   else
026000031119     c                   movel     tbeuni        dcaf
026100031119     c                   movel     §cafdes       v1dtab
026200031201     c                   movel     §caftip       v1ctip
026300031119     c                   end
026400031119     c                   end
026500031201     c* decodicfico tipo anagrafico
026600031201     c                   select
026700031201     c                   when      v1ctip = 'A'
026800031201     c                   eval      v1dtip = 'AUTOTRASPORTATORI'
026900031201     c                   when      v1ctip = 'C'
027000031201     c                   eval      v1dtip = 'COOPERATIVE'
027100031201     c                   when      v1ctip = 'D'
027200031201     c                   eval      v1dtip = 'AFFLUENZE/DEFLUENZE'
027300031201     c                   endsl
027400031120     c* data prestazione
027500031120     c                   if        v1cdti = 0
027600031120     C                   SETON                                        47
027700040107     C                   leave
027800031120     C                   ELSE
027900031120     c     *eur          test(d)                 v1cdti                 47
028000031120     c                   if        *in47
028100031120     c     *dmy          test(d)                 v1cdti                 47
028200031120     c  n47*dmy          move      v1cdti        dataeur
028300031120     c                   else
028400031120     c                   move      v1cdti        dataeur
028500031120     c                   end
028600031120     c  n47              move      dataeur       v1cdti
028700031120     c                   end
028800031120     c* controllo che la data sia in un mese in cui si può contabilizzare
028900031120     c                   if        not *in47
029000031120     c                   extrct    dataeur:*m    mm                2 0
029100031120     c                   if        mes(mm) = ' '
029200031120     c                   seton                                        43
029300031203     c                   leave
029400031120     c                   end
029500031203     c                   move      dataeur       datapre
029600040107     c                   else
029700040107     c                   leave
029800031120     c                   end
029900031204     c* se l'importo del mese nella tabella è 0 sproteggo il campo dell'imp.
030000031204     c* totale
030100031204     c                   if        §cafqta = 'N' and imp(mm) = 0
030200031204     c                   setoff                                       33
030300031204     c                   end
030400031203     c* reperisco la tabella BLC/BLD/BLO
030500031203     c                   exsr      srtabblo
030600031203     c* controllo che l'INSERIMENTO delle prestazioni avvenga nel mese in
030700031203     c* cui si può contabilizzare fino alla data blocco contabilizzazione
030800031203     c                   move      *date         dataeur
030900031203     c                   move      dataeur       dataoggi
031000031203     c* oggi non può essere minore della data di protocollo
031100031203     c                   if        dataoggi < datapro
031200031203     c                   seton                                        48
031300031203     c                   leave
031400031203     c                   end
031500040107     c**********************************************************************
031600040107     c* esempio: data blocco           10/12/03
031700040107     c*          data oggi              1/12/03
031800040107     c*          range prestazioni      1/11/03 - 30/11/03
031900040107     c*          range data immissione  1/11/03 - 10/12/03
032000040107     c* esempio: data blocco           10/12/03
032100040107     c*          data oggi             15/12/03
032200040107     c*          range prestazioni      1/12/03 - 31/12/03
032300040107     c*          range data immissione  1/12/03 - 10/01/03
032400040107     c**********************************************************************
032500040107     c* data inizio
032600040107     c                   move      datablo       comodo           10
032700040107     c                   move      '01'          comodo
032800040107     c                   move      comodo        dataini
032900061206     c* se oggi è < data blocco il range slitta indietro di un mese solo se
033000061206     c* anno protocollo <> da anno blocco
033100061206     c                   EXTRCT    datablo:*y    annoblo
033200061206     c                   EXTRCT    datapro:*y    annopro
033300140307     c*                  if        dataoggi<=datablo and annopro<>annoblo
033400140307     c                   if        dataoggi<=datablo
033500040107     c                   subdur    1:*m          dataini
033600040107     c                   end
033700040107     c* la data fine
033800031203     c     dataini       adddur    1:*m          datafin
033900031203     c                   subdur    1:*d          datafin
034000040107     c* la data prestazione deve essere in questi limiti
034100040107     c                   if        datapre>=dataini and datapre<=datafin
034200040107     c                   else
034300040107     c                   seton                                        47
034400040107     c                   leave
034500040107     c                   end
034600040107     c* oggi deve essere maggiore della data inizio e minore della data fine
034700040112     c* più i 15 gg ipotetici
034800040107     c                   eval      datacom = datafin
034900040112     c                   adddur    15:*d         datacom
035000040107     c                   if        dataoggi>=dataini and datapre<=datacom
035100040107     c                   else
035200040107     c                   seton                                        47
035300040107     c                   leave
035400040107     c                   end
035500031119      *  Controllo codice autotrasportatore
035600031119     c                   clear                   v1dpdr
035700031120     c                   if        v1cpdr =  0
035800031120     c                   seton                                        40
035900031120     c                   leave
036000031120     c                   else
036100031120     c* controllo il p.o.
036200140122     c                   movel     v1cpdr        com03
036300031120     c                   if        dutpous <> '046'
036400031120     c                   if        com03 <> dutpous
036500031120     c                   seton                                        40
036600031120     c                   leave
036700031120     c                   end
036800031120     c                   end
036900140122     c* chiodo momentaneo solo filiali 93 141 173 132
037000140122     c                   if        v1ctab = 'SPUC' and
037100140122     c                             com03 <> '093' and
037200140122     c                             com03 <> '141' and
037300140122     c                             com03 <> '132' and
037400140122     c                             com03 <> '173'
037500140122     c                   seton                                        40
037600140122     c                   leave
037700140122     c                   end
037800140122     c
037900031120     c*
038000031120     c     kapd          chain     fiapd01l
038100031120     c                   if        not %found(fiapd01l)
038200031119     c                   seton                                        40
038300031119     c                   leave
038400031119     c                   else
038500141124      *
038600141124ab   c                   eval      Targa_da_anagrafica = *blank
038700150107ab   c                   eval      Targa_uguale_a_anagrafica = *blank
038800141124      *
038900150107      *  Solo per PUBBLICITA' e AUTISTI
039000150107      *    adesso rileva sempre la targa
039100150107      *   ....e NON solo se il campo era vuoto
039200150107     c                   if        v1ctip = 'A'
039300140122     c                             and v1ctab = 'PUBC'
039400150107     c********                     and v1cnot = ' '
039500141124      *
039600040112     c     v1cpdr        setll     aitra03l
039700141124      *
039800040112     c                   do        *hival
039900040112     c     v1cpdr        reade     aitra03l
040000040112     c                   if        %eof(aitra03l)
040100040112     c                   leave
040200040112     c                   end
040300040112     c* controllo che non sia disaccreditato e che ne esista uno solo
040400040112     c                   if        tradfi <> 0
040500040112     c                   iter
040600040112     c                   end
040700040112     c*
040800150107     c*  Imposta la targa se il campo è vuoto
040900150107     c                   if            v1cnot = ' '
041000031127     c                   eval      v1cnot = 'Targa ' + trataa
041100141124ab   c                   eval      Targa_da_anagrafica = 'S'
041200150107     c                   end
041300150107      *
041400150107      *  Controlla se sul campo note c'è la targa dell'anagrafica
041500150107     c     trataa        scan      v1cnot
041600150107     c                   if        %Found
041700150107ab   c                   eval      Targa_uguale_a_anagrafica = 'S'
041800150107     c                   end
041900150107      *
042000150107     c                   leave
042100150107      *
042200040112     c                   enddo
042300141125ab    *----
042400141125ab    * Controllo da eseguire solo per AUTISTI e per il tipo addebito PUBBLICITA
042500141125ab    * se nel campo Nota la Targa non è impostata dall'anagrafica AITRA
042600141125ab    *  Devono esserci almeno 6 caratteri alfanumerici esclusi simboli come
042700141125ab    *  punti, barre etc.
042800150107ab    *  Esegue il controllo anche se da Anagrafica hanno modificato la Targa
042900141125ab   c                   if        Targa_da_anagrafica <>'S'
043000150107     c                               or
043100150107ab   c                             Targa_da_anagrafica  ='S'   and
043200150107ab   c                             Targa_uguale_a_anagrafica <> 'S'
043300141125ab    *
043400141125ab   c                   clear                   lungh_targa       3 0
043500141125ab   c                   z-add     1             da                1 0
043600150107ab    *
043700150107      * se inizia con Targa: deve tenerne conto nel controllo
043800150107ab   c                   if        %subst(v1cnot:1:6) = 'TARGA ' or
043900150107ab   c                             %subst(v1cnot:1:6) = 'Targa '
044000150107ab   c                   eval          %subst(v1cnot:1:6) = 'Targa '
044100150107      * da Dopo lo Spazio  x trovare i caratteri non Corretti
044200141125ab   c                   z-add     7             da
044300141125ab   c                   end
044400141125ab    *
044500150107ab    * Cerca il primo carattere non previsto per la Targa:
044600150107ab   c     Chars_targa   check     v1cnot:da     finoa             3 0
044700150107     c                   if        not %Found
044800150107ab   c                   eval      finoa = %len(%trim(v1cNOT))
044900150107     c                   end
045000150107      *
045100150107      *   adesso conosce la lunghezza della Targa immessa  e non deve essere < 6
045200150107ab   c                   eval      lungh_targa = finoa - da
045300150107ab   c                   IF        lungh_targa < 6
045400141125ab   C                   SETON                                        45
045500141125ab   c                   end
045600141125ab    *
045700141125ab   c                   end
045800150107ab   c                   end
045900141125ab    *----
046000141124      *
046100100223     c                   movel     apdrsf        v1dpdr
046200031119     c                   movel     apdcsf        v1csoc
046300031119     c                   move      *all'0'       v1cfor
046400031119     c                   move      apdksc        v1cfor
046500040112     c                   end
046600031119     c                   end
046700031120      *  decodifico codice società
046800101116     c**** v1csoc        chain     ansif01l
046900101116     c****               if        not %found(ansif01l) or
047000101116     c****                         (%found(ansif01l) and SIFDTFIVL <>
047100101116     c****                         *loval)
047200101116     c****               seton                                        50
047300101116     c****               else
047400101116     c****               movel     sifdesbrev    v1dsoc
047500101116     c****               end
047600101116      *
047700101116      **
047800101116     c                   clear                   v1dsoc
047900101116     c                   exsr      decod_societa
048000101116      *
048100031120      *  Decodifico codice fornitore
048200031119     C                   clear                   v1dfor
048300031119     C                   move      V1Cfor        kksc
048400980910     c                   movel     *blank        sogdes
048500980910     C     Kfrn          CHAIN     anfrn01l                           30
048600980910     C  N30Krco          CHAIN     anrco01l                           30
048700980910     C  N30rcosogg       CHAIN     ansog01l                           30
048800031119     C                   MOVEL(p)  sogdes        v1dfor
048900031120      *  Controllo il numero dei mesi da moltiplicare solo in immissione
049000031121     C                   IF        §cafqta = 'S' and v1cnrm = 0 and *in06
049100031120     C                   SETON                                        41
049200031119     C                   leave                                                  Data iniz.>fina.
049300951002     C                   END
049400031120     C                   IF        §cafqta = 'N' and v1cnrm <>0
049500031120     C                   SETON                                        41
049600031120     C                   leave                                                  Data iniz.>fina.
049700031120     C                   END
049800031120     C                   IF        v1cnrm <> 0 and v1cnrm > mm
049900031120     C                   SETON                                        41
050000031120     C                   leave                                                  Data iniz.>fina.
050100031120     C                   END
050200031121     C*
050300031121     c                   if        v1cnrm <> 0
050400031121     c     imp(mm)       mult      v1cnrm        v1cimp
050500031121     c                   else
050600031204     c* se l'importo del mese nella tabella è 0 obbligatorio importo totale
050700040107     c                   if        imp(mm) = 0 and
050800040107     c                             v1cimp = 0
050900031204     c                   seton                                        44
051000031204     c                   leave
051100031204     c                   end
051200140121     c                   if        imp(mm) <>0 and
051300140121     c                             v1cimp = 0
051400140121     c                   eval      v1cimp = imp(mm)
051500140121     c                   end
051600031125     c                   end
051700031121     c* se sono in modifica avviso che se si aggiorna l'importo cambierà
051800031121     c                   if        opz = '02' and v1cimp <> v1cimps
051900031121     c                   eval      msgv = 'Se si conferma l''aggionamento -
052000031121     c                             l''importo verrà cambiato'
052100031121     c                   end
052200031121     c* controllo che non esista già il record
052300031121     c                   exsr      srctrrec
052400031119     C                   ENDdo
052500031120     c*
052600031120     c                   if        *in40 or *in41 or *in47 or *in49 or *in42
052700080108     c                             or *in43 or *in48 or *in44 or *in50
052800141124ab   c                             or *in45
052900031120     c                   seton                                        28
053000031120     c                   end
053100951003      *
053200031119     C                   ENDSR
053300020404     C************************************************************
053400031203     C* tabella blocchi
053500020404     C************************************************************
053600031203     C     srtabblo      BEGSR
053700031203      *
053800031203     c                   clear                   datablo
053900031203     c                   clear                   datapro
054000031203     c                   eval      tbeke1 = '1'
054100031203     c                   select
054200031203     c                   when      v1ctip = 'A'
054300031203     c                   eval      tbecod = 'BLC'
054400031203     c                   when      v1ctip = 'C'
054500031203     c                   eval      tbecod = 'BLO'
054600031203     c                   when      v1ctip = 'D'
054700031203     c                   eval      tbecod = 'BLD'
054800031203     c                   endsl
054900031203     c     ktbe          chain     tntbe01l
055000031203     c                   if        %found(tntbe01l) and tbeatb = ' '
055100031203     c                   select
055200031203     c                   when      v1ctip = 'A'
055300031203     c                   eval      dblc = tbeuni
055400031203     c                   move      §blcdbl       datablo
055500031203     c                   move      §blcdtp       datapro
055600031203     c                   when      v1ctip = 'C'
055700031203     c                   eval      dblo = tbeuni
055800031203     c                   move      §blodbl       datablo
055900031203     c                   move      §blodtp       datapro
056000031203     c                   when      v1ctip = 'D'
056100031203     c                   eval      dbld = tbeuni
056200031203     c                   move      §blddbl       datablo
056300031203     c                   move      §blddtp       datapro
056400031203     c                   endsl
056500031203     c                   end
056600031203      *
056700031203     C                   ENDSR
056800031203     C************************************************************
056900031203     C* RICERCHE
057000031203     C************************************************************
057100031203     C     SEARCH        BEGSR
057200980910     C* determino Riga/Colonna del cursore
057300980910     C                   MOVE      nrg           R$$
057400980910     C                   MOVE      ncl           C$$
057500980910     C                   Z-ADD     RIRI          CURR
057600980910     C                   Z-ADD     COCO          CURC
057700980910     C*
0578009809101    C                   SELECT
057900031119     C* FMTD1  - ricerca autotrasp.
058000031120    >C                   WHEn      h1nmfl = 'V1CPDR'
058100031119     C                   clear                   fnlv24ds
058200031120     c* imposto il p.o.
058300031120     c                   if        dutpous <> '046'
058400031120     C                   MOVEL     dutpou        d24fil
058500031120     c                   end
058600031120     C                   MOVEL     v1ctip        d24tip
058700031119     C                   MOVEL     'R'           d24FLG
058800031119     C                   MOVEL(p)  fnlv24ds      KPJBU
058900031119     C                   CALL      'FNLV24R'
059000031119     C                   PARM                    KPJBA
059100031119     C                   MOVEL     KPJBU         fnlv24ds
059200031119     c                   move      d24pdr        v1cpdr
059300031119     c                   movel     d24rsc        v1dpdr
059400031119     C* FMTD1  - ricerca tabella prestazioni
059500031119    >C     H1NMFL        WHEneq    'V1CTAB'
059600031119     C                   clear                   Tibs02DS
059700031119     C                   eval      t02tla = 'L'
059800031119     C                   movel     'R'           t02mod
059900031119     C                   movel     'CAF'         t02cod
060000031119     C                   CALL      'TIBS02R'
060100031119     C                   parm                    KPJBA
060200031119     C                   parm                    TIBS02DS
060300031119    2C                   IF        T02err = *blanks
060400031119     c                   movel     t02ke1        v1ctab
060500031119     c                   movel     t02uni        dcaf
060600031119     c                   movel     §cafdes       v1dtab
060700031119     c                   end
060800020510     C                   ENDsl
060900980910     C* imposto pos. del cursore
061000980910     C                   Z-ADD     CURR          H1RIGA
061100980910     C                   Z-ADD     CURC          H1COLO
061200980910     C                   ENDsr
061300980910     C*----------------------------------------------------*
061400031120     C* aggiornamento
061500980910     C*----------------------------------------------------*
061600031120     C     aggio         BEGSR
061700031121     c* inserimento, copia, modifica
061800031121     c                   if        *in06 or opz = '02'
061900031120     c                   clear                   fiott000
062000031120     C                   eval      ottpdr = V1CPDR
062100031120     C                   eval      ottsoc = V1Csoc
062200031120     C                   eval      ottcdf = V1Cfor
062300031120     C                   move      v1cdti        dataeur
062400031120     C                   move      dataeur       dataiso
062500031120     C                   move      dataiso       ottddc
062600040113     C                   eval      ottqta = V1Cnrm
062700040113     C                   eval      otttim = V1Cimp
062800040113     C                   eval      ottimp = imp(mm)
062900031120     C                   eval      otttab = v1ctab
063000031120     C                   eval      ottnot = v1cnot
063100031120     C                   eval      otttip = v1ctip
063200031121     c                   end
063300031121     c                   select
063400031121     c                   when      *in06
063500031120     c                   write     fiott000
063600031121     c                   when      opz = '02'
063700031121     c                   update    fiott000
063800031121     c                   when      opz = '04'
063900031121     c                   delete    fiott000
064000031121     c                   endsl
064100980910     C*
064200031120     C                   ENDsr
064300951006?     *--------------------------------------------------------------*
064400031120?     * srctrrec - controllo record duplicato                        *
064500951006?     *--------------------------------------------------------------*
064600031120     C     srctrrec      BEGSR
064700140123     c                   move      v1cdti        dataeur
064800140123     c                   extrct    dataeur:*Y    year              4 0
064900140122     c*                  move      dataeur       dataiso
065000140123     c                   move      00010101      com08i
065100140122     c                   movel     year          com08i            8 0
065200140123     c                   move      00011231      com08f
065300140122     c                   movel     year          com08f            8 0
065400951006      *
065500031120     C/EXEC SQL
065600031121     C+ DECLARE A1 CURSOR FOR SELECT rrn(fiott00f) FROM fiott00F WHERE
065700031121     C+ otttab = :v1ctab and ottddc between :com08i and :com08f and
065800031121     C+ ottpdr = :v1cpdr and ottsoc = :v1csoc and ottcdf = :v1cfor
065900031120     C/END-EXEC
066000031120      *
066100031120     C/EXEC SQL
066200031120     C+ OPEN A1
066300031120     C/END-EXEC
066400031120      *
066500031121     C                   DO        *hival
066600031120     C/EXEC SQL
066700031121     C+ FETCH  next FROM A1 INTO :ottrrn
066800031120     C/END-EXEC
066900031121     C* controllo numero relativo di record se sono in modifica
067000031121     C                   if        SqlCod=0
067100031121     C                   if        opz='02' and rrn<>ottrrn
067200031121     c                   seton                                        42
067300031121     c                   leave
067400031121     c                   end
067500031121     C                   if        *in06
067600031121     c                   seton                                        42
067700031121     c                   leave
067800031121     c                   end
067900031121     C                   else
068000031121     c                   leave
068100031121     C                   END
068200031121     C                   ENDDO
068300031120      *
068400031120     C/EXEC SQL
068500031120     C+ CLOSE A1
068600031120     C/END-EXEC
068700031120     C*
068800031120     C                   ENDSR
068900031120?     *--------------------------------------------------------------*
069000031120?     * fine: Operazioni finali
069100031120?     *--------------------------------------------------------------*
069200031120     C     fine          BEGSR
069300031120     c*
069400031120     c                   clear                   cmd
069500031120     c                   select
069600031120     c                   when      *inkc
069700031120     c                   move      '03'          cmd
069800031120     c                   when      *inkl
069900031120     c                   move      '12'          cmd
070000031120     c                   when      *inkf
070100031120     c                   move      '06'          cmd
070200031120     c                   endsl
070300031120     c                   movel(p)  param         kpjbu
070400031120     c                   seton                                        lr
070500101116      *
070600101116     ** Chiudo il programma.
070700101116     C                   CALL      'TIBSSOCR'
070800101116     C                   PARM      'FINALIZE'    prmRqsOpCode
070900101116     C                   PARM                    prmRpyOpCode
071000101116     C                   PARM                    prmRpyIdMsg
071100101116
071200031120     c                   return
071300031120     C*
071400031120     C                   ENDSR
071500031120?     *--------------------------------------------------------------*
071600031120?     * *INZSR: Operazioni iniziali                                  *
071700031120?     *--------------------------------------------------------------*
071800031120     C     *INZSR        BEGSR
071900031120      *
072000951006     C     *ENTRY        PLIST
072100951006     C                   PARM                    KPJBA
072200031119     C                   MOVEL     KPJBU         PARAM
072300101116     C*
072400101116     ** Inizializzo il programma.
072500101116     C                   CALL      'TIBSSOCR'
072600101116     C                   PARM      'INIT'        prmRqsOpCode
072700101116     C                   PARM                    prmRpyOpCode
072800101116     C                   PARM                    prmRpyIdMsg
072900101116
073000031120     c     *dtaara       define    §azute        azuteds
073100031120     c     *dtaara       define    §datiute      ddatiute
073200031120     C                   in(E)     *dtaara
073300031120     C                   IF        %Error  or  RSUT = *blanks
073400031120     C                   call      'TIBS34R'
073500031120     C                   parm                    Tibs34Ds
073600031120     C                   in        *dtaara
073700031120     c                   ENDIF
073800031120     c                   movel     dutpou        dutpous
073900031120     c*
074000031120     c                   select
074100031121     c                   when      opz = '02'
074200031121     c                   eval      decopz = '   MODIFICA'
074300031121     c                   when      opz = '03'
074400031121     c                   eval      decopz = '     COPIA'
074500031120     c                   when      opz = '04'
074600031120     c                   eval      decopz = '   CANCELLA'
074700031120     c                   when      opz = '05'
074800031120     c                   eval      decopz = '  VISUALIZZA'
074900031120     c                   when      opz = '06'
075000031120     c                   eval      decopz = ' INSERIMENTO'
075100031120     c                   endsl
075200100729     C                   movel     '000400'      forita            6
075300020423     C     Kfrn          KLIST
075400031119     C                   KFLD                    v1csoc
075500020423     C                   KFLD                    kksc
075600980910     C     Krco          KLIST
075700031119     C                   KFLD                    v1csoc
075800020417     C                   KFLD                    forita
075900980910     C                   KFLD                    KKSC
076000031119     C     Ktbe          KLIST
076100031119     C                   KFLD                    tbecod
076200031119     C                   KFLD                    tbeke1
076300031119     C     Kapd          KLIST
076400031119     C                   KFLD                    v1ctip
076500031120     C                   KFLD                    v1cpdr
076600951006      *  Definisco variabili
076700980910     C                   Z-ADD     0             CURR              2 0
076800980910     C                   Z-ADD     0             CURC              2 0
076900980910     c                   endsr
077000101116     C**************************************************************************
077100101116     C* decodifica la società
077200101116     C************************************************************
077300101116     C     Decod_societa begSR
077400101116      *
077500101116     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
077600101116     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
077700101116     C********           RESET                   tibsSocI0
077800101116     C                   EVAL      tibsSocI0.idSocieta = v1csoc
077900101116     C
078000101116     C                   CALL      'TIBSSOCR'
078100101116     C                   PARM      'GETANAGRAF'  prmRqsOpCode
078200101116     C                   PARM      *BLANK        prmRpyOpCode
078300101116     C                   PARM      *ZERO         prmRpyIdMsg
078400101116     C                   PARM      'TIBSSOCI0'   prmRqsFormato
078500101116     C                   PARM                    tibsSocI0
078600101116     C                   PARM                    prmRqsDataSize
078700101116     C                   PARM      'TIBSSOCO0'   prmRpyFormato
078800101116     C                   PARM                    tibsSocO0
078900101116     C                   PARM                    prmRpyDataSize
079000101116     c                   if         PRMRPYIDMSG >= 0
079100101116     c                              and TIBSSOCO0.IDSOCIETA <> *blank
079200101116     c                   eval       v1dsoc  = tibsSocO0.RAGSOCIALE
079300101116     c                   else
079400101116     c                   seton                                        50
079500101116     c                   end
079600101116      *
079700101116     C                   ENDSR
079800101116     C*--------------------------------------------------------------------
