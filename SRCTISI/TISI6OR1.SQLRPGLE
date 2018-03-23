000100051116      *PARMS  TGTRLS(*PRV)
000200000000     H*------------------------------------------------------------------------*
000300131023     H* Cap/località con orari servizi per filiale - Crea File                 *
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900000000     FTABEL00F  IF   E           K DISK
001000140129     Fazorg01l  IF   E           K DISK
001100131024     FTNTBE01L  iF   E           K DISK
001200970707     FAZCPC01L  IF   E           K DISK
001300980210     FAZCPL01L  IF   E           K DISK
001400121012     FAZCPE01L  IF   E           K DISK
001500131022     FWADLY10F  O    E           K DISK    USROPN
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D*-------------------
002000000000     D* COMANDI CLP
002100000000     D*-------------------
002200091211     D CMD             S             80    DIM(4) CTDATA PERRCD(1)
002300960709     D*-------------------
002400980216     D* PROVINCIE
002500960709     D*-------------------
002600051116     d PRV             s                   dim(600)  inz  like(DLYprv)          *PROVINCIA
002700051116     d Pcts            s                   dim(600)  inz  like(§PRcts)          *CODICE TASSAZIONE
002800131024     d*Pfcr            s                   dim(600)  inz  like(DLYfcr)          *CAPOLUOGO DI REGION
002900131024     d*Prap            s                   dim(600)  inz  like(DLYrap)          *REGIONE APPARTENENZ
003000131024     d*Pcor            s                   dim(600)  inz  like(DLYcor)          *ORDINAMENTO
003100000000     D*------------------------------------------------------------------------*
003200000000     D* INPUT
003300000000     D*------------------------------------------------------------------------*
003400980210     D* DS REPERIMENTO DATI UTENTE
003500980210     D*-------------------
003600020919     D TIBS34DS      E DS                                                       *Profili utente
003700020919     D DDATIUTE      E DS                                                       *Dati utente
003800020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003900971020     D*------------------
004000960709     D*-------------------
004100960709     D* DS PROVINCIE
004200960709     D*-------------------
004300960709     D DSPR          E DS
004400000000     D*-------------------
004500000000     D* DS CODICI TASSAZIONE
004600000000     D*-------------------
004700000000     D DSCT          E DS
004800020925     D*-------------------
004900020925     D* DS STATISTICHE DATI DIVERSI
005000020925     D*-------------------
005100020925     D DSDD          E DS
005200980210     D*------------------
005300980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
005400980210     D*------------------
005500980210     D WLBDA8          DS
005600980210     D  G08DAT                 1      8  0
005700980210     D  G08INV                 9     16  0
005800980210     D  G08ERR                17     17
005900980210     D  G08TGI                18     22  0
006000000000     D*-------------------
006100000000     D* DS PARAMETRI DI LANCIO
006200000000     D*-------------------
006300131023     D tisi6ods      e DS
006400131024
006500140109     D tisio9ds      e DS
006600980210     D*-------------------
006700980210     D* ARCHITETTURA
006800980210     D*-------------------
006900980210     D KPJBA         E DS
007000140129     D OG143         e ds                  inz
007100051116      *
007200051116      * V A R I A B I L I
007300051116      *
007400051116     d XX              s              3  0 inz
007500131024     d wtip            s              1    inz
007600140109     d s_MIISC         s                   like(DLYMIISC)
007700140109     d s_MXFSC         s                   like(DLYMXFSC)
007800140109     d s_STISC         s                   like(DLYSTISC)
007900140109     d s_STFSC         s                   like(DLYSTFSC)
008000140110     d s_trzs          s                   like(DLYtrzs)
008100140210     d wfi2            s                   like(cpcfi2)
008200140109
008300131023       // -?Stringa SQL da eseguire?
008400131023     d wSQL            s           5000    inz  varying
008500000000     C*------------------------------------------------------------------------*
008600000000     C* MAIN LINE
008700000000     C*------------------------------------------------------------------------*
008800051116      *
008900051116      * Carica le tabelle occorrenti
009000051116     c                   exsr      CarTAB
009100020925     C*
009200020925     C*
009300050318     C* OPERAZIONI INIZIALI APERTURA FILE
009400020925     C                   EXSR      OPNFIL
009500140129     c* Se richieste tutte le filiali loop su azorg
009600140129    1c                   if        si6ofel=999
009700140129     c     *loval        setll     azorg01l
009800140129     c                   read      azorg01l
009900140129    2c                   dow       not %eof(azorg01l)
010000140129    3c                   if        (orgfag='F' or orgfag ='A') and orgfva=*blanks
010100140129     c                   movel     orgde3        og143
010200140129    4c                   if        §ogntw<>'FED' and §ogntw<>'EEX'
010300140129     c                             and §ogntw<>'DPD'
010400140129     c                   eval      si6ofel=orgfil
010500140214     c* verifico se filiale con soglia
010600140214     c                   clear                   si6osgl
010700140214     c                   clear                   tisio9ds
010800140214     c                   eval      DO0TSER='C'
010900140214     c                   move      si6ofel       DO0LNA
011000140217     c                   eval      do0tric='S '
011100140214     c                   call      'TISIO9R'
011200140214     C                   PARM                    KPJBA
011300140214     C                   PARM                    tiSIO9ds
011400140214     c* carico orari se no errore e no dati standard
011500140214     c                   if        do0fcsgl='S'
011600140214     c                   eval      SI6OSGL='S'
011700140214     c                   endif
011800140129     c                   exsr      elaboraFil
011900140129    4c                   endif
012000140129    3c                   endif
012100140129     c                   read      azorg01l
012200140129    2c                   enddo
012300140129   x1c                   else
012400140129     c* Altrimenti elaboro la filiale ricevuta
012500140129     c                   exsr      elaboraFil
012600140129    1c                   endif
012700000000     C*
012800000000     C* OPERAZIONI DI CHIUSURA
012900971020     C                   EXSR      FINSR
013000000000     C*
013100000000     C                   SETON                                        LR
013200140129     c*
013300140129     C*------------------------------------------------------------------------*
013400140129     C* Elabora
013500140129     C*------------------------------------------------------------------------*
013600140129     C     ElaboraFil    BEGSR
013700140129     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
013800140129     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
013900140129     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
014000140129     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
014100151211     c                   clear                   wdoppiag          1
014200151211     c* se è con soglia verifica se ha la seconda fascia di consegna
014300151211     C                   MOVEL(P)  'FLO'         KBECOD
014400151211     C                   MOVEL(P)  si6ofel       KBEKE1
014500151211     C     KEYTBE        CHAIN     TNTBE01L
014600151211     C                   IF        %found(TNTBE01L)  and tbeatb=' '
014700151211     C                   eval      wdoppiag='S'
014800151211     C                   ENDIF
014900140129     c* creazione record con standard di filiale
015000140129     c                   eval      wtip='S'
015100140129     c                   exsr      credly
015200140129     C*
015300140129     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -CAP-
015400140129IF  1C     $ERR          IFEQ      '0'                                          *NO USCITA
015500140129     C                   EXSR      SETFIL
015600140129     C*
015700140129     C* CICLO FINO A FINE FILE
015800140129DO  2C     $FINE         DOWEQ     '0'
015900140129     C*
016000140129     C* OPERAZIONI PER NUOVO CAP
016100140129     C                   EXSR      NEWCAP
016200140129     C*
016300140129     C* CICLO FINO A ROTTURA CAP
016400140129DO  3C     $FINE         DOWEQ     '0'
016500140129     C     CPCNAR        ANDEQ     DEPNAR
016600140129     C     CPCCAP        ANDEQ     DEPCAP
016700140129     C*
016800140129     C                   Z-ADD     CPClna        deplna
016900140129     C                   Z-ADD     CPCLOL        deplol                         *LINEA OLTRE
017000140129     C                   Z-ADD     CPCLOS        deplos                         *LINEA sotto
017100140129     C*
017200140129     c
017300140129     c* verifico se presenti eccezioni per la linea richiesta
017400140129     c                   if        cpcese='S'
017500140129     c     keycpe        setll     azcpe01l
017600140129     c     keycpe        reade     azcpe01l
017700140129     c                   dow       not %eof(azcpe01l)
017800140129     c                   if        cpeatb=*blanks
017900140129     c                   if        si6ofel=cpelna
018000140129     C                   Z-ADD     CPeLNA        deplna                         *LINEA ARRIVO
018100140129     c                   endif
018200140129     c                   if        si6ofel=cpelol
018300140129     C                   Z-ADD     CPeLol        deplol                         *LINEA ARRIVO
018400140129     c                   endif
018500140129     c                   if        si6ofel=cpelos
018600140129     C                   Z-ADD     CPeLos        deplos                         *LINEA ARRIVO
018700140129     c                   endif
018800140129     c                   endif
018900140129     c     keycpe        reade     azcpe01l
019000140129     c                   enddo
019100140129     c                   endif
019200140129     c
019300140401     C                   MOVEL     CPCISO        WISO                           *DATI CAP
019400140401     C                   Z-ADD     DEPlna        Wilna
019500140401     C                   Z-ADD     DEPlol        Wilol
019600140401     C                   Z-ADD     DEPlos        Wilos
019700140401     C                   move      cpcfi2        wfi2
019800140401     c                   clear                   wloc
019900140401     c                   if        si6ofel=wilna
020000140401     c                             or si6ofel=wilol
020100140401     c                             or si6ofel=wilos
020200140401     c                   eval      wtip=' '
020300140401     C                   EXSR      CREDLY
020400140401     c                   endif
020500140129     C*
020600140129     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
020700140129     C                   EXSR      SETCPL
020800140129     C*
020900140129     C* CICLO FINO A FINE FILE
021000140129DO  4C     $FINL         DOWEQ     '0'
021100140129     C*
021200140129     C*
021300140129     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
021400140129IF  5C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
021500140129     C                   Z-ADD     CPLLNA        WILNA                          *LINEA ARRIVO
021600140129     C                   Z-ADD     CPLLOL        WILOL                          *LINEA OLTRE
021700140129     C                   Z-ADD     CPLLOs        WILOs                          *LINEA sotto
021800140129     C                   MOVEL     CPLISO        WISO                           *DATI LOCALITA'
021900140210     C                   move      CPlfi2        wfi2
022000140129X   5C                   ELSE                                                   *LIVELLO CAP
022100140129     C                   MOVEL     CPCISO        WISO                           *DATI CAP
022200140129     C                   Z-ADD     DEPlna        Wilna
022300140129     C                   Z-ADD     DEPlol        Wilol
022400140129     C                   Z-ADD     DEPlos        Wilos
022500140210     C                   move      cpcfi2        wfi2
022600140129E   5C                   ENDIF
022700140129     c
022800140129     c                   movel     cplloc        wloc
022900140129     c* creo record solo se la filiale da elaborare è presente nelle linee
023000140129     c                   if        si6ofel=wilna
023100140129     c                             or si6ofel=wilol
023200140129     c                             or si6ofel=wilos
023300140129     c                   eval      wtip=' '
023400140129     C                   EXSR      CREDLY
023500140129     c                   endif
023600140129     C*
023700140129     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
023800140129     C                   EXSR      LETCPL
023900140129E   4C                   ENDDO                                                  *FINE -LOCALITA'-
024000140129     C*
024100140129     C* LETTURA RECORD SUCCESSIVO -CAP-
024200140129     C                   EXSR      LETFIL
024300140129E   3C                   ENDDO                                                  *ROTTURA CAP
024400140129E   2C                   ENDDO                                                  *FINE -CAP-
024500140129E   1C                   ENDIF
024600140129     c                   endsr
024700000000     C*------------------------------------------------------------------------*
024800980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
024900000000     C*------------------------------------------------------------------------*
025000000000     C     SETFIL        BEGSR
025100000000     C*
025200970707     C                   Z-ADD     P96VER        KCCVER
025300131023     C     KEYCPC        SETLL     AZCPC01L
025400131023IF  1C                   IF        not %equal(azcpc01l)
025500000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
025600000000X   1C                   ELSE
025700000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
025800000000E   1C                   ENDIF
025900000000     C*
026000000000     C                   ENDSR
026100000000     C*------------------------------------------------------------------------*
026200980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
026300000000     C*------------------------------------------------------------------------*
026400000000     C     LETFIL        BEGSR
026500000000     C*
026600000000     C* LEGGE FINO A:
026700000000     C*     - FINE FILE
026800000000     C*     - TROVATO RECORD VALIDO
026900000000     C                   MOVEL     'N'           WRECOK
027000000000DO  1C     $FINE         DOWEQ     '0'
027100000000     C     WRECOK        ANDEQ     'N'
027200131023     C     KEYCPC        READE     AZCPC01L
027300131023IF  2C                   IF        %eof(azcpc01l)
027400000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
027500000000X   2C                   ELSE
027600000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
027700000000E   2C                   ENDIF
027800000000E   1C                   ENDDO
027900960701     C*
028000000000     C                   ENDSR
028100000000     C*------------------------------------------------------------------------*
028200980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
028300000000     C*------------------------------------------------------------------------*
028400000000     C     CHKREC        BEGSR
028500000000     C*
028600000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
028700000000     C*
028800000000     C* CAP ESTERO, ESCLUDE
028900970707IF  1C     CPCNAR        IFNE      *BLANKS
029000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029100000000E   1C                   ENDIF
029200000000     C*
029300000000     C* CAP FITTIZIO, ESCLUDE
029400970707IF  1C     CPCFIT        IFEQ      'S'
029500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029600000000E   1C                   ENDIF
029700980217     C*
029800980217     C* CAP OBSOLETO, ESCLUDE
029900131023IF  1C**   CPCFI1        IFEQ      'O'
030000131023     C***                MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
030100131023E   1C***                ENDIF
030200000000     C*
030300000000     C                   ENDSR
030400980216     C*------------------------------------------------------------------------*
030500980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
030600980216     C*------------------------------------------------------------------------*
030700980216     C     NEWCAP        BEGSR
030800980216     C*
030900980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
031000980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
031100980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
031200980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
031300980216     C*
031400980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
031500131024     C*                  Z-ADD     1             I
031600131024     C*                  SETOFF                                           28
031700131024     C*    CPCPRV        LOOKUP    PRV(I)                                 28
031800131024IF  1C*    *IN28         IFEQ      *ON                                          *TROVATO
031900131024     C*                  MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
032000131024     C*                  MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
032100131024     C*                  MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
032200131024X   1C*                  ELSE                                                   *NON TROVATO
032300131024     C*                  MOVEL     *BLANKS       DEPFCR
032400131024     C*                  MOVEL     *BLANKS       DEPRAP
032500131024     C*                  MOVEL     *BLANKS       DEPCOR
032600131024E   1C*                  ENDIF
032700980216     C*
032800980216     C                   ENDSR
032900980216     C*------------------------------------------------------------------------*
033000980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
033100980216     C*------------------------------------------------------------------------*
033200980216     C     SETCPL        BEGSR
033300980216     C*
033400980216     C* REIMPOSTA LE VARIABILI DI LAVORO
033500980216     C                   MOVEL     '0'           $FINL
033600980216     C*
033700980216     C                   Z-ADD     P96VER        KCLVER
033800980216     C                   MOVEL     DEPNAR        KCLNAR
033900980216     C                   MOVEL     DEPCAP        KCLCAP
034000131023     C     KEYCPL        SETLL     AZCPL01L
034100131023IF  1C                   IF        not %equal(azcpl01l)
034200980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
034300980216X   1C                   ELSE
034400980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
034500980216E   1C                   ENDIF
034600980216     C*
034700980216     C                   ENDSR
034800980216     C*------------------------------------------------------------------------*
034900980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
035000980216     C*------------------------------------------------------------------------*
035100980216     C     LETCPL        BEGSR
035200980216     C*
035300980216     C* LEGGE FINO A:
035400980216     C*     - FINE FILE
035500980216     C*     - TROVATO RECORD VALIDO
035600980216     C                   MOVEL     'N'           WRECOK
035700980216DO  1C     $FINL         DOWEQ     '0'
035800980216     C     WRECOK        ANDEQ     'N'
035900131023     C     KEYCPL        READE     AZCPL01L
036000131023IF  2C                   IF        %eof(azcpl01l)
036100980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
036200980216X   2C                   ELSE
036300980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
036400980216E   2C                   ENDIF
036500980216E   1C                   ENDDO
036600980216     C*
036700980216     C                   ENDSR
036800980216     C*------------------------------------------------------------------------*
036900980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
037000980216     C*------------------------------------------------------------------------*
037100980216     C     CHKCPL        BEGSR
037200980216     C*
037300980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
037400980216     C*
037500980217     C* LOCALITA' OBSOLETA, ESCLUDE
037600131023IF  1C*    CPLFI1        IFEQ      'O'
037700131023     C*                  MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
037800131023E   1C*                  ENDIF
037900980216     C*
038000980216     C                   ENDSR
038100980216     C*------------------------------------------------------------------------*
038200980216     C* CREDLY - CREA IL RECORD SUL WORKFILE
038300980216     C*------------------------------------------------------------------------*
038400980216     C     CREDLY        BEGSR
038500980216     C*
038600131023     C                   CLEAR                   WADLY100
038700140109     c                   clear                   s_MIISC
038800140109     c                   clear                   s_MXFSC
038900140109     c                   clear                   s_STISC
039000140109     c                   clear                   s_STFSC
039100140109
039200140109     c*---
039300980216     C* DATI DEDOTTI
039400980216     C*---
039500131023     C                   z-add     si6ofel       DLYfel
039600131023     c                   if        wtip='S'
039700131023     c                   eval      dlyloc='STANDARD'
039800131023     c                   clear                   depnar
039900131023     c                   clear                   depcap
040000131023     c                   clear                   wloc
040100131023     c                   exsr      ela_cos
040200131023     c
040300131023     c                   else
040400131024     C*                  MOVEL     DEPFCR        DLYFCR                         *CAPOLUOGO DI REGION
040500131024     C*                  MOVEL     DEPRAP        DLYRAP                         *REGIONE APPARTENENZ
040600131024     C*                  MOVEL     DEPCOR        DLYCOR                         *CODICE ORDINAMENTO
040700980216     C*---
040800980216     C* DATI CAPPARIO
040900980216     C*---
041000980216     C                   MOVEL     DEPNAR        DLYNAR                         *NAZIONE
041100980216     C                   MOVEL     DEPCAP        DLYCAP                         *CAP
041200980216     C                   MOVEL     DEPPRV        DLYPRV                         *PROVINCIA
041300980216     C                   MOVEL     WLOC          DLYLOC                         *LOCALITA'
041400980216     C                   MOVEL     WISO          DLYISO                         *INOLTRO
041500140401     c                   if        cpcfi1='O' or ( wloc<>*blanks and cplfi1='O')
041600131023     c                   eval      dlyobs='O'
041700131023     c                   endif
041800121012     c* imposto le 3 linee di arrivo previste dal cap/località
041900131023     c                   z-add     wilna         dlylna
042000131023     c                   z-add     wilol         dlylnao
042100131023     c                   z-add     wilos         dlylnas
042200140210     c                   if        wfi2<>'01'
042300140210     c                   eval      dlyfgp='N'
042400140210     C                   endif
042500131023     c* orari servizi della linea da elaborare
042600131023     c                   exsr      ela_cos
042700131023     c* se non reperiti orari per località prendo quelli per cap
042800121012     c
042900131023     c                   endif
043000131023     C                   WRITE     WADLY100                                     I N S E R I M E N T
043100140109
043200140109     c* Se soglia=S --> Creazione record "POM"
043300140109     c                   if        si6osgl='S'
043400140109     c                   clear                   DLYSTISR
043500140109     c                   clear                   DLYSTFSR
043600140109     c                   clear                   DLYLRSC
043700140109     c                   clear                   DLYLRNC
043800140109     c                   clear                   DLYLRNK
043900151211     c                   if        wdoppiag='S'
044000151211     c                   eval      dlysgl='2'
044100151211     c                   else
044200151211     c                   eval      dlysgl='P'
044300151211     c                   endif
044400140109     c                   eval      DLYMIISC = s_MIISC
044500140109     c                   eval      DLYMXFSC = s_MXFSC
044600140109     c                   eval      DLYSTISC = s_STISC
044700140109     c                   eval      DLYSTFSC = s_STFSC
044800140110     c                   eval      DLYtrzs  = s_trzs
044900140109     C                   WRITE     WADLY100                                     I N S E R I M E N T
045000140109     c                   endif
045100090921     c
045200980216     C                   ENDSR
045300131023     C*------------------------------------------------------------------------*
045400131023     C*
045500131023     C*------------------------------------------------------------------------*
045600131023     C     ela_cos       BEGSR
045700131024
045800140109     c                   clear                   tisio9ds
045900131024     c* Ritiri
046000140207     c                   eval      do0tric='S '
046100131024     c                   eval      DO0VER = p96ver
046200140214     c                   eval      DO0dri = p96dri
046300131024     c                   eval      DO0NAR = depnar
046400131024     c                   eval      DO0CAP =depcap
046500131024     c                   eval      DO0LOC =wloc
046600131024     c                   eval      DO0LNA =si6ofel
046700131024     c                   eval      DO0TSER='R'
046800140109     c                   call      'TISIO9R'
046900131024     C                   PARM                    KPJBA
047000140109     C                   PARM                    tiSIO9ds
047100131024     c* carico orari se no errore e no dati standard
047200131024     c                   if        do0err=*blanks and do0rif<>'9'
047300131024     c                   eval      DLYSTISR = DO0OSTIS
047400131024     c                   eval      DLYSTFSR = DO0OSTFS
047500131024     c                   eval      DLYLRSC  = DO0OLRSC
047600131024     c                   eval      DLYLRNC  = DO0OLRNC
047700131024     c                   eval      DLYLRNK  = DO0OLRNK
047800131024     c                   endif
047900131024     c* Consegne
048000131024     c                   eval      DO0TSER='C'
048100140109     c                   eval      DO0SOGLIA='T'
048200140109     c                   call      'TISIO9R'
048300131024     C                   PARM                    KPJBA
048400140109     C                   PARM                    tiSIO9ds
048500131024     c* carico orari se no errore e no dati standard
048600131024     c                   if        do0err=*blanks and do0rif<>'9'
048700131024     c                   eval      DLYMIISC = DO0OMIIS
048800131024     c                   eval      DLYMXFSC = DO0OMXFS
048900131024     c                   eval      DLYSTISC = DO0OSTIS
049000131024     c                   eval      DLYSTFSC = DO0OSTFS
049100140110     c                   eval      DLYtrzs  = DO0TRZS
049200131024     c                   endif
049300140109     c* se deve creare riga anche per la soglia richiamo anche per pomeriggio
049400140109     c                   if        si6osgl='S'
049500140109     c                   eval      DO0SOGLIA='P'
049600140109     c                   call      'TISIO9R'
049700140109     C                   PARM                    KPJBA
049800140109     C                   PARM                    tiSIO9ds
049900140109     c* carico orari se no errore e no dati standard
050000140109     c                   if        do0err=*blanks and do0rif<>'9'
050100140109     c                   eval      s_MIISC = DO0OMIIS
050200140109     c                   eval      s_MXFSC = DO0OMXFS
050300140109     c                   eval      s_STISC = DO0OSTIS
050400140109     c                   eval      s_STFSC = DO0OSTFS
050500140110     c                   eval      s_trzs  = DO0trzs
050600140109     c                   endif
050700140109     c                   endif
050800131023     C                   ENDSR
050900000000     C*------------------------------------------------------------------------*
051000971020     C* FINSR - OPERAZIONI FINALI
051100000000     C*------------------------------------------------------------------------*
051200971020     C     FINSR         BEGSR
051300971020     C*
051400971020     C* CHIUDE I FILE DI LAVORO
051500131023     C                   CLOSE     WADLY10F
051600000000     C*
051700000000     C                   ENDSR
051800000000     C*------------------------------------------------------------------------*
051900050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
052000000000     C*------------------------------------------------------------------------*
052100000000     C     OPNFIL        BEGSR
052200091211     C                   Z-ADD     80            LENGH            15 5
052300091211     c* se si tratta di una prova, creo il file, vuoto in EDPES
052400131023    1c                   if        si6oprova='S'
052500091211     c* creazione file
052600091211     C                   MOVEL(P)  CMD(2)        COMMAN           80
052700091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
052800091211     C                   PARM                    COMMAN
052900091211     C                   PARM                    LENGH
053000091211     c
053100091211     c* Se c'e' già lo pulisco
053200091211    2C                   IF        %error
053300091211     C                   MOVEL(P)  CMD(3)        COMMAN           80
053400091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
053500091211     C                   PARM                    COMMAN
053600091211     c                   PARM                    LENGH
053700091211    3C                   IF        %error
053800091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
053900091211    3C                   ENDIF
054000091211    2C                   ENDIF
054100091211     c
054200091211     c* Eseguo ovrdbf
054300091211     C                   MOVEL(P)  CMD(4)        COMMAN           80
054400091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
054500091211     C                   PARM                    COMMAN
054600091211     c                   PARM                    LENGH
054700091211    3C                   IF        %error
054800091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
054900091211    3C                   ENDIF
055000091214     c
055100091211   x1c                   else
055200020925     c*
055300131023     c* Cancella dal file i record della filiale richiesta
055400140129     c                   if        si6ofel<>999
055500131023      /FREE
055600131023         wSQL = 'DELETE from WADLY10F where DLYFEL = '
055700131023              + %char(si6ofel) ;
055800131023         exec SQL   execute immediate :wSQL;
055900131023         if sqlcod < 0 ;
056000131023            $err='1';
056100131023         endif;
056200131023      /END-FREE
056300140129     c                   else
056400140129      /FREE
056500140129         wSQL = 'DELETE from WADLY10F';
056600140129         exec SQL   execute immediate :wSQL;
056700140129         if sqlcod < 0 ;
056800140129            $err='1';
056900140129         endif;
057000140129      /END-FREE
057100140129     c                   endif
057200091211    1C                   ENDIF
057300020925     C*
057400050318     C* Apre il file
057500131023     C                   OPEN      WADLY10F                                     *APRE FILE
057600960701     C*
057700000000     C                   ENDSR
057800960709     C*------------------------------------------------------------------------*
057900980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
058000960709     C*------------------------------------------------------------------------*
058100960709     C     CARTAB        BEGSR
058200020925     C*---
058300020925     C* Dati statistiche
058400020925     C*---
058500020925     C                   CLEAR                   DSDD
058600020925     C                   MOVEL(P)  'SDD'         KBECOD
058700020925     C                   MOVEL(P)  '1'           KBEKE1
058800131024     C     KEYTBE        CHAIN     TNTBE01L
058900020925     C                   IF        %found(TNTBE01L)
059000020925     C                   MOVEL     TBEUNI        DSDD
059100020925     C                   ENDIF
059200980211     C*---
059300980211     C* PROVINCIE
059400980211     C*---
059500980216     C                   Z-ADD     *ZEROS        I                 5 0
059600020919     C                   Z-ADD     1             KTBKUT
059700960709     C                   MOVEL     'PR'          KTBCOD
059800960709     C     KE2TAB        SETLL     TABEL00F
059900960709     C     KE2TAB        READE     TABEL00F                               99
060000960709DO  1C     *IN99         DOWEQ     *OFF
060100960709IF  2C     TBLFLG        IFEQ      *BLANKS
060200960709     C                   ADD       1             I
060300960709     C                   MOVEL     TBLUNI        DSPR
060400960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
060500960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
060600131024     c****               movel     §PRfcr        Pfcr(I)                        *Capoluogo Regione
060700960709E   2C                   ENDIF
060800960709     C     KE2TAB        READE     TABEL00F                               99
060900960709E   1C                   ENDDO
061000051116      *
061100051116      * Imposta il progressivo per le province (diverse) con lo stesso
061200051116      *   codice tassazione (da cui viene reperito il cod. ordinamento
061300051116      *   - che NON può essere uguale per due province diverse:
061400051116      *   ha causato confusione nel reperimento dei tempi di consegna
061500051116      *   e/o nell'elenco delle località)
061600131024     c***                clear                   xx
061700131024do  1c***                DOW       xx       <  %elem(PRV)
061800131024     c***                add       1             xx
061900131024if  2c***                if        Prv(xx)  =  *blanks
062000131024     c***                leave
062100131024e   2c***                endif
062200051116      * - scarto le province per le quali è già stata impostata la
062300051116      *   "sigla" finale della provincia per il codice ordinamento
062400131024     c***                if        Pcor(xx) <> *blanks
062500131024     c***                iter
062600131024     c***                endif
062700051116      * - cerco le altre province con lo stesso codice tassazione
062800131024     c***                eval      I        =  xx
062900131024do  2c***                DOW       I        <  %elem(PRV)
063000131024     c***                add       1             I
063100131024     c***                eval      *in28    =  *off
063200131024     c***  Pcts(xx)      lookup    Pcts(I)                                28
063300131024if  3c***                if        *in28    =  *off
063400131024     c***                leave
063500131024e   3c***                endif
063600051116      * - e per ognuna di esse (tranne che per il capoluogo regionale)
063700051116      *   memorizzo la provincia stessa - da posporre al codice
063800051116      *   ordinamento
063900051116      *   Esempio finale: "CCA  " = Milano    "CCALO" = Lodi
064000051116      *                   "HHCFO" = Forlì     "HHCRN" = Rimini
064100051116      *   (ora memorizzo "   LO", "   FO", "   RN")
064200131024if  3c***                if        Pfcr(xx) =  *blanks
064300131024     c***                move      Prv(xx)       Pcor(xx)
064400131024e   3c***                endif
064500131024if  3c***                if        Pfcr(I)  =  *blanks
064600131024     c***                move      Prv(I)        Pcor(I)
064700131024e   3c***                endif
064800131024e   2c***                ENDDO
064900131024e   1c***                ENDDO
065000980211     C*---
065100960709     C* DATI CODICI TASSAZIONE
065200980211     C*---
065300020919     C                   Z-ADD     1             KTBKUT
065400960709     C                   MOVEL     'CT'          KTBCOD
065500041213DO  1C     1             DO        600           I
065600960709IF  2C     PRV(I)        IFNE      *BLANKS
065700960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
065800960709     C     KEYTAB        CHAIN     TABEL00F                           99
065900960709IF  3C     *IN99         IFEQ      *OFF
066000960709     C     TBLFLG        ANDEQ     *BLANKS
066100960709     C                   MOVEL     TBLUNI        DSCT
066200131024     C***                MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
066300131024     C****               MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
066400960709E   3C                   ENDIF
066500020925E   3C                   ENDIF
066600960709E   1C                   ENDDO
066700960709     C*
066800960709     C                   ENDSR
066900020919     C*--------------------------------------------------------------------------------------------*
067000020919     C* REPERISCE I DATI UTENTE
067100020919     C*--------------------------------------------------------------------------------------------*
067200020919     C     REPDATIUTE    BEGSR
067300020919     C*
067400020919     C* INIZIALIZZA VARIABILI DI WRK
067500020919     C                   CLEAR                   TIBS34DS
067600020919     C                   CLEAR                   AZUTEDS
067700020919     C                   CLEAR                   DDATIUTE
067800020919     C*
067900020919     C     *DTAARA       DEFINE    §azute        azuteds
068000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
068100020919     C                   IN(E)     *DTAARA
068200020919if  1C                   IF        %Error
068300020919     c                   EVAL      I34Tla = 'L'
068400020919     C                   CALL      'TIBS34R'
068500020919     C                   PARM                    Tibs34Ds
068600020919     C                   IN        *DTAARA
068700020919e   1C                   ENDIF
068800020919     C*
068900020919     C                   ENDSR
069000000000     C*------------------------------------------------------------------------*
069100000000     C* *INZSR - OPERAZIONI INIZIALI
069200000000     C*------------------------------------------------------------------------*
069300000000     C     *INZSR        BEGSR
069400980211     C*---
069500000000     C* RICEVIMENTO PARAMETRI
069600980211     C*---
069700000000     C     *ENTRY        PLIST
069800000000     C                   PARM                    KPJBA
069900131023     C                   MOVEL     KPJBU         tisi6ods
070000091204
070100980211     C*---
070200000000     C* VARIABILI RIFERITE AL DATABASE
070300980211     C*---
070400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
070500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
070600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
070700020925     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
070800020925     C     *LIKE         DEFINE    TBEKE1        KBEKE1
070900980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
071000980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
071100980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
071200980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
071300980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
071400980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
071500980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
071600131024     C*    *LIKE         DEFINE    DLYFCR        DEPFCR
071700131024     C*    *LIKE         DEFINE    DLYRAP        DEPRAP
071800131024     C*    *LIKE         DEFINE    DLYCOR        DEPCOR
071900121015     C     *LIKE         DEFINE    CPCLNA        DEPLna
072000121015     C     *LIKE         DEFINE    CPCLNA        DEPLIN
072100121015     C     *LIKE         DEFINE    CPCLNA        DEPlol
072200121015     C     *LIKE         DEFINE    CPCLNA        DEPlos
072300980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
072400980216     C     *LIKE         DEFINE    CPCISO        WISO
072500980302     C     *LIKE         DEFINE    CPCLNA        WILNA
072600980302     C     *LIKE         DEFINE    CPCLOL        WILOL
072700121012     C     *LIKE         DEFINE    CPCLOS        WILOS
072800980211     C*---
072900000000     C* CHIAVI DI LETTURA
073000980211     C*---
073100980211     C     KEYTAB        KLIST                                                  *TABEL00F
073200980211     C                   KFLD                    KTBKUT                          -UTENTE
073300980211     C                   KFLD                    KTBCOD                          -TABELLA
073400980211     C                   KFLD                    KTBKEY                          -CHIAVE
073500980211     C     KE2TAB        KLIST                                                  *TABEL00F
073600980211     C                   KFLD                    KTBKUT                          -UTENTE
073700980211     C                   KFLD                    KTBCOD                          -TEBELLA
073800020925     C     KEYTBE        KLIST                                                  *TNTBE01L
073900020925     C                   KFLD                    KBECOD                          -TABELLA
074000020925     C                   KFLD                    KBEKE1                          -CHIAVE
074100980211     C     KEYCPC        KLIST                                                  *AZCPC00F
074200980211     C                   KFLD                    KCCVER                          -VERSIONE
074300980211     C     KEYCPL        KLIST                                                  *AZCPL00F
074400980211     C                   KFLD                    KCLVER                          -VERSIONE
074500980216     C                   KFLD                    KCLNAR                          -NAZIONE
074600980216     C                   KFLD                    KCLCAP                          -CAP
074700121012     C     KEYCPE        KLIST                                                  *AZCPL00F
074800121012     C                   KFLD                    p96VER                          -VERSIONE
074900121012     C                   KFLD                    depNAR                          -NAZIONE
075000121012     C                   KFLD                    depCAP                          -CAP
075100980211     C*---
075200000000     C* VARIABILI DI CONTROLLO
075300980211     C*---
075400980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
075500980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
075600980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
075700980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
075800050318     C                   MOVEL     *BLANKS       $DLY              1            *CREAZIONE WADLY (C/
075900020527     C*
076000020527     C* CALCOLA LA DATA CORRENTE -DAL JOB-
076100020527     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
076200020527     C                   Z-ADD     N8            G08DAT
076300020527     C                   Z-ADD     *ZEROS        G08INV
076400020527     C                   MOVEL     '0'           G08ERR
076500020527     C                   CALL      'XSRDA8'
076600020527     C                   PARM                    WLBDA8
076700020527     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
076800020925     C*
076900020925     C* REPERISCE I DATI UTENTE
077000020925     C                   EXSR      REPDATIUTE
077100970707     C*
077200131023     C                   Z-ADD     si6odri       P96DRI            8 0
077300970707     C                   Z-ADD     *ZEROS        P96VER            5 0
077400970707     C                   CALL      'TISI96R'
077500970707     C                   PARM                    P96DRI
077600970707     C                   PARM                    P96VER
077700960709     C*
077800000000     C                   ENDSR
077900000000     C*------------------------------------------------------------------------*
078000000000**   CMD -COMANDI CLP-
078100131023
078200131023CRTDUPOBJ OBJ(WADLY10F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(EDPRM)
078300131023CLRPFM FILE(EDPRM/WADLY10F)
078400131023OVRDBF FILE(WADLY10F) TOFILE(EDPRM/WADLY10F)
