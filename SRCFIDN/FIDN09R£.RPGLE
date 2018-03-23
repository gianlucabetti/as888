000100001227      *------------------------------------------------------------------------*
000200981026      * Stampa lettere                                                         *
000300981027      *    AL7 - Lettera di Invio in Sede C.A. Relative ad Evento              *
000400981027      *    A8B - Lettera Invio Denuncia Evento al P.O. per Vidimazione         *
000500981026      *------------------------------------------------------------------------*
000600980731
000700980731      ****************************************************************
000800981026      *  FASI C.A. FISSATE A PROGRAMMA
000900980731      ****************************************************************
001000980731      ***************************************************************
001100980729
001200980729      ****************************************************************
001300980729      *  RIEPILOGO INDICATORI
001400980729      ***************************************************************
001500980729      * 01   - SALTO PAGINA
001600981026      * 02   - STAMPO seconda riga oggetto
001700981117      * 03   -
001800980803      * 04   -
001900980803      * 05   -
002000981126      * 06   - STAMPO Interlocutore
002100980729      ****************************************************************
002200980729
002300980730     H DECEDIT('0,') DATEDIT(*DMY/)
002400980729
002500981026     fFIDN09P   O    E             PRINTER OFLIND(*IN01)     USROPN
002600980731     fFNDFA01L  IF   E           K DISK
002700980729     fAZORG01L  IF   E           K DISK
002800980729
002900980803     D TEEL            S              3    DIM(2)                               testi da elaborare
003000980803      *----------------------------------
003100990119     D CMD             S             80    DIM(4)  CTDATA PERRCD(1)             OVRPRTF
003200021108     D COTE            S              3    DIM(2) CTDATA PERRCD(1)              codice testo
003300021108     D COSC            s              6S 0 DIM(2) ALT(cote)                     coordinate schiera
003400021108     D TES1            S            100    DIM(10) CTDATA PERRCD(1)             testo lettera 1 riga
003500021108     D TES2            S             55    DIM(10) CTDATA PERRCD(1)             testo lettera 2 riga
003600021108     D TES3            S             50    DIM(10) CTDATA PERRCD(1)             testo lettera 2 riga
003700980730      *----------------------------------
003800110505     D KBAR            C                   CONST('BRT  S.p.a.')
003900980729      *----------------------------------
004000980616     D                SDS
004100980616     D  VTCPGM                 1     10
004200980730      *----------------------------------
004300980730     D WLBDAT          DS                  INZ
004400980730     D  g02dat                 1      8  0
004500980730     D  g02inv                 9     16  0
004600980730     D  g02err                17     17
004700980730     D  g02tgi                18     22  0
004800980928      *
004900980731     D                 DS
005000980803     D  Wtestoda               1      3  0
005100980803     D  Wtestoa                4      6  0
005200980803     D  Wtestodaa              1      6  0
005300980928      *
005400980730     D FIDN00DS      E DS
005500980730     D FIDN05DS      E DS
005600980730     D TIBS02DS      E DS
005700980507     D KPJBA         E DS
005800980729     D DTAD          E DS
005900981027     D DTLD          E DS
006000990510     D DSTD2         E DS
006100980729      *----------------------------------
006200981028     D WAanno          s              4
006300981028     D WAfil           s              3
006400981028     D WAnum           s              7
006500981026     D command         S             80
006600980730     D w007a           s              7
006700980929     D w006a           s              6
006800980929     D w006b           s              6
006900980803     D wdataoggi       S             10
007000980928     D wdataeur        S             10
007100980803     D wsocie          S             20
007200990119     D lung            S             15  5 inz(80)
007300981028     D Wfil            s              3  0
007400980730     D XX              s              3  0
007500980731     D YY              s              3  0
007600980731     D JJ              s              3  0
007700980730     D dateu           s              8  0
007800980730     D wdtgio          S              8  0
007900980730     D w0140           S             14  0
008000981027     D W§TLDstp        S                   LIKE(§TLDstp1)
008100990120     D W§TADdes        S                   LIKE(§TADdest)
008200980730      *
008300980730     D DATA_eur        S               D   DATFMT(*eur)
008400980730     D DATA_oggi       S               D   DATFMT(*eur)
008500980918      *
008600981002     D SEDE_uas        S                   LIKE(PICrag)
008700980928     d                                     INZ('Ufficio ASSICURAZIONI MERCE')
008800981002     D SEDE_ind        S                   LIKE(PICind)
008900980928     d                                     INZ('BOLOGNA SEDE')
009000980731      *---------------------------------------------------------------------
009100981026
009200981204      * Aggancio anagrafico fasi per tipi testo
009300980731     c     I00fca        CHAIN     FNDFA000                           30
009400981204
009500981001      * Memorizzo in schiera i tipi testo da elaborare
009600980731     c                   z-add     1             YY
009700980731     c     DFAle1        lookup    COTE(YY)                               30
009800980731     c   30              movel     DFALE1        TEEL(1)
009900980731     c                   z-add     1             YY
010000980731     c     DFAle2        lookup    COTE(YY)                               30
010100980731     c   30              movel     DFALE2        TEEL(2)
010200981204
010300980805      * Elaboro i tipi testo
010400980731     c     1             DO        2             JJ
010500981027     c                   IF        teel(JJ) <> *blanks
010600981027      *
010700981027      * Aggancio tabella TIPO TESTO
010800981027     c                   EXSR      DECtld
010900981001      *
011000981001      *  Memorizzo le coordinate del testo nelle schiere
011100980803     c                   z-add     1             YY
011200980803     c     TEEL(jj)      lookup    COTE(YY)                               30
011300980803     c   30              movel     COSC(YY)      Wtestodaa
011400980803      *
011500980805     c                   EXSR      SUB_INTES                                     intestazione
011600980805     c                   EXSR      SUB_OGGE                                      oggetto
011700980805     c                   EXSR      SUB_TESTO                                     testo
011800980805     c                   EXSR      SUB_FIRMA                                     firma
011900980803      *
012000981027     c                   ENDIF
012100981204      *
012200981204     c                   CLOSE     FIDN09P
012300980731     c                   ENDDO
012400981026      *
012500981029     c                   clear                   tibs02ds
012600981029     c                   movel     'C'           t02tla
012700981029     c                   call      'TIBS02R'
012800981029     c                   parm                    kpjba
012900981029     c                   parm                    tibs02ds
013000981204      *
013100980506     C                   EVAL      *INLR = *ON
013200980729      *****************************************************************
013300980730      *   INTESTAZIONE
013400980729      *****************************************************************
013500980730     C     SUB_intes     BEGSR
013600980729      *
013700980730      * DATI del P.O.
013800981028     c                   SELECT
013900981028      *
014000021108     c                   WHEN        TEEL(jj) = 'AL7'
014100981028     c                   z-add     I00fgs        Wfil
014200981028      *
014300981028     c                   WHEN        TEEL(jj) = 'A8B'
014400981028     c                   z-add     I00fce        Wfil
014500981028      *
014600981028     c                   ENDSL
014700981028      *
014800981028     c     WFIL          CHAIN     AZORG                              30
014900980803      *
015000981026      * Imposto Società
015100981026     c                   movel     KBAR          Wsocie
015200981027      *
015300981027      * Imposto Tipo Modulo e Stampante
015400981027     c                   EXSR      IMPPRT
015500981027      *
015600981026      * Apro file stampa
015700981026     c                   OPEN      FIDN09P
015800981026      *
015900981026      * Imposto mittente e destinatario
016000981002     c                   SELECT
016100981007      *
016200021108     c                   WHEN      TEEL(jj) = 'AL7'
016300981117     c                   movel     'TEL:'        PICtel
016400981117     c                   movel     'FAX:'        PICfax
016500070514     c                   eval      PICint  =  'Filiale di'
016600981027     c                   eval      PICin1  =  'Spett.le'
016700981002     c                   movel     SEDE_uas      PICrag
016800981002     c                   movel     SEDE_ind      PICind
016900981002     c                   movel     *blanks       PICcap
017000981002     c                   movel     *blanks       PICloc
017100981002     c                   movel     *blanks       PICpro
017200981002     c                   movel     *blanks       PICnaz
017300981026      *
017400981026     c                   movel     ORGdes        PICpod
017500981026     c                   movel     ORGind        PICpoi
017600981026     c                   movel     ORGcpf        PICpoc
017700981026     c                   movel     ORGloc        PICpol
017800981026     c                   movel     ORGpro        PICpop
017900981026     c                   movel     ORGtel        PICpot
018000981026     c                   movel     ORGfax        PICpof
018100981026
018200981027     c                   WHEN      TEEL(jj) = 'A8B'
018300070514     c                   eval      PICin1  =  'Spett.le Filiale di'
018400981007     c                   movel     ORGDES        PICrag
018500981007     c                   movel     *blanks       PICind
018600981007     c                   movel     *blanks       PICcap
018700981007     c                   movel     *blanks       PICloc
018800981007     c                   movel     *blanks       PICpro
018900981007     c                   movel     *blanks       PICnaz
019000981026      *
019100981026     c                   movel     SEDE_uas      PICpod
019200981026     c                   movel     sEDE_ind      PICpoi
019300981026     c                   clear                   PICpoc
019400981026     c                   clear                   PICpol
019500981026     c                   clear                   PICpop
019600981026     c                   clear                   PICpot
019700981026     c                   clear                   PICpof
019800981026
019900981002     c                   ENDSL
020000981007      *
020100981026      * Imposto Località + data
020200981007     C     *eur          movel     DATA_oggi     wdataoggi
020300981007      *
020400021108     c                   IF        TEEL(jj) = 'A8B'
020500981007     c                   eval      PICdal = 'BOLOGNA, ' + wdataoggi
020600981007     C                   ELSE
020700981007     c                   eval      PICdal = %TRIM(ORGDES) + ', ' + wdataoggi
020800981007     c                   ENDIF
020900980918      *
021000981026      * Imposto Note intestazione
021100980805      *
021200981026      * Scrivo l'intestazione
021300981026     c                   WRITE     DN09PI1
021400980730      *
021500980730     C                   ENDSR
021600980730      *****************************************************************
021700980730      *   OGGETTO
021800980730      *****************************************************************
021900980730     C     SUB_ogge      BEGSR
022000980928
022100980928     c                   SELECT
022200980928
022300981026      *--------------
022400981027     c                   WHEN      TEEL(jj) = 'AL7'  or  TEEL(jj) = 'A8B'
022500981026      *
022600981026     c                   exsr      CHTAD
022700980929      *
022800981028     c                   movel     I00aae        WAanno
022900980928     c                   movel     I00nev        w007a
023000980928     c     '0'           check     w007a         yy
023100981028     c                   eval      WAnum = %subst(W007a:yy)
023200980928     C                   if        I00dev > *zeros
023300980928     c     *iso          movel     I00dev        DATA_eur
023400980928     c     *eur          movel     DATA_eur      Wdataeur
023500980928     C                   endif
023600980928      *
023700990120     C                   eval      POCogg =   'Evento N.  '  +  %trim(WAnum)
023800990120     c                                       +  ' anno '  +  WAanno +  '  del '
023900990120     c                                       +  wdataeur  +  '    '  +  W§taddes
024000990120      *
024100981026     c                   WRITE     DN09PO1
024200981007
024300981026      *--------------
024400980928     c                   ENDSL
024500980928
024600980729     C                   ENDSR
024700980729      *****************************************************************
024800981026      *  REPERIMENTO DESCRIZIONE ANOMALIA EVENTO
024900980729      *****************************************************************
025000980729     C     CHTAD         BEGSR
025100980729      *
025200980730     c                   clear                   tibs02ds
025300980729     c                   clear                   dtad
025400980729      *
025500980729     C                   movel     'C'           t02mod
025600980729     C                   movel     knsif         t02sif
025700980729     C                   movel     'TAD'         t02cod
025800981026     C                   movel(P)  I00tae        t02ke1
025900980729      *
026000980729     C                   CALL      'TIBS02R'
026100980729     C                   PARM                    KPJBA
026200980730     C                   PARM                    TIBS02DS
026300980729      *
026400980729     C                   movel     T02uni        DTAD
026500990120      *
026600990120     c                   if          §TADdest <> *blanks
026700990120     c                   movel     §TADdest      W§taddes
026800990120     c                   else
026900990120     c                   movel     §TADdesc      W§taddes
027000990120     c                   endif
027100980729      *
027200980729     C                   ENDSR
027300980730      *****************************************************************
027400980730      *   TESTO
027500980730      *****************************************************************
027600980730     C     SUB_testo     BEGSR
027700981026      *
027800981001      *  SE LE PRIME 5 POSIZIONI DELLA SCHIERA COSTANTE VALGONO "XJUMP" NON UTILIZZO QUELLA SCHIERA
027900981026      *
028000980803      *  Lascio 2 righe dall'oggetto
028100981026     C                   WRITE     DN09P00
028200981026     C                   WRITE     DN09P00
028300980731
028400980803     c     Wtestoda      DO        Wtestoa       XX
028500981002     c                   clear                   PTCde1
028600981002     c                   clear                   PTCde2
028700980803      *
028800981001     c                   if        %SUBST( TES1(xx) :1 :5 ) <> 'XJUMP'
028900981002     c                   movel(P)  TES1(xx)      PTCde1
029000980730     c                   endif
029100981001     c                   if        %SUBST( TES2(xx) :1 :5 ) <> 'XJUMP'
029200981002     c                   movel(P)  TES2(xx)      PTCde2
029300980730     c                   endif
029400981001     c                   if        %SUBST( TES3(xx) :1 :5 ) <> 'XJUMP'
029500981002     c                   move      TES3(xx)      PTCde2
029600980730     c                   endif
029700980730      *
029800980803      *  Eccezioni al testo standard e campi da inserire
029900980803     c                   EXSR      eccetesto
030000980803      *
030100981002     c                   if        PTCde1 <> *blanks
030200981026     C                   WRITE     DN09P00                                      vuota
030300981026     C                   WRITE     DN09PT1
030400980803     c                   endif
030500981002     c                   if        PTCde2 <> *blanks
030600981026     C                   WRITE     DN09PT2
030700980803     c                   endif
030800980730      *
030900980730     c                   ENDDO
031000980730      *
031100980730     C                   ENDSR
031200980730      *****************************************************************
031300980803      *   ECCEZIONI AL TESTO STANDARD
031400980730      *****************************************************************
031500980803     C     ECCEtesto     BEGSR
031600980803
031700981002     c                   SELECT
031800980928
031900980928      *------------------
032000980928      * T E S T O  AL7
032100980928      *------------------
032200981002     c                   WHEN      TEEL(jj) =  'AL7'
032300980928      *
032400021108      *    ELEMENTO 1 - Stampo numero C.A. allegate
032500021108     c                   IF        XX = 01
032600980929     c                   movel     I00nce        w006a
032700980929     c     '0'           check     w006a         yy
032800980929     c                   eval      w006b = %subst(W006a:yy)
032900981026     c                   eval      %subst(PTCde1:71:6) = W006b
033000980928     c                   ENDIF
033100981002
033200981002      *------------------
033300981027      * T E S T O  A8B
033400981002      *------------------
033500981027     c                   WHEN      TEEL(jj) =  'A8B'
033600981005      *
033700021108      *    ELEMENTO 9 - Stampo P.O
033800021108     c                   IF        XX = 09
033900981026     c                   eval      %subst(PTCde1:1:21) = %TRIM(ORGDES) + ','
034000981005     c                   ENDIF
034100980928
034200981005      *------------------
034300981005     C                   ENDSL
034400981005
034500980803     C                   ENDSR
034600980803      *****************************************************************
034700980803      *   FIRMA
034800980803      *****************************************************************
034900980803     C     SUB_firma     BEGSR
035000980803      *
035100981002     c                   movel     Wsocie        PFDsoc
035200981026     c
035300070514     c                   eval      PFCINT = 'Filiale di '  +  ORGdes
035400980805      *  1 Spazio
035500981026     C                   WRITE     DN09P00
035600981026     C                   WRITE     DN09PF1
035700980730      *
035800980730     C                   ENDSR
035900981027      *****************************************************************
036000981027      *   IMPOSTO TIPO MODULO E STAMPANTE
036100981027      *****************************************************************
036200981027     C     IMPPRT        BEGSR
036300981027      *
036400981027     c                   Clear                   COMMAND
036500981027     c                   Clear                   W§TLDstp
036600981027      *
036700981027      * Solo per la Sede imposto una stampante diversa per società
036800981027     c                   IF        I00fgs = 46
036900981027     c                   movel     §TLDstp1      W§TLDstp
037000981027     c                   ENDIF
037100981027      *
037200981027     c                   SELECT
037300981027      *
037400981027     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp <> *blanks
037500981027     c                   movea     CMD(1)        COMMAND
037600981027     c                   eval      %subst(COMMAND:26:10) = §TLDtmod
037700981027     c                   eval      %subst(COMMAND:43:10) = W§TLDstp
037800981027      *
037900981027     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp = *blanks
038000981027     c                   movea     CMD(2)        COMMAND
038100981027     c                   eval      %subst(COMMAND:26:10) = §TLDtmod
038200981027      *
038300981027     c                   WHEN      §TLDtmod = *blanks  and  W§TLDstp <> *blanks
038400981027     c                   movea     CMD(3)        COMMAND
038500981027     c                   eval      %subst(COMMAND:22:10) = W§TLDstp
038600981027      *
038700021120     c***                OTHER
038800021120     c***                movea     CMD(4)        COMMAND
038900990119      *
039000981027     c                   ENDSL
039100981027      *
039200981027      * Se ho impostato qualcosa eseguo l'OVRPRTF
039300021108     c                   IF        COMMAND > *BLANKS
039400981027     c                   call      'QCMDEXC'
039500981027     c                   parm                    COMMAND
039600981027     c                   parm                    LUNG
039700981027     c                   ENDIF
039800981027      *
039900981027     C                   ENDSR
040000981027      *****************************************************************
040100981027      *   AGGANCIO TABELLA TIPO TESTO
040200981027      *****************************************************************
040300981027     C     DECTLD        BEGSR
040400981027      *
040500981027     c                   clear                   tibs02ds
040600981027     c                   clear                   dtld
040700981027      *
040800981027     C                   movel     'C'           t02mod
040900981027     C                   movel     knsif         t02sif
041000981117     C                   movel     'TLD'         t02cod
041100981027     C                   movel(P)  TEEL(jj)      t02ke1
041200981027      *
041300981027     C                   CALL      'TIBS02R'
041400981027     C                   PARM                    KPJBA
041500981027     C                   PARM                    TIBS02DS
041600981027      *
041700981027     C                   movel     T02uni        DTLD
041800981027      *
041900981027     C                   ENDSR
042000981007      *****************************************************************
042100980506      *   ROUTINE INIZIALE
042200980506      *****************************************************************
042300980506     C     *INZSR        BEGSR
042400980506      *
042500980506     C     *ENTRY        PLIST
042600980506     C                   PARM                    KPJBA
042700980730      *
042800980610     C                   MOVEL     KPJBU         fidn00ds
042900980729      *
043000980729     C                   TIME                    W0140
043100980507     C                   MOVE      W0140         WDTGIO
043200980730      *
043300980728      * UDATE IN AAAAMMGG
043400980728     C                   Z-ADD     WDTGIO        G02DAT
043500980728     C                   MOVEL     *BLANK        G02ERR
043600980728     C                   CALL      'XSRDA8'
043700980728     C                   PARM                    WLBDAT
043800980728     C                   MOVEL     G02INV        DATEU
043900980730     C     *iso          MOVEL     DATEU         DATA_oggi
044000990510      ***
044100990510      * Aggancio tabella con dati consuldanni
044200990510      ***
044300990510     c                   clear                   TIBS02DS
044400990510     C                   MOVEL     'C'           t02mod
044500990510     C                   MOVEL     knsif         t02sif
044600990510     C                   MOVEL     'STD'         t02cod
044700990510     C                   MOVEL(P)  '2'           t02ke1
044800990510      *
044900990510     C                   CALL      'TIBS02R'
045000990510     C                   PARM                    KPJBA
045100990510     C                   PARM                    TIBS02DS
045200990510      *
045300990510     C                   clear                   DSTD2
045400990510     C                   If        t02err = *BLANKS
045500990510     C                   MOVEL     T02UNI        DSTD2
045600990510     C                   Endif
045700990510      *
045800990510      *
045900980729     C                   ENDSR
046000980804      *****************************************************************
046100981027** CMD   (Lunga 80)                                                            *
046200021120OVRPRTF FIDN09P FORMTYPE(xxxxxxxxxx) OUTQ(xxxxxxxxxx)                            1
046300021120OVRPRTF FIDN09P FORMTYPE(xxxxxxxxxx)                                             2
046400021120OVRPRTF FIDN09P OUTQ(xxxxxxxxxx)                                                 3
046500021120OVRPRTF FIDN09P SAVE(*YES)                                                       4
046600980803** COTE: 1-3 cod.testo; COSC: 4-6 posizione primo elemento; 7-9 posizione ultimo elemento
046700021108AL7001001
046800021108A8B002010
046900981027** TES1  (Lunga 100)                                                                               *
047000981026Con la presente Vi comunichiamo di aver provveduto all'apertura di N. xxxxxx C. A. di nostra
047100981201In allegato Vi trasmettiamo elenco delle spedizioni coinvolte nell'evento indicato in oggetto per
047200981001Attendiamo di ritorno quanto richiesto allegato alla presente.
047300981001Cordiali saluti.
047400110505                                                            BRT S.p.A.
047500981001----------------------------------------------------------------------------------------------------
047600981117                                                        Spett.le
047700981001XJUMP
047800981026xxxxxxxxxxxxxxxxxxxxx
047900981117       Alleghiamo denuncia vidimata.
048000981027** TES2  (Lunga 55)   TES2 + TES3 = 1 riga            *
048100021108competenza, relative all'evento indicato in oggetto, di  1
048200021108la vidimazione alle competenti autorità.                 2
048300021108XJUMP                                                    3
048400021108XJUMP                                                    4
048500021108XJUMP                                                    5
048600021108XJUMP                                                    6
048700021108XJUMP                                                    7
048800021108XJUMP                                                    8
048900021108XJUMP                                                    9
049000021108XJUMP                                                    10
049100981027** TES3  (Lunga 50)   TES2 + TES3 = 1 riga       *
049200021108 cui Vi alleghiamo documentazione.                       1
049300021108XJUMP                                                    2
049400021108XJUMP                                                    3
049500021108XJUMP                                                    4
049600021108          Ufficio Assicurazioni merci                    5
049700021108XJUMP                                                    6
049800021108      Ufficio ASSICURAZIONI MERCI                        7
049900021108      BOLOGNA SEDE                                       8
050000021108XJUMP                                                    9
050100021108XJUMP                                                    10
