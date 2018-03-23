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
006000980729      *----------------------------------
006100981028     D WAanno          s              4
006200981028     D WAfil           s              3
006300981028     D WAnum           s              7
006400981026     D command         S             80
006500980730     D w007a           s              7
006600980929     D w006a           s              6
006700980929     D w006b           s              6
006800980803     D wdataoggi       S             10
006900980928     D wdataeur        S             10
007000980803     D wsocie          S             20
007100990119     D lung            S             15  5 inz(80)
007200981028     D Wfil            s              3  0
007300980730     D XX              s              3  0
007400980731     D YY              s              3  0
007500980731     D JJ              s              3  0
007600980730     D dateu           s              8  0
007700980730     D wdtgio          S              8  0
007800980730     D w0140           S             14  0
007900981027     D W§TLDstp        S                   LIKE(§TLDstp1)
008000990120     D W§TADdes        S                   LIKE(§TADdest)
008100980730      *
008200980730     D DATA_eur        S               D   DATFMT(*eur)
008300980730     D DATA_oggi       S               D   DATFMT(*eur)
008400980918      *
008500981002     D SEDE_uas        S                   LIKE(PICrag)
008600980928     d                                     INZ('Ufficio ASSICURAZIONI MERCE')
008700981002     D SEDE_ind        S                   LIKE(PICind)
008800980928     d                                     INZ('BOLOGNA SEDE')
008900980731      *---------------------------------------------------------------------
009000981026
009100981204      * Aggancio anagrafico fasi per tipi testo
009200980731     c     I00fca        CHAIN     FNDFA000                           30
009300981204
009400981001      * Memorizzo in schiera i tipi testo da elaborare
009500980731     c                   z-add     1             YY
009600980731     c     DFAle1        lookup    COTE(YY)                               30
009700980731     c   30              movel     DFALE1        TEEL(1)
009800980731     c                   z-add     1             YY
009900980731     c     DFAle2        lookup    COTE(YY)                               30
010000980731     c   30              movel     DFALE2        TEEL(2)
010100981204
010200980805      * Elaboro i tipi testo
010300980731     c     1             DO        2             JJ
010400981027     c                   IF        teel(JJ) <> *blanks
010500981027      *
010600981027      * Aggancio tabella TIPO TESTO
010700981027     c                   EXSR      DECtld
010800981001      *
010900981001      *  Memorizzo le coordinate del testo nelle schiere
011000980803     c                   z-add     1             YY
011100980803     c     TEEL(jj)      lookup    COTE(YY)                               30
011200980803     c   30              movel     COSC(YY)      Wtestodaa
011300980803      *
011400980805     c                   EXSR      SUB_INTES                                     intestazione
011500980805     c                   EXSR      SUB_OGGE                                      oggetto
011600980805     c                   EXSR      SUB_TESTO                                     testo
011700980805     c                   EXSR      SUB_FIRMA                                     firma
011800980803      *
011900981027     c                   ENDIF
012000981204      *
012100981204     c                   CLOSE     FIDN09P
012200980731     c                   ENDDO
012300981026      *
012400981029     c                   clear                   tibs02ds
012500981029     c                   movel     'C'           t02tla
012600981029     c                   call      'TIBS02R'
012700981029     c                   parm                    kpjba
012800981029     c                   parm                    tibs02ds
012900981204      *
013000980506     C                   EVAL      *INLR = *ON
013100980729      *****************************************************************
013200980730      *   INTESTAZIONE
013300980729      *****************************************************************
013400980730     C     SUB_intes     BEGSR
013500980729      *
013600980730      * DATI del P.O.
013700981028     c                   SELECT
013800981028      *
013900021108     c                   WHEN        TEEL(jj) = 'AL7'
014000981028     c                   z-add     I00fgs        Wfil
014100981028      *
014200981028     c                   WHEN        TEEL(jj) = 'A8B'
014300981028     c                   z-add     I00fce        Wfil
014400981028      *
014500981028     c                   ENDSL
014600981028      *
014700981028     c     WFIL          CHAIN     AZORG                              30
014800980803      *
014900981026      * Imposto Società
015000981026     c                   movel     KBAR          Wsocie
015100981027      *
015200981027      * Imposto Tipo Modulo e Stampante
015300981027     c                   EXSR      IMPPRT
015400981027      *
015500981026      * Apro file stampa
015600981026     c                   OPEN      FIDN09P
015700981026      *
015800981026      * Imposto mittente e destinatario
015900981002     c                   SELECT
016000981007      *
016100021108     c                   WHEN      TEEL(jj) = 'AL7'
016200981117     c                   movel     'TEL:'        PICtel
016300981117     c                   movel     'FAX:'        PICfax
016400070514     c                   eval      PICint  =  'Filiale di'
016500981027     c                   eval      PICin1  =  'Spett.le'
016600981002     c                   movel     SEDE_uas      PICrag
016700981002     c                   movel     SEDE_ind      PICind
016800981002     c                   movel     *blanks       PICcap
016900981002     c                   movel     *blanks       PICloc
017000981002     c                   movel     *blanks       PICpro
017100981002     c                   movel     *blanks       PICnaz
017200981026      *
017300981026     c                   movel     ORGdes        PICpod
017400981026     c                   movel     ORGind        PICpoi
017500981026     c                   movel     ORGcpf        PICpoc
017600981026     c                   movel     ORGloc        PICpol
017700981026     c                   movel     ORGpro        PICpop
017800981026     c                   movel     ORGtel        PICpot
017900981026     c                   movel     ORGfax        PICpof
018000981026
018100981027     c                   WHEN      TEEL(jj) = 'A8B'
018200070514     c                   eval      PICin1  =  'Spett.le Filiale di'
018300981007     c                   movel     ORGDES        PICrag
018400981007     c                   movel     *blanks       PICind
018500981007     c                   movel     *blanks       PICcap
018600981007     c                   movel     *blanks       PICloc
018700981007     c                   movel     *blanks       PICpro
018800981007     c                   movel     *blanks       PICnaz
018900981026      *
019000981026     c                   movel     SEDE_uas      PICpod
019100981026     c                   movel     sEDE_ind      PICpoi
019200981026     c                   clear                   PICpoc
019300981026     c                   clear                   PICpol
019400981026     c                   clear                   PICpop
019500981026     c                   clear                   PICpot
019600981026     c                   clear                   PICpof
019700981026
019800981002     c                   ENDSL
019900981007      *
020000981026      * Imposto Località + data
020100981007     C     *eur          movel     DATA_oggi     wdataoggi
020200981007      *
020300021108     c                   IF        TEEL(jj) = 'A8B'
020400981007     c                   eval      PICdal = 'BOLOGNA, ' + wdataoggi
020500981007     C                   ELSE
020600981007     c                   eval      PICdal = %TRIM(ORGDES) + ', ' + wdataoggi
020700981007     c                   ENDIF
020800980918      *
020900981026      * Imposto Note intestazione
021000980805      *
021100981026      * Scrivo l'intestazione
021200981026     c                   WRITE     DN09PI1
021300980730      *
021400980730     C                   ENDSR
021500980730      *****************************************************************
021600980730      *   OGGETTO
021700980730      *****************************************************************
021800980730     C     SUB_ogge      BEGSR
021900980928
022000980928     c                   SELECT
022100980928
022200981026      *--------------
022300981027     c                   WHEN      TEEL(jj) = 'AL7'  or  TEEL(jj) = 'A8B'
022400981026      *
022500981026     c                   exsr      CHTAD
022600980929      *
022700981028     c                   movel     I00aae        WAanno
022800980928     c                   movel     I00nev        w007a
022900980928     c     '0'           check     w007a         yy
023000981028     c                   eval      WAnum = %subst(W007a:yy)
023100980928     C                   if        I00dev > *zeros
023200980928     c     *iso          movel     I00dev        DATA_eur
023300980928     c     *eur          movel     DATA_eur      Wdataeur
023400980928     C                   endif
023500980928      *
023600990120     C                   eval      POCogg =   'Evento N.  '  +  %trim(WAnum)
023700990120     c                                       +  ' anno '  +  WAanno +  '  del '
023800990120     c                                       +  wdataeur  +  '    '  +  W§taddes
023900990120      *
024000981026     c                   WRITE     DN09PO1
024100981007
024200981026      *--------------
024300980928     c                   ENDSL
024400980928
024500980729     C                   ENDSR
024600980729      *****************************************************************
024700981026      *  REPERIMENTO DESCRIZIONE ANOMALIA EVENTO
024800980729      *****************************************************************
024900980729     C     CHTAD         BEGSR
025000980729      *
025100980730     c                   clear                   tibs02ds
025200980729     c                   clear                   dtad
025300980729      *
025400980729     C                   movel     'C'           t02mod
025500980729     C                   movel     knsif         t02sif
025600980729     C                   movel     'TAD'         t02cod
025700981026     C                   movel(P)  I00tae        t02ke1
025800980729      *
025900980729     C                   CALL      'TIBS02R'
026000980729     C                   PARM                    KPJBA
026100980730     C                   PARM                    TIBS02DS
026200980729      *
026300980729     C                   movel     T02uni        DTAD
026400990120      *
026500990120     c                   if          §TADdest <> *blanks
026600990120     c                   movel     §TADdest      W§taddes
026700990120     c                   else
026800990120     c                   movel     §TADdesc      W§taddes
026900990120     c                   endif
027000980729      *
027100980729     C                   ENDSR
027200980730      *****************************************************************
027300980730      *   TESTO
027400980730      *****************************************************************
027500980730     C     SUB_testo     BEGSR
027600981026      *
027700981001      *  SE LE PRIME 5 POSIZIONI DELLA SCHIERA COSTANTE VALGONO "XJUMP" NON UTILIZZO QUELLA SCHIERA
027800981026      *
027900980803      *  Lascio 2 righe dall'oggetto
028000981026     C                   WRITE     DN09P00
028100981026     C                   WRITE     DN09P00
028200980731
028300980803     c     Wtestoda      DO        Wtestoa       XX
028400981002     c                   clear                   PTCde1
028500981002     c                   clear                   PTCde2
028600980803      *
028700981001     c                   if        %SUBST( TES1(xx) :1 :5 ) <> 'XJUMP'
028800981002     c                   movel(P)  TES1(xx)      PTCde1
028900980730     c                   endif
029000981001     c                   if        %SUBST( TES2(xx) :1 :5 ) <> 'XJUMP'
029100981002     c                   movel(P)  TES2(xx)      PTCde2
029200980730     c                   endif
029300981001     c                   if        %SUBST( TES3(xx) :1 :5 ) <> 'XJUMP'
029400981002     c                   move      TES3(xx)      PTCde2
029500980730     c                   endif
029600980730      *
029700980803      *  Eccezioni al testo standard e campi da inserire
029800980803     c                   EXSR      eccetesto
029900980803      *
030000981002     c                   if        PTCde1 <> *blanks
030100981026     C                   WRITE     DN09P00                                      vuota
030200981026     C                   WRITE     DN09PT1
030300980803     c                   endif
030400981002     c                   if        PTCde2 <> *blanks
030500981026     C                   WRITE     DN09PT2
030600980803     c                   endif
030700980730      *
030800980730     c                   ENDDO
030900980730      *
031000980730     C                   ENDSR
031100980730      *****************************************************************
031200980803      *   ECCEZIONI AL TESTO STANDARD
031300980730      *****************************************************************
031400980803     C     ECCEtesto     BEGSR
031500980803
031600981002     c                   SELECT
031700980928
031800980928      *------------------
031900980928      * T E S T O  AL7
032000980928      *------------------
032100981002     c                   WHEN      TEEL(jj) =  'AL7'
032200980928      *
032300021108      *    ELEMENTO 1 - Stampo numero C.A. allegate
032400021108     c                   IF        XX = 01
032500980929     c                   movel     I00nce        w006a
032600980929     c     '0'           check     w006a         yy
032700980929     c                   eval      w006b = %subst(W006a:yy)
032800981026     c                   eval      %subst(PTCde1:71:6) = W006b
032900980928     c                   ENDIF
033000981002
033100981002      *------------------
033200981027      * T E S T O  A8B
033300981002      *------------------
033400981027     c                   WHEN      TEEL(jj) =  'A8B'
033500981005      *
033600021108      *    ELEMENTO 9 - Stampo P.O
033700021108     c                   IF        XX = 09
033800981026     c                   eval      %subst(PTCde1:1:21) = %TRIM(ORGDES) + ','
033900981005     c                   ENDIF
034000980928
034100981005      *------------------
034200981005     C                   ENDSL
034300981005
034400980803     C                   ENDSR
034500980803      *****************************************************************
034600980803      *   FIRMA
034700980803      *****************************************************************
034800980803     C     SUB_firma     BEGSR
034900980803      *
035000981002     c                   movel     Wsocie        PFDsoc
035100981026     c
035200070514     c                   eval      PFCINT = 'Filiale di '  +  ORGdes
035300980805      *  1 Spazio
035400981026     C                   WRITE     DN09P00
035500981026     C                   WRITE     DN09PF1
035600980730      *
035700980730     C                   ENDSR
035800981027      *****************************************************************
035900981027      *   IMPOSTO TIPO MODULO E STAMPANTE
036000981027      *****************************************************************
036100981027     C     IMPPRT        BEGSR
036200981027      *
036300981027     c                   Clear                   COMMAND
036400981027     c                   Clear                   W§TLDstp
036500981027      *
036600981027      * Solo per la Sede imposto una stampante diversa per società
036700981027     c                   IF        I00fgs = 46
036800981027     c                   movel     §TLDstp1      W§TLDstp
036900981027     c                   ENDIF
037000981027      *
037100981027     c                   SELECT
037200981027      *
037300981027     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp <> *blanks
037400981027     c                   movea     CMD(1)        COMMAND
037500981027     c                   eval      %subst(COMMAND:26:10) = §TLDtmod
037600981027     c                   eval      %subst(COMMAND:43:10) = W§TLDstp
037700981027      *
037800981027     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp = *blanks
037900981027     c                   movea     CMD(2)        COMMAND
038000981027     c                   eval      %subst(COMMAND:26:10) = §TLDtmod
038100981027      *
038200981027     c                   WHEN      §TLDtmod = *blanks  and  W§TLDstp <> *blanks
038300981027     c                   movea     CMD(3)        COMMAND
038400981027     c                   eval      %subst(COMMAND:22:10) = W§TLDstp
038500981027      *
038600021120     c***                OTHER
038700021120     c***                movea     CMD(4)        COMMAND
038800990119      *
038900981027     c                   ENDSL
039000981027      *
039100981027      * Se ho impostato qualcosa eseguo l'OVRPRTF
039200021108     c                   IF        COMMAND > *BLANKS
039300981027     c                   call      'QCMDEXC'
039400981027     c                   parm                    COMMAND
039500981027     c                   parm                    LUNG
039600981027     c                   ENDIF
039700981027      *
039800981027     C                   ENDSR
039900981027      *****************************************************************
040000981027      *   AGGANCIO TABELLA TIPO TESTO
040100981027      *****************************************************************
040200981027     C     DECTLD        BEGSR
040300981027      *
040400981027     c                   clear                   tibs02ds
040500981027     c                   clear                   dtld
040600981027      *
040700981027     C                   movel     'C'           t02mod
040800981027     C                   movel     knsif         t02sif
040900981117     C                   movel     'TLD'         t02cod
041000981027     C                   movel(P)  TEEL(jj)      t02ke1
041100981027      *
041200981027     C                   CALL      'TIBS02R'
041300981027     C                   PARM                    KPJBA
041400981027     C                   PARM                    TIBS02DS
041500981027      *
041600981027     C                   movel     T02uni        DTLD
041700981027      *
041800981027     C                   ENDSR
041900981007      *****************************************************************
042000980506      *   ROUTINE INIZIALE
042100980506      *****************************************************************
042200980506     C     *INZSR        BEGSR
042300980506      *
042400980506     C     *ENTRY        PLIST
042500980506     C                   PARM                    KPJBA
042600980730      *
042700980610     C                   MOVEL     KPJBU         fidn00ds
042800980729      *
042900980729     C                   TIME                    W0140
043000980507     C                   MOVE      W0140         WDTGIO
043100980730      *
043200980728      * UDATE IN AAAAMMGG
043300980728     C                   Z-ADD     WDTGIO        G02DAT
043400980728     C                   MOVEL     *BLANK        G02ERR
043500980728     C                   CALL      'XSRDA8'
043600980728     C                   PARM                    WLBDAT
043700980728     C                   MOVEL     G02INV        DATEU
043800980730     C     *iso          MOVEL     DATEU         DATA_oggi
043900990510      *
044000990510      *
044100980729     C                   ENDSR
044200980804      *****************************************************************
044300981027** CMD   (Lunga 80)                                                            *
044400021120OVRPRTF FIDN09P FORMTYPE(xxxxxxxxxx) OUTQ(xxxxxxxxxx)                            1
044500021120OVRPRTF FIDN09P FORMTYPE(xxxxxxxxxx)                                             2
044600021120OVRPRTF FIDN09P OUTQ(xxxxxxxxxx)                                                 3
044700021120OVRPRTF FIDN09P SAVE(*YES)                                                       4
044800980803** COTE: 1-3 cod.testo; COSC: 4-6 posizione primo elemento; 7-9 posizione ultimo elemento
044900021108AL7001001
045000021108A8B002010
045100981027** TES1  (Lunga 100)                                                                               *
045200981026Con la presente Vi comunichiamo di aver provveduto all'apertura di N. xxxxxx C. A. di nostra
045300981201In allegato Vi trasmettiamo elenco delle spedizioni coinvolte nell'evento indicato in oggetto per
045400981001Attendiamo di ritorno quanto richiesto allegato alla presente.
045500981001Cordiali saluti.
045600110505                                                            BRT S.p.A.
045700981001----------------------------------------------------------------------------------------------------
045800981117                                                        Spett.le
045900981001XJUMP
046000981026xxxxxxxxxxxxxxxxxxxxx
046100981117       Alleghiamo denuncia vidimata.
046200981027** TES2  (Lunga 55)   TES2 + TES3 = 1 riga            *
046300021108competenza, relative all'evento indicato in oggetto, di  1
046400021108la vidimazione alle competenti autorità.                 2
046500021108XJUMP                                                    3
046600021108XJUMP                                                    4
046700021108XJUMP                                                    5
046800021108XJUMP                                                    6
046900021108XJUMP                                                    7
047000021108XJUMP                                                    8
047100021108XJUMP                                                    9
047200021108XJUMP                                                    10
047300981027** TES3  (Lunga 50)   TES2 + TES3 = 1 riga       *
047400021108 cui Vi alleghiamo documentazione.                       1
047500021108XJUMP                                                    2
047600021108XJUMP                                                    3
047700021108XJUMP                                                    4
047800021108          Ufficio Assicurazioni merci                    5
047900021108XJUMP                                                    6
048000021108      Ufficio ASSICURAZIONI MERCI                        7
048100021108      BOLOGNA SEDE                                       8
048200021108XJUMP                                                    9
048300021108XJUMP                                                    10
