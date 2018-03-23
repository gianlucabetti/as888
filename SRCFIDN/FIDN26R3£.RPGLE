000100981103      *------------------------------------------------------------------------*
000200981023      * Stampa lettere                                                         *
000300981118      *    A14 - Invio copia denuncia al cliente                               *
000400980506      *------------------------------------------------------------------------*
000500980731
000600980731      ****************************************************************
000700981023      *  FASI C.A. FISSATE A PROGRAMMA
000800980731      ****************************************************************
000900980731      ***************************************************************
001000980729
001100980729      ****************************************************************
001200980729      *  RIEPILOGO INDICATORI
001300980729      ***************************************************************
001400981116      * 01   - SALTO PAGINA
001500981126      * 06   - STAMPO interlocutore
001600980729      ****************************************************************
001700980729
001800980730     H DECEDIT('0,') DATEDIT(*DMY/)
001900980729
002000981216     fFIDN26P3  O    E             PRINTER OFLIND(*IN01)     USROPN
002100981118     fFNDCS01L  IF   E           K DISK
002200980918     fTABEL00F  IF   E           K DISK
002300980729
002400980803      *----------------------------------
002500981119     D CMD             S             80    DIM(3)  CTDATA PERRCD(1)             OVRPRTF
002600981119     D COTE            S              3    DIM(1) CTDATA PERRCD(1)              codice testo
002700981119     D COSC            s              6S 0 DIM(1) ALT(cote)                     coordinate schiera
002800981119     D TES1            S            100    DIM(2) CTDATA PERRCD(1)              testo lettera 1 riga
002900981119     D TES2            S             55    DIM(2) CTDATA PERRCD(1)              testo lettera 2 riga
003000981119     D TES3            S             50    DIM(2) CTDATA PERRCD(1)              testo lettera 2 riga
003100980730      *----------------------------------
003200981119     D W35             S             35    DIM(4)                               descr. tipo anomalia
003300981119      *----------------------------------
003400980730     D WLBDAT          DS                  INZ
003500980730     D  g02dat                 1      8  0
003600980730     D  g02inv                 9     16  0
003700980730     D  g02err                17     17
003800980730     D  g02tgi                18     22  0
003900980928      *
004000981119     D wdataoggi       DS
004100981119     D  WAA                    1      4
004200981119     D  WMM                    5      6
004300981216     D  WGG                    7      8
004400981119      *
004500980731     D                 DS
004600980803     D  Wtestoda               1      3  0
004700980803     D  Wtestoa                4      6  0
004800980803     D  Wtestodaa              1      6  0
004900981026      *
005000981118     D FIDN26DS      E DS
005100981119     d   Wnumca                1     14
005200081222      *
005300081222     D FIDNBEDS      E DS
005400981119      *
005500980730     D TIBS02DS      E DS
005600980507     D KPJBA         E DS
005700981027     D DTLD          E DS
005800980729      *----------------------------------
005900980928     D wanno           s              4
006000980730     D wfil            s              3
006100980928     D wnum            s              7
006200981119     D wannop          s              4
006300981119     D wnump           s              7
006400981026     D w007a           s              7
006500990311     D wtibs69r        s              1
006600981027     D command         S             80
006700981216     D lung            S             15  5 inz(80)
006800980730     D XX              s              3  0
006900980731     D YY              s              3  0
007000980731     D JJ              s              3  0
007100980730     D dateu           s              8  0
007200980730     D wdtgio          S              8  0
007300980730     D w0140           S             14  0
007400980918     D w0040           S              4  0
007500980918      *
007600981119     D DATA_oggi       S               D   DATFMT(*eur)
007700981027     D W§TLDstp        S                   LIKE(§TLDstp1)
007800981119     D CODUT           S                   LIKE(TBLkut)
007900980929     D Ktpd            S                   LIKE(DCStpd)
008000980929     D Knkt            S                   LIKE(DCSnkt)
008100980929     D Kstd            S                   LIKE(DCSstd)
008200980929     D Kdim            S                   LIKE(DCSdim)
008300980929     D Khim            S                   LIKE(DCShim)
008400980929     D Knks            S                   LIKE(DCSnks)
008500980929     D Ktrc            S                   LIKE(DCStrc)
008600981028     D Kkey            S                   LIKE(TBLkey)
008700980731      *---------------------------------------------------------------------
008800981023
008900981027      * Aggancio tabella TIPO TESTO
009000981027     c                   EXSR      DECtld
009100981001      *
009200981001      *  Memorizzo le coordinate del testo nelle schiere
009300980803     c                   z-add     1             YY
009400981119     c     'A14'         lookup    COTE(YY)                               30
009500980803     c   30              movel     COSC(YY)      Wtestodaa
009600980803      *
009700980805     c                   EXSR      SUB_INTES                                     intestazione
009800980805     c                   EXSR      SUB_OGGE                                      oggetto
009900980805     c                   EXSR      SUB_TESTO                                     testo
010000980805     c                   EXSR      SUB_FIRMA                                     firma
010100980804
010200981029     c                   clear                   tibs02ds
010300981029     c                   movel     'C'           t02tla
010400981029     c                   call      'TIBS02R'
010500981029     c                   parm                    kpjba
010600981029     c                   parm                    tibs02ds
010700981113
010800990311
010900980506     C                   EVAL      *INLR = *ON
011000980729      *****************************************************************
011100980730      *   INTESTAZIONE
011200980729      *****************************************************************
011300980730     C     SUB_intes     BEGSR
011400980730      *
011500981116     c                   movel     §TLDstp1      W§TLDstp
011600980730      *
011700981007      * DATI del destinatario lettera
011800980928     C                   exsr      RIEcli
011900981103      *
012000981126      * Recupero l'interlocutore
012100981103     c                   exsr      RECpercon
012200981007      *
012300981007      * Località + data
012400981119     C                   movel     I26dic        wdataoggi
012500030514     c*****              eval      PICdal = 'MILANO, '   +  WGG  +  '/'  +  WMM
012600030514     c                   eval      PICdal = 'BOLOGNA, '  +  WGG  +  '/'  +  WMM
012700030514     c                                                   +  '/'  +  WAA
012800030514      *
012900030514      * Firma
013000050208     c***                eval      PFCdit = 'CONSULDANNI SRL     '
013100050208     c                   eval      PFCdit = 'BARTOLINI S.p.A.    '
013200981113      *
013300981113      * Apro file stampa e imposto Tipo Modulo e Stampante
013400981113     c                   EXSR      IMPPRT
013500981113      *
013600980805      * Scrivo l'intestazione
013700981118     c                   WRITE     DN26PI1
013800980730      *
013900980730     C                   ENDSR
014000980731      *****************************************************************
014100980918      *   IMPOSTO DATI CLIENTE PER STAMPA
014200980731      *****************************************************************
014300980918     C     RIEcli        BEGSR
014400980731      *
014500081222      * altrimanti cerco il beneficiario con il programma esterno
014600081222     c                   clear                   fidnbeds
014700081222      *
014800081222     c                   movel     'B'           IBEmod
014900081222     c                   z-add     i26aac        IBEaac
015000081222     c                   z-add     i26fil        IBEfil
015100081222     c                   z-add     i26nca        IBEnca
015200081222     c                   z-add     i26aas        IBEaas
015300081222     c                   z-add     i26lnp        IBElnp
015400081222     c                   z-add     i26nrs        IBEnrs
015500081222     c                   z-add     i26nsp        IBEnsp
015600081222     c                   z-add     i26ksc        IBEksc
015700081222     c                   movel     i26ptr        IBEptr
015800081222     c                   movel     i26flo        IBEflo
015900081222     c                   movel     'S'           IBEtpb
016000081222     c                   If        %subst(knsif:7:1) = 'P'
016100081222     c                   eval      IBEsif = 'P'
016200081222     c                   EndIf
016300081222      *
016400081222     c                   call      'FIDNBER'
016500081222     c                   parm                    fidnbeds
016600081222      *
016700081222     c                   If        oberag <> *blanks
016800081222      *
016900081222     c                   movel     oberag        PICrag
017000081222     c                   movel     obevia        PICind
017100081222     c                   movel     obecap        PICcap
017200081222     c                   movel     obeloc        PICloc
017300081222     c                   movel     obeprv        PICpro
017400081222     c                   movel     obenaz        PICnaz
017500081222     c                   endif
017600980918      *
017700980918     C                   ENDSR
017800980918      *****************************************************************
017900981126      *   RECUPERO IL NOMINATIVO DELL'INTERLOCUTORE
018000980918      *****************************************************************
018100980918     C     RECpercon     BEGSR
018200980918      *
018300980731     c                   CLEAR                   DCSnot
018400981002     c                   CLEAR                   PICpec
018500980731      *
018600980929     C                   movel(P)  'C'           Ktpd
018700980929     C                   movel(P)  Wnumca        Knkt
018800980929     C                   movel(P)  'N'           Kstd
018900980929     C                   clear                   Khim
019000980929     C                   clear                   Kdim
019100980929     C                   clear                   Knks
019200980929     C                   movel     'D'           Ktrc
019300980731      *
019400981026     c     KDCS          CHAIN     FNDCS000                           06
019500981026      *
019600981026     c                   IF        *IN06 = *OFF and  DCSnot <> *BLANKS
019700981103     c                   eval      PICpec  =  'All''attenzione di: '  +  DCSnot
019800981026     c                   eval      *IN06 = *ON
019900981026     c                   ENDIF
020000980731      *
020100980731     C                   ENDSR
020200980730      *****************************************************************
020300980730      *   OGGETTO
020400980730      *****************************************************************
020500980730     C     SUB_ogge      BEGSR
020600980928
020700981119     c                   movel     I26aac        wanno
020800981119     c                   movel     I26fil        wfil
020900981119     c                   movel     I26nca        w007a
021000981119     c     '0'           check     w007a         yy
021100981119     c                   eval      wnum = %subst(W007a:yy)
021200981119      *
021300981119     c                   movel     I26pra        wannop
021400981119     c                   movel     I26prn        w007a
021500980803     c     '0'           check     w007a         yy
021600981119     c                   eval      wnump = %subst(W007a:yy)
021700990120      *
021800990120     C                   eval      POCogg  =    'Avviso Presunto Danno N. '
021900990120     C                                        +  I26DIT  +  ' '  +  %trim(wnump)
022000990120     c                                        +  ' Anno '  +  wannop
022100050927     ***C                                        + '   COMUNICAZIONE ANOMALIA N. '
022200050927     ***c                                        +  wfil  +  '/'   +  %trim(wnum)
022300050927     ***c                                        +  ' Anno '  +  wanno
022400051017     C                   eval      POCog1  =  'COMUNICAZIONE ANOMALIA N. '
022500051017     c                                        +  wfil  +  '/'   +  %trim(wnum)
022600051017     c                                        +  ' Anno '  +  wanno
022700981119      *
022800981119     c                   clear                   wnum
022900981119     c                   movel     I26aad        wanno
023000981119     c                   movel     I26pod        wfil
023100981119     c                   movel     I26nde        w007a
023200981119     c     '0'           check     w007a         yy
023300981119     c                   eval      wnum = %subst(W007a:yy)
023400981119      *
023500990224     C*                  eval      POCog1  =  'Denuncia N. '  +  wanno   +  '/'
023600990224     c*                                        +  wfil  +  '/'  +  %trim(wnum)
023700980730      *
023800980803      *  Oggetto
023900981118     c                   WRITE     DN26PO1
024000980928
024100980729     C                   ENDSR
024200980730      *****************************************************************
024300980730      *   TESTO
024400980730      *****************************************************************
024500980730     C     SUB_testo     BEGSR
024600980804
024700981001      *  SE LE PRIME 5 POSIZIONI DELLA SCHIERA COSTANTE VALGONO "XJUMP" NON UTILIZZO QUELLA SCHIERA
024800980730
024900980803      *  Lascio 2 righe dall'oggetto
025000981118     C                   WRITE     DN26P00
025100981118     C                   WRITE     DN26P00
025200980731
025300980803     c     Wtestoda      DO        Wtestoa       XX
025400981103      *
025500981103      *  Prima dei saluti stampo le note
025600981103     c                   IF        XX = Wtestoa
025700981103     c                   EXSR      SUB_NOTE                                      note
025800981103     c                   ENDIF
025900981103      *
026000981002     c                   clear                   PTCde1
026100981002     c                   clear                   PTCde2
026200980803      *
026300981001     c                   if        %SUBST( TES1(xx) :1 :5 ) <> 'XJUMP'
026400981002     c                   movel(P)  TES1(xx)      PTCde1
026500980730     c                   endif
026600981001     c                   if        %SUBST( TES2(xx) :1 :5 ) <> 'XJUMP'
026700981002     c                   movel(P)  TES2(xx)      PTCde2
026800980730     c                   endif
026900981001     c                   if        %SUBST( TES3(xx) :1 :5 ) <> 'XJUMP'
027000981002     c                   move      TES3(xx)      PTCde2
027100980730     c                   endif
027200980803      *
027300981002     c                   if        PTCde1 <> *blanks
027400981118     C                   WRITE     DN26P00                                      vuota
027500050927     C                   WRITE     DN26P00                                      vuota
027600981118     C                   WRITE     DN26PT1
027700980803     c                   endif
027800981002     c                   if        PTCde2 <> *blanks
027900981118     C                   WRITE     DN26PT2
028000980803     c                   endif
028100980730      *
028200980730     c                   ENDDO
028300980730      *
028400980730     C                   ENDSR
028500981026      *****************************************************************
028600981026      *   NOTE
028700981026      *****************************************************************
028800981026     C     SUB_note      BEGSR
028900981026      *
029000981027     c                   eval      *in31 = *on
029100981027     c                   clear                   yy
029200981027     c                   clear                   W35
029300981103     c                   clear                   PTCde2
029400981026      *
029500981026     C                   movel(P)  'C'           Ktpd
029600981026     C                   movel(P)  Wnumca        Knkt
029700981027     C                   movel     'T'           Kstd
029800981119     C                   movel(P)  'A14'         Knks
029900981026     C                   movel     'D'           Ktrc
030000981027     C                   clear                   Khim
030100981027     C                   clear                   Kdim
030200981026      *
030300990531     c     kdcse         chain     fndcs000                           30
030400981026     c     *in30         DOWeq     *off
030500981026      *
030600981026      * Ogni 3 rcd letti scrivo
030700981215     c                   IF        yy = 3
030800981215     c                   exsr      noteprt
030900981215     c                   ENDIF
031000981026      *
031100981026     c                   add       1             yy
031200981026     c                   movel     dcsnot        W35(yy)
031300990531     c     kdcse         reade     fndcs000                               30
031400981026     c                   ENDDO
031500981026      *
031600981215      * Ogni scrivo rcd residui
031700981215     c                   IF        yy > *zeros
031800981215     c                   exsr      noteprt
031900981215     c                   ENDIF
032000981026      *
032100981026     C                   ENDSR
032200981215      *****************************************************************
032300981215      *   STAMPA NOTE
032400981215      *****************************************************************
032500981215     C     NOTEPRT       BEGSR
032600981215      *
032700981215     c                   eval      PTCde2 = W35(1)  +  W35(2)  +  W35(3)
032800981215     c                   IF        PTCde2 <> *blanks
032900981215      *
033000981215      * La prima volta lascio uno spazio
033100981215     c                   If        *IN31 = *ON
033200981215     c                   WRITE     DN26P00
033300981215     c                   eval      *in31 = *off
033400981215     c                   Endif
033500981215      *
033600981215     c                   WRITE     DN26PT2
033700981215     c                   clear                   yy
033800981215     c                   clear                   W35
033900981215     c                   clear                   PTCde2
034000981215     c                   ENDIF
034100981215      *
034200981215     C                   ENDSR
034300980803      *****************************************************************
034400980803      *   FIRMA
034500980803      *****************************************************************
034600980803     C     SUB_firma     BEGSR
034700980803      *
034800980805      *  1 Spazio
034900981118     C                   WRITE     DN26P00
035000981118     C                   WRITE     DN26PF1
035100980730      *
035200980730     C                   ENDSR
035300981027      *****************************************************************
035400981027      *   IMPOSTO TIPO MODULO E STAMPANTE
035500981027      *****************************************************************
035600981027     C     IMPPRT        BEGSR
035700981027      *
035800981027     c                   Clear                   COMMAND
035900981113      *
036000981027     c                   SELECT
036100981027      *
036200981118     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp <> *blanks
036300981113     c                   movea     CMD(1)        COMMAND
036400981027     c                   eval      %subst(COMMAND:26:10) = §TLDtmod
036500981027     c                   eval      %subst(COMMAND:43:10) = W§TLDstp
036600981027      *
036700981118     c                   WHEN      §TLDtmod <> *blanks  and  W§TLDstp = *blanks
036800981113     c                   movea     CMD(2)        COMMAND
036900981027     c                   eval      %subst(COMMAND:26:10) = §TLDtmod
037000981027      *
037100981118     c                   WHEN      §TLDtmod = *blanks  and  W§TLDstp <> *blanks
037200981027     c                   movea     CMD(3)        COMMAND
037300981027     c                   eval      %subst(COMMAND:22:10) = W§TLDstp
037400981116      *
037500981027     c                   ENDSL
037600981027      *
037700981027      * Se ho impostato qualcosa eseguo l'OVRPRTF
037800981216     c                   IF        COMMAND <> *blanks
037900981027     c                   call      'QCMDEXC'
038000981027     c                   parm                    COMMAND
038100981027     c                   parm                    LUNG
038200981027     c                   ENDIF
038300981027      *
038400981113      *  Apro file di stampa
038500981216     c                   open      FIDN26P3
038600981113      *
038700981027     C                   ENDSR
038800981027      *****************************************************************
038900981027      *   AGGANCIO TABELLA TIPO TESTO
039000981027      *****************************************************************
039100981027     C     DECTLD        BEGSR
039200981027      *
039300981027     c                   clear                   tibs02ds
039400981027     c                   clear                   dtld
039500981027      *
039600981027     C                   movel     'C'           t02mod
039700981027     C                   movel     knsif         t02sif
039800990302     C                   movel     'TLD'         t02cod
039900981119     C                   movel(P)  'A14'         t02ke1
040000981027      *
040100981027     C                   CALL      'TIBS02R'
040200981027     C                   PARM                    KPJBA
040300981027     C                   PARM                    TIBS02DS
040400981027      *
040500981027     C                   movel     T02uni        DTLD
040600981027      *
040700981027     C                   ENDSR
040800981116      *****************************************************************
040900981116      *   ROUTINE INIZIALE
041000981116      *****************************************************************
041100981116     C     *INZSR        BEGSR
041200981116      *
041300981116     C     *ENTRY        PLIST
041400981116     C                   PARM                    KPJBA
041500981116      *
041600981118     C                   MOVEL     KPJBU         FIDN26DS
041700981116      *
041800981119     c                   z-add     1             codut
041900981116      *
042000981116     C                   TIME                    W0140
042100981116      *
042200981119      * UDATE IN GGMMAAAA
042300981116     C                   MOVE      W0140         WDTGIO
042400981116      * UDATE IN AAAAMMGG
042500981116     C                   Z-ADD     WDTGIO        G02DAT
042600981116     C                   MOVEL     *BLANK        G02ERR
042700981116     C                   CALL      'XSRDA8'
042800981116     C                   PARM                    WLBDAT
042900981116     C                   MOVEL     G02INV        DATEU
043000981116     C     *iso          MOVEL     DATEU         DATA_oggi
043100981116
043200981116     C     KDKA          klist
043300981119     C                   KFLD                    I26aac
043400981119     C                   KFLD                    I26fil
043500981119     C                   KFLD                    I26nca
043600981116     C                   KFLD                    Ktrc
043700981116      *
043800981116     C     KDCS          klist
043900981116     C                   KFLD                    Ktpd
044000981116     C                   KFLD                    Knkt
044100981116     C                   KFLD                    Kstd
044200990531      *
044300990531     C     KDCSE         klist
044400990531     C                   KFLD                    KTPD
044500990531     C                   KFLD                    KNKT
044600990531     C                   KFLD                    KSTD
044700990531     C                   KFLD                    KDIM
044800990531     C                   KFLD                    KHIM
044900990531     C                   KFLD                    KNKS
045000990531     C                   KFLD                    KTRC
045100981116      *
045200981116     c     kbol          klist
045300981119     c                   kfld                    I26aas
045400981119     c                   kfld                    I26lnp
045500981119     c                   kfld                    I26nrs
045600981119     c                   kfld                    I26nsp
045700981116
045800981116     C                   ENDSR
045900980804      *****************************************************************
046000981113** CMD   (Lungh. 80)                                                           *
046100000504OVRPRTF FIDN26P3 FORMTYPE(xxxxxxxxxx) OUTQ(xxxxxxxxxx)
046200000504OVRPRTF FIDN26P3 FORMTYPE(xxxxxxxxxx)
046300000504OVRPRTF FIDN26P3 OUTQ(xxxxxxxxxx)
046400980803** COTE: 1-3 cod.testo; COSC: 4-6 posizione primo elemento; 7-9 posizione ultimo elemento
046500981118A14001002
046600980731** TES1  (Lungh. 100)                                                                              *
046700030514Come da Vostra richiesta in allegato trasmettiamo copia della denuncia relativa all'evento
046800981118A Vostra disposizione per eventuali chiarimenti porgiamo cordiali saluti.
046900980731** TES2  (Lungh. 55)  TES2 + TES3 = 1 riga            *
047000030514indicato in oggetto.                                     1
047100981119XJUMP                                                    2
047200981023** TES3  (Lungh. 50)  TES2 + TES3 = 1 riga       *
047300981023XJUMP                                               1
047400981119XJUMP                                               2
