000100051116      *PARMS  TGTRLS(*PRV)
000200000000     H*------------------------------------------------------------------------*
000300131023     H* Cap/località con orari servizi per filiale - Crea File                 *
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900000000     FTABEL00F  IF   E           K DISK
001000131024     FTNTBE01L  iF   E           K DISK
001100970707     FAZCPC01L  IF   E           K DISK
001200980210     FAZCPL01L  IF   E           K DISK
001300121012     FAZCPE01L  IF   E           K DISK
001401131022     FWADLY10F  O    E           K DISK    USROPN
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* COMANDI CLP
002000000000     D*-------------------
002100091211     D CMD             S             80    DIM(4) CTDATA PERRCD(1)
002200960709     D*-------------------
002300980216     D* PROVINCIE
002400960709     D*-------------------
002500051116     d PRV             s                   dim(600)  inz  like(DLYprv)          *PROVINCIA
002600051116     d Pcts            s                   dim(600)  inz  like(§PRcts)          *CODICE TASSAZIONE
002700131024     d*Pfcr            s                   dim(600)  inz  like(DLYfcr)          *CAPOLUOGO DI REGION
002800131024     d*Prap            s                   dim(600)  inz  like(DLYrap)          *REGIONE APPARTENENZ
002900131024     d*Pcor            s                   dim(600)  inz  like(DLYcor)          *ORDINAMENTO
003500000000     D*------------------------------------------------------------------------*
003600000000     D* INPUT
003700000000     D*------------------------------------------------------------------------*
003900980210     D* DS REPERIMENTO DATI UTENTE
004000980210     D*-------------------
004100020919     D TIBS34DS      E DS                                                       *Profili utente
004200020919     D DDATIUTE      E DS                                                       *Dati utente
004300020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004800971020     D*------------------
005200960709     D*-------------------
005300960709     D* DS PROVINCIE
005400960709     D*-------------------
005500960709     D DSPR          E DS
005600000000     D*-------------------
005700000000     D* DS CODICI TASSAZIONE
005800000000     D*-------------------
005900000000     D DSCT          E DS
006000020925     D*-------------------
006100020925     D* DS STATISTICHE DATI DIVERSI
006200020925     D*-------------------
006300020925     D DSDD          E DS
006400980210     D*------------------
006500980210     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600980210     D*------------------
006700980210     D WLBDA8          DS
006800980210     D  G08DAT                 1      8  0
006900980210     D  G08INV                 9     16  0
007000980210     D  G08ERR                17     17
007100980210     D  G08TGI                18     22  0
007200000000     D*-------------------
007300000000     D* DS PARAMETRI DI LANCIO
007400000000     D*-------------------
007500131023     D tisi6ods      e DS
007501131024
007502140109     D tisio9ds      e DS
008900980210     D*-------------------
009000980210     D* ARCHITETTURA
009100980210     D*-------------------
009200980210     D KPJBA         E DS
010100051116      *
010200051116      * V A R I A B I L I
010300051116      *
010400051116     d XX              s              3  0 inz
010403131024     d wtip            s              1    inz
010404140109     d s_MIISC         s                   like(DLYMIISC)
010405140109     d s_MXFSC         s                   like(DLYMXFSC)
010406140109     d s_STISC         s                   like(DLYSTISC)
010407140109     d s_STFSC         s                   like(DLYSTFSC)
010408140110     d s_trzs          s                   like(DLYtrzs)
010409140109
010410131023       // -?Stringa SQL da eseguire?
010411131023     d wSQL            s           5000    inz  varying
010500000000     C*------------------------------------------------------------------------*
010600000000     C* MAIN LINE
010700000000     C*------------------------------------------------------------------------*
010800051116      *
010900051116      * Carica le tabelle occorrenti
011000051116     c                   exsr      CarTAB
011100020925     C*
011500020925     C*
011600050318     C* OPERAZIONI INIZIALI APERTURA FILE
011700020925     C                   EXSR      OPNFIL
011701131023     c* creazione record con standard di filiale
011704131023     c                   eval      wtip='S'
011705131023     c                   exsr      credly
011800000000     C*
011900980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -CAP-
012000980216IF  1C     $ERR          IFEQ      '0'                                          *NO USCITA
012100000000     C                   EXSR      SETFIL
012200000000     C*
012300000000     C* CICLO FINO A FINE FILE
012400000000DO  2C     $FINE         DOWEQ     '0'
012500980216     C*
012600980216     C* OPERAZIONI PER NUOVO CAP
012700980216     C                   EXSR      NEWCAP
012800980216     C*
012900980216     C* CICLO FINO A ROTTURA CAP
013000980216DO  3C     $FINE         DOWEQ     '0'
013100980216     C     CPCNAR        ANDEQ     DEPNAR
013200980216     C     CPCCAP        ANDEQ     DEPCAP
013300980302     C*
013600131023     C                   Z-ADD     CPClna        deplna
013800121015     C                   Z-ADD     CPCLOL        deplol                         *LINEA OLTRE
014000121015     C                   Z-ADD     CPCLOS        deplos                         *LINEA sotto
014400980302     C*
015500121015     c
015600131023     c* verifico se presenti eccezioni per la linea richiesta
015700121015     c                   if        cpcese='S'
015800131023     c     keycpe        setll     azcpe01l
015801131023     c     keycpe        reade     azcpe01l
015900131023     c                   dow       not %eof(azcpe01l)
015901131023     c                   if        cpeatb=*blanks
015904131023     c                   if        si6ofel=cpelna
016000131023     C                   Z-ADD     CPeLNA        deplna                         *LINEA ARRIVO
016001131023     c                   endif
016002131023     c                   if        si6ofel=cpelol
016003131023     C                   Z-ADD     CPeLol        deplol                         *LINEA ARRIVO
016004131023     c                   endif
016005131023     c                   if        si6ofel=cpelos
016006131023     C                   Z-ADD     CPeLos        deplos                         *LINEA ARRIVO
016007131023     c                   endif
017100121012     c                   endif
017101131023     c     keycpe        reade     azcpe01l
017102131023     c                   enddo
017200121015     c                   endif
017300121012     c
018400980216     C*
018500980216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA -LOCALITA'-
018600980216     C                   EXSR      SETCPL
018700980216     C*
018800980216     C* CICLO FINO A FINE FILE
018900980216DO  4C     $FINL         DOWEQ     '0'
019000980216     C*
019500980302     C*
019600980302     C* CALCOLA LA LINEA ARRIVO DA CONSIDERARE
019700980302IF  5C     CPLLIV        IFEQ      'L'                                          *LIVELLO LOCALITA'
019800980302     C                   Z-ADD     CPLLNA        WILNA                          *LINEA ARRIVO
019900980302     C                   Z-ADD     CPLLOL        WILOL                          *LINEA OLTRE
020000121012     C                   Z-ADD     CPLLOs        WILOs                          *LINEA sotto
020300980220     C                   MOVEL     CPLISO        WISO                           *DATI LOCALITA'
020500980220X   5C                   ELSE                                                   *LIVELLO CAP
020600980220     C                   MOVEL     CPCISO        WISO                           *DATI CAP
020800121015     C                   Z-ADD     DEPlna        Wilna
020900121015     C                   Z-ADD     DEPlol        Wilol
021000121015     C                   Z-ADD     DEPlos        Wilos
021100980220E   5C                   ENDIF
023900121015     c
023901131023     c                   movel     cplloc        wloc
023903131023     c* creo record solo se la filiale da elaborare è presente nelle linee
023904131023     c                   if        si6ofel=wilna
023905131023     c                             or si6ofel=wilol
023906131023     c                             or si6ofel=wilos
023907131024     c                   eval      wtip=' '
024000020925     C                   EXSR      CREDLY
024001131023     c                   endif
025000980216     C*
025100980216     C* LETTURA RECORD SUCCESSIVO -LOCALITA'-
025200980216     C                   EXSR      LETCPL
025300980216E   4C                   ENDDO                                                  *FINE -LOCALITA'-
025400000000     C*
025500980216     C* LETTURA RECORD SUCCESSIVO -CAP-
025600000000     C                   EXSR      LETFIL
025700980216E   3C                   ENDDO                                                  *ROTTURA CAP
025800980216E   2C                   ENDDO                                                  *FINE -CAP-
025900000000E   1C                   ENDIF
026000000000     C*
026100000000     C* OPERAZIONI DI CHIUSURA
026200971020     C                   EXSR      FINSR
026400000000     C*
026500000000     C                   SETON                                        LR
026600000000     C*------------------------------------------------------------------------*
026700980216     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -CAP-
026800000000     C*------------------------------------------------------------------------*
026900000000     C     SETFIL        BEGSR
027000000000     C*
027100970707     C                   Z-ADD     P96VER        KCCVER
027200131023     C     KEYCPC        SETLL     AZCPC01L
027300131023IF  1C                   IF        not %equal(azcpc01l)
027400000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
027500000000X   1C                   ELSE
027600000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
027700000000E   1C                   ENDIF
027800000000     C*
027900000000     C                   ENDSR
028000000000     C*------------------------------------------------------------------------*
028100980216     C* LETFIL - LETTURA PROSSIMO RECORD -CAP-
028200000000     C*------------------------------------------------------------------------*
028300000000     C     LETFIL        BEGSR
028400000000     C*
028500000000     C* LEGGE FINO A:
028600000000     C*     - FINE FILE
028700000000     C*     - TROVATO RECORD VALIDO
028800000000     C                   MOVEL     'N'           WRECOK
028900000000DO  1C     $FINE         DOWEQ     '0'
029000000000     C     WRECOK        ANDEQ     'N'
029100131023     C     KEYCPC        READE     AZCPC01L
029200131023IF  2C                   IF        %eof(azcpc01l)
029300000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
029400000000X   2C                   ELSE
029500000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
029600000000E   2C                   ENDIF
029700000000E   1C                   ENDDO
029800960701     C*
029900000000     C                   ENDSR
030000000000     C*------------------------------------------------------------------------*
030100980216     C* CHKREC - CONTROLLA VALIDITA' RECORD -CAP-
030200000000     C*------------------------------------------------------------------------*
030300000000     C     CHKREC        BEGSR
030400000000     C*
030500000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
030600000000     C*
030700000000     C* CAP ESTERO, ESCLUDE
030800970707IF  1C     CPCNAR        IFNE      *BLANKS
030900000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031000000000E   1C                   ENDIF
031100000000     C*
031200000000     C* CAP FITTIZIO, ESCLUDE
031300970707IF  1C     CPCFIT        IFEQ      'S'
031400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031500000000E   1C                   ENDIF
031600980217     C*
031700980217     C* CAP OBSOLETO, ESCLUDE
031800131023IF  1C**   CPCFI1        IFEQ      'O'
031900131023     C***                MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
032000131023E   1C***                ENDIF
032100000000     C*
032200000000     C                   ENDSR
033100980216     C*------------------------------------------------------------------------*
033200980216     C* NEWCAP - OPERAZIONI PER NUOVO CAP
033300980216     C*------------------------------------------------------------------------*
033400980216     C     NEWCAP        BEGSR
033500980216     C*
033600980216     C* MEMORIZZA IL NUOVO CAP -E ALCUNI SUOI DATI- IN UN DEPOSITO
033700980216     C                   MOVEL     CPCNAR        DEPNAR                         *NAZIONE
033800980216     C                   MOVEL     CPCCAP        DEPCAP                         *CAP
033900980216     C                   MOVEL     CPCPRV        DEPPRV                         *PROVINCIA
034800980216     C*
034900980216     C* RECUPERA I DATI DALLA PROVINCIA DEL CAP
035000131024     C*                  Z-ADD     1             I
035100131024     C*                  SETOFF                                           28
035200131024     C*    CPCPRV        LOOKUP    PRV(I)                                 28
035300131024IF  1C*    *IN28         IFEQ      *ON                                          *TROVATO
035400131024     C*                  MOVEL     PFCR(I)       DEPFCR                         *CAPOLUOGO DI REGION
035500131024     C*                  MOVEL     PRAP(I)       DEPRAP                         *REGIONE APPARTENENZ
035600131024     C*                  MOVEL     PCOR(I)       DEPCOR                         *CODICE ORDINAMENTO
035700131024X   1C*                  ELSE                                                   *NON TROVATO
035800131024     C*                  MOVEL     *BLANKS       DEPFCR
035900131024     C*                  MOVEL     *BLANKS       DEPRAP
036000131024     C*                  MOVEL     *BLANKS       DEPCOR
036100131024E   1C*                  ENDIF
036200980216     C*
036300980216     C                   ENDSR
036400980216     C*------------------------------------------------------------------------*
036500980216     C* SETCPL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE -LOCALITA'-
036600980216     C*------------------------------------------------------------------------*
036700980216     C     SETCPL        BEGSR
036800980216     C*
036900980216     C* REIMPOSTA LE VARIABILI DI LAVORO
037000980216     C                   MOVEL     '0'           $FINL
037100980216     C*
037200980216     C                   Z-ADD     P96VER        KCLVER
037300980216     C                   MOVEL     DEPNAR        KCLNAR
037400980216     C                   MOVEL     DEPCAP        KCLCAP
037500131023     C     KEYCPL        SETLL     AZCPL01L
037600131023IF  1C                   IF        not %equal(azcpl01l)
037700980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
037800980216X   1C                   ELSE
037900980216     C                   EXSR      LETCPL                                       *LETTURA RECORD
038000980216E   1C                   ENDIF
038100980216     C*
038200980216     C                   ENDSR
038300980216     C*------------------------------------------------------------------------*
038400980216     C* LETCPL - LETTURA PROSSIMO RECORD -LOCALITA'-
038500980216     C*------------------------------------------------------------------------*
038600980216     C     LETCPL        BEGSR
038700980216     C*
038800980216     C* LEGGE FINO A:
038900980216     C*     - FINE FILE
039000980216     C*     - TROVATO RECORD VALIDO
039100980216     C                   MOVEL     'N'           WRECOK
039200980216DO  1C     $FINL         DOWEQ     '0'
039300980216     C     WRECOK        ANDEQ     'N'
039400131023     C     KEYCPL        READE     AZCPL01L
039500131023IF  2C                   IF        %eof(azcpl01l)
039600980216     C                   MOVEL     '1'           $FINL                          *FINE PGM
039700980216X   2C                   ELSE
039800980216     C                   EXSR      CHKCPL                                       *CONTROLLA RECORD
039900980216E   2C                   ENDIF
040000980216E   1C                   ENDDO
040100980216     C*
040200980216     C                   ENDSR
040300980216     C*------------------------------------------------------------------------*
040400980216     C* CHKCPL - CONTROLLA VALIDITA' RECORD -LOCALITA'-
040500980216     C*------------------------------------------------------------------------*
040600980216     C     CHKCPL        BEGSR
040700980216     C*
040800980216     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
040900980216     C*
041000980217     C* LOCALITA' OBSOLETA, ESCLUDE
041100131023IF  1C*    CPLFI1        IFEQ      'O'
041200131023     C*                  MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
041300131023E   1C*                  ENDIF
041400980216     C*
041500980216     C                   ENDSR
041600980216     C*------------------------------------------------------------------------*
041700980216     C* CREDLY - CREA IL RECORD SUL WORKFILE
041800980216     C*------------------------------------------------------------------------*
041900980216     C     CREDLY        BEGSR
042000980216     C*
042100131023     C                   CLEAR                   WADLY100
042101140109     c                   clear                   s_MIISC
042102140109     c                   clear                   s_MXFSC
042103140109     c                   clear                   s_STISC
042104140109     c                   clear                   s_STFSC
042106140109
042200140109     c*---
042300980216     C* DATI DEDOTTI
042400980216     C*---
042500131023     C                   z-add     si6ofel       DLYfel
042501131023     c                   if        wtip='S'
042502131023     c                   eval      dlyloc='STANDARD'
042503131023     c                   clear                   depnar
042504131023     c                   clear                   depcap
042505131023     c                   clear                   wloc
042506131023     c                   exsr      ela_cos
042509131023     c
042510131023     c                   else
042600131024     C*                  MOVEL     DEPFCR        DLYFCR                         *CAPOLUOGO DI REGION
042700131024     C*                  MOVEL     DEPRAP        DLYRAP                         *REGIONE APPARTENENZ
042800131024     C*                  MOVEL     DEPCOR        DLYCOR                         *CODICE ORDINAMENTO
042900980216     C*---
043000980216     C* DATI CAPPARIO
043100980216     C*---
043200980216     C                   MOVEL     DEPNAR        DLYNAR                         *NAZIONE
043300980216     C                   MOVEL     DEPCAP        DLYCAP                         *CAP
043400980216     C                   MOVEL     DEPPRV        DLYPRV                         *PROVINCIA
043500980216     C                   MOVEL     WLOC          DLYLOC                         *LOCALITA'
043600980216     C                   MOVEL     WISO          DLYISO                         *INOLTRO
043601131023     c                   if        cpcfi1='O' or cplfi1='O'
043602131023     c                   eval      dlyobs='O'
043603131023     c                   endif
049900121012     c* imposto le 3 linee di arrivo previste dal cap/località
050000131023     c                   z-add     wilna         dlylna
050100131023     c                   z-add     wilol         dlylnao
050200131023     c                   z-add     wilos         dlylnas
050201131023     c* orari servizi della linea da elaborare
050207131023     c                   exsr      ela_cos
050226131023     c* se non reperiti orari per località prendo quelli per cap
050300121012     c
050301131023     c                   endif
050400131023     C                   WRITE     WADLY100                                     I N S E R I M E N T
050401140109
050402140109     c* Se soglia=S --> Creazione record "POM"
050403140109     c                   if        si6osgl='S'
050404140109     c                   clear                   DLYSTISR
050405140109     c                   clear                   DLYSTFSR
050406140109     c                   clear                   DLYLRSC
050407140109     c                   clear                   DLYLRNC
050408140109     c                   clear                   DLYLRNK
050409140110     c                   eval      dlysgl='P'
050410140109     c                   eval      DLYMIISC = s_MIISC
050411140109     c                   eval      DLYMXFSC = s_MXFSC
050412140109     c                   eval      DLYSTISC = s_STISC
050413140109     c                   eval      DLYSTFSC = s_STFSC
050414140110     c                   eval      DLYtrzs  = s_trzs
050415140109     C                   WRITE     WADLY100                                     I N S E R I M E N T
050416140109     c                   endif
050500090921     c
059600980216     C                   ENDSR
059601131023     C*------------------------------------------------------------------------*
059602131023     C*
059603131023     C*------------------------------------------------------------------------*
059604131023     C     ela_cos       BEGSR
059605131024
059606140109     c                   clear                   tisio9ds
059607131024     c* Ritiri
059608131024     c                   eval      DO0VER = p96ver
059609131024     c                   eval      DO0NAR = depnar
059610131024     c                   eval      DO0CAP =depcap
059611131024     c                   eval      DO0LOC =wloc
059612131024     c                   eval      DO0LNA =si6ofel
059613131024     c                   eval      DO0TSER='R'
059614140109     c                   call      'TISIO9R'
059615131024     C                   PARM                    KPJBA
059616140109     C                   PARM                    tiSIO9ds
059617131024     c* carico orari se no errore e no dati standard
059618131024     c                   if        do0err=*blanks and do0rif<>'9'
059619131024     c                   eval      DLYSTISR = DO0OSTIS
059620131024     c                   eval      DLYSTFSR = DO0OSTFS
059621131024     c                   eval      DLYLRSC  = DO0OLRSC
059622131024     c                   eval      DLYLRNC  = DO0OLRNC
059623131024     c                   eval      DLYLRNK  = DO0OLRNK
059624131024     c                   endif
059625131024     c* Consegne
059626131024     c                   eval      DO0TSER='C'
059627140109     c                   eval      DO0SOGLIA='T'
059628140109     c                   call      'TISIO9R'
059629131024     C                   PARM                    KPJBA
059630140109     C                   PARM                    tiSIO9ds
059631131024     c* carico orari se no errore e no dati standard
059632131024     c                   if        do0err=*blanks and do0rif<>'9'
059633131024     c                   eval      DLYMIISC = DO0OMIIS
059634131024     c                   eval      DLYMXFSC = DO0OMXFS
059635131024     c                   eval      DLYSTISC = DO0OSTIS
059636131024     c                   eval      DLYSTFSC = DO0OSTFS
059637140110     c                   eval      DLYtrzs  = DO0TRZS
059638131024     c                   endif
059639140109     c* se deve creare riga anche per la soglia richiamo anche per pomeriggio
059640140109     c                   if        si6osgl='S'
059641140109     c                   eval      DO0SOGLIA='P'
059642140109     c                   call      'TISIO9R'
059643140109     C                   PARM                    KPJBA
059644140109     C                   PARM                    tiSIO9ds
059645140109     c* carico orari se no errore e no dati standard
059646140109     c                   if        do0err=*blanks and do0rif<>'9'
059647140109     c                   eval      s_MIISC = DO0OMIIS
059648140109     c                   eval      s_MXFSC = DO0OMXFS
059649140109     c                   eval      s_STISC = DO0OSTIS
059650140109     c                   eval      s_STFSC = DO0OSTFS
059651140110     c                   eval      s_trzs  = DO0trzs
059652140109     c                   endif
059653140109     c                   endif
059658131023     C                   ENDSR
059700000000     C*------------------------------------------------------------------------*
059800971020     C* FINSR - OPERAZIONI FINALI
059900000000     C*------------------------------------------------------------------------*
060000971020     C     FINSR         BEGSR
060100971020     C*
060200971020     C* CHIUDE I FILE DI LAVORO
060300131023     C                   CLOSE     WADLY10F
062500000000     C*
062600000000     C                   ENDSR
062700000000     C*------------------------------------------------------------------------*
062800050318     C* OPNFIL - OPERAZIONI INIZIALI APERTURA FILE
062900000000     C*------------------------------------------------------------------------*
063000000000     C     OPNFIL        BEGSR
063100091211     C                   Z-ADD     80            LENGH            15 5
063200091211     c* se si tratta di una prova, creo il file, vuoto in EDPES
063300131023    1c                   if        si6oprova='S'
063400091211     c* creazione file
063500091211     C                   MOVEL(P)  CMD(2)        COMMAN           80
063600091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
063700091211     C                   PARM                    COMMAN
063800091211     C                   PARM                    LENGH
063900091211     c
064000091211     c* Se c'e' già lo pulisco
064100091211    2C                   IF        %error
064200091211     C                   MOVEL(P)  CMD(3)        COMMAN           80
064300091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
064400091211     C                   PARM                    COMMAN
064500091211     c                   PARM                    LENGH
064600091211    3C                   IF        %error
064700091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
064800091211    3C                   ENDIF
064900091211    2C                   ENDIF
065000091211     c
065100091211     c* Eseguo ovrdbf
065200091211     C                   MOVEL(P)  CMD(4)        COMMAN           80
065300091211     C                   CALL(e)   'QCMDEXC'                                    *PULISCE FILE
065400091211     C                   PARM                    COMMAN
065500091211     c                   PARM                    LENGH
065600091211    3C                   IF        %error
065700091211     C                   MOVEL     '1'           $ERR                           *ERRORE GENERICO
065800091211    3C                   ENDIF
065900091214     c
066000091211   x1c                   else
066100020925     c*
066200131023     c* Cancella dal file i record della filiale richiesta
066201131023      /FREE
066202131023         wSQL = 'DELETE from WADLY10F where DLYFEL = '
066203131023              + %char(si6ofel) ;
066204131023         exec SQL   execute immediate :wSQL;
066205131023         if sqlcod < 0 ;
066206131023            $err='1';
066207131023         endif;
066208131023      /END-FREE
067000091211    1C                   ENDIF
067100020925     C*
067200050318     C* Apre il file
067300131023     C                   OPEN      WADLY10F                                     *APRE FILE
067400960701     C*
067500000000     C                   ENDSR
067600960709     C*------------------------------------------------------------------------*
067700980211     C* CARTAB - CARICA LE TABELLE OCCORRENTI
067800960709     C*------------------------------------------------------------------------*
067900960709     C     CARTAB        BEGSR
068000020925     C*---
068100020925     C* Dati statistiche
068200020925     C*---
068300020925     C                   CLEAR                   DSDD
068400020925     C                   MOVEL(P)  'SDD'         KBECOD
068500020925     C                   MOVEL(P)  '1'           KBEKE1
068600131024     C     KEYTBE        CHAIN     TNTBE01L
068700020925     C                   IF        %found(TNTBE01L)
068800020925     C                   MOVEL     TBEUNI        DSDD
068900020925     C                   ENDIF
069000980211     C*---
069100980211     C* PROVINCIE
069200980211     C*---
069300980216     C                   Z-ADD     *ZEROS        I                 5 0
069400020919     C                   Z-ADD     1             KTBKUT
069500960709     C                   MOVEL     'PR'          KTBCOD
069600960709     C     KE2TAB        SETLL     TABEL00F
069700960709     C     KE2TAB        READE     TABEL00F                               99
069800960709DO  1C     *IN99         DOWEQ     *OFF
069900960709IF  2C     TBLFLG        IFEQ      *BLANKS
070000960709     C                   ADD       1             I
070100960709     C                   MOVEL     TBLUNI        DSPR
070200960709     C                   MOVEL     TBLKEY        PRV(I)                         *PROVINCIA
070300960709     C                   MOVEL     §PRCTS        PCTS(I)                        *CODICE TASSAZIONE
070400131024     c****               movel     §PRfcr        Pfcr(I)                        *Capoluogo Regione
070500960709E   2C                   ENDIF
070600960709     C     KE2TAB        READE     TABEL00F                               99
070700960709E   1C                   ENDDO
070800051116      *
070900051116      * Imposta il progressivo per le province (diverse) con lo stesso
071000051116      *   codice tassazione (da cui viene reperito il cod. ordinamento
071100051116      *   - che NON può essere uguale per due province diverse:
071200051116      *   ha causato confusione nel reperimento dei tempi di consegna
071300051116      *   e/o nell'elenco delle località)
071400131024     c***                clear                   xx
071500131024do  1c***                DOW       xx       <  %elem(PRV)
071600131024     c***                add       1             xx
071700131024if  2c***                if        Prv(xx)  =  *blanks
071800131024     c***                leave
071900131024e   2c***                endif
072000051116      * - scarto le province per le quali è già stata impostata la
072100051116      *   "sigla" finale della provincia per il codice ordinamento
072200131024     c***                if        Pcor(xx) <> *blanks
072300131024     c***                iter
072400131024     c***                endif
072500051116      * - cerco le altre province con lo stesso codice tassazione
072600131024     c***                eval      I        =  xx
072700131024do  2c***                DOW       I        <  %elem(PRV)
072800131024     c***                add       1             I
072900131024     c***                eval      *in28    =  *off
073000131024     c***  Pcts(xx)      lookup    Pcts(I)                                28
073100131024if  3c***                if        *in28    =  *off
073200131024     c***                leave
073300131024e   3c***                endif
073400051116      * - e per ognuna di esse (tranne che per il capoluogo regionale)
073500051116      *   memorizzo la provincia stessa - da posporre al codice
073600051116      *   ordinamento
073700051116      *   Esempio finale: "CCA  " = Milano    "CCALO" = Lodi
073800051116      *                   "HHCFO" = Forlì     "HHCRN" = Rimini
073900051116      *   (ora memorizzo "   LO", "   FO", "   RN")
074000131024if  3c***                if        Pfcr(xx) =  *blanks
074100131024     c***                move      Prv(xx)       Pcor(xx)
074200131024e   3c***                endif
074300131024if  3c***                if        Pfcr(I)  =  *blanks
074400131024     c***                move      Prv(I)        Pcor(I)
074500131024e   3c***                endif
074600131024e   2c***                ENDDO
074700131024e   1c***                ENDDO
074800980211     C*---
074900960709     C* DATI CODICI TASSAZIONE
075000980211     C*---
075100020919     C                   Z-ADD     1             KTBKUT
075200960709     C                   MOVEL     'CT'          KTBCOD
075300041213DO  1C     1             DO        600           I
075400960709IF  2C     PRV(I)        IFNE      *BLANKS
075500960709     C                   MOVEL(P)  PCTS(I)       KTBKEY
075600960709     C     KEYTAB        CHAIN     TABEL00F                           99
075700960709IF  3C     *IN99         IFEQ      *OFF
075800960709     C     TBLFLG        ANDEQ     *BLANKS
075900960709     C                   MOVEL     TBLUNI        DSCT
076000131024     C***                MOVEL     §CTRAP        PRAP(I)                        *REGIONE APPARTENENZ
076100131024     C****               MOVEL     §CTCOR        PCOR(I)                        *CODICE ORDINAMENTO
076200960709E   3C                   ENDIF
076300020925E   3C                   ENDIF
076400960709E   1C                   ENDDO
081400960709     C*
081500960709     C                   ENDSR
081600020919     C*--------------------------------------------------------------------------------------------*
081700020919     C* REPERISCE I DATI UTENTE
081800020919     C*--------------------------------------------------------------------------------------------*
081900020919     C     REPDATIUTE    BEGSR
082000020919     C*
082100020919     C* INIZIALIZZA VARIABILI DI WRK
082200020919     C                   CLEAR                   TIBS34DS
082300020919     C                   CLEAR                   AZUTEDS
082400020919     C                   CLEAR                   DDATIUTE
082500020919     C*
082600020919     C     *DTAARA       DEFINE    §azute        azuteds
082700020919     C     *DTAARA       DEFINE    §datiute      ddatiute
082800020919     C                   IN(E)     *DTAARA
082900020919if  1C                   IF        %Error
083000020919     c                   EVAL      I34Tla = 'L'
083100020919     C                   CALL      'TIBS34R'
083200020919     C                   PARM                    Tibs34Ds
083300020919     C                   IN        *DTAARA
083400020919e   1C                   ENDIF
083800020919     C*
083900020919     C                   ENDSR
084000000000     C*------------------------------------------------------------------------*
084100000000     C* *INZSR - OPERAZIONI INIZIALI
084200000000     C*------------------------------------------------------------------------*
084300000000     C     *INZSR        BEGSR
084400980211     C*---
084500000000     C* RICEVIMENTO PARAMETRI
084600980211     C*---
084700000000     C     *ENTRY        PLIST
084800000000     C                   PARM                    KPJBA
084900131023     C                   MOVEL     KPJBU         tisi6ods
085000091204
085700980211     C*---
085800000000     C* VARIABILI RIFERITE AL DATABASE
085900980211     C*---
086000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
086100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
086200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
086300020925     C     *LIKE         DEFINE    TBECOD        KBECOD                         *TNTBE01L
086400020925     C     *LIKE         DEFINE    TBEKE1        KBEKE1
086600980211     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
086700980216     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
086800980216     C     *LIKE         DEFINE    CPLNAR        KCLNAR
086900980216     C     *LIKE         DEFINE    CPLCAP        KCLCAP
087200980216     C     *LIKE         DEFINE    CPCNAR        DEPNAR
087300980216     C     *LIKE         DEFINE    CPCCAP        DEPCAP
087400980216     C     *LIKE         DEFINE    CPCPRV        DEPPRV
087500131024     C*    *LIKE         DEFINE    DLYFCR        DEPFCR
087600131024     C*    *LIKE         DEFINE    DLYRAP        DEPRAP
087700131024     C*    *LIKE         DEFINE    DLYCOR        DEPCOR
087800121015     C     *LIKE         DEFINE    CPCLNA        DEPLna
087900121015     C     *LIKE         DEFINE    CPCLNA        DEPLIN
088000121015     C     *LIKE         DEFINE    CPCLNA        DEPlol
088100121015     C     *LIKE         DEFINE    CPCLNA        DEPlos
088800980216     C     *LIKE         DEFINE    CPLLOC        WLOC                           *DI LAVORO
088900980216     C     *LIKE         DEFINE    CPCISO        WISO
089200980302     C     *LIKE         DEFINE    CPCLNA        WILNA
089300980302     C     *LIKE         DEFINE    CPCLOL        WILOL
089400121012     C     *LIKE         DEFINE    CPCLOS        WILOS
089600980211     C*---
089700000000     C* CHIAVI DI LETTURA
089800980211     C*---
089900980211     C     KEYTAB        KLIST                                                  *TABEL00F
090000980211     C                   KFLD                    KTBKUT                          -UTENTE
090100980211     C                   KFLD                    KTBCOD                          -TABELLA
090200980211     C                   KFLD                    KTBKEY                          -CHIAVE
090300980211     C     KE2TAB        KLIST                                                  *TABEL00F
090400980211     C                   KFLD                    KTBKUT                          -UTENTE
090500980211     C                   KFLD                    KTBCOD                          -TEBELLA
090600020925     C     KEYTBE        KLIST                                                  *TNTBE01L
090700020925     C                   KFLD                    KBECOD                          -TABELLA
090800020925     C                   KFLD                    KBEKE1                          -CHIAVE
090900980211     C     KEYCPC        KLIST                                                  *AZCPC00F
091000980211     C                   KFLD                    KCCVER                          -VERSIONE
091100980211     C     KEYCPL        KLIST                                                  *AZCPL00F
091200980211     C                   KFLD                    KCLVER                          -VERSIONE
091300980216     C                   KFLD                    KCLNAR                          -NAZIONE
091400980216     C                   KFLD                    KCLCAP                          -CAP
091500121012     C     KEYCPE        KLIST                                                  *AZCPL00F
091600121012     C                   KFLD                    p96VER                          -VERSIONE
091700121012     C                   KFLD                    depNAR                          -NAZIONE
091800121012     C                   KFLD                    depCAP                          -CAP
092000980211     C*---
092100000000     C* VARIABILI DI CONTROLLO
092200980211     C*---
092300980216     C                   MOVEL     '0'           $ERR              1            *ERRORE GENERICO
092400980216     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA CAP
092500980216     C                   MOVEL     '0'           $FINL             1            *FINE LETTURA LOCALI
092600980220     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
092700050318     C                   MOVEL     *BLANKS       $DLY              1            *CREAZIONE WADLY (C/
092800020527     C*
092900020527     C* CALCOLA LA DATA CORRENTE -DAL JOB-
093000020527     C                   MOVE      *DATE         N8                8 0          *DATA JOB IN G/M/AA
093100020527     C                   Z-ADD     N8            G08DAT
093200020527     C                   Z-ADD     *ZEROS        G08INV
093300020527     C                   MOVEL     '0'           G08ERR
093400020527     C                   CALL      'XSRDA8'
093500020527     C                   PARM                    WLBDA8
093600020527     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA JOB IN AA/M/G
093700020925     C*
093800020925     C* REPERISCE I DATI UTENTE
093900020925     C                   EXSR      REPDATIUTE
094000970707     C*
095200131023     C                   Z-ADD     si6odri       P96DRI            8 0
095400970707     C                   Z-ADD     *ZEROS        P96VER            5 0
095500970707     C                   CALL      'TISI96R'
095600970707     C                   PARM                    P96DRI
095700970707     C                   PARM                    P96VER
096600960709     C*
096700000000     C                   ENDSR
096800000000     C*------------------------------------------------------------------------*
096900000000**   CMD -COMANDI CLP-
097000131023
097100131023CRTDUPOBJ OBJ(WADLY10F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(EDPRM)
097200131023CLRPFM FILE(EDPRM/WADLY10F)
097300131023OVRDBF FILE(WADLY10F) TOFILE(EDPRM/WADLY10F)
