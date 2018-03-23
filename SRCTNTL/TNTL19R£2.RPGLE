000100000211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200000218     H*PARMS ACTGRP(*CALLER)
000300000000     H*------------------------------------------------------------------------*
000400000000     H* STAMPA TRAINI - ESEGUE
000500000000     H*------------------------------------------------------------------------*
000600000000     H DECEDIT('0,') DATEDIT(*DMY.)
000700000000     F*------------------------------------------------------------------------*
000800000000     F* DATA BASE                                                              *
000900000000     F*------------------------------------------------------------------------*
001000000000     FTNTL19P   O    E             PRINTER OFLIND(*IN01)
001100000000     FAZORG01L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300000821     FANATB01L  IF   E           K DISK    USROPN
001400000000     FTNTLZ01L  IF   E           K DISK
001500000000     FTNTLT01L  IF   E           K DISK
001600000000     FTNTLR01L  IF   E           K DISK
001700000000     FTNTLO01L  IF   E           K DISK
001800000000     FTNTLM01L  IF   E           K DISK
001900010725     FTITLL01L  IF   E           K DISK
002000060323     FTITLL02L  IF   E           K DISK    rename(titll000:titll002)
002100000000     FTNTLN01L  IF   E           K DISK
002200110526     FTNTLS01L  IF   E           K DISK
002300110526     FTNPSC01L  IF   E           K DISK
002400000000     D*------------------------------------------------------------------------*
002500000000     D* SCHIERE                                                                *
002600000000     D*------------------------------------------------------------------------*
002700000000     D*------------------
002800000000     D* DECODIFICHE PARAMETRI
002900000000     D*------------------
003000960118     D DPAR            S             20    DIM(15) CTDATA PERRCD(1)
003100960115     D*------------------
003200960115     D* DECODIFICHE INTESTAZIONE
003300960115     D*------------------
003400960115     D DIN2            S             19    DIM(8) CTDATA PERRCD(1)
003500000000     D*-------------------
003600000000     D* FILIALI
003700000000     D*-------------------
003800971125     D ORG             S              3  0 DIM(500)                             *CODICE
003900971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
004000000000     D*-------------------
004100000000     D* TIPI TRAINO
004200000000     D*-------------------
004300000000     D TTR             S              1    DIM(20)                              *TIPI TRAINI
004400000000     D DTTR            S             30    DIM(20)                              *DESCRIZIONE
004500000000     D*-------------------
004600000000     D* TIPI AUTOMEZZO
004700000000     D*-------------------
004800000000     D TMZ             S              2    DIM(20)                              *TIPI AUTOMEZZO
004900000000     D DTMZ            S             30    DIM(20)                              *DESCRIZIONE
005000000000     D*-------------------
005100000000     D* CAUSALI CONTABILIZZAZIONE
005200000000     D*-------------------
005300000000     D CAU             S              1    DIM(20)                              *CODICE
005400000000     D DCAU            S             30    DIM(20)                              *DESCRIZIONE
005500960115     D*-------------------
005600960115     D* DATI TRAINO
005700960115     D*-------------------
005800980323     D TTRN            S              7  0 DIM(3000)                            *NUMERO TRAINO
005900980323     D TDDE            S              8  0 DIM(3000)                            *DECORRENZA
006000980323     D TTFP            S              8  0 DIM(3000)                            *FILIALE PARTENZA
006100980323     D TGHA            S              5    DIM(3000)                            *GIORNO ARRIVO
006200980323     D TGHP            S              5    DIM(3000)                            *GIORNO PARTENZA
006300000000     D*-------------------
006400960115     D* DATI FERMATE
006500000000     D*-------------------
006600960115     D FGSE            S              1    DIM(8)                               *GIORNI DI PARTENZA
006700960115     D FGGA            S              1    DIM(8)                               *GIORNO ARRIVO
006800960115     D FHHA            S              4  0 DIM(8)                               *ORA ARRIVO
006900960115     D FGGP            S              1    DIM(8)                               *GIORNO PARTENZA
007000960115     D FHHP            S              4  0 DIM(8)                               *ORA PARTENZA
007100000000     D*------------------------------------------------------------------------*
007200000000     D* INPUT                                                                  *
007300000000     D*------------------------------------------------------------------------*
007400000000     D*---------------------
007500000000     D* ARCHITETTURA
007600000000     D*---------------------
007700000000     D KPJBA         E DS
007800000000     D  PROFIL                 1     10                                         UTENTE
007900000000     D*---------------------
008000000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
008100000000     D*---------------------
008200000000     D DSMSG         E DS
008300020715
008400020715     d DdatiUte      e ds
008500020715     d AzuteDs       e ds                  ExtName(Azute00f)
008600020715     d Tibs34Ds      e ds                  Inz
008700020715
008800960115     D*---------------------
008900000211     D* DS PARAMETRI DI LANCIO
009000960115     D*------------------
009100960115     D PARAM           DS
009200960118     D  PARORD                 1      1
009300960118     D  PARDDE                 2      9  0
009400960118     D  PARFIL                10     12  0
009500960118     D  PARGIO                13     13
009600961021     D  PARTR1                14     20  0
009700961021     D  PARTR2                21     27  0
009800060323     D  PARpdr                28     34  0
009900060323     D  PARdda                35     42  0
010000060323     D  PARbis                43     43
010100110527     D  PARTpsc               44     44
010200110609     D  PARpsc                45     54
010300000000     D*---------------------
010400000000     D* DS TIPI TRAINO
010500000000     D*---------------------
010600000000     D DSTV          E DS
010700000000     D*---------------------
010800000000     D* DS TIPI AUOTMEZZO
010900000000     D*---------------------
011000000000     D DSTU          E DS
011100000000     D*---------------------
011200000000     D* DS CAUSALI CONTABILIZZAZIONE
011300000000     D*---------------------
011400000211     D ANGY4ZDS      E DS
011500010928      *--------------------
011600010928      * Ds - moneta di conto
011700010928      *--------------------
011800010928     d dged          e ds
011900010928     d tibs02ds      e ds
012000010928     d dscv          e ds
012100010928     d yeurcods      e ds
012200000000     D*---------------------
012300000000     D* DS CONTROLLO DATA (8)
012400000000     D*------------------
012500000000     D WLBDA8          DS                  INZ
012600000000     D  G08DAT                 1      8  0
012700000000     D  G08INV                 9     16  0
012800000000     D  G08ERR                17     17
012900000000     D  G08TGI                18     22  0
013000000000     D*------------------
013100000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
013200000000     D*------------------
013300960118     D* COMMENTI
013400000000     D DES999          C                   CONST('DI CARTELLO')
013500110526     d
013600110526     d Ktrn            S                   LIKE(TLTTRN)
013700110526     d KDDE            S                   LIKE(TLTDDE)
013800110526     d scaggs          S                   LIKE(TLsggs)
013900110526     d scahhs          S                   LIKE(TLshhs)
014000110526     d Wstampa         S              1
014100000000     C*------------------------------------------------------------------------*
014200000000     C* MAIN LINE
014300000000     C*------------------------------------------------------------------------*
014400000000     C*
014500000000     C* STAMPA PARAMETRI LANCIO
014600000000     C                   EXSR      STAPAR
014700000000     C*
014800960115     C* POSIZIONAMENTO E PRIMA LETTURA TRAINI CARIATI IN MEMORIA
014900960115     C                   Z-ADD     1             T
015000000000     C*
015100000000     C* FINO A FINE TRAINI
015200960115DO  1C     TTRN(T)       DOWGT     *ZEROS
015300000000     C*
015400000000     C* OPERAZIONI PER NUOVO TRAINO
015500000000     C                   EXSR      NEWTLT
015600000000     C*
015700000000     C* STAMPA IL TRAINO
015800960115IF  2C     $ERR          IFEQ      '0'                                          *NO ERRORI
015900000000     C                   EXSR      STATLT
016000000000     C*
016100960115     C* STAMPA GLI ALTRI DATI
016200000000     C                   EXSR      ELATLR                                       *ELABORA LE FERMATE
016300000000     C                   EXSR      ELATLM                                       *ELABORA I METRICUBI
016400000000     C                   EXSR      ELATLL                                       *ELABORA I LISTINI
016500000000     C                   EXSR      ELATLN                                       *ELABORA LE NOTE
016600960115E   2C                   ENDIF
016700000000     C*
016800960115     C* LETTURA SUCCESSIVA TRAINI CARIATI IN MEMORIA
016900960115     C                   ADD       1             T
017000000000E   1C                   ENDDO
017100961227     C*
017200961227     C* STAMPA RIGA DEI TOTALI LISTINI
017300961227IF  1C     TOTILI        IFGT      *ZEROS
017400961227     C     TOTIES        ORGT      *ZEROS
017500961227     C                   EXSR      STATOT
017600961227E   1C                   ENDIF
017700000000     C*
017800000000     C* STAMPA RIGA DI FINE PROSPETTO
017900000000     C                   WRITE     TL19FI
018000000000     C*
018100000000     C                   SETON                                        LR
018200000000     C*------------------------------------------------------------------------*
018300000000     C* STATLT - STAMPA RIGA TRAINO
018400000000     C*------------------------------------------------------------------------*
018500000000     C     STATLT        BEGSR
018600110527     c                   clear                   wstampa
018700110526     c* Verifico se per questo traino è presente il punti dii scambio
018800110526     c*  lo cerco
018900110526     C                   EXSR      CERCATLS
019000000000     C*
019100960115     C                   CLEAR                   TL19DE
019200960115     C*
019300960115     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI SOLO LA RIGA DI SEPARAZIONE
019400000000IF  1C     *IN01         IFEQ      *ON
019500000000     C                   MOVEL     'T'           WINT
019600000000     C                   EXSR      STATES
019700000000E   1C                   ENDIF
019800000000     C* CAMPI
019900000000     C                   Z-ADD     TLTTRN        PDETRN                         *TRAINO
020000960115     C                   SETOFF                                           28
020100960115     C                   Z-ADD     1             J
020200960115     C                   MOVEL     *BLANKS       PDEFPD                         *FILIALE PARTENZA
020300960115     C     TLTTFP        LOOKUP    ORG(J)                                 28
020400960115IF  1C     *IN28         IFEQ      *ON
020500960115     C                   MOVEL     DORG(J)       PDEFPD
020600960115E   1C                   ENDIF
020700960115     C                   SETOFF                                           28
020800960115     C                   Z-ADD     1             J
020900960115     C                   MOVEL     *BLANKS       PDEFAD                         *FILIALE ARRIVO
021000960115     C     TLTTFA        LOOKUP    ORG(J)                                 28
021100960115IF  1C     *IN28         IFEQ      *ON
021200960115     C                   MOVEL     DORG(J)       PDEFAD
021300960115E   1C                   ENDIF
021400960115     C                   SETOFF                                           28
021500960115     C                   Z-ADD     1             J
021600960115     C                   MOVEL     *BLANKS       PDETRD                         *TIPO TRAINO
021700960115     C     TLTTTR        LOOKUP    TTR(J)                                 28
021800960115IF  1C     *IN28         IFEQ      *ON
021900960115     C                   MOVEL     DTTR(J)       PDETRD
022000960115E   1C                   ENDIF
022100960115IF  1C     TLTBIS        IFEQ      'B'
022200960424     C                   MOVEL     'BIS'         PDEISD                         *NORMALE/BIS
022300960115X   1C                   ELSE
022400960424     C                   MOVEL     *BLANKS       PDEISD                         *NORMALE/BIS
022500960115E   1C                   ENDIF
022600960115     C                   MOVEA     TLTGSE        FGSE
022700960115     C                   MOVEL     '...'         PDELUN
022800960115IF  1C     FGSE(1)       IFNE      *BLANKS
022900960115     C                   MOVEL     DPAR(1)       PDELUN                         *LUNEDI'
023000960115E   1C                   ENDIF
023100960115     C                   MOVEL     '...'         PDEMAR
023200960115IF  1C     FGSE(2)       IFNE      *BLANKS
023300960115     C                   MOVEL     DPAR(2)       PDEMAR                         *MARTEDI'
023400960115E   1C                   ENDIF
023500960115     C                   MOVEL     '...'         PDEMER
023600960115IF  1C     FGSE(3)       IFNE      *BLANKS
023700960115     C                   MOVEL     DPAR(3)       PDEMER                         *MERCOLEDI'
023800960115E   1C                   ENDIF
023900960115     C                   MOVEL     '...'         PDEGIO
024000960115IF  1C     FGSE(4)       IFNE      *BLANKS
024100960115     C                   MOVEL     DPAR(4)       PDEGIO                         *GIOVEDI'
024200960115E   1C                   ENDIF
024300960115     C                   MOVEL     '...'         PDEVEN
024400960115IF  1C     FGSE(5)       IFNE      *BLANKS
024500960115     C                   MOVEL     DPAR(5)       PDEVEN                         *VENERDI'
024600960115E   1C                   ENDIF
024700960115     C                   MOVEL     '...'         PDESAB
024800960115IF  1C     FGSE(6)       IFNE      *BLANKS
024900960115     C                   MOVEL     DPAR(6)       PDESAB                         *SABATO
025000960115E   1C                   ENDIF
025100960115     C                   MOVEL     '...'         PDEDOM
025200960115IF  1C     FGSE(7)       IFNE      *BLANKS
025300960115     C                   MOVEL     DPAR(7)       PDEDOM                         *DOMENICA
025400960115E   1C                   ENDIF
025500960115     C                   MOVEL     *BLANKS       PDEMZD                         *TIPO AUTOMEZZO
025600960115     C                   SETOFF                                           28
025700960115     C                   Z-ADD     1             J
025800960115     C     TLTTMZ        LOOKUP    TMZ(J)                                 28
025900960115IF  1C     *IN28         IFEQ      *ON
026000960115     C                   MOVEL     DTMZ(J)       PDEMZD
026100960115E   1C                   ENDIF
026200960115     C                   SETOFF                                           28
026300960115     C                   Z-ADD     1             J
026400960115     C                   MOVEL     *BLANKS       PDEESD                         *FILIALE IMPORTO
026500960115     C     TLTFES        LOOKUP    ORG(J)                                 28
026600960115IF  1C     *IN28         IFEQ      *ON
026700960115     C                   MOVEL     DORG(J)       PDEESD
026800960115E   1C                   ENDIF
026900960115     C                   Z-ADD     TLTDDE        G08INV                         *DATA DECORRENZA
027000960115     C                   Z-ADD     *ZEROS        G08DAT
027100960115     C                   MOVEL     '3'           G08ERR
027200960115     C                   CALL      'XSRDA8'
027300960115     C                   PARM                    WLBDA8
027400960115     C                   Z-ADD     G08DAT        PDEDDE
027500960115     C                   Z-ADD     TLTDSC        G08INV                         *DATA SCADENZA
027600960115     C                   Z-ADD     *ZEROS        G08DAT
027700960115     C                   MOVEL     '3'           G08ERR
027800960115     C                   CALL      'XSRDA8'
027900960115     C                   PARM                    WLBDA8
028000960115     C                   Z-ADD     G08DAT        PDEDSC
028100000000     C* STAMPA
028200970423     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
028300970423     C                   WRITE     TL19DE                                       *RIGA TRAINO
028400000000     C*
028500000000     C                   ENDSR
028600000000     C*------------------------------------------------------------------------*
028700110526     C* CERCA SE PRESENTE PUNTI DI SCAMBIO
028800000000     C*------------------------------------------------------------------------*
028900110526     C     CercaTLS      BEGSR
029000110526     c
029100110526     c                   clear                   scaggs
029200110526     c                   clear                   scahhs
029300110526     c                   clear                   westls            1
029400110609     c                   clear                   wesPSC            1
029500110526     c                   eval      ktrn=tlttrn
029600110526     c                   eval      kdde=tltdde
029700110526     c     ktlt          setll     tntls01l
029800110526     c     ktlt          reade     tntls01l
029900110527     c                   dow       not %eof(tntls01l)
030000110526     c
030100110526     c                   if        tlsatb=' '
030200110526     c                   eval      westls='S'
030300110609     c
030400110609     c                   if        parpsc<>*blanks and parpsc=tlspsc
030500110609     c                   eval      wespsc='S'
030600110609     c                   endif
030700110609     c
030800110609     c* memorizzo orario di scambio se si tratta del giorno ' '
030900110526     c                   if        tlsgse=' '
031000110526     c                   eval      scaggs=tlsggs
031100110526     c                   eval      scahhs=tlshhs
031200110526     c                   else
031300110526     c                   if        scaggs=' '
031400110526     c                   eval      scaggs=tlsggs
031500110526     c                   eval      scahhs=tlshhs
031600110526     c                   endif
031700110526     c                   endif
031800110526     c
031900110526     c                   endif
032000110526     c
032100110526     c     ktlt          reade     tntls01l
032200110526     c                   enddo
032300110526     c
032400110526     C                   ENDSR
032500110526     C*------------------------------------------------------------------------*
032600110526     C* ELATLR - ELABORA LE FERMATE
032700110526     C*------------------------------------------------------------------------*
032800110526     C     ELATLR        BEGSR
032900000000     C*
033000000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG PRIMA RIGA FERMATA
033100000000     C                   MOVEL     '0'           $FIN2
033200000000     C                   Z-ADD     *ZEROS        WTLR
033300000000     C*
033400000000     C* POSIZIONAMENTO E PRIMA LETTURA FERMATE DEL TRAINO
033500000000     C                   EXSR      SETFI2
033600000000     C*
033700000000     C* FINO A FINE FERMATE
033800000000DO  1C     $FIN2         DOWEQ     '0'
033900000000     C*
034000000000     C* STAMPA LA FERMATA
034100000000     C                   EXSR      STATLR
034200000000     C*
034300000000     C* LETTURA SUCESSIVA FERMATE
034400000000     C                   EXSR      LETFI2
034500000000E   1C                   ENDDO
034600110527     c
034700110527     c* se non ancora stampato punto di scambio lo faccio ora
034800110527     c                   if        westls='S' and wstampa=' '
034900110527     c                   exsr      StaTLS
035000110527     c                   endif
035100000000     C*
035200000000     C                   ENDSR
035300000000     C*------------------------------------------------------------------------*
035400000000     C* SETFI2 - POSIZIONAMENTO E PRIMA LETTURA FERMATE
035500000000     C*------------------------------------------------------------------------*
035600000000     C     SETFI2        BEGSR
035700000000     C*
035800000000     C* POSIZIONAMENTO
035900000000     C                   Z-ADD     TLTTRN        KLRTRN                         *TRAINO
036000000000     C                   Z-ADD     TLTDDE        KLRDDE                         *DATA DECORRENZA
036100000000     C     KEYTLR        SETLL     TNTLR01L                           98
036200000000     C* LETTURA
036300000000IF  1C     *IN98         IFEQ      *OFF
036400000000     C                   EXSR      LETFI2
036500000000X   1C                   ELSE
036600000000     C                   MOVEL     '1'           $FIN2
036700000000E   1C                   ENDIF
036800000000     C*
036900000000     C                   ENDSR
037000000000     C*------------------------------------------------------------------------*
037100000000     C* LETFI2 - LETTURA SUCESSIVA FERMATE
037200000000     C*------------------------------------------------------------------------*
037300000000     C     LETFI2        BEGSR
037400000000     C*
037500000000     C                   MOVEL     'N'           WRECOK
037600000000     C*
037700000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
037800000000DO  1C     $FIN2         DOWEQ     '0'
037900000000     C     WRECOK        ANDEQ     'N'
038000000000     C     KEYTLR        READE     TNTLR01L                               98
038100000000     C*
038200000000IF  2C     *IN98         IFEQ      *ON
038300000000     C                   MOVEL     '1'           $FIN2
038400000000X   2C                   ELSE
038500000000     C                   EXSR      CHKRE2                                       *CONTROLLA RECORD
038600000000E   2C                   ENDIF
038700000000E   1C                   ENDDO
038800000000     C*
038900000000     C                   ENDSR
039000000000     C*------------------------------------------------------------------------*
039100000000     C* CHKRE2 - CONTROLLA VALIDITA' FERMATE
039200000000     C*------------------------------------------------------------------------*
039300000000     C     CHKRE2        BEGSR
039400960102     C*
039500000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
039600960102     C*
039700960102     C* FERMATA ANNULLATA, ESCLUDE
039800960102IF  1C     TLRATB        IFNE      *BLANKS
039900960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
040000960102E   1C                   ENDIF
040100960102     C*
040200000000     C                   ENDSR
040300000000     C*------------------------------------------------------------------------*
040400000000     C* STATLR - STAMPA RIGA FERMATA
040500000000     C*------------------------------------------------------------------------*
040600000000     C     STATLR        BEGSR
040700960115     C*
040800960115     C                   CLEAR                   TL19I2
040900960115     C                   CLEAR                   TL19D2
041000000000     C*--------------------
041100000000     C* FERMATA
041200000000     C*--------------------
041300000000     C* CAMPI
041400000000     C                   MOVEL     TLRSCA        PD2SCA                         *SCARICO
041500000000     C                   MOVEL     TLRCAR        PD2CAR                         *CARICO
041600000000     C                   Z-ADD     TLRKMT        PD2KMT                         *CHILOMETRI
041700000000     C                   Z-ADD     TLRTEM        PD2TEM                         *TEMPO
041800000000     C                   MOVEL     TLRGFV        PD2GFV                         *GIORNO FV
041900000000     C* DECODIFICHE
042000000000     C                   SETOFF                                           28
042100000000     C                   Z-ADD     1             J
042200000000     C                   MOVEL     *BLANKS       PD2ILD                         *FERMATA
042300000000     C     TLRFIL        LOOKUP    ORG(J)                                 28
042400000000IF  1C     *IN28         IFEQ      *ON
042500000000     C                   MOVEL     DORG(J)       PD2ILD
042600000000E   1C                   ENDIF
042700960115IF  1C     TLRTFM        IFEQ      'R'                                          *TIPO FERMATA RITORN
042800960115     C                   MOVEL     'R'           PD2TFM
042900000000E   1C                   ENDIF
043000000000     C*--------------------
043100000000     C* ORARI
043200000000     C*--------------------
043300960115     C                   CLEAR                   FGGA
043400960115     C                   CLEAR                   FHHA
043500960115     C                   CLEAR                   FGGP
043600960115     C                   CLEAR                   FHHP
043700000000     C*
043800000000     C* MEMORIZZA GLI ORARI DI TUTTI I GIORNI DELLA FERMATA
043900000000     C                   Z-ADD     TLRTRN        KLOTRN                         *TRAINO
044000000000     C                   Z-ADD     TLRDDE        KLODDE                         *DECORRENZA
044100000000     C                   Z-ADD     TLRNFM        KLONFM                         *NUMERO FERMATA
044200000000DO  1C     1             DO        8             J
044300960115IF  2C     FGSE(J)       IFNE      *BLANKS
044400000000     C     J             OREQ      8
044500000000IF  3C     J             IFEQ      8
044600000000     C                   MOVEL     *BLANKS       KLOGSE                         *GIORNO
044700000000X   3C                   ELSE
044800960115     C                   MOVEL     FGSE(J)       KLOGSE
044900000000E   3C                   ENDIF
045000000000     C     KEYTLO        CHAIN     TNTLO01L                           98
045100000000IF  3C     *IN98         IFEQ      *OFF
045200000000     C     TLOATB        ANDEQ     *BLANKS
045300960115     C                   MOVEL     TLOGGA        FGGA(J)
045400960115     C                   Z-ADD     TLOHHA        FHHA(J)
045500960115     C                   MOVEL     TLOGGP        FGGP(J)
045600960115     C                   Z-ADD     TLOHHP        FHHP(J)
045700000000E   3C                   ENDIF
045800000000E   2C                   ENDIF
045900000000E   1C                   ENDDO
046000110526     c* Verifico se prima di stampare la fermata ---> devo stampare punti scambio
046100110526     c                   exsr      ctrpsc
046200110526     c
046300000000     C* CAMPI
046400960115IF  1C     FHHA(1)       IFGT      *ZEROS                                       *ORARI DI LUNEDI'
046500960115     C     FHHP(1)       ORGT      *ZEROS
046600960115     C                   MOVEL     '¦'           PD2L11
046700960115     C                   MOVEL     DIN2(1)       I2DLUN
046800960115     C                   MOVEL     FGGA(1)       PD2GA1
046900960115     C                   Z-ADD     FHHA(1)       PD2HA1
047000960115     C                   MOVEL     '-'           PD2TR1
047100960115     C                   MOVEL     FGGP(1)       PD2GP1
047200960115     C                   Z-ADD     FHHP(1)       PD2HP1
047300960115     C                   MOVEL     '¦'           PD2L21
047400000000E   1C                   ENDIF
047500960115IF  1C     FHHA(2)       IFGT      *ZEROS                                       *ORARI DI MARTEDI'
047600960115     C     FHHP(2)       ORGT      *ZEROS
047700960115     C                   MOVEL     '¦'           PD2L12
047800960115     C                   MOVEL     DIN2(2)       I2DMAR
047900960115     C                   MOVEL     FGGA(2)       PD2GA2
048000960115     C                   Z-ADD     FHHA(2)       PD2HA2
048100960115     C                   MOVEL     '-'           PD2TR2
048200960115     C                   MOVEL     FGGP(2)       PD2GP2
048300960115     C                   Z-ADD     FHHP(2)       PD2HP2
048400960115     C                   MOVEL     '¦'           PD2L22
048500000000E   1C                   ENDIF
048600960115IF  1C     FHHA(3)       IFGT      *ZEROS                                       *ORARI DI MERCOLEDI'
048700960115     C     FHHP(3)       ORGT      *ZEROS
048800960115     C                   MOVEL     '¦'           PD2L13
048900960115     C                   MOVEL     DIN2(3)       I2DMER
049000960115     C                   MOVEL     FGGA(3)       PD2GA3
049100960115     C                   Z-ADD     FHHA(3)       PD2HA3
049200960115     C                   MOVEL     '-'           PD2TR3
049300960115     C                   MOVEL     FGGP(3)       PD2GP3
049400960115     C                   Z-ADD     FHHP(3)       PD2HP3
049500960115     C                   MOVEL     '¦'           PD2L23
049600000000E   1C                   ENDIF
049700960115IF  1C     FHHA(4)       IFGT      *ZEROS                                       *ORARI DI GIOVEDI'
049800960115     C     FHHP(4)       ORGT      *ZEROS
049900960115     C                   MOVEL     '¦'           PD2L14
050000960115     C                   MOVEL     DIN2(4)       I2DGIO
050100960115     C                   MOVEL     FGGA(4)       PD2GA4
050200960115     C                   Z-ADD     FHHA(4)       PD2HA4
050300960115     C                   MOVEL     '-'           PD2TR4
050400960115     C                   MOVEL     FGGP(4)       PD2GP4
050500960115     C                   Z-ADD     FHHP(4)       PD2HP4
050600960115     C                   MOVEL     '¦'           PD2L24
050700000000E   1C                   ENDIF
050800960115IF  1C     FHHA(5)       IFGT      *ZEROS                                       *ORARI DI VENERDI'
050900960115     C     FHHP(5)       ORGT      *ZEROS
051000960115     C                   MOVEL     '¦'           PD2L15
051100960115     C                   MOVEL     DIN2(5)       I2DVEN
051200960115     C                   MOVEL     FGGA(5)       PD2GA5
051300960115     C                   Z-ADD     FHHA(5)       PD2HA5
051400960115     C                   MOVEL     '-'           PD2TR5
051500960115     C                   MOVEL     FGGP(5)       PD2GP5
051600960115     C                   Z-ADD     FHHP(5)       PD2HP5
051700960115     C                   MOVEL     '¦'           PD2L25
051800000000E   1C                   ENDIF
051900960115IF  1C     FHHA(6)       IFGT      *ZEROS                                       *ORARI DI SABATO
052000960115     C     FHHP(6)       ORGT      *ZEROS
052100960115     C                   MOVEL     '¦'           PD2L16
052200960115     C                   MOVEL     DIN2(6)       I2DSAB
052300960115     C                   MOVEL     FGGA(6)       PD2GA6
052400960115     C                   Z-ADD     FHHA(6)       PD2HA6
052500960115     C                   MOVEL     '-'           PD2TR6
052600960115     C                   MOVEL     FGGP(6)       PD2GP6
052700960115     C                   Z-ADD     FHHP(6)       PD2HP6
052800960115     C                   MOVEL     '¦'           PD2L26
052900000000E   1C                   ENDIF
053000960115IF  1C     FHHA(7)       IFGT      *ZEROS                                       *ORARI DI DOMENICA
053100960115     C     FHHP(7)       ORGT      *ZEROS
053200960115     C                   MOVEL     '¦'           PD2L17
053300960115     C                   MOVEL     DIN2(7)       I2DDOM
053400960115     C                   MOVEL     FGGA(7)       PD2GA7
053500960115     C                   Z-ADD     FHHA(7)       PD2HA7
053600960115     C                   MOVEL     '-'           PD2TR7
053700960115     C                   MOVEL     FGGP(7)       PD2GP7
053800960115     C                   Z-ADD     FHHP(7)       PD2HP7
053900960115     C                   MOVEL     '¦'           PD2L27
054000000000E   1C                   ENDIF
054100960115IF  1C     FHHA(8)       IFGT      *ZEROS                                       *ORARI DI TUTTI I GG
054200960115     C     FHHP(8)       ORGT      *ZEROS
054300960115     C                   MOVEL     '¦'           PD2L18
054400960115     C                   MOVEL     DIN2(8)       I2DTUT
054500960115     C                   MOVEL     FGGA(8)       PD2GA8
054600960115     C                   Z-ADD     FHHA(8)       PD2HA8
054700960115     C                   MOVEL     '-'           PD2TR8
054800960115     C                   MOVEL     FGGP(8)       PD2GP8
054900960115     C                   Z-ADD     FHHP(8)       PD2HP8
055000960115     C                   MOVEL     '¦'           PD2L28
055100000000E   1C                   ENDIF
055200960115     C*
055300960115     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'NTESTAZIONE FERMATA
055400960115IF  1C     *IN01         IFEQ      *ON
055500960115     C                   MOVEL     'F'           WINT
055600960115     C                   EXSR      STATES
055700960115X   1C                   ELSE
055800960115IF  2C     WTLR          IFEQ      *ZEROS
055900960115     C                   WRITE     TL19I2
056000960115     C                   Z-ADD     1             WTLR
056100960115E   2C                   ENDIF
056200960115E   1C                   ENDIF
056300960115     C* STAMPA
056400000000     C                   WRITE     TL19D2                                       *RIGA FERMATA E ORAR
056500000000     C*
056600000000     C                   ENDSR
056700000000     C*------------------------------------------------------------------------*
056800110526     C* controlla se devo stampare punti di scambio
056900000000     C*------------------------------------------------------------------------*
057000110526     C     ctrPSC        BEGSR
057100110526     c                   if        westls='S'  and wstampa=' '
057200110526    2c                   if        fgga(8)<>' '
057300110526    3c                   if        scaggs<fgga(8) or (scaggs=fgga(8) and
057400110526     c                             scahhs<fhha(8))
057500110526     c                   eval      wstampa   ='S'
057600110526    3c                   endif
057700110526   x2c                   else
057800110526     c*
057900110526    3c                   if        scaggs<fggp(8) or (scaggs=fggp(8) and
058000110527     c                             scahhs<fhhp(8))
058100110526     c                   eval      wstampa   ='S'
058200110526    3c                   endif
058300110526    2c                   endif
058400110526     c
058500110526    2c                   if        wstampa   ='S'
058600110526     c                   exsr      StaTLS
058700110526    2c                   endif
058800110526     c                   endif
058900110526     c
059000110526     C                   ENDSR
059100110526     C*------------------------------------------------------------------------*
059200110526     C* Stampa datio punto di scambio
059300110526     C*------------------------------------------------------------------------*
059400110526     C     STAtls        BEGSR
059500110527     c* stampo prima il giorno  TUTTI e poi gli altri in ordine
059600110527     c                   eval      ktrn=tlttrn
059700110527     c                   eval      kdde=tltdde
059800110527     c     ktlt          setll     tntls01l
059900110527     c     ktlt          reade     tntls01l
060000110527     c                   dow       not %eof(tntls01l)
060100110527
060200110527     c                   if        tlsatb=' '
060300110527     c                   select
060400110527     c                   when      tlsgse=' '
060500110527     c                   eval      p6dgior=%subst(din2(8):8:5)
060600110527     c                   when      tlsgse='1'
060700110527     c                   eval      p6dgior=%subst(din2(1):8:5)
060800110527     c                   when      tlsgse='2'
060900110527     c                   eval      p6dgior=%subst(din2(2):8:5)
061000110527     c                   when      tlsgse='3'
061100110527     c                   eval      p6dgior=%subst(din2(3):8:5)
061200110527     c                   when      tlsgse='4'
061300110527     c                   eval      p6dgior=%subst(din2(4):8:5)
061400110527     c                   when      tlsgse='5'
061500110527     c                   eval      p6dgior=%subst(din2(5):8:5)
061600110527     c                   when      tlsgse='6'
061700110527     c                   eval      p6dgior=%subst(din2(6):8:5)
061800110527     c                   when      tlsgse='7'
061900110527     c                   eval      p6dgior=%subst(din2(7):8:5)
062000110527     c                   ENDSL
062100110527
062200110527     c                   eval      p6cggs=tlsggs
062300110527     c                   eval      p6chhs=tlshhs
062400110527     c                   eval      p6cnot=tlsnot
062500110527     c                   eval      p6ctrns=tlstrns
062600110527     c     tlspsc        chain     tnpsc01l
062700110527     c                   if        %found(tnpsc01l)
062800110527     c                   eval      p6dpsc=%trim(tlspsc) + '-' + %trim(pscloc) +
062900110527     c                             ' ' + pscprv + ' ' + %trim(pscind)
063000110527     c                   else
063100110527     c                   eval      p6dpsc=%trim(tlspsc) + '-' + '??????????????'
063200110527     c                   endif
063300110527     c
063400110527     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'NTESTAZIONE FERMATA
063500110527IF  1C     *IN01         IFEQ      *ON
063600110527     C                   MOVEL     'F'           WINT
063700110527     C                   EXSR      STATES
063800110527X   1C                   ELSE
063900110527IF  2C     WTLR          IFEQ      *ZEROS
064000110527     C                   WRITE     TL19I2
064100110527     C                   Z-ADD     1             WTLR
064200110527E   2C                   ENDIF
064300110527E   1C                   ENDIF
064400110527     C* STAMPA
064500110527     C                   WRITE     TL19D6                                       *RIGA FERMATA E ORAR
064600110527     c                   endif
064700110527     c
064800110527     c     ktlt          reade     tntls01l
064900110527     c                   enddo
065000110527     c
065100110526     C                   ENDSR
065200110526     C*------------------------------------------------------------------------*
065300110526     C* ELATLM - ELABORA I METRICUBI
065400110526     C*------------------------------------------------------------------------*
065500110526     C     ELATLM        BEGSR
065600000000     C*
065700000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG STAMPA PRIMO METROCUBO
065800000000     C                   MOVEL     '0'           $FIN3
065900000000     C                   Z-ADD     *ZEROS        WTLM
066000000000     C*
066100000000     C* POSIZIONAMENTO E PRIMA LETTURA METRICUBI DEL TRAINO
066200000000     C                   EXSR      SETFI3
066300000000     C*
066400000000     C* FINO A FINE METRICUBI
066500000000DO  1C     $FIN3         DOWEQ     '0'
066600000000     C*
066700000000     C* STAMPA I METRICUBI
066800000000     C                   EXSR      STATLM
066900000000     C*
067000000000     C* LETTURA SUCESSIVA METRICUBI
067100000000     C                   EXSR      LETFI3
067200000000E   1C                   ENDDO
067300000000     C*
067400000000     C                   ENDSR
067500000000     C*------------------------------------------------------------------------*
067600000000     C* SETFI3 - POSIZIONAMENTO E PRIMA LETTURA METRICUBI
067700000000     C*------------------------------------------------------------------------*
067800000000     C     SETFI3        BEGSR
067900000000     C*
068000000000     C* POSIZIONAMENTO
068100000000     C                   Z-ADD     TLTTRN        KLMTRN                         *TRAINO
068200000000     C                   Z-ADD     TLTDDE        KLMDDE                         *DATA DECORRENZA
068300000000     C     KEYTLM        SETLL     TNTLM01L                           98
068400000000     C* LETTURA
068500000000IF  1C     *IN98         IFEQ      *OFF
068600000000     C                   EXSR      LETFI3
068700000000X   1C                   ELSE
068800000000     C                   MOVEL     '1'           $FIN3
068900000000E   1C                   ENDIF
069000000000     C*
069100000000     C                   ENDSR
069200000000     C*------------------------------------------------------------------------*
069300000000     C* LETFI3 - LETTURA SUCESSIVA METRICUBI
069400000000     C*------------------------------------------------------------------------*
069500000000     C     LETFI3        BEGSR
069600000000     C*
069700000000     C                   MOVEL     'N'           WRECOK
069800000000     C*
069900000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
070000000000DO  1C     $FIN3         DOWEQ     '0'
070100000000     C     WRECOK        ANDEQ     'N'
070200000000     C     KEYTLM        READE     TNTLM01L                               98
070300000000     C*
070400000000IF  2C     *IN98         IFEQ      *ON
070500000000     C                   MOVEL     '1'           $FIN3
070600000000X   2C                   ELSE
070700000000     C                   EXSR      CHKRE3                                       *CONTROLLA RECORD
070800000000E   2C                   ENDIF
070900000000E   1C                   ENDDO
071000000000     C*
071100000000     C                   ENDSR
071200000000     C*------------------------------------------------------------------------*
071300000000     C* CHKRE3 - CONTROLLA VALIDITA' METRICUBI
071400000000     C*------------------------------------------------------------------------*
071500000000     C     CHKRE3        BEGSR
071600000000     C*
071700000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
071800960102     C*
071900960102     C* METRICUBI ANNULLATI, ESCLUDE
072000960102IF  1C     TLMATB        IFNE      *BLANKS
072100960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
072200960102E   1C                   ENDIF
072300000000     C*
072400000000     C                   ENDSR
072500000000     C*------------------------------------------------------------------------*
072600000000     C* STATLM - STAMPA RIGA METRICUBI
072700000000     C*------------------------------------------------------------------------*
072800000000     C     STATLM        BEGSR
072900960115     C*
073000960115     C                   CLEAR                   TL19I3
073100960115     C                   CLEAR                   TL19D3
073200000000     C*
073300000000     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'INTESTAZIONE METRICUBI
073400000000IF  1C     *IN01         IFEQ      *ON
073500000000     C                   MOVEL     'M'           WINT
073600000000     C                   EXSR      STATES
073700000000X   1C                   ELSE
073800000000IF  2C     WTLM          IFEQ      *ZEROS
073900000000     C                   WRITE     TL19I3
074000000000     C                   Z-ADD     1             WTLM
074100000000E   2C                   ENDIF
074200000000E   1C                   ENDIF
074300000000     C* CAMPI
074400000000     C                   Z-ADD     TLMTFP        PD3TFP                         *FILIALE PARTENZA
074500000000     C                   Z-ADD     TLMTFA        PD3TFA                         *FILIALE SCARICO
074600000000     C                   Z-ADD     TLMTFF        PD3TFF                         *FILIALE ARRIVO
074700960424     C                   Z-ADD     TLMMCA        PD3MCA                         *MCUBI NORMALI
074800960424     C                   Z-ADD     TLMMCB        PD3MCB                         *MCUBI BIS
074900960105     C                   Z-ADD     TLMMCT        PD3MCT                         *MCUBI TRAINI SEDE
075000050928     c* incremento campi di totale metri cubi da stampare a fine elab.
075100050928     c                   add       tlmmca        tocmca
075200050928     c                   add       tlmmcb        tocmcb
075300050928     c                   add       tlmmct        tocmct
075400000000     C* DECODIFICHE
075500000000     C                   SETOFF                                           28
075600000000     C                   Z-ADD     1             J
075700000000     C                   MOVEL     *BLANKS       PD3FPD                         *FILIALE PARTENZA
075800000000     C     TLMTFP        LOOKUP    ORG(J)                                 28
075900000000IF  1C     *IN28         IFEQ      *ON
076000000000     C                   MOVEL     DORG(J)       PD3FPD
076100000000E   1C                   ENDIF
076200000000     C                   SETOFF                                           28
076300000000     C                   Z-ADD     1             J
076400000000     C                   MOVEL     *BLANKS       PD3FAD                         *FILIALE SCARICO
076500000000     C     TLMTFA        LOOKUP    ORG(J)                                 28
076600000000IF  1C     *IN28         IFEQ      *ON
076700000000     C                   MOVEL     DORG(J)       PD3FAD
076800000000E   1C                   ENDIF
076900000000     C                   SETOFF                                           28
077000000000     C                   Z-ADD     1             J
077100000000     C                   MOVEL     *BLANKS       PD3FFD                         *FILIALE ARRIVO
077200000000     C     TLMTFF        LOOKUP    ORG(J)                                 28
077300000000IF  1C     *IN28         IFEQ      *ON
077400000000     C                   MOVEL     DORG(J)       PD3FFD
077500000000E   1C                   ENDIF
077600000000     C* STAMPA
077700000000     C                   WRITE     TL19D3                                       *RIGA METRICUBI
077800000000     C*
077900000000     C                   ENDSR
078000000000     C*------------------------------------------------------------------------*
078100000000     C* ELATLL - ELABORA I LISTINI
078200000000     C*------------------------------------------------------------------------*
078300000000     C     ELATLL        BEGSR
078400000000     C*
078500000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG STAMPA PRIMO LISTINO
078600000000     C                   MOVEL     '0'           $FIN4
078700000000     C                   Z-ADD     *ZEROS        WTLL
078800000000     C*
078900000000     C* POSIZIONAMENTO E PRIMA LETTURA LISTINI DEL TRAINO
079000000000     C                   EXSR      SETFI4
079100000000     C*
079200000000     C* FINO A FINE LISTINI
079300000000DO  1C     $FIN4         DOWEQ     '0'
079400000000     C*
079500000000     C* STAMPA I LISTINI
079600000000     C                   EXSR      STATLL
079700000000     C*
079800000000     C* LETTURA SUCESSIVA LISTINI
079900000000     C                   EXSR      LETFI4
080000000000E   1C                   ENDDO
080100000000     C*
080200000000     C                   ENDSR
080300000000     C*------------------------------------------------------------------------*
080400000000     C* SETFI4 - POSIZIONAMENTO E PRIMA LETTURA LISTINI
080500000000     C*------------------------------------------------------------------------*
080600000000     C     SETFI4        BEGSR
080700000000     C*
080800000000     C* POSIZIONAMENTO
080900000000     C                   Z-ADD     TLTTRN        KLLTRN                         *TRAINO
081000000000     C                   Z-ADD     TLTDDE        KLLDDE                         *DATA DECORRENZA
081100010725     C     KEYTLL        SETLL     TITLL01L                           98
081200000000     C* LETTURA
081300000000IF  1C     *IN98         IFEQ      *OFF
081400000000     C                   EXSR      LETFI4
081500000000X   1C                   ELSE
081600000000     C                   MOVEL     '1'           $FIN4
081700000000E   1C                   ENDIF
081800000000     C*
081900000000     C                   ENDSR
082000000000     C*------------------------------------------------------------------------*
082100000000     C* LETFI4 - LETTURA SUCESSIVA LISTINI
082200000000     C*------------------------------------------------------------------------*
082300000000     C     LETFI4        BEGSR
082400000000     C*
082500000000     C                   MOVEL     'N'           WRECOK
082600000000     C*
082700000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
082800000000DO  1C     $FIN4         DOWEQ     '0'
082900000000     C     WRECOK        ANDEQ     'N'
083000010725     C     KEYTLL        READE     TITLL01L                               98
083100000000     C*
083200000000IF  2C     *IN98         IFEQ      *ON
083300000000     C                   MOVEL     '1'           $FIN4
083400000000X   2C                   ELSE
083500000000     C                   EXSR      CHKRE4                                       *CONTROLLA RECORD
083600000000E   2C                   ENDIF
083700000000E   1C                   ENDDO
083800000000     C*
083900000000     C                   ENDSR
084000000000     C*------------------------------------------------------------------------*
084100000000     C* CHKRE4 - CONTROLLA VALIDITA' LISTINI
084200000000     C*------------------------------------------------------------------------*
084300000000     C     CHKRE4        BEGSR
084400000000     C*
084500000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
084600960102     C*
084700960102     C* LISTINI ANNULLATI, ESCLUDE
084800960102IF  1C     TLLATB        IFNE      *BLANKS
084900960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
085000960102E   1C                   ENDIF
085100000000     C*
085200000000     C                   ENDSR
085300000000     C*------------------------------------------------------------------------*
085400000000     C* STATLL - STAMPA RIGA LISTINI
085500000000     C*------------------------------------------------------------------------*
085600000000     C     STATLL        BEGSR
085700960115     C*
085800960115     C                   CLEAR                   TL19I4
085900960115     C                   CLEAR                   TL19D4
086000000000     C*
086100000000     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'INTESTAZIONE LISTINI
086200000000IF  1C     *IN01         IFEQ      *ON
086300000000     C                   MOVEL     'L'           WINT
086400000000     C                   EXSR      STATES
086500000000X   1C                   ELSE
086600000000IF  2C     WTLL          IFEQ      *ZEROS
086700000000     C                   WRITE     TL19I4
086800000000     C                   Z-ADD     1             WTLL
086900000000E   2C                   ENDIF
087000000000E   1C                   ENDIF
087100000000     C* CAMPI
087200000000     C                   MOVEL     TLLTMZ        PD4TMZ                         *TIPO AUTOMEZZO
087300000000     C                   Z-ADD     TLLPDR        PD4PDR                         *TRAZIONISTA
087400010725     C                   MOVEL     TLLDIV        PD4DIV                         *DIVISA
087500000000     C                   Z-ADD     TLLILI        PD4ILI                         *IMPORTO LISTINO
087600000000     C                   Z-ADD     TLLIES        PD4IES                         *IMPORTO DA ESPORRE
087700000000     C                   MOVEL     TLLCAU        PD4CAU                         *CAUSALE CONTAB.NE
087800000000     C* DECODIFICHE
087900000000     C                   SETOFF                                           28
088000000000     C                   Z-ADD     1             J
088100000000     C                   MOVEL     *BLANKS       PD4MZD                         *TIPO AUTOMEZZO
088200000000     C     TLLTMZ        LOOKUP    TMZ(J)                                 28
088300000000IF  1C     *IN28         IFEQ      *ON
088400000000     C                   MOVEL     DTMZ(J)       PD4MZD
088500000000X   1C                   ELSE
088600000000IF  2C     TLLTMZ        IFEQ      '99'
088700000000     C                   MOVEL     DES999        PD4MZD
088800000000E   2C                   ENDIF
088900000000E   1C                   ENDIF
089000000000     C                   MOVEL     *BLANKS       PD4DRD                         *TRAZIONISTA
089100000821     C                   EVAL      KLZTIP = 'T '
089200000000     C                   Z-ADD     TLLPDR        KLZPDR
089300000000     C     KEYTLZ        CHAIN     TNTLZ01L                           28
089400000000IF  1C     *IN28         IFEQ      *OFF
089500000000     C                   MOVEL     TLZRSC        PD4DRD
089600000000X   1C                   ELSE
089700000000IF  2C     TLLPDR        IFEQ      9999999
089800000000     C                   MOVEL     DES999        PD4DRD
089900000000E   2C                   ENDIF
090000000000E   1C                   ENDIF
090100000000     C                   SETOFF                                           28
090200000000     C                   Z-ADD     1             J
090300000000     C                   MOVEL     *BLANKS       PD4AUD                         *CAUSALE
090400000000     C     TLLCAU        LOOKUP    CAU(J)                                 28
090500000000IF  1C     *IN28         IFEQ      *ON
090600000000     C                   MOVEL     DCAU(J)       PD4AUD
090700000000E   1C                   ENDIF
090800000000     C* STAMPA
090900000000     C                   WRITE     TL19D4                                       *RIGA LISTINI
091000961227     C*
091100010928      * converto gli importi nella moneta di conto
091200010928     c                   if        tlldiv <> §gedcn
091300010928      * tllili
091400010928     c                   clear                   yeurcods
091500010928     c                   movel     tlldiv        yecdvi
091600010928     c                   z-add     tllili        yecimi
091700010928     c                   movel     §gedcn        yecdvo
091800010928     c                   move      §cvdec        yecdco
091900010928     c                   call      'YEURCO'
092000010928     c                   parm                    yeurcods
092100010928    5c                   if        yecesi  = ' '
092200010928     c                   z-add     yecimo        tllili
092300010928    5c                   endif
092400010928      * tllies
092500010928     c                   clear                   yeurcods
092600010928     c                   movel     tlldiv        yecdvi
092700010928     c                   z-add     tllies        yecimi
092800010928     c                   movel     §gedcn        yecdvo
092900010928     c                   move      §cvdec        yecdco
093000010928     c                   call      'YEURCO'
093100010928     c                   parm                    yeurcods
093200010928    5c                   if        yecesi  = ' '
093300010928     c                   z-add     yecimo        tllies
093400010928    5c                   endif
093500010928     c                   endif
093600961227     C* SOMMA GLI IMPORTI LISTINI -SE PIU' DI UNO PRENDE IL PRIMO-
093700961227IF  1C     $STLL         IFEQ      '0'                                          *LISTINO NON STAMPAT
093800010725     C     TOTILI        ADD       TLLILI        TOTILI           13 2
093900010725     C     TOTIES        ADD       TLLIES        TOTIES           13 2
094000961227     C                   MOVEL     '1'           $STLL
094100961227E   1C                   ENDIF
094200000000     C*
094300000000     C                   ENDSR
094400000000     C*------------------------------------------------------------------------*
094500000000     C* ELATLN - ELABORA LE NOTE
094600000000     C*------------------------------------------------------------------------*
094700000000     C     ELATLN        BEGSR
094800000000     C*
094900000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG STAMPA PRIMA NOTA
095000000000     C                   MOVEL     '0'           $FIN5
095100000000     C                   Z-ADD     *ZEROS        WTLN
095200000000     C*
095300000000     C* POSIZIONAMENTO E PRIMA LETTURA NOTE DEL TRAINO
095400000000     C                   EXSR      SETFI5
095500000000     C*
095600000000     C* FINO A FINE NOTE
095700000000DO  1C     $FIN5         DOWEQ     '0'
095800000000     C*
095900000000     C* STAMPA LE NOTE
096000000000     C                   EXSR      STATLN
096100000000     C*
096200000000     C* LETTURA SUCESSIVA NOTE
096300000000     C                   EXSR      LETFI5
096400000000E   1C                   ENDDO
096500000000     C*
096600000000     C                   ENDSR
096700000000     C*------------------------------------------------------------------------*
096800000000     C* SETFI5 - POSIZIONAMENTO E PRIMA LETTURA NOTE
096900000000     C*------------------------------------------------------------------------*
097000000000     C     SETFI5        BEGSR
097100000000     C*
097200000000     C* POSIZIONAMENTO
097300000000     C                   Z-ADD     TLTTRN        KLNTRN                         *TRAINO
097400000000     C                   Z-ADD     TLTDDE        KLNDDE                         *DATA DECORRENZA
097500000000     C     KEYTLN        SETLL     TNTLN01L                           98
097600000000     C* LETTURA
097700000000IF  1C     *IN98         IFEQ      *OFF
097800000000     C                   EXSR      LETFI5
097900000000X   1C                   ELSE
098000000000     C                   MOVEL     '1'           $FIN5
098100000000E   1C                   ENDIF
098200000000     C*
098300000000     C                   ENDSR
098400000000     C*------------------------------------------------------------------------*
098500000000     C* LETFI5 - LETTURA SUCESSIVA NOTE
098600000000     C*------------------------------------------------------------------------*
098700000000     C     LETFI5        BEGSR
098800000000     C*
098900000000     C                   MOVEL     'N'           WRECOK
099000000000     C*
099100000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
099200000000DO  1C     $FIN5         DOWEQ     '0'
099300000000     C     WRECOK        ANDEQ     'N'
099400000000     C     KEYTLN        READE     TNTLN01L                               98
099500000000     C*
099600000000IF  2C     *IN98         IFEQ      *ON
099700000000     C                   MOVEL     '1'           $FIN5
099800000000X   2C                   ELSE
099900000000     C                   EXSR      CHKRE5                                       *CONTROLLA RECORD
100000000000E   2C                   ENDIF
100100000000E   1C                   ENDDO
100200000000     C*
100300000000     C                   ENDSR
100400000000     C*------------------------------------------------------------------------*
100500000000     C* CHKRE5 - CONTROLLA VALIDITA' NOTE
100600000000     C*------------------------------------------------------------------------*
100700000000     C     CHKRE5        BEGSR
100800000000     C*
100900000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
101000960102     C*
101100960102     C* NOTE ANNULLATE, ESCLUDE
101200960102IF  1C     TLNATB        IFNE      *BLANKS
101300960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
101400960102E   1C                   ENDIF
101500000000     C*
101600000000     C                   ENDSR
101700000000     C*------------------------------------------------------------------------*
101800000000     C* STATLN - STAMPA RIGA NOTE
101900000000     C*------------------------------------------------------------------------*
102000000000     C     STATLN        BEGSR
102100960115     C*
102200960115     C                   CLEAR                   TL19I3
102300960115     C                   CLEAR                   TL19D3
102400000000     C*
102500000000     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'INTESTAZIONE NOTE
102600000000IF  1C     *IN01         IFEQ      *ON
102700000000     C                   MOVEL     'N'           WINT
102800000000     C                   EXSR      STATES
102900000000X   1C                   ELSE
103000000000IF  2C     WTLN          IFEQ      *ZEROS
103100000000     C                   WRITE     TL19I5
103200000000     C                   Z-ADD     1             WTLN
103300000000E   2C                   ENDIF
103400000000E   1C                   ENDIF
103500000000     C* CAMPI
103600000000     C                   Z-ADD     TLNNFM        PD5NFM                         *NUMERO FERMATA
103700000000     C                   MOVEL     TLNTNO        PD5TNO                         *TESTO
103800000000     C* STAMPA
103900000000     C                   WRITE     TL19D5                                       *RIGA NOTE
104000000000     C*
104100000000     C                   ENDSR
104200000000     C*------------------------------------------------------------------------*
104300000000     C* STATES - STAMPA TESTATA
104400000000     C*          INPUT -> WINT: TIPO INTESTAZIONE
104500000000     C*------------------------------------------------------------------------*
104600000000     C     STATES        BEGSR
104700000000     C*
104800000000     C                   WRITE     TL19TE                                       *TESTATA
104900000000     C*
105000000000     C* STAMPA IL TIPO DI INTESTAZIONE RICHIESTO
105100000000IF  1C     WINT          IFEQ      'F'
105200000000     C                   WRITE     TL19I2                                       *INTESTAZIONE FERMAT
105300960116     C                   Z-ADD     1             WTLR
105400000000E   1C                   ENDIF
105500000000IF  1C     WINT          IFEQ      'M'
105600000000     C                   WRITE     TL19I3                                       *INTESTAZIONE MCUBI
105700960116     C                   Z-ADD     1             WTLM
105800000000E   1C                   ENDIF
105900000000IF  1C     WINT          IFEQ      'L'
106000000000     C                   WRITE     TL19I4                                       *INTESTAZIONE LISTIN
106100960116     C                   Z-ADD     1             WTLL
106200000000E   1C                   ENDIF
106300000000IF  1C     WINT          IFEQ      'N'
106400000000     C                   WRITE     TL19I5                                       *INTESTAZIONE NOTE
106500960116     C                   Z-ADD     1             WTLN
106600000000E   1C                   ENDIF
106700000000     C                   MOVEL     '0'           *IN01
106800000000     C*
106900000000     C                   ENDSR
107000961227     C*------------------------------------------------------------------------*
107100961227     C* STATOT - STAMPA RIGA DEI TOTALI LISTINI
107200961227     C*------------------------------------------------------------------------*
107300961227     C     STATOT        BEGSR
107400961227     C*
107500970423     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
107600961227     C                   WRITE     TL19TE                                       *TESTATA
107700961227     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
107800961227     C*
107900961227     C                   Z-ADD     TOTILI        TOCILI
108000961227     C                   Z-ADD     TOTIES        TOCIES
108100011001     c                   if        totili <> *zeros
108200010928     c                   movel     §gedcn        todili
108300011001     c                   endif
108400011001     c                   if        toties <> *zeros
108500010928     c                   movel     §gedcn        todies
108600011001     c                   endif
108700961227     C                   WRITE     TL19TO                                       *TOTALI
108800961227     C*
108900961227     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
109000961227     C*
109100961227     C                   ENDSR
109200960115     C*------------------------------------------------------------------------*
109300960115     C* ELATLT - MEMORIZZA I TRAINI DA STAMPARE
109400960115     C*------------------------------------------------------------------------*
109500960115     C     ELATLT        BEGSR
109600960115     C*
109700960115     C* POSIZIONAMENTO E PRIMA LETTURA TRAINI
109800960115     C                   Z-ADD     *ZEROS        T                 5 0
109900960115     C                   EXSR      SETFIL
110000960115     C*
110100960115     C* FINO A FINE TRAINI
110200960115DO  1C     $FINE         DOWEQ     '0'
110300960115     C*
110400960115     C                   EXSR      MEMTLT
110500960115     C*
110600960115     C* LETTURA SUCESSIVA TRAINI
110700960115     C                   EXSR      LETFIL
110800960115E   1C                   ENDDO
110900960116     C*
111000960116     C* ORDINA I TRAINI PER LA STAMPA
111100960116     C                   EXSR      ORDTLT
111200960115     C*
111300960115     C                   ENDSR
111400960115     C*------------------------------------------------------------------------*
111500960115     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA TRAINI
111600960115     C*------------------------------------------------------------------------*
111700960115     C     SETFIL        BEGSR
111800960115     C*
111900960115     C* POSIZIONAMENTO
112000960115     C     *LOVAL        SETLL     TNTLT01L                           99        *FINE FILE
112100960115     C* RPIMA LETTURA
112200960115IF  1C     *IN99         IFEQ      *OFF
112300960115     C                   EXSR      LETFIL
112400960115X   1C                   ELSE
112500960115     C                   MOVEL     '1'           $FINE
112600960115E   1C                   ENDIF
112700960115     C*
112800960115     C                   ENDSR
112900960115     C*------------------------------------------------------------------------*
113000960115     C* LETFIL - LETTURA SUCESSIVA TRAINI
113100960115     C*------------------------------------------------------------------------*
113200960115     C     LETFIL        BEGSR
113300960115     C*
113400960115     C                   MOVEL     'N'           WRECOK
113500960115     C*
113600960115     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
113700960115DO  1C     $FINE         DOWEQ     '0'
113800960115     C     WRECOK        ANDEQ     'N'
113900960115     C                   READ      TNTLT01L                               99
114000960115IF  2C     *IN99         IFEQ      *ON
114100960115     C                   MOVEL     '1'           $FINE
114200960115X   2C                   ELSE
114300960115     C                   EXSR      CHKREC                                       *CONTROLLA RECROD
114400960115E   2C                   ENDIF
114500960115E   1C                   ENDDO
114600960115     C*
114700960115     C                   ENDSR
114800960115     C*------------------------------------------------------------------------*
114900960115     C* CHKREC - CONTROLLA VALIDITA' TRAINI
115000960115     C*------------------------------------------------------------------------*
115100960115     C     CHKREC        BEGSR
115200960115     C*
115300960115     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
115400960115     C*
115500960115     C* TRAINO ANNULLATO, ESCLUDE
115600960115IF  1C     TLTATB        IFNE      *BLANKS
115700960115     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
115800960115E   1C                   ENDIF
115900960115     C*
116000960115     C* TRAINO NON IN DECORRENZA ALLA DATA RICHIESTA, ESCLUDE
116100060323IF  1C**   PARDDE        IFGT      *ZEROS
116200060323IF  2C**   PARDDE        IFGE      TLTDDE
116300060323     C**   PARDDE        ANDLE     TLTDSC
116400060323X   2C**                 ELSE
116500060323     C**                 MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
116600060323E   2C**                 ENDIF
116700060323E   1C**                 ENDIF
116800060323     c* Traino scaduto prima della data dal o decorrente dopo la data al, es
116900060323     c* clude
117000060323     c     tltdsc        iflt      pardda
117100060323     c     tltdde        orgt      pardde
117200060323     c                   movel     'N'           wrecok
117300060323     c                   endif
117400961021     C*
117500961021     C* NUMERO TRAINO MINORE DEL NUMERO TRAINO DAL RICHIESTO,  ESCLUDE
117600961021IF  1C     PARTR1        IFGT      *ZEROS
117700961021     C     TLTTRN        ANDLT     PARTR1
117800961021     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
117900961021E   1C                   ENDIF
118000961021     C*
118100961021     C* NUMERO TRAINO MAGGIORE DEL NUMERO TRAINO AL RICHIESTO,  ESCLUDE
118200961021IF  1C     PARTR2        IFGT      *ZEROS
118300961021     C     TLTTRN        ANDGT     PARTR2
118400961021     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
118500961021E   1C                   ENDIF
118600961021     C*
118700960115     C* GIORNO PARTENZA <> DA QUELLO RICHIESTO, ESCLUDE
118800960115IF  1C     PARGIO        IFNE      *BLANKS
118900960115     C     PARGIO        SCAN      TLTGSE                                 28
119000960115IF  2C     *IN28         IFEQ      *OFF
119100960115     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
119200960115E   2C                   ENDIF
119300960115E   1C                   ENDIF
119400060323     c* trazionista <> da quello richiesto, esclude
119500060323     c     parpdr        ifgt      *zeros
119600060323     C                   Z-ADD     TLTTRN        KLLTRN                         *TRAINO
119700060323     C                   Z-ADD     TLTDDE        KLLDDE                         *DATA DECORRENZA
119800060323     C                   Z-ADD     parpdr        KLLpdr                         *DATA DECORRENZA
119900060323     C     KEYTLL2       SETLL     TITLL02L
120000060323     c                   if        not %equal(titll02l)
120100060323     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
120200060323     c                   endif
120300060323     c                   endif
120400060323     c* se richiesto bis e traino non bis, esclude
120500060323     c* se richiesti i non bis e traino bis, esclude
120600060323     c                   if        (parbis='S' and tltbis <> 'B' and
120700060323     c                              tlttrn < 9000000) or
120800060323     c                             (parbis='E' and (tltbis='B' or
120900060323     c                              tlttrn >=9000000))
121000060323     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
121100060323     c                   endif
121200060323     c
121300960115     C*--------------------
121400960115     C* FERMATE
121500960115     C*--------------------
121600960115     C***
121700960213     C* TRAINI DI UNA FILIALE ==> FERMATA ALLA FILIALE (SE RICHIESTA)
121800960115     C* TRAINI PARTENZA ==> SOLO FERMATE CHE CARICANO  ALLA FILIALE RICHIESTA
121900960115     C* TRAINI ARRIVO   ==> SOLO FERMATE CHE SCARICANO ALLA FILIALE RICHIESTA
122000960115     C***
122100960115     C                   Z-ADD     *ZEROS        DEPNFM
122200960115IF  1C     WRECOK        IFEQ      'S'                                          *TRAINO ANCORA VALID
122300960213IF  2C     PARORD        IFEQ      '3'                                          *ORD X ORARIO PART
122400960213     C     PARORD        OREQ      '4'                                          *ORD X ORARIO ARR
122500960213     C     PARORD        OREQ      '2'                                          *ORD X FILIALE
122600960213     C     PARFIL        ANDGT     *ZEROS                                       *SE RICH UNA FILIALE
122700960115     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
122800960115     C                   Z-ADD     TLTTRN        KLRTRN                         *TRAINO
122900960115     C                   Z-ADD     TLTDDE        KLRDDE                         *DATA DECORRENZA
123000960115     C     KEYTLR        SETLL     TNTLR01L                           98
123100960115     C     KEYTLR        READE     TNTLR01L                               98
123200960115DO  3C     *IN98         DOWEQ     *OFF
123300960115     C     WRECOK        ANDEQ     'N'
123400960115IF  4C     TLRATB        IFEQ      *BLANKS                                      *NON ANULLATA
123500960115IF  5C     PARFIL        IFEQ      TLRFIL
123600960118IF  6C     PARORD        IFEQ      '3'
123700960116     C     TLRCAR        ANDEQ     'S'
123800960118     C     PARORD        OREQ      '4'
123900960116     C     TLRSCA        ANDEQ     'S'
124000960213     C     PARORD        OREQ      '2'
124100960115     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
124200960115     C                   Z-ADD     TLRNFM        DEPNFM                         *NUMERO FERMATA
124300960115E   6C                   ENDIF
124400960115E   5C                   ENDIF
124500960115E   4C                   ENDIF
124600960115     C     KEYTLR        READE     TNTLR01L                               98
124700960115E   3C                   ENDDO
124800960115E   2C                   ENDIF
124900960115E   1C                   ENDIF
125000960115     C*
125100110527     c* se richiesti solo traini con punti di scambio, cerco
125200110527     c                   if        partpsc<>'N'
125300110527     c                   exsr      cercaTLS
125400110527     c                   if        (partpsc='S' and westls<>'S') or
125500110527     c                             (partpsc='E' and westls='S')
125600110527     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
125700110609     c                   ELSE
125800110609     c                   if        parpsc<>*blanks and wespsc<>'S'
125900110609     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
126000110609     c                   endif
126100110609     c                   endif
126200110609     c
126300110527     c                   endif
126400110609     c
126500960115     C                   ENDSR
126600960115     C*------------------------------------------------------------------------*
126700960115     C* MEMTLT - MEMORIZZA IN SCHIERE I TRAINI VALIDI
126800960115     C*------------------------------------------------------------------------*
126900960115     C     MEMTLT        BEGSR
127000960115     C* TRAINO
127100960115     C                   ADD       1             T
127200960115     C                   Z-ADD     TLTTRN        TTRN(T)
127300960115     C                   Z-ADD     TLTDDE        TDDE(T)
127400960118     C                   Z-ADD     TLTTFP        TTFP(T)
127500960115     C* ORARI
127600960118IF  1C     PARORD        IFEQ      '3'
127700960118     C     PARORD        OREQ      '4'
127800960118     C                   Z-ADD     TLTTRN        KLOTRN                         *TRAINO VALIDO
127900960115     C                   Z-ADD     TLTDDE        KLODDE
128000960115     C                   Z-ADD     DEPNFM        KLONFM                         *FERMATA DEPOSITATA
128100960115     C                   MOVEL     PARGIO        KLOGSE                         *GIORNO SELEZIONATO
128200960115     C     KEYTLO        CHAIN     TNTLO01L                           98
128300960115IF  2C     *IN98         IFEQ      *ON
128400960115     C                   MOVEL     *BLANKS       KLOGSE                         *GIORNO "TUTTI"
128500960115     C     KEYTLO        CHAIN     TNTLO01L                           98
128600960115E   2C                   ENDIF
128700960115IF  2C     *IN98         IFEQ      *OFF
128800960115     C                   MOVEL     TLOGGA        TGHA(T)                        *ARRIVO
128900960115     C                   MOVE      TLOHHA        TGHA(T)
129000960115     C                   MOVEL     TLOGGP        TGHP(T)                        *PARTENZA
129100960115     C                   MOVE      TLOHHP        TGHP(T)
129200960115E   2C                   ENDIF
129300960115E   1C                   ENDIF
129400960115     C*
129500960115     C                   ENDSR
129600960116     C*------------------------------------------------------------------------*
129700960116     C* ORDTLT - ORDINA I TRAINI PER LA STAMPA
129800960116     C*------------------------------------------------------------------------*
129900960116     C     ORDTLT        BEGSR
130000960118     C*
130100960118DO  1C     1             DO        T             J                 4 0
130200960116     C     1             ADD       J             K
130300960118     C*
130400960118DO  2C     K             DO        T             K                 4 0
130500960116     C*
130600960118IF  3C     PARORD        IFEQ      '1'                                          *SECONDO ORDINAMENTO
130700960118     C     TTRN(J)       ANDGT     TTRN(K)                                       TRAINO
130800960118     C     PARORD        OREQ      '2'                                          *SECONDO ORDINAMENTO
130900960118     C     TTFP(J)       ANDGT     TTFP(K)                                       FILIALE
131000960118     C     PARORD        OREQ      '3'                                          *TERZO ORDINAMENTO
131100960118     C     TGHP(J)       ANDGT     TGHP(K)                                       ORARIO PARTENZA
131200960118     C     PARORD        OREQ      '4'                                          *QUARDO ORDINAMENTO
131300960118     C     TGHA(J)       ANDGT     TGHA(K)                                       ORARIO ARRIVO
131400960116     C*
131500960116     C                   MOVEL     TGHP(J)       WGHP              5            *GIORNO+ORA PARTENZA
131600960116     C                   MOVEL     TGHP(K)       TGHP(J)
131700960116     C                   MOVEL     WGHP          TGHP(K)
131800960116     C*
131900960116     C                   MOVEL     TGHA(J)       WGHA              5            *GIORNO+ORA ARRIVO
132000960116     C                   MOVEL     TGHA(K)       TGHA(J)
132100960116     C                   MOVEL     WGHA          TGHA(K)
132200960116     C*
132300960116     C                   Z-ADD     TTRN(J)       WTRN              7 0          *NUMERO TRAINO
132400960116     C                   Z-ADD     TTRN(K)       TTRN(J)
132500960116     C                   Z-ADD     WTRN          TTRN(K)
132600960116     C*
132700960116     C                   Z-ADD     TDDE(J)       WDDE              8 0          *DATA DECORRENZA
132800960116     C                   Z-ADD     TDDE(K)       TDDE(J)
132900960116     C                   Z-ADD     WDDE          TDDE(K)
133000960118     C*
133100960118     C                   Z-ADD     TTFP(J)       WTFP              3 0          *FILIALE
133200960118     C                   Z-ADD     TTFP(K)       TTFP(J)
133300960118     C                   Z-ADD     WTFP          TTFP(K)
133400960116     C*
133500960118E   3C                   ENDIF
133600960118E   2C                   ENDDO
133700960118E   1C                   ENDDO
133800960116     C*
133900960116     C                   ENDSR
134000000000     C*------------------------------------------------------------------------*
134100000000     C* NEWTLT - OPERAZIONI PER NUOVO TRAINO
134200000000     C*------------------------------------------------------------------------*
134300000000     C     NEWTLT        BEGSR
134400960115     C*
134500960115     C                   MOVEL     '0'           $ERR
134600000000     C*
134700000000     C* MEMORIZZA IL NUOVO TRAINO IN UN DEPOSITO
134800960115     C                   Z-ADD     TTRN(T)       DEPTRN                         *TRAINO
134900960115     C                   Z-ADD     TDDE(T)       DEPDDE                         *DATA DECORRENZA
135000960115     C*
135100960115     C* LEGGE IL TRAINO MEMORIZZATO
135200960115     C                   Z-ADD     TTRN(T)       KLTTRN
135300960115     C                   Z-ADD     TDDE(T)       KLTDDE
135400960115     C     KEYTLT        CHAIN     TNTLT01L                           99        *FINE FILE
135500960115IF  1C     *IN99         IFEQ      *ON
135600960115     C                   MOVEL     '1'           $ERR
135700960115E   1C                   ENDIF
135800961227     C*
135900961227     C* FLAG DI PASSAGGIO STAMPA LISTINO
136000961227     C                   MOVEL     '0'           $STLL                          *STAMPA LISTINO
136100000000     C*
136200000000     C                   ENDSR
136300000000     C*------------------------------------------------------------------------*
136400000000     C* STAPAR - STAMPA PAGINA PARAMETRI
136500000000     C*------------------------------------------------------------------------*
136600000000     C     STAPAR        BEGSR
136700960115     C* PARAMETRI
136800960118     C                   MOVEL     PARORD        PACORD
136900960118IF  1C     PARORD        IFEQ      '1'
137000960118     C                   MOVEL     DPAR(11)      PADORD
137100960115E   1C                   ENDIF
137200960118IF  1C     PARORD        IFEQ      '2'
137300960118     C                   MOVEL     DPAR(12)      PADORD
137400960118E   1C                   ENDIF
137500960118IF  1C     PARORD        IFEQ      '3'
137600960118     C                   MOVEL     DPAR(13)      PADORD
137700960118E   1C                   ENDIF
137800960118IF  1C     PARORD        IFEQ      '4'
137900960118     C                   MOVEL     DPAR(14)      PADORD
138000960118E   1C                   ENDIF
138100960118     C*
138200060323IF  1C     PARDDa        IFGT      *ZEROS
138300060323     C                   Z-ADD     PARDDa        G08INV                         *DECORRENZA
138400060323     C                   Z-ADD     *ZEROS        G08DAT
138500060323     C                   MOVEL     '3'           G08ERR
138600060323     C                   CALL      'XSRDA8'
138700060323     C                   PARM                    WLBDA8
138800060323     C                   Z-ADD     G08DAT        PACDDA
138900060323E   1C                   ENDIF
139000960115IF  1C     PARDDE        IFGT      *ZEROS
139100000000     C                   Z-ADD     PARDDE        G08INV                         *DECORRENZA
139200000000     C                   Z-ADD     *ZEROS        G08DAT
139300000000     C                   MOVEL     '3'           G08ERR
139400000000     C                   CALL      'XSRDA8'
139500000000     C                   PARM                    WLBDA8
139600000000     C                   Z-ADD     G08DAT        PACDDE
139700960115E   1C                   ENDIF
139800960115IF  1C     PARFIL        IFGT      *ZEROS
139900960115     C                   Z-ADD     PARFIL        PACFIL                         *FILIALE
140000960115     C                   Z-ADD     1             J
140100960115     C     PARFIL        LOOKUP    ORG(J)                                 28
140200960115     C                   MOVEL     DORG(J)       PADFIL
140300960115E   1C                   ENDIF
140400960115IF  1C     PARGIO        IFNE      *BLANKS
140500960115     C                   MOVEL     PARGIO        PACGIO                         *GIORNO DI PARTENZA
140600960115     C                   MOVEL     PARGIO        N1                1 0
140700960115     C                   MOVEL     DPAR(N1)      PADGIO
140800960115E   1C                   ENDIF
140900961021     C                   Z-ADD     PARTR1        PACTR1                         *NUMERO TRAINO DAL
141000961021     C                   Z-ADD     PARTR2        PACTR2                         *NUMERO TRAINO AL
141100960115     C* TESTATA
141200960118     C                   MOVEL     PADORD        TEDORD
141300060323IF  1C     PARDDA        IFGT      *ZEROS
141400060323     C                   MOVEL     'DAL'         TIDDDA
141500060323     C                   Z-ADD     PACDDA        TECDDA
141600060323E   1C                   ENDIF
141700960118IF  1C     PARDDE        IFGT      *ZEROS
141800960118     C                   MOVEL     'AL'          TIDDDE
141900960118     C                   Z-ADD     PACDDE        TECDDE
142000960118E   1C                   ENDIF
142100960116IF  1C     PARFIL        IFGT      *ZEROS
142200960116     C                   MOVEL     'DA'          TIDFIL
142300960116     C                   MOVEL     PADFIL        TEDFIL
142400960116E   1C                   ENDIF
142500060323     c* trazionista
142600060323     c                   clear                   padpdr
142700060323     c                   clear                   pacpdr
142800060323     c                   if        parpdr > *zeros
142900060323     c                   move      parpdr        pacpdr
143000060323     C                   EVAL      KLZTIP = 'T '
143100060323     C                   z-add     parpdr        KLZPDR
143200060323     C     KEYTLZ        CHAIN     TNTLZ01L                           28
143300060323IF  2C     *IN28         IFEQ      *OFF
143400060323     C                   MOVEL     TLZRSC        padpdr                         *DESCR.NE TRAZIONIST
143500060323     c                   endif
143600060323     c                   endif
143700060323     c* traini bis si/no/escludi
143800060323     c                   clear                   padbis
143900060323     c                   select
144000060323     c                   when      parbis = 'S'
144100060323     c                   eval      padbis = 'Si'
144200060323     c                   when      parbis = 'N'
144300060323     c                   eval      padbis = 'No'
144400060323     c                   when      parbis = 'E'
144500060323     c                   eval      padbis = 'Escludi'
144600060323     c                   endsl
144700110527
144800110527     c                   select
144900110527     c                   when      partpsc = 'S'
145000110527     c                   eval      padtpsc = 'Si'
145100110527     c                   when      partpsc = 'N'
145200110527     c                   eval      padtpsc = 'No'
145300110527     c                   when      partpsc = 'E'
145400110527     c                   eval      padtpsc = 'Escludi'
145500110527     c                   endsl
145600960115     C*
145700110609     c* codice spunti di scambio
145800110609     c     parpsc        chain     tnpsc01l
145900110609    2c                   if        %found(tnpsc01l)
146000110609     c                   eval      desloc=pscloc
146100110609    2c                   if        pscprv<>*blanks
146200110609     c                   eval      desloc=%trim(desloc)+'  '+pscprv+pscnar
146300110609     c                   else
146400110609     c                   eval      desloc=%trim(desloc)+' '+pscnar
146500110609    2c                   endif
146600110609     c                   eval      desind=pscind
146700110609     c                   eval      descap=psccap
146800110609    1c                   endif
146900110609     c
147000110609     C                   WRITE     TL19TE                                       *TESTATA
147100110609     C                   WRITE     TL19PA                                       *PARAMETRI
147200000000     C*
147300000000     C                   ENDSR
147400000000     C*------------------------------------------------------------------------*
147500000000     C* CARTAB - CARICA LE TABELLE
147600000000     C*------------------------------------------------------------------------*
147700000000     C     CARTAB        BEGSR
147800000000     C*--------------------
147900000000     C* FILIALI
148000000000     C*--------------------
148100000000     C                   Z-ADD     *ZEROS        J                 4 0
148200000000     C     *LOVAL        SETLL     AZORG01L
148300000000     C                   READ      AZORG01L                               99
148400000000DO  1C     *IN99         DOWEQ     *OFF
148500000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
148600000000     C                   ADD       1             J
148700000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
148800000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
148900000000E   2C                   ENDIF
149000000000     C                   READ      AZORG01L                               99
149100000000E   1C                   ENDDO
149200000000     C*--------------------
149300000000     C* TIPI TRAINO
149400000000     C*--------------------
149500000000     C                   Z-ADD     *ZEROS        J
149600020715     c                   Z-Add     1             KtbKut
149700000000     C                   MOVEL     'TV'          KTBCOD
149800000000     C     KE2TAB        SETLL     TABEL00F
149900000000     C     KE2TAB        READE     TABEL00F                               99
150000000000DO  1C     *IN99         DOWEQ     *OFF
150100000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
150200000000     C                   ADD       1             J
150300000000     C                   MOVEL     TBLKEY        TTR(J)                         *CODICE
150400000000     C                   MOVEL     TBLUNI        DSTV
150500000000     C                   MOVEL     §TVDES        DTTR(J)                        *DESCRIZIONE
150600000000E   2C                   ENDIF
150700000000     C     KE2TAB        READE     TABEL00F                               99
150800000000E   1C                   ENDDO
150900000000     C*--------------------
151000000000     C* TIPI AUTOMEZZO
151100000000     C*--------------------
151200000000     C                   Z-ADD     *ZEROS        J
151300020715     c                   Z-Add     1             KtbKut
151400000000     C                   MOVEL     'TU'          KTBCOD
151500000000     C     KE2TAB        SETLL     TABEL00F
151600000000     C     KE2TAB        READE     TABEL00F                               99
151700000000DO  1C     *IN99         DOWEQ     *OFF
151800000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
151900000000     C                   ADD       1             J
152000000000     C                   MOVEL     TBLKEY        TMZ(J)                         *CODICE
152100000000     C                   MOVEL     TBLUNI        DSTU
152200000000     C                   MOVEL     §TUDES        DTMZ(J)                        *DESCRIZIONE
152300000000E   2C                   ENDIF
152400000000     C     KE2TAB        READE     TABEL00F                               99
152500000000E   1C                   ENDDO
152600000000     C*--------------------
152700000000     C* CAUSALI CONTABILIZZAZIONE
152800000000     C*--------------------
152900020715     c                   If        DutLpo = 'S'
153000000821     C                   OPEN      ANATB01L
153100000000     C                   Z-ADD     *ZEROS        J
153200000821     C                   MOVE      KNSIF         WCOMO4            4
153300000821     C                   MOVEL     WCOMO4        WSOC              3
153400000821     C                   movel     WSOC          KAtazi
153500000211     C                   MOVEL     'Y4Z'         KatCOD
153600000211     C     KEATAB        CHAIN     ANATB01L                           99
153700000821IF  2C     *IN99         IFEQ      *ON
153800000218     C                   movel     '888'         KAtazi
153900000211     C     KEATAB        CHAIN     ANATB01L                           99
154000000821E   2C                   ENDIF
154100000821DO  2C     *IN99         DOWEQ     *OFF
154200000211     C                   ADD       1             J
154300000211     C                   MOVEL     ATBKEY        CAU(J)
154400000211     C                   MOVEL     ATBUNI        ANGY4ZDS
154500000211     C                   MOVEL     §4ZDES        DCAU(J)
154600000211     C     KEATAB        READE     ANATB01L                               99
154700000821E   2C                   ENDDO
154800000821     C                   CLOSE     ANATB01L
154900000821E   1C                   ENDIF
155000010928      *--------------------
155100010928      * Moneta di conto
155200010928      *--------------------
155300010928     c                   clear                   dged
155400010928     c                   clear                   tibs02ds
155500010928     c                   movel     'L'           t02tla
155600010928     c                   movel     'C'           t02mod
155700010928     c                   movel     knsif         t02sif
155800010928     c                   movel     'GED'         t02cod
155900010928     c                   movel     '1'           t02ke1
156000010928     c                   call      'TIBS02R'
156100010928     c                   parm                    kpjba
156200010928     c                   parm                    tibs02ds
156300010928    5c                   movel     t02uni        dged
156400010928
156500010928     c                   clear                   dscv
156600010928     c                   movel     'CV'          ktbcod
156700010928     c                   movel(p)  §gedcn        ktbkey
156800010928     c     ktab          chain     tabel00f
156900010928     c                   if        %found(tabel00f)
157000010928     c                   movel     tbluni        dscv
157100010928     c                   endif
157200010928
157300000000     C*
157400000000     C                   ENDSR
157500000000     C*------------------------------------------------------------------------*
157600000000     C* *INZSR - ROUTINE INIZIALE
157700000000     C*------------------------------------------------------------------------*
157800000000     C     *INZSR        BEGSR
157900000000     C*--------------------
158000000000     C* RICEVIMENTO PARAMETRI
158100000000     C*--------------------
158200000000     C     *ENTRY        PLIST
158300000000     C                   PARM                    KPJBA
158400000000     C                   MOVEL     KPJBU         PARAM
158500000000     C*--------------------
158600000000     C* VARIABILI RIFERITE AL DATA BASE
158700000000     C*--------------------
158800000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
158900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
159000010928     C     *LIKE         DEFINE    TBLkey        KTBkey
159100000211     C     *LIKE         DEFINE    ATBCOD        KATCOD
159200000211     C     *LIKE         DEFINE    ATBAZI        KATAZI
159300000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
159400000000     C     *LIKE         DEFINE    TLZPDR        KLZPDR                         *TNTLZ00F
159500000821     C     *LIKE         DEFINE    TLZTIP        KLZTIP
159600000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
159700000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
159800000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
159900000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
160000000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
160100000000     C     *LIKE         DEFINE    TLODDE        KLODDE
160200000000     C     *LIKE         DEFINE    TLONFM        KLONFM
160300000000     C     *LIKE         DEFINE    TLOGSE        KLOGSE
160400000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
160500000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
160600010725     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
160700000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
160800060323     C     *LIKE         DEFINE    TLLpdr        KLLpdr
160900000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
161000000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
161100000000     C     *LIKE         DEFINE    TLTTRN        DEPTRN                         *DEPOSITI
161200000000     C     *LIKE         DEFINE    TLTDDE        DEPDDE
161300960116     C     *LIKE         DEFINE    TLRNFM        DEPNFM
161400000000     C*--------------------
161500000000     C* CHIAVI DI LETTURA
161600000000     C*--------------------
161700000000     C* LETTURA AZORG01L - COMPLETA
161800000000     C     KEYORG        KLIST
161900000000     C                   KFLD                    KORFIL                         *FILIALE
162000000000     C* LETTURA TNTAB00F - PARZIALE
162100000000     C     KE2TAB        KLIST
162200000000     C                   KFLD                    KTBKUT                         *UTENTE
162300000000     C                   KFLD                    KTBCOD                         *TABELLA
162400010928
162500010928     C     KTAB          KLIST
162600010928     C                   KFLD                    ktbkut
162700010928     C                   KFLD                    Ktbcod
162800010928     C                   KFLD                    Ktbkey
162900000211     C* LETTURA ANATB01L - PARZIALE
163000000211     C     KEATAB        KLIST
163100000211     C                   KFLD                    KATCOD
163200000211     C                   KFLD                    KATAZI
163300000000     C* LETTURA TNTLZ01L - COMPLETA
163400000000     C     KEYTLZ        KLIST
163500000821     C                   KFLD                    KLZTIP                         *TIPO
163600000821     C                   KFLD                    KLZPDR                         *CODICE
163700000000     C* LETTURA TNTLT01L - COMPLETA
163800000000     C     KEYTLT        KLIST
163900000000     C                   KFLD                    KLTTRN                         *TRAINO
164000000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
164100000000     C* LETTURA TNTLR01L - PARZIALE
164200000000     C     KEYTLR        KLIST
164300000000     C                   KFLD                    KLRTRN                         *TRAINO
164400000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
164500000000     C* LETTURA TNTLO01L - COMPLETA
164600000000     C     KEYTLO        KLIST
164700000000     C                   KFLD                    KLOTRN                         *TRAINO
164800000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
164900000000     C                   KFLD                    KLONFM                         *NUMERO FERMATA
165000000000     C                   KFLD                    KLOGSE                         *GIORNO DI PARTENZA
165100000000     C* LETTURA TNTLM01L - PARZIALE
165200000000     C     KEYTLM        KLIST
165300000000     C                   KFLD                    KLMTRN                         *TRAINO
165400000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
165500010725     C* LETTURA TITLL01L - PARZIALE
165600000000     C     KEYTLL        KLIST
165700000000     C                   KFLD                    KLLTRN                         *TRAINO
165800000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
165900060323     C* LETTURA TITLL02L - PARZIALE
166000060323     C     KEYTLL2       KLIST
166100060323     C                   KFLD                    KLLpdr                         *TRAINO
166200060323     C                   KFLD                    KLLTRN                         *TRAINO
166300060323     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
166400000000     C* LETTURA TNTLN01L - PARZIALE
166500000000     C     KEYTLN        KLIST
166600000000     C                   KFLD                    KLNTRN                         *TRAINO
166700000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
166800110526     C* LETTURA generica per traino
166900110526     C     KTLT          KLIST
167000110526     C                   KFLD                    ktrn                           *TRAINO
167100110526     C                   KFLD                    Kdde                           *DATA DECORRENZA
167200000000     C*--------------------
167300000000     C* VARIABILI DI CONTROLLO
167400000000     C*--------------------
167500000000     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA TRAINI
167600000000     C                   MOVEL     '0'           $FIN2             1            *FINE LETTURA FERMAT
167700000000     C                   MOVEL     '0'           $FIN3             1            *FINE LETTURA MCUBI
167800000000     C                   MOVEL     '0'           $FIN4             1            *FINE LETTURA LISTIN
167900000000     C                   MOVEL     '0'           $FIN5             1            *FINE LETTURA NOTE
168000000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
168100000000     C                   MOVEL     '1'           *IN01                          *OVERFLOW
168200000000     C                   MOVEL     *BLANKS       WINT              1            *INTESTAZ. IN STAMPA
168300000000     C                   Z-ADD     *ZEROS        WTLR              1 0          *STAMPA 1^FERMATA
168400000000     C                   Z-ADD     *ZEROS        WTLM              1 0          *STAMPA 1^MCUBO
168500000000     C                   Z-ADD     *ZEROS        WTLL              1 0          *STAMPA 1^LISTINO
168600000000     C                   Z-ADD     *ZEROS        WTLN              1 0          *STAMPA 1^NOTA
168700960115     C                   MOVEL     '0'           $ERR              1            *ERRORE IN LETTURA
168800961227     C                   MOVEL     '0'           $STLL             1            *STAMPA LISTINO
168900000000     C*
169000000000     C* REPERISCE I DATI UTENTE
169100020715
169200020715     c     *dtaara       define    §azute        azuteds
169300020715     c     *dtaara       define    §datiute      ddatiute
169400020715
169500020715     c                   in(E)     *dtaara
169600020715     c                   if        %Error or rsut = *blanks
169700020715     c                   clear                   tibs34ds
169800020715     c                   call      'TIBS34R'
169900020715     c                   parm                    Tibs34Ds
170000020715     c                   in        *dtaara
170100020715     c                   endif
170200000000     C*
170300000000     C* CARICA LE TABELLE
170400000000     C                   EXSR      CARTAB
170500960115     C*
170600960118     C* MEMORIZZA IN SCHIERA I TRAINI VALIDI DA STAMPARE CON ORDINE PRESCELTO
170700960115     C                   EXSR      ELATLT
170800000000     C*
170900000000     C                   ENDSR
171000000000     O*------------------------------------------------------------------------*
171100960115**   DPAR - DECODIFICHE PARAMETRI
171200960115LUNEDI'             1
171300960115MARTEDI'            2
171400960115MERCOLEDI'          3
171500960115GIOVEDI'            4
171600960115VENERDI'            5
171700960115SABATO              6
171800960115DOMENICA            7
1719009601188
1720009601189
17210096011810
172200960118NUMERO TRAINO       11
172300961022PUNTO OPERATIVO     12
172400960118ORARIO DI PARTENZA  13
172500960118ORARIO DI ARRIVO    14
17260096011815
172700960115**   DIN2 - DECODIFICHE INTESTAZIONE
172800960115¦ARR    LUN    PAR¦   1
172900960115¦ARR    MAR    PAR¦   2
173000960115¦ARR    MER    PAR¦   3
173100960115¦ARR    GIO    PAR¦   4
173200960115¦ARR    VEN    PAR¦   5
173300960115¦ARR    SAB    PAR¦   6
173400960115¦ARR    DOM    PAR¦   7
173500960115¦ARR   TUTTI   PAR¦   8
