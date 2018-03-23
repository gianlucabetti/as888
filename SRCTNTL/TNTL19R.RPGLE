000100000000     H*------------------------------------------------------------------------*
000200000000     H* STAMPA TRAINI - ESEGUE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE                                                              *
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL19P   O    E             PRINTER OFLIND(*IN01)
000900000000     FAZORG01L  IF   E           K DISK
001000000000     FTABEL00F  IF   E           K DISK
001100000821     FANATB01L  IF   E           K DISK    USROPN
001200000000     FTNTLZ01L  IF   E           K DISK
001300000000     FTNTLT01L  IF   E           K DISK
001400000000     FTNTLR01L  IF   E           K DISK
001500000000     FTNTLO01L  IF   E           K DISK
001600000000     FTNTLM01L  IF   E           K DISK
001700010725     FTITLL01L  IF   E           K DISK
001800060323     FTITLL02L  IF   E           K DISK    rename(titll000:titll002)
001900000000     FTNTLN01L  IF   E           K DISK
002000110526     FTNTLS01L  IF   E           K DISK
002100110526     FTNPSC01L  IF   E           K DISK
002200000000     D*------------------------------------------------------------------------*
002300000000     D* SCHIERE                                                                *
002400000000     D*------------------------------------------------------------------------*
002500000000     D*------------------
002600000000     D* DECODIFICHE PARAMETRI
002700000000     D*------------------
002800960118     D DPAR            S             20    DIM(15) CTDATA PERRCD(1)
002900960115     D*------------------
003000960115     D* DECODIFICHE INTESTAZIONE
003100960115     D*------------------
003200960115     D DIN2            S             19    DIM(8) CTDATA PERRCD(1)
003300000000     D*-------------------
003400000000     D* FILIALI
003500000000     D*-------------------
003600971125     D ORG             S              3  0 DIM(500)                             *CODICE
003700971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
003800000000     D*-------------------
003900000000     D* TIPI TRAINO
004000000000     D*-------------------
004100000000     D TTR             S              1    DIM(20)                              *TIPI TRAINI
004200000000     D DTTR            S             30    DIM(20)                              *DESCRIZIONE
004300000000     D*-------------------
004400000000     D* TIPI AUTOMEZZO
004500000000     D*-------------------
004600000000     D TMZ             S              2    DIM(20)                              *TIPI AUTOMEZZO
004700000000     D DTMZ            S             30    DIM(20)                              *DESCRIZIONE
004800000000     D*-------------------
004900000000     D* CAUSALI CONTABILIZZAZIONE
005000000000     D*-------------------
005100000000     D CAU             S              1    DIM(20)                              *CODICE
005200000000     D DCAU            S             30    DIM(20)                              *DESCRIZIONE
005300960115     D*-------------------
005400960115     D* DATI TRAINO
005500960115     D*-------------------
005600150123     D TTRN            S              7  0 DIM(9000)                            *NUMERO TRAINO
005700150123     D TDDE            S              8  0 DIM(9000)                            *DECORRENZA
005800150123     D TTFP            S              8  0 DIM(9000)                            *FILIALE PARTENZA
005900150123     D TGHA            S              5    DIM(9000)                            *GIORNO ARRIVO
006000150123     D TGHP            S              5    DIM(9000)                            *GIORNO PARTENZA
006100000000     D*-------------------
006200960115     D* DATI FERMATE
006300000000     D*-------------------
006400960115     D FGSE            S              1    DIM(8)                               *GIORNI DI PARTENZA
006500960115     D FGGA            S              1    DIM(8)                               *GIORNO ARRIVO
006600960115     D FHHA            S              4  0 DIM(8)                               *ORA ARRIVO
006700960115     D FGGP            S              1    DIM(8)                               *GIORNO PARTENZA
006800960115     D FHHP            S              4  0 DIM(8)                               *ORA PARTENZA
006900000000     D*------------------------------------------------------------------------*
007000000000     D* INPUT                                                                  *
007100000000     D*------------------------------------------------------------------------*
007200000000     D*---------------------
007300000000     D* ARCHITETTURA
007400000000     D*---------------------
007500000000     D KPJBA         E DS
007600000000     D  PROFIL                 1     10                                         UTENTE
007700000000     D*---------------------
007800000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007900000000     D*---------------------
008000000000     D DSMSG         E DS
008100020715
008200020715     d DdatiUte      e ds
008300020715     d AzuteDs       e ds                  ExtName(Azute00f)
008400020715     d Tibs34Ds      e ds                  Inz
008500020715
008600960115     D*---------------------
008700000211     D* DS PARAMETRI DI LANCIO
008800960115     D*------------------
008900960115     D PARAM           DS
009000960118     D  PARORD                 1      1
009100960118     D  PARDDE                 2      9  0
009200960118     D  PARFIL                10     12  0
009300960118     D  PARGIO                13     13
009400961021     D  PARTR1                14     20  0
009500961021     D  PARTR2                21     27  0
009600060323     D  PARpdr                28     34  0
009700060323     D  PARdda                35     42  0
009800060323     D  PARbis                43     43
009900110527     D  PARTpsc               44     44
010000110609     D  PARpsc                45     54
010100000000     D*---------------------
010200000000     D* DS TIPI TRAINO
010300000000     D*---------------------
010400000000     D DSTV          E DS
010500000000     D*---------------------
010600000000     D* DS TIPI AUOTMEZZO
010700000000     D*---------------------
010800000000     D DSTU          E DS
010900000000     D*---------------------
011000000000     D* DS CAUSALI CONTABILIZZAZIONE
011100000000     D*---------------------
011200000211     D ANGY4ZDS      E DS
011300010928      *--------------------
011400010928      * Ds - moneta di conto
011500010928      *--------------------
011600010928     d dged          e ds
011700010928     d tibs02ds      e ds
011800010928     d dscv          e ds
011900010928     d yeurcods      e ds
012000000000     D*---------------------
012100000000     D* DS CONTROLLO DATA (8)
012200000000     D*------------------
012300000000     D WLBDA8          DS                  INZ
012400000000     D  G08DAT                 1      8  0
012500000000     D  G08INV                 9     16  0
012600000000     D  G08ERR                17     17
012700000000     D  G08TGI                18     22  0
012800000000     D*------------------
012900000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
013000000000     D*------------------
013100960118     D* COMMENTI
013200000000     D DES999          C                   CONST('DI CARTELLO')
013300110526     d
013400110526     d Ktrn            S                   LIKE(TLTTRN)
013500110526     d KDDE            S                   LIKE(TLTDDE)
013600110526     d scaggs          S                   LIKE(TLsggs)
013700110526     d scahhs          S                   LIKE(TLshhs)
013800110526     d Wstampa         S              1
013900000000     C*------------------------------------------------------------------------*
014000000000     C* MAIN LINE
014100000000     C*------------------------------------------------------------------------*
014200000000     C*
014300000000     C* STAMPA PARAMETRI LANCIO
014400000000     C                   EXSR      STAPAR
014500000000     C*
014600960115     C* POSIZIONAMENTO E PRIMA LETTURA TRAINI CARIATI IN MEMORIA
014700960115     C                   Z-ADD     1             T
014800000000     C*
014900000000     C* FINO A FINE TRAINI
015000960115DO  1C     TTRN(T)       DOWGT     *ZEROS
015100000000     C*
015200000000     C* OPERAZIONI PER NUOVO TRAINO
015300000000     C                   EXSR      NEWTLT
015400000000     C*
015500000000     C* STAMPA IL TRAINO
015600960115IF  2C     $ERR          IFEQ      '0'                                          *NO ERRORI
015700000000     C                   EXSR      STATLT
015800000000     C*
015900960115     C* STAMPA GLI ALTRI DATI
016000000000     C                   EXSR      ELATLR                                       *ELABORA LE FERMATE
016100000000     C                   EXSR      ELATLM                                       *ELABORA I METRICUBI
016200000000     C                   EXSR      ELATLL                                       *ELABORA I LISTINI
016300000000     C                   EXSR      ELATLN                                       *ELABORA LE NOTE
016400960115E   2C                   ENDIF
016500000000     C*
016600960115     C* LETTURA SUCCESSIVA TRAINI CARIATI IN MEMORIA
016700960115     C                   ADD       1             T
016800000000E   1C                   ENDDO
016900961227     C*
017000961227     C* STAMPA RIGA DEI TOTALI LISTINI
017100961227IF  1C     TOTILI        IFGT      *ZEROS
017200961227     C     TOTIES        ORGT      *ZEROS
017300961227     C                   EXSR      STATOT
017400961227E   1C                   ENDIF
017500000000     C*
017600000000     C* STAMPA RIGA DI FINE PROSPETTO
017700000000     C                   WRITE     TL19FI
017800000000     C*
017900000000     C                   SETON                                        LR
018000000000     C*------------------------------------------------------------------------*
018100000000     C* STATLT - STAMPA RIGA TRAINO
018200000000     C*------------------------------------------------------------------------*
018300000000     C     STATLT        BEGSR
018400110527     c                   clear                   wstampa
018500110526     c* Verifico se per questo traino è presente il punti dii scambio
018600110526     c*  lo cerco
018700110526     C                   EXSR      CERCATLS
018800000000     C*
018900960115     C                   CLEAR                   TL19DE
019000960115     C*
019100960115     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI SOLO LA RIGA DI SEPARAZIONE
019200000000IF  1C     *IN01         IFEQ      *ON
019300000000     C                   MOVEL     'T'           WINT
019400000000     C                   EXSR      STATES
019500000000E   1C                   ENDIF
019600000000     C* CAMPI
019700000000     C                   Z-ADD     TLTTRN        PDETRN                         *TRAINO
019800960115     C                   SETOFF                                           28
019900960115     C                   Z-ADD     1             J
020000960115     C                   MOVEL     *BLANKS       PDEFPD                         *FILIALE PARTENZA
020100960115     C     TLTTFP        LOOKUP    ORG(J)                                 28
020200960115IF  1C     *IN28         IFEQ      *ON
020300960115     C                   MOVEL     DORG(J)       PDEFPD
020400960115E   1C                   ENDIF
020500960115     C                   SETOFF                                           28
020600960115     C                   Z-ADD     1             J
020700960115     C                   MOVEL     *BLANKS       PDEFAD                         *FILIALE ARRIVO
020800960115     C     TLTTFA        LOOKUP    ORG(J)                                 28
020900960115IF  1C     *IN28         IFEQ      *ON
021000960115     C                   MOVEL     DORG(J)       PDEFAD
021100960115E   1C                   ENDIF
021200960115     C                   SETOFF                                           28
021300960115     C                   Z-ADD     1             J
021400960115     C                   MOVEL     *BLANKS       PDETRD                         *TIPO TRAINO
021500960115     C     TLTTTR        LOOKUP    TTR(J)                                 28
021600960115IF  1C     *IN28         IFEQ      *ON
021700960115     C                   MOVEL     DTTR(J)       PDETRD
021800960115E   1C                   ENDIF
021900960115IF  1C     TLTBIS        IFEQ      'B'
022000960424     C                   MOVEL     'BIS'         PDEISD                         *NORMALE/BIS
022100960115X   1C                   ELSE
022200960424     C                   MOVEL     *BLANKS       PDEISD                         *NORMALE/BIS
022300960115E   1C                   ENDIF
022400960115     C                   MOVEA     TLTGSE        FGSE
022500960115     C                   MOVEL     '...'         PDELUN
022600960115IF  1C     FGSE(1)       IFNE      *BLANKS
022700960115     C                   MOVEL     DPAR(1)       PDELUN                         *LUNEDI'
022800960115E   1C                   ENDIF
022900960115     C                   MOVEL     '...'         PDEMAR
023000960115IF  1C     FGSE(2)       IFNE      *BLANKS
023100960115     C                   MOVEL     DPAR(2)       PDEMAR                         *MARTEDI'
023200960115E   1C                   ENDIF
023300960115     C                   MOVEL     '...'         PDEMER
023400960115IF  1C     FGSE(3)       IFNE      *BLANKS
023500960115     C                   MOVEL     DPAR(3)       PDEMER                         *MERCOLEDI'
023600960115E   1C                   ENDIF
023700960115     C                   MOVEL     '...'         PDEGIO
023800960115IF  1C     FGSE(4)       IFNE      *BLANKS
023900960115     C                   MOVEL     DPAR(4)       PDEGIO                         *GIOVEDI'
024000960115E   1C                   ENDIF
024100960115     C                   MOVEL     '...'         PDEVEN
024200960115IF  1C     FGSE(5)       IFNE      *BLANKS
024300960115     C                   MOVEL     DPAR(5)       PDEVEN                         *VENERDI'
024400960115E   1C                   ENDIF
024500960115     C                   MOVEL     '...'         PDESAB
024600960115IF  1C     FGSE(6)       IFNE      *BLANKS
024700960115     C                   MOVEL     DPAR(6)       PDESAB                         *SABATO
024800960115E   1C                   ENDIF
024900960115     C                   MOVEL     '...'         PDEDOM
025000960115IF  1C     FGSE(7)       IFNE      *BLANKS
025100960115     C                   MOVEL     DPAR(7)       PDEDOM                         *DOMENICA
025200960115E   1C                   ENDIF
025300960115     C                   MOVEL     *BLANKS       PDEMZD                         *TIPO AUTOMEZZO
025400960115     C                   SETOFF                                           28
025500960115     C                   Z-ADD     1             J
025600960115     C     TLTTMZ        LOOKUP    TMZ(J)                                 28
025700960115IF  1C     *IN28         IFEQ      *ON
025800960115     C                   MOVEL     DTMZ(J)       PDEMZD
025900960115E   1C                   ENDIF
026000960115     C                   SETOFF                                           28
026100960115     C                   Z-ADD     1             J
026200960115     C                   MOVEL     *BLANKS       PDEESD                         *FILIALE IMPORTO
026300960115     C     TLTFES        LOOKUP    ORG(J)                                 28
026400960115IF  1C     *IN28         IFEQ      *ON
026500960115     C                   MOVEL     DORG(J)       PDEESD
026600960115E   1C                   ENDIF
026700960115     C                   Z-ADD     TLTDDE        G08INV                         *DATA DECORRENZA
026800960115     C                   Z-ADD     *ZEROS        G08DAT
026900960115     C                   MOVEL     '3'           G08ERR
027000960115     C                   CALL      'XSRDA8'
027100960115     C                   PARM                    WLBDA8
027200960115     C                   Z-ADD     G08DAT        PDEDDE
027300960115     C                   Z-ADD     TLTDSC        G08INV                         *DATA SCADENZA
027400960115     C                   Z-ADD     *ZEROS        G08DAT
027500960115     C                   MOVEL     '3'           G08ERR
027600960115     C                   CALL      'XSRDA8'
027700960115     C                   PARM                    WLBDA8
027800960115     C                   Z-ADD     G08DAT        PDEDSC
027900000000     C* STAMPA
028000970423     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
028100970423     C                   WRITE     TL19DE                                       *RIGA TRAINO
028200000000     C*
028300000000     C                   ENDSR
028400000000     C*------------------------------------------------------------------------*
028500110526     C* CERCA SE PRESENTE PUNTI DI SCAMBIO
028600000000     C*------------------------------------------------------------------------*
028700110526     C     CercaTLS      BEGSR
028800110526     c
028900110526     c                   clear                   scaggs
029000110526     c                   clear                   scahhs
029100110526     c                   clear                   westls            1
029200110609     c                   clear                   wesPSC            1
029300110526     c                   eval      ktrn=tlttrn
029400110526     c                   eval      kdde=tltdde
029500110526     c     ktlt          setll     tntls01l
029600110526     c     ktlt          reade     tntls01l
029700110527     c                   dow       not %eof(tntls01l)
029800110526     c
029900110526     c                   if        tlsatb=' '
030000110526     c                   eval      westls='S'
030100110609     c
030200110609     c                   if        parpsc<>*blanks and parpsc=tlspsc
030300110609     c                   eval      wespsc='S'
030400110609     c                   endif
030500110609     c
030600110609     c* memorizzo orario di scambio se si tratta del giorno ' '
030700110526     c                   if        tlsgse=' '
030800110526     c                   eval      scaggs=tlsggs
030900110526     c                   eval      scahhs=tlshhs
031000110526     c                   else
031100110526     c                   if        scaggs=' '
031200110526     c                   eval      scaggs=tlsggs
031300110526     c                   eval      scahhs=tlshhs
031400110526     c                   endif
031500110526     c                   endif
031600110526     c
031700110526     c                   endif
031800110526     c
031900110526     c     ktlt          reade     tntls01l
032000110526     c                   enddo
032100110526     c
032200110526     C                   ENDSR
032300110526     C*------------------------------------------------------------------------*
032400110526     C* ELATLR - ELABORA LE FERMATE
032500110526     C*------------------------------------------------------------------------*
032600110526     C     ELATLR        BEGSR
032700000000     C*
032800000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG PRIMA RIGA FERMATA
032900000000     C                   MOVEL     '0'           $FIN2
033000000000     C                   Z-ADD     *ZEROS        WTLR
033100000000     C*
033200000000     C* POSIZIONAMENTO E PRIMA LETTURA FERMATE DEL TRAINO
033300000000     C                   EXSR      SETFI2
033400000000     C*
033500000000     C* FINO A FINE FERMATE
033600000000DO  1C     $FIN2         DOWEQ     '0'
033700000000     C*
033800000000     C* STAMPA LA FERMATA
033900000000     C                   EXSR      STATLR
034000000000     C*
034100000000     C* LETTURA SUCESSIVA FERMATE
034200000000     C                   EXSR      LETFI2
034300000000E   1C                   ENDDO
034400110527     c
034500110527     c* se non ancora stampato punto di scambio lo faccio ora
034600110527     c                   if        westls='S' and wstampa=' '
034700110527     c                   exsr      StaTLS
034800110527     c                   endif
034900000000     C*
035000000000     C                   ENDSR
035100000000     C*------------------------------------------------------------------------*
035200000000     C* SETFI2 - POSIZIONAMENTO E PRIMA LETTURA FERMATE
035300000000     C*------------------------------------------------------------------------*
035400000000     C     SETFI2        BEGSR
035500000000     C*
035600000000     C* POSIZIONAMENTO
035700000000     C                   Z-ADD     TLTTRN        KLRTRN                         *TRAINO
035800000000     C                   Z-ADD     TLTDDE        KLRDDE                         *DATA DECORRENZA
035900000000     C     KEYTLR        SETLL     TNTLR01L                           98
036000000000     C* LETTURA
036100000000IF  1C     *IN98         IFEQ      *OFF
036200000000     C                   EXSR      LETFI2
036300000000X   1C                   ELSE
036400000000     C                   MOVEL     '1'           $FIN2
036500000000E   1C                   ENDIF
036600000000     C*
036700000000     C                   ENDSR
036800000000     C*------------------------------------------------------------------------*
036900000000     C* LETFI2 - LETTURA SUCESSIVA FERMATE
037000000000     C*------------------------------------------------------------------------*
037100000000     C     LETFI2        BEGSR
037200000000     C*
037300000000     C                   MOVEL     'N'           WRECOK
037400000000     C*
037500000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
037600000000DO  1C     $FIN2         DOWEQ     '0'
037700000000     C     WRECOK        ANDEQ     'N'
037800000000     C     KEYTLR        READE     TNTLR01L                               98
037900000000     C*
038000000000IF  2C     *IN98         IFEQ      *ON
038100000000     C                   MOVEL     '1'           $FIN2
038200000000X   2C                   ELSE
038300000000     C                   EXSR      CHKRE2                                       *CONTROLLA RECORD
038400000000E   2C                   ENDIF
038500000000E   1C                   ENDDO
038600000000     C*
038700000000     C                   ENDSR
038800000000     C*------------------------------------------------------------------------*
038900000000     C* CHKRE2 - CONTROLLA VALIDITA' FERMATE
039000000000     C*------------------------------------------------------------------------*
039100000000     C     CHKRE2        BEGSR
039200960102     C*
039300000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
039400960102     C*
039500960102     C* FERMATA ANNULLATA, ESCLUDE
039600960102IF  1C     TLRATB        IFNE      *BLANKS
039700960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
039800960102E   1C                   ENDIF
039900960102     C*
040000000000     C                   ENDSR
040100000000     C*------------------------------------------------------------------------*
040200000000     C* STATLR - STAMPA RIGA FERMATA
040300000000     C*------------------------------------------------------------------------*
040400000000     C     STATLR        BEGSR
040500960115     C*
040600960115     C                   CLEAR                   TL19I2
040700960115     C                   CLEAR                   TL19D2
040800000000     C*--------------------
040900000000     C* FERMATA
041000000000     C*--------------------
041100000000     C* CAMPI
041200000000     C                   MOVEL     TLRSCA        PD2SCA                         *SCARICO
041300000000     C                   MOVEL     TLRCAR        PD2CAR                         *CARICO
041400000000     C                   Z-ADD     TLRKMT        PD2KMT                         *CHILOMETRI
041500000000     C                   Z-ADD     TLRTEM        PD2TEM                         *TEMPO
041600000000     C                   MOVEL     TLRGFV        PD2GFV                         *GIORNO FV
041700000000     C* DECODIFICHE
041800000000     C                   SETOFF                                           28
041900000000     C                   Z-ADD     1             J
042000000000     C                   MOVEL     *BLANKS       PD2ILD                         *FERMATA
042100000000     C     TLRFIL        LOOKUP    ORG(J)                                 28
042200000000IF  1C     *IN28         IFEQ      *ON
042300000000     C                   MOVEL     DORG(J)       PD2ILD
042400000000E   1C                   ENDIF
042500960115IF  1C     TLRTFM        IFEQ      'R'                                          *TIPO FERMATA RITORN
042600960115     C                   MOVEL     'R'           PD2TFM
042700000000E   1C                   ENDIF
042800000000     C*--------------------
042900000000     C* ORARI
043000000000     C*--------------------
043100960115     C                   CLEAR                   FGGA
043200960115     C                   CLEAR                   FHHA
043300960115     C                   CLEAR                   FGGP
043400960115     C                   CLEAR                   FHHP
043500000000     C*
043600000000     C* MEMORIZZA GLI ORARI DI TUTTI I GIORNI DELLA FERMATA
043700000000     C                   Z-ADD     TLRTRN        KLOTRN                         *TRAINO
043800000000     C                   Z-ADD     TLRDDE        KLODDE                         *DECORRENZA
043900000000     C                   Z-ADD     TLRNFM        KLONFM                         *NUMERO FERMATA
044000000000DO  1C     1             DO        8             J
044100960115IF  2C     FGSE(J)       IFNE      *BLANKS
044200000000     C     J             OREQ      8
044300000000IF  3C     J             IFEQ      8
044400000000     C                   MOVEL     *BLANKS       KLOGSE                         *GIORNO
044500000000X   3C                   ELSE
044600960115     C                   MOVEL     FGSE(J)       KLOGSE
044700000000E   3C                   ENDIF
044800000000     C     KEYTLO        CHAIN     TNTLO01L                           98
044900000000IF  3C     *IN98         IFEQ      *OFF
045000000000     C     TLOATB        ANDEQ     *BLANKS
045100960115     C                   MOVEL     TLOGGA        FGGA(J)
045200960115     C                   Z-ADD     TLOHHA        FHHA(J)
045300960115     C                   MOVEL     TLOGGP        FGGP(J)
045400960115     C                   Z-ADD     TLOHHP        FHHP(J)
045500000000E   3C                   ENDIF
045600000000E   2C                   ENDIF
045700000000E   1C                   ENDDO
045800110526     c* Verifico se prima di stampare la fermata ---> devo stampare punti scambio
045900110526     c                   exsr      ctrpsc
046000110526     c
046100000000     C* CAMPI
046200960115IF  1C     FHHA(1)       IFGT      *ZEROS                                       *ORARI DI LUNEDI'
046300960115     C     FHHP(1)       ORGT      *ZEROS
046400960115     C                   MOVEL     '¦'           PD2L11
046500960115     C                   MOVEL     DIN2(1)       I2DLUN
046600960115     C                   MOVEL     FGGA(1)       PD2GA1
046700960115     C                   Z-ADD     FHHA(1)       PD2HA1
046800960115     C                   MOVEL     '-'           PD2TR1
046900960115     C                   MOVEL     FGGP(1)       PD2GP1
047000960115     C                   Z-ADD     FHHP(1)       PD2HP1
047100960115     C                   MOVEL     '¦'           PD2L21
047200000000E   1C                   ENDIF
047300960115IF  1C     FHHA(2)       IFGT      *ZEROS                                       *ORARI DI MARTEDI'
047400960115     C     FHHP(2)       ORGT      *ZEROS
047500960115     C                   MOVEL     '¦'           PD2L12
047600960115     C                   MOVEL     DIN2(2)       I2DMAR
047700960115     C                   MOVEL     FGGA(2)       PD2GA2
047800960115     C                   Z-ADD     FHHA(2)       PD2HA2
047900960115     C                   MOVEL     '-'           PD2TR2
048000960115     C                   MOVEL     FGGP(2)       PD2GP2
048100960115     C                   Z-ADD     FHHP(2)       PD2HP2
048200960115     C                   MOVEL     '¦'           PD2L22
048300000000E   1C                   ENDIF
048400960115IF  1C     FHHA(3)       IFGT      *ZEROS                                       *ORARI DI MERCOLEDI'
048500960115     C     FHHP(3)       ORGT      *ZEROS
048600960115     C                   MOVEL     '¦'           PD2L13
048700960115     C                   MOVEL     DIN2(3)       I2DMER
048800960115     C                   MOVEL     FGGA(3)       PD2GA3
048900960115     C                   Z-ADD     FHHA(3)       PD2HA3
049000960115     C                   MOVEL     '-'           PD2TR3
049100960115     C                   MOVEL     FGGP(3)       PD2GP3
049200960115     C                   Z-ADD     FHHP(3)       PD2HP3
049300960115     C                   MOVEL     '¦'           PD2L23
049400000000E   1C                   ENDIF
049500960115IF  1C     FHHA(4)       IFGT      *ZEROS                                       *ORARI DI GIOVEDI'
049600960115     C     FHHP(4)       ORGT      *ZEROS
049700960115     C                   MOVEL     '¦'           PD2L14
049800960115     C                   MOVEL     DIN2(4)       I2DGIO
049900960115     C                   MOVEL     FGGA(4)       PD2GA4
050000960115     C                   Z-ADD     FHHA(4)       PD2HA4
050100960115     C                   MOVEL     '-'           PD2TR4
050200960115     C                   MOVEL     FGGP(4)       PD2GP4
050300960115     C                   Z-ADD     FHHP(4)       PD2HP4
050400960115     C                   MOVEL     '¦'           PD2L24
050500000000E   1C                   ENDIF
050600960115IF  1C     FHHA(5)       IFGT      *ZEROS                                       *ORARI DI VENERDI'
050700960115     C     FHHP(5)       ORGT      *ZEROS
050800960115     C                   MOVEL     '¦'           PD2L15
050900960115     C                   MOVEL     DIN2(5)       I2DVEN
051000960115     C                   MOVEL     FGGA(5)       PD2GA5
051100960115     C                   Z-ADD     FHHA(5)       PD2HA5
051200960115     C                   MOVEL     '-'           PD2TR5
051300960115     C                   MOVEL     FGGP(5)       PD2GP5
051400960115     C                   Z-ADD     FHHP(5)       PD2HP5
051500960115     C                   MOVEL     '¦'           PD2L25
051600000000E   1C                   ENDIF
051700960115IF  1C     FHHA(6)       IFGT      *ZEROS                                       *ORARI DI SABATO
051800960115     C     FHHP(6)       ORGT      *ZEROS
051900960115     C                   MOVEL     '¦'           PD2L16
052000960115     C                   MOVEL     DIN2(6)       I2DSAB
052100960115     C                   MOVEL     FGGA(6)       PD2GA6
052200960115     C                   Z-ADD     FHHA(6)       PD2HA6
052300960115     C                   MOVEL     '-'           PD2TR6
052400960115     C                   MOVEL     FGGP(6)       PD2GP6
052500960115     C                   Z-ADD     FHHP(6)       PD2HP6
052600960115     C                   MOVEL     '¦'           PD2L26
052700000000E   1C                   ENDIF
052800960115IF  1C     FHHA(7)       IFGT      *ZEROS                                       *ORARI DI DOMENICA
052900960115     C     FHHP(7)       ORGT      *ZEROS
053000960115     C                   MOVEL     '¦'           PD2L17
053100960115     C                   MOVEL     DIN2(7)       I2DDOM
053200960115     C                   MOVEL     FGGA(7)       PD2GA7
053300960115     C                   Z-ADD     FHHA(7)       PD2HA7
053400960115     C                   MOVEL     '-'           PD2TR7
053500960115     C                   MOVEL     FGGP(7)       PD2GP7
053600960115     C                   Z-ADD     FHHP(7)       PD2HP7
053700960115     C                   MOVEL     '¦'           PD2L27
053800000000E   1C                   ENDIF
053900960115IF  1C     FHHA(8)       IFGT      *ZEROS                                       *ORARI DI TUTTI I GG
054000960115     C     FHHP(8)       ORGT      *ZEROS
054100960115     C                   MOVEL     '¦'           PD2L18
054200960115     C                   MOVEL     DIN2(8)       I2DTUT
054300960115     C                   MOVEL     FGGA(8)       PD2GA8
054400960115     C                   Z-ADD     FHHA(8)       PD2HA8
054500960115     C                   MOVEL     '-'           PD2TR8
054600960115     C                   MOVEL     FGGP(8)       PD2GP8
054700960115     C                   Z-ADD     FHHP(8)       PD2HP8
054800960115     C                   MOVEL     '¦'           PD2L28
054900000000E   1C                   ENDIF
055000960115     C*
055100960115     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'NTESTAZIONE FERMATA
055200960115IF  1C     *IN01         IFEQ      *ON
055300960115     C                   MOVEL     'F'           WINT
055400960115     C                   EXSR      STATES
055500960115X   1C                   ELSE
055600960115IF  2C     WTLR          IFEQ      *ZEROS
055700960115     C                   WRITE     TL19I2
055800960115     C                   Z-ADD     1             WTLR
055900960115E   2C                   ENDIF
056000960115E   1C                   ENDIF
056100960115     C* STAMPA
056200000000     C                   WRITE     TL19D2                                       *RIGA FERMATA E ORAR
056300000000     C*
056400000000     C                   ENDSR
056500000000     C*------------------------------------------------------------------------*
056600110526     C* controlla se devo stampare punti di scambio
056700000000     C*------------------------------------------------------------------------*
056800110526     C     ctrPSC        BEGSR
056900110526     c                   if        westls='S'  and wstampa=' '
057000110526    2c                   if        fgga(8)<>' '
057100110526    3c                   if        scaggs<fgga(8) or (scaggs=fgga(8) and
057200110526     c                             scahhs<fhha(8))
057300110526     c                   eval      wstampa   ='S'
057400110526    3c                   endif
057500110526   x2c                   else
057600110526     c*
057700110526    3c                   if        scaggs<fggp(8) or (scaggs=fggp(8) and
057800110527     c                             scahhs<fhhp(8))
057900110526     c                   eval      wstampa   ='S'
058000110526    3c                   endif
058100110526    2c                   endif
058200110526     c
058300110526    2c                   if        wstampa   ='S'
058400110526     c                   exsr      StaTLS
058500110526    2c                   endif
058600110526     c                   endif
058700110526     c
058800110526     C                   ENDSR
058900110526     C*------------------------------------------------------------------------*
059000110526     C* Stampa datio punto di scambio
059100110526     C*------------------------------------------------------------------------*
059200110526     C     STAtls        BEGSR
059300110527     c* stampo prima il giorno  TUTTI e poi gli altri in ordine
059400110527     c                   eval      ktrn=tlttrn
059500110527     c                   eval      kdde=tltdde
059600110527     c     ktlt          setll     tntls01l
059700110527     c     ktlt          reade     tntls01l
059800110527     c                   dow       not %eof(tntls01l)
059900110527
060000110527     c                   if        tlsatb=' '
060100110527     c                   select
060200110527     c                   when      tlsgse=' '
060300110527     c                   eval      p6dgior=%subst(din2(8):8:5)
060400110527     c                   when      tlsgse='1'
060500110527     c                   eval      p6dgior=%subst(din2(1):8:5)
060600110527     c                   when      tlsgse='2'
060700110527     c                   eval      p6dgior=%subst(din2(2):8:5)
060800110527     c                   when      tlsgse='3'
060900110527     c                   eval      p6dgior=%subst(din2(3):8:5)
061000110527     c                   when      tlsgse='4'
061100110527     c                   eval      p6dgior=%subst(din2(4):8:5)
061200110527     c                   when      tlsgse='5'
061300110527     c                   eval      p6dgior=%subst(din2(5):8:5)
061400110527     c                   when      tlsgse='6'
061500110527     c                   eval      p6dgior=%subst(din2(6):8:5)
061600110527     c                   when      tlsgse='7'
061700110527     c                   eval      p6dgior=%subst(din2(7):8:5)
061800110527     c                   ENDSL
061900110527
062000110527     c                   eval      p6cggs=tlsggs
062100110527     c                   eval      p6chhs=tlshhs
062200110527     c                   eval      p6cnot=tlsnot
062300110527     c                   eval      p6ctrns=tlstrns
062400110527     c     tlspsc        chain     tnpsc01l
062500110527     c                   if        %found(tnpsc01l)
062600110527     c                   eval      p6dpsc=%trim(tlspsc) + '-' + %trim(pscloc) +
062700110527     c                             ' ' + pscprv + ' ' + %trim(pscind)
062800110527     c                   else
062900110527     c                   eval      p6dpsc=%trim(tlspsc) + '-' + '??????????????'
063000110527     c                   endif
063100110527     c
063200110527     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'NTESTAZIONE FERMATA
063300110527IF  1C     *IN01         IFEQ      *ON
063400110527     C                   MOVEL     'F'           WINT
063500110527     C                   EXSR      STATES
063600110527X   1C                   ELSE
063700110527IF  2C     WTLR          IFEQ      *ZEROS
063800110527     C                   WRITE     TL19I2
063900110527     C                   Z-ADD     1             WTLR
064000110527E   2C                   ENDIF
064100110527E   1C                   ENDIF
064200110527     C* STAMPA
064300110527     C                   WRITE     TL19D6                                       *RIGA FERMATA E ORAR
064400110527     c                   endif
064500110527     c
064600110527     c     ktlt          reade     tntls01l
064700110527     c                   enddo
064800110527     c
064900110526     C                   ENDSR
065000110526     C*------------------------------------------------------------------------*
065100110526     C* ELATLM - ELABORA I METRICUBI
065200110526     C*------------------------------------------------------------------------*
065300110526     C     ELATLM        BEGSR
065400000000     C*
065500000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG STAMPA PRIMO METROCUBO
065600000000     C                   MOVEL     '0'           $FIN3
065700000000     C                   Z-ADD     *ZEROS        WTLM
065800000000     C*
065900000000     C* POSIZIONAMENTO E PRIMA LETTURA METRICUBI DEL TRAINO
066000000000     C                   EXSR      SETFI3
066100000000     C*
066200000000     C* FINO A FINE METRICUBI
066300000000DO  1C     $FIN3         DOWEQ     '0'
066400000000     C*
066500000000     C* STAMPA I METRICUBI
066600000000     C                   EXSR      STATLM
066700000000     C*
066800000000     C* LETTURA SUCESSIVA METRICUBI
066900000000     C                   EXSR      LETFI3
067000000000E   1C                   ENDDO
067100000000     C*
067200000000     C                   ENDSR
067300000000     C*------------------------------------------------------------------------*
067400000000     C* SETFI3 - POSIZIONAMENTO E PRIMA LETTURA METRICUBI
067500000000     C*------------------------------------------------------------------------*
067600000000     C     SETFI3        BEGSR
067700000000     C*
067800000000     C* POSIZIONAMENTO
067900000000     C                   Z-ADD     TLTTRN        KLMTRN                         *TRAINO
068000000000     C                   Z-ADD     TLTDDE        KLMDDE                         *DATA DECORRENZA
068100000000     C     KEYTLM        SETLL     TNTLM01L                           98
068200000000     C* LETTURA
068300000000IF  1C     *IN98         IFEQ      *OFF
068400000000     C                   EXSR      LETFI3
068500000000X   1C                   ELSE
068600000000     C                   MOVEL     '1'           $FIN3
068700000000E   1C                   ENDIF
068800000000     C*
068900000000     C                   ENDSR
069000000000     C*------------------------------------------------------------------------*
069100000000     C* LETFI3 - LETTURA SUCESSIVA METRICUBI
069200000000     C*------------------------------------------------------------------------*
069300000000     C     LETFI3        BEGSR
069400000000     C*
069500000000     C                   MOVEL     'N'           WRECOK
069600000000     C*
069700000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
069800000000DO  1C     $FIN3         DOWEQ     '0'
069900000000     C     WRECOK        ANDEQ     'N'
070000000000     C     KEYTLM        READE     TNTLM01L                               98
070100000000     C*
070200000000IF  2C     *IN98         IFEQ      *ON
070300000000     C                   MOVEL     '1'           $FIN3
070400000000X   2C                   ELSE
070500000000     C                   EXSR      CHKRE3                                       *CONTROLLA RECORD
070600000000E   2C                   ENDIF
070700000000E   1C                   ENDDO
070800000000     C*
070900000000     C                   ENDSR
071000000000     C*------------------------------------------------------------------------*
071100000000     C* CHKRE3 - CONTROLLA VALIDITA' METRICUBI
071200000000     C*------------------------------------------------------------------------*
071300000000     C     CHKRE3        BEGSR
071400000000     C*
071500000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
071600960102     C*
071700960102     C* METRICUBI ANNULLATI, ESCLUDE
071800960102IF  1C     TLMATB        IFNE      *BLANKS
071900960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
072000960102E   1C                   ENDIF
072100000000     C*
072200000000     C                   ENDSR
072300000000     C*------------------------------------------------------------------------*
072400000000     C* STATLM - STAMPA RIGA METRICUBI
072500000000     C*------------------------------------------------------------------------*
072600000000     C     STATLM        BEGSR
072700960115     C*
072800960115     C                   CLEAR                   TL19I3
072900960115     C                   CLEAR                   TL19D3
073000000000     C*
073100000000     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'INTESTAZIONE METRICUBI
073200000000IF  1C     *IN01         IFEQ      *ON
073300000000     C                   MOVEL     'M'           WINT
073400000000     C                   EXSR      STATES
073500000000X   1C                   ELSE
073600000000IF  2C     WTLM          IFEQ      *ZEROS
073700000000     C                   WRITE     TL19I3
073800000000     C                   Z-ADD     1             WTLM
073900000000E   2C                   ENDIF
074000000000E   1C                   ENDIF
074100000000     C* CAMPI
074200000000     C                   Z-ADD     TLMTFP        PD3TFP                         *FILIALE PARTENZA
074300000000     C                   Z-ADD     TLMTFA        PD3TFA                         *FILIALE SCARICO
074400000000     C                   Z-ADD     TLMTFF        PD3TFF                         *FILIALE ARRIVO
074500960424     C                   Z-ADD     TLMMCA        PD3MCA                         *MCUBI NORMALI
074600960424     C                   Z-ADD     TLMMCB        PD3MCB                         *MCUBI BIS
074700960105     C                   Z-ADD     TLMMCT        PD3MCT                         *MCUBI TRAINI SEDE
074800050928     c* incremento campi di totale metri cubi da stampare a fine elab.
074900050928     c                   add       tlmmca        tocmca
075000050928     c                   add       tlmmcb        tocmcb
075100050928     c                   add       tlmmct        tocmct
075200000000     C* DECODIFICHE
075300000000     C                   SETOFF                                           28
075400000000     C                   Z-ADD     1             J
075500000000     C                   MOVEL     *BLANKS       PD3FPD                         *FILIALE PARTENZA
075600000000     C     TLMTFP        LOOKUP    ORG(J)                                 28
075700000000IF  1C     *IN28         IFEQ      *ON
075800000000     C                   MOVEL     DORG(J)       PD3FPD
075900000000E   1C                   ENDIF
076000000000     C                   SETOFF                                           28
076100000000     C                   Z-ADD     1             J
076200000000     C                   MOVEL     *BLANKS       PD3FAD                         *FILIALE SCARICO
076300000000     C     TLMTFA        LOOKUP    ORG(J)                                 28
076400000000IF  1C     *IN28         IFEQ      *ON
076500000000     C                   MOVEL     DORG(J)       PD3FAD
076600000000E   1C                   ENDIF
076700000000     C                   SETOFF                                           28
076800000000     C                   Z-ADD     1             J
076900000000     C                   MOVEL     *BLANKS       PD3FFD                         *FILIALE ARRIVO
077000000000     C     TLMTFF        LOOKUP    ORG(J)                                 28
077100000000IF  1C     *IN28         IFEQ      *ON
077200000000     C                   MOVEL     DORG(J)       PD3FFD
077300000000E   1C                   ENDIF
077400000000     C* STAMPA
077500000000     C                   WRITE     TL19D3                                       *RIGA METRICUBI
077600000000     C*
077700000000     C                   ENDSR
077800000000     C*------------------------------------------------------------------------*
077900000000     C* ELATLL - ELABORA I LISTINI
078000000000     C*------------------------------------------------------------------------*
078100000000     C     ELATLL        BEGSR
078200000000     C*
078300000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG STAMPA PRIMO LISTINO
078400000000     C                   MOVEL     '0'           $FIN4
078500000000     C                   Z-ADD     *ZEROS        WTLL
078600000000     C*
078700000000     C* POSIZIONAMENTO E PRIMA LETTURA LISTINI DEL TRAINO
078800000000     C                   EXSR      SETFI4
078900000000     C*
079000000000     C* FINO A FINE LISTINI
079100000000DO  1C     $FIN4         DOWEQ     '0'
079200000000     C*
079300000000     C* STAMPA I LISTINI
079400000000     C                   EXSR      STATLL
079500000000     C*
079600000000     C* LETTURA SUCESSIVA LISTINI
079700000000     C                   EXSR      LETFI4
079800000000E   1C                   ENDDO
079900000000     C*
080000000000     C                   ENDSR
080100000000     C*------------------------------------------------------------------------*
080200000000     C* SETFI4 - POSIZIONAMENTO E PRIMA LETTURA LISTINI
080300000000     C*------------------------------------------------------------------------*
080400000000     C     SETFI4        BEGSR
080500000000     C*
080600000000     C* POSIZIONAMENTO
080700000000     C                   Z-ADD     TLTTRN        KLLTRN                         *TRAINO
080800000000     C                   Z-ADD     TLTDDE        KLLDDE                         *DATA DECORRENZA
080900010725     C     KEYTLL        SETLL     TITLL01L                           98
081000000000     C* LETTURA
081100000000IF  1C     *IN98         IFEQ      *OFF
081200000000     C                   EXSR      LETFI4
081300000000X   1C                   ELSE
081400000000     C                   MOVEL     '1'           $FIN4
081500000000E   1C                   ENDIF
081600000000     C*
081700000000     C                   ENDSR
081800000000     C*------------------------------------------------------------------------*
081900000000     C* LETFI4 - LETTURA SUCESSIVA LISTINI
082000000000     C*------------------------------------------------------------------------*
082100000000     C     LETFI4        BEGSR
082200000000     C*
082300000000     C                   MOVEL     'N'           WRECOK
082400000000     C*
082500000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
082600000000DO  1C     $FIN4         DOWEQ     '0'
082700000000     C     WRECOK        ANDEQ     'N'
082800010725     C     KEYTLL        READE     TITLL01L                               98
082900000000     C*
083000000000IF  2C     *IN98         IFEQ      *ON
083100000000     C                   MOVEL     '1'           $FIN4
083200000000X   2C                   ELSE
083300000000     C                   EXSR      CHKRE4                                       *CONTROLLA RECORD
083400000000E   2C                   ENDIF
083500000000E   1C                   ENDDO
083600000000     C*
083700000000     C                   ENDSR
083800000000     C*------------------------------------------------------------------------*
083900000000     C* CHKRE4 - CONTROLLA VALIDITA' LISTINI
084000000000     C*------------------------------------------------------------------------*
084100000000     C     CHKRE4        BEGSR
084200000000     C*
084300000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
084400960102     C*
084500960102     C* LISTINI ANNULLATI, ESCLUDE
084600960102IF  1C     TLLATB        IFNE      *BLANKS
084700960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
084800960102E   1C                   ENDIF
084900000000     C*
085000000000     C                   ENDSR
085100000000     C*------------------------------------------------------------------------*
085200000000     C* STATLL - STAMPA RIGA LISTINI
085300000000     C*------------------------------------------------------------------------*
085400000000     C     STATLL        BEGSR
085500960115     C*
085600960115     C                   CLEAR                   TL19I4
085700960115     C                   CLEAR                   TL19D4
085800000000     C*
085900000000     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'INTESTAZIONE LISTINI
086000000000IF  1C     *IN01         IFEQ      *ON
086100000000     C                   MOVEL     'L'           WINT
086200000000     C                   EXSR      STATES
086300000000X   1C                   ELSE
086400000000IF  2C     WTLL          IFEQ      *ZEROS
086500000000     C                   WRITE     TL19I4
086600000000     C                   Z-ADD     1             WTLL
086700000000E   2C                   ENDIF
086800000000E   1C                   ENDIF
086900000000     C* CAMPI
087000000000     C                   MOVEL     TLLTMZ        PD4TMZ                         *TIPO AUTOMEZZO
087100000000     C                   Z-ADD     TLLPDR        PD4PDR                         *TRAZIONISTA
087200010725     C                   MOVEL     TLLDIV        PD4DIV                         *DIVISA
087300000000     C                   Z-ADD     TLLILI        PD4ILI                         *IMPORTO LISTINO
087400000000     C                   Z-ADD     TLLIES        PD4IES                         *IMPORTO DA ESPORRE
087500000000     C                   MOVEL     TLLCAU        PD4CAU                         *CAUSALE CONTAB.NE
087600000000     C* DECODIFICHE
087700000000     C                   SETOFF                                           28
087800000000     C                   Z-ADD     1             J
087900000000     C                   MOVEL     *BLANKS       PD4MZD                         *TIPO AUTOMEZZO
088000000000     C     TLLTMZ        LOOKUP    TMZ(J)                                 28
088100000000IF  1C     *IN28         IFEQ      *ON
088200000000     C                   MOVEL     DTMZ(J)       PD4MZD
088300000000X   1C                   ELSE
088400000000IF  2C     TLLTMZ        IFEQ      '99'
088500000000     C                   MOVEL     DES999        PD4MZD
088600000000E   2C                   ENDIF
088700000000E   1C                   ENDIF
088800000000     C                   MOVEL     *BLANKS       PD4DRD                         *TRAZIONISTA
088900000821     C                   EVAL      KLZTIP = 'T '
089000000000     C                   Z-ADD     TLLPDR        KLZPDR
089100000000     C     KEYTLZ        CHAIN     TNTLZ01L                           28
089200000000IF  1C     *IN28         IFEQ      *OFF
089300000000     C                   MOVEL     TLZRSC        PD4DRD
089400000000X   1C                   ELSE
089500000000IF  2C     TLLPDR        IFEQ      9999999
089600000000     C                   MOVEL     DES999        PD4DRD
089700000000E   2C                   ENDIF
089800000000E   1C                   ENDIF
089900000000     C                   SETOFF                                           28
090000000000     C                   Z-ADD     1             J
090100000000     C                   MOVEL     *BLANKS       PD4AUD                         *CAUSALE
090200000000     C     TLLCAU        LOOKUP    CAU(J)                                 28
090300000000IF  1C     *IN28         IFEQ      *ON
090400000000     C                   MOVEL     DCAU(J)       PD4AUD
090500000000E   1C                   ENDIF
090600000000     C* STAMPA
090700000000     C                   WRITE     TL19D4                                       *RIGA LISTINI
090800961227     C*
090900010928      * converto gli importi nella moneta di conto
091000010928     c                   if        tlldiv <> §gedcn
091100010928      * tllili
091200010928     c                   clear                   yeurcods
091300010928     c                   movel     tlldiv        yecdvi
091400010928     c                   z-add     tllili        yecimi
091500010928     c                   movel     §gedcn        yecdvo
091600010928     c                   move      §cvdec        yecdco
091700010928     c                   call      'YEURCO'
091800010928     c                   parm                    yeurcods
091900010928    5c                   if        yecesi  = ' '
092000010928     c                   z-add     yecimo        tllili
092100010928    5c                   endif
092200010928      * tllies
092300010928     c                   clear                   yeurcods
092400010928     c                   movel     tlldiv        yecdvi
092500010928     c                   z-add     tllies        yecimi
092600010928     c                   movel     §gedcn        yecdvo
092700010928     c                   move      §cvdec        yecdco
092800010928     c                   call      'YEURCO'
092900010928     c                   parm                    yeurcods
093000010928    5c                   if        yecesi  = ' '
093100010928     c                   z-add     yecimo        tllies
093200010928    5c                   endif
093300010928     c                   endif
093400961227     C* SOMMA GLI IMPORTI LISTINI -SE PIU' DI UNO PRENDE IL PRIMO-
093500961227IF  1C     $STLL         IFEQ      '0'                                          *LISTINO NON STAMPAT
093600010725     C     TOTILI        ADD       TLLILI        TOTILI           13 2
093700010725     C     TOTIES        ADD       TLLIES        TOTIES           13 2
093800961227     C                   MOVEL     '1'           $STLL
093900961227E   1C                   ENDIF
094000000000     C*
094100000000     C                   ENDSR
094200000000     C*------------------------------------------------------------------------*
094300000000     C* ELATLN - ELABORA LE NOTE
094400000000     C*------------------------------------------------------------------------*
094500000000     C     ELATLN        BEGSR
094600000000     C*
094700000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG STAMPA PRIMA NOTA
094800000000     C                   MOVEL     '0'           $FIN5
094900000000     C                   Z-ADD     *ZEROS        WTLN
095000000000     C*
095100000000     C* POSIZIONAMENTO E PRIMA LETTURA NOTE DEL TRAINO
095200000000     C                   EXSR      SETFI5
095300000000     C*
095400000000     C* FINO A FINE NOTE
095500000000DO  1C     $FIN5         DOWEQ     '0'
095600000000     C*
095700000000     C* STAMPA LE NOTE
095800000000     C                   EXSR      STATLN
095900000000     C*
096000000000     C* LETTURA SUCESSIVA NOTE
096100000000     C                   EXSR      LETFI5
096200000000E   1C                   ENDDO
096300000000     C*
096400000000     C                   ENDSR
096500000000     C*------------------------------------------------------------------------*
096600000000     C* SETFI5 - POSIZIONAMENTO E PRIMA LETTURA NOTE
096700000000     C*------------------------------------------------------------------------*
096800000000     C     SETFI5        BEGSR
096900000000     C*
097000000000     C* POSIZIONAMENTO
097100000000     C                   Z-ADD     TLTTRN        KLNTRN                         *TRAINO
097200000000     C                   Z-ADD     TLTDDE        KLNDDE                         *DATA DECORRENZA
097300000000     C     KEYTLN        SETLL     TNTLN01L                           98
097400000000     C* LETTURA
097500000000IF  1C     *IN98         IFEQ      *OFF
097600000000     C                   EXSR      LETFI5
097700000000X   1C                   ELSE
097800000000     C                   MOVEL     '1'           $FIN5
097900000000E   1C                   ENDIF
098000000000     C*
098100000000     C                   ENDSR
098200000000     C*------------------------------------------------------------------------*
098300000000     C* LETFI5 - LETTURA SUCESSIVA NOTE
098400000000     C*------------------------------------------------------------------------*
098500000000     C     LETFI5        BEGSR
098600000000     C*
098700000000     C                   MOVEL     'N'           WRECOK
098800000000     C*
098900000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
099000000000DO  1C     $FIN5         DOWEQ     '0'
099100000000     C     WRECOK        ANDEQ     'N'
099200000000     C     KEYTLN        READE     TNTLN01L                               98
099300000000     C*
099400000000IF  2C     *IN98         IFEQ      *ON
099500000000     C                   MOVEL     '1'           $FIN5
099600000000X   2C                   ELSE
099700000000     C                   EXSR      CHKRE5                                       *CONTROLLA RECORD
099800000000E   2C                   ENDIF
099900000000E   1C                   ENDDO
100000000000     C*
100100000000     C                   ENDSR
100200000000     C*------------------------------------------------------------------------*
100300000000     C* CHKRE5 - CONTROLLA VALIDITA' NOTE
100400000000     C*------------------------------------------------------------------------*
100500000000     C     CHKRE5        BEGSR
100600000000     C*
100700000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
100800960102     C*
100900960102     C* NOTE ANNULLATE, ESCLUDE
101000960102IF  1C     TLNATB        IFNE      *BLANKS
101100960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
101200960102E   1C                   ENDIF
101300000000     C*
101400000000     C                   ENDSR
101500000000     C*------------------------------------------------------------------------*
101600000000     C* STATLN - STAMPA RIGA NOTE
101700000000     C*------------------------------------------------------------------------*
101800000000     C     STATLN        BEGSR
101900960115     C*
102000960115     C                   CLEAR                   TL19I3
102100960115     C                   CLEAR                   TL19D3
102200000000     C*
102300000000     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'INTESTAZIONE NOTE
102400000000IF  1C     *IN01         IFEQ      *ON
102500000000     C                   MOVEL     'N'           WINT
102600000000     C                   EXSR      STATES
102700000000X   1C                   ELSE
102800000000IF  2C     WTLN          IFEQ      *ZEROS
102900000000     C                   WRITE     TL19I5
103000000000     C                   Z-ADD     1             WTLN
103100000000E   2C                   ENDIF
103200000000E   1C                   ENDIF
103300000000     C* CAMPI
103400000000     C                   Z-ADD     TLNNFM        PD5NFM                         *NUMERO FERMATA
103500000000     C                   MOVEL     TLNTNO        PD5TNO                         *TESTO
103600000000     C* STAMPA
103700000000     C                   WRITE     TL19D5                                       *RIGA NOTE
103800000000     C*
103900000000     C                   ENDSR
104000000000     C*------------------------------------------------------------------------*
104100000000     C* STATES - STAMPA TESTATA
104200000000     C*          INPUT -> WINT: TIPO INTESTAZIONE
104300000000     C*------------------------------------------------------------------------*
104400000000     C     STATES        BEGSR
104500000000     C*
104600000000     C                   WRITE     TL19TE                                       *TESTATA
104700000000     C*
104800000000     C* STAMPA IL TIPO DI INTESTAZIONE RICHIESTO
104900000000IF  1C     WINT          IFEQ      'F'
105000000000     C                   WRITE     TL19I2                                       *INTESTAZIONE FERMAT
105100960116     C                   Z-ADD     1             WTLR
105200000000E   1C                   ENDIF
105300000000IF  1C     WINT          IFEQ      'M'
105400000000     C                   WRITE     TL19I3                                       *INTESTAZIONE MCUBI
105500960116     C                   Z-ADD     1             WTLM
105600000000E   1C                   ENDIF
105700000000IF  1C     WINT          IFEQ      'L'
105800000000     C                   WRITE     TL19I4                                       *INTESTAZIONE LISTIN
105900960116     C                   Z-ADD     1             WTLL
106000000000E   1C                   ENDIF
106100000000IF  1C     WINT          IFEQ      'N'
106200000000     C                   WRITE     TL19I5                                       *INTESTAZIONE NOTE
106300960116     C                   Z-ADD     1             WTLN
106400000000E   1C                   ENDIF
106500000000     C                   MOVEL     '0'           *IN01
106600000000     C*
106700000000     C                   ENDSR
106800961227     C*------------------------------------------------------------------------*
106900961227     C* STATOT - STAMPA RIGA DEI TOTALI LISTINI
107000961227     C*------------------------------------------------------------------------*
107100961227     C     STATOT        BEGSR
107200961227     C*
107300970423     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
107400961227     C                   WRITE     TL19TE                                       *TESTATA
107500961227     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
107600961227     C*
107700961227     C                   Z-ADD     TOTILI        TOCILI
107800961227     C                   Z-ADD     TOTIES        TOCIES
107900011001     c                   if        totili <> *zeros
108000010928     c                   movel     §gedcn        todili
108100011001     c                   endif
108200011001     c                   if        toties <> *zeros
108300010928     c                   movel     §gedcn        todies
108400011001     c                   endif
108500961227     C                   WRITE     TL19TO                                       *TOTALI
108600961227     C*
108700961227     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
108800961227     C*
108900961227     C                   ENDSR
109000960115     C*------------------------------------------------------------------------*
109100960115     C* ELATLT - MEMORIZZA I TRAINI DA STAMPARE
109200960115     C*------------------------------------------------------------------------*
109300960115     C     ELATLT        BEGSR
109400960115     C*
109500960115     C* POSIZIONAMENTO E PRIMA LETTURA TRAINI
109600960115     C                   Z-ADD     *ZEROS        T                 5 0
109700960115     C                   EXSR      SETFIL
109800960115     C*
109900960115     C* FINO A FINE TRAINI
110000960115DO  1C     $FINE         DOWEQ     '0'
110100960115     C*
110200960115     C                   EXSR      MEMTLT
110300960115     C*
110400960115     C* LETTURA SUCESSIVA TRAINI
110500960115     C                   EXSR      LETFIL
110600960115E   1C                   ENDDO
110700960116     C*
110800960116     C* ORDINA I TRAINI PER LA STAMPA
110900960116     C                   EXSR      ORDTLT
111000960115     C*
111100960115     C                   ENDSR
111200960115     C*------------------------------------------------------------------------*
111300960115     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA TRAINI
111400960115     C*------------------------------------------------------------------------*
111500960115     C     SETFIL        BEGSR
111600960115     C*
111700960115     C* POSIZIONAMENTO
111800960115     C     *LOVAL        SETLL     TNTLT01L                           99        *FINE FILE
111900960115     C* RPIMA LETTURA
112000960115IF  1C     *IN99         IFEQ      *OFF
112100960115     C                   EXSR      LETFIL
112200960115X   1C                   ELSE
112300960115     C                   MOVEL     '1'           $FINE
112400960115E   1C                   ENDIF
112500960115     C*
112600960115     C                   ENDSR
112700960115     C*------------------------------------------------------------------------*
112800960115     C* LETFIL - LETTURA SUCESSIVA TRAINI
112900960115     C*------------------------------------------------------------------------*
113000960115     C     LETFIL        BEGSR
113100960115     C*
113200960115     C                   MOVEL     'N'           WRECOK
113300960115     C*
113400960115     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
113500960115DO  1C     $FINE         DOWEQ     '0'
113600960115     C     WRECOK        ANDEQ     'N'
113700960115     C                   READ      TNTLT01L                               99
113800960115IF  2C     *IN99         IFEQ      *ON
113900960115     C                   MOVEL     '1'           $FINE
114000960115X   2C                   ELSE
114100960115     C                   EXSR      CHKREC                                       *CONTROLLA RECROD
114200960115E   2C                   ENDIF
114300960115E   1C                   ENDDO
114400960115     C*
114500960115     C                   ENDSR
114600960115     C*------------------------------------------------------------------------*
114700960115     C* CHKREC - CONTROLLA VALIDITA' TRAINI
114800960115     C*------------------------------------------------------------------------*
114900960115     C     CHKREC        BEGSR
115000960115     C*
115100960115     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
115200960115     C*
115300960115     C* TRAINO ANNULLATO, ESCLUDE
115400960115IF  1C     TLTATB        IFNE      *BLANKS
115500960115     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
115600960115E   1C                   ENDIF
115700960115     C*
115800960115     C* TRAINO NON IN DECORRENZA ALLA DATA RICHIESTA, ESCLUDE
115900060323IF  1C**   PARDDE        IFGT      *ZEROS
116000060323IF  2C**   PARDDE        IFGE      TLTDDE
116100060323     C**   PARDDE        ANDLE     TLTDSC
116200060323X   2C**                 ELSE
116300060323     C**                 MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
116400060323E   2C**                 ENDIF
116500060323E   1C**                 ENDIF
116600060323     c* Traino scaduto prima della data dal o decorrente dopo la data al, es
116700060323     c* clude
116800060323     c     tltdsc        iflt      pardda
116900060323     c     tltdde        orgt      pardde
117000060323     c                   movel     'N'           wrecok
117100060323     c                   endif
117200961021     C*
117300961021     C* NUMERO TRAINO MINORE DEL NUMERO TRAINO DAL RICHIESTO,  ESCLUDE
117400961021IF  1C     PARTR1        IFGT      *ZEROS
117500961021     C     TLTTRN        ANDLT     PARTR1
117600961021     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
117700961021E   1C                   ENDIF
117800961021     C*
117900961021     C* NUMERO TRAINO MAGGIORE DEL NUMERO TRAINO AL RICHIESTO,  ESCLUDE
118000961021IF  1C     PARTR2        IFGT      *ZEROS
118100961021     C     TLTTRN        ANDGT     PARTR2
118200961021     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
118300961021E   1C                   ENDIF
118400961021     C*
118500960115     C* GIORNO PARTENZA <> DA QUELLO RICHIESTO, ESCLUDE
118600960115IF  1C     PARGIO        IFNE      *BLANKS
118700960115     C     PARGIO        SCAN      TLTGSE                                 28
118800960115IF  2C     *IN28         IFEQ      *OFF
118900960115     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
119000960115E   2C                   ENDIF
119100960115E   1C                   ENDIF
119200060323     c* trazionista <> da quello richiesto, esclude
119300060323     c     parpdr        ifgt      *zeros
119400060323     C                   Z-ADD     TLTTRN        KLLTRN                         *TRAINO
119500060323     C                   Z-ADD     TLTDDE        KLLDDE                         *DATA DECORRENZA
119600060323     C                   Z-ADD     parpdr        KLLpdr                         *DATA DECORRENZA
119700060323     C     KEYTLL2       SETLL     TITLL02L
119800060323     c                   if        not %equal(titll02l)
119900060323     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
120000060323     c                   endif
120100060323     c                   endif
120200060323     c* se richiesto bis e traino non bis, esclude
120300060323     c* se richiesti i non bis e traino bis, esclude
120400060323     c                   if        (parbis='S' and tltbis <> 'B' and
120500060323     c                              tlttrn < 9000000) or
120600060323     c                             (parbis='E' and (tltbis='B' or
120700060323     c                              tlttrn >=9000000))
120800060323     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
120900060323     c                   endif
121000060323     c
121100960115     C*--------------------
121200960115     C* FERMATE
121300960115     C*--------------------
121400960115     C***
121500960213     C* TRAINI DI UNA FILIALE ==> FERMATA ALLA FILIALE (SE RICHIESTA)
121600960115     C* TRAINI PARTENZA ==> SOLO FERMATE CHE CARICANO  ALLA FILIALE RICHIESTA
121700960115     C* TRAINI ARRIVO   ==> SOLO FERMATE CHE SCARICANO ALLA FILIALE RICHIESTA
121800960115     C***
121900960115     C                   Z-ADD     *ZEROS        DEPNFM
122000960115IF  1C     WRECOK        IFEQ      'S'                                          *TRAINO ANCORA VALID
122100960213IF  2C     PARORD        IFEQ      '3'                                          *ORD X ORARIO PART
122200960213     C     PARORD        OREQ      '4'                                          *ORD X ORARIO ARR
122300960213     C     PARORD        OREQ      '2'                                          *ORD X FILIALE
122400960213     C     PARFIL        ANDGT     *ZEROS                                       *SE RICH UNA FILIALE
122500960115     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
122600960115     C                   Z-ADD     TLTTRN        KLRTRN                         *TRAINO
122700960115     C                   Z-ADD     TLTDDE        KLRDDE                         *DATA DECORRENZA
122800960115     C     KEYTLR        SETLL     TNTLR01L                           98
122900960115     C     KEYTLR        READE     TNTLR01L                               98
123000960115DO  3C     *IN98         DOWEQ     *OFF
123100960115     C     WRECOK        ANDEQ     'N'
123200960115IF  4C     TLRATB        IFEQ      *BLANKS                                      *NON ANULLATA
123300960115IF  5C     PARFIL        IFEQ      TLRFIL
123400960118IF  6C     PARORD        IFEQ      '3'
123500960116     C     TLRCAR        ANDEQ     'S'
123600960118     C     PARORD        OREQ      '4'
123700960116     C     TLRSCA        ANDEQ     'S'
123800960213     C     PARORD        OREQ      '2'
123900960115     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
124000960115     C                   Z-ADD     TLRNFM        DEPNFM                         *NUMERO FERMATA
124100960115E   6C                   ENDIF
124200960115E   5C                   ENDIF
124300960115E   4C                   ENDIF
124400960115     C     KEYTLR        READE     TNTLR01L                               98
124500960115E   3C                   ENDDO
124600960115E   2C                   ENDIF
124700960115E   1C                   ENDIF
124800960115     C*
124900110527     c* se richiesti solo traini con punti di scambio, cerco
125000110527     c                   if        partpsc<>'N'
125100110527     c                   exsr      cercaTLS
125200110527     c                   if        (partpsc='S' and westls<>'S') or
125300110527     c                             (partpsc='E' and westls='S')
125400110527     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
125500110609     c                   ELSE
125600110609     c                   if        parpsc<>*blanks and wespsc<>'S'
125700110609     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
125800110609     c                   endif
125900110609     c                   endif
126000110609     c
126100110527     c                   endif
126200110609     c
126300960115     C                   ENDSR
126400960115     C*------------------------------------------------------------------------*
126500960115     C* MEMTLT - MEMORIZZA IN SCHIERE I TRAINI VALIDI
126600960115     C*------------------------------------------------------------------------*
126700960115     C     MEMTLT        BEGSR
126800960115     C* TRAINO
126900960115     C                   ADD       1             T
127000960115     C                   Z-ADD     TLTTRN        TTRN(T)
127100960115     C                   Z-ADD     TLTDDE        TDDE(T)
127200960118     C                   Z-ADD     TLTTFP        TTFP(T)
127300960115     C* ORARI
127400960118IF  1C     PARORD        IFEQ      '3'
127500960118     C     PARORD        OREQ      '4'
127600960118     C                   Z-ADD     TLTTRN        KLOTRN                         *TRAINO VALIDO
127700960115     C                   Z-ADD     TLTDDE        KLODDE
127800960115     C                   Z-ADD     DEPNFM        KLONFM                         *FERMATA DEPOSITATA
127900960115     C                   MOVEL     PARGIO        KLOGSE                         *GIORNO SELEZIONATO
128000960115     C     KEYTLO        CHAIN     TNTLO01L                           98
128100960115IF  2C     *IN98         IFEQ      *ON
128200960115     C                   MOVEL     *BLANKS       KLOGSE                         *GIORNO "TUTTI"
128300960115     C     KEYTLO        CHAIN     TNTLO01L                           98
128400960115E   2C                   ENDIF
128500960115IF  2C     *IN98         IFEQ      *OFF
128600960115     C                   MOVEL     TLOGGA        TGHA(T)                        *ARRIVO
128700960115     C                   MOVE      TLOHHA        TGHA(T)
128800960115     C                   MOVEL     TLOGGP        TGHP(T)                        *PARTENZA
128900960115     C                   MOVE      TLOHHP        TGHP(T)
129000960115E   2C                   ENDIF
129100960115E   1C                   ENDIF
129200960115     C*
129300960115     C                   ENDSR
129400960116     C*------------------------------------------------------------------------*
129500960116     C* ORDTLT - ORDINA I TRAINI PER LA STAMPA
129600960116     C*------------------------------------------------------------------------*
129700960116     C     ORDTLT        BEGSR
129800960118     C*
129900960118DO  1C     1             DO        T             J                 4 0
130000960116     C     1             ADD       J             K
130100960118     C*
130200960118DO  2C     K             DO        T             K                 4 0
130300960116     C*
130400960118IF  3C     PARORD        IFEQ      '1'                                          *SECONDO ORDINAMENTO
130500960118     C     TTRN(J)       ANDGT     TTRN(K)                                       TRAINO
130600960118     C     PARORD        OREQ      '2'                                          *SECONDO ORDINAMENTO
130700960118     C     TTFP(J)       ANDGT     TTFP(K)                                       FILIALE
130800960118     C     PARORD        OREQ      '3'                                          *TERZO ORDINAMENTO
130900960118     C     TGHP(J)       ANDGT     TGHP(K)                                       ORARIO PARTENZA
131000960118     C     PARORD        OREQ      '4'                                          *QUARDO ORDINAMENTO
131100960118     C     TGHA(J)       ANDGT     TGHA(K)                                       ORARIO ARRIVO
131200960116     C*
131300960116     C                   MOVEL     TGHP(J)       WGHP              5            *GIORNO+ORA PARTENZA
131400960116     C                   MOVEL     TGHP(K)       TGHP(J)
131500960116     C                   MOVEL     WGHP          TGHP(K)
131600960116     C*
131700960116     C                   MOVEL     TGHA(J)       WGHA              5            *GIORNO+ORA ARRIVO
131800960116     C                   MOVEL     TGHA(K)       TGHA(J)
131900960116     C                   MOVEL     WGHA          TGHA(K)
132000960116     C*
132100960116     C                   Z-ADD     TTRN(J)       WTRN              7 0          *NUMERO TRAINO
132200960116     C                   Z-ADD     TTRN(K)       TTRN(J)
132300960116     C                   Z-ADD     WTRN          TTRN(K)
132400960116     C*
132500960116     C                   Z-ADD     TDDE(J)       WDDE              8 0          *DATA DECORRENZA
132600960116     C                   Z-ADD     TDDE(K)       TDDE(J)
132700960116     C                   Z-ADD     WDDE          TDDE(K)
132800960118     C*
132900960118     C                   Z-ADD     TTFP(J)       WTFP              3 0          *FILIALE
133000960118     C                   Z-ADD     TTFP(K)       TTFP(J)
133100960118     C                   Z-ADD     WTFP          TTFP(K)
133200960116     C*
133300960118E   3C                   ENDIF
133400960118E   2C                   ENDDO
133500960118E   1C                   ENDDO
133600960116     C*
133700960116     C                   ENDSR
133800000000     C*------------------------------------------------------------------------*
133900000000     C* NEWTLT - OPERAZIONI PER NUOVO TRAINO
134000000000     C*------------------------------------------------------------------------*
134100000000     C     NEWTLT        BEGSR
134200960115     C*
134300960115     C                   MOVEL     '0'           $ERR
134400000000     C*
134500000000     C* MEMORIZZA IL NUOVO TRAINO IN UN DEPOSITO
134600960115     C                   Z-ADD     TTRN(T)       DEPTRN                         *TRAINO
134700960115     C                   Z-ADD     TDDE(T)       DEPDDE                         *DATA DECORRENZA
134800960115     C*
134900960115     C* LEGGE IL TRAINO MEMORIZZATO
135000960115     C                   Z-ADD     TTRN(T)       KLTTRN
135100960115     C                   Z-ADD     TDDE(T)       KLTDDE
135200960115     C     KEYTLT        CHAIN     TNTLT01L                           99        *FINE FILE
135300960115IF  1C     *IN99         IFEQ      *ON
135400960115     C                   MOVEL     '1'           $ERR
135500960115E   1C                   ENDIF
135600961227     C*
135700961227     C* FLAG DI PASSAGGIO STAMPA LISTINO
135800961227     C                   MOVEL     '0'           $STLL                          *STAMPA LISTINO
135900000000     C*
136000000000     C                   ENDSR
136100000000     C*------------------------------------------------------------------------*
136200000000     C* STAPAR - STAMPA PAGINA PARAMETRI
136300000000     C*------------------------------------------------------------------------*
136400000000     C     STAPAR        BEGSR
136500960115     C* PARAMETRI
136600960118     C                   MOVEL     PARORD        PACORD
136700960118IF  1C     PARORD        IFEQ      '1'
136800960118     C                   MOVEL     DPAR(11)      PADORD
136900960115E   1C                   ENDIF
137000960118IF  1C     PARORD        IFEQ      '2'
137100960118     C                   MOVEL     DPAR(12)      PADORD
137200960118E   1C                   ENDIF
137300960118IF  1C     PARORD        IFEQ      '3'
137400960118     C                   MOVEL     DPAR(13)      PADORD
137500960118E   1C                   ENDIF
137600960118IF  1C     PARORD        IFEQ      '4'
137700960118     C                   MOVEL     DPAR(14)      PADORD
137800960118E   1C                   ENDIF
137900960118     C*
138000060323IF  1C     PARDDa        IFGT      *ZEROS
138100060323     C                   Z-ADD     PARDDa        G08INV                         *DECORRENZA
138200060323     C                   Z-ADD     *ZEROS        G08DAT
138300060323     C                   MOVEL     '3'           G08ERR
138400060323     C                   CALL      'XSRDA8'
138500060323     C                   PARM                    WLBDA8
138600060323     C                   Z-ADD     G08DAT        PACDDA
138700060323E   1C                   ENDIF
138800960115IF  1C     PARDDE        IFGT      *ZEROS
138900000000     C                   Z-ADD     PARDDE        G08INV                         *DECORRENZA
139000000000     C                   Z-ADD     *ZEROS        G08DAT
139100000000     C                   MOVEL     '3'           G08ERR
139200000000     C                   CALL      'XSRDA8'
139300000000     C                   PARM                    WLBDA8
139400000000     C                   Z-ADD     G08DAT        PACDDE
139500960115E   1C                   ENDIF
139600960115IF  1C     PARFIL        IFGT      *ZEROS
139700960115     C                   Z-ADD     PARFIL        PACFIL                         *FILIALE
139800960115     C                   Z-ADD     1             J
139900960115     C     PARFIL        LOOKUP    ORG(J)                                 28
140000960115     C                   MOVEL     DORG(J)       PADFIL
140100960115E   1C                   ENDIF
140200960115IF  1C     PARGIO        IFNE      *BLANKS
140300960115     C                   MOVEL     PARGIO        PACGIO                         *GIORNO DI PARTENZA
140400960115     C                   MOVEL     PARGIO        N1                1 0
140500960115     C                   MOVEL     DPAR(N1)      PADGIO
140600960115E   1C                   ENDIF
140700961021     C                   Z-ADD     PARTR1        PACTR1                         *NUMERO TRAINO DAL
140800961021     C                   Z-ADD     PARTR2        PACTR2                         *NUMERO TRAINO AL
140900960115     C* TESTATA
141000960118     C                   MOVEL     PADORD        TEDORD
141100060323IF  1C     PARDDA        IFGT      *ZEROS
141200060323     C                   MOVEL     'DAL'         TIDDDA
141300060323     C                   Z-ADD     PACDDA        TECDDA
141400060323E   1C                   ENDIF
141500960118IF  1C     PARDDE        IFGT      *ZEROS
141600960118     C                   MOVEL     'AL'          TIDDDE
141700960118     C                   Z-ADD     PACDDE        TECDDE
141800960118E   1C                   ENDIF
141900960116IF  1C     PARFIL        IFGT      *ZEROS
142000960116     C                   MOVEL     'DA'          TIDFIL
142100960116     C                   MOVEL     PADFIL        TEDFIL
142200960116E   1C                   ENDIF
142300060323     c* trazionista
142400060323     c                   clear                   padpdr
142500060323     c                   clear                   pacpdr
142600060323     c                   if        parpdr > *zeros
142700060323     c                   move      parpdr        pacpdr
142800060323     C                   EVAL      KLZTIP = 'T '
142900060323     C                   z-add     parpdr        KLZPDR
143000060323     C     KEYTLZ        CHAIN     TNTLZ01L                           28
143100060323IF  2C     *IN28         IFEQ      *OFF
143200060323     C                   MOVEL     TLZRSC        padpdr                         *DESCR.NE TRAZIONIST
143300060323     c                   endif
143400060323     c                   endif
143500060323     c* traini bis si/no/escludi
143600060323     c                   clear                   padbis
143700060323     c                   select
143800060323     c                   when      parbis = 'S'
143900060323     c                   eval      padbis = 'Si'
144000060323     c                   when      parbis = 'N'
144100060323     c                   eval      padbis = 'No'
144200060323     c                   when      parbis = 'E'
144300060323     c                   eval      padbis = 'Escludi'
144400060323     c                   endsl
144500110527
144600110527     c                   select
144700110527     c                   when      partpsc = 'S'
144800110527     c                   eval      padtpsc = 'Si'
144900110527     c                   when      partpsc = 'N'
145000110527     c                   eval      padtpsc = 'No'
145100110527     c                   when      partpsc = 'E'
145200110527     c                   eval      padtpsc = 'Escludi'
145300110527     c                   endsl
145400960115     C*
145500110609     c* codice spunti di scambio
145600110609     c     parpsc        chain     tnpsc01l
145700110609    2c                   if        %found(tnpsc01l)
145800110609     c                   eval      desloc=pscloc
145900110609    2c                   if        pscprv<>*blanks
146000110609     c                   eval      desloc=%trim(desloc)+'  '+pscprv+pscnar
146100110609     c                   else
146200110609     c                   eval      desloc=%trim(desloc)+' '+pscnar
146300110609    2c                   endif
146400110609     c                   eval      desind=pscind
146500110609     c                   eval      descap=psccap
146600110609    1c                   endif
146700110609     c
146800110609     C                   WRITE     TL19TE                                       *TESTATA
146900110609     C                   WRITE     TL19PA                                       *PARAMETRI
147000000000     C*
147100000000     C                   ENDSR
147200000000     C*------------------------------------------------------------------------*
147300000000     C* CARTAB - CARICA LE TABELLE
147400000000     C*------------------------------------------------------------------------*
147500000000     C     CARTAB        BEGSR
147600000000     C*--------------------
147700000000     C* FILIALI
147800000000     C*--------------------
147900000000     C                   Z-ADD     *ZEROS        J                 4 0
148000000000     C     *LOVAL        SETLL     AZORG01L
148100000000     C                   READ      AZORG01L                               99
148200000000DO  1C     *IN99         DOWEQ     *OFF
148300000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
148400000000     C                   ADD       1             J
148500000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
148600000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
148700000000E   2C                   ENDIF
148800000000     C                   READ      AZORG01L                               99
148900000000E   1C                   ENDDO
149000000000     C*--------------------
149100000000     C* TIPI TRAINO
149200000000     C*--------------------
149300000000     C                   Z-ADD     *ZEROS        J
149400020715     c                   Z-Add     1             KtbKut
149500000000     C                   MOVEL     'TV'          KTBCOD
149600000000     C     KE2TAB        SETLL     TABEL00F
149700000000     C     KE2TAB        READE     TABEL00F                               99
149800000000DO  1C     *IN99         DOWEQ     *OFF
149900000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
150000000000     C                   ADD       1             J
150100000000     C                   MOVEL     TBLKEY        TTR(J)                         *CODICE
150200000000     C                   MOVEL     TBLUNI        DSTV
150300000000     C                   MOVEL     §TVDES        DTTR(J)                        *DESCRIZIONE
150400000000E   2C                   ENDIF
150500000000     C     KE2TAB        READE     TABEL00F                               99
150600000000E   1C                   ENDDO
150700000000     C*--------------------
150800000000     C* TIPI AUTOMEZZO
150900000000     C*--------------------
151000000000     C                   Z-ADD     *ZEROS        J
151100020715     c                   Z-Add     1             KtbKut
151200000000     C                   MOVEL     'TU'          KTBCOD
151300000000     C     KE2TAB        SETLL     TABEL00F
151400000000     C     KE2TAB        READE     TABEL00F                               99
151500000000DO  1C     *IN99         DOWEQ     *OFF
151600000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
151700000000     C                   ADD       1             J
151800000000     C                   MOVEL     TBLKEY        TMZ(J)                         *CODICE
151900000000     C                   MOVEL     TBLUNI        DSTU
152000000000     C                   MOVEL     §TUDES        DTMZ(J)                        *DESCRIZIONE
152100000000E   2C                   ENDIF
152200000000     C     KE2TAB        READE     TABEL00F                               99
152300000000E   1C                   ENDDO
152400000000     C*--------------------
152500000000     C* CAUSALI CONTABILIZZAZIONE
152600000000     C*--------------------
152700020715     c                   If        DutLpo = 'S'
152800000821     C                   OPEN      ANATB01L
152900000000     C                   Z-ADD     *ZEROS        J
153000000821     C                   MOVE      KNSIF         WCOMO4            4
153100000821     C                   MOVEL     WCOMO4        WSOC              3
153200000821     C                   movel     WSOC          KAtazi
153300000211     C                   MOVEL     'Y4Z'         KatCOD
153400000211     C     KEATAB        CHAIN     ANATB01L                           99
153500000821IF  2C     *IN99         IFEQ      *ON
153600000218     C                   movel     '888'         KAtazi
153700000211     C     KEATAB        CHAIN     ANATB01L                           99
153800000821E   2C                   ENDIF
153900000821DO  2C     *IN99         DOWEQ     *OFF
154000000211     C                   ADD       1             J
154100000211     C                   MOVEL     ATBKEY        CAU(J)
154200000211     C                   MOVEL     ATBUNI        ANGY4ZDS
154300000211     C                   MOVEL     §4ZDES        DCAU(J)
154400000211     C     KEATAB        READE     ANATB01L                               99
154500000821E   2C                   ENDDO
154600000821     C                   CLOSE     ANATB01L
154700000821E   1C                   ENDIF
154800010928      *--------------------
154900010928      * Moneta di conto
155000010928      *--------------------
155100010928     c                   clear                   dged
155200010928     c                   clear                   tibs02ds
155300010928     c                   movel     'L'           t02tla
155400010928     c                   movel     'C'           t02mod
155500010928     c                   movel     knsif         t02sif
155600010928     c                   movel     'GED'         t02cod
155700010928     c                   movel     '1'           t02ke1
155800010928     c                   call      'TIBS02R'
155900010928     c                   parm                    kpjba
156000010928     c                   parm                    tibs02ds
156100010928    5c                   movel     t02uni        dged
156200010928
156300010928     c                   clear                   dscv
156400010928     c                   movel     'CV'          ktbcod
156500010928     c                   movel(p)  §gedcn        ktbkey
156600010928     c     ktab          chain     tabel00f
156700010928     c                   if        %found(tabel00f)
156800010928     c                   movel     tbluni        dscv
156900010928     c                   endif
157000010928
157100000000     C*
157200000000     C                   ENDSR
157300000000     C*------------------------------------------------------------------------*
157400000000     C* *INZSR - ROUTINE INIZIALE
157500000000     C*------------------------------------------------------------------------*
157600000000     C     *INZSR        BEGSR
157700000000     C*--------------------
157800000000     C* RICEVIMENTO PARAMETRI
157900000000     C*--------------------
158000000000     C     *ENTRY        PLIST
158100000000     C                   PARM                    KPJBA
158200000000     C                   MOVEL     KPJBU         PARAM
158300000000     C*--------------------
158400000000     C* VARIABILI RIFERITE AL DATA BASE
158500000000     C*--------------------
158600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
158700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
158800010928     C     *LIKE         DEFINE    TBLkey        KTBkey
158900000211     C     *LIKE         DEFINE    ATBCOD        KATCOD
159000000211     C     *LIKE         DEFINE    ATBAZI        KATAZI
159100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
159200000000     C     *LIKE         DEFINE    TLZPDR        KLZPDR                         *TNTLZ00F
159300000821     C     *LIKE         DEFINE    TLZTIP        KLZTIP
159400000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
159500000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
159600000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
159700000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
159800000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
159900000000     C     *LIKE         DEFINE    TLODDE        KLODDE
160000000000     C     *LIKE         DEFINE    TLONFM        KLONFM
160100000000     C     *LIKE         DEFINE    TLOGSE        KLOGSE
160200000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
160300000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
160400010725     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
160500000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
160600060323     C     *LIKE         DEFINE    TLLpdr        KLLpdr
160700000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
160800000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
160900000000     C     *LIKE         DEFINE    TLTTRN        DEPTRN                         *DEPOSITI
161000000000     C     *LIKE         DEFINE    TLTDDE        DEPDDE
161100960116     C     *LIKE         DEFINE    TLRNFM        DEPNFM
161200000000     C*--------------------
161300000000     C* CHIAVI DI LETTURA
161400000000     C*--------------------
161500000000     C* LETTURA AZORG01L - COMPLETA
161600000000     C     KEYORG        KLIST
161700000000     C                   KFLD                    KORFIL                         *FILIALE
161800000000     C* LETTURA TNTAB00F - PARZIALE
161900000000     C     KE2TAB        KLIST
162000000000     C                   KFLD                    KTBKUT                         *UTENTE
162100000000     C                   KFLD                    KTBCOD                         *TABELLA
162200010928
162300010928     C     KTAB          KLIST
162400010928     C                   KFLD                    ktbkut
162500010928     C                   KFLD                    Ktbcod
162600010928     C                   KFLD                    Ktbkey
162700000211     C* LETTURA ANATB01L - PARZIALE
162800000211     C     KEATAB        KLIST
162900000211     C                   KFLD                    KATCOD
163000000211     C                   KFLD                    KATAZI
163100000000     C* LETTURA TNTLZ01L - COMPLETA
163200000000     C     KEYTLZ        KLIST
163300000821     C                   KFLD                    KLZTIP                         *TIPO
163400000821     C                   KFLD                    KLZPDR                         *CODICE
163500000000     C* LETTURA TNTLT01L - COMPLETA
163600000000     C     KEYTLT        KLIST
163700000000     C                   KFLD                    KLTTRN                         *TRAINO
163800000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
163900000000     C* LETTURA TNTLR01L - PARZIALE
164000000000     C     KEYTLR        KLIST
164100000000     C                   KFLD                    KLRTRN                         *TRAINO
164200000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
164300000000     C* LETTURA TNTLO01L - COMPLETA
164400000000     C     KEYTLO        KLIST
164500000000     C                   KFLD                    KLOTRN                         *TRAINO
164600000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
164700000000     C                   KFLD                    KLONFM                         *NUMERO FERMATA
164800000000     C                   KFLD                    KLOGSE                         *GIORNO DI PARTENZA
164900000000     C* LETTURA TNTLM01L - PARZIALE
165000000000     C     KEYTLM        KLIST
165100000000     C                   KFLD                    KLMTRN                         *TRAINO
165200000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
165300010725     C* LETTURA TITLL01L - PARZIALE
165400000000     C     KEYTLL        KLIST
165500000000     C                   KFLD                    KLLTRN                         *TRAINO
165600000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
165700060323     C* LETTURA TITLL02L - PARZIALE
165800060323     C     KEYTLL2       KLIST
165900060323     C                   KFLD                    KLLpdr                         *TRAINO
166000060323     C                   KFLD                    KLLTRN                         *TRAINO
166100060323     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
166200000000     C* LETTURA TNTLN01L - PARZIALE
166300000000     C     KEYTLN        KLIST
166400000000     C                   KFLD                    KLNTRN                         *TRAINO
166500000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
166600110526     C* LETTURA generica per traino
166700110526     C     KTLT          KLIST
166800110526     C                   KFLD                    ktrn                           *TRAINO
166900110526     C                   KFLD                    Kdde                           *DATA DECORRENZA
167000000000     C*--------------------
167100000000     C* VARIABILI DI CONTROLLO
167200000000     C*--------------------
167300000000     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA TRAINI
167400000000     C                   MOVEL     '0'           $FIN2             1            *FINE LETTURA FERMAT
167500000000     C                   MOVEL     '0'           $FIN3             1            *FINE LETTURA MCUBI
167600000000     C                   MOVEL     '0'           $FIN4             1            *FINE LETTURA LISTIN
167700000000     C                   MOVEL     '0'           $FIN5             1            *FINE LETTURA NOTE
167800000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
167900000000     C                   MOVEL     '1'           *IN01                          *OVERFLOW
168000000000     C                   MOVEL     *BLANKS       WINT              1            *INTESTAZ. IN STAMPA
168100000000     C                   Z-ADD     *ZEROS        WTLR              1 0          *STAMPA 1^FERMATA
168200000000     C                   Z-ADD     *ZEROS        WTLM              1 0          *STAMPA 1^MCUBO
168300000000     C                   Z-ADD     *ZEROS        WTLL              1 0          *STAMPA 1^LISTINO
168400000000     C                   Z-ADD     *ZEROS        WTLN              1 0          *STAMPA 1^NOTA
168500960115     C                   MOVEL     '0'           $ERR              1            *ERRORE IN LETTURA
168600961227     C                   MOVEL     '0'           $STLL             1            *STAMPA LISTINO
168700000000     C*
168800000000     C* REPERISCE I DATI UTENTE
168900020715
169000020715     c     *dtaara       define    §azute        azuteds
169100020715     c     *dtaara       define    §datiute      ddatiute
169200020715
169300020715     c                   in(E)     *dtaara
169400020715     c                   if        %Error or rsut = *blanks
169500020715     c                   clear                   tibs34ds
169600020715     c                   call      'TIBS34R'
169700020715     c                   parm                    Tibs34Ds
169800020715     c                   in        *dtaara
169900020715     c                   endif
170000000000     C*
170100000000     C* CARICA LE TABELLE
170200000000     C                   EXSR      CARTAB
170300960115     C*
170400960118     C* MEMORIZZA IN SCHIERA I TRAINI VALIDI DA STAMPARE CON ORDINE PRESCELTO
170500960115     C                   EXSR      ELATLT
170600000000     C*
170700000000     C                   ENDSR
170800000000     O*------------------------------------------------------------------------*
170900960115**   DPAR - DECODIFICHE PARAMETRI
171000960115LUNEDI'             1
171100960115MARTEDI'            2
171200960115MERCOLEDI'          3
171300960115GIOVEDI'            4
171400960115VENERDI'            5
171500960115SABATO              6
171600960115DOMENICA            7
1717009601188
1718009601189
17190096011810
172000960118NUMERO TRAINO       11
172100961022PUNTO OPERATIVO     12
172200960118ORARIO DI PARTENZA  13
172300960118ORARIO DI ARRIVO    14
17240096011815
172500960115**   DIN2 - DECODIFICHE INTESTAZIONE
172600960115¦ARR    LUN    PAR¦   1
172700960115¦ARR    MAR    PAR¦   2
172800960115¦ARR    MER    PAR¦   3
172900960115¦ARR    GIO    PAR¦   4
173000960115¦ARR    VEN    PAR¦   5
173100960115¦ARR    SAB    PAR¦   6
173200960115¦ARR    DOM    PAR¦   7
173300960115¦ARR   TUTTI   PAR¦   8
